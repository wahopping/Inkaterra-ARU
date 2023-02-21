
# Chris Sayers
# updated February 20, 2023

#---------------------- LOADING/MERGING THE DATA -------------------------------
library(tidyverse)
library(dplyr)

# pulling in habitat information and community metrics calculated in Python
SiteData <- read.csv("Spreadsheets/Site-Data.csv") %>%
  dplyr::select(Site, Day, Hab1, Hab2, Hab3, Edge.Distance) %>% 
  mutate(Day = as.factor(Day), Site = as.factor(Site))

# pulling in annotation data to calculate species richness at each 5-minute interval
# between 0600-0700
Annotations <- read.csv("Spreadsheets/All-Annotations.csv") %>% 
  mutate(Day = as.factor(Day), Site = as.factor(Site)) %>% 
  mutate(Time.Window.Start = if_else(Begin.Time..s. >= 0 & Begin.Time..s. < 300, 1,
                                     if_else(Begin.Time..s. >= 300 & Begin.Time..s. < 600, 2,
                                     if_else(Begin.Time..s. >= 600 & Begin.Time..s. < 900, 3,
                                     if_else(Begin.Time..s. >= 900 & Begin.Time..s. < 1200, 4,
                                     if_else(Begin.Time..s. >= 1200 & Begin.Time..s. < 1500, 5,
                                     if_else(Begin.Time..s. >= 1500 & Begin.Time..s. < 1800, 6,
                                     if_else(Begin.Time..s. >= 1800 & Begin.Time..s. < 2100, 7,
                                     if_else(Begin.Time..s. >= 2100 & Begin.Time..s. < 2400, 8,
                                     if_else(Begin.Time..s. >= 2400 & Begin.Time..s. < 2700, 9,
                                     if_else(Begin.Time..s. >= 2700 & Begin.Time..s. < 3000, 10,
                                     if_else(Begin.Time..s. >= 3000 & Begin.Time..s. < 3300, 11, 12)))))))))))) %>%
  mutate(Time.Window.End = if_else(End.Time..s. >= 0 & End.Time..s. < 300, 1,
                                     if_else(End.Time..s. >= 300 & End.Time..s. < 600, 2,
                                     if_else(End.Time..s. >= 600 & End.Time..s. < 900, 3,
                                     if_else(End.Time..s. >= 900 & End.Time..s. < 1200, 4,
                                     if_else(End.Time..s. >= 1200 & End.Time..s. < 1500, 5,
                                     if_else(End.Time..s. >= 1500 & End.Time..s. < 1800, 6,
                                     if_else(End.Time..s. >= 1800 & End.Time..s. < 2100, 7,
                                     if_else(End.Time..s. >= 2100 & End.Time..s. < 2400, 8,
                                     if_else(End.Time..s. >= 2400 & End.Time..s. < 2700, 9,
                                     if_else(End.Time..s. >= 2700 & End.Time..s. < 3000, 10,
                                     if_else(End.Time..s. >= 3000 & End.Time..s. < 3300, 11, 12)))))))))))) %>% 
  pivot_longer(c(Time.Window.Start, Time.Window.End),
               names_to = "Time.Window.Label", values_to = "Time.Window")

SpeciesRichnessDay <- Annotations %>% 
  # excluding individuals that were not identified with 100% confidence
  filter(exclusion.code <= 3) %>% 
  # calculating species richness per day per site
  group_by(Site, Day) %>% 
  summarize(Species.Richness.Day = length(unique(species)))

SpeciesRichnessWindow <- Annotations %>% 
  # excluding individuals that were not identified with 100% confidence
  filter(exclusion.code <= 3) %>% 
  # calculating species richness per time window per day per site
  group_by(Site, Day, Time.Window) %>% 
  summarize(Species.Richness.Window = length(unique(species)))

# putting everything together
WindowData <- left_join(SpeciesRichnessWindow, SiteData, by = c("Site", "Day"))
  
# DATA VISUALIZATION ------------------------------------------------------
library(ggpubr)
library(MuMIn)
library(glmmTMB)
library(performance)
library(car)
library(DHARMa)
library(lawstat)
library(emmeans)
library(multcomp)

# much of the strategy below is from Zurr et al. (2010)  https://doi.org/10.1111/j.2041-210X.2009.00001.x

# OUTLIERS & NORMALITY OF RESPONSE VARIABLES -----------------------------------
ggdensity(SpeciesRichnessDay$Species.Richness.Day, xlab = "Species Richness") # looks great
ggqqplot(SpeciesRichnessDay$Species.Richness.Day, ylab = "Species Richness") # looks great
shapiro.test(SpeciesRichnessDay$Species.Richness.Day) # W = 0.98469, p-value = 0.9854, normal

ggdensity(WindowData$Species.Richness.Window, xlab = "Species Richness") # looks great
ggqqplot(WindowData$Species.Richness.Window, ylab = "Species Richness") # tails stray from normal
shapiro.test(WindowData$Species.Richness.Window) # W = 0.98073, p-value = 0.005175, not normal

# SPECIES RICHNESS MODEL -------------------------------------------------------

# unpacked structure of Time.Window*Day*Site
SRmodel <- glmmTMB(Species.Richness.Window ~ Time.Window + Day + Site + 
                     Time.Window*Day + Day*Site + Time.Window*Site + 
                     Time.Window*Day*Site,
                     data = WindowData, family = "gaussian", REML = F)

#SRmodel <- glmmTMB(Species.Richness.Window ~ Hab2 + Edge.Distance +
#                     (1 | Day/Time.Window) + (1 | Site),
#                   data = WindowData, family = "gaussian", REML = F)

summary(SRmodel)
as.data.frame(confint(SRmodel)) %>% 
  mutate(Estimate = exp(Estimate), `2.5 %` = exp(`2.5 %`), `97.5 %` = exp(`97.5 %`))
performance::r2(SRmodel)
car::Anova(SRmodel, type = 3)

## global species richness model
#SRmodel <- glmmTMB(Species.Richness.Day ~ Day*Site,
#                   data = SpeciesRichnessDay, family = "gaussian", REML = F)
#
#summary(SRmodel)
#as.data.frame(confint(SRmodel)) %>% 
#  mutate(Estimate = exp(Estimate), `2.5 %` = exp(`2.5 %`), `97.5 %` = exp(`97.5 %`))
#performance::r2(SRmodel)
#car::Anova(SRmodel, type = 3)

# CHECKING MODEL ASSUMPTIONS -------------------------------------
# Checking for homogeneity of variance & normality of residuals
mean(residuals(SRmodel)) # VERY close to 0

simulateResiduals(SRmodel, plot = T, refit = F, use.u = T)
shapiro.test(residuals(SRmodel)) # W = 0.99464, p-value = 0.6516, normal!
# residual plots look okay

## Checking for autocorrelation/independence
#acf(SpeciesRichnessWindow$Species.Richness.Window) # raw data is autocorrelated
#acf(residuals(SRmodel)) # random effects variable corrects for this
#runs.test(residuals(SRmodel)) # we do not have autocorrelated data
#
#library(DataCombine)
#SpeciesRichnessWindow.lag <- data.frame(SpeciesRichnessWindow, resid.mod = residuals(SRmodel)) %>% 
#  slide(Var = "resid.mod", NewVar = "lag1", slideBy = -1) %>% 
#  na.omit()
#
#SRmodel.lag <- glmmTMB(Species.Richness.Window ~ Time.Window*Day*Site + lag1,
#                   data = SpeciesRichnessWindow.lag, family = "gaussian", REML = F)
#
#acf(SpeciesRichnessWindow.lag$Species.Richness.Window) # raw data is autocorrelated
#acf(residuals(SRmodel.lag)) # random effects variable corrects for this
#runs.test(residuals(SRmodel.lag)) # we do not have autocorrelated data


# MODEL SELECTION ---------------------------------------------------------
SRmodel <- glmmTMB(Species.Richness.Window ~ Time.Window*Day*Site,
                   data = SpeciesRichnessWindow, family = "gaussian", REML = F)

options(na.action = "na.fail")
# computes marginal and conditional R^2
d.out <- MuMIn::dredge(SRmodel, extra = list("Rsq" = function(x){performance::r2(x)}))
View(d.out)
options(na.action = "na.omit")
write.csv(d.out, "Outputs/sr-model-selection.csv")

# 1st place model by a long-shot (R2 = 0.92, wi = 71%)
topSRmodel <- glmmTMB(Species.Richness.Window ~ Time.Window + Day + Site + 
                     Day*Site + Time.Window*Site,
                   data = WindowData, family = "gaussian", REML = F)

# computing post-hoc comparisons to determine significant differences among the modeled means
# need to switch model to be as.factor(Time.Window) first before performing this comparison
emmeans(topSRmodel, "Time.Window", type = "response") %>% 
  cld(Letter = "abcdefg")

emmeans(topSRmodel, "Day", type = "response") %>% 
  cld(Letter = "abcdefg")

emmeans(topSRmodel, "Site", type = "response") %>% 
  cld(Letter = "abcdefg")
