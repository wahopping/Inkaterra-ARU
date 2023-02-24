
# Chris Sayers
# updated February 22, 2023

#---------------------- LOADING/MERGING THE DATA -------------------------------
library(tidyverse)
library(dplyr)

# pulling in habitat information and community metrics calculated in Python
SiteData <- read.csv("Spreadsheets/Site-Data.csv") %>%
  dplyr::select(Site, Day, Hab1, Hab2, Hab3, Edge.Distance) %>% 
  mutate(Day = as.factor(Day), Site = as.factor(Site))

# pulling in raw annotations from Raven Pro 1.5
Annotations <- read.csv("Spreadsheets/All-Annotations.csv") %>% 
  rename(Start = Begin.Time..s., End = End.Time..s., Species = species) %>% 
  mutate(Day = as.factor(Day), Site = as.factor(Site))
 
# SPECIES RICHNESS --------------------------------------------------------

# calculating species richness at each 1-minute interval between 0600-0700
#for(i in nrow(Annotations)) {
#  
#  for(j in seq(0:60)) {
#    
#    if(Start >= (60*j) & End < (60*(j+1))){Annotations$SR.1 <- 1} # annotation lies completely within window
#    else if(Start < (60*j) & End >= (60*j) & End < (60*(j+1))){Annotations$SR.1 <- } # annotation only intersects lower window bound
#    else if(Start >= (60*j) & Start < (60*(j+1)) & End >= (60*(j+1))){Annotations$SR.1 <- } # annotation only intersects upper window bound
#    else if(Start <= (60*j) & Start >= (60*(j+1))){Annotations$SR.1 <- }
#    else {Annotations$SR.1 <- 0} # annotation does not overlap the window
#      
#    }
#  }
#}





# calculating species richness at each 5-minute interval between 0600-0700
SR.Annotations.5 <- Annotations %>% 
  mutate(Time.Window.Start = if_else(Start >= (300*0) & Start < (300*1), 1,
                             if_else(Start >= (300*1) & Start < (300*2), 2,
                             if_else(Start >= (300*2) & Start < (300*3), 3,
                             if_else(Start >= (300*3) & Start < (300*4), 4,
                             if_else(Start >= (300*4) & Start < (300*5), 5,
                             if_else(Start >= (300*5) & Start < (300*6), 6,
                             if_else(Start >= (300*6) & Start < (300*7), 7,
                             if_else(Start >= (300*7) & Start < (300*8), 8,
                             if_else(Start >= (300*8) & Start < (300*9), 9,
                             if_else(Start >= (300*9) & Start < (300*10), 10,
                             if_else(Start >= (300*10) & Start < (300*11), 11, 12)))))))))))) %>%
  mutate(Time.Window.End = if_else(End >= (300*0) & End < (300*1), 1,
                           if_else(End >= (300*1) & End < (300*2), 2,
                           if_else(End >= (300*2) & End < (300*3), 3,
                           if_else(End >= (300*3) & End < (300*4), 4,
                           if_else(End >= (300*4) & End < (300*5), 5,
                           if_else(End >= (300*5) & End < (300*6), 6,
                           if_else(End >= (300*6) & End < (300*7), 7,
                           if_else(End >= (300*7) & End < (300*8), 8,
                           if_else(End >= (300*8) & End < (300*9), 9,
                           if_else(End >= (300*9) & End < (300*10), 10,
                           if_else(End >= (300*10) & End < (300*11), 11, 12)))))))))))) %>% 
  pivot_longer(c(Time.Window.Start, Time.Window.End), names_to = "Time.Window.Label", values_to = "Time.Window")

SR.Day <- Annotations %>% 
  # excluding individuals that were not identified with 100% confidence
  filter(exclusion.code <= 3) %>% 
  # calculating species richness per day per site
  group_by(Site, Day) %>% 
  summarize(Species.Richness.Day = length(unique(species)))

SR.Window.5 <- SR.Annotations.5 %>% 
  # excluding individuals that were not identified with 100% confidence
  filter(exclusion.code <= 3) %>% 
  # calculating species richness per time window per day per site
  group_by(Site, Day, Time.Window) %>% 
  summarize(Species.Richness.Window = length(unique(species)))

# putting everything together
SR.Window.5 <- left_join(SR.Window.5, SiteData, by = c("Site", "Day"))

# calculating species richness at each 1-minute interval between 0600-0700
#SR.Annotations.1 <- Annotations %>% 
#  mutate(Time.Window.Start = if_else(Start >= (60*0) & Start < (60*1), 1,
#                             if_else(Start >= (60*1) & Start < (60*2), 2,
#                             if_else(Start >= (60*2) & Start < (60*3), 3,
#                             if_else(Start >= (60*3) & Start < (60*4), 4,
#                             if_else(Start >= (60*4) & Start < (60*5), 5,
#                             if_else(Start >= (60*5) & Start < (60*6), 6,
#                             if_else(Start >= (60*6) & Start < (60*7), 7,
#                             if_else(Start >= (60*7) & Start < (60*8), 8,
#                             if_else(Start >= (60*8) & Start < (60*9), 9,
#                             if_else(Start >= (60*9) & Start < (60*10), 10,
#                             if_else(Start >= (60*10) & Start < (60*11), 11,
#                             if_else(Start >= (60*11) & Start < (60*12), 12,
#                             if_else(Start >= (60*12) & Start < (60*13), 13,
#                             if_else(Start >= (60*13) & Start < (60*14), 14,
#                             if_else(Start >= (60*14) & Start < (60*15), 15,
#                             if_else(Start >= (60*15) & Start < (60*16), 16,
#                             if_else(Start >= (60*16) & Start < (60*17), 17,
#                             if_else(Start >= (60*17) & Start < (60*18), 18,
#                             if_else(Start >= (60*18) & Start < (60*19), 19,
#                             if_else(Start >= (60*19) & Start < (60*20), 20,
#                             if_else(Start >= (60*20) & Start < (60*21), 21,
#                             if_else(Start >= (60*21) & Start < (60*22), 22,
#                             if_else(Start >= (60*22) & Start < (60*23), 23,
#                             if_else(Start >= (60*23) & Start < (60*24), 24,
#                             if_else(Start >= (60*24) & Start < (60*25), 25,
#                             if_else(Start >= (60*25) & Start < (60*26), 26,
#                             if_else(Start >= (60*26) & Start < (60*27), 27,
#                             if_else(Start >= (60*27) & Start < (60*28), 28,
#                             if_else(Start >= (60*28) & Start < (60*29), 29,
#                             if_else(Start >= (60*29) & Start < (60*30), 30,
#                             if_else(Start >= (60*30) & Start < (60*31), 31,
#                             if_else(Start >= (60*31) & Start < (60*32), 32,
#                             if_else(Start >= (60*32) & Start < (60*33), 33,
#                             if_else(Start >= (60*33) & Start < (60*34), 34,
#                             if_else(Start >= (60*34) & Start < (60*35), 35,
#                             if_else(Start >= (60*35) & Start < (60*36), 36,
#                             if_else(Start >= (60*36) & Start < (60*37), 37,
#                             if_else(Start >= (60*37) & Start < (60*38), 38,
#                             if_else(Start >= (60*38) & Start < (60*39), 39,
#                             if_else(Start >= (60*39) & Start < (60*40), 40,
#                             if_else(Start >= (60*40) & Start < (60*41), 41,
#                             if_else(Start >= (60*41) & Start < (60*42), 42,
#                             if_else(Start >= (60*42) & Start < (60*43), 43,
#                             if_else(Start >= (60*43) & Start < (60*44), 44,
#                             if_else(Start >= (60*44) & Start < (60*45), 45,
#                             if_else(Start >= (60*45) & Start < (60*46), 46,
#                             if_else(Start >= (60*46) & Start < (60*47), 47,
#                             if_else(Start >= (60*47) & Start < (60*48), 48,
#                             if_else(Start >= (60*48) & Start < (60*49), 49,
#                             if_else(Start >= (60*49) & Start < (60*50), 50,
#                             if_else(Start >= (60*50) & Start < (60*51), 51,
#                             if_else(Start >= (60*51) & Start < (60*52), 52,
#                             if_else(Start >= (60*52) & Start < (60*53), 53,
#                             if_else(Start >= (60*53) & Start < (60*54), 54,
#                             if_else(Start >= (60*54) & Start < (60*55), 55,
#                             if_else(Start >= (60*55) & Start < (60*56), 56,
#                             if_else(Start >= (60*56) & Start < (60*57), 57,
#                             if_else(Start >= (60*57) & Start < (60*58), 58,
#                             if_else(Start >= (60*58) & Start < (60*59), 59, 60)
#                             ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) %>%
#  mutate(Time.Window.End = if_else(End >= (60*0) & End < (60*1), 1,
#                           if_else(End >= (60*1) & End < (60*2), 2,
#                           if_else(End >= (60*2) & End < (60*3), 3,
#                           if_else(End >= (60*3) & End < (60*4), 4,
#                           if_else(End >= (60*4) & End < (60*5), 5,
#                           if_else(End >= (60*5) & End < (60*6), 6,
#                           if_else(End >= (60*6) & End < (60*7), 7,
#                           if_else(End >= (60*7) & End < (60*8), 8,
#                           if_else(End >= (60*8) & End < (60*9), 9,
#                           if_else(End >= (60*9) & End < (60*10), 10,
#                           if_else(End >= (60*10) & End < (60*11), 11,
#                           if_else(End >= (60*11) & End < (60*12), 12,
#                           if_else(End >= (60*12) & End < (60*13), 13,
#                           if_else(End >= (60*13) & End < (60*14), 14,
#                           if_else(End >= (60*14) & End < (60*15), 15,
#                           if_else(End >= (60*15) & End < (60*16), 16,
#                           if_else(End >= (60*16) & End < (60*17), 17,
#                           if_else(End >= (60*17) & End < (60*18), 18,
#                           if_else(End >= (60*18) & End < (60*19), 19,
#                           if_else(End >= (60*19) & End < (60*20), 20,
#                           if_else(End >= (60*20) & End < (60*21), 21,
#                           if_else(End >= (60*21) & End < (60*22), 22,
#                           if_else(End >= (60*22) & End < (60*23), 23,
#                           if_else(End >= (60*23) & End < (60*24), 24,
#                           if_else(End >= (60*24) & End < (60*25), 25,
#                           if_else(End >= (60*25) & End < (60*26), 26,
#                           if_else(End >= (60*26) & End < (60*27), 27,
#                           if_else(End >= (60*27) & End < (60*28), 28,
#                           if_else(End >= (60*28) & End < (60*29), 29,
#                           if_else(End >= (60*29) & End < (60*30), 30,
#                           if_else(End >= (60*30) & End < (60*31), 31,
#                           if_else(End >= (60*31) & End < (60*32), 32,
#                           if_else(End >= (60*32) & End < (60*33), 33,
#                           if_else(End >= (60*33) & End < (60*34), 34,
#                           if_else(End >= (60*34) & End < (60*35), 35,
#                           if_else(End >= (60*35) & End < (60*36), 36,
#                           if_else(End >= (60*36) & End < (60*37), 37,
#                           if_else(End >= (60*37) & End < (60*38), 38,
#                           if_else(End >= (60*38) & End < (60*39), 39,
#                           if_else(End >= (60*39) & End < (60*40), 40,
#                           if_else(End >= (60*40) & End < (60*41), 41,
#                           if_else(End >= (60*41) & End < (60*42), 42,
#                           if_else(End >= (60*42) & End < (60*43), 43,
#                           if_else(End >= (60*43) & End < (60*44), 44,
#                           if_else(End >= (60*44) & End < (60*45), 45,
#                           if_else(End >= (60*45) & End < (60*46), 46,
#                           if_else(End >= (60*46) & End < (60*47), 47,
#                           if_else(End >= (60*47) & End < (60*48), 48,
#                           if_else(End >= (60*48) & End < (60*49), 49,
#                           if_else(End >= (60*49) & End < (60*50), 50,
#                           if_else(End >= (60*50) & End < (60*51), 51,
#                           if_else(End >= (60*51) & End < (60*52), 52,
#                           if_else(End >= (60*52) & End < (60*53), 53,
#                           if_else(End >= (60*53) & End < (60*54), 54,
#                           if_else(End >= (60*54) & End < (60*55), 55,
#                           if_else(End >= (60*55) & End < (60*56), 56,
#                           if_else(End >= (60*56) & End < (60*57), 57,
#                           if_else(End >= (60*57) & End < (60*58), 58,
#                           if_else(End >= (60*58) & End < (60*59), 59, 60)
#                           ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) %>%
#  pivot_longer(c(Time.Window.Start, Time.Window.End),
#               names_to = "Time.Window.Label", values_to = "Time.Window")
#
#
#
#
#SpeciesRichnessDay.1 <- SR.Annotations.1 %>% 
#  # excluding individuals that were not identified with 100% confidence
#  filter(exclusion.code <= 3) %>% 
#  # calculating species richness per day per site
#  group_by(Site, Day) %>% 
#  summarize(Species.Richness.Day = length(unique(species)))
#
#SpeciesRichnessWindow.1 <- SR.Annotations.1 %>% 
#  # excluding individuals that were not identified with 100% confidence
#  filter(exclusion.code <= 3) %>% 
#  # calculating species richness per time window per day per site
#  group_by(Site, Day, Time.Window) %>% 
#  summarize(Species.Richness.Window = length(unique(species)))
#
## putting everything together
#WindowData.1 <- left_join(SpeciesRichnessWindow.1, SiteData, by = c("Site", "Day"))
  
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
ggdensity(SR.Day$Species.Richness.Day, xlab = "Species Richness") # looks great
ggqqplot(SR.Day$Species.Richness.Day, ylab = "Species Richness") # looks great
shapiro.test(SR.Day$Species.Richness.Day) # W = 0.98469, p-value = 0.9854, normal

ggdensity(SR.Window.5$Species.Richness.Window, xlab = "Species Richness") # looks great
ggqqplot(SR.Window.5$Species.Richness.Window, ylab = "Species Richness") # tails stray from normal
shapiro.test(SR.Window.5$Species.Richness.Window) # W = 0.98073, p-value = 0.005175, not normal

# SPECIES RICHNESS MODEL -------------------------------------------------------

# unpacked structure of Time.Window*Day*Site
SRmodel <- glmmTMB(Species.Richness.Window ~ Time.Window + Day + Site + 
                     Time.Window*Day + Day*Site + Time.Window*Site + 
                     Time.Window*Day*Site,
                     data = WindowData, family = "gaussian", REML = F)

SRmodel <- glmmTMB(Species.Richness.Window ~ Time.Window*Day*Hab2 + (1 | Site),
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

summary(topSRmodel)
as.data.frame(confint(topSRmodel)) %>% 
  mutate(Estimate = exp(Estimate), `2.5 %` = exp(`2.5 %`), `97.5 %` = exp(`97.5 %`))
performance::r2(topSRmodel)
car::Anova(topSRmodel, type = 3)

# computing post-hoc comparisons to determine significant differences among the modeled means
# need to switch model to be as.factor(Time.Window) first before performing this comparison
emmeans(topSRmodel, "Time.Window", type = "response") %>% 
  cld(Letter = "abcdefg")

emmeans(topSRmodel, "Day", type = "response") %>% 
  cld(Letter = "abcdefg")

emmeans(topSRmodel, "Site", type = "response") %>% 
  cld(Letter = "abcdefg")

# VOCAL PRESENCE --------------------------------------------------------

# calculating vocal presence for each species at 15 s time windows between 0600-0700

# there are a total of 5 ways that an annotation can interact with a time window:
# (1) the annotation lies completely within the window
# (2) the annotation only intersects the lower window bound
# (3) the annotation only intersects the upper window bound
# (4) the annotation completely overlaps the window
# (5) the annotation lies completely outside the window

# defining the bounds of our windows and recording length
window.start <- 0
window.end <- 15
max.time <- 3600

while(window.start < max.time) {
  
  # create a new column in Annotations to populate with vocal presence/abscence
  Annotations[, ncol(Annotations) + 1] <- NA
  names(Annotations)[ncol(Annotations)] <- paste0("Time.Window.", window.end)
  
  for(i in nrow(Annotations)) {
    if(((Annotations$Start[i] >= window.start) & (Annotations$End[i] < window.end)) | # annotation lies completely within window
       ((Annotations$Start[i] < window.start) & (Annotations$End[i] >= window.start) & (Annotations$End[i] < window.end)) | # annotation only intersects lower window bound
       ((Annotations$Start[i] >= window.start) & (Annotations$Start[i] < window.end) & (Annotations$End[i] >= window.end)) | #annotation only intersects upper window bound
       ((Annotations$Start[i] < window.start) & (Annotations$End[i] >= window.end))) # annotation completely overlaps window
      {Annotations[i, ncol(Annotations)] <- 1}
    else {Annotations[i, ncol(Annotations)] <- 0}
  }
  # adjusting our counter
  window.start <- window.start + 15
  window.end <- window.end + 15
}





# calculating species richness at each 1-minute interval between 0600-0700
#for(i in nrow(Annotations)) {
#  
#  for(j in seq(0:60)) {
#    
#    if(Start >= (60*j) & End < (60*(j+1))){Annotations$SR.1 <- 1} # annotation lies completely within window
#    else if(Start < (60*j) & End >= (60*j) & End < (60*(j+1))){Annotations$SR.1 <- } # annotation only intersects lower window bound
#    else if(Start >= (60*j) & Start < (60*(j+1)) & End >= (60*(j+1))){Annotations$SR.1 <- } # annotation only intersects upper window bound
#    else if(Start <= (60*j) & Start >= (60*(j+1))){Annotations$SR.1 <- }
#    else {Annotations$SR.1 <- 0} # annotation does not overlap the window
#      
#    }
#  }
#}




# calculating vocal prevalence for each species at each 5-minute interval between 0600-0700
# there are a total of 5 ways that an annotation can interact with a 60s time window:
# (1) the annotation lies completely within the window
# (2) the annotation only intersects the lower window bound
# (3) the annotation only intersects the upper window bound
# (4) the annotation completely overlaps the window
# (5) the annotation lies completely outside the window

VP.Annotations.5 <- Annotations %>%
  mutate(VP.1 = if_else(End < (300*1), End - Start, # annotation lies completely within window
                        if_else(Start < (300*1) & End >= (300*1), (300*1) - Start, 0))) %>% # annotation only intersects upper window bound
  mutate(VP.2 = if_else(Start >= (300*1) & End < (300*2), End - Start, # annotation lies completely within window
                        if_else(Start < (300*1) & End >= (300*1) & End < (300*2), End - (300*1), # annotation only intersects lower window bound
                                if_else(Start >= (300*1) & Start < (300*2) & End >= (300*2), (300*2) - Start, # annotation only intersects upper window bound
                                        if_else(Start <= (300*1) & Start >= (300*2), 300, 0))))) %>% # annotation completely overlaps window
  mutate(VP.3 = if_else(Start >= (300*2) & End < (300*3), End - Start,
                        if_else(Start < (300*2) & End >= (300*2) & End < (300*3), End - (300*2),
                                if_else(Start >= (300*2) & Start < (300*3) & End >= (300*3), (300*3) - Start,
                                        if_else(Start <= (300*2) & Start >= (300*3), 300, 0))))) %>%
  mutate(VP.4 = if_else(Start >= (300*3) & End < (300*4), End - Start,
                        if_else(Start < (300*3) & End >= (300*3) & End < (300*4), End - (300*3),
                                if_else(Start >= (300*3) & Start < (300*4) & End >= (300*4), (300*4) - Start,
                                        if_else(Start <= (300*3) & Start >= (300*4), 300, 0))))) %>%
  mutate(VP.5 = if_else(Start >= (300*4) & End < (300*5), End - Start,
                        if_else(Start < (300*4) & End >= (300*4) & End < (300*5), End - (300*4),
                                if_else(Start >= (300*4) & Start < (300*5) & End >= (300*5), (300*5) - Start,
                                        if_else(Start <= (300*4) & Start >= (300*5), 300, 0))))) %>%
  mutate(VP.6 = if_else(Start >= (300*5) & End < (300*6), End - Start,
                        if_else(Start < (300*5) & End >= (300*5) & End < (300*6), End - (300*5),
                                if_else(Start >= (300*5) & Start < (300*6) & End >= (300*6), (300*6) - Start,
                                        if_else(Start <= (300*5) & Start >= (300*6), 300, 0))))) %>%
  mutate(VP.7 = if_else(Start >= (300*6) & End < (300*7), End - Start,
                        if_else(Start < (300*6) & End >= (300*6) & End < (300*7), End - (300*6),
                                if_else(Start >= (300*6) & Start < (300*7) & End >= (300*7), (300*7) - Start,
                                        if_else(Start <= (300*6) & Start >= (300*7), 300, 0))))) %>%
  mutate(VP.8 = if_else(Start >= (300*7) & End < (300*8), End - Start,
                        if_else(Start < (300*7) & End >= (300*7) & End < (300*8), End - (300*7),
                                if_else(Start >= (300*7) & Start < (300*8) & End >= (300*8), (300*8) - Start,
                                        if_else(Start <= (300*7) & Start >= (300*8), 300, 0))))) %>%
  mutate(VP.9 = if_else(Start >= (300*8) & End < (300*9), End - Start,
                        if_else(Start < (300*8) & End >= (300*8) & End < (300*9), End - (300*8),
                                if_else(Start >= (300*8) & Start < (300*9) & End >= (300*9), (300*9) - Start,
                                        if_else(Start <= (300*8) & Start >= (300*9), 300, 0))))) %>%
  mutate(VP.10 = if_else(Start >= (300*9) & End < (300*10), End - Start,
                         if_else(Start < (300*9) & End >= (300*9) & End < (300*10), End - (300*9),
                                 if_else(Start >= (300*9) & Start < (300*10) & End >= (300*10), (300*10) - Start,
                                         if_else(Start <= (300*9) & Start >= (300*10), 300, 0))))) %>%
  mutate(VP.11 = if_else(Start >= (300*10) & End < (300*11), End - Start,
                         if_else(Start < (300*10) & End >= (300*10) & End < (300*11), End - (300*10),
                                 if_else(Start >= (300*10) & Start < (300*11) & End >= (300*11), (300*11) - Start,
                                         if_else(Start <= (300*10) & Start >= (300*11), 300, 0))))) %>%
  mutate(VP.12 = if_else(Start >= (300*11) & End < (300*12), End - Start,
                         if_else(Start < (300*11) & End >= (300*11) & End < (300*11), End - (300*11),
                                 if_else(Start >= (300*11) & Start < (300*12) & End >= (300*12), (300*12) - Start,
                                         if_else(Start <= (300*11) & Start >= (300*12), 300, 0))))) %>% 
  pivot_longer(c(VP.1, VP.2, VP.3, VP.4, VP.5, VP.6, VP.7, VP.8, VP.9, VP.10, VP.11, VP.12),
               names_to = "Time.Window", values_to = "VP") %>% 
  mutate(Time.Window = if_else(Time.Window == "VP.1", 1,
                               if_else(Time.Window == "VP.2", 2,
                                       if_else(Time.Window == "VP.3", 3,
                                               if_else(Time.Window == "VP.4", 4,
                                                       if_else(Time.Window == "VP.5", 5,
                                                               if_else(Time.Window == "VP.6", 6,
                                                                       if_else(Time.Window == "VP.7", 7,
                                                                               if_else(Time.Window == "VP.8", 8,
                                                                                       if_else(Time.Window == "VP.9", 9,
                                                                                               if_else(Time.Window == "VP.10", 10,
                                                                                                       if_else(Time.Window == "VP.11", 11, 12))))))))))))

VP.Day <- Annotations %>% 
  # excluding individuals that were not identified with 100% confidence
  filter(exclusion.code <= 3) %>% 
  mutate(VP = End - Start) %>% 
  # calculating species richness per day per site
  group_by(Site, Day, Species) %>% 
  summarize(VP = sum(VP))

VP.Window.5 <- VP.Annotations.5 %>% 
  # excluding individuals that were not identified with 100% confidence
  filter(exclusion.code <= 3) %>% 
  # calculating species richness per time window per day per site
  group_by(Site, Day, Time.Window, Species) %>% 
  summarize(VP = round(sum(VP))) # temporarily making these integer values for poisson distribution

# putting everything together
VP.Window.5 <- left_join(VP.Window.5, SiteData, by = c("Site", "Day"))

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
# This distribution is to be expected when dealing with count data/ many zero counts
ggdensity(VP.Day$VP, xlab = "Vocal Prevalence") # looks horrible
ggqqplot(VP.Day$VP, ylab = "Vocal Prevalence") # looks horrible
shapiro.test(VP.Day$VP) # W = 0.51073, p-value < 2.2e-16, not normal

ggdensity(VP.Window.5$VP, xlab = "Vocal Prevalence") # looks horrible
ggqqplot(VP.Window.5$VP, ylab = "Vocal Prevalence") # tails stray from normal

# VOCAL PREVALENCE MODEL -------------------------------------------------------

# can't get this to converge
#VPmodel <- glmmTMB(VP ~ Time.Window*Day*Site + (1 | Species),
#                   zi = ~ Time.Window*Day*Site,
#                   data = VP.Window.5, family = truncated_poisson, REML = F)

VPmodel <- glmmTMB(VP ~ Time.Window + Day + Site + (1 | Species),
                   zi = ~ Time.Window + Day + Site,
                   data = VP.Window.5, family = truncated_poisson, REML = F)

VPmodel <- glmmTMB(VP ~ Time.Window + Day + Hab3 + (1 | Species) + (1 | Site),
                   zi = ~ Time.Window + Day + Hab3,
                   data = VP.Window.5, family = truncated_poisson, REML = F)

#SRmodel <- glmmTMB(Species.Richness.Window ~ Hab2 + Edge.Distance +
#                     (1 | Day/Time.Window) + (1 | Site),
#                   data = WindowData, family = "gaussian", REML = F)

summary(VPmodel)
as.data.frame(confint(VPmodel)) %>% 
  mutate(Estimate = exp(Estimate), `2.5 %` = exp(`2.5 %`), `97.5 %` = exp(`97.5 %`))
performance::r2(VPmodel)
car::Anova(VPmodel, type = 3)

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
mean(residuals(VPmodel)) # VERY close to 0

simulateResiduals(VPmodel, plot = T, refit = F, use.u = T)
runs.test(residuals(VPmodel)) # W = 0.99464, p-value = 0.6516, normal!
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

summary(topSRmodel)
as.data.frame(confint(topSRmodel)) %>% 
  mutate(Estimate = exp(Estimate), `2.5 %` = exp(`2.5 %`), `97.5 %` = exp(`97.5 %`))
performance::r2(topSRmodel)
car::Anova(topSRmodel, type = 3)

# computing post-hoc comparisons to determine significant differences among the modeled means
# need to switch model to be as.factor(Time.Window) first before performing this comparison
emmeans(topSRmodel, "Time.Window", type = "response") %>% 
  cld(Letter = "abcdefg")

emmeans(topSRmodel, "Day", type = "response") %>% 
  cld(Letter = "abcdefg")

emmeans(topSRmodel, "Site", type = "response") %>% 
  cld(Letter = "abcdefg")
