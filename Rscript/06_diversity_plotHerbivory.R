#----------------------------------------------------------#
#
#
#                      SAR Herbivory
#
#                Diversity vs. herbivory
#
#             Katerina Sam  - Heveakore Maraia
#                         2021
#
#----------------------------------------------------------

## X.1 loda data on diversity and herbivory -----
dataset_diversity <-  
  readxl::read_xlsx("data/input/Diversity_herbivory.xlsx")
summary(dataset_diversity)

dataset_diversity<-subset(dataset_diversity, herb_type == "total")

library(MASS)
library(ggplot2)

herbivory_savanna <- subset(dataset_diversity, habitat  == "savanna")
herbivory_other <- subset(dataset_diversity, habitat  == "forest"|habitat  == "thicket")

plot(dataset_diversity$plot_diversitySP, dataset_diversity$herbivory_cm2, pch = 20, 
     col = ifelse(dataset_diversity$habitat == "savanna",  "red", "blue"),
     xlab = "Diversity",
     ylab = "Herbivory (in cm2)")
abline(lm(herbivory_savanna$herbivory_cm2 ~ herbivory_savanna$plot_diversitySP), col = "red")
abline(lm(herbivory_other$herbivory_cm2 ~ herbivory_other$plot_diversitySP), col = "blue")

legend("topleft", 
       pch = c(19, 19), 
       c("savanna", "other"), 
       col = c("red", "blue"))

cor.test(herbivory_other$herbivory_cm2, herbivory_other$plot_diversitySP, method="pearson")
cor.test(herbivory_savanna$herbivory_cm2, herbivory_savanna$plot_diversitySP, method="pearson")

plot(dataset_diversity$plot_diversityAlpha, dataset_diversity$herbivory_cm2, pch = 20, 
     col = ifelse(dataset_diversity$habitat == "savanna",  "red", "blue"),
     xlab = "Diversity",
     ylab = "Herbivory (in cm2)")
abline(lm(herbivory_savanna$herbivory_cm2 ~ herbivory_savanna$plot_diversityAlpha), col = "red")
abline(lm(herbivory_other$herbivory_cm2 ~ herbivory_other$plot_diversityAlpha), col = "blue")

cor.test(herbivory_other$herbivory_cm2, herbivory_other$plot_diversityAlpha, method="pearson")
cor.test(herbivory_savanna$herbivory_cm2, herbivory_savanna$plot_diversityAlpha, method="pearson")

#### this needs to be standardized - it is not correct now -----
##  ACA_KAR
plot(dataset_diversity$plot_diversityAlpha, dataset_diversity$ACA_KAR, pch = 20, 
     col = ifelse(dataset_diversity$habitat == "savanna",  "red", "blue"),
     xlab = "Diversity",
     ylab = "Herbivory (in cm2)")
abline(lm(herbivory_savanna$ACA_KAR ~ herbivory_savanna$plot_diversityAlpha), col = "red")
abline(lm(herbivory_other$ACA_KAR ~ herbivory_other$plot_diversityAlpha), col = "blue")

legend("topleft", 
       pch = c(19, 19), 
       c("savanna", "other"), 
       col = c("red", "blue"))

cor.test(herbivory_other$ACA_KAR, herbivory_other$plot_diversitySP, method="pearson")
cor.test(herbivory_savanna$ACA_KAR, herbivory_savanna$plot_diversitySP, method="pearson")

##  DIC_CIN
plot(dataset_diversity$plot_diversityAlpha, dataset_diversity$DIC_CIN, pch = 20, 
     col = ifelse(dataset_diversity$habitat == "savanna",  "red", "blue"),
     xlab = "Diversity",
     ylab = "Herbivory (in cm2)")
abline(lm(herbivory_savanna$DIC_CIN ~ herbivory_savanna$plot_diversityAlpha), col = "red")
abline(lm(herbivory_other$DIC_CIN ~ herbivory_other$plot_diversityAlpha), col = "blue")

legend("topleft", 
       pch = c(19, 19), 
       c("savanna", "other"), 
       col = c("red", "blue"))

cor.test(herbivory_other$DIC_CIN, herbivory_other$plot_diversitySP, method="pearson")
cor.test(herbivory_savanna$DIC_CIN, herbivory_savanna$plot_diversitySP, method="pearson")
