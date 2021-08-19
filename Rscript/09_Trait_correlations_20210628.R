#------------------------------------------------------------------------------------------------------


#                                           Kore and Katka


#-----------------------------------------------------------------------------------------------------
#
#                              MULTICOLLINEAR MODELS
#
#-----------------------------------------------------------------------------------------------------
#
#
#    1. MULTICOLLINEARITY FOR TOTAL HERBIVORY
#
View(South.Africa.data)
head(South.Africa.data)
#
#---------
#  Subsetting of Data for Total Herbivory (TH)
# -------
TH <- South.Africa.data[ , -c(2:12, 14:16, 18, 20, 23, 26, 28:31)]
#----
# Install packages
#----
library(tidyverse)
install.packages("Stat2Data")
library(Stat2Data)
install.packages("skimr")
library(skimr)
install.packages("GGally")
library(GGally)
#------------
# Use the ggpairs() function to visualize the collinearity of traits in the scatter plots
#------------
ggpairs(TH)


#------------------------------
# 1. MULTICOLLINEARITY FOR CHEWING HERBIVORY
#-----------------------------

View(South.Africa.data)
head(South.Africa.data)
#
#---------
#  Subsetting of Data for Total Herbivory (TH)
# -------
CH <- South.Africa.data[ , -c(1,3:12, 14:16, 18, 20, 23, 26, 28:31)]
#----
# Install packages
#----
library(tidyverse)
install.packages("Stat2Data")
library(Stat2Data)
install.packages("skimr")
library(skimr)
install.packages("GGally")
library(GGally)
#------------
# Use the ggpairs() function to visualize the collinearity of traits in the scatter plots
#------------
ggpairs(TH, lower = list(continuous = wrap(lowerfun)))

ggpairs(TH[, c("total_perc_mean", "BitSizeIndex", "C", "P_Elsenburg", "CN_ratio", "LDMC_leaf", "SLA", "FreshW")],lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue"))

# -----------------------------
# 3. Correlations for mining herbivory 
#-----------------------------
