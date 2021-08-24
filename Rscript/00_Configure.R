#----------------------------------------------------------#
#
#
#                      SAR Herbivory
#
#                       Config file
#
#             Katerina Sam  - Heveakore Maraia
#                         2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0.1. Load libraries and functions -----
#----------------------------------------------------------#

# delete existing workspace to start clean
rm(list = ls())

# Package version control
library(renv)
# renv::init()
# renv::snapshot(lockfile = "data/lock/revn.lock")
renv::restore(lockfile = "data/lock/revn.lock")

# libraries
library(tidyverse)
library(readr)
library(tidyr)
library(ggpubr)
library(RColorBrewer)
library(MuMIn)
library(emmeans)
library(performance)
library(glmmTMB)
library(ggplot2)
library(dplyr)
library(see)
library(qqplotr)
library(randomForest)
library(betareg)
library(bbmle)
library(buildmer)
library(lme4)
library(lmerTest)
library(reshape2)
library(corrplot)
library(gridExtra)
library(MASS)

#----------------------------------------------------------#
# 0.2. Import data -----
#----------------------------------------------------------#
dataset_herbivory <-  
  readxl::read_xlsx("data/input/Herbivory_raw_final_20210609.xlsx")
summary(dataset_herbivory)

#----------------------------------------------------------#
# 0.1 total herbivory data summarization and data preparation -----
#----------------------------------------------------------#
# calculate means of leaf area, total herbivory etc per tree individuals
dataset_herbivory_sum <-
  dataset_herbivory %>% 
  mutate(
    tot_pct) %>%  # recalculate 
  group_by(species, site, leaf_age, side, habitat, individual) %>% 
  summarize(
    .groups = "keep",
    #  leaf_area_total = sum(LeafAreaIdeal)/10e3,
    mean_leaf_area = mean(leaf_area), 
    mean_total_herbivory = mean(total_herbivory)
  )
summary(dataset_herbivory_sum)

# standardize the total leaf damage per 100 cm2 so it can be compared between plant species
dataset_herbivory_sum$total_herbivory_standardized <- (100/dataset_herbivory_sum$mean_leaf_area)*dataset_herbivory_sum$mean_total_herbivory

write_csv(
  dataset_herbivory_sum,
  "data/output/01_dataset_herbivory_sum.csv")

#----------------------------------------------------------#
# 0.2 herbivory data summarization per type of herbivory -----
#----------------------------------------------------------#
# prepare the long format of the dataset form the raw data
dataset_long<- gather(dataset_herbivory, herbivory_type, herbivory, sap_miner_herbivory, chew_herbivory, factor_key=TRUE)
dataset_long

# save the long format of the data
write_csv(
  dataset_long,
  "data/output/01a_dataset_herbivory_long.csv")

# by this, you calculate mean herbivory per plant species in individual sites
dataset_herbtype_sum <-
  dataset_long %>% 
  mutate(
    herbivory) %>%  # recalculate 
  group_by(species, site, leaf_age, herbivory_type, side, habitat, individual) %>% 
  summarize(
    .groups = "keep",
    #  leaf_area_total = sum(LeafAreaIdeal)/10e3,
    mean_herbivory = mean(herbivory),
    mean_leaf_area = mean(leaf_area) 
)
summary(dataset_herbtype_sum)

# standardize the leaf damage per 100 cm2 so it can be compared between plant species and at the same time,
# it expresses herbivory in % per plant individual (but not per individual leaves)
dataset_herbtype_sum$herbivory_standardized <- (100/dataset_herbtype_sum$mean_leaf_area)*dataset_herbtype_sum$mean_herbivory


write_csv(
  dataset_herbtype_sum,
  "data/output/02_dataset_herbtype_sum.csv")


#----------------------------------------------------------#
# 0.3. graphical properties definition  -----
#----------------------------------------------------------#

theme_set(theme_classic())
text_size <-  22

PDF_width <-  10
PDF_height <-  6

# get the flat violin geom
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

