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
# 1. Load libraries and functions -----
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

#----------------------------------------------------------#
# 2. Import data -----
#----------------------------------------------------------#

# 1.1 herbivory data -----
dataset_herbivory <-  
  readxl::read_xlsx("data/input/herbivory_raw_final_20210609.xlsx")

# sum herbivory per tree in percentages
dataset_herbivory_sum <-
  dataset_herbivory %>% 
  mutate(
    tot_pct) %>%  # recalculate 
  group_by(species, site, leaf_age, side, habitat, individual) %>% 
  summarize(
    .groups = "keep",
    #  leaf_area_total = sum(LeafAreaIdeal)/10e3,
    herbivory_percentage_median = median(tot_pct),
    herbivory_percentage_mean = mean(tot_pct)
  )
summary(dataset_herbivory_sum)

write_csv(
  dataset_herbivory_sum,
  "data/output/dataset_herbivory_sum.csv")


#----------------------------------------------------------#
# 3. graphical properties definition  -----
#----------------------------------------------------------#

theme_set(theme_classic())
text_size <-  22

PDF_width <-  10
PDF_height <-  6

# get the flat violin geom
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

