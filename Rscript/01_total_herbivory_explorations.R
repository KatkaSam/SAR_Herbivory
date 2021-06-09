#----------------------------------------------------------#
#
#
#            BABE exclosure experiment TOMAKOMAI
#
#             Leaf herbivory main experiment
#
#             Katerina Sam  - Marketa Tahadlova
#                         2021
#
#----------------------------------------------------------#

source("Rscripts/00_Configure.R")

dataset_herbivory <-  
  readxl::read_xlsx("data/input/herbivory_raw_final_20210609.xlsx")

#----------------------------------------------------------#
# 3. Total leaf herbivory per tree individual -----
#----------------------------------------------------------#

# per habitat
(ext_plot_01 <- 
   dataset_herbivory_sum %>% 
  ggplot(
    aes(
      x = habitat,
      y = herbivory_percentage_mean)) +
  
  geom_flat_violin(
    col = "gray30",
    alpha = 1/2,
    trim = TRUE,
    position = position_nudge(
      x = 0.2,
      y = 0)) +
  
  geom_point(
    position = position_jitter(width = 0.15),
    alpha = 1,
    size = 1) +
  
  geom_boxplot(
    width=0.2,
    outlier.shape = NA,
    col = "gray30",
    alpha = 0.5) +
  
  labs(
    x = "Habitat", 
    y = expression(paste("Herbivory per individual (%)"))) +
  theme(
    text = element_text(size = text_size),
    legend.position = "none"))

ggsave(
  "figures/total_herbivory/explor_plot_01_habitats.pdf",
  ext_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

#----------------------------------------------------------#
# per plant species
(ext_plot_02 <- 
    dataset_herbivory_sum %>% 
    ggplot(
      aes(
        x = species,
        y = herbivory_percentage_mean)) +
    
    geom_flat_violin(
      col = "gray30",
      alpha = 1/2,
      trim = TRUE,
      position = position_nudge(
        x = 0.2,
        y = 0)) +
    
    geom_point(
      position = position_jitter(width = 0.15),
      alpha = 1,
      size = 1) +
    
    geom_boxplot(
      width=0.2,
      outlier.shape = NA,
      col = "gray30",
      alpha = 0.5) +
    
    labs(
      x = "Plant species", 
      y = expression(paste("Herbivory per individual (%)"))) +
    theme(
      text = element_text(size = text_size, angle = 90, vjust = 0.5),
      legend.position = "none"))

ggsave(
  "figures/total_herbivory/explor_plot_02_plant_species.pdf",
  ext_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")

#----------------------------------------------------------#
# per plant age of leaf
(ext_plot_03 <- 
    dataset_herbivory_sum %>% 
    ggplot(
      aes(
        x = leaf_age,
        y = herbivory_percentage_mean)) +
    
    geom_flat_violin(
      col = "gray30",
      alpha = 1/2,
      trim = TRUE,
      position = position_nudge(
        x = 0.2,
        y = 0)) +
    
    geom_point(
      position = position_jitter(width = 0.15),
      alpha = 1,
      size = 1) +
    
    geom_boxplot(
      width=0.2,
      outlier.shape = NA,
      col = "gray30",
      alpha = 0.5) +
    
    labs(
      x = "Age of leaf", 
      y = expression(paste("Herbivory per individual (%)"))) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

ggsave(
  "figures/total_herbivory/explor_plot_03_leaf_age.pdf",
  ext_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")


#----------------------------------------------------------#
# per leaf side
(ext_plot_04 <- 
    dataset_herbivory_sum %>% 
    ggplot(
      aes(
        x = side,
        y = herbivory_percentage_mean)) +
    
    geom_flat_violin(
      col = "gray30",
      alpha = 1/2,
      trim = TRUE,
      position = position_nudge(
        x = 0.2,
        y = 0)) +
    
    geom_point(
      position = position_jitter(width = 0.15),
      alpha = 1,
      size = 1) +
    
    geom_boxplot(
      width=0.2,
      outlier.shape = NA,
      col = "gray30",
      alpha = 0.5) +
    
    labs(
      x = "Side of the leaf", 
      y = expression(paste("Herbivory per individual (%)"))) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

ggsave(
  "figures/total_herbivory/explor_plot_04_leaf_side.pdf",
  ext_plot_04,
  width = PDF_width,
  height = PDF_height,
  units = "in")

