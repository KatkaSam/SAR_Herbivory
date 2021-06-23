#----------------------------------------------------------#
#
#
#                      SAR Herbivory
#
#      Effect of habitat on herbivory in full dataset
# (in full dataset, design is unbalanced, many species occur at 1 site only)
#
#             Katerina Sam  - Heveakore Maraia
#                         2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 7. Leaf sizes investigations  for all plant species  -----
#----------------------------------------------------------#

library(lme4)
library(lmerTest)
summary(dataset_herbivory_sum)

(ext_plot_01 <- 
    dataset_herbivory_sum %>% 
    ggplot(
      aes(
        x = habitat,
        y = leaf_area)) +
    
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
      y = "Leaf area (mm)") +
    scale_fill_manual(values = c("#42adc7", "#ffb902")) +
    scale_y_continuous(
      limits = c(0,100)) +
    scale_color_manual(values = c("#42adc7", "#ffb902")) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))

glm_leafarea<-lmer(leaf_area ~ habitat + (1|species), dataset_herbivory_sum)
anova(glm_leafarea)
emmeans(glm_leafarea, list(pairwise ~ habitat), adjust = "tukey")


#----------------------------------------------------------#
# 8. Leaf sizes investigations  for  the subset of 7 plant species  -----
#----------------------------------------------------------#

unique(dataset_herbivory_sum$species)   
leafareasubset <- subset(dataset_herbivory_sum, species =='ACA_ROB'| species == 'EUC_RAC'| species == 'GYM_HAR'| species == 'SCU_MYR'|species == 'SID_INE'| species == 'SPI_AFR'|species == 'KRA_FLO')
unique(leafareasubset$species)                

write_csv(
  leafareasubset,
  "data/output/leafarea_subset7.csv")
summary(leafareasubset)

glm_leafarea_subset<-lmer(leaf_area ~ habitat * species + (1|individual), leafareasubset)
anova(glm_leafarea_subset)
emmeans(glm_leafarea_subset, list(pairwise ~ habitat * species), adjust = "tukey")

# calculate emmeans hab
glm_leafarea_subset_emmeans <-
  emmeans(
    glm_leafarea_subset,
    pairwise ~ habitat * species,
    type = "response", lmerTest.limit = 5922, pbkrtest.limit = 5922) 

# save the pairwise test 
glm_habherbtype_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/herbivory_pairwise_habherbtype_contrast.csv")
glm_leafarea_subset_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/leafarea_pairwise_habspec_emmeans.csv")

data<-read.csv("data/output/leafarea_pairwise_habspec_emmeans.csv")

(model_plot_04 <-
    data %>% 
    ggplot(
      aes(
        x = species,
        y = emmean,
        col = habitat,
        fill = habitat)) +
    
    geom_point(
      data = leafareasubset,
      aes(y = leaf_area),
      alpha = 0.4,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size=0.5,  color="black") +
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5), color="black")+
    
    labs(
      x = "Species",
      y = "Leaf area" )+
    scale_fill_manual(values = c("#42adc7", "#ffb902", "orangered3"), 
                      breaks=c('Forest','Thicket','Savanna'),
                      labels=c('Forest','Thicket','Savanna'))+
    scale_color_manual(values = c("#42adc7", "#ffb902", "orangered3"),
                       breaks=c('Forest','Thicket','Savanna'),
                       labels=c('Forest','Thicket','Savanna'))+
    theme(
      text = element_text(size = text_size),
      legend.position = "top")) 
   
