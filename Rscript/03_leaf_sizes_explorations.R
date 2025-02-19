#----------------------------------------------------------#
#
#
#                      SAR Herbivory
#
#             Explorations of the leaf sizes
#
#             Katerina Sam  - Heveakore Maraia
#                         2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 3.0. Leaf sizes investigations  for all plant species  -----
#----------------------------------------------------------#

summary(dataset_herbivory_sum)

(exp_plot_04 <- 
    dataset_herbivory_sum %>% 
    ggplot(
      aes(
        x = habitat,
        y =  mean_leaf_area)) +
    
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
    
    theme(axis.text.x=element_text(colour="black")) +
    theme(axis.text.y=element_text(colour="black")) +
    theme(axis.line = element_line(colour = 'black', size = 1)) +
    theme(axis.ticks = element_line(colour = "black", size = 1)) +
    
    labs(
      x = "Habitat", 
      y = expression("Leaf area"~(cm^2) )) +
    scale_fill_manual(values = c("#42adc7", "#ffb902")) +
    scale_y_continuous(
      limits = c(0,70)) +
    scale_color_manual(values = c("#42adc7", "#ffb902")) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))

# save pdf
ggsave(
  "figures/exp_plot_04_leaf_area_habitat.pdf",
  exp_plot_04,
  width = PDF_width,
  height = PDF_height,
  units = "in")
# it will be in supplement

glm_leafarea<-lmer(mean_leaf_area ~ habitat + (1|species), dataset_herbivory_sum)
anova(glm_leafarea)
emmeans(glm_leafarea, list(pairwise ~ habitat), adjust = "tukey")
# this analyses were done for text

#----------------------------------------------------------#
# 3.1. Leaf sizes investigations  for  the subset of 7 plant species  -----
#----------------------------------------------------------#
unique(dataset_herbivory_sum$species)   
leafareasubset <- subset(dataset_herbivory_sum, species =='ACA_ROB'| species == 'EUC_RAC'| species == 'GYM_HAR'| species == 'SCU_MYR'|species == 'SID_INE'| species == 'SPI_AFR'|species == 'KRA_FLO')
unique(leafareasubset$species)                

write_csv(
  leafareasubset,
  "data/output/05_leafarea_subset7.csv")
summary(leafareasubset)

# there is a problem with leaf are of ACA_ROB as it was not considered as a compound leaf,
# so I had to manually multily the leaf area of leaflets by 92, which is average number of leaflets per leaf
leafareasubset<-read.csv("data/output/05_leafarea_subset7.csv")

glm_leafarea_subset<-lmer( mean_leaf_area ~ habitat * species + (1|individual), leafareasubset)
anova(glm_leafarea_subset)
emmeans(glm_leafarea_subset, list(pairwise ~ habitat * species), adjust = "tukey")

# calculate emmeans hab
glm_leafarea_subset_emmeans <-
  emmeans(
    glm_leafarea_subset,
    pairwise ~ habitat * species,
    type = "response", lmerTest.limit = 5922, pbkrtest.limit = 5922) 

# save the pairwise test 
glm_leafarea_subset_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/05a_leafarea_pairwise_habspec_emmeans.csv")
glm_leafarea_subset_emmeans$contrasts %>% 
  as_tibble() %>% 
  write_csv("data/output/05b_leafarea_pairwise_habspec_contrasts.csv")

data<-read.csv("data/output/05a_leafarea_pairwise_habspec_emmeans.csv")

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
      aes(y = mean_leaf_area),
      alpha = 0.4,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width=0.5,
      position = position_dodge(width = 0.5, preserve = "single"),
      size=0.5,  color="black") +
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5), color="black")+
    
    theme(axis.text.x=element_text(colour="black")) +
    theme(axis.text.y=element_text(colour="black")) +
    theme(axis.line = element_line(colour = 'black', size = 1)) +
    theme(axis.ticks = element_line(colour = "black", size = 1)) +
    
    labs(
      x = "Species",
      y = expression("Leaf area"~(cm^2) )) +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) + 
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    theme(
      text = element_text(size = 18),
      legend.position = "top")) 

# save pdf
ggsave(
  "figures/model5_leafarea_subset7.pdf",
  model_plot_04,
  width = PDF_width,
  height = PDF_height,
  units = "in")
# this figures will be used in supplement only
