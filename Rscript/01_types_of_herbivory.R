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
# 4. Full dataset herbivory explorations  -----
#----------------------------------------------------------#

# 4.1. per side of leaf and age of the leaf
(ext_plot_01 <- 
   dataset_herbtype_sum %>% 
   ggplot(
     aes(
       x = side,
       y = herbivory_percentage_mean,
       fill = leaf_age,
       col = leaf_age)) +
   
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
     y = "Herbivory per individual (prop)") +
   scale_fill_manual(values = c("#42adc7", "#ffb902")) +
    scale_y_continuous(
     limits = c(0,1)) +
   scale_color_manual(values = c("#42adc7", "#ffb902")) +
    theme(
    text = element_text(size = text_size),
    legend.position = "right"))


ggsave(
  "figures/explor_plot_01_leaf_side_age.pdf",
  ext_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")
# I am not sure what this coudl mean and how this shoudl be discussed, but look like
# only old leaves are eaten from both sides, while young leaves are eaten only from upper side
# it is however very likley the methodological issue, where we did not measure herbivory 
# on low side of young leaves, I will have to check the scans once again

# 4.2. per habitat and herbivory type
(ext_plot_02 <- 
    dataset_herbtype_sum %>% 
    ggplot(
      aes(
        x = habitat,
        y = herbivory_percentage_mean,
        fill = herbivory_type,
        col = herbivory_type)) +
    
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
      y = "Herbivory per individual (prop)") +
    scale_fill_manual(values = c("#42adc7", "#ffb902")) +
    scale_y_continuous(
      limits = c(0,1)) +
    scale_color_manual(values = c("#42adc7", "#ffb902")) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))


ggsave(
  "figures/explor_plot_02_habitat_herbtype.pdf",
  ext_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")

#----------------------------------------------------------#
# 5. Build the model for full dataset   -----
#----------------------------------------------------------#
# 5.1. build and investigate the model, select the best one
# as leaf age and side of the leaf show overlap and there is moreoover one cathegory missing
# we sum the data bit more excluding these factors and make the further work easier
dataset_herbtype_sum2 <-
  dataset_long %>% 
  mutate(
    herbivory) %>%  # recalculate 
  group_by(species, site, herbivory_type, habitat, individual) %>% 
  summarize(
    .groups = "keep",
    #  leaf_area_total = sum(LeafAreaIdeal)/10e3,
    herbivory_percentage_median = median(herbivory),
    herbivory_percentage_mean = mean(herbivory)
  )
summary(dataset_herbtype_sum2)

write_csv(
  dataset_herbtype_sum,
  "data/output/dataset_herbtype_sum2.csv")

# run the full model which includes all possible combinations and has plant species as a random factor
dataset_herbtype_sum2$herbratio <- dataset_herbtype_sum2$herbivory_percentage_mean/100+0.0001
summary(dataset_herbtype_sum2)
glm_herbtype_sum2<-glmmTMB(herbratio ~ herbivory_type * habitat + (1|species), dataset_herbtype_sum2, family=beta_family)
summary(glm_herbtype_sum2)

# select the best model 
glm_herbtype_sum_dd <- 
  MuMIn::dredge(glm_herbtype_sum2,
                trace = TRUE)

# view the results
glm_herbtype_sum_dd %>% 
  as_tibble() %>% 
  filter(delta <2 ) %>% 
  View()

glm_herbivory_select<-
  glmmTMB(herbratio ~ habitat + herbivory_type + habitat : herbivory_type + (1|species),family=beta_family, data = dataset_herbtype_sum2)

summary(glm_herbivory_select)
check_model(glm_herbivory_select)
model_performance(glm_herbivory_select)
check_heteroscedasticity(glm_herbivory_select)
qplot(residuals(glm_herbivory_select))

# 5.2 Make model plots  -----
# calculate emmeans hab
glm_habherbtype_emmeans <-
  emmeans(
    glm_herbivory_select,
    pairwise ~ habitat * herbivory_type,
    type = "response") 

# save the pairwise test 
glm_habherbtype_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/herbivory_pairwise_habherbtype_contrast.csv")
glm_habherbtype_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/herbivory_pairwise_habherbtype_emmeans.csv")


(model_plot_01 <-
    glm_habherbtype_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = habitat,
        y = response,
        col = herbivory_type,
        fill = herbivory_type)) +
    
    geom_point(
      data = dataset_herbtype_sum2,
      aes(y = herbratio),
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
      x = "Habitat",
      y = "Mean herbivory damage (prop)" )+
    scale_fill_manual(values = c("#42adc7", "#ffb902"))+
    scale_color_manual(values = c("#42adc7", "#ffb902"))+
    theme(
      text = element_text(size = text_size),
      legend.position = "top"))

# save pdf
ggsave(
  "figures/model1_habherbtype_fulldata.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

