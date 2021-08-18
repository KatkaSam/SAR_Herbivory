#----------------------------------------------------------#
#
#
#                      SAR Herbivory
#
#      Effect of habitat only on species occuring at all sites
#
#             Katerina Sam  - Heveakore Maraia
#                         2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 6. subset the full data for the species occuring at all 3 habitat types only
#----------------------------------------------------------#

# 6.1. Explore the data and subset them before that
unique(dataset_herbtype_sum2$species)   
herbivorysubset <- subset(dataset_herbtype_sum2, species =='ACA_ROB'| species == 'EUC_RAC'| species == 'GYM_HAR'| species == 'SCU_MYR'|species == 'SID_INE'| species == 'SPI_AFR'|species == 'KRA_FLO')
unique(herbivorysubset$species)                

write_csv(
  herbivorysubset,
  "data/output/dataset_subset7.csv")
summary(herbivorysubset)

# just trying to split the herbivory type
# herbivorysubset <- subset(herbivorysubset, herbivory_type =='chew_perc')
# unique(herbivorysubset$herbivory_type) 
# herbivorysubset <- subset(herbivorysubset, herbivory_type =='sap_min_perc')
# unique(herbivorysubset$herbivory_type)  

# per species and habitat
(exp_plot_03 <- 
    herbivorysubset %>% 
    ggplot(
      aes(
        x = species,
        y = herbratio,
        fill = habitat,
        col = habitat)) +
    
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
      y = "Herbivory per individual (prop)") +
    scale_fill_manual(values = c("#42adc7", "#ffb902", "olivedrab4")) +
    scale_y_continuous(
      limits = c(0,0.20)) +
    scale_color_manual(values = c("#42adc7", "#ffb902", "olivedrab4")) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))

ggsave(
  "figures/explor_plot_03_species_habitat_subset7.pdf",
  exp_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# 6.2. Make the full model including all possible options for the subset of the data for 7 species
glm_herbtype_subset7<-glmmTMB(herbratio ~ herbivory_type * habitat * species + (1|individual), data=herbivorysubset, family = "beta_family", na.action = "na.fail")
summary(glm_herbtype_subset7)

# # compute all posible combinations
glm_subset7_dd <- 
  MuMIn::dredge(glm_herbtype_subset7,
                trace = TRUE)

# view the results
glm_subset7_dd %>% 
  as_tibble() %>% 
  filter(delta <2 ) %>% 
  View()

glm_subset7_select<-
  glmmTMB(herbratio ~ herbivory_type + habitat + species + herbivory_type : habitat + habitat: species + herbivory_type : species + (1|individual), data=herbivorysubset, family = "beta_family")
summary(glm_subset7_select)

# as the full interactions of the 3 factors is not significant, we have to anlayse the 2 individual interactions of 2 factors separately
# 6.2.a Calculate emmeans for herbivory_type : habitat
a_glm_subset7_select_emmeans <-
  emmeans(
    glm_subset7_select,
    pairwise ~ herbivory_type : habitat,
    type = "response")
a_glm_subset7_select_emmeans

# save the pairwise test 
a_glm_subset7_select_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/a_subset7_pairwise_habherbtype_contrast.csv")
a_glm_subset7_select_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/a_subset7_pairwise_habherbtype_emmeans.csv")


(model_plot_02 <-
    a_glm_subset7_select_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = habitat,
        y = response,
        col = herbivory_type,
        fill = herbivory_type)) +
    
    geom_point(
      data = herbivorysubset,
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
    
    ylim(0,0.25) +
    
    theme(axis.text.x=element_text(colour="black")) +
    theme(axis.text.y=element_text(colour="black")) +
    theme(axis.line = element_line(colour = 'black', size = 1)) +
    theme(axis.ticks = element_line(colour = "black", size = 1)) +
    
    annotate("text", x = 2, y=0.25, label = "Subset of 7 species occuring at all habitat types", size = 6) +
    
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
  "figures/model2_herbtype_habitat_subset7.pdf",
  model_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# 6.2.b  Calculate emmeans for habitat:species
b_glm_subset7_select_emmeans <-
  emmeans(
    glm_subset7_select,
    pairwise ~ habitat:species,
    type = "response")
b_glm_subset7_select_emmeans

# save the pairwise test 
b_glm_subset7_select_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/b_subset7_pairwise_habspec_contrast.csv")
b_glm_subset7_select_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/b_subset7_pairwise_habspec_emmeans.csv")


(model_plot_03 <-
    b_glm_subset7_select_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = species,
        y = response,
        col = habitat,
        fill = habitat)) +
    
    geom_point(
      data = herbivorysubset,
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
    
    ylim(0,0.25) +
    
    theme(axis.text.x=element_text(colour="black")) +
    theme(axis.text.y=element_text(colour="black")) +
    theme(axis.line = element_line(colour = 'black', size = 1)) +
    theme(axis.ticks = element_line(colour = "black", size = 1)) +
    
    annotate("text", x = 4, y=0.25, label = "Subset of 7 species occuring at all habitat types", size = 6) +
    
    labs(
      x = "Plant species",
      y = "Mean herbivory damage (prop)" )+
    scale_fill_manual(values = c("#42adc7", "#ffb902", "olivedrab4"))+
    scale_color_manual(values = c("#42adc7", "#ffb902", "olivedrab4"))+
    theme(
      text = element_text(size = 20),
      legend.position = "top"))

# save pdf
ggsave(
  "figures/model3_spec_hab_subset7.pdf",
  model_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# 6.2.a Calculate emmeans for herbivory_type : species (Kore, try to do this one by yourself)
