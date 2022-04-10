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
# 2.0. subset the full data for the species occuring at all 3 habitat types only ----
#----------------------------------------------------------#
# Explore the data and subset them before that
unique(dataset_herbtype_sum2$species)   
herbivorysubset <- subset(dataset_herbtype_sum2, species =='ACA_ROB'| species == 'EUC_RAC'| species == 'GYM_HAR'| species == 'SCU_MYR'|species == 'SID_INE'| species == 'SPI_AFR'|species == 'KRA_FLO')
unique(herbivorysubset$species)                

write_csv(
  herbivorysubset,
  "data/output/04_dataset_subset7.csv")
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
      y = "Proportion of leaf area lost (%)") +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    scale_y_continuous(
      limits = c(0,0.3)) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))

ggsave(
  "figures/exp_plot_03_species_habitat_subset7.pdf",
  exp_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")

#----------------------------------------------------------#
# 2.1. Make a model  ----
#----------------------------------------------------------#
# Make the full model including all possible options for the subset of the data for 7 species
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
  glmmTMB(herbratio ~ herbivory_type + habitat + species + herbivory_type : species + habitat: species + habitat: herbivory_type + (1|individual), data=herbivorysubset, family = "beta_family")
summary(glm_subset7_select)
# as the full interactions of the 3 factors is not significant, we have to analyse the 2 individual interactions of 2 factors separately

#----------------------------------------------------------#
# 2.2.a Calculate emmeans for herbivory_type : habitat ----
#----------------------------------------------------------#
# BUT BE CAREFUL - THIS INTERACTION IS NOT SIGNIFICANT ANYMORE ACCORDING TO THE NEW DATA
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
  write_csv("data/output/04a_subset7_pairwise_habherbtype_contrast.csv")
a_glm_subset7_select_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/04a_subset7_pairwise_habherbtype_emmeans.csv")


(model_plot_02 <-
    a_glm_subset7_select_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = habitat,
        y = response*100,
        col = herbivory_type,
        fill = herbivory_type)) +
    
    geom_point(
      data = herbivorysubset,
      aes(y = herbratio*100),
      alpha = 0.4, size=2,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL*100,
        ymax = upper.CL*100),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size=0.5,  color="black") +
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5), color="black")+
    
    ylim(0,25) +
    
    theme(axis.text.x=element_text(colour="black")) +
    theme(axis.text.y=element_text(colour="black")) +
    theme(axis.line = element_line(colour = 'black', size = 1)) +
    theme(axis.ticks = element_line(colour = "black", size = 1)) +
    
    annotate("text", x = 2, y=25, label = "Subset of 7 species occuring at all habitat types", size = 6) +
    
    labs(
      x = "Habitat",
      y = "Proportion of leaf area lost (%)")+
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

#----------------------------------------------------------#
# 2.2.b  Calculate emmeans for habitat:species ----
#----------------------------------------------------------#
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
  write_csv("data/output/04b_subset7_pairwise_habspec_contrast.csv")
b_glm_subset7_select_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/04b_subset7_pairwise_habspec_emmeans.csv")

# note that I use the binomial data from the models but I multiply all values by 100
# so it appears like the damage in cm2 per 100
(model_plot_03 <-
    b_glm_subset7_select_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = species,
        y = response*100,
        col = habitat,
        fill = habitat)) +
    
    geom_point(
      data = herbivorysubset,
      aes(y = herbratio*100),
      alpha = 0.4, size=2,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL*100,
        ymax = upper.CL*100),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size=0.5,  color="black") +
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5), color="black")+
    
    ylim(-0.2,25) +
    
    theme(axis.text.x=element_text(colour="black")) +
    theme(axis.text.y=element_text(colour="black")) +
    theme(axis.line = element_line(colour = 'black', size = 1)) +
    theme(axis.ticks = element_line(colour = "black", size = 1)) +
    
    annotate("text", x = 4, y=25, label = "Subset of 7 species occuring at all habitat types", size = 6) +
    
    labs(
      x = "Plant species",
      y = "Proportion of leaf area lost (%)" )+
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))+
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))+
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


#----------------------------------------------------------#
# 2.2.c  Calculate emmeans for herbivory_type:species ----
#----------------------------------------------------------#
c_glm_subset7_select_emmeans <-
  emmeans(
    glm_subset7_select,
    pairwise ~ herbivory_type:species,
    type = "response")
c_glm_subset7_select_emmeans

# save the pairwise test 
c_glm_subset7_select_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/04c_subset7_pairwise_habspec_contrast.csv")
c_glm_subset7_select_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/04c_subset7_pairwise_habspec_emmeans.csv")

# note that I use the binomial data from the models but I multiply all values by 100
# so it appears like the damage in cm2 per 100
(model_plot_04 <-
    c_glm_subset7_select_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = species,
        y = response*100,
        col = herbivory_type,
        fill = herbivory_type)) +
    
    geom_point(
      data = herbivorysubset,
      aes(y = herbratio*100),
      alpha = 0.4, size=2,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL*100,
        ymax = upper.CL*100),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size=0.5,  color="black") +
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5), color="black")+
    
    ylim(-0.2,25) +
    
    theme(axis.text.x=element_text(colour="black")) +
    theme(axis.text.y=element_text(colour="black")) +
    theme(axis.line = element_line(colour = 'black', size = 1)) +
    theme(axis.ticks = element_line(colour = "black", size = 1)) +
    
    annotate("text", x = 4, y=25, label = "Subset of 7 species occuring at all habitat types", size = 6) +
    
    labs(
      x = "Plant species",
      y = "Proportion of leaf area lost (%)" )+
    scale_fill_manual(values = c("#42adc7", "#ffb902"))+
    scale_color_manual(values = c("#42adc7", "#ffb902"))+
    theme(
      text = element_text(size = 20),
      legend.position = "top"))

# save pdf
ggsave(
  "figures/model4_spec_herb_type_subset7.pdf",
  model_plot_04,
  width = PDF_width,
  height = PDF_height,
  units = "in")
