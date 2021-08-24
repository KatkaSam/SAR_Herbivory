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
# 1.0. Full dataset herbivory explorations  -----
#----------------------------------------------------------#
# per side of leaf and age of the leaf
(exp_plot_01 <- 
   dataset_herbtype_sum %>% 
   ggplot(
     aes(
       x = side,
       y = herbivory_standardized,
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
     y = "Proportion of leaf area lost (%)") +
   scale_fill_manual(values = c("#42adc7", "#ffb902")) +
    scale_y_continuous(
     limits = c(0,60)) +
   scale_color_manual(values = c("#42adc7", "#ffb902")) +
    theme(
    text = element_text(size = text_size),
    legend.position = "right"))

# this plot will be used in supplement only
ggsave(
  "figures/exp_plot_01_leaf_side_age.pdf",
  exp_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")


# here, I am subsetting the data to OLD leaves only and investigating if the type of herbivory differs on
# on the upper and lower side of these old leaves
newdata<-subset(dataset_herbtype_sum, leaf_age == "old")
summary(newdata)
# in the data, the herbivory is in % so for the analysis, we have to divide them by 100 and add tiny number so it is >0
newdata$herbratio <- newdata$herbivory_standardized/100+0.0001
library(dplyr)
group_by(newdata, herbivory_type) %>%
  summarise(
    count = n(),
    mean = mean(herbratio, na.rm = TRUE),
    sd = sd(herbratio, na.rm = TRUE))
m0<-glmmTMB(herbratio~1, data = newdata, family=beta_family)
m1<-glmmTMB(herbratio~side, data = newdata, family=beta_family)
m2<-glmmTMB(herbratio~herbivory_type, data = newdata, family=beta_family)
m3<-glmmTMB(herbratio~side*herbivory_type, data = newdata, family=beta_family)
m4<-glmmTMB(herbratio~side+herbivory_type, data = newdata, family=beta_family)
AICctab(m0,m1,m2,m3, m4)
summary(m3)
emmeans1 <-
  emmeans(
    m3,
    pairwise ~ herbivory_type*side,
    type = "response")
emmeans1
plot(emmeans1)
#  there is no significant difference between herbivory at both sides of the leaf

emmeans2 <-
  emmeans(
    m2,
    pairwise ~ herbivory_type,
    type = "response")
emmeans2
plot(emmeans2)
#  but there is significant difference between the types of herbivory


# now, I had to read the original data again and subset them to upper side only and investigate
# if the herbivory differs between ond and young leaves and types of herbivory
newdata1<-subset(dataset_herbtype_sum, side == "up")
newdata1$herbratio <- newdata1$herbivory_standardized/100+0.0001
summary(newdata1)
group_by(newdata1, herbivory_type, leaf_age) %>%
  summarise(
    count = n(),
    mean = mean(herbratio, na.rm = TRUE),
    sd = sd(herbratio, na.rm = TRUE))
m0<-glmmTMB(herbratio~1, data = newdata1, family=beta_family)
m1<-glmmTMB(herbratio~leaf_age, data = newdata1, family=beta_family)
m2<-glmmTMB(herbratio~herbivory_type, data = newdata1, family=beta_family)
m3<-glmmTMB(herbratio~leaf_age*herbivory_type, data = newdata1, family=beta_family)
m4<-glmmTMB(herbratio~leaf_age+herbivory_type, data = newdata1, family=beta_family)
AICctab(m0,m1,m2,m3, m4)
summary(m4)
emmeans3 <-
  emmeans(
    m4,
    pairwise ~ herbivory_type,
    type = "response")
emmeans3
plot(emmeans3)


m2<-lm(herbivory_standardized~leaf_age, data = newdata1)
anova(m2)
res.aov1 <- aov(herbivory_standardized~leaf_age, data = newdata1)
summary(res.aov1)
plot(res.aov1, 2)
# the data are far from being normal, so it looks like the anova can't be used
plot(res.aov1, 1)
# homogeneity is not met euther so we have to do non=parametrical test
aov_residuals <- residuals(object = res.aov1)
shapiro.test(x = aov_residuals)
# shapiro test is very significant which means that normality of data is violated, so we really have to use non-parametrical test here
kruskal.test(herbivory_standardized~leaf_age, data = newdata1)
by(newdata1, newdata1$leaf_age, summary)
# all this overlall mean that the age of the leaf and side of the leaf is not very important for the analyses and do not have to be considered

#----------------------------------------------------------#
# 1.1. Per habitat and herbivory type  -----
#----------------------------------------------------------#
(exp_plot_02 <- 
    dataset_herbtype_sum %>% 
    ggplot(
      aes(
        x = habitat,
        y = herbivory_standardized,
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
      y = "Proportion of leaf area lost (%)") +
    scale_fill_manual(values = c("#42adc7", "#ffb902")) +
    scale_y_continuous(
      limits = c(0,30)) +
    scale_color_manual(values = c("#42adc7", "#ffb902")) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))


ggsave(
  "figures/exp_plot_02_habitat_herbtype.pdf",
  exp_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")
# this graph willnot be used at all in the end

#----------------------------------------------------------#
# 1.2. Build the model for full dataset   -----
#----------------------------------------------------------#
# build and investigate the model, select the best one
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
    mean_herbivory = mean(herbivory),
    mean_leaf_area = mean(leaf_area) 
  )
summary(dataset_herbtype_sum2)

# standardize the leaf damage per 100 cm2 so it can be compared between plant species
dataset_herbtype_sum2$herbivory_standardized <- (100/dataset_herbtype_sum2$mean_leaf_area)*dataset_herbtype_sum2$mean_herbivory

write_csv(
  dataset_herbtype_sum2,
  "data/output/03_dataset_herbtype_sum2.csv")

# run the full model which includes all possible combinations and has plant species as a random factor
summary(dataset_herbtype_sum2)
dataset_herbtype_sum2$herbratio <- dataset_herbtype_sum2$herbivory_standardized/100+0.0001
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

#----------------------------------------------------------#
# 1.3.  Make model plots   -----
#----------------------------------------------------------#
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
  write_csv("data/output/03a_herbivory_pairwise_habherbtype_contrast.csv")
glm_habherbtype_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/03b_herbivory_pairwise_habherbtype_emmeans.csv")

(model_plot_01 <-
    glm_habherbtype_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = habitat,
        y = response*100,
        col = herbivory_type,
        fill = herbivory_type)) +
    
    geom_point(
      data = dataset_herbtype_sum2,
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
      size=1,  color="black") +
    
    theme(axis.text.x=element_text(colour="black")) +
    theme(axis.text.y=element_text(colour="black")) +
    theme(axis.line = element_line(colour = 'black', size = 1)) +
    theme(axis.ticks = element_line(colour = "black", size = 1)) +
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5), color="black")+
    
    labs(
      x = "Habitat",
      y = "Proportion of leaf area lost (%)" )+
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
# this figure will be used in the main text of the manuscript
# best to export it also manually as pdf with size 8.79 x 7.10 inches (after saving this pdf)

# this is to took at the means of the raw data
summary(dataset_herbtype_sum2)
group_by(dataset_herbtype_sum2, habitat, herbivory_type) %>%
  summarise(
    count = n(),
    mean = mean(herbratio, na.rm = TRUE),
    sd = sd(herbratio, na.rm = TRUE))

# but in the text, these values should be used
glm_habherbtype_emmeans$emmeans

# this is to took at the means of the raw data
# to be used only in the summary sentence in the text
summary(dataset_herbtype_sum2)
group_by(dataset_herbtype_sum2, habitat) %>%
  summarise(
    count = n(),
    mean = mean(herbratio, na.rm = TRUE),
    sd = sd(herbratio, na.rm = TRUE))

