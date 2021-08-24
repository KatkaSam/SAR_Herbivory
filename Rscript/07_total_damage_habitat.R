#----------------------------------------------------------#
#
#
#                      SAR Herbivory
#
#              Total herbivory of each plot
#
#             Katerina Sam  - Heveakore Maraia
#                         2021
#
#----------------------------------------------------------

#----------------------------------------------------------#
# 7.0. Total herbivory data summarization -----
#----------------------------------------------------------#

dataset_herbivory_div <-  
  readxl::read_xlsx("data/input/Total_damage_abundances_20210819.xlsx")
summary(dataset_herbivory_div)
sum_herb <- dataset_herbivory_div [-c(7:54)]
summary(sum_herb)

sum_herb_new <- subset (sum_herb, herb_type == "chewing" |herb_type =="mining")
summary(sum_herb_new)

# make an exploratory graph
(exp_plot_05 <- 
    sum_herb_new %>% 
    ggplot(
      aes(
        x = habitat,
        y = herbivory_cm2,
        fill = herb_type,
        col = herb_type)) +
    
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
      y = "Herbivory per plot") +
    scale_fill_manual(values = c("#42adc7", "#ffb902", "red")) +
    scale_y_continuous(
      limits = c(0,500)) +
    scale_color_manual(values = c("#42adc7", "#ffb902", "red")) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))

# make models and select the best one
m.0<-glmmTMB(herbivory_cm2 ~ 1, data=sum_herb_new, na.action = "na.fail")
m.1<-glmmTMB(herbivory_cm2 ~ herb_type , data=sum_herb_new, na.action = "na.fail")
m.2<-glmmTMB(herbivory_cm2 ~ habitat, data=sum_herb_new, na.action = "na.fail")
m.3<-glmmTMB(herbivory_cm2 ~ herb_type + habitat, data=sum_herb_new, na.action = "na.fail")
m.4<-glmmTMB(herbivory_cm2 ~ herb_type : habitat, data=sum_herb_new, na.action = "na.fail")
m.full<-glmmTMB(herbivory_cm2 ~ herb_type * habitat, data=sum_herb_new, na.action = "na.fail")

AICctab(m.0, m.2, m.1, m.3,m.4,m.full)

# calculate emmeans and predicted values
m.full_emmeans <-
  emmeans(
    m.full,
    pairwise ~ herb_type : habitat,
    type = "response")
m.full_emmeans

# save the pairwise test 
m.full_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/11_total_damage_mc2_contrast.csv")
m.full_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/11_total_damage_mc2_emmeans.csv")

# draw the predicted values form the model 
(model_plot_9 <-
    m.full_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = habitat,
        y = emmean,
        col = herb_type,
        fill = herb_type)) +
    
    geom_point(
      data = sum_herb_new,
      aes(y = herbivory_cm2),
      alpha = 0.6,
      size = 3,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.1)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size=0.7,  color="black") +
    
    geom_point(
      shape = 0,
      size = 4,
      position = position_dodge(width = 0.5), color="black")+
    
    ylim(0,500) +
    
    theme(axis.text.x=element_text(colour="black")) +
    theme(axis.text.y=element_text(colour="black")) +
    theme(axis.line = element_line(colour = 'black', size = 1)) +
    theme(axis.ticks = element_line(colour = "black", size = 1)) +
    
    labs(
      x = "Habitat",
      y = expression("Summarized leaf area loss"~(cm^2))) +
    scale_fill_manual(values = c("#ffb902", "#42adc7"))+
    scale_color_manual(values = c("#ffb902", "#42adc7"))+
    theme(
      text = element_text(size = text_size),
      legend.position = "top"))

# save pdf
ggsave(
  "figures/model9_total_damage_per_plot.pdf",
  model_plot_9,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# plot also the total herbivory without split into types of herbivory
sum_herb_total <- subset (sum_herb, herb_type == "total")
(exp_plot_06 <- 
    sum_herb_total %>% 
    ggplot(
      aes(
        x = habitat,
        y = herbivory_cm2,
        fill = herb_type,
        col = herb_type)) +
    
    geom_point(
      position = position_jitter(width = 0.15),
      alpha = 1,
      size = 2) +
    
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
      y = "Herbivory per plot") +
    scale_fill_manual(values = c("red")) +
    scale_y_continuous(
      limits = c(0,500)) +
    scale_color_manual(values = c("red")) +
    theme(
      text = element_text(size = text_size),
      legend.position = "top"))
# this exploratory graph will be used as part b of the model graph, 
#thus it needs to be exported manually as narrow picture

