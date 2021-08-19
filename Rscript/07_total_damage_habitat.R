# 2.1 total herbivory data summarization -----
dataset_herbivory_div <-  
  readxl::read_xlsx("data/input/Species_abundances_analyses.xlsx")
summary(dataset_herbivory_div)
sum_herb <- dataset_herbivory_div [-c(7:54)]
summary(sum_herb)

sum_herb_new <- subset (sum_herb, herb_type == "chewing" |herb_type =="mining")
summary(sum_herb_new)


(exp_plot_03 <- 
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


m.0<-glmmTMB(herbivory_cm2 ~ 1, data=sum_herb_new, na.action = "na.fail")
m.1<-glmmTMB(herbivory_cm2 ~ herb_type , data=sum_herb_new, na.action = "na.fail")
m.2<-glmmTMB(herbivory_cm2 ~ habitat, data=sum_herb_new, na.action = "na.fail")
m.3<-glmmTMB(herbivory_cm2 ~ herb_type + habitat, data=sum_herb_new, na.action = "na.fail")
m.4<-glmmTMB(herbivory_cm2 ~ herb_type : habitat, data=sum_herb_new, na.action = "na.fail")
m.full<-glmmTMB(herbivory_cm2 ~ herb_type * habitat, data=sum_herb_new, na.action = "na.fail")

AICctab(m.0, m.2, m.1, m.3,m.4,m.full)

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
  write_csv("data/output/total_damage_mc2_contrast.csv")
m.full_emmeans$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/total_damage_mc2_emmeans.csv")

(model_plot_04 <-
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
    
    ylim(0,500) +
    
    theme(axis.text.x=element_text(colour="black")) +
    theme(axis.text.y=element_text(colour="black")) +
    theme(axis.line = element_line(colour = 'black', size = 1)) +
    theme(axis.ticks = element_line(colour = "black", size = 1)) +
    
    labs(
      x = "Habitat",
      y = "Total herbivory damage per plot" )+
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




sum_herb_total <- subset (sum_herb, herb_type == "total")
(exp_plot_04 <- 
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
      y = "Herbivory per plot") +
    scale_fill_manual(values = c("red")) +
    scale_y_continuous(
      limits = c(0,500)) +
    scale_color_manual(values = c("red")) +
    theme(
      text = element_text(size = text_size),
      legend.position = "top"))


