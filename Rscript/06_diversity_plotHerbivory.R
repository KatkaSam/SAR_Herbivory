#----------------------------------------------------------#
#
#
#                      SAR Herbivory
#
#                Diversity vs. herbivory
#
#             Katerina Sam  - Heveakore Maraia
#                         2021
#
#----------------------------------------------------------

#----------------------------------------------------------#
# 6.0. Load data on diversity and herbivory -----
#----------------------------------------------------------#

dataset_diversity <-  
  readxl::read_xlsx("data/input/Herbivory_vs_diversity_20210819.xlsx")
summary(dataset_diversity)


dataset_diversity<-subset(dataset_diversity, herb_type == "total")
cor.test(dataset_diversity$herbivory_stand, dataset_diversity$plot_diversitySP, method="pearson")

0.# this plot shows correlation between herbivory and estimated diversity (as number of species)
#  it plots savanna points in different colour, but makes a single fit of correlation
plot11 <-ggplot(dataset_diversity, aes(x=plot_diversitySP, y = herbivory_stand, color=hab_type)) +
          geom_point(shape=19, size = 4) +
          geom_smooth(aes(group=1), method=lm, fullrange=TRUE,se=FALSE, color="black") +
  theme(axis.text.x=element_text(colour="black")) +
  theme(axis.text.y=element_text(colour="black")) +
  theme(axis.line = element_line(colour = 'black', size = 1)) +
  theme(axis.ticks = element_line(colour = "black", size = 1)) +
  labs(
    x = "Diversity of the plot (estimated number of woody species)",
    y = "Proportion of leaf area lost (%)" )+
  scale_fill_manual(values = c("#d95f02", "#488795"))+
  scale_color_manual(values = c("#d95f02", "#488795"))+
  theme(
    text = element_text(size = 20),
    legend.position = "top")
plot11

# save pdf
ggsave(
  "figures/model11_diversity_herbivory.pdf",
  plot11,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# a similar plot but in one colour - we can use the earlier
plot12 <-ggplot(dataset_diversity, aes(x=plot_diversitySP, y = herbivory_stand)) +
  geom_point(shape=19, size = 4) +
  geom_smooth(method=lm, fullrange=TRUE) +
  theme(axis.text.x=element_text(colour="black")) +
  theme(axis.text.y=element_text(colour="black")) +
  theme(axis.line = element_line(colour = 'black', size = 1)) +
  theme(axis.ticks = element_line(colour = "black", size = 1)) +
  labs(
    x = "Diversity of the plot",
    y = "Proportion of leaf area lost (%)" )+
  theme(
    text = element_text(size = 20),
    legend.position = "top")
plot12


# individual correlations - for differnt types of the forest
herbivory_savanna <- subset(dataset_diversity, habitat  == "savanna")
cor.test(herbivory_savanna$herbivory_stand, herbivory_savanna$plot_diversitySP, method="pearson")
herbivory_other <- subset(dataset_diversity, habitat  == "forest"|habitat  == "thicket")
cor.test(herbivory_other$herbivory_stand, herbivory_other$plot_diversitySP, method="pearson")

#----------------------------------------------------------#
# 6.1. Load data on diversity and herbivory for individual species-----
#----------------------------------------------------------#
dataset_diversity1 <-  
  readxl::read_xlsx("data/input/Herbivory_vs_diversity_species_20210824.xlsx")
summary(dataset_diversity1)

# here I remove the explanatory columns
dataset_diversity1 <- dataset_diversity1[ , -c(1:6, 8)]
summary(dataset_diversity1)

# alternative - to check alpha diversity
#dataset_diversity1 <- dataset_diversity1[ , -c(1:7)]
#summary(dataset_diversity1)

herb_div<-cor(dataset_diversity1)
testRes = cor.mtest(herb_div, conf.level = 0.95)
testRes

# premaring my custom colour scale, so I avoid white colour
col<- colorRampPalette(c("red", "violet", "blue"))(20)

# colours now mean R of the correlation, while blank cell means non-significant results
# you can see that many correlations are non-significant, 
# but the herbivory of the trees always correlates positively with diversity (if it correlates at all)
model_plot_8<-corrplot(herb_div, type="upper", method="circle", col=col, p.mat = testRes$p,
                       sig.level = 0.08, insig = "blank",  tl.cex=0.7,
                       tl.col="black", tl.srt=45, diag=FALSE)
# to save the graph, you have to do it manually again - it can't be doen by the script
# but I do not think it shows anything useful

# correlation for ZIZ_MUC
plot13 <-ggplot(dataset_diversity1, aes(x=plot_diversitySP, y = ZIZ_MUC)) +
  geom_point(shape=19, size = 4) +
  geom_smooth(method=lm, fullrange=TRUE) +
  theme(axis.text.x=element_text(colour="black")) +
  theme(axis.text.y=element_text(colour="black")) +
  theme(axis.line = element_line(colour = 'black', size = 1)) +
  theme(axis.ticks = element_line(colour = "black", size = 1)) +
  labs(
    x = "Diversity of the plot",
    y = "Proportion of leaf area lost (%)" )+
  theme(
    text = element_text(size = 20),
    legend.position = "top")
plot13

plot14 <-ggplot(dataset_diversity1, aes(x=plot_diversitySP, y = GYM_MAR)) +
  geom_point(shape=19, size = 4) +
  geom_smooth(method=lm, fullrange=TRUE) +
  theme(axis.text.x=element_text(colour="black")) +
  theme(axis.text.y=element_text(colour="black")) +
  theme(axis.line = element_line(colour = 'black', size = 1)) +
  theme(axis.ticks = element_line(colour = "black", size = 1)) +
  labs(
    x = "Diversity of the plot",
    y = "Proportion of leaf area lost (%)" )+
  theme(
    text = element_text(size = 20),
    legend.position = "top")
plot14

plot15 <-ggplot(dataset_diversity1, aes(x=plot_diversitySP, y = BER_LUC)) +
  geom_point(shape=19, size = 4) +
  geom_smooth(method=lm, fullrange=TRUE) +
  theme(axis.text.x=element_text(colour="black")) +
  theme(axis.text.y=element_text(colour="black")) +
  theme(axis.line = element_line(colour = 'black', size = 1)) +
  theme(axis.ticks = element_line(colour = "black", size = 1)) +
  labs(
    x = "Diversity of the plot",
    y = "Proportion of leaf area lost (%)" )+
  theme(
    text = element_text(size = 20),
    legend.position = "top")
plot15

