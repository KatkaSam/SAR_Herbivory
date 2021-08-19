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

herbivory_savanna <- subset(dataset_diversity, habitat  == "savanna")
herbivory_other <- subset(dataset_diversity, habitat  == "forest"|habitat  == "thicket")

# this plot shows correlation between total herbivory and the diversity as NUMBER OF ESTIMATED SPECIES of the plot
plot(dataset_diversity$plot_diversitySP, dataset_diversity$herbivory_cm2, pch = 20, 
     col = ifelse(dataset_diversity$habitat == "savanna",  "red", "blue"),
     xlab = "Diversity",
     ylab = "Herbivory (in cm2)")
abline(lm(herbivory_savanna$herbivory_cm2 ~ herbivory_savanna$plot_diversitySP), col = "red")
abline(lm(herbivory_other$herbivory_cm2 ~ herbivory_other$plot_diversitySP), col = "blue")

legend("topleft", 
       pch = c(19, 19), 
       c("savanna", "other"), 
       col = c("red", "blue"))

cor.test(herbivory_other$herbivory_cm2, herbivory_other$plot_diversitySP, method="pearson")
cor.test(herbivory_savanna$herbivory_cm2, herbivory_savanna$plot_diversitySP, method="pearson")

library(sjPlot)
set_theme(
  geom.outline.color = "antiquewhite4", 
  geom.outline.size = 1, 
  geom.label.size = 2,
  geom.label.color = "grey50",
  title.color = "red", 
  title.size = 1.5, 
  axis.angle.x = 45, 
  axis.textcolor = "blue", 
  base = theme_bw()
)

# very similar but used ALPHA diversity
model_plot_8a<-plot(dataset_diversity$plot_diversityAlpha, dataset_diversity$herbivory_cm2, pch = 20, 
     col = ifelse(dataset_diversity$habitat == "savanna",  "#ffb902", "olivedrab4"),
     xlab = "Diversity",
     ylab = "Herbivory (in cm2)",
     bty = "l",
     las = 1.5, cex.axis = 1.5, tcl = -0.3, 
     cex = 2, cex.lab=1.5) +
abline(lm(herbivory_savanna$herbivory_cm2 ~ herbivory_savanna$plot_diversityAlpha), col = "#ffb902", lwd=3 ) +
abline(lm(herbivory_other$herbivory_cm2 ~ herbivory_other$plot_diversityAlpha), col = "olivedrab4", lwd=3)
model_plot_8a
# save it manually 

cor.test(herbivory_other$herbivory_cm2, herbivory_other$plot_diversityAlpha, method="pearson")
cor.test(herbivory_savanna$herbivory_cm2, herbivory_savanna$plot_diversityAlpha, method="pearson")
legend("topleft", 
       pch = c(19, 19), 
       c("savanna", "other"), 
       col = c("#ffb902", "olivedrab4"))
# with the ALPHA DIVERSITY, the results are more tight and more correlated

# here I summarize the data and remove the explanatory columns
summary(dataset_diversity)
dataset_diversity1 <- dataset_diversity[ , -c(1:5)]

# here, I found that two species have 0 herbivory within the dataset and it is causing further crash of script
# so I saved the table I made, opened it, deleted the two species and loaded the data under new name
# it worked but if it crashes to you, you might need to do the same thing
write_csv(
  dataset_diversity1,
  "data/output/dataset_diversity1.csv")
dataset_diversity2 <- read.csv ("data/output/dataset_diversity1.csv")

herb_div<-cor(dataset_diversity2)
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







