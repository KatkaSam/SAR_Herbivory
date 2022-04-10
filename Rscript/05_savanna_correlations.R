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
# 5.0. Importing of dataframe for savannas only ----
#----------------------------------------------------------#
# Total herbivory data summarization for each of the study sites-----
dataset_herbivory <-  
  readxl::read_xlsx("data/input/Herbivory_raw_final_20210609.xlsx")
summary(dataset_herbivory)

# sum total herbivory per tree in percentages
dataset_herbivory_savanna <-
  dataset_herbivory %>% 
  mutate(
    tot_pct) %>%  # recalculate 
  group_by(site) %>% 
  summarize(
    .groups = "keep",
    #  leaf_area_total = sum(LeafAreaIdeal)/10e3,
    mean_chewing_herbivory= mean(chew_herbivory),
    mean_mining_herbivory = mean(sap_miner_herbivory),
    mean_leaf_area = mean(leaf_area))
summary(dataset_herbivory_savanna)

# the vlaues willbe standardized now, but column names are kept short
dataset_herbivory_savanna <- dataset_herbivory_savanna %>% mutate(mean_total_herbivory = mean_chewing_herbivory+mean_mining_herbivory)
dataset_herbivory_savanna$total_herbivory <- (100/dataset_herbivory_savanna$mean_leaf_area)*dataset_herbivory_savanna$mean_total_herbivory
dataset_herbivory_savanna$chewing_herbivory <- (100/dataset_herbivory_savanna$mean_leaf_area)*dataset_herbivory_savanna$mean_chewing_herbivory
dataset_herbivory_savanna$mining_herbivory <- (100/dataset_herbivory_savanna$mean_leaf_area)*dataset_herbivory_savanna$mean_mining_herbivory
summary(dataset_herbivory_savanna)

# write the summarized table
write_csv(
  dataset_herbivory_savanna,
  "data/output/08_dataset_herbivory_savanna.csv")
# now because it was verz complicated, I too k the values form this table and put them manually to the table with habitat characteristics of the savanna site 
# and now I have to lead the newlz made table
savanna <- read.csv ("data/input/Environment_savanna_20210820.csv")
summary(savanna)

# Subsetting of dataframe 
savanna <- savanna[-c(1:2, 7:18, 19)]
str(savanna)
summary(savanna)

# Convert factor variables to numeric variable if there are in the dataset (savanna)
savanna$X10yr_fire <- as.numeric(savanna$X10yr_fire)
savanna$X20yr_fire <- as.numeric(savanna$X20yr_fire)
savanna$elevation <- as.numeric(savanna$elevation)


# To convert a wide to long (data condensation) dataset, the package 'reshape2' will be used which I already extracted from the library from
# the previous line above. The package contained a 'melt function' to convert wide to long columns.
savanna <- melt(savanna, id.vars = c("X10yr_fire", "X20yr_fire", "elevation", "Avg_Rain"))

# Once the melt () is used, with the names in the brackets, these names will be kept as they are but the names which are not included will be
# joined together and converted to long format in the column.

#    The renaming of columns
colnames(savanna)= c("X10yr_fire", "X20yr_fire", "elevation", "Avg_Rain", "Herb_type", "Herbivory")
summary(savanna)


# To test models, we need to extract the bbmle package from the library(bblme). Maximum Likelihood Estimate is used for probability estimation for
# best fit models.
# but first, decide whether 20 or 10 year regime is better predictor
m.a <- lm(Herbivory~Herb_type+X10yr_fire, data = savanna)
m.b <- lm(Herbivory~Herb_type+X20yr_fire, data = savanna)
AICctab(m.a, m.b)
# based on the result, 20-year regime is way better predictor

#----------------------------------------------------------#
# 5.1. Make the models and select the best/reasonable one -----
#----------------------------------------------------------#
m0 <- lm(Herbivory~1, data = savanna)
m1 <- lm(Herbivory~Herb_type, data = savanna)
m2 <- lm(Herbivory~X20yr_fire, data = savanna)
m3.add <- lm(Herbivory~Herb_type+X20yr_fire, data = savanna)
m3.int <- lm(Herbivory~Herb_type:X20yr_fire, data = savanna)
m3.int.elev <- lm(Herbivory~Herb_type:X20yr_fire + poly(elevation, 2), data = savanna)
m4.add <- lm(Herbivory~Herb_type+poly(elevation, 2), data = savanna)
m4.int <- lm(Herbivory~Herb_type:poly(elevation, 2), data = savanna)
m5.add <- lm(Herbivory~Herb_type+Avg_Rain, data = savanna)
m5.int <- lm(Herbivory~Herb_type:Avg_Rain, data = savanna)
m11 <- lm(Herbivory ~ X20yr_fire + poly(elevation, 2), data = savanna)
m11.int <- lm(Herbivory ~ X20yr_fire : poly(elevation, 2), data = savanna)
m12 <- lm(Herbivory ~ Avg_Rain, data = savanna)
m12.add <- lm(Herbivory ~ X20yr_fire + Avg_Rain, data = savanna)
m12.int <- lm(Herbivory ~ X20yr_fire : Avg_Rain, data = savanna)
m13 <- lm(Herbivory ~ poly(elevation, 2) + X20yr_fire + Herb_type, data = savanna)
m14 <- lm(Herbivory ~ poly(elevation, 2) + X20yr_fire : Herb_type, data = savanna)
m15 <- lm(Herbivory ~ poly(elevation, 2) : X20yr_fire + Herb_type, data = savanna)

# The best models here are, m1, m2, m3, m4 in the order of sig. p_value
# KATKA's note: we do not compare the models based on th p values as they have different factors involved, thus complexity
#  which needs to be weighted. THerefore, the best models are selected based on AIC (akaike like criterion)
AICctab(m0, m1, m2, m3.add, m3.int, m3.int.elev, m4.add, m4.int, m5.add, m5.int, m11, m11.int, m12, m12.add, m12.int, m13, m14, m15)
# based on AIC, the best model is m2.int

#another approach woudl be to make a full model (to include both 20 and 10 year fire is too much, so try first 10 and then 20 year)
m.full <- lm(Herbivory ~ Herb_type * poly(elevation, 2) * Avg_Rain * X20yr_fire, data = savanna, na.action = "na.fail")
m.full_dd <- 
  MuMIn::dredge(m.full,
                trace = TRUE)

# observe the best model
m.full_dd %>% 
  as_tibble() %>% 
  filter(delta < 2) %>% 
  View()

# save result table
m.full_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/09_savanna_correlations.csv")

# based on the results from dredge, you have to make all potential models, i.e. those with dAIC up to 2
m16 <- lm(Herbivory~ Herb_type + poly(elevation, 2) + Avg_Rain +  X20yr_fire + Avg_Rain:X20yr_fire, data = savanna)
m16a <- lm(Herbivory~ Herb_type + Avg_Rain:X20yr_fire, data = savanna)
m16b <- lm(Herbivory~ Herb_type + poly(elevation, 2):X20yr_fire, data = savanna)
m17 <- lm(Herbivory~Herb_type + poly(elevation, 2) + Avg_Rain +  X20yr_fire,  data = savanna)
m18 <- lm(Herbivory~Herb_type + poly(elevation, 2) + X20yr_fire,  data = savanna)
m19 <- lm(Herbivory~Avg_Rain + poly(elevation, 2) + Herb_type, data = savanna)
m20<-lm(Herbivory~ Herb_type + poly(elevation, 2) + Avg_Rain +  X20yr_fire + Avg_Rain:X20yr_fire + Herb_type:X20yr_fire, data = savanna)
AICctab(m16, m17, m18, m3.int.elev, m19, m20)
# based on this results model including type of herbivory in interaction with fire regime and addive factor of elevatin is the best

# looking once again at all models should give the same result
AICctab(m0, m1, m2, m3.add, m3.int, m3.int.elev, m4.add, m4.int, m5.add, m5.int, m11, m11.int, m12, m12.add, m12.int, m13, m14, m15,
        m16, m16a, m16b, m17, m18, m19,m20)

# look at the values within the factor "elevation"
savanna$elevation
savanna$Herb_type
savanna$Avg_Rain
# TAKE CARE HERE - for some reason, somehow, the savanna$Herb_type shows Herb_type or Herbivory values only,
# it should be reading levels: chew_perc and sap_min_perc - you  might need to reupload the data aga if this happens
# I did not work out where it is happening but if I slowly rerun everything again it usually dissapears

theme_set(theme_classic())
text_size <-  18

#----------------------------------------------------------#
# 5.2. Make predicted values based on the selected model -----
#----------------------------------------------------------#

# obtain the values predicted by the models with interaction
# first prepare the dataframe
NewDataSavanna3 <- data.frame(elevation = rep(seq.int(1,350), 22),
                              X20yr_fire = rep(seq.int(2,12), 2),
                              Herb_type = rep(c("chewing_herbivory", "mining_herbivory")))
# now you add anothe column with previsted values for herbivory based on the model
NewDataSavanna3$herbivory <- predict(m3.int.elev, newdata = NewDataSavanna3, re.form = NA, type = "response")

# subset the data to chewers only and the certain number of fires (the number of fires is changes simply for visual reasons)
NewDataSavanna_chew <-subset (NewDataSavanna3, Herb_type == "chewing_herbivory")
NewDataSavanna_chew <-subset (NewDataSavanna_chew, X20yr_fire == "2"|X20yr_fire == "5"|X20yr_fire == "9"|X20yr_fire == "12")

# Figure Savanna - correlation plot for chewers
model_plot_07a <- ggplot(data = NewDataSavanna_chew, aes(x=elevation, y = herbivory, color = X20yr_fire))+
  geom_point(alpha = 0.5, pch = 16, size = 4,
             position = position_jitterdodge(
               dodge.width = 0.1,
               jitter.width = 0.1)) +
  ylim(0,6) +
  xlim(100,350) +
  labs(
    x = "Elevation (m)",
    y = expression(paste("Proportion of leaf area eaten by chewers (%)"))) +
  theme(
    text = element_text(size = text_size),
    axis.text=element_text(color="black"), 
    legend.position = "top") +
  theme(axis.line = element_line(colour = "black", size = 1.2, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1.2, linetype = "solid"))
model_plot_07a 

# subset the original data to mienrs only and then select the specific fire regimes
NewDataSavanna_min <-subset (NewDataSavanna3, Herb_type == "mining_herbivory")
NewDataSavanna_min <-subset (NewDataSavanna_min, X20yr_fire == "2"|X20yr_fire == "5"|X20yr_fire == "9"|X20yr_fire == "12")
summary(NewDataSavanna_min)


# Figure Savanna - plot the interaction for the fire, elevation and miners only
model_plot_07b <- ggplot(data = NewDataSavanna_min, aes(x=elevation, y = herbivory, color = X20yr_fire))+
  geom_point(alpha = 0.5, pch = 16, size = 3,
             position = position_jitterdodge(
               dodge.width = 0,
               jitter.width = 0)) +
  ylim(0,6) +
  xlim(100, 350) +
  labs(
    x = "Elevation (m)",
    y = expression(paste("Proportion of leaf area eaten by miners (%)"))) +
  theme(
    text = element_text(size = text_size),
    axis.text=element_text(color="black"), 
    legend.position = "top") +
  scale_colour_gradient(low="darkgreen",high="darkolivegreen2") +
  theme(axis.line = element_line(colour = "black", size = 1.2, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1.2, linetype = "solid"))
model_plot_07b 

# combine both final graphs into 1 picture
require(gridExtra)
model_7<-grid.arrange(model_plot_07a, model_plot_07b , ncol=2)

# save pdf
ggsave(
  "figures/model7_savanna_habitat_correlations.pdf",
  model_7,
  width = PDF_width,
  height = PDF_height,
  units = "in")


#------------------------------------------------------------------------------------------------------
#UNUSED CODES FOR FIGURES WE DECIDED THAT THEY ARE NOT GOOD

#Generate the fitted lines for each "factor" separately based on the model
NewDataSavanna <- data.frame(X20yr_fire = rep(seq.int(1,15), 2),
                             Herb_type = rep(c("chew_perc", "sap_min_perc")))
NewDataSavanna$herbivory <- predict(m3.int, newdata = NewDataSavanna, re.form = NA, type = "response")

summary(savanna)
# Figure Savanna - correlation 
plot_01a <- ggplot(data = savanna, aes(x=X20yr_fire, y = Herbivory, color = Herb_type))+
  geom_point(alpha = 0.5, pch = 16, size = 4,
             position = position_jitterdodge(
               dodge.width = 0.1,
               jitter.width = 0.1)) +
  ylim(0,15) +
  xlim(0,15) +
  geom_line(data = NewDataSavanna, aes(x=X20yr_fire, y = herbivory, color = Herb_type), size = 2) +
  labs(
    x = "Number of fires in last 20 years",
    y = expression(paste("Herbivory"))) +
  scale_fill_manual(values = c( "navyblue", "#1B9E77"))+
  scale_color_manual(values = c( "navyblue", "#1B9E77"))+
  theme(
    text = element_text(size = text_size),
    axis.text=element_text(color="black"), 
    legend.position = "top") +
  theme(axis.line = element_line(colour = "black", size = 1.2, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1.2, linetype = "solid"))
plot_01a 


NewDataSavanna2 <- data.frame(elevation = rep(seq.int(1,350), 2),
                              Herb_type = rep(c("chew_perc", "sap_min_perc")))
NewDataSavanna2$herbivory <- predict(m3., newdata = NewDataSavanna2, re.form = NA, type = "response")

summary(savanna)
# Figure Savanna - correlation 
plot_01b <- ggplot(data = savanna, aes(elevation, y = Herbivory, color = Herb_type))+
  geom_point(alpha = 0.5, pch = 16, size = 3,
             position = position_jitterdodge(
               dodge.width = 0,
               jitter.width = 0)) +
  ylim(0,15) +
  xlim(100, 350) +
  geom_line(data = NewDataSavanna2, aes(elevation, y = herbivory, color = Herb_type), size = 2) +
  labs(
    x = "Elevation (m)",
    y = expression(paste("Herbivory"))) +
  scale_fill_manual(values = c("olivedrab3", "maroon2"))+
  scale_color_manual(values = c("olivedrab3", "maroon2"))+
  theme(
    text = element_text(size = text_size),
    axis.text=element_text(color="black"), 
    legend.position = "top") +
  theme(axis.line = element_line(colour = "black", size = 1.2, linetype = "solid")) +
  theme(axis.ticks = element_line(colour = "black", size = 1.2, linetype = "solid"))
plot_01b 

require(gridExtra)
grid.arrange(plot_01a, plot_01b , ncol=2)


# Using ggplot to create scatter plot with trend line
ggplot(savanna, aes(x = X20yr_fire, y = Herbivory, color = Herb_type)) +
 geom_point(size =4, alpha = 0.4, position = position_jitter(width = 0.1)) + 
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color = "grey", fill = NA, size = 1.5)) +
geom_smooth(method = 'lm') 

#------------------------------------------------------------------------------------------------------
