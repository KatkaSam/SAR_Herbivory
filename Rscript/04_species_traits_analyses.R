#----------------------------------------------------------#
#
#
#                      SAR Herbivory
#
#                       Config file
#
#             Katerina Sam  - Heveakore Maraia
#                         2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 4.0. Prepare datasets for the species-trait analyses -----
#----------------------------------------------------------#

# Total herbivory data summarization -----
dataset_herbivory <-  
  readxl::read_xlsx("data/input/Herbivory_raw_final_20210609.xlsx")
summary(dataset_herbivory)

# sum total herbivory per tree in percentages
dataset_herbivory_fortraits <-
  dataset_herbivory %>% 
  mutate(
    tot_pct) %>%  # recalculate 
  group_by(species) %>% 
  summarize(
    .groups = "keep",
    #  leaf_area_total = sum(LeafAreaIdeal)/10e3,
    mean_chewing_herbivory= mean(chew_herbivory),
    mean_mining_herbivory = mean(sap_miner_herbivory),
    mean_leaf_area = mean(leaf_area))
summary(dataset_herbivory_fortraits)

# calculate the total herbivory as the sum of herbivory caused by chewers and miners
dataset_herbivory_fortraits <- dataset_herbivory_fortraits %>% mutate(mean_total_herbivory = mean_chewing_herbivory+mean_mining_herbivory)
# standardize the values - make them into 3 new columns
# the vlaues willbe standardized now, but column names are kept short
dataset_herbivory_fortraits$total_herbivory <- (100/dataset_herbivory_fortraits$mean_leaf_area)*dataset_herbivory_fortraits$mean_total_herbivory
dataset_herbivory_fortraits$chewing_herbivory <- (100/dataset_herbivory_fortraits$mean_leaf_area)*dataset_herbivory_fortraits$mean_chewing_herbivory
dataset_herbivory_fortraits$mining_herbivory <- (100/dataset_herbivory_fortraits$mean_leaf_area)*dataset_herbivory_fortraits$mean_mining_herbivory
summary(dataset_herbivory_fortraits)

# write the summarized table
write_csv(
  dataset_herbivory_fortraits,
  "data/output/06_dataset_herbivory_fortraits.csv")

# read the trait data
dataset_traits <-  
  readxl::read_xlsx("data/input/KTraits_20210819.xlsx")
summary(dataset_traits)
str(dataset_traits)

# merge the trait data nad the herbivory data
df_merged <- merge(dataset_traits, dataset_herbivory_fortraits, by='species', all.x=TRUE, all.y=FALSE, sort = TRUE)
summary(df_merged)

# write the merged table
write_csv(
  df_merged,
  "data/output/07_dataset_herbivory_fortraits.csv")

# for some reason, it was not working weel when I aimed to work directly with the df_merged, so I am saving it and reading again and it works well now
herbivory_traits <- read.csv ("data/output/07_dataset_herbivory_fortraits.csv")
summary(herbivory_traits)

#----------------------------------------------------------#
# 4.1. Build models for the species-trait analyses - for total herbivory now  -----
#----------------------------------------------------------#

herbivory_traits$total_herbivory<-herbivory_traits$total_herbivory/100

# trying binomial distribution, and quasibinomial but nothings seems to work reasonably, and models are bit weird, providing no AIC
bin1_Bite <- glm(total_herbivory~ BiteSizeIndex, family = quasibinomial, data = herbivory_traits)
summary(bin1_Bite)
bin2_Height <- glm(total_herbivory ~ Max_Height, family = quasibinomial, data = herbivory_traits)
summary(bin2_Height)
bin3_C <- glm(total_herbivory ~ C,family = quasibinomial, data = herbivory_traits)
summary(bin3_C)
AICctab(bin1_Bite,bin2_Height, bin3_C)

# doing betaregression instead
m0<- betareg(total_herbivory ~ 1, data = herbivory_traits)
m1_Bite <- betareg(total_herbivory ~ BiteSizeIndex, data = herbivory_traits)
summary(m1_Bite)
fitted(m1_Bite)
m2_Height <- betareg(total_herbivory ~ Max_Height, data = herbivory_traits)
summary(m2_Height)
fitted(m2_Height)
m3_C <- betareg(total_herbivory ~ C, data = herbivory_traits)
summary(m3_C)
fitted(m3_C)
m4_P <- betareg(total_herbivory ~ P_Elsenburg, data = herbivory_traits)
summary(m4_P)
fitted(m4_P)
m5_N <- betareg(total_herbivory ~ N_Elsenburg, data = herbivory_traits)
summary(m5_N)
fitted(m5_N)
m6_CN <- betareg(total_herbivory ~ CN_ratio, data = herbivory_traits)
summary(m6_CN)
fitted(m6_CN)
m7_LDMC <- betareg(total_herbivory ~ LDMC_leaf, data = herbivory_traits)
summary(m7_LDMC)
fitted(m7_LDMC)
m8_SLA <- betareg(total_herbivory ~ SLA, data = herbivory_traits)
summary(m8_SLA)
fitted(m8_SLA)
m9_FreshW <- betareg(total_herbivory ~ FreshW, data = herbivory_traits)
summary(m9_FreshW)
fitted(m9_FreshW)
m10_LeafArea <- betareg(total_herbivory ~ LeafArea, data = herbivory_traits)
fitted(m10_LeafArea)
m_11_NormanI<- betareg(total_herbivory ~ NormanI, data = herbivory_traits)
m_12_CT<- betareg(total_herbivory ~ CT, data = herbivory_traits)

AICctab(m0, m1_Bite, m2_Height, m3_C, m4_P, m5_N, m6_CN, m7_LDMC, m8_SLA, m9_FreshW, m10_LeafArea, m_11_NormanI, m_12_CT)
AICctab(         m2_Height, m3_C, m4_P, m5_N, m6_CN, m7_LDMC, m8_SLA, m9_FreshW, m10_LeafArea, m_11_NormanI, m_12_CT)

m13_CPadd <- betareg(total_herbivory ~ C + P_Elsenburg, data = herbivory_traits)
m14_CNadd <- betareg(total_herbivory ~ C + N_Elsenburg, data = herbivory_traits)
m15_CLDMCadd <- betareg(total_herbivory ~ C + LDMC_leaf, data = herbivory_traits)
m16_CCNadd <- betareg(total_herbivory ~ C +  CN_ratio, data = herbivory_traits)
m17_CfreshWadd <- betareg(total_herbivory ~ C +  FreshW, data = herbivory_traits)
m18_CLeafArea <- betareg(total_herbivory ~ C + LeafArea, data = herbivory_traits)
m19_LDMC_SLA_add <- betareg(total_herbivory ~ LDMC_leaf + SLA, data = herbivory_traits)
m20_LDMC_LA <- betareg(total_herbivory ~ LDMC_leaf + LeafArea, data = herbivory_traits)
m21_LDMC_P <- betareg(total_herbivory ~ LDMC_leaf + P_Elsenburg, data = herbivory_traits)
m22_LDMC_N <- betareg(total_herbivory ~ LDMC_leaf + N_Elsenburg, data = herbivory_traits)

AICctab(m2_Height, m3_C, m4_P, m5_N,m6_CN, m7_LDMC, m8_SLA, m9_FreshW, m10_LeafArea,m_11_NormanI, m_12_CT, 
        m13_CPadd, m14_CNadd, m15_CLDMCadd, m16_CCNadd, m17_CfreshWadd, m18_CLeafArea)
        
AICctab(m0, m2_Height, m3_C, m4_P, m5_N,m6_CN, m7_LDMC, m8_SLA, m9_FreshW, m10_LeafArea, m_11_NormanI, m_12_CT,
        m13_CPadd, m14_CNadd, m15_CLDMCadd, m16_CCNadd, m17_CfreshWadd, m18_CLeafArea, 
        m19_LDMC_SLA_add, m20_LDMC_LA, m21_LDMC_P, m22_LDMC_N)
# LDMC (leaf dry content matter, seems to be the best all the time, then C - carbon,
# and carbon additive to LDMS, then LDMC with phosphrus, leaf area or specific leaf area)


#----------------------------------------------------------#
# 4.2. Run all correlations   -----
#----------------------------------------------------------#
head(herbivory_traits)
TH1 <- herbivory_traits[ , -c(2:13, 27:28, 31:35)]
summary(TH1)
TH2 <- TH1 [ , -c(1, 5, 6, 15, 16)]
summary(TH2)

# standardize all data except column with species
TH2$BiteSizeIndex<-scale(TH2$BiteSizeIndex)
TH2$C<-scale(TH2$C)
TH2$P<-scale(TH2$P_Elsenburg)
TH2$N<-scale(TH2$N_Elsenburg)
TH2$CN_ratio<-scale(TH2$CN_ratio)
TH2$CT<-scale(TH2$CT)
TH2$LDMC_leaf<-scale(TH2$LDMC_leaf)
TH2$SLA <-scale(TH2$SLA )
TH2$NormanI<-scale(TH2$NormanI)
TH2$FreshW<-scale(TH2$FreshW)
TH2$LeafArea<-scale(TH2$LeafArea)
TH2$chewing_herbivory<-scale(TH2$chewing_herbivory)
TH2$mining_herbivory<-scale(TH2$mining_herbivory)
TH2$total_herbivory<-scale(TH2$total_herbivory)
summary(TH2) # now all values are equally distributed around 0
TH2 <- TH2 [ , -c(3,4)]

# remove the names of species and reorganize the columns, reorganize, remove again the one we forgot
col_order <- c(10, 11, 12, 1, 2, 13, 14, 3, 4, 5, 6, 7, 8, 9)
TH2 <- TH2[, col_order]
summary(TH2)
TH2 <- TH2 [ , -c(14)]

herb<-cor(TH2)
corrplot(herb, method="number", type="upper")

testRes = cor.mtest(herb, conf.level = 0.95)
testRes

corrplot(herb, type="upper", method="color",p.mat = testRes$p,
         sig.level = 0.05,   addCoef.col = "black", insig = "label_sig",
         tl.col="black", tl.srt=45, diag=FALSE, pch.col = "red", pch.cex = 0.8)

model_plot_5<-corrplot(herb, type="upper", method="color",
                       sig.level = 0.05,   addCoef.col = "black",
                       tl.col="black", tl.srt=45, diag=FALSE, pch.col = "red", pch.cex = 0.8)
## addCoef.col = "black" ## adds coefficient as number to the square
## insig = "label_sig" = remove cells which are not significant 
# we can put this Figure to the main text
# this one has to be extracted manually - can't be saved via script (or I do not know how to do it)
# save the model_polt_5 and we will add the stars manually later on

library("PerformanceAnalytics")
chart.Correlation(TH2, pch=19, histogram=FALSE)
# and this one to supplement as it is not so nice but provides more info

#----------------------------------------------------------#
# 4.3. Run correlation between insect and mammal herbivory only  -----
#----------------------------------------------------------#

library("ggpubr")
summary(herbivory_traits)
herbivory_traits$BiteSizeIndex<-scale(herbivory_traits$BiteSizeIndex)
herbivory_traits$chewing_herbivory<-scale(herbivory_traits$chewing_herbivory)
herbivory_traits$mining_herbivory<-scale(herbivory_traits$mining_herbivory)

plot1<-ggscatter(herbivory_traits, y = "chewing_herbivory", x = "BiteSizeIndex", 
          add = "reg.line", conf.int = TRUE, label = "species", 
          cor.coef = TRUE, cor.method = "pearson",
          repel = TRUE, label.select = c("CEL_AFR", "DOM_ROT", "COR_CAF", "DRY_ARG", "RAW_LUC", 
                                         "BER_ZEY", "ZIZ_MUC", "SCO_ZEY", "GRE_FLA","GYM_MAR", "TAR_PAV", 
                                         "DIC_CIN", "DIO_DIC", "BRA_ILL", "EUC_RAC", "ACA_CAF"), 
          xlab = "Herbivory by mammals", ylab = "Herbivory by chewers",
          add.params = list(color = "blue", fill = "lightgray")) +
  theme_classic() +
  font("xlab", size = 22, color = "black")+
  font("ylab", size = 22, color = "black")
plot1

plot2<-ggscatter(herbivory_traits, y = "mining_herbivory", x = "BiteSizeIndex", 
          add = "reg.line", conf.int = TRUE, label = "species", 
          cor.coef = TRUE, cor.method = "pearson",
          repel = TRUE, label.select = c("DOM_ROT", "COR_CAF", "DRY_ARG", "ECU_NAT",
                                         "BER_ZEY", "ZIZ_MUC", "COM_MOL","SID_INE", "TAR_PAV",
                                         "DIC_CIN", "DIO_DIC", "EUC_RAC", "EUC_NAT", "ACA_CAF", "GYM_MAR"), 
          xlab = "Herbivory by mammals", ylab = "Herbivory by miners",
          add.params = list(color = "blue",
                            fill = "lightgray"))+
  theme_classic() +
  font("xlab", size = 22, color = "black")+
  font("ylab", size = 22, color = "black")
plot2
model_plot_6<-grid.arrange(plot1, plot2, ncol=2)

# save pdf
ggsave(
  "figures/model6_mammal_insect_herbivory.pdf",
  model_plot_6,
  width = PDF_width,
  height = PDF_height,
  units = "in")

