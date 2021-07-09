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
# 8. Prepare datasets for the species-trait analyses -----
#----------------------------------------------------------#

# 8.1 total herbivory data summarization -----
dataset_herbivory <-  
  readxl::read_xlsx("data/input/herbivory_raw_final_20210609.xlsx")
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
  mining_herbivory = mean(chew_herbivory),
  chewing_herbivory = mean(sap_miner_herbivory),
  leaf_area = mean(leaf_area)
  )
summary(dataset_herbivory_fortraits)

dataset_herbivory_fortraits <- dataset_herbivory_fortraits %>% mutate(total_herbivory = chewing_herbivory+mining_herbivory)
summary(dataset_herbivory_fortraits)
  
dataset_traits <-  
  readxl::read_xlsx("data/input/KTraits.xlsx")
summary(dataset_traits)
str(dataset_traits)

write_csv(
  dataset_herbivory_fortraits,
  "data/output/dataset_herbivory_fortraits.csv")

df_merged <- merge(dataset_traits, dataset_herbivory_fortraits, by='species', all.x=TRUE, all.y=FALSE, sort = TRUE)
summary(df_merged)

write_csv(
  df_merged,
  "data/output/dataset_herbivory_fortraits.csv")

herbivory_traits <- read.csv ("data/output/dataset_herbivory_fortraits.csv")

herbivory_traits <- herbivory_traits %>% mutate(binary_herbivory_tot = total_herbivory/(leaf_area-total_herbivory))
summary(herbivory_traits)

m1_Bite <- glm(binary_herbivory_tot~ BiteSizeIndex, family = binomial(link='logit'), data = herbivory_traits)
summary(m1_Bite)
m2_Height <- glm(binary_herbivory_tot~ Max_Height, family = binomial(link='logit'), data = herbivory_traits)
summary(m2_Height)

m1_Bite <- glmmTMB(c(total_perc_mean/100) ~ Max_Height, family=beta_family, data = df_merged)
summary(m1_Bite)
fitted(m1_Bite)
m2_Height <- glmmTMB(c(total_perc_mean/100) ~ Max_Height, family=beta_family, data = df_merged)
summary(m2_Height)
fitted(m2_Height)
m3_C <- glmmTMB(c(total_perc_mean/100) ~ C, family=beta_family, data = df_merged)
summary(m3_C)
fitted(m3_C)
m4_P <- glmmTMB(c(total_perc_mean/100) ~ P_Elsenburg, family=beta_family, data = df_merged)
summary(m4_P)
fitted(m4_P)
m5_N <- glmmTMB(c(total_perc_mean/100) ~ N_Elsenburg, family=beta_family, data = df_merged)
summary(m5_N)
fitted(m5_N)
m6_CN <- glmmTMB(c(total_perc_mean/100) ~ CN_ratio, family=beta_family, data = dataset_traits)
summary(m6_CN)
fitted(m6_CN)
m7_LDMC <- glmmTMB(c(total_perc_mean/100) ~ LDMC_leaf, family=beta_family, data = df_merged)
summary(m7_LDMC)
fitted(m7_LDMC)
m8_SLA <- glmmTMB(c(total_perc_mean/100) ~ SLA, family=beta_family, data = df_merged)
summary(m8_SLA)
fitted(m8_SLA)
m9_FreshW <- glmmTMB(c(total_perc_mean/100) ~ FreshW, family=beta_family, data = df_merged)
summary(m9_FreshW)
fitted(m9_FreshW)
m10_LeafArea <- glmmTMB(c(total_perc_mean/100) ~ LeafArea, family=beta_family, data = df_merged)
summary(m10_LeafArea)
fitted(m10_LeafArea)

AICctab(m1_Bite, m2_Height, m3_C, m4_P, m5_N, m6_CN, m7_LDMC, m8_SLA, m9_FreshW, m10_LeafArea)

m11_CPadd <- glmmTMB(c(total_perc_mean/100) ~ C + P_Elsenburg, family=beta_family, data = dataset_traits)
m12_CNadd <- glmmTMB(c(total_perc_mean/100) ~ C + N_Elsenburg, family=beta_family, data = dataset_traits)
m13_CLDMCadd <- glmmTMB(c(total_perc_mean/100) ~ C + LDMC_leaf, family=beta_family, data = dataset_traits)
m14_CBiteInter <- glmmTMB(c(total_perc_mean/100) ~ C * BiteSizeIndex, family=beta_family, data = dataset_traits)
m14_CBiteadd <- glmmTMB(c(total_perc_mean/100) ~ C + BiteSizeIndex, family=beta_family, data = dataset_traits)
m15_CCNadd <- glmmTMB(c(total_perc_mean/100) ~ C +  CN_ratio, family=beta_family, data = dataset_traits)
m16_CfreshWadd <- glmmTMB(c(total_perc_mean/100) ~ C +  FreshW, family=beta_family, data = dataset_traits)
m17_CLeafArea <- glmmTMB(c(total_perc_mean/100) ~ C + LeafArea, family=beta_family, data = dataset_traits)

AICctab(m1_Bite, m2_Height, m3_C, m4_P, m5_N,m6_CN, m7_LDMC, m8_SLA, m9_FreshW, m10_LeafArea, 
        m11_CPadd, m12_CNadd, m13_CLDMCadd, m14_CBiteInter, m14_CBiteadd, 
        m15_CCNadd, m16_CfreshWadd, m17_CLeafArea)

AICctab(m2_Height, m3_C, m4_P, m5_N,m6_CN, m7_LDMC, m8_SLA, m9_FreshW, m10_LeafArea, 
        m11_CPadd, m12_CNadd, m13_CLDMCadd, 
        m15_CCNadd, m16_CfreshWadd, m17_CLeafArea)


library(buildmer)
if (requireNamespace('glmmTMB')) {model <- buildglmmTMB(c(total_perc_mean/100) ~ C *  N_Elsenburg * BiteSizeIndex * Max_Height * P_Elsenburg * N_Elsenburg * CN_ratio
                                                        * CN_ratio * LDMC_leaf * SLA * FreshW * LeafArea,family=beta_family, data = dataset_traits)}
