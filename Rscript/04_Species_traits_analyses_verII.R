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
    chewing_herbivory= mean(chew_herbivory),
    mining_herbivory = mean(sap_miner_herbivory),
    leaf_area = mean(leaf_area),
   total_pct_direct = mean(tot_pct)
  )
summary(dataset_herbivory_fortraits)

# calculate the total herbivory as the sum of herbivory caused by chewers and miners
dataset_herbivory_fortraits <- dataset_herbivory_fortraits %>% mutate(total_herbivory = chewing_herbivory+mining_herbivory)
# this is just for my check, to make sure that the herbivory is correctly counted (it is correct if total_pct_counted = total_pct_direct/100)
# total_pct_counted to be used in further analyses of the factors affecting total herbivory
dataset_herbivory_fortraits <- dataset_herbivory_fortraits %>% mutate(total_pct_counted = total_herbivory/leaf_area)
summary(dataset_herbivory_fortraits)

# write the summarized table
write_csv(
  dataset_herbivory_fortraits,
  "data/output/dataset_herbivory_fortraits.csv")

# read the trait data
dataset_traits <-  
  readxl::read_xlsx("data/input/KTraits.xlsx")
summary(dataset_traits)
str(dataset_traits)

# merge the trait data nad the herbivory data
df_merged <- merge(dataset_traits, dataset_herbivory_fortraits, by='species', all.x=TRUE, all.y=FALSE, sort = TRUE)
summary(df_merged)

# write the merged table
write_csv(
  df_merged,
  "data/output/dataset_herbivory_fortraits.csv")

# for some reason, it was not working weel when I aimed to work directly with the df_merged, so I am saving it and reading again and it works well now
herbivory_traits <- read.csv ("data/output/dataset_herbivory_fortraits.csv")

#----------------------------------------------------------#
# 8.2. Build models for the species-trait analyses - for total herbivory now -----
# trying binomial distribution, and quasibinomial but nothings seems to work reasonably, and models are bit weird, providing no AIC
bin1_Bite <- glm(total_pct_counted~ BiteSizeIndex, family = quasibinomial, data = herbivory_traits)
summary(bin1_Bite)
bin2_Height <- glm(total_pct_counted ~ Max_Height, family = quasibinomial, data = herbivory_traits)
summary(bin2_Height)
bin3_C <- glm(total_pct_counted ~ C,family = quasibinomial, data = herbivory_traits)
summary(bin3_C)
AICctab(bin1_Bite,bin2_Height, bin3_C)

# doing betaregression instead
library(betareg)
m1_Bite <- betareg(total_pct_counted ~ BiteSizeIndex, data = herbivory_traits)
summary(m1_Bite)
fitted(m1_Bite)
m2_Height <- betareg(total_pct_counted ~ Max_Height, data = herbivory_traits)
summary(m2_Height)
fitted(m2_Height)
m3_C <- betareg(total_pct_counted ~ C, data = herbivory_traits)
summary(m3_C)
fitted(m3_C)
m4_P <- betareg(total_pct_counted ~ P_Elsenburg, data = herbivory_traits)
summary(m4_P)
fitted(m4_P)
m5_N <- betareg(total_pct_counted ~ N_Elsenburg, data = herbivory_traits)
summary(m5_N)
fitted(m5_N)
m6_CN <- betareg(total_pct_counted ~ CN_ratio, data = herbivory_traits)
summary(m6_CN)
fitted(m6_CN)
m7_LDMC <- betareg(total_pct_counted ~ LDMC_leaf, data = herbivory_traits)
summary(m7_LDMC)
fitted(m7_LDMC)
m8_SLA <- betareg(total_pct_counted ~ SLA, data = herbivory_traits)
summary(m8_SLA)
fitted(m8_SLA)
m9_FreshW <- betareg(total_pct_counted ~ FreshW, data = herbivory_traits)
summary(m9_FreshW)
fitted(m9_FreshW)
m10_LeafArea <- betareg(total_pct_counted ~ LeafArea, data = herbivory_traits)
fitted(m10_LeafArea)
m_11_NormanI<- betareg(total_pct_counted ~ NormanI, data = herbivory_traits)
m_12_CT<- betareg(total_pct_counted ~ CT, data = herbivory_traits)

AICctab(m1_Bite, m2_Height, m3_C, m4_P, m5_N, m6_CN, m7_LDMC, m8_SLA, m9_FreshW, m10_LeafArea, m_11_NormanI, m_12_CT)
AICctab(         m2_Height, m3_C, m4_P, m5_N, m6_CN, m7_LDMC, m8_SLA, m9_FreshW, m10_LeafArea, m_11_NormanI, m_12_CT)

m13_CPadd <- betareg(total_pct_counted ~ C + P_Elsenburg, data = herbivory_traits)
m14_CNadd <- betareg(total_pct_counted ~ C + N_Elsenburg, data = herbivory_traits)
m15_CLDMCadd <- betareg(total_pct_counted ~ C + LDMC_leaf, data = herbivory_traits)
summary(m15_CLDMCadd)
m16_CCNadd <- betareg(total_pct_counted ~ C +  CN_ratio, data = herbivory_traits)
m17_CfreshWadd <- betareg(total_pct_counted ~ C +  FreshW, data = herbivory_traits)
m18_CLeafArea <- betareg(total_pct_counted ~ C + LeafArea, data = herbivory_traits)

m19_LDMC_SLA_add <- betareg(total_pct_counted ~ LDMC_leaf + SLA, data = herbivory_traits)
m20_LDMC_LA <- betareg(total_pct_counted ~ LDMC_leaf + LeafArea, data = herbivory_traits)
m21_LDMC_P <- betareg(total_pct_counted ~ LDMC_leaf + P_Elsenburg, data = herbivory_traits)
m22_LDMC_N <- betareg(total_pct_counted ~ LDMC_leaf + N_Elsenburg, data = herbivory_traits)

AICctab(m2_Height, m3_C, m4_P, m5_N,m6_CN, m7_LDMC, m8_SLA, m9_FreshW, m10_LeafArea,m_11_NormanI, m_12_CT, 
        m13_CPadd, m14_CNadd, m15_CLDMCadd, m16_CCNadd, m17_CfreshWadd, m18_CLeafArea)
        
AICctab(m2_Height, m3_C, m4_P, m5_N,m6_CN, m7_LDMC, m8_SLA, m9_FreshW, m10_LeafArea, m_11_NormanI, m_12_CT,
        m13_CPadd, m14_CNadd, m15_CLDMCadd, m16_CCNadd, m17_CfreshWadd, m18_CLeafArea, 
        m19_LDMC_SLA_add, m20_LDMC_LA, m21_LDMC_P, m22_LDMC_N)
# LDMC (leaf dry content matter, seems to be the best all the time, then C - carbon,
# and carbon additive to LDMS, then LDMC with phosphrus, leaf area or specific leaf area)

library(buildmer)
if (requireNamespace('glmmTMB')) {model <- buildglmmTMB(total_pct_counted ~ C *  N_Elsenburg *  P_Elsenburg * N_Elsenburg * CN_ratio *
                                                         LDMC_leaf * SLA * FreshW * LeafArea, data = herbivory_traits)}

