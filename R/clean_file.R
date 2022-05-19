library(haven)
library(tidyverse)
library(gsubfn)

SAS_data <- read_sav("~/Transpo Research/Nutrition Environments/SPSS Data/NEMS-S+Grocery+Store+Data+Collection+Tool+-+V2_April+1%2C+2022_14.23.sav")

staff_check <- SAS_data$A_1_1_1

ss_check <- SAS_data$A_2_1_1

# total dataframe for final calculations
all_data <- SAS_data %>%
  mutate("organic_milk_avail" = if_else(MILK_1_7_1 == 1, TRUE, FALSE)) %>%
  mutate("lowfat_milk_avail" = if_else(MILK_2A_1 == 1, TRUE, FALSE)) %>%
  mutate("dairy_milk_avail" = if_else(MILK_1_AVAIL_1 == 1, TRUE, FALSE)) %>%
  mutate("nondairy_milk_avail" = if_else(MILK_1_6_1 == 1, TRUE, FALSE)) %>%
  mutate("lowfat_pint" = MILK_SHELF_LF_1_SPACE_1) %>%
  mutate("lowfat_quart" = MILK_SHELF_LF_1_SPACE_2) %>%
  mutate("lowfat_half_gal" = MILK_SHELF_LF_1_SPACE_3) %>%
  mutate("lowfat_gal" = MILK_SHELF_LF_1_SPACE_4) %>%
  mutate("whole_pint" = MILK_SHELF_WHL_1_SPACE_1) %>%
  mutate("whole_quart" = MILK_SHELF_WHL_1_SPACE_2) %>%
  mutate("whole_half_gal" = MILK_SHELF_WHL_1_SPACE_3) %>%
  mutate("whole_gal" = MILK_SHELF_WHL_1_SPACE_4) %>%
  mutate("lowfat_quart_price" = MILK_PRICE_LFQ_1_1) %>%
  mutate("lowfat_half_gal_price" = MILK_PRICE_LQH_1_1) %>%
  mutate("lowfat_gal_price" = MILK_PRICE_LFG_1_1) %>%
  mutate("whole_quart_price" = MILK_PRICE_WHLQ_1_1) %>%
  mutate("whole_half_gal_price" = MILK_PRICE_WHLH_1_1) %>%
  mutate("whole_gal_price" = MILK_PRICE_WHLG_1_1) %>%
  mutate("lean_beef_varieties" = BEEF_H_CNT)%>%
  mutate("lean_beef_price" = if_else(BEEF_H_GS_3_1 == "", if_else(BEEF_H_LGB_3_1 == "", BEEF_H_GT_3_1, BEEF_H_LGB_3_1), BEEF_H_GS_3_1))%>%
  mutate("regular_beef_price" = if_else(BEEF_R_80_2_PRICE_1 == "", BEEF_R_ALT_2_PRICE_1, BEEF_R_80_2_PRICE_1))%>%
  mutate("varieties_of_fruit" = FRUIT_CNT)%>%
  mutate("varieties_of_vegetables" = VEG_CNT)%>%
  mutate("fat_free_hot_dogs" = if_else(HD_H_OM_1==1, 1, 0), "light_hot_dogs" = if_else(HD_H_OTH_1==1,1,0))%>%
  mutate("lean_wieners_price" = if_else(HD_H_OM_2_1 == "", HD_H_OTH_2_1, HD_H_OM_2_1))%>%
  mutate("lean_wieners_size" = if_else(HD_H_OM_3_1 == "", HD_H_OTH_3_1, HD_H_OM_3_1)) %>%
  mutate("regular_wieners_price" = if_else(HD_R_OM_2_1 == "", HD_R_OTH_2_1, HD_R_OM_2_1)) %>%
  mutate("regular_wieners_size" = if_else(HD_R_OM_3_1 == "", HD_R_OTH_3_1, HD_R_OM_3_1)) %>%
  mutate("frozen_dinners_pref_avail" = if_else(FRZ_PRICE_PREF_LAS_1_REDFAT_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_RTB_1_REDFAT_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_MEA_1_REDFAT_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_LAS_2_REG_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_RTB_2_REG_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_MEA_2_REG_1 != "", 1, 0)) %>%
  mutate("frozen_dinners_oth_avail" = if_else(FRZ_PRICE_OTH_1_1_REDFAT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_2_1_REDFAT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_3_1_REDFAT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_1_2_ALT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_2_2_ALT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_3_2_ALT_2 != "", 1, 0)) %>%
  mutate("frozen_dinner_varieties" = frozen_dinners_pref_avail + frozen_dinners_oth_avail) %>%
  mutate("healthier_frozen_dinners_price_1" = FRZ_PRICE_PREF_LAS_1_REDFAT_2) %>%
  mutate("regular_frozen_dinners_price_1" = FRZ_PRICE_PREF_LAS_2_REG_2) %>%
  mutate("healthier_frozen_dinners_price_2" = FRZ_PRICE_PREF_RTB_1_REDFAT_2) %>%
  mutate("regular_frozen_dinners_price_2" = FRZ_PRICE_PREF_RTB_2_REG_2) %>%
  mutate("healthier_frozen_dinners_price_3" = FRZ_PRICE_PREF_MEA_1_REDFAT_2) %>%
  mutate("regular_frozen_dinners_price_3" = FRZ_PRICE_PREF_MEA_2_REG_2) %>%
  mutate("healthier_frozen_dinners_price_4" = FRZ_PRICE_OTH_1_1_REDFAT_1) %>%
  mutate("regular_frozen_dinners_price_4" = FRZ_PRICE_OTH_1_2_ALT_1) %>%
  mutate("healthier_frozen_dinners_price_5" = FRZ_PRICE_OTH_2_1_REDFAT_1) %>%
  mutate("regular_frozen_dinners_price_5" = FRZ_PRICE_OTH_2_2_ALT_1) %>%
  mutate("healthier_frozen_dinners_price_6" = FRZ_PRICE_OTH_3_1_REDFAT_1) %>%
  mutate("regular_frozen_dinners_price_6" = FRZ_PRICE_OTH_3_2_ALT_1) %>%
  mutate("lowfat_baked_goods" = if_else(BKD_H_BG1_1==1|BKD_H_BGPK_1==1|BKD_H_ENG_1==1, 1, 0))%>%
  mutate("diet_soda_varieties" = if_else(BVG_HS_DC_1_AVAIL==1|BVG_HS_DOTH_1_AVAIL==1, 1, 0))%>%
  mutate("diet_soda_cost" = if_else(BVG_HS_DC_2_PRICE_1 == "", BVG_HS_DOTH_2_PRICE_1, BVG_HS_DC_2_PRICE_1))%>%
  mutate("regular_soda_cost" = if_else(BVG_RS_COK_2_PRICE_1 == "", BVG_RS_OTH_2_PRICE_1, BVG_RS_COK_2_PRICE_1))%>%
  mutate("healthy_juice_varieties" = if_else(BVG_1_5_1 == 1, 1, 0))%>%
  mutate("healthier_juice_drinks_price" = if_else(BVG_HJ_MM_2_PRICE_1 == "", if_else(BVG_HJ_TRO_2_PRICE_1 == "", BVG_HJ_OTH_2_PRICE_1, BVG_HJ_TRO_2_PRICE_1), BVG_HJ_MM_2_PRICE_1))%>%
  mutate("healthier_juice_drinks_comments" = if_else(BVG_HJ_MM_3_COMM_1 == "", if_else(BVG_HJ_TRO_3_COMM_1 == "", BVG_HJ_OTH_3_COMM_1, BVG_HJ_TRO_3_COMM_1), BVG_HJ_MM_3_COMM_1)) %>%
  mutate("regular_juice_drinks_price" = if_else(BVG_RJ_MM_2_PRICE_1 == "", if_else(BVG_RJ_TRO_2_PRICE_1 == "", BVG_RJ_OTH_2_PRICE_1, BVG_RJ_TRO_2_PRICE_1), BVG_RJ_MM_2_PRICE_1)) %>%
  mutate("regular_juice_drinks_comments" = if_else(BVG_RJ_MM_3_COMM_1 == "", if_else(BVG_RJ_TRO_3_COMM_1 == "", BVG_RJ_OTH_3_COMM_1, BVG_RJ_TRO_3_COMM_1), BVG_RJ_MM_3_COMM_1)) %>%
  mutate("varieties_of_whole_grain_bread" = BRD_H_CNT)%>%
  mutate("whole_wheat_bread_price" = if_else(BRD_H_NO_3_PRICE_1 == "", if_else(BRD_H_SLC_3_PRICE_1 == "", BRD_H_OTH_3_PRICE_1, BRD_H_SLC_3_PRICE_1), BRD_H_NO_3_PRICE_1))%>%
  mutate("whole_wheat_bread_size" = if_else(BRD_H_NO_2_OZ_1 == "", if_else(BRD_H_SLC_2_OZ_1 == "", BRD_H_OTH_2_OZ_1, BRD_H_SLC_2_OZ_1), BRD_H_NO_2_OZ_1)) %>%
  mutate("white_bread_price" = if_else(BRD_R_NO_3_PRICE_1 == "", if_else(BRD_R_SLC_3_PRICE_1 == "", BRD_R_OTH_3_PRICE_1, BRD_R_SLC_3_PRICE_1), BRD_R_NO_3_PRICE_1)) %>%
  mutate("white_bread_size" = if_else(BRD_R_NO_2_OZ_1 == "", if_else(BRD_R_SLC_2_OZ_1 == "", BRD_R_OTH_2_OZ_1, BRD_R_SLC_2_OZ_1), BRD_R_NO_2_OZ_1)) %>%
  mutate("lowfat_chip_varieties" = CHIP_H_CNT)%>%
  mutate("lowfat_chips_price" = if_else(CHIP_H_BL_3_PRICE_1 == "", CHIP_H_OTH_3_PRICE_1, CHIP_H_BL_3_PRICE_1)) %>%
  mutate("lowfat_chips_size" = if_else(CHIP_H_BL_1_SIZE_1 == "", CHIP_H_OTH_1_SIZE_1, CHIP_H_BL_1_SIZE_1)) %>%
  mutate("regular_chips_price" = if_else(CHIP_R_LP_3_PRICE_1 == "", CHIP_R_OTH_3_PRICE_1, CHIP_R_LP_3_PRICE_1)) %>%
  mutate("regular_chips_size" = if_else(CHIP_R_LP_1_SIZE_1 == "", CHIP_R_OTH_1_SIZE_1, CHIP_R_LP_1_SIZE_1)) %>%
  mutate("healthier_cereal_varieties" = CRL_H_CNT)%>%
  mutate("healthier_cereal_price" = if_else(CRL_H_CHE_3_PRICE_1 == "", CRL_H_OTH_3_PRICE_1, CRL_H_CHE_3_PRICE_1))%>%
  mutate("regular_cereal_price" = if_else(CRL_R_FCH_3_PRICE_1 == "", CRL_R_OTH_3_PRICE_1, CRL_R_FCH_3_PRICE_1)) %>%
  select(STORE_ID, organic_milk_avail, lowfat_milk_avail, dairy_milk_avail, nondairy_milk_avail,
         lowfat_pint, lowfat_quart, lowfat_half_gal, lowfat_gal, whole_pint, whole_quart,
         whole_half_gal, whole_gal, lowfat_quart_price, lowfat_half_gal_price, lowfat_gal_price,
         whole_quart_price, whole_half_gal_price, whole_gal_price, lean_beef_varieties, lean_beef_price, regular_beef_price,
         varieties_of_fruit, varieties_of_vegetables, fat_free_hot_dogs, light_hot_dogs, lean_wieners_price, lean_wieners_size,
         regular_wieners_price, regular_wieners_size, frozen_dinner_varieties, regular_frozen_dinners_price_1,
         healthier_frozen_dinners_price_1, regular_frozen_dinners_price_2, healthier_frozen_dinners_price_2,
         regular_frozen_dinners_price_3, healthier_frozen_dinners_price_3, regular_frozen_dinners_price_4,
         healthier_frozen_dinners_price_4, regular_frozen_dinners_price_5, healthier_frozen_dinners_price_5,
         regular_frozen_dinners_price_6, healthier_frozen_dinners_price_6, lowfat_baked_goods, diet_soda_varieties,
         diet_soda_cost, regular_soda_cost, healthy_juice_varieties, healthier_juice_drinks_price, healthier_juice_drinks_comments,
         regular_juice_drinks_price, regular_juice_drinks_comments, varieties_of_whole_grain_bread, whole_wheat_bread_price,
         whole_wheat_bread_size, white_bread_price, white_bread_size, lowfat_chip_varieties, lowfat_chips_price, lowfat_chips_size,
         regular_chips_price, regular_chips_size, healthier_cereal_varieties, healthier_cereal_price, regular_cereal_price)




# organize the data into different categories and name columns correctly

#organize and select milk data
milk_data <- SAS_data%>%
  mutate("organic_milk_avail" = if_else(MILK_1_7_1 == 1, TRUE, FALSE)) %>%
  mutate("lowfat_milk_avail" = if_else(MILK_2A_1 == 1, TRUE, FALSE)) %>%
  mutate("dairy_milk_avail" = if_else(MILK_1_AVAIL_1 == 1, TRUE, FALSE)) %>%
  mutate("nondairy_milk_avail" = if_else(MILK_1_6_1 == 1, TRUE, FALSE)) %>%
  mutate("lowfat_pint" = MILK_SHELF_LF_1_SPACE_1) %>%
  mutate("lowfat_quart" = MILK_SHELF_LF_1_SPACE_2) %>%
  mutate("lowfat_half_gal" = MILK_SHELF_LF_1_SPACE_3) %>%
  mutate("lowfat_gal" = MILK_SHELF_LF_1_SPACE_4) %>%
  #mutate("Lowfat_Total" = Lowfat_Pint[1:69] + Lowfat_Quart[1:69] + Lowfat_Half_Gal[1:69] + Lowfat_Gal[1:69]) %>%
  mutate("whole_pint" = MILK_SHELF_WHL_1_SPACE_1) %>%
  mutate("whole_quart" = MILK_SHELF_WHL_1_SPACE_2) %>%
  mutate("whole_half_gal" = MILK_SHELF_WHL_1_SPACE_3) %>%
  mutate("whole_gal" = MILK_SHELF_WHL_1_SPACE_4) %>%
  #mutate("Whole_Total" = MILK_SHELF_WHL_1_SPACE_1 + MILK_SHELF_WHL_1_SPACE_2 + MILK_SHELF_WHL_1_SPACE_3 + MILK_SHELF_WHL_1_SPACE_4) %>%
  mutate("lowfat_quart_price" = MILK_PRICE_LFQ_1_1) %>%
  mutate("lowfat_half_gal_price" = MILK_PRICE_LQH_1_1) %>%
  mutate("lowfat_gal_price" = MILK_PRICE_LFG_1_1) %>%
  mutate("whole_quart_price" = MILK_PRICE_WHLQ_1_1) %>%
  mutate("whole_half_gal_price" = MILK_PRICE_WHLH_1_1) %>%
  mutate("whole_gal_price" = MILK_PRICE_WHLG_1_1) %>%
  select(STORE_ID, organic_milk_avail, lowfat_milk_avail, dairy_milk_avail, nondairy_milk_avail,
         lowfat_pint, lowfat_quart, lowfat_half_gal, lowfat_gal, whole_pint, whole_quart,
         whole_half_gal, whole_gal, lowfat_quart_price, lowfat_half_gal_price, lowfat_gal_price,
         whole_quart_price, whole_half_gal_price, whole_gal_price)

#organize and select beef data
beef_data <- SAS_data %>%
  mutate("lean_beef_varieties" = BEEF_H_CNT)%>%
  mutate("lean_beef_price" = if_else(BEEF_H_GS_3_1 == "", if_else(BEEF_H_LGB_3_1 == "", BEEF_H_GT_3_1, BEEF_H_LGB_3_1), BEEF_H_GS_3_1))%>%
  mutate("regular_beef_price" = if_else(BEEF_R_80_2_PRICE_1 == "", BEEF_R_ALT_2_PRICE_1, BEEF_R_80_2_PRICE_1))%>%
  select(lean_beef_varieties, lean_beef_price, regular_beef_price)

#clean beef data: get rid of dollar signs

#organized and select fruit data
fruit_data <- SAS_data %>%
  mutate("varieties_of_fruit" = if_else(FRUIT_CNT == "", "0", FRUIT_CNT)) %>%
  select(varieties_of_fruit)

#organize and select vegetable data
vegetable_data <- SAS_data %>%
  mutate("varieties_of_vegetables" = VEG_CNT)%>%
  select(varieties_of_vegetables)

#organize and select hot dog data
# still need to get price per ounce
hot_dog_data <- SAS_data %>%
  mutate("fat_free_hot_dogs" = if_else(HD_H_OM_1==1, 1, 0), "light_hot_dogs" = if_else(HD_H_OTH_1==1,1,0))%>%
  mutate("lean_wieners_price" = if_else(HD_H_OM_2_1 == "", HD_H_OTH_2_1, HD_H_OM_2_1))%>%
  mutate("lean_wieners_size" = if_else(HD_H_OM_3_1 == "", HD_H_OTH_3_1, HD_H_OM_3_1)) %>%
  mutate("regular_wieners_price" = if_else(HD_R_OM_2_1 == "", HD_R_OTH_2_1, HD_R_OM_2_1)) %>%
  mutate("regular_wieners_size" = if_else(HD_R_OM_3_1 == "", HD_R_OTH_3_1, HD_R_OM_3_1)) %>%
  select(fat_free_hot_dogs, light_hot_dogs, lean_wieners_price, lean_wieners_size, regular_wieners_price, regular_wieners_size)

#organize and select frozen dinner data
frozen_dinners_data <- SAS_data %>%
  mutate("frozen_dinners_pref_avail" = if_else(FRZ_PRICE_PREF_LAS_1_REDFAT_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_RTB_1_REDFAT_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_MEA_1_REDFAT_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_LAS_2_REG_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_RTB_2_REG_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_MEA_2_REG_1 != "", 1, 0)) %>%
  mutate("frozen_dinners_oth_avail" = if_else(FRZ_PRICE_OTH_1_1_REDFAT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_2_1_REDFAT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_3_1_REDFAT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_1_2_ALT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_2_2_ALT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_3_2_ALT_2 != "", 1, 0)) %>%
  mutate("frozen_dinner_varieties" = frozen_dinners_pref_avail + frozen_dinners_oth_avail) %>%
  mutate("healthier_frozen_dinners_price_1" = FRZ_PRICE_PREF_LAS_1_REDFAT_2) %>%
  mutate("regular_frozen_dinners_price_1" = FRZ_PRICE_PREF_LAS_2_REG_2) %>%
  mutate("healthier_frozen_dinners_price_2" = FRZ_PRICE_PREF_RTB_1_REDFAT_2) %>%
  mutate("regular_frozen_dinners_price_2" = FRZ_PRICE_PREF_RTB_2_REG_2) %>%
  mutate("healthier_frozen_dinners_price_3" = FRZ_PRICE_PREF_MEA_1_REDFAT_2) %>%
  mutate("regular_frozen_dinners_price_3" = FRZ_PRICE_PREF_MEA_2_REG_2) %>%
  mutate("healthier_frozen_dinners_price_4" = FRZ_PRICE_OTH_1_1_REDFAT_1) %>%
  mutate("regular_frozen_dinners_price_4" = FRZ_PRICE_OTH_1_2_ALT_1) %>%
  mutate("healthier_frozen_dinners_price_5" = FRZ_PRICE_OTH_2_1_REDFAT_1) %>%
  mutate("regular_frozen_dinners_price_5" = FRZ_PRICE_OTH_2_2_ALT_1) %>%
  mutate("healthier_frozen_dinners_price_6" = FRZ_PRICE_OTH_3_1_REDFAT_1) %>%
  mutate("regular_frozen_dinners_price_6" = FRZ_PRICE_OTH_3_2_ALT_1) %>%
  select(frozen_dinner_varieties, regular_frozen_dinners_price_1, healthier_frozen_dinners_price_1, regular_frozen_dinners_price_2,
         healthier_frozen_dinners_price_2, regular_frozen_dinners_price_3, healthier_frozen_dinners_price_3, regular_frozen_dinners_price_4, healthier_frozen_dinners_price_4,
         regular_frozen_dinners_price_5, healthier_frozen_dinners_price_5, regular_frozen_dinners_price_6, healthier_frozen_dinners_price_6)

4#organize and select baked goods data
baked_goods_data <- SAS_data %>%
  mutate("lowfat_baked_goods" = if_else(BKD_H_BG1_1==1|BKD_H_BGPK_1==1|BKD_H_ENG_1==1, 1, 0))%>%
  select(lowfat_baked_goods)

#organize and select soda data
soda_data <- SAS_data %>%
  mutate("diet_soda_varieties" = if_else(BVG_HS_DC_1_AVAIL==1|BVG_HS_DOTH_1_AVAIL==1, 1, 0))%>%
  mutate("diet_soda_cost" = if_else(BVG_HS_DC_2_PRICE_1 == "", BVG_HS_DOTH_2_PRICE_1, BVG_HS_DC_2_PRICE_1))%>%
  mutate("regular_soda_cost" = if_else(BVG_RS_COK_2_PRICE_1 == "", BVG_RS_OTH_2_PRICE_1, BVG_RS_COK_2_PRICE_1))%>%
  select(diet_soda_varieties, diet_soda_cost, regular_soda_cost)

#organize and select juice data
juice_data <- SAS_data %>%
  mutate("healthy_juice_varieties" = if_else(BVG_1_5_1 == 1, 1, 0))%>%
  mutate("healthier_juice_drinks_price" = if_else(BVG_HJ_MM_2_PRICE_1 == "", if_else(BVG_HJ_TRO_2_PRICE_1 == "", BVG_HJ_OTH_2_PRICE_1, BVG_HJ_TRO_2_PRICE_1), BVG_HJ_MM_2_PRICE_1))%>%
  mutate("healthier_juice_drinks_comments" = if_else(BVG_HJ_MM_3_COMM_1 == "", if_else(BVG_HJ_TRO_3_COMM_1 == "", BVG_HJ_OTH_3_COMM_1, BVG_HJ_TRO_3_COMM_1), BVG_HJ_MM_3_COMM_1)) %>%
  mutate("regular_juice_drinks_price" = if_else(BVG_RJ_MM_2_PRICE_1 == "", if_else(BVG_RJ_TRO_2_PRICE_1 == "", BVG_RJ_OTH_2_PRICE_1, BVG_RJ_TRO_2_PRICE_1), BVG_RJ_MM_2_PRICE_1)) %>%
  mutate("regular_juice_drinks_comments" = if_else(BVG_RJ_MM_3_COMM_1 == "", if_else(BVG_RJ_TRO_3_COMM_1 == "", BVG_RJ_OTH_3_COMM_1, BVG_RJ_TRO_3_COMM_1), BVG_RJ_MM_3_COMM_1)) %>%
  select(healthy_juice_varieties, healthier_juice_drinks_price, healthier_juice_drinks_comments, regular_juice_drinks_price, regular_juice_drinks_comments)

#clean juice data: get rid of dollar signs, organize comments and sizing, find difference
juice_data <- as.data.frame(lapply(juice_data, gsub, pattern='\\$',replacement=''))
juice_data <- juice_data %>% #need to figure out sizing from the comments...
  mutate("price_difference" = if_else(healthier_juice_drinks_price == "" | regular_juice_drinks_price == "" | is.na(healthier_juice_drinks_price) | is.na(regular_juice_drinks_price), 1, as.numeric(healthier_juice_drinks_price)-as.numeric(regular_juice_drinks_price)))

#organize and select bread data
bread_data <- SAS_data %>%
  mutate("varieties_of_whole_grain_bread" = BRD_H_CNT)%>%
  mutate("whole_wheat_bread_price" = if_else(BRD_H_NO_3_PRICE_1 == "", if_else(BRD_H_SLC_3_PRICE_1 == "", BRD_H_OTH_3_PRICE_1, BRD_H_SLC_3_PRICE_1), BRD_H_NO_3_PRICE_1))%>%
  mutate("whole_wheat_bread_size" = if_else(BRD_H_NO_2_OZ_1 == "", if_else(BRD_H_SLC_2_OZ_1 == "", BRD_H_OTH_2_OZ_1, BRD_H_SLC_2_OZ_1), BRD_H_NO_2_OZ_1)) %>%
  mutate("white_bread_price" = if_else(BRD_R_NO_3_PRICE_1 == "", if_else(BRD_R_SLC_3_PRICE_1 == "", BRD_R_OTH_3_PRICE_1, BRD_R_SLC_3_PRICE_1), BRD_R_NO_3_PRICE_1)) %>%
  mutate("white_bread_size" = if_else(BRD_R_NO_2_OZ_1 == "", if_else(BRD_R_SLC_2_OZ_1 == "", BRD_R_OTH_2_OZ_1, BRD_R_SLC_2_OZ_1), BRD_R_NO_2_OZ_1)) %>%
  select(varieties_of_whole_grain_bread, whole_wheat_bread_price, whole_wheat_bread_size, white_bread_price, white_bread_size)

#clean bread data: get rid of dollar sign and letters, find difference
bread_data <- as.data.frame(lapply(bread_data, gsub, pattern='\\$',replacement=''))
bread_data <- as.data.frame(lapply(bread_data, gsub, pattern = ' oz.', replacement = ''))
bread_data <- as.data.frame(lapply(bread_data, gsub, pattern = 'NA', replacement = '1'))
bread_data <- bread_data %>%
  mutate("price_difference" = if_else(whole_wheat_bread_price == "" | white_bread_price == "" | is.na(whole_wheat_bread_price) | is.na(whole_wheat_bread_price), 1, as.numeric(whole_wheat_bread_price)/as.numeric(whole_wheat_bread_size) - as.numeric(white_bread_price)/as.numeric(white_bread_size)))

#organize and select chip data
chips_data <- SAS_data %>%
  mutate("lowfat_chip_varieties" = CHIP_H_CNT)%>%
  mutate("lowfat_chips_price" = if_else(CHIP_H_BL_3_PRICE_1 == "", CHIP_H_OTH_3_PRICE_1, CHIP_H_BL_3_PRICE_1)) %>%
  mutate("lowfat_chips_size" = if_else(CHIP_H_BL_1_SIZE_1 == "", CHIP_H_OTH_1_SIZE_1, CHIP_H_BL_1_SIZE_1)) %>%
  mutate("regular_chips_price" = if_else(CHIP_R_LP_3_PRICE_1 == "", CHIP_R_OTH_3_PRICE_1, CHIP_R_LP_3_PRICE_1)) %>%
  mutate("regular_chips_size" = if_else(CHIP_R_LP_1_SIZE_1 == "", CHIP_R_OTH_1_SIZE_1, CHIP_R_LP_1_SIZE_1)) %>%
  select(lowfat_chip_varieties, lowfat_chips_price, lowfat_chips_size, regular_chips_price, regular_chips_size)

#clean chip data: convert fraction to decimal, get rid of dollar sign, find difference
chips_data$lowfat_chips_size <- gsubfn("(\\d+) (\\d+)", ~ as.numeric(x) + as.numeric(y),
                                       gsubfn("(\\d+)/(\\d+)", ~ as.numeric(x)/as.numeric(y), chips_data$lowfat_chips_size))
chips_data$regular_chips_size <- gsubfn("(\\d+) (\\d+)", ~ as.numeric(x) + as.numeric(y),
                                        gsubfn("(\\d+)/(\\d+)", ~ as.numeric(x)/as.numeric(y), chips_data$regular_chips_size))
chips_data <- as.data.frame(lapply(chips_data,gsub,pattern='\\$',replacement=''))
chips_data <- as.data.frame(lapply(chips_data, gsub, pattern = 'g', replacement = '')) # need to change one gram value to ounce
chips_data <- chips_data %>%
  mutate("price_difference" = if_else(lowfat_chips_price == ""| regular_chips_price == "" |is.na(lowfat_chips_price) | is.na(regular_chips_price), 1, as.numeric(lowfat_chips_price)/as.numeric(lowfat_chips_size) - as.numeric(regular_chips_price)/as.numeric(regular_chips_size)))

#organize and select cereal data
cereal_data <- SAS_data %>%
  mutate("healthier_cereal_varieties" = CRL_H_CNT)%>%
  mutate("healthier_cereal_price" = if_else(CRL_H_CHE_3_PRICE_1 == "", CRL_H_OTH_3_PRICE_1, CRL_H_CHE_3_PRICE_1))%>%
  mutate("regular_cereal_price" = if_else(CRL_R_FCH_3_PRICE_1 == "", CRL_R_OTH_3_PRICE_1, CRL_R_FCH_3_PRICE_1)) %>%
  select(healthier_cereal_varieties, healthier_cereal_price, regular_cereal_price)






