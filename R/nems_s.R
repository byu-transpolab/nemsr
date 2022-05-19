
# This is my function.

calculate_score <- function(clean_data, detail = FALSE) {
  scores <- data.frame("ID" = clean_data$STORE_ID)
  scores <- scores %>%
    mutate("milk_avail_score" = milk_avail(as.numeric(clean_data$lowfat_gal), as.numeric(clean_data$whole_gal))) %>%
    mutate("fruit_avail_score" = fruit_avail(as.numeric(clean_data$varieties_of_fruit))) %>%
    mutate("vegetable_avail_score" = vegetable_avail(as.numeric(clean_data$varieties_of_vegetables))) %>%
    mutate("ground_beef_avail_score" = ground_beef_avail(as.numeric(clean_data$lean_beef_varieties))) %>%
    mutate("hot_dog_avail_score" = hot_dog_avail(as.numeric(clean_data$fat_free_hot_dogs), as.numeric(clean_data$light_hot_dogs))) %>%
    mutate("frozen_dinners_avail_score" = frozen_dinners_avail(as.numeric(clean_data$frozen_dinner_varieties))) %>%
    mutate("baked_goods_avail_score" = baked_goods_avail(as.numeric(clean_data$lowfat_baked_goods))) %>%
    mutate("soda_avail_score" = soda_avail(as.numeric(clean_data$diet_soda_varieties))) %>%
    mutate("juice_drinks_avail_score" = juice_drinks_avail(as.numeric(clean_data$healthy_juice_varieties))) %>%
    mutate("bread_avail_score" = bread_avail(as.numeric(clean_data$varieties_of_whole_grain_bread))) %>%
    mutate("chip_avail_score" = chips_avail(as.numeric(clean_data$lowfat_chip_varieties))) %>%
    mutate("cereal_avail_score" = cereal_avail(as.numeric(clean_data$healthier_cereal_varieties))) %>%
    mutate("Total_Availability_Score" = milk_avail_score + fruit_avail_score + vegetable_avail_score + ground_beef_avail_score +
                                                        hot_dog_avail_score + frozen_dinners_avail_score + baked_goods_avail_score +
                                                        soda_avail_score + juice_drinks_avail_score + bread_avail_score + chip_avail_score +
                                                        cereal_avail_score) %>%
    mutate("milk_cost_score" = milk_cost(as.numeric(clean_data$lowfat_gal_price), as.numeric(clean_data$whole_gal_price))) %>%
    mutate("ground_beef_cost_score" = ground_beef_cost(as.numeric(clean_data$lean_beef_price), as.numeric(clean_data$regular_beef_price))) %>%
    mutate("wieners_cost_score" = wieners_cost(as.numeric(clean_data$lean_wieners_price), as.numeric(clean_data$regular_wieners_price))) %>%
    mutate("frozen_dinners_cost_score" = frozen_dinners_cost(as.numeric(clean_data$healthier_frozen_dinners_price_1), as.numeric(clean_data$regular_frozen_dinners_price_1))) %>%
    mutate("baked_goods_cost_score" = baked_goods_cost(as.numeric(clean_data$healthier_baked_goods_cost))) %>%
    mutate("soda_cost_score" = soda_cost(as.numeric(clean_data$diet_soda_cost), as.numeric(clean_data$regular_soda_cost))) %>%
    mutate("juice_cost_score" = juice_cost(as.numeric(clean_data$healthier_juice_drinks_price), as.numeric(clean_data$regular_juice_drinks_price))) %>%
    mutate("juice_drinks_cost_score" = juice_drinks_cost(as.numeric(clean_data$diet_soda_cost), as.numeric(clean_data$regular_soda_cost), as.numeric(clean_data$healthier_juice_drinks_price), as.numeric(clean_data$regular_juice_drinks_price))) %>%
    mutate("bread_cost_score" = bread_cost(as.numeric(clean_data$whole_wheat_bread_price)/as.numeric(clean_data$whole_wheat_bread_size), as.numeric(clean_data$white_bread_price)/as.numeric(clean_data$white_bread_size))) %>%
    mutate("chips_cost_score" = chips_cost(as.numeric(clean_data$lowfat_chips_price)/as.numeric(clean_data$lowfat_chips_size), as.numeric(clean_data$regular_chips_price)/as.numeric(clean_data$regular_chips_size))) %>%
    mutate("cereal_cost_score" = cereal_cost(as.numeric(clean_data$healthier_cereal_price), as.numeric(clean_data$regular_cereal_price))) %>%
    replace_na(list(milk_cost_score = 0, ground_beef_cost_score = 0, wieners_cost_score = 0, frozen_dinners_cost_score = 0,
                    soda_cost_score = 0, juice_cost_score = 0, juice_drinks_cost_score = 0, bread_cost_score = 0, chips_cost_score = 0,
                    cereal_cost_score = 0)) %>%
    mutate("Total_Cost_Score" = milk_cost_score + ground_beef_cost_score + wieners_cost_score + frozen_dinners_cost_score +
             soda_cost_score + juice_cost_score + juice_drinks_cost_score + bread_cost_score + chips_cost_score + cereal_cost_score)

  if(detail == TRUE) {select(scores, c(ID, milk_avail_score, fruit_avail_score, vegetable_avail_score, ground_beef_avail_score, hot_dog_avail_score,
                                  frozen_dinners_avail_score, baked_goods_avail_score, soda_avail_score, juice_drinks_avail_score,
                                   bread_avail_score, chip_avail_score, cereal_avail_score, Total_Availability_Score, milk_cost_score,
                                  ground_beef_cost_score, wieners_cost_score, frozen_dinners_cost_score, soda_cost_score, juice_cost_score,
                                  juice_drinks_cost_score, bread_cost_score, chips_cost_score, cereal_cost_score, Total_Cost_Score))}
  else {select(scores, c(ID, Total_Availability_Score))}

}

#' Read a NEMS-S raw data file
#'
#' @param file Path to the file. Can be a `.csv` or `.sav` exported from Qualtrics survey.
#'
#'
read_nemss <- function(file) {

  # TODO: Can we set this up to do an if statement for multiple file types?
  SAS_data <- if (get_ext(file) == "csv"){
    readr::read_csv(file)
  } else if (get_ext(file) == "sav") {
    haven::read_sav(file)
  }

  #rename and sort data
  all_data <- SAS_data %>%
    mutate("organic_milk_avail" = if_else(MILK_1_7_1 == 1, TRUE, FALSE)) %>%
    mutate("lowfat_milk_avail" = if_else(MILK_2A_1 == 1, TRUE, FALSE)) %>%
    mutate("dairy_milk_avail" = if_else(MILK_1_AVAIL_1 == 1, TRUE, FALSE)) %>%
    mutate("nondairy_milk_avail" = if_else(MILK_1_6_1 == 1, TRUE, FALSE)) %>%
    mutate("lowfat_pint" = MILK_SHELF_LF_1_SPACE_1) %>%
    mutate("lowfat_quart" = MILK_SHELF_LF_1_SPACE_2) %>%
    mutate("lowfat_half_gal" = MILK_SHELF_LF_1_SPACE_3) %>%
    mutate("lowfat_gal" = if_else(MILK_SHELF_LF_1_SPACE_4 == "", "0", MILK_SHELF_LF_1_SPACE_4)) %>%
    mutate("whole_pint" = MILK_SHELF_WHL_1_SPACE_1) %>%
    mutate("whole_quart" = MILK_SHELF_WHL_1_SPACE_2) %>%
    mutate("whole_half_gal" = MILK_SHELF_WHL_1_SPACE_3) %>%
    mutate("whole_gal" = if_else(MILK_SHELF_WHL_1_SPACE_4 == "", "0", MILK_SHELF_WHL_1_SPACE_4)) %>%
    mutate("lowfat_quart_price" = MILK_PRICE_LFQ_1_1) %>%
    mutate("lowfat_half_gal_price" = MILK_PRICE_LQH_1_1) %>%
    mutate("lowfat_gal_price" = MILK_PRICE_LFG_1_1) %>%
    mutate("whole_quart_price" = MILK_PRICE_WHLQ_1_1) %>%
    mutate("whole_half_gal_price" = MILK_PRICE_WHLH_1_1) %>%
    mutate("whole_gal_price" = MILK_PRICE_WHLG_1_1) %>%
    mutate("lean_beef_varieties" = BEEF_H_CNT)%>%
    mutate("lean_beef_price" = if_else(BEEF_H_GS_3_1 == "", if_else(BEEF_H_LGB_3_1 == "", BEEF_H_GT_3_1, BEEF_H_LGB_3_1), BEEF_H_GS_3_1))%>%
    mutate("regular_beef_price" = if_else(BEEF_R_80_2_PRICE_1 == "", BEEF_R_ALT_2_PRICE_1, BEEF_R_80_2_PRICE_1))%>%
    mutate("varieties_of_fruit" = if_else(FRUIT_CNT == "", "0", FRUIT_CNT))%>%
    mutate("varieties_of_vegetables" = if_else(VEG_CNT == "", "0", VEG_CNT))%>%
    mutate("fat_free_hot_dogs" = if_else(HD_H_OM_1==1, 1, 0), "light_hot_dogs" = if_else(HD_H_OTH_1==1,1,0))%>%
    mutate("lean_wieners_price" = if_else(HD_H_OM_2_1 == "", HD_H_OTH_2_1, HD_H_OM_2_1))%>%
    mutate("lean_wieners_size" = if_else(HD_H_OM_3_1 == "", HD_H_OTH_3_1, HD_H_OM_3_1)) %>%
    mutate("regular_wieners_price" = if_else(HD_R_OM_2_1 == "", HD_R_OTH_2_1, HD_R_OM_2_1)) %>%
    mutate("regular_wieners_size" = if_else(HD_R_OM_3_1 == "", HD_R_OTH_3_1, HD_R_OM_3_1)) %>%
    mutate("frozen_dinners_pref_avail" = if_else(FRZ_PRICE_PREF_LAS_1_REDFAT_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_RTB_1_REDFAT_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_MEA_1_REDFAT_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_LAS_2_REG_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_RTB_2_REG_1 != "", 1, 0) + if_else(FRZ_PRICE_PREF_MEA_2_REG_1 != "", 1, 0)) %>%
    mutate("frozen_dinners_oth_avail" = if_else(FRZ_PRICE_OTH_1_1_REDFAT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_2_1_REDFAT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_3_1_REDFAT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_1_2_ALT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_2_2_ALT_2 != "", 1, 0) + if_else(FRZ_PRICE_OTH_3_2_ALT_2 != "", 1, 0)) %>%
    mutate("frozen_dinner_varieties" = (frozen_dinners_pref_avail + frozen_dinners_oth_avail)/2) %>%
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
    mutate("lowfat_baked_goods" = if_else(BKD_H_BG1_1==1|BKD_H_BGPK_1==1|BKD_H_ENG_1==1|BKD_H_LFM_1==1, 1, 0))%>%
    mutate("lowfat_baked_goods_cost" = if_else(BKD_H_BG1_2_1 != "", BKD_H_BG1_2_1, if_else()))
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
           regular_chips_price, regular_chips_size, healthier_cereal_varieties, healthier_cereal_price, regular_cereal_price) %>%
    replace_na(list(lean_beef_varieties = 0, fat_free_hot_dogs = 0, light_hot_dogs = 0, frozen_dinner_varieties = 0, lowfat_baked_goods = 0,
                    diet_soda_varieties = 0, lowfat_chip_varieties = 0, varieties_of_whole_grain_bread = 0, healthier_cereal_varieties = 0,
                    lowfat_gal = 0, whole_gal = 0, whole_wheat_bread_price = 0, white_bread_price = 0))

  #clean data
  all_data$lowfat_chips_size <- gsubfn("(\\d+) (\\d+)", ~ as.numeric(x) + as.numeric(y),
                                         gsubfn("(\\d+)/(\\d+)", ~ as.numeric(x)/as.numeric(y), all_data$lowfat_chips_size))
  all_data$lowfat_chips_size <- gsub("g","",all_data$lowfat_chips_size)
  all_data$regular_chips_size <- gsubfn("(\\d+) (\\d+)", ~ as.numeric(x) + as.numeric(y),
                                          gsubfn("(\\d+)/(\\d+)", ~ as.numeric(x)/as.numeric(y), all_data$regular_chips_size))
  all_data$regular_chips_size <- gsub("g","",all_data$regular_chips_size)
  clean_data <- as.data.frame(lapply(all_data, gsub, pattern='\\$',replacement=''))
  clean_data <- as.data.frame(lapply(clean_data, gsub, pattern = ' oz.', replacement = ''))
  clean_data <- as.data.frame(lapply(clean_data, gsub, pattern = 'NA', replacement = '1'))

# figure out how to clean the rest of the data errors...





}


# Availability (30 points)
#'Compute milk availability points
#'
#' This function takes in the number of lowfat and whole milk varieties at a given store and returns the NEMS-S points associated with the availability.
#'
#' @details This function implements the scoring method described in Measure 1 of the NEMS-S Protocol. "Low-fat milk" is considered skim milk or 1 percent fat, whichever is available.
#' @param lowfat_milk_varieties The number of low fat milk cartons available.
#' @param whole_milk_varieties The number of whole milk cartons available.
#' @return The NEMS-S points associated with milk availability.
#' @examples
#' lowfat_milk_varieties <- rnorm(10,,)
#' whole_milk_varieties <- rnorm(10,,)
#' milk_avail(lowfat_milk_varieties, whole_milk_varieties)
milk_avail <- function(lowfat_milk_varieties,whole_milk_varieties) {
  case_when(
    # 0 points if no lowfat/skim milk is available
    lowfat_milk_varieties == 0 ~ 0,
    # 1 additional point if >50% ratio of lowest-fat to whole milk
    lowfat_milk_varieties / whole_milk_varieties > 0.5 ~ 3,
    # 2 points if lowfat/skim milk is available
    lowfat_milk_varieties > 0 ~ 2,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute fruit availability points
#'
#' This function takes in the number of varieties of fruit at a given store and returns the NEMS-S points associated with the availability.
#'
#' @details This function implements the scoring method described in Measure 2 of the NEMS-S Protocol.
#' @param varieties_of_fruit The number of fruit types available.
#' @return The NEMS-S points associated with fruit availability.
#' @examples
#' varieties_of_fruit <- sample(1:10, 10)
#' fruit_avail(varieties_of_fruit)
fruit_avail <- function(varieties_of_fruit) {
  case_when(
    # 0 points if 0 varieties of fruit are available
    varieties_of_fruit == 0 ~ 0,
    # 1 point if <5 varieties are available
    varieties_of_fruit < 5 ~ 1,
    # 2 points if 5-9 varieties are available
    varieties_of_fruit >= 5 & varieties_of_fruit < 10 ~ 2,
    # 3 points if all 10 varieties are available
    varieties_of_fruit == 10 ~ 3,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute vegetable availability points
#'
#' This function takes in the number of varieties of vegetables at a given store and returns the NEMS-S points associated with the availability.
#'
#' @details This function implements the scoring method described in Measure 3 of the NEMS-S Protocol.
#' @param varieties_of_vegetables Number of types of vegetables available.
#' @return The NEMS-S points associated with vegetables availability.
#' @examples
#' varieties_of_vegetables <- sample(1:10, 10)
#' vegetable_avail(varieties_of_vegetables)
vegetable_avail <- function(varieties_of_vegetables) {
  case_when(
    # 0 points if 0 varieties of vegetables that are available
    varieties_of_vegetables == 0 ~ 0,
    # 1 point if <5 varieties are available
    varieties_of_vegetables < 5 ~ 1,
    # 2 points if 5-9 varieties are available
    varieties_of_vegetables >= 5 & varieties_of_vegetables < 10 ~ 2,
    # 3 points if all 10 varieties are available
    varieties_of_vegetables == 10 ~ 3,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute the availability points associated with ground beef
#'
#' This function takes in the number of varieties of lean and regular fat beef and returns the NEMS-S points associated with availability.
#'
#' @details This function implements the scoring method described in Measure 4 of the NEMS-S Protocol. "Standard ground beef" is 80 percent lean and 20 percent fat by weight. "Lean ground beef" is 90 percent lean and 10 percent fat (or less fat percentage) by weight.
#' @param lean_beef_varieties The number of lean beef varieties (<10 percent fat).
#' @return The NEMS-S points associated with ground beef availability.
#' @examples
#' lean_beef_varieties <- sample(1:4, 10)
#' ground_beef_avail(lean_beef_varieties)
ground_beef_avail <- function(lean_beef_varieties) {
  case_when(
    # 0 points if there is not a lean beef option
    lean_beef_varieties == 0 ~ 0,
    # 2 points if there is a lean beef option
    lean_beef_varieties == 1 ~ 2,
    # 3 points if there are 2-3 varieties of lean beef
    lean_beef_varieties >= 2 & lean_beef_varieties < 4 ~ 3,
    # 4 points if there are >3 varieties of lean beef
    lean_beef_varieties > 3 ~ 4,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute the availability points associated with hot dogs
#'
#' This function takes in the number of varieties of fat free and light hot dogs and returns the NEMS-S points associated with availability.
#'
#' @details This function implements the scoring method described in Measure 5 of the NEMS-S Protocol. "Fat free hot dogs" have 0 grams of fat per serving, and "light hot dogs" have less than or equal to 7 grams of fat per serving.
#' @param fat_free_hot_dogs The number of varieties of fat free hot dogs (0g fat per serving).
#' @param light_hot_dogs The number of varieties of light hot dogs (<=7g fat per serving).
#' @return the NEMS-S points associated with availability of fat free and light fat hot dogs.
#' @examples
#' fat_free_hot_dogs <- sample()
#' light_hot_dogs <- sample()
#' hot_dog_avail(fat_free_hot_dogs, light_hot_dogs)
hot_dog_avail <- function(fat_free_hot_dogs, light_hot_dogs) {
  case_when(
    # 0 points if no fat free or light hot dogs are available
    fat_free_hot_dogs == 0 & light_hot_dogs == 0 ~ 0,
    # 2 points if there are fat free hot dogs
    fat_free_hot_dogs > 0 ~ 2,
    # 1 point if there are not fat free but there are light hot dogs
    fat_free_hot_dogs == 0 & light_hot_dogs > 0 ~ 1,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute the availability points associated with frozen dinners
#'
#' This function takes in the number of varieties of frozen dinners and returns the NEMS-S points associated with availability.
#'
#' @details This function implements the scoring method described in Measure 6 of the NEMS-S Protocol.
#' @param frozen_dinner_varieties The number of frozen dinner varieties offered.
#' @return The NEMS-S points associated with availability of frozen dinners.
#' @examples
#' frozen_dinner_varieties <- sample(0:3, 10)
#' frozen_dinners_avail(frozen_dinner_varieties)
frozen_dinners_avail <- function(frozen_dinner_varieties) {
  case_when(
    # 3 points if there are 3 options
    frozen_dinner_varieties == 3 ~ 3,
    # 2 points if there are 2 options
    frozen_dinner_varieties >= 2 ~ 2,
    # 2 points if there is one option
    frozen_dinner_varieties >= 1 ~ 2,
    # 0 points if there are 0 options
    frozen_dinner_varieties < 1 ~ 0,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute the availability points associated with low-fat baked goods
#'
#' This function takes in the number of varieties of low-fat baked goods and returns the NEMS-S points associated with availability.
#'
#' @details This function implements the scoring method described in Measure 7 of the NEMS-S Protocol. "Low-fat baked goods" are baked goods with less than or equal to 3 grams of fat per serving.
#' @param lowfat_baked_goods The number of low-fat baked goods.
#' @return The NEMS-S points associated with availability of low-fat baked goods.
#' @examples
#' lowfat_baked_goods <- sample(0:3, 10)
#' baked_goods_avail(lowfat_baked_goods)
baked_goods_avail <- function(lowfat_baked_goods) {
  case_when(
    # 0 points if there are not any low-fat baked goods
    lowfat_baked_goods == 0 ~ 0,
    # 2 points if there are any low-fat baked goods
    lowfat_baked_goods > 0 ~ 2,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute the availability points associated with diet soda
#'
#' This function takes in the number of varieties of diet sodas available and returns the NEMS-S points associated with availability.
#'
#' @details This function implements the scoring method described in Measure 8 of the NEMS-S Protocol. "Diet soda" has 0 kcal per serving.
#' @param diet_soda_varieties The number of diet sodas available.
#' @return The NEMS-S points associated with availability of diet soda.
#' @examples
#' diet_soda_varieties <- sample(0:3, 10)
#' soda_avail(diet_soda_varieties)
soda_avail <- function(diet_soda_varieties){
  case_when(
    # 0 points if no diet soda is available
    diet_soda_varieties == 0 ~ 0,
    # 1 point if diet soda is available
    diet_soda_varieties > 0 ~ 1,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute the availability points associated with healthier juice drinks
#'
#' This function takes in the number of varieties of 100 percent juice drinks and returns the NEMS-S points associated with availability.
#'
#' @details This function implements the scoring method described in Measure 8 of the NEMS-S Protocol. "Healthy juice" is 100 percent juice drinks, natural fruit juice with no added sugars.
#' @param healthy_juice_varieties The number of 100 percent juice drinks available.
#' @return The NEMS-S points associated with availability of healthy juice drinks.
#' @examples6
#' healthy_juice_varieties <- sample(0:3, 10)
#' juice_drinks_avail(healthy_juice_varieties)
juice_drinks_avail <- function(healthy_juice_varieties){
  case_when(
    # 0 points if healthier juice drinks are not available
    healthy_juice_varieties == 0 ~ 0,
    # 1 point if healthier juice drinks are available
    healthy_juice_varieties > 0 ~ 1,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute whole grain bread availability points
#'
#' This function takes in the number of varieties of whole grain bread and returns the NEMS-S points associated with availability.
#'
#' @details This function implements the scoring method described in Measure 9 of the NEMS-S Protocol. "Whole grain bread" is 100% whole wheat and whole grain bread, and it is the healthier option.
#' @param varieties_of_whole_grain_bread The number of types of whole grain bread available.
#' @return The NEMS-S points associated with whole grain bread availability.
#' @examples
#' varieties_of_whole_grain_bread <- sample(1:10, 10)
#' bread_avail(varieties_of_whole_grain_bread)
bread_avail <- function(varieties_of_whole_grain_bread) {
  case_when(
    # 0 points if they do not offer whole grain bread
    varieties_of_whole_grain_bread == 0 ~ 0,
    # 2 points if they offer whole grain bread
    varieties_of_whole_grain_bread > 0 & varieties_of_whole_grain_bread < 2 ~ 2,
    # additional point if they offer >2 varieties of whole grain bread
    varieties_of_whole_grain_bread >= 2 ~ 3,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute lowfat chips availability points
#'
#' This function takes in the number of varieties of lowfat chips and returns the NEMS-S points associated with availability.
#'
#' @details This function implements the scoring method described in Measure 10 of the NEMS-S Protocol. "Lowfat chips" are chips with less than or equal to 3g of fat per one ounce serving.
#' @param lowfat_chips_varieties The number of varieties of lowfat chips offered.
#' @return The NEMS-S points associated with lowfat chips availability.
#' @examples
#' lowfat_chips_varieties <- sample(1:10, 10)
#' chips_avail(lowfat_chips_varieties)
chips_avail <- function(lowfat_chips_varieties) {
  case_when(
    # 0 points if they do not have low-fat chips
    lowfat_chips_varieties == 0 ~ 0,
    # 2 points for having low-fat chips
    lowfat_chips_varieties > 0 & lowfat_chips_varieties <= 2 ~ 2,
    # 1 additional point for having >2 varieties of low-fat chips
    lowfat_chips_varieties > 2 ~ 3,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute healthier cereal availability points
#'
#' This function takes in the number of varieties of healthier cereal options and returns the NEMS-S points associated with availability.
#'
#' @details This function implements the scoring method described in Measure 11 of the NEMS-S Protocol. "Healthier cereal" has less than 7g of sugar per serving.
#' @param healthier_cereal_varieties The number of varieties of cereal with less than 7g of sugar per serving.
#' @return The NEMS-S points associated with healthier cereal availability.
#' @examples
#' healthier_cereal_varieties <- sample(1:10, 10)
#' cereal_avail(healthier_cereal_varieties)
cereal_avail <- function(healthier_cereal_varieties) {
  case_when(
    # 0 points if there are no cereals with <7g sugar per serving available
    healthier_cereal_varieties == 0 ~ 0,
    # 2 points if a cereal with <7g sugar per serving available
    healthier_cereal_varieties > 0 ~ 2,
    TRUE ~ as.numeric(NA)
  )
}

# Cost (18 points)

#' Compute milk cost points
#'
#' This function takes in the cost of low-fat and whole milk and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of Lundsford (2016). "Lowfat milk" is considered skim milk or 1 percent fat, whichever is available.
#' @param lowfat_milk_price The price of low-fat milk.
#' @param whole_milk_price The price of whole milk.
#' @return The NEMS-S points associated with milk price.
#' @examples
#' lowfat_milk_price <- rnorm(10,2.8,.5)
#' whole_milk_price <- rnorm(10,3.1,.3)
#' milk_cost(lowfat_milk_price, whole_milk_price)
milk_cost <- function(lowfat_milk_price, whole_milk_price){
  case_when(
    # 2 points if low-fat is less expensive than whole
    lowfat_milk_price - whole_milk_price < 0 ~ 2,
    # 1 point if low-fat and whole are the same
    lowfat_milk_price == whole_milk_price ~ 1,
    # -1 if whole milk is less expensive than low-fat milk
    lowfat_milk_price - whole_milk_price > 0 ~ -1,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute ground beef cost points
#'
#' This function takes in the cost of lean and regular ground beef and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of Lundsford (2016). "Standard ground beef" is 80 percent lean and 20 percent fat by weight. "Lean ground beef" is 90 percent lean and 10 percent fat (or less fat percentage) by weight.
#' @param lean_beef_price The price of lean ground beef.
#' @param regular_beef_price The price of normal fat ground beef.
#' @return the NEMS-S points associated with beef price
#' @examples
#' lean_beef_price <- rnorm(10,5.8,.5)
#' regular_beef_price <- rnorm(10,5.5,.3)
#' ground_beef_cost(lean_beef_price, regular_beef_price)
ground_beef_cost <- function(lean_beef_price, regular_beef_price){
  case_when(
    # -1 point if healthier option (lean) is more expensive
    lean_beef_price - regular_beef_price > 0 ~ -1,
    # 2 points if the healthier option is cheaper
    lean_beef_price - regular_beef_price < 0 ~ 2,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute wieners cost points
#'
#' This function takes in the cost of lean and regular fat weiners and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of Lundsford (2016). "Fat free hot dogs" have 0 grams of fat per serving, and "light hot dogs" have less than or equal to 7 grams of fat per serving.
#' @param lean_wieners_price The price of hot dogs with less fat.
#' @param regular_wieners_price The price of normal fat hot dogs.
#' @return The NEMS-S points associated with wiener price.
#' @examples
#' lean_wieners_price <- rnorm(10,2.5,.3)
#' regular_wieners_price <- rnorm(10,1.4,.5)
#' wieners_cost(lean_wieners_price,regular_wieners_price)
wieners_cost <- function(lean_wieners_price, regular_wieners_price){
  case_when(
    # -1 point if healthier option is more expensive
    lean_wieners_price - regular_wieners_price > 0 ~ -1,
    # 2 points if healthier option is less expensive
    lean_wieners_price - regular_wieners_price < 0 ~ 2,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute frozen dinners cost points
#'
#' This function takes in the cost of healthier and regular frozen dinners and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of Lundsford (2016). "Healthier frozen dinners" have less than or equal to 9g of fat per serving (an 8-11 oz package), as written in Measure 6 in the NEMS-S Protocol.
#' @param healthier_frozen_dinners_price The price of healthier option for frozen dinners.
#' @param regular_frozen_dinners_price The price of regular fat frozen dinners.
#' @return The NEMS-S points associated with frozen dinners price.
#' @examples
#' healthier_frozen_dinners_price <- rnorm(10,4.5,.5)
#' regular_frozen_dinners_price <- rnorm(10,5.1,.3)
frozen_dinners_cost <- function(healthier_frozen_dinners_price, regular_frozen_dinners_price){
  case_when(
    # -1 point if healthier option is more expensive
    healthier_frozen_dinners_price - regular_frozen_dinners_price > 0 ~ -1,
    # 2 points if healthier option is less expensive
    healthier_frozen_dinners_price - regular_frozen_dinners_price < 0 ~ 2,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute baked goods cost points
#'
#' This function takes in the cost of healthier and regular baked goods and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of Lundsford (2016). "Low-fat baked goods" are baked goods with less than or equal to 3 grams of fat per serving.
#' @param healthier_baked_goods_price The price of lower fat baked goods.
#' @param regular_baked_goods_price The price of normal fat baked goods.
#' @return The NEMS-S points associated with baked goods price.
#' @examples
#' healthier_baked_goods_price <- rnorm(10,3.2,.5)
#' regular_baked_goods_price <- rnorm(10,3.0,.3)
baked_goods_cost <- function(healthier_baked_goods_price, regular_baked_goods_price){
  case_when(
    # -1 point if healthier option more expensive
    healthier_baked_goods_price - regular_baked_goods_price > 0 ~ -1,
    # 2 points if healthier option is less expensive
    healthier_baked_goods_price - regular_baked_goods_price < 0 ~ 2,
  )
}

#' Compute soda cost points
#'
#' This function takes in the cost of healthier and regular soda and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of Lundsford (2016). "Diet soda" has 0 kcal per serving.
#' @param diet_soda_price The price of regular soda.
#' @param regular_soda_price The price of healthier soda.
#' @return The NEMS_S points associated with soda price.
#' @examples
#' diet_soda_price <- rnorm(10,4.1,.5)
#' regular_soda_price <- rnorm(10,4.1,.3)
soda_cost <- function(diet_soda_price, regular_soda_price){
  case_when(
    # 0 points if diet soda is more expensive
    diet_soda_price - regular_soda_price > 0 ~ 0,
    # 2 points if diet soda is less expensive
    diet_soda_price - regular_soda_price <= 0 ~ 1,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute juice drinks cost points
#'
#' This function takes in the cost of healthier and regular juice and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of Lundsford (2016). "Healthy juice" is 100 percent juice drinks, natural fruit juice with no added sugars. "Regular juice" is fruit juice with added sugar and water.
#' @param healthier_juice_drinks_price The cost of 100% juice drinks.
#' @param regular_juice_drinks_price The cost of non 100% juice drinks.
#' @return The NEMS_S points associated with juice drinks cost.
#' @examples
#' healthier_juice_drinks_price <- rnorm(10,4.1,.5)
#' regular_juice_drinks_price <- rnorm(10,4.1,.3)
juice_cost <- function(healthier_juice_drinks_price, regular_juice_drinks_price){
  case_when(
    # 0 point if 100% juice drink is more expensive
    healthier_juice_drinks_price - regular_juice_drinks_price > 0 ~ 0,
    # 1 point if 100% juice drink is less expensive
    healthier_juice_drinks_price - regular_juice_drinks_price <= 0 ~ 1,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute beverage cost points
#'
#' This function takes in the cost of diet soda, regular soda, 100% juice and regular juice drinks and compares them to return an NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 Lundsford (2016). "Healthy juice" is 100 percent juice drinks, natural fruit juice with no added sugars. "Regular juice" is fruit juice with added sugar and water.
#' @param diet_soda_price The price of regular soda.
#' @param regular_soda_price The price of healthier soda.
#' @param healthier_juice_drinks_price The cost of 100% juice drinks.
#' @param regular_juice_drinks_price The cost of non 100% juice drinks.
#' @return The NEMS_S points associated with soda price compared to juice price.
#' @examples
#' diet_soda_price <- rnorm(10,4.1,.5)
#' regular_soda_price <- rnorm(10,4.1,.3)
#' healthier_juice_drinks_price <- rnorm(10,4.1,.5)
#' regular_juice_drinks_price <- rnorm(10,4.1,.3)
juice_drinks_cost <- function(diet_soda_price, regular_soda_price, healthier_juice_drinks_price, regular_juice_drinks_price){
  case_when(
    # 0 points if 100% juice drink is less expensive or diet soda is less expensive
    healthier_juice_drinks_price - regular_juice_drinks_price <= 0 | diet_soda_price - regular_soda_price <= 0 ~ 0,
    # -1 points if 100% juice drink is more expensive and diet soda is more expensive
    healthier_juice_drinks_price - regular_juice_drinks_price > 0 & diet_soda_price - regular_soda_price > 0 ~ -1,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute bread cost points
#'
#' This function takes in the cost of whole wheat bread and white bread and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of Lundsford (2016). "Whole grain bread" is 100% whole wheat and whole grain bread, and it is the healthier option. "White bread" is the regular option, bread made with refined flour.
#' @param whole_wheat_bread_price The cost of whole wheat bread.
#' @param white_bread_price The cost of white bread.
#' @return The NEMS-S points associated with bread cost.
#' @examples
#' whole_wheat_bread_price <- rnorm(10,1.5,.5)
#' white_bread_price <- rnorm(10,1.5,.3)
bread_cost <- function(whole_wheat_bread_price, white_bread_price){
  case_when(
    # -1 point if wheat bread is more expensive than white bread
    whole_wheat_bread_price - white_bread_price > 0 ~ -1,
    # 2 points if wheat bread option is less expensive than white bread
    whole_wheat_bread_price - white_bread_price < 0 ~ 2,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute chips cost points
#'
#' This function takes in the cost of lowfat and regular chips and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of Lundsford (2016). "Lowfat chips" contain less than or equal to 3g of fat per 1oz serving.
#' @param lowfat_chips_price The price of lowfat chips.
#' @param regular_chips_price The price of regular chips.
#' @return The NEMS-S points associated with chips cost.
#' @examples
#' lowfat_chips_price <- rnorm(10,2.5,.5)
#' regular_chips_price <- rnorm(10,2.8,.3)
chips_cost <- function(lowfat_chips_price, regular_chips_price){
  case_when(
    # 2 points if lowfat chips are less expensive than regular chips
    lowfat_chips_price - regular_chips_price < 0 ~ 2,
    # -1 points if lowfat chips are more expensive than regular chips
    lowfat_chips_price - regular_chips_price > 0 ~ -1,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute cereal cost points
#'
#' This function takes in the cost of healthier and regular cereal and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of Lundsford (2016). "Healthier cereal" has less than 7g of sugar per serving.
#' @param healthier_cereal_price The price of healthier cereal.
#' @param regular_cereal_price The price of regular cereal.
#' @return The NEMS-S points associated with cereal cost.
#' @examples
#' healthier_cereal_price <- rnorm(10,4.1,.5)
#' regular_cereal_price <- rnorm(10,3.5,.3)
cereal_cost <- function(healthier_cereal_price, regular_cereal_price){
  case_when(
    # 2 points if healthier cereal is less expensive
    healthier_cereal_price - regular_cereal_price < 0 ~ 2,
    # -1 points if healthier cereal is more expensive
    healthier_cereal_price - regular_cereal_price > 0 ~ -1,
    TRUE ~ as.numeric(NA)
  )
}

#'compute
#' @param
#' @param
#' @return the NEMS-S points associated with
#' @examples
#'  <- rnorm()
#'  <- rnorm()
cost_score <- function(){

}

# quality (6 points)


