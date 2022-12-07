


#' Compute Market Basket Cost
#'
#' This function uses the basic outline of the Thrifty Food Plan Market Basket to find an overall market basket cost for each store.
#'
#' @details This function using the Market Basket outline found in the Thrifty Food Plan, 2021 (https://fns-prod.azureedge.us/sites/default/files/resource-files/TFP2021.pdf)
#' @param clean_data The data retrieved from a cleaned version of the Qualtrics Survey.
#' @return Total Cost for Market Basket as well as number of items not available.
#' @examples
#' clean_data <- read_nemss(file)
#' calculate_market_basket(clean_data)
calculate_market_basket <- function(clean_data) {
  prices <- data.frame("ID" = clean_data$STORE_ID)
  prices <- prices |>
    dplyr::mutate("bread_price" = as.numeric(clean_data$whole_wheat_bread_price)/as.numeric(clean_data$whole_wheat_bread_size) - as.numeric(clean_data$white_bread_price)/as.numeric(clean_data$white_bread_size)) |>
    dplyr::mutate("milk_price" = as.numeric(clean_data$whole_gal_price) - as.numeric(clean_data$lowfat_gal_price))

  average_bread <- mean(prices$bread_price, na.rm = TRUE)
  average_wheat <- mean(as.numeric(clean_data$whole_wheat_bread_price)/as.numeric(clean_data$whole_wheat_bread_size), na.rm = TRUE)
  average_white <- mean(as.numeric(clean_data$white_bread_price)/as.numeric(clean_data$white_bread_size), na.rm = TRUE)
  average_milk <- mean(prices$milk_price, na.rm = TRUE)
  average_whole <- mean(as.numeric(clean_data$whole_gal_price), na.rm = TRUE)
  average_lowfat <- mean(as.numeric(clean_data$lowfat_gal_price), na.rm = TRUE)
  average_carrot <- mean(as.numeric(clean_data$carrot_price), na.rm = TRUE)
  average_tomato <- mean(as.numeric(clean_data$tomato_price), na.rm = TRUE)
  average_broccoli <- mean(as.numeric(clean_data$broccoli_price), na.rm = TRUE)
  average_lettuce <- mean(as.numeric(clean_data$lettuce_price), na.rm = TRUE)
  average_cucumber <- mean(as.numeric(clean_data$cucumber_price), na.rm = TRUE)
  average_corn <- mean(as.numeric(clean_data$corn_price), na.rm = TRUE)
  average_cauliflower <- mean(as.numeric(clean_data$cauliflower_price), na.rm = TRUE)
  average_apple <- mean(as.numeric(clean_data$apple_price), na.rm = TRUE)
  average_banana <- mean(as.numeric(clean_data$banana_price), na.rm = TRUE)
  average_orange <- mean(as.numeric(clean_data$orange_price), na.rm = TRUE)
  average_grape <- mean(as.numeric(clean_data$grape_price), na.rm = TRUE)
  average_soda <- mean(as.numeric(clean_data$regular_soda_cost), na.rm = TRUE)
  average_beef <- mean(as.numeric(clean_data$regular_beef_price), na.rm = TRUE)
  average_frozen_dinner <- mean(as.numeric(clean_data$regular_frozen_dinners_price_1), na.rm = TRUE)
  average <- data.frame(average_bread, average_milk)

  scores <- data.frame("ID" = clean_data$STORE_ID)
  scores <- scores |>
    dplyr::mutate("grain_replacements" = dplyr::if_else(clean_data$whole_wheat_bread_price == "", 1, 0) + dplyr::if_else(clean_data$white_bread_price == "", 1, 0)) |>
    dplyr::mutate("grain" = dplyr::if_else(grain_replacements == 2, average_white*16*5.65*1.25 + average_wheat*16*6.7*1.25,dplyr::if_else(grain_replacements == 0, as.numeric(clean_data$whole_wheat_bread_price)/as.numeric(clean_data$whole_wheat_bread_size)*16*6.7 +
                    as.numeric(clean_data$white_bread_price)/as.numeric(clean_data$white_bread_size)*16*5.65, dplyr::if_else(clean_data$white_bread_price == "", (as.numeric(clean_data$whole_wheat_bread_price)/as.numeric(clean_data$whole_wheat_bread_size) + average_bread)*16*5.65 + as.numeric(clean_data$whole_wheat_bread_price)/as.numeric(clean_data$whole_wheat_bread_size)*16*6.7, (as.numeric(clean_data$white_bread_price)/as.numeric(clean_data$white_bread_size)-average_bread)*16*6.7 + as.numeric(clean_data$white_bread_price)/as.numeric(clean_data$white_bread_size)*16*5.65))))|>
    dplyr::mutate("dairy_replacements" = dplyr::if_else(clean_data$whole_gal_price == "", 1, 0) + dplyr::if_else(clean_data$lowfat_gal_price == "", 1, 0)) |>
    dplyr::mutate("dairy" = dplyr::if_else(dairy_replacements == 2, average_whole/18.6*15.13 + average_lowfat/18.6*25.48, dplyr::if_else(dairy_replacements == 0, as.numeric(clean_data$whole_gal_price)/8.6*15.13 + as.numeric(clean_data$lowfat_gal_price)/8.6*25.48, dplyr::if_else(clean_data$whole_gal_price == "", (as.numeric(clean_data$lowfat_gal_price)-average_milk)/8.6*25.48 + (as.numeric(clean_data$lowfat_gal_price)-average_milk)/8.6*15.13, (as.numeric(clean_data$whole_gal_price)-average_milk)/8.6*25.48 + as.numeric(clean_data$whole_gal_price)/8.6*15.13)))) |>
    dplyr::mutate("vegetable_replacements" = dplyr::if_else(clean_data$carrot_price == "", 1, 0) + dplyr::if_else(clean_data$tomato_price == "", 1, 0) + dplyr::if_else(clean_data$broccoli_price == "", 1, 0) + dplyr::if_else(clean_data$lettuce_price == "", 1, 0) + dplyr::if_else(clean_data$cucumber_price == "", 1, 0) + dplyr::if_else(clean_data$corn_price == "", 1, 0) + dplyr::if_else(clean_data$cauliflower_price == "", 1, 0)) |>
    dplyr::mutate("vegetable" = dplyr::if_else(clean_data$carrot_price == "", dplyr::if_else(clean_data$tomato_price == "",average_carrot*4.14*1.25, as.numeric(clean_data$tomato_price)*4.14), as.numeric(clean_data$carrot_price)*4.14) +
                    dplyr::if_else(clean_data$tomato_price == "", dplyr::if_else(clean_data$carrot_price == "", average_tomato*4.14*1.25, as.numeric(clean_data$carrot_price)*4.14), as.numeric(clean_data$tomato_price)*4.14) +
                    dplyr::if_else(clean_data$broccoli_price == "", dplyr::if_else(clean_data$lettuce_price == "", dplyr::if_else(clean_data$cucumber_price == "", average_broccoli*1.23*1.25, as.numeric(clean_data$cucumber_price)*1.23), as.numeric(clean_data$lettuce_price)*1.23), as.numeric(clean_data$broccoli_price)*1.23) +
                    dplyr::if_else(clean_data$lettuce_price == "", dplyr::if_else(clean_data$cucumber_price == "", dplyr::if_else(clean_data$broccoli_price == "", average_lettuce*1*1.25, as.numeric(clean_data$broccoli_price)*1), as.numeric(clean_data$cucumber_price)*1), as.numeric(clean_data$lettuce_price)*1) +
                    dplyr::if_else(clean_data$cucumber_price == "", dplyr::if_else(clean_data$broccoli_price == "", dplyr::if_else(clean_data$lettuce_price == "", average_cucumber*1*1.25, as.numeric(clean_data$lettuce_price)*1), as.numeric(clean_data$broccoli_price)*1), as.numeric(clean_data$cucumber_price)*1) +
                    dplyr::if_else(clean_data$corn_price == "", dplyr::if_else(clean_data$cauliflower_price == "", average_corn*1.25*5.64, as.numeric(clean_data$cauliflower_price)*5.64), as.numeric(clean_data$corn_price)*5.64) +
                    dplyr::if_else(clean_data$cauliflower_price == "", dplyr::if_else(clean_data$corn_price == "", average_cauliflower*1.25*5, as.numeric(clean_data$corn_price)*5), as.numeric(clean_data$cauliflower_price)*5)
                  ) |>
    dplyr::mutate("fruit_replacements" = dplyr::if_else(clean_data$apple_price == "", 1, 0) + dplyr::if_else(clean_data$banana_price == "", 1, 0) + dplyr::if_else(clean_data$orange_price == "", 1, 0) + dplyr::if_else(clean_data$grape_price == "", 1, 0)) |>
    dplyr::mutate("fruit" = dplyr::if_else(clean_data$apple_price == "", average_apple*5*1.25, as.numeric(clean_data$apple_price)*5) +
                    dplyr::if_else(clean_data$banana_price == "", average_banana*5*1.25, as.numeric(clean_data$banana_price)*5) +
                    dplyr::if_else(clean_data$orange_price == "", average_orange*5*1.25, as.numeric(clean_data$orange_price)*5) +
                    dplyr::if_else(clean_data$grape_price == "", average_grape*2.76*1.25, as.numeric(clean_data$grape_price)*2.76))|>
    dplyr::mutate("soda_replacements" = dplyr::if_else(clean_data$regular_soda_cost == "", 1, 0))|>
    dplyr::mutate("soda" = dplyr::if_else(clean_data$regular_soda_cost == "", average_soda/144*16*.57*1.25, as.numeric(clean_data$regular_soda_cost)/144*16*.57))|>
    dplyr::mutate("beef_replacements" = dplyr::if_else(clean_data$regular_beef_price == "", 1, 0))|>
    dplyr::mutate("beef" = dplyr::if_else(clean_data$regular_beef_price == "", average_beef*2.26*1.25, as.numeric(clean_data$regular_beef_price)*2.26))|>
    dplyr::mutate("frozen_dinner_replacements" = dplyr::if_else(clean_data$regular_frozen_dinners_price_1 == "" & clean_data$regular_frozen_dinners_price_2 == "" & clean_data$regular_frozen_dinners_price_3 == "" & clean_data$regular_frozen_dinners_price_4 == "" & clean_data$regular_frozen_dinners_price_5 == "" & clean_data$regular_frozen_dinners_price_6 == "", 1, 0)) |>
    dplyr::mutate("frozen_dinner" =
                    dplyr::if_else(clean_data$regular_frozen_dinners_price_1 != "", as.numeric(clean_data$regular_frozen_dinners_price_1)*1.51, dplyr::if_else(clean_data$regular_frozen_dinners_price_2 != "", as.numeric(clean_data$regular_frozen_dinners_price_2)*1.51,
                    dplyr::if_else(clean_data$regular_frozen_dinners_price_3 != "", as.numeric(clean_data$regular_frozen_dinners_price_3)*1.51, dplyr::if_else(clean_data$regular_frozen_dinners_price_4 != "", as.numeric(clean_data$regular_frozen_dinners_price_4)*1.51,
                    dplyr::if_else(clean_data$regular_frozen_dinners_price_5 != "", as.numeric(clean_data$regular_frozen_dinners_price_5)*1.51, dplyr::if_else(clean_data$regular_frozen_dinners_price_6 != "", as.numeric(clean_data$regular_frozen_dinners_price_6)*1.51, average_frozen_dinner*1.51))))))
                    ) |>
    dplyr::mutate("total_replacements" = grain_replacements + dairy_replacements + vegetable_replacements + fruit_replacements + soda_replacements + beef_replacements + frozen_dinner_replacements) |>
    dplyr::mutate("total" = grain + dairy + vegetable + fruit + soda + beef + frozen_dinner)
}


#' Compute Difference in "Healthy" and "Less Healthy" food options
#'
#' This function finds the average difference in price between the healthier and less healthy food options.
#'
#' @details This function is used to determine adequate replacements for the market basket prices.
#' @param clean_data The data retrieved from a cleaned version of the Qualtrics Survey.
#' @return Average_Difference for the average price difference.
#' @examples
#' clean_data <- read_nemss(file)
#' find_difference(clean_data)

average_difference <- function(clean_data){
  prices <- data.frame("ID" = clean_data$STORE_ID)
  prices <- prices |>
    dplyr::mutate("bread_price" = as.numeric(clean_data$whole_wheat_bread_price)/as.numeric(clean_data$whole_wheat_bread_size) - as.numeric(clean_data$white_bread_price)/as.numeric(clean_data$white_bread_size)) |>
    dplyr::mutate("milk_price" = as.numeric(clean_data$whole_gal_price) - as.numeric(clean_data$lowfat_gal_price))

  average_bread <- mean(prices$bread_price, na.rm = TRUE)
  average_milk <- mean(prices$milk_price, na.rm = TRUE)
  average <- data.frame(average_bread, average_milk)

  }

#' Compute Availability and Cost Scores for all variables
#'
#' This function uses all functions previously created and scores assigned to each variable and adds them into one total score for both availability and cost.
#'
#' @details This function implements the scoring method described in Measure 9 of the NEMS-S Protocol.
#' @param clean_data The data retrieved from a cleaned version of the Qualtrics Survey.
#' @return Scores for total Availability and Cost, as well as given the option if Detail = TRUE to see the score for each individual variable.
#' @examples
#' clean_data <- read_nemss(file)
#' calculate_score(clean_data)
calculate_score <- function(clean_data, detail = FALSE) {
  scores <- data.frame("ID" = clean_data$STORE_ID)
  scores <- scores |>
    dplyr::mutate("type" = clean_data$store_type) |>
    dplyr::mutate("pharmacy" = clean_data$pharmacy) |>
    dplyr::mutate("ethnic" = clean_data$ethnic) |>
    dplyr::mutate("merch" = clean_data$merch) |>
    dplyr::mutate("registers" = clean_data$register) |>
    dplyr::mutate("selfchecko" = clean_data$self_checkout) |>
    dplyr::mutate("total_registers" = as.numeric(clean_data$register) + as.numeric(clean_data$self_checkout)) |>
    dplyr::mutate("milk_avail_score" = milk_avail(as.numeric(clean_data$lowfat_gal), as.numeric(clean_data$whole_gal))) |>
    dplyr::mutate("fruit_avail_score" = fruit_avail(as.numeric(clean_data$varieties_of_fruit))) |>
    dplyr::mutate("vegetable_avail_score" = vegetable_avail(as.numeric(clean_data$varieties_of_vegetables))) |>
    dplyr::mutate("ground_beef_avail_score" = ground_beef_avail(as.numeric(clean_data$lean_beef_varieties))) |>
    dplyr::mutate("hot_dog_avail_score" = hot_dog_avail(as.numeric(clean_data$fat_free_hot_dogs), as.numeric(clean_data$light_hot_dogs))) |>
    dplyr::mutate("frozen_dinners_avail_score" = frozen_dinners_avail(as.numeric(clean_data$frozen_dinner_varieties))) |>
    dplyr::mutate("baked_goods_avail_score" = baked_goods_avail(as.numeric(clean_data$lowfat_baked_goods))) |>
    dplyr::mutate("soda_avail_score" = soda_avail(as.numeric(clean_data$diet_soda_varieties))) |>
    dplyr::mutate("juice_drinks_avail_score" = juice_drinks_avail(as.numeric(clean_data$healthy_juice_varieties))) |>
    dplyr::mutate("bread_avail_score" = bread_avail(as.numeric(clean_data$varieties_of_whole_grain_bread))) |>
    dplyr::mutate("chip_avail_score" = chips_avail(as.numeric(clean_data$lowfat_chip_varieties))) |>
    dplyr::mutate("cereal_avail_score" = cereal_avail(as.numeric(clean_data$healthier_cereal_varieties))) |>
    dplyr::mutate("Total_Availability_Score" = milk_avail_score + fruit_avail_score + vegetable_avail_score + ground_beef_avail_score +
                                                        hot_dog_avail_score + frozen_dinners_avail_score + baked_goods_avail_score +
                                                        soda_avail_score + juice_drinks_avail_score + bread_avail_score + chip_avail_score +
                                                        cereal_avail_score) |>
    dplyr::mutate("milk_cost_score" = milk_cost(as.numeric(clean_data$lowfat_gal_price), as.numeric(clean_data$whole_gal_price))) |>
    dplyr::mutate("ground_beef_cost_score" = ground_beef_cost(as.numeric(clean_data$lean_beef_price), as.numeric(clean_data$regular_beef_price))) |>
    dplyr::mutate("wieners_cost_score" = wieners_cost(as.numeric(clean_data$lean_wieners_price), as.numeric(clean_data$regular_wieners_price))) |>
    dplyr::mutate("frozen_dinners_cost_score" = frozen_dinners_cost(as.numeric(clean_data$healthier_frozen_dinners_price_1), as.numeric(clean_data$regular_frozen_dinners_price_1))) |>
    #dplyr::mutate("baked_goods_cost_score" = baked_goods_cost(as.numeric(clean_data$lowfat_baked_goods_cost))) |>
    dplyr::mutate("soda_cost_score" = soda_cost(as.numeric(clean_data$diet_soda_cost), as.numeric(clean_data$regular_soda_cost))) |>
    dplyr::mutate("juice_cost_score" = juice_cost(as.numeric(clean_data$healthier_juice_drinks_price), as.numeric(clean_data$regular_juice_drinks_price))) |>
    dplyr::mutate("juice_drinks_cost_score" = juice_drinks_cost(as.numeric(clean_data$diet_soda_cost), as.numeric(clean_data$regular_soda_cost), as.numeric(clean_data$healthier_juice_drinks_price), as.numeric(clean_data$regular_juice_drinks_price))) |>
    dplyr::mutate("bread_cost_score" = bread_cost(as.numeric(clean_data$whole_wheat_bread_price)/as.numeric(clean_data$whole_wheat_bread_size), as.numeric(clean_data$white_bread_price)/as.numeric(clean_data$white_bread_size))) |>
    dplyr::mutate("chips_cost_score" = chips_cost(as.numeric(clean_data$lowfat_chips_price)/as.numeric(clean_data$lowfat_chips_size), as.numeric(clean_data$regular_chips_price)/as.numeric(clean_data$regular_chips_size))) |>
    dplyr::mutate("cereal_cost_score" = cereal_cost(as.numeric(clean_data$healthier_cereal_price), as.numeric(clean_data$regular_cereal_price))) |>
    dplyr::mutate("Latitude" = clean_data$LocationLatitude) |>
    dplyr::mutate("Longitude" = clean_data$LocationLongitude) |>
    tidyr::replace_na(list(milk_cost_score = 0, ground_beef_cost_score = 0, wieners_cost_score = 0, frozen_dinners_cost_score = 0,
                    soda_cost_score = 0, juice_cost_score = 0, juice_drinks_cost_score = 0, bread_cost_score = 0, chips_cost_score = 0,
                    cereal_cost_score = 0)) |>
    dplyr::mutate("Total_Cost_Score" = milk_cost_score + ground_beef_cost_score + wieners_cost_score + frozen_dinners_cost_score +
             soda_cost_score + juice_cost_score + juice_drinks_cost_score + bread_cost_score + chips_cost_score + cereal_cost_score)

  if(detail == TRUE) {dplyr::select(scores, c(ID, type, pharmacy, ethnic, merch, registers, selfchecko, total_registers, milk_avail_score, fruit_avail_score, vegetable_avail_score, ground_beef_avail_score, hot_dog_avail_score,
                                  frozen_dinners_avail_score, baked_goods_avail_score, soda_avail_score, juice_drinks_avail_score,
                                   bread_avail_score, chip_avail_score, cereal_avail_score, Total_Availability_Score, milk_cost_score,
                                  ground_beef_cost_score, wieners_cost_score, frozen_dinners_cost_score, soda_cost_score, juice_cost_score,
                                  juice_drinks_cost_score, bread_cost_score, chips_cost_score, cereal_cost_score, Total_Cost_Score,
                                  Latitude, Longitude))}
  else {dplyr::select(scores, c(ID, type, pharmacy, ethnic, merch, registers, selfchecko, total_registers, Total_Availability_Score, Total_Cost_Score, Latitude, Longitude))}

}

#' Import and Clean Qualtrics Dataset to be used to calculate scores. The Qualtrics Survey is found at
#'
#' This function takes the output of the Qualtrics survey and returns it as a cleaned R dataframe.
#'
#' @details This function implements the scoring method described in Measure 9 of the NEMS-S Protocol.
#' @param file The data file retrieved from the Qualtrics survey.
#' @return A cleaned R dataframe with availability values and cost values for each variable.
#' @examples
#' file <- file.path(qualtricsdata)
#' read_nemss(file)
read_nemss <- function(file) {
  SAS_data <- haven::read_sav(file)

  if(!"MILK_PRICE_LFQ_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_LFQ_1_1 <- SAS_data$MILK_PRICE_LFQ_1_PRICE_1
  if(!"MILK_PRICE_LQH_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_LQH_1_1 <- SAS_data$MILK_PRICE_LQH_1_PRICE_1
  if(!"MILK_PRICE_LFG_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_LFG_1_1 <- SAS_data$MILK_PRICE_LFG_1_PRICE_1
  if(!"MILK_PRICE_WHLQ_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_WHLQ_1_1 <- SAS_data$MILK_PRICE_WHLQ_1_PRICE_1
  if(!"MILK_PRICE_WHLH_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_WHLH_1_1 <- SAS_data$MILK_PRICE_WHLH_1_PRICE_1
  if(!"MILK_PRICE_WHLG_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_WHLG_1_1 <- SAS_data$MILK_PRICE_WHLG_1_PRICE_1
  if(!"BVG_1_5_1"%in% colnames(SAS_data)) SAS_data$BVG_1_5_1 <- 0



  #rename and sort data
  all_data <- SAS_data |>
    dplyr::mutate("store_type" = dplyr::if_else(STORE_T == 1, "Grocery Store", if_else(STORE_T == 2, "Convenience Store", if_else(STORE_T == 3, "Other", if_else(STORE_T == 4, "Dollar Store", "Trading Post"))))) |>
    dplyr::mutate("pharmacy" = dplyr::if_else(STORE_T2_2 == 1|STORE_T2_3 == 1, TRUE, FALSE, missing = FALSE)) |>
    dplyr::mutate("ethnic" = dplyr::if_else(STORE_T2_4 == 1, TRUE, FALSE, missing = FALSE)) |>
    dplyr::mutate("merch" = dplyr::if_else(STORE_T2_6 == 1, TRUE, FALSE, missing = FALSE))|>
    dplyr::mutate("register" = dplyr::if_else(A_1_1_1 == ".", "0", A_1_1_1)) |>
    dplyr::mutate("self_checkout" = dplyr::if_else(A_2_1_1 == ".", "0", A_2_1_1)) |>
    dplyr::mutate("lowfat_milk_avail" = dplyr::if_else(MILK_2A_1 == 1, TRUE, FALSE)) |>
    dplyr::mutate("dairy_milk_avail" = dplyr::if_else(MILK_1_AVAIL_1 == 1, TRUE, FALSE)) |>
    #dplyr::mutate("nondairy_milk_avail" = dplyr::if_else(MILK_1_6_1 == 1, TRUE, FALSE)) |>
    dplyr::mutate("lowfat_pint" = MILK_SHELF_LF_1_SPACE_1) |>
    dplyr::mutate("lowfat_quart" = MILK_SHELF_LF_1_SPACE_2) |>
    dplyr::mutate("lowfat_half_gal" = MILK_SHELF_LF_1_SPACE_3) |>
    dplyr::mutate("lowfat_gal" = as.numeric(dplyr::if_else(MILK_SHELF_LF_1_SPACE_4 == "", "0", MILK_SHELF_LF_1_SPACE_4))) |>
    dplyr::mutate("whole_pint" = MILK_SHELF_WHL_1_SPACE_1) |>
    dplyr::mutate("whole_quart" = MILK_SHELF_WHL_1_SPACE_2) |>
    dplyr::mutate("whole_half_gal" = MILK_SHELF_WHL_1_SPACE_3) |>
    dplyr::mutate("whole_gal" = dplyr::if_else(MILK_SHELF_WHL_1_SPACE_4 == "", "0", MILK_SHELF_WHL_1_SPACE_4)) |>
    dplyr::mutate("lowfat_quart_price" = MILK_PRICE_LFQ_1_1) |>
    dplyr::mutate("lowfat_half_gal_price" = MILK_PRICE_LQH_1_1) |>
    dplyr::mutate("lowfat_gal_price" = MILK_PRICE_LFG_1_1) |>
    dplyr::mutate("whole_quart_price" = MILK_PRICE_WHLQ_1_1) |>
    dplyr::mutate("whole_half_gal_price" = MILK_PRICE_WHLH_1_1) |>
    dplyr::mutate("whole_gal_price" = MILK_PRICE_WHLG_1_1) |>
    dplyr::mutate("lean_beef_varieties" = BEEF_H_CNT)|>
    dplyr::mutate("lean_beef_price" = dplyr::if_else(BEEF_H_GS_3_1 == "", dplyr::if_else(BEEF_H_LGB_3_1 == "", BEEF_H_GT_3_1, BEEF_H_LGB_3_1), BEEF_H_GS_3_1))|>
    dplyr::mutate("regular_beef_price" = dplyr::if_else(BEEF_R_80_2_PRICE_1 == "", BEEF_R_ALT_2_PRICE_1, BEEF_R_80_2_PRICE_1))|>
    dplyr::mutate("varieties_of_fruit" = dplyr::if_else(FRUIT_CNT == "", "0", FRUIT_CNT))|>
    dplyr::mutate("apple_price" = FRUIT_AVAIL_APP_2_1) |>
    dplyr::mutate("banana_price" = FRUIT_AVAIL_BAN_2_1) |>
    dplyr::mutate("orange_price" = FRUIT_AVAIL_ORA_2_1) |>
    dplyr::mutate("grape_price" = FRUIT_AVAIL_GRA_2_1) |>
    dplyr::mutate("varieties_of_vegetables" = dplyr::if_else(VEG_CNT == "", "0", VEG_CNT))|>
    dplyr::mutate("carrot_price" = VEG_AVAIL_CAR_2_1) |>
    dplyr::mutate("tomato_price" = VEG_AVAIL_TOM_2_1) |>
    dplyr::mutate("broccoli_price" = VEG_AVAIL_BRO_2_1) |>
    dplyr::mutate("lettuce_price" = VEG_AVAIL_LET_2_1) |>
    dplyr::mutate("cucumber_price" = VEG_AVAIL_CUC_2_1) |>
    dplyr::mutate("corn_price" = VEG_AVAIL_COR_2_1) |>
    dplyr::mutate("cauliflower_price" = VEG_AVAIL_CAU_2_1) |>
    dplyr::mutate("fat_free_hot_dogs" = dplyr::if_else(HD_H_OM_1==1, 1, 0), "light_hot_dogs" = dplyr::if_else(HD_H_OTH_1==1,1,0))|>
    dplyr::mutate("lean_wieners_price" = dplyr::if_else(HD_H_OM_2_1 == "", HD_H_OTH_2_1, HD_H_OM_2_1))|>
    dplyr::mutate("lean_wieners_size" = dplyr::if_else(HD_H_OM_3_1 == "", HD_H_OTH_3_1, HD_H_OM_3_1)) |>
    dplyr::mutate("regular_wieners_price" = dplyr::if_else(HD_R_OM_2_1 == "", HD_R_OTH_2_1, HD_R_OM_2_1)) |>
    dplyr::mutate("regular_wieners_size" = dplyr::if_else(HD_R_OM_3_1 == "", HD_R_OTH_3_1, HD_R_OM_3_1)) |>
    dplyr::mutate("frozen_dinners_pref_avail" = dplyr::if_else(FRZ_PRICE_PREF_LAS_1_REDFAT_1 != "", 1, 0) + dplyr::if_else(FRZ_PRICE_PREF_RTB_1_REDFAT_1 != "", 1, 0) + dplyr::if_else(FRZ_PRICE_PREF_MEA_1_REDFAT_1 != "", 1, 0) + dplyr::if_else(FRZ_PRICE_PREF_LAS_2_REG_1 != "", 1, 0) + dplyr::if_else(FRZ_PRICE_PREF_RTB_2_REG_1 != "", 1, 0) + dplyr::if_else(FRZ_PRICE_PREF_MEA_2_REG_1 != "", 1, 0)) |>
    dplyr::mutate("frozen_dinners_oth_avail" = dplyr::if_else(FRZ_PRICE_OTH_1_1_REDFAT_2 != "", 1, 0) + dplyr::if_else(FRZ_PRICE_OTH_2_1_REDFAT_2 != "", 1, 0) + dplyr::if_else(FRZ_PRICE_OTH_3_1_REDFAT_2 != "", 1, 0) + dplyr::if_else(FRZ_PRICE_OTH_1_2_ALT_2 != "", 1, 0) + dplyr::if_else(FRZ_PRICE_OTH_2_2_ALT_2 != "", 1, 0) + dplyr::if_else(FRZ_PRICE_OTH_3_2_ALT_2 != "", 1, 0)) |>
    dplyr::mutate("frozen_dinner_varieties" = (frozen_dinners_pref_avail + frozen_dinners_oth_avail)/2) |>
    dplyr::mutate("healthier_frozen_dinners_price_1" = FRZ_PRICE_PREF_LAS_1_REDFAT_2) |>
    dplyr::mutate("regular_frozen_dinners_price_1" = FRZ_PRICE_PREF_LAS_2_REG_2) |>
    dplyr::mutate("healthier_frozen_dinners_price_2" = FRZ_PRICE_PREF_RTB_1_REDFAT_2) |>
    dplyr::mutate("regular_frozen_dinners_price_2" = FRZ_PRICE_PREF_RTB_2_REG_2) |>
    dplyr::mutate("healthier_frozen_dinners_price_3" = FRZ_PRICE_PREF_MEA_1_REDFAT_2) |>
    dplyr::mutate("regular_frozen_dinners_price_3" = FRZ_PRICE_PREF_MEA_2_REG_2) |>
    dplyr::mutate("healthier_frozen_dinners_price_4" = FRZ_PRICE_OTH_1_1_REDFAT_1) |>
    dplyr::mutate("regular_frozen_dinners_price_4" = FRZ_PRICE_OTH_1_2_ALT_1) |>
    dplyr::mutate("healthier_frozen_dinners_price_5" = FRZ_PRICE_OTH_2_1_REDFAT_1) |>
    dplyr::mutate("regular_frozen_dinners_price_5" = FRZ_PRICE_OTH_2_2_ALT_1) |>
    dplyr::mutate("healthier_frozen_dinners_price_6" = FRZ_PRICE_OTH_3_1_REDFAT_1) |>
    dplyr::mutate("regular_frozen_dinners_price_6" = FRZ_PRICE_OTH_3_2_ALT_1) |>
    dplyr::mutate("lowfat_baked_goods" = dplyr::if_else(BKD_H_BG1_1==1|BKD_H_BGPK_1==1|BKD_H_ENG_1==1|BKD_H_LFM_1==1, 1, 0))|>
    dplyr::mutate("lowfat_baked_goods_cost" = dplyr::if_else(BKD_H_BG1_2_1 != "", BKD_H_BG1_2_1, dplyr::if_else(BKD_H_BGPK_2_1 != "", BKD_H_BGPK_2_1, "0"))) |>
    dplyr::mutate("diet_soda_varieties" = dplyr::if_else(BVG_HS_DC_1_AVAIL==1|BVG_HS_DOTH_1_AVAIL==1, 1, 0))|>
    dplyr::mutate("diet_soda_cost" = dplyr::if_else(BVG_HS_DC_2_PRICE_1 == "", BVG_HS_DOTH_2_PRICE_1, BVG_HS_DC_2_PRICE_1))|>
    dplyr::mutate("regular_soda_cost" = dplyr::if_else(BVG_RS_COK_2_PRICE_1 == "", BVG_RS_OTH_2_PRICE_1, BVG_RS_COK_2_PRICE_1))|>
    #dplyr::mutate("regular_soda_size" = dplyr::if_else(BVG_RS_COK_4_SIZE == '12 pack 12 oz', "144", dplyr::if_else(BVG_RS_COK_4_SIZE == '6 pack 12 oz', "72", "0")))|>
    dplyr::mutate("healthy_juice_varieties" = dplyr::if_else(BVG_1_5_1 == 1, 1, 0))|>
    dplyr::mutate("healthier_juice_drinks_price" = dplyr::if_else(BVG_HJ_MM_2_PRICE_1 == "", dplyr::if_else(BVG_HJ_TRO_2_PRICE_1 == "", BVG_HJ_OTH_2_PRICE_1, BVG_HJ_TRO_2_PRICE_1), BVG_HJ_MM_2_PRICE_1))|>
    dplyr::mutate("healthier_juice_drinks_comments" = dplyr::if_else(BVG_HJ_MM_3_COMM_1 == "", dplyr::if_else(BVG_HJ_TRO_3_COMM_1 == "", BVG_HJ_OTH_3_COMM_1, BVG_HJ_TRO_3_COMM_1), BVG_HJ_MM_3_COMM_1)) |>
    dplyr::mutate("regular_juice_drinks_price" = dplyr::if_else(BVG_RJ_MM_2_PRICE_1 == "", dplyr::if_else(BVG_RJ_TRO_2_PRICE_1 == "", BVG_RJ_OTH_2_PRICE_1, BVG_RJ_TRO_2_PRICE_1), BVG_RJ_MM_2_PRICE_1)) |>
    dplyr::mutate("regular_juice_drinks_comments" = dplyr::if_else(BVG_RJ_MM_3_COMM_1 == "", dplyr::if_else(BVG_RJ_TRO_3_COMM_1 == "", BVG_RJ_OTH_3_COMM_1, BVG_RJ_TRO_3_COMM_1), BVG_RJ_MM_3_COMM_1)) |>
    dplyr::mutate("varieties_of_whole_grain_bread" = BRD_H_CNT)|>
    dplyr::mutate("whole_wheat_bread_price" = dplyr::if_else(BRD_H_NO_3_PRICE_1 == "", dplyr::if_else(BRD_H_SLC_3_PRICE_1 == "", BRD_H_OTH_3_PRICE_1, BRD_H_SLC_3_PRICE_1), BRD_H_NO_3_PRICE_1))|>
    dplyr::mutate("whole_wheat_bread_size" = dplyr::if_else(BRD_H_NO_2_OZ_1 == "", dplyr::if_else(BRD_H_SLC_2_OZ_1 == "", BRD_H_OTH_2_OZ_1, BRD_H_SLC_2_OZ_1), BRD_H_NO_2_OZ_1)) |>
    dplyr::mutate("white_bread_price" = dplyr::if_else(BRD_R_NO_3_PRICE_1 == "", dplyr::if_else(BRD_R_SLC_3_PRICE_1 == "", BRD_R_OTH_3_PRICE_1, BRD_R_SLC_3_PRICE_1), BRD_R_NO_3_PRICE_1)) |>
    dplyr::mutate("white_bread_size" = dplyr::if_else(BRD_R_NO_2_OZ_1 == "", dplyr::if_else(BRD_R_SLC_2_OZ_1 == "", BRD_R_OTH_2_OZ_1, BRD_R_SLC_2_OZ_1), BRD_R_NO_2_OZ_1)) |>
    dplyr::mutate("lowfat_chip_varieties" = CHIP_H_CNT)|>
    dplyr::mutate("lowfat_chips_price" = dplyr::if_else(CHIP_H_BL_3_PRICE_1 == "", CHIP_H_OTH_3_PRICE_1, CHIP_H_BL_3_PRICE_1)) |>
    dplyr::mutate("lowfat_chips_size" = dplyr::if_else(CHIP_H_BL_1_SIZE_1 == "", CHIP_H_OTH_1_SIZE_1, CHIP_H_BL_1_SIZE_1)) |>
    dplyr::mutate("regular_chips_price" = dplyr::if_else(CHIP_R_LP_3_PRICE_1 == "", CHIP_R_OTH_3_PRICE_1, CHIP_R_LP_3_PRICE_1)) |>
    dplyr::mutate("regular_chips_size" = dplyr::if_else(CHIP_R_LP_1_SIZE_1 == "", CHIP_R_OTH_1_SIZE_1, CHIP_R_LP_1_SIZE_1)) |>
    dplyr::mutate("healthier_cereal_varieties" = CRL_H_CNT)|>
    dplyr::mutate("healthier_cereal_price" = dplyr::if_else(CRL_H_CHE_3_PRICE_1 == "", CRL_H_OTH_3_PRICE_1, CRL_H_CHE_3_PRICE_1))|>
    dplyr::mutate("regular_cereal_price" = dplyr::if_else(CRL_R_FCH_3_PRICE_1 == "", CRL_R_OTH_3_PRICE_1, CRL_R_FCH_3_PRICE_1)) |>
    dplyr::select(tidyselect::any_of(c("STORE_ID", "store_type", "pharmacy", "ethnic", "merch", "register", "self_checkout", "total_registers", "organic_milk_avail", "lowfat_milk_avail", "dairy_milk_avail", "nondairy_milk_avail",
           "lowfat_pint", "lowfat_quart", "lowfat_half_gal", "lowfat_gal", "whole_pint", "whole_quart",
           "whole_half_gal", "whole_gal", "lowfat_quart_price", "lowfat_half_gal_price", "lowfat_gal_price",
           "whole_quart_price", "whole_half_gal_price", "whole_gal_price", "lean_beef_varieties", "lean_beef_price", "regular_beef_price",
           "varieties_of_fruit", "apple_price", "banana_price", "orange_price", "grape_price", "varieties_of_vegetables", "carrot_price", "tomato_price", "broccoli_price", "lettuce_price", "cucumber_price", "corn_price", "cauliflower_price", "fat_free_hot_dogs", "light_hot_dogs", "lean_wieners_price", "lean_wieners_size",
           "regular_wieners_price", "regular_wieners_size", "frozen_dinner_varieties", "regular_frozen_dinners_price_1",
           "healthier_frozen_dinners_price_1", "regular_frozen_dinners_price_2", "healthier_frozen_dinners_price_2",
           "regular_frozen_dinners_price_3", "healthier_frozen_dinners_price_3", "regular_frozen_dinners_price_4",
           "healthier_frozen_dinners_price_4", "regular_frozen_dinners_price_5", "healthier_frozen_dinners_price_5",
           "regular_frozen_dinners_price_6", "healthier_frozen_dinners_price_6", "lowfat_baked_goods", "diet_soda_varieties",
           "diet_soda_cost", "regular_soda_cost", "healthy_juice_varieties", "healthier_juice_drinks_price", "healthier_juice_drinks_comments",
           "regular_juice_drinks_price", "regular_juice_drinks_comments", "varieties_of_whole_grain_bread", "whole_wheat_bread_price",
           "whole_wheat_bread_size", "white_bread_price", "white_bread_size", "lowfat_chip_varieties", "lowfat_chips_price", "lowfat_chips_size",
           "regular_chips_price", "regular_chips_size", "healthier_cereal_varieties", "healthier_cereal_price", "regular_cereal_price", "lowfat_baked_goods_cost",
           "LocationLatitude", "LocationLongitude"))) |>
    tidyr::replace_na(list(lean_beef_varieties = 0, fat_free_hot_dogs = 0, light_hot_dogs = 0, frozen_dinner_varieties = 0, lowfat_baked_goods = 0,
                    diet_soda_varieties = 0, lowfat_chip_varieties = 0, varieties_of_whole_grain_bread = 0, healthier_cereal_varieties = 0,
                    lowfat_gal = 0, whole_gal = 0, whole_wheat_bread_price = 0, white_bread_price = 0))

  #clean data
  all_data$lowfat_chips_size <- gsubfn::gsubfn("(\\d+) (\\d+)", ~ as.numeric(x) + as.numeric(y),
                                         gsubfn::gsubfn("(\\d+)/(\\d+)", ~ as.numeric(x)/as.numeric(y), all_data$lowfat_chips_size))
  all_data$lowfat_chips_size <- gsub("g","",all_data$lowfat_chips_size)
  all_data$regular_chips_size <- gsubfn::gsubfn("(\\d+) (\\d+)", ~ as.numeric(x) + as.numeric(y),
                                          gsubfn::gsubfn("(\\d+)/(\\d+)", ~ as.numeric(x)/as.numeric(y), all_data$regular_chips_size))
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
    whole_wheat_bread_price - white_bread_price <= 0 ~ 2,
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
    lowfat_chips_price - regular_chips_price >= 0 ~ -1,
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
    healthier_cereal_price - regular_cereal_price >= 0 ~ -1,
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


