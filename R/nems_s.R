
#' Compute Availability and Cost Scores for all variables
#'
#' This function uses all functions previously created and scores assigned to each variable and adds them into one total score for both availability and cost.
#'
#' @details This function implements the scoring method described in Measure 9
#'   of the NEMS-S Protocol.
#'
#' @param nems_data A dataframe or tibble created by reading the raw qualtrics data
#'   with `read_nems`
#' @return Scores for total Availability and Cost, as well as given the option
#'   if Detail = TRUE to see the score for each individual variable.
#' @examples
#' calculate_nems_score(nems_sample)
#'
#' @export
#'
calculate_nems_score <- function(nems_data, detail = FALSE) {
  scores <- tibble::tibble("ID" = nems_data$STORE_ID) |>
    dplyr::mutate(
      "type" = nems_data$store_type,
      "pharmacy" = nems_data$pharmacy,
      "ethnic" = nems_data$ethnic,
      "merch" = nems_data$merch,
      "registers" = nems_data$register,
      "selfchecko" = nems_data$self_checkout,
      "total_registers" = as.numeric(nems_data$register) +
        as.numeric(nems_data$self_checkout),
      "milk_avail_score" = milk_avail(as.numeric(nems_data$lowfat_gal),
                                      as.numeric(nems_data$whole_gal)),
      "fruit_avail_score" = fruit_avail(as.numeric(nems_data$varieties_of_fruit)),
      "vegetable_avail_score" = vegetable_avail(as.numeric(nems_data$varieties_of_vegetables)),
      "ground_beef_avail_score" = ground_beef_avail(as.numeric(nems_data$lean_beef_varieties)),
      "hot_dog_avail_score" = hot_dog_avail(as.numeric(nems_data$fat_free_hot_dogs),
                                            as.numeric(nems_data$light_hot_dogs)),
      "frozen_dinners_avail_score" = frozen_dinners_avail(
        as.numeric(nems_data$frozen_dinner_varieties)),
      "baked_goods_avail_score" = baked_goods_avail(as.numeric(nems_data$lowfat_baked_goods)),
      "soda_avail_score" = soda_avail(as.numeric(nems_data$diet_soda_varieties)),
      "juice_drinks_avail_score" = juice_drinks_avail(as.numeric(nems_data$healthy_juice_varieties)),
      "bread_avail_score" = bread_avail(as.numeric(nems_data$varieties_of_whole_grain_bread)),
      "chip_avail_score" = chips_avail(as.numeric(nems_data$lowfat_chip_varieties)),
      "cereal_avail_score" = cereal_avail(as.numeric(nems_data$healthier_cereal_varieties)),


      "Total_Availability_Score" = milk_avail_score + fruit_avail_score +
        vegetable_avail_score + ground_beef_avail_score +
        hot_dog_avail_score + frozen_dinners_avail_score + baked_goods_avail_score +
        soda_avail_score + juice_drinks_avail_score + bread_avail_score + chip_avail_score +
        cereal_avail_score
    ) |>

    dplyr::mutate(
      "milk_cost_score" = milk_cost(as.numeric(nems_data$lowfat_gal_price),
                                    as.numeric(nems_data$whole_gal_price)),
      "ground_beef_cost_score" = ground_beef_cost(as.numeric(nems_data$lean_beef_price),
                                                  as.numeric(nems_data$regular_beef_price)),
      "wieners_cost_score" = wieners_cost(as.numeric(nems_data$lean_wieners_price),
                                          as.numeric(nems_data$regular_wieners_price)),
      "frozen_dinners_cost_score" = frozen_dinners_cost(
         as.numeric(nems_data$healthier_frozen_dinners_price_1),
         as.numeric(nems_data$regular_frozen_dinners_price_1)),

      "baked_goods_cost_score" = baked_goods_cost(as.numeric(nems_data$lowfat_baked_goods_cost)),
      "soda_cost_score" = soda_cost(as.numeric(nems_data$diet_soda_cost),
                                    as.numeric(nems_data$regular_soda_cost)),
      "juice_cost_score" = juice_cost(as.numeric(nems_data$healthier_juice_drinks_price),
                                      as.numeric(nems_data$regular_juice_drinks_price)),
      "juice_drinks_cost_score" = juice_drinks_cost(as.numeric(nems_data$diet_soda_cost),
                                                    as.numeric(nems_data$regular_soda_cost),
                                                    as.numeric(nems_data$healthier_juice_drinks_price),
                                                    as.numeric(nems_data$regular_juice_drinks_price)),
      "bread_cost_score" = bread_cost(as.numeric(nems_data$whole_wheat_bread_price) /
                                        as.numeric(nems_data$whole_wheat_bread_size),
                                      as.numeric(nems_data$white_bread_price) /
                                        as.numeric(nems_data$white_bread_size)),
      "chips_cost_score" = chips_cost(as.numeric(nems_data$lowfat_chips_price) /
                                        as.numeric(nems_data$lowfat_chips_size),
                                      as.numeric(nems_data$regular_chips_price) /
                                        as.numeric(nems_data$regular_chips_size)),
      "cereal_cost_score" = cereal_cost(as.numeric(nems_data$healthier_cereal_price),
                                        as.numeric(nems_data$regular_cereal_price)),
      "Latitude" = nems_data$LocationLatitude,
      "Longitude" = nems_data$LocationLongitude
    ) |>
    tidyr::replace_na(
      list(milk_cost_score = 0, ground_beef_cost_score = 0,
           wieners_cost_score = 0, frozen_dinners_cost_score = 0,
           soda_cost_score = 0, juice_cost_score = 0,
           juice_drinks_cost_score = 0, bread_cost_score = 0,
           chips_cost_score = 0, cereal_cost_score = 0
      )
    ) |>

    dplyr::mutate(
      "Total_Cost_Score" = milk_cost_score + ground_beef_cost_score +
        wieners_cost_score + frozen_dinners_cost_score + soda_cost_score +
        juice_cost_score + juice_drinks_cost_score + bread_cost_score +
        chips_cost_score + cereal_cost_score
    )

  if(detail) {
    dplyr::select(scores, c(
      ID, type, pharmacy, ethnic, merch, registers, selfchecko, total_registers,
      milk_avail_score, fruit_avail_score, vegetable_avail_score,
      ground_beef_avail_score, hot_dog_avail_score, frozen_dinners_avail_score,
      baked_goods_avail_score, soda_avail_score, juice_drinks_avail_score,
      bread_avail_score, chip_avail_score, cereal_avail_score,
      Total_Availability_Score, milk_cost_score, ground_beef_cost_score,
      wieners_cost_score, frozen_dinners_cost_score, soda_cost_score, juice_cost_score,
      juice_drinks_cost_score, bread_cost_score, chips_cost_score, cereal_cost_score,
      Total_Cost_Score, Latitude, Longitude)
    )
  } else {
    dplyr::select(scores, c(
      ID, type, pharmacy, ethnic, merch, registers, selfchecko, total_registers,
      Total_Availability_Score, Total_Cost_Score, Latitude, Longitude)
    )
  }

}



#' Compute market basket cost at a store
#'
#' @details This function applies the market basket outline found in the
#'   [Thrifty Food Plan, 2021](https://fns-prod.azureedge.us/sites/default/files/resource-files/TFP2021.pdf)
#'   Some adjustments needed to be made because the NEMS-S survey is mostly
#'   focused on the availability of low-calorie and low-fat options, and the
#'   market basket is based more on the price of observed consumer purchases. For
#'   example, the market basket includes a certain amount of poultry, but because
#'   low-fat poultry is not really a thing in the way that lean beef is, the NEMS-S
#'   instrument does not collect the price of poultry.
#'
#'   Additionally, if a price was not available, we replaced it with the average
#'   price for all stores in the dataset.
#'
#' @param nems_data A dataframe or tibble containing one row per store, cleaned
#'   from raw qualtrics survey data with the read_nemss function
#' @return A tibble containing the weighted price for several types of goods
#'   including dairy, grain, etc. An additional column is appended with the
#'   number of replacements with mean values that had to be made in each type.
#'   A Total column  contains the cost of filling the market basket in that store.
#' @examples
#' calculate_market_basket(nems_sample)
#'
#' @export
calculate_market_basket <- function(nems_data) {

  prices <- tibble::tibble("ID" = nems_data$STORE_ID) |>
    dplyr::mutate("bread_price" = as.numeric(nems_data$whole_wheat_bread_price) /
                        as.numeric(nems_data$whole_wheat_bread_size) -
                    as.numeric(nems_data$white_bread_price) /
                        as.numeric(nems_data$white_bread_size)) |>
    dplyr::mutate("milk_price" = as.numeric(nems_data$whole_gal_price) -
                    as.numeric(nems_data$lowfat_gal_price))

  # Compute average prices for market basket goods;
  # this is used to replace values that are missing
  average_bread <- mean(prices$bread_price, na.rm = TRUE)
  average_wheat <- mean(as.numeric(nems_data$whole_wheat_bread_price) /
                          as.numeric(nems_data$whole_wheat_bread_size), na.rm = TRUE)
  average_white <- mean(as.numeric(nems_data$white_bread_price) /
                          as.numeric(nems_data$white_bread_size), na.rm = TRUE)
  average_milk <- mean(prices$milk_price, na.rm = TRUE)
  average_whole <- mean(as.numeric(nems_data$whole_gal_price), na.rm = TRUE)
  average_lowfat <- mean(as.numeric(nems_data$lowfat_gal_price), na.rm = TRUE)
  average_carrot <- mean(as.numeric(nems_data$carrot_price), na.rm = TRUE)
  average_tomato <- mean(as.numeric(nems_data$tomato_price), na.rm = TRUE)
  average_broccoli <- mean(as.numeric(nems_data$broccoli_price), na.rm = TRUE)
  average_lettuce <- mean(as.numeric(nems_data$lettuce_price), na.rm = TRUE)
  average_cucumber <- mean(as.numeric(nems_data$cucumber_price), na.rm = TRUE)
  average_corn <- mean(as.numeric(nems_data$corn_price), na.rm = TRUE)
  average_cauliflower <- mean(as.numeric(nems_data$cauliflower_price), na.rm = TRUE)
  average_apple <- mean(as.numeric(nems_data$apple_price), na.rm = TRUE)
  average_banana <- mean(as.numeric(nems_data$banana_price), na.rm = TRUE)
  average_orange <- mean(as.numeric(nems_data$orange_price), na.rm = TRUE)
  average_grape <- mean(as.numeric(nems_data$grape_price), na.rm = TRUE)
  average_soda <- mean(as.numeric(nems_data$regular_soda_cost), na.rm = TRUE)
  average_beef <- mean(as.numeric(nems_data$regular_beef_price), na.rm = TRUE)
  average_frozen_dinner <- mean(as.numeric(nems_data$regular_frozen_dinners_price_1), na.rm = TRUE)


  average <- tibble::tibble(average_bread, average_milk)


  # calculate the per-good prices
  scores <- tibble::tibble("ID" = nems_data$STORE_ID) |>
    dplyr::mutate(
      "grain_replacements" =
        dplyr::if_else(nems_data$whole_wheat_bread_price == "0", 1, 0) +
        dplyr::if_else(nems_data$white_bread_price == "0", 1, 0),

      "grain" = dplyr::case_when(
        grain_replacements == 2 ~ average_white*16*5.65*1.25 + average_wheat*16*6.7*1.25,
        grain_replacements == 0 ~
          as.numeric(nems_data$whole_wheat_bread_price) /
              as.numeric(nems_data$whole_wheat_bread_size)*16*6.7 +
          as.numeric(nems_data$white_bread_price) /
              as.numeric(nems_data$white_bread_size)*16*5.65,
        nems_data$white_bread_price == "" ~
          (as.numeric(nems_data$whole_wheat_bread_price) /
            as.numeric(nems_data$whole_wheat_bread_size) + average_bread)*16*5.65 +
          as.numeric(nems_data$whole_wheat_bread_price) /
            as.numeric(nems_data$whole_wheat_bread_size)*16*6.7,
        TRUE ~ (as.numeric(nems_data$white_bread_price) /
            as.numeric(nems_data$white_bread_size) -
          average_bread)*16*6.7 + as.numeric(nems_data$white_bread_price) /
          as.numeric(nems_data$white_bread_size)*16*5.65),


      "dairy_replacements" = dplyr::if_else(nems_data$whole_gal_price == "", 1, 0) +
        dplyr::if_else(nems_data$lowfat_gal_price == "", 1, 0),

      "dairy" =
        dplyr::if_else(
          dairy_replacements == 2,
          average_whole/18.6*15.13 + average_lowfat/18.6*25.48,
          dplyr::if_else(
            dairy_replacements == 0,
            as.numeric(nems_data$whole_gal_price)/8.6*15.13 + as.numeric(nems_data$lowfat_gal_price)/8.6*25.48,
            dplyr::if_else(
              nems_data$whole_gal_price == "",
              (as.numeric(nems_data$lowfat_gal_price)-average_milk) / 8.6*25.48 +
                (as.numeric(nems_data$lowfat_gal_price)-average_milk)/8.6*15.13,
              (as.numeric(nems_data$whole_gal_price)-average_milk)/8.6*25.48 +
                as.numeric(nems_data$whole_gal_price)/8.6*15.13))),


      "vegetable_replacements" =
        dplyr::if_else(nems_data$carrot_price == "", 1, 0) +
        dplyr::if_else(nems_data$tomato_price == "", 1, 0) +
        dplyr::if_else(nems_data$broccoli_price == "", 1, 0) +
        dplyr::if_else(nems_data$lettuce_price == "", 1, 0) +
        dplyr::if_else(nems_data$cucumber_price == "", 1, 0) +
        dplyr::if_else(nems_data$corn_price == "", 1, 0) +
        dplyr::if_else(nems_data$cauliflower_price == "", 1, 0),


      "vegetable" =
        dplyr::if_else(
          nems_data$carrot_price == "",
          # missing carrot prices
          dplyr::if_else(
            nems_data$tomato_price == "",
            average_carrot*4.14*1.25,
            as.numeric(nems_data$tomato_price)*4.14),
          as.numeric(nems_data$carrot_price)*4.14) +
        dplyr::if_else(
          nems_data$tomato_price == "",
          dplyr::if_else(
            nems_data$carrot_price == "",
            average_tomato*4.14*1.25,
            as.numeric(nems_data$carrot_price)*4.14),
          as.numeric(nems_data$tomato_price)*4.14) +
        dplyr::if_else(
          nems_data$broccoli_price == "",
          dplyr::if_else(
            nems_data$lettuce_price == "",
            dplyr::if_else(
              nems_data$cucumber_price == "",
              average_broccoli*1.23*1.25,
              as.numeric(nems_data$cucumber_price)*1.23),
            as.numeric(nems_data$lettuce_price)*1.23),
          as.numeric(nems_data$broccoli_price)*1.23) +
        dplyr::if_else(
          nems_data$lettuce_price == "",
          dplyr::if_else(
            nems_data$cucumber_price == "",
            dplyr::if_else(
              nems_data$broccoli_price == "",
              average_lettuce*1*1.25,
              as.numeric(nems_data$broccoli_price)*1),
            as.numeric(nems_data$cucumber_price)*1),
          as.numeric(nems_data$lettuce_price)*1) +
        dplyr::if_else(
          nems_data$cucumber_price == "",
          dplyr::if_else(
            nems_data$broccoli_price == "",
            dplyr::if_else(
              nems_data$lettuce_price == "",
              average_cucumber*1*1.25,
              as.numeric(nems_data$lettuce_price)*1),
            as.numeric(nems_data$broccoli_price)*1),
          as.numeric(nems_data$cucumber_price)*1) +
        dplyr::if_else(
          nems_data$corn_price == "",
          dplyr::if_else(
            nems_data$cauliflower_price == "",
            average_corn*1.25*5.64,
            as.numeric(nems_data$cauliflower_price)*5.64),
          as.numeric(nems_data$corn_price)*5.64) +
        dplyr::if_else(
          nems_data$cauliflower_price == "",
          dplyr::if_else(
            nems_data$corn_price == "",
            average_cauliflower*1.25*5,
            as.numeric(nems_data$corn_price)*5),
          as.numeric(nems_data$cauliflower_price)*5),



      "fruit_replacements" =
        dplyr::if_else(nems_data$apple_price == "", 1, 0) +
        dplyr::if_else(nems_data$banana_price == "", 1, 0) +
        dplyr::if_else(nems_data$orange_price == "", 1, 0) +
        dplyr::if_else(nems_data$grape_price == "", 1, 0),

      "fruit" = dplyr::if_else( nems_data$apple_price == "",
                                average_apple*5*1.25,
                                as.numeric(nems_data$apple_price)*5) +
        dplyr::if_else(nems_data$banana_price == "",
                       average_banana*5*1.25,
                       as.numeric(nems_data$banana_price)*5) +
        dplyr::if_else(nems_data$orange_price == "",
                       average_orange*5*1.25,
                       as.numeric(nems_data$orange_price)*5) +
        dplyr::if_else(nems_data$grape_price == "",
                       average_grape*2.76*1.25,
                       as.numeric(nems_data$grape_price)*2.76),


      "soda_replacements" = dplyr::if_else(nems_data$regular_soda_cost == "", 1, 0),
       "soda" = dplyr::if_else(nems_data$regular_soda_cost == "",
                               average_soda/144*16*.57*1.25,
                               as.numeric(nems_data$regular_soda_cost)/144*16*.57),

      "beef_replacements" = dplyr::if_else(nems_data$regular_beef_price == "", 1, 0),
      "beef" = dplyr::if_else(nems_data$regular_beef_price == "",
                              average_beef*2.26*1.25,
                              as.numeric(nems_data$regular_beef_price)*2.26),

      "frozen_dinner_replacements" =
        dplyr::if_else(nems_data$regular_frozen_dinners_price_1 == "" &
                         nems_data$regular_frozen_dinners_price_2 == "" &
                         nems_data$regular_frozen_dinners_price_3 == "" &
                         nems_data$regular_frozen_dinners_price_4 == "" &
                         nems_data$regular_frozen_dinners_price_5 == "" &
                         nems_data$regular_frozen_dinners_price_6 == "", 1, 0),
       "frozen_dinner" = dplyr::if_else(
         nems_data$regular_frozen_dinners_price_1 != "",
         as.numeric(nems_data$regular_frozen_dinners_price_1)*1.51,
         dplyr::if_else(
           nems_data$regular_frozen_dinners_price_2 != "",
           as.numeric(nems_data$regular_frozen_dinners_price_2)*1.51,
           dplyr::if_else(
             nems_data$regular_frozen_dinners_price_3 != "",
             as.numeric(nems_data$regular_frozen_dinners_price_3)*1.51,
             dplyr::if_else(
               nems_data$regular_frozen_dinners_price_4 != "",
               as.numeric(nems_data$regular_frozen_dinners_price_4)*1.51,
               dplyr::if_else(
                 nems_data$regular_frozen_dinners_price_5 != "",
                 as.numeric(nems_data$regular_frozen_dinners_price_5)*1.51,
                 dplyr::if_else(
                   nems_data$regular_frozen_dinners_price_6 != "",
                   as.numeric(nems_data$regular_frozen_dinners_price_6)*1.51,
                   average_frozen_dinner*1.51)))))),

      "total_replacements" = grain_replacements + dairy_replacements +
        vegetable_replacements + fruit_replacements + soda_replacements +
        beef_replacements + frozen_dinner_replacements,
      "total" = grain + dairy + vegetable + fruit + soda + beef + frozen_dinner
    )

  scores
}


#' Compute Difference in "Healthy" and "Less Healthy" food options
#'
#' This function finds the average difference in price between the healthier and
#' less healthy food options for a store
#'
#' @details This function is used to determine adequate replacements for the market basket prices.
#' @param clean_data The data retrieved from a cleaned version of the Qualtrics Survey.
#' @return Average_Difference for the average price difference.
#' @examples
#' clean_data <- read_nemss(file)
#' find_difference(clean_data)
#'
average_difference <- function(clean_data){
  prices <- data.frame("ID" = clean_data$STORE_ID)
  prices <- prices |>
    dplyr::mutate("bread_price" = as.numeric(clean_data$whole_wheat_bread_price)/as.numeric(clean_data$whole_wheat_bread_size) - as.numeric(clean_data$white_bread_price)/as.numeric(clean_data$white_bread_size)) |>
    dplyr::mutate("milk_price" = as.numeric(clean_data$whole_gal_price) - as.numeric(clean_data$lowfat_gal_price))

  average_bread <- mean(prices$bread_price, na.rm = TRUE)
  average_milk <- mean(prices$milk_price, na.rm = TRUE)
  average <- data.frame(average_bread, average_milk)


  average
}


#' Import and Clean Qualtrics NEMS_S Dataset
#'
#' This function takes the output of the Qualtrics survey and returns it as a
#' cleaned R dataframe.
#'
#' @param file The data file retrieved from the Qualtrics survey.
#'
#' @return A cleaned R dataframe with availability values and cost values for each variable.
#' @examples
#' file <- system.file("extdata", "example_nems.sav", package = "nemsr")
#' read_nemss(file)
#'
#' @importFrom dplyr mutate if_else
#' @importFrom haven read_sav
#'
#' @export
read_nemss <- function(file) {
  SAS_data <- haven::read_sav(file)


  # At some point during data collection we changed the name of
  # the fields used to collect dairy information. This makes all the data fill
  # the same columns in the R data frame.
  if(!"MILK_PRICE_LFQ_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_LFQ_1_1 <- SAS_data$MILK_PRICE_LFQ_1_PRICE_1
  if(!"MILK_PRICE_LQH_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_LQH_1_1 <- SAS_data$MILK_PRICE_LQH_1_PRICE_1
  if(!"MILK_PRICE_LFG_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_LFG_1_1 <- SAS_data$MILK_PRICE_LFG_1_PRICE_1
  if(!"MILK_PRICE_WHLQ_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_WHLQ_1_1 <- SAS_data$MILK_PRICE_WHLQ_1_PRICE_1
  if(!"MILK_PRICE_WHLH_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_WHLH_1_1 <- SAS_data$MILK_PRICE_WHLH_1_PRICE_1
  if(!"MILK_PRICE_WHLG_1_1"%in% colnames(SAS_data)) SAS_data$MILK_PRICE_WHLG_1_1 <- SAS_data$MILK_PRICE_WHLG_1_PRICE_1
  if(!"BVG_1_5_1"%in% colnames(SAS_data)) SAS_data$BVG_1_5_1 <- 0



  # Make descriptive names that are easier to work with in R.
  all_data <- SAS_data |>
    dplyr::mutate(
      "store_type" = dplyr::case_when(
        STORE_T == 1 ~ "Grocery Store",
        STORE_T == 2 ~ "Convenience Store",
        STORE_T == 3 ~ "Other",
        STORE_T == 4 ~ "Dollar Store",
        TRUE ~ "Trading Post"
      ),
      "pharmacy" = dplyr::if_else(STORE_T2_2 == 1|STORE_T2_3 == 1, TRUE, FALSE, missing = FALSE),
      "ethnic" = dplyr::if_else(STORE_T2_4 == 1, TRUE, FALSE, missing = FALSE),
      "merch" = dplyr::if_else(STORE_T2_6 == 1, TRUE, FALSE, missing = FALSE),
      "register" = dplyr::if_else(A_1_1_1 == ".", "0", A_1_1_1),
      "self_checkout" = dplyr::if_else(A_2_1_1 == ".", "0", A_2_1_1),
      "lowfat_milk_avail" = dplyr::if_else(MILK_2A_1 == 1, TRUE, FALSE),
      "dairy_milk_avail" = dplyr::if_else(MILK_1_AVAIL_1 == 1, TRUE, FALSE),
      "lowfat_pint" = MILK_SHELF_LF_1_SPACE_1,
      "lowfat_quart" = MILK_SHELF_LF_1_SPACE_2,
      "lowfat_half_gal" = MILK_SHELF_LF_1_SPACE_3,
      "lowfat_gal" = as.numeric(dplyr::if_else(
        MILK_SHELF_LF_1_SPACE_4 == "", "0", MILK_SHELF_LF_1_SPACE_4)),
      "whole_pint" = MILK_SHELF_WHL_1_SPACE_1,
      "whole_quart" = MILK_SHELF_WHL_1_SPACE_2,
      "whole_half_gal" = MILK_SHELF_WHL_1_SPACE_3,
      "whole_gal" = as.numeric(dplyr::if_else(
        MILK_SHELF_WHL_1_SPACE_4 == "", "0", MILK_SHELF_WHL_1_SPACE_4)),
      "lowfat_quart_price" = MILK_PRICE_LFQ_1_1,
      "lowfat_half_gal_price" = MILK_PRICE_LQH_1_1,
      "lowfat_gal_price" = MILK_PRICE_LFG_1_1,
      "whole_quart_price" = MILK_PRICE_WHLQ_1_1,
      "whole_half_gal_price" = MILK_PRICE_WHLH_1_1,
      "whole_gal_price" = MILK_PRICE_WHLG_1_1,
      "lean_beef_varieties" = BEEF_H_CNT,
      "lean_beef_price" = dplyr::if_else(
        BEEF_H_GS_3_1 == "", dplyr::if_else(
          BEEF_H_LGB_3_1 == "", BEEF_H_GT_3_1, BEEF_H_LGB_3_1),
        BEEF_H_GS_3_1),
      "regular_beef_price" = dplyr::if_else(
        BEEF_R_80_2_PRICE_1 == "",  BEEF_R_ALT_2_PRICE_1, BEEF_R_80_2_PRICE_1),
      "varieties_of_fruit" = dplyr::if_else(FRUIT_CNT == "", "0", FRUIT_CNT),
      "apple_price" = FRUIT_AVAIL_APP_2_1,
      "banana_price" = FRUIT_AVAIL_BAN_2_1,
      "orange_price" = FRUIT_AVAIL_ORA_2_1,
      "grape_price" = FRUIT_AVAIL_GRA_2_1,
      "varieties_of_vegetables" = dplyr::if_else(VEG_CNT == "", "0", VEG_CNT),
      "carrot_price" = VEG_AVAIL_CAR_2_1,
      "tomato_price" = VEG_AVAIL_TOM_2_1,
      "broccoli_price" = VEG_AVAIL_BRO_2_1,
      "lettuce_price" = VEG_AVAIL_LET_2_1,
      "cucumber_price" = VEG_AVAIL_CUC_2_1,
      "corn_price" = VEG_AVAIL_COR_2_1,
      "cauliflower_price" = VEG_AVAIL_CAU_2_1,


      "fat_free_hot_dogs" = dplyr::if_else(HD_H_OM_1==1, 1, 0),
      "light_hot_dogs" = dplyr::if_else(HD_H_OTH_1==1,1,0),
      "lean_wieners_price" = dplyr::if_else(HD_H_OM_2_1 == "", HD_H_OTH_2_1, HD_H_OM_2_1),
      "lean_wieners_size" = dplyr::if_else(HD_H_OM_3_1 == "", HD_H_OTH_3_1, HD_H_OM_3_1),
      "regular_wieners_price" = dplyr::if_else(HD_R_OM_2_1 == "", HD_R_OTH_2_1, HD_R_OM_2_1),
      "regular_wieners_size" = dplyr::if_else(HD_R_OM_3_1 == "", HD_R_OTH_3_1, HD_R_OM_3_1),
      "frozen_dinners_pref_avail" = dplyr::if_else(FRZ_PRICE_PREF_LAS_1_REDFAT_1 != "", 1, 0) +
        dplyr::if_else(FRZ_PRICE_PREF_RTB_1_REDFAT_1 != "", 1, 0) +
        dplyr::if_else(FRZ_PRICE_PREF_MEA_1_REDFAT_1 != "", 1, 0) +
        dplyr::if_else(FRZ_PRICE_PREF_LAS_2_REG_1 != "", 1, 0) +
        dplyr::if_else(FRZ_PRICE_PREF_RTB_2_REG_1 != "", 1, 0) +
        dplyr::if_else(FRZ_PRICE_PREF_MEA_2_REG_1 != "", 1, 0),

      "frozen_dinners_oth_avail" = dplyr::if_else(FRZ_PRICE_OTH_1_1_REDFAT_2 != "", 1, 0) +
        dplyr::if_else(FRZ_PRICE_OTH_2_1_REDFAT_2 != "", 1, 0) +
        dplyr::if_else(FRZ_PRICE_OTH_3_1_REDFAT_2 != "", 1, 0) +
        dplyr::if_else(FRZ_PRICE_OTH_1_2_ALT_2 != "", 1, 0) +
        dplyr::if_else(FRZ_PRICE_OTH_2_2_ALT_2 != "", 1, 0) +
        dplyr::if_else(FRZ_PRICE_OTH_3_2_ALT_2 != "", 1, 0),
      "frozen_dinner_varieties" = (frozen_dinners_pref_avail + frozen_dinners_oth_avail)/2,
      "healthier_frozen_dinners_price_1" = FRZ_PRICE_PREF_LAS_1_REDFAT_2,
      "regular_frozen_dinners_price_1" = FRZ_PRICE_PREF_LAS_2_REG_2,
      "healthier_frozen_dinners_price_2" = FRZ_PRICE_PREF_RTB_1_REDFAT_2,
      "regular_frozen_dinners_price_2" = FRZ_PRICE_PREF_RTB_2_REG_2,
      "healthier_frozen_dinners_price_3" = FRZ_PRICE_PREF_MEA_1_REDFAT_2,
      "regular_frozen_dinners_price_3" = FRZ_PRICE_PREF_MEA_2_REG_2,
      "healthier_frozen_dinners_price_4" = FRZ_PRICE_OTH_1_1_REDFAT_1,
      "regular_frozen_dinners_price_4" = FRZ_PRICE_OTH_1_2_ALT_1,
      "healthier_frozen_dinners_price_5" = FRZ_PRICE_OTH_2_1_REDFAT_1,
      "regular_frozen_dinners_price_5" = FRZ_PRICE_OTH_2_2_ALT_1,
      "healthier_frozen_dinners_price_6" = FRZ_PRICE_OTH_3_1_REDFAT_1,
      "regular_frozen_dinners_price_6" = FRZ_PRICE_OTH_3_2_ALT_1,
      "lowfat_baked_goods" = dplyr::if_else(BKD_H_BG1_1==1|BKD_H_BGPK_1==1|
                                              BKD_H_ENG_1==1|BKD_H_LFM_1==1, 1, 0),
      "lowfat_baked_goods_cost" = dplyr::if_else(
        BKD_H_BG1_2_1 != "", BKD_H_BG1_2_1,
        dplyr::if_else(BKD_H_BGPK_2_1 != "", BKD_H_BGPK_2_1, "0")),



      "diet_soda_varieties" = dplyr::if_else(BVG_HS_DC_1_AVAIL==1|
                                               BVG_HS_DOTH_1_AVAIL==1, 1, 0),
      "diet_soda_cost" = dplyr::if_else(
        BVG_HS_DC_2_PRICE_1 == "", BVG_HS_DOTH_2_PRICE_1, BVG_HS_DC_2_PRICE_1),
      "regular_soda_cost" = dplyr::if_else(
        BVG_RS_COK_2_PRICE_1 == "", BVG_RS_OTH_2_PRICE_1, BVG_RS_COK_2_PRICE_1),

      "healthy_juice_varieties" = dplyr::if_else(BVG_1_5_1 == 1, 1, 0),
      "healthier_juice_drinks_price" = dplyr::if_else(
        BVG_HJ_MM_2_PRICE_1 == "",
        dplyr::if_else(BVG_HJ_TRO_2_PRICE_1 == "", BVG_HJ_OTH_2_PRICE_1, BVG_HJ_TRO_2_PRICE_1),
        BVG_HJ_MM_2_PRICE_1),
      "healthier_juice_drinks_comments" = dplyr::if_else(
        BVG_HJ_MM_3_COMM_1 == "", dplyr::if_else(
          BVG_HJ_TRO_3_COMM_1 == "", BVG_HJ_OTH_3_COMM_1, BVG_HJ_TRO_3_COMM_1),
        BVG_HJ_MM_3_COMM_1),
      "regular_juice_drinks_price" = dplyr::if_else(
        BVG_RJ_MM_2_PRICE_1 == "", dplyr::if_else(
          BVG_RJ_TRO_2_PRICE_1 == "", BVG_RJ_OTH_2_PRICE_1, BVG_RJ_TRO_2_PRICE_1),
        BVG_RJ_MM_2_PRICE_1),
      "regular_juice_drinks_comments" = dplyr::if_else(
        BVG_RJ_MM_3_COMM_1 == "", dplyr::if_else(
          BVG_RJ_TRO_3_COMM_1 == "", BVG_RJ_OTH_3_COMM_1, BVG_RJ_TRO_3_COMM_1),
        BVG_RJ_MM_3_COMM_1),

      "varieties_of_whole_grain_bread" = BRD_H_CNT,
      "whole_wheat_bread_price" = as.numeric(
        dplyr::if_else(BRD_H_NO_3_PRICE_1 == "",
                       dplyr::if_else(BRD_H_SLC_3_PRICE_1 == "", BRD_H_OTH_3_PRICE_1,
                                      BRD_H_SLC_3_PRICE_1), BRD_H_NO_3_PRICE_1)),
      "whole_wheat_bread_size" = dplyr::if_else(
        BRD_H_NO_2_OZ_1 == "", dplyr::if_else(
          BRD_H_SLC_2_OZ_1 == "", BRD_H_OTH_2_OZ_1, BRD_H_SLC_2_OZ_1),
        BRD_H_NO_2_OZ_1),
      "white_bread_price" = as.numeric(
        dplyr::if_else(BRD_R_NO_3_PRICE_1 == "", dplyr::if_else(
          BRD_R_SLC_3_PRICE_1 == "", BRD_R_OTH_3_PRICE_1, BRD_R_SLC_3_PRICE_1),
          BRD_R_NO_3_PRICE_1)),
      "white_bread_size" = dplyr::if_else(
        BRD_R_NO_2_OZ_1 == "", dplyr::if_else(
          BRD_R_SLC_2_OZ_1 == "", BRD_R_OTH_2_OZ_1, BRD_R_SLC_2_OZ_1),
        BRD_R_NO_2_OZ_1),
      "lowfat_chip_varieties" = CHIP_H_CNT,
      "lowfat_chips_price" = dplyr::if_else(
        CHIP_H_BL_3_PRICE_1 == "", CHIP_H_OTH_3_PRICE_1, CHIP_H_BL_3_PRICE_1),
      "lowfat_chips_size" = dplyr::if_else(
        CHIP_H_BL_1_SIZE_1 == "", CHIP_H_OTH_1_SIZE_1, CHIP_H_BL_1_SIZE_1),
      "regular_chips_price" = dplyr::if_else(
        CHIP_R_LP_3_PRICE_1 == "", CHIP_R_OTH_3_PRICE_1, CHIP_R_LP_3_PRICE_1),
      "regular_chips_size" = dplyr::if_else(
        CHIP_R_LP_1_SIZE_1 == "", CHIP_R_OTH_1_SIZE_1, CHIP_R_LP_1_SIZE_1),
      "healthier_cereal_varieties" = CRL_H_CNT,
      "healthier_cereal_price" = dplyr::if_else(
        CRL_H_CHE_3_PRICE_1 == "", CRL_H_OTH_3_PRICE_1, CRL_H_CHE_3_PRICE_1),
      "regular_cereal_price" = dplyr::if_else(
        CRL_R_FCH_3_PRICE_1 == "", CRL_R_OTH_3_PRICE_1, CRL_R_FCH_3_PRICE_1)
    ) |>


    dplyr::select(tidyselect::any_of(
      c("STORE_ID", "store_type", "pharmacy", "ethnic", "merch", "register",
        "self_checkout", "total_registers", "organic_milk_avail", "lowfat_milk_avail",
        "dairy_milk_avail", "nondairy_milk_avail", "lowfat_pint", "lowfat_quart",
        "lowfat_half_gal", "lowfat_gal", "whole_pint", "whole_quart", "whole_half_gal",
        "whole_gal", "lowfat_quart_price", "lowfat_half_gal_price", "lowfat_gal_price",
        "whole_quart_price", "whole_half_gal_price", "whole_gal_price", "lean_beef_varieties",
        "lean_beef_price", "regular_beef_price", "varieties_of_fruit", "apple_price",
        "banana_price", "orange_price", "grape_price", "varieties_of_vegetables",
        "carrot_price", "tomato_price", "broccoli_price", "lettuce_price",
        "cucumber_price", "corn_price", "cauliflower_price", "fat_free_hot_dogs",
        "light_hot_dogs", "lean_wieners_price", "lean_wieners_size", "regular_wieners_price",
        "regular_wieners_size", "frozen_dinner_varieties", "regular_frozen_dinners_price_1",
        "healthier_frozen_dinners_price_1", "regular_frozen_dinners_price_2",
        "healthier_frozen_dinners_price_2", "regular_frozen_dinners_price_3",
        "healthier_frozen_dinners_price_3", "regular_frozen_dinners_price_4",
        "healthier_frozen_dinners_price_4", "regular_frozen_dinners_price_5",
        "healthier_frozen_dinners_price_5", "regular_frozen_dinners_price_6",
        "healthier_frozen_dinners_price_6",
        "lowfat_baked_goods",
        "diet_soda_varieties", "diet_soda_cost", "regular_soda_cost",
        "healthy_juice_varieties", "healthier_juice_drinks_price",
        "healthier_juice_drinks_comments", "regular_juice_drinks_price",
        "regular_juice_drinks_comments", "varieties_of_whole_grain_bread",
        "whole_wheat_bread_price", "whole_wheat_bread_size", "white_bread_price",
        "white_bread_size", "lowfat_chip_varieties", "lowfat_chips_price",
        "lowfat_chips_size", "regular_chips_price", "regular_chips_size",
        "healthier_cereal_varieties", "healthier_cereal_price",
        "regular_cereal_price", "lowfat_baked_goods_cost",
        "LocationLatitude", "LocationLongitude"))
    ) |>
    tidyr::replace_na(list(lean_beef_varieties = 0, fat_free_hot_dogs = 0,
                           light_hot_dogs = 0, frozen_dinner_varieties = 0,
                           lowfat_baked_goods = 0, diet_soda_varieties = 0,
                           lowfat_chip_varieties = 0,
                           varieties_of_whole_grain_bread = 0,
                           healthier_cereal_varieties = 0, lowfat_gal = 0,
                           whole_gal = 0, whole_wheat_bread_price = 0,
                           white_bread_price = 0))

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


  tibble::as_tibble(clean_data)


}


