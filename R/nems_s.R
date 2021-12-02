# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


convert_files <- function() {
  #convert files given into csv
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
    # 2 points if lowfat/skim milk is available
    lowfat_milk_varieties > 0 ~ 2,
    # 1 additional point if >50% ratio of lowest-fat to whole milk
    lowfat_milk_varieties / whole_milk_varieties > 0.5 ~ 3,
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
    fat_free_hot_dogs == 0 & light_hot_dogs == 0 ~ 0
    # 2 points if there are fat free hot dogs
    fat_free_hot_dogs > 0 ~ 2,
    # 1 point if there are not fat free but there are light hot dogs
    light_hot_dogs > 0 ~ 1,
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
    frozen_dinner_varieties == 2 ~ 2,
    # 2 points if there is one option
    frozen_dinner_varieties == 1 ~ 2,
    # 0 points if there are 0 options
    frozen_dinner_varieties == 0 ~ 0,
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
#' @examples
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
#' @details This function implements the scoring method described in Table 4.1 of NDSU Thesis (Glanz et al., 2007). "Lowfat milk" is considered skim milk or 1 percent fat, whichever is available.
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
#' @details This function implements the scoring method described in Table 4.1 of NDSU Thesis (Glanz et al., 2007). "Standard ground beef" is 80 percent lean and 20 percent fat by weight. "Lean ground beef" is 90 percent lean and 10 percent fat (or less fat percentage) by weight.
#' @param lean_beef_price The price of lean ground beef.
#' @param regular_beef_price The price of normal fat ground beef. (the specific fat content?)
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
#' @details This function implements the scoring method described in Table 4.1 of NDSU Thesis (Glanz et al., 2007). "Fat free hot dogs" have 0 grams of fat per serving, and "light hot dogs" have less than or equal to 7 grams of fat per serving.
#' @param lean_wieners_price The price of hot dogs with less fat (specific fat content?).
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
#' @details This function implements the scoring method described in Table 4.1 of NDSU Thesis (Glanz et al., 2007). "Healthier frozen dinners" have less than or equal to 9g of fat per serving (an 8-11 oz package), as written in Measure 6 in the NEMS-S Protocol.
#' @param healthier_frozen_dinners_price The price of healthier option for frozen dinners. (specifics?)
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
#' @details This function implements the scoring method described in Table 4.1 of NDSU Thesis (Glanz et al., 2007). "Low-fat baked goods" are baked goods with less than or equal to 3 grams of fat per serving.
#' @param healthier_baked_goods_price The price of lower fat baked goods (fat content).
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
#' @details This function implements the scoring method described in Table 4.1 of NDSU Thesis (Glanz et al., 2007). "Diet soda" has 0 kcal per serving.
#' @param diet_soda_price The price of regular soda.
#' @param regular_soda_price The price of healthier soda.
#' @return The NEMS_S points associated with soda price.
#' @examples
#' diet_soda_price <- rnorm(10,4.1,.5)
#' regular_soda_price <- rnorm(10,4.1,.3)
soda_cost <- function(diet_soda_price, regular_soda_price){
  case_when(
    # 0 points if diet soda is more expensive
    diet_soda_price - regular_soda_price < 0 ~ 0,
    # 2 points if diet soda is less expensive
    diet_soda_price - regular_soda_price < 0 ~ 1,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute juice drinks cost points
#'
#' This function takes in the cost of healthier and regular juice and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of NDSU Thesis (Glanz et al., 2007). "Healthy juice" is 100 percent juice drinks, natural fruit juice with no added sugars. "Regular juice" is fruit juice with added sugar and water.
#' @param healthier_juice_drinks_price The cost of 100% juice drinks.
#' @param regular_juice_drinks_price The cost of non 100% juice drinks.
#' @return The NEMS_S points associated with juice drinks cost.
#' @examples
#' healthier_juice_drinks_price <- rnorm(10,4.1,.5)
#' regular_juice_drinks_price <- rnorm(10,4.1,.3)
juice_drinks_cost <- function(healthier_juice_drinks_price, regular_juice_drinks_price){
  case_when(
    # 0 point if 100% juice drink is more expensive
    healthier_juice_drinks_price - regular_juice_drinks_price > 0 ~ 0,
    # 1 point if 100% juice drink is less expensive
    healthier_juice_drinks_price - regular_juice_drinks_price < 0 ~ 1,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute beverage cost points
#'
#' This function takes in the cost of diet soda, regular soda, 100% juice and regular juice drinks and compares them to return an NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of NDSU Thesis (Glanz et al., 2007). "Healthy juice" is 100 percent juice drinks, natural fruit juice with no added sugars. "Regular juice" is fruit juice with added sugar and water.
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
    # need help with this
    # -1 points if 100% juice drink is more expensive and diet soda is more expensive
    healthier_juice_drinks_price - regular_juice_drinks_price > 0 & diet_soda_price - regular_soda_price > 0 ~ -1,
    TRUE ~ as.numeric(NA)
  )
}

#' Compute bread cost points
#'
#' This function takes in the cost of whole wheat bread and white bread and compares them to return a NEMS-S score for the cost.
#'
#' @details This function implements the scoring method described in Table 4.1 of NDSU Thesis (Glanz et al., 2007). "Whole grain bread" is 100% whole wheat and whole grain bread, and it is the healthier option. "White bread" is the regular option, bread made with refined flour.
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
#' @details This function implements the scoring method described in Table 4.1 of NDSU Thesis (Glanz et al., 2007). "Lowfat chips" contain less than or equal to 3g of fat per 1oz serving.
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
#' @details This function implements the scoring method described in Table 4.1 of NDSU Thesis (Glanz et al., 2007). "Healthier cereal" has less than 7g of sugar per serving.
#' @param healthier_cereal_price The price of healthier cereal (<7g of sugar per serving).
#' @param regular_cereal_price The price of regular cereal (>7g of sugar per serving).
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


