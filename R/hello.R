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

hello <- function() {
  print("Hello, world!")
}

convert_files <- function() {
  #convert files given into csv
}

# Availability (30 points)
#'compute milk availability points
#' @param lowfat_milk_varieties the number of low fat milk cartons available
#' @param whole_milk_varieties the number of whole milk cartons available
#' @return the NEMS-S points associated with milk availability
#' @examples
#' lowfat_milk_varieties <- rnorm(10,,)
#' whole_milk_varieties <- rnorm(10,,)
#' milk_avail(lowfat_milk_varieties, whole_milk_varieties)
milk_avail <- function(lowfat_milk_varieties,whole_milk_varieties) {
  case_when(
    # 2 points if lowfat/skim milk is available
    lowfat_milk_varieties > 0 ~ 2,
    # 1 point if >50% ratio of lowest-fat to whole milk
    lowfat_milk_varieties / whole_milk_varieties > 0.5 ~ 1,
    TRUE ~ as.numeric(NA)
  )
}

#' compute fruit availability points
#' @param varieties_of_fruit
#' @return the NEMS-S points associated with fruit availability
#' @examples
#' varieties_of_fruit <- sample(1:10, 10)
#' fruit_avail(varieties_of_fruit)
fruit_avail <- function(varieties_of_fruit) {
  case_when(
    # 0 points if 0 varieties of fruit are available
    varieties_of_fruit = 0 ~ 0,
    # 1 point if <5 varieties are available
    varieties_of_fruit < 5 ~ 1,
    # 2 points if 5-9 varieties are available
    # varieties_of_fruit -- need some help with this part
    # 3 points if all 10 varieties are available
    varieties_of_fruit = 10 ~ 3,
    TRUE ~ as.numeric(NA)
  )
}

#' compute vegetable availability points
#' @param varieties_of_vegetables number of types of vegetables available
#' @return the NEMS-S points associated with vegetables availability
#' @examples
#' varieties_of_vegetables <- sample(1:10, 10)
#' vegetable_avail(varieties_of_vegetables)
vegetable_avail <- function(varieties_of_vegetables) {
  case_when(
    # 0 points if 0 varieties of vegetables that are available
    varieties_of_vegetables = 0 ~ 0,
    # 1 point if <5 varieties are available
    varieties_of_vegetables < 5 ~ 1,
    # 2 points if 5-9 varieties are available
    # varieties_of_vegetables -- need some help with this part
    # 3 points if all 10 varieties are available
    varieties_of_vegetables = 10 ~ 3,
    TRUE ~ as.numeric(NA)
  )
}

# compute the availability points associated with ground beef
#' @param lean_beef_varieties the number of lean beef varieties (<10% fat)
#' @return the NEMS-S points associated with ground beef availability
#' @examples
#' lean_beef_varieties <- sample(1:4, 10)
#' ground_beef_avail(lean_beef_varieties)
ground_beef_avail <- function(lean_beef_varieties) {
  case_when(
    # 2 points if there is a lean beef option
    lean_beef_varieties = 1 ~ 2,
    # 3 points if there are 2-3 varieties of lean beef
    # lean_beef_varieties ~ need help with the range
    # 4 points if there are >3 varieties of lean beef
    lean_beef_varieties > 3 ~ 4,
    TRUE ~ as.numeric(NA)
  )
}

# compute the availability points associated with wieners
#' @param fat_free_wieners (fat content?)
#' @param light_wieners (specific fat content?)
#' @return the NEMS-S points associated with availability of fat free and light fat wieners
#' @examples
#' fat_free_wieners <- sample()
#' light_wieners <- sample()
#' wieners_avail(fat_free_wieners, light_wieners)
wieners_avail <- function(fat_free_wieners, light_wieners) {
  case_when(
    # 2 points if there is fat free
    # not sure how to do this-- it wouldnt be a numerical value
    # 1 point if there are light but not fat free hot dogs (what are the ranges of these?)
    # need help with this too
  )
}

# compute the availability points associated with frozen dinners
#' @param frozen_dinner_varieties the number of frozen dinner varieties offered (only )
#' @return the NEMS-S points associated with availability of frozen dinners
#' @examples
#' frozen_dinner_varieties <- sample(0:3, 10)
#' frozen_dinners_avail(frozen_dinner_varieties)
frozen_dinners_avail <- function(frozen_dinner_varieties) {
  case_when(
    # 3 points if there are 3 options
    frozen_dinner_varieties = 3 ~ 3,
    # 2 points if there are 2 options
    frozen_dinner_varieties = 2 ~ 2,
    # 2 points if there is one option
    frozen_dinner_varieties = 1 ~ 2,
    # 0 points if there are 0 options
    frozen_dinner_varieties = 0 ~ 0,
    TRUE ~ as.numeric(NA)
  )
}

# compute the availability points associated with low-fat baked goods
#' @param lowfat_baked_goods number of low-fat baked goods
#' @return the NEMS-S points associated with availability of low-fat baked goods
#' @examples
#' lowfat_baked_goods <- sample(0:3, 10)
#' baked_goods_avail(lowfat_baked_goods)
baked_goods_avail <- function(lowfat_baked_goods) {
  case_when(
    # 2 points if there are any low-fat baked goods
    lowfat_baked_goods > 0 ~ 2,
    TRUE ~ as.numeric(NA)
  )
}

# compute the availability points associated with diet soda
#' @param diet_soda_varieties the number of diet sodas available
#' @return the NEMS-S points associated with availability of diet soda
#' @examples
#' diet_soda_varieties <- sample(0:3, 10)
#' soda_avail(diet_soda_varieties)
soda_avail <- function(diet_soda_varieties){
  case_when(
    # 1 point if diet soda is available
    diet_soda_varieties > 0 ~ 1,
    TRUE ~ as.numeric(NA)
  )
}

# compute the availability points associated with 100% juice drinks
#' @param healthy_juice_varieties the number of 100% juice drinks available
#' @return the NEMS-S points associated with availability of healthy juice drinks
#' @examples
#' healthy_juice_varieties <- sample(0:3, 10)
#' juice_drinks_avail(healthy_juice_varieties)
juice_drinks_avail <- function(healthy_juice_varieties){
  case_when(
    # 1 point if 100% juice drinks are available
    healthy_juice_varieties > 0 ~ 1,
    TRUE ~ as.numeric(NA)
  )
}

#' compute whole grain bread availability points
#' @param varieties_of_whole_grain_bread
#' @return the NEMS-S points associated with whole grain bread availability
#' @examples
#' varieties_of_whole_grain_bread <- sample(1:10, 10)
#' bread_avail(varieties_of_whole_grain_bread)
bread_avail <- function(varieties_of_whole_grain_bread) {
  case_when(
    # 2 points if they offer whole grain bread
    varieties_of_whole_grain_bread > 0 ~ 2,
    # additional point if they offer >2 varieties of whole grain bread
    varieties_of_whole_grain_bread > 2 ~ 3,
    TRUE ~ as.numeric(NA)
  )
}

#' compute baked chips availability points
#' @param baked_chips_varieties varieties of baked chips offered
#' @return the NEMS-S points associated with baked chips availability
#' @examples
#' baked_chips_varieties <- sample(1:10, 10)
#' chips_avail(baked_chips_varieties)
chips_avail <- function(baked_chips_varieties) {
  case_when(
    # 2 points for having baked chips
    baked_chips_varieties > 0 ~ 2,
    # 1 additional point for having >2 varieties of baked chips
    baked_chips_varieties > 2 ~ 3,
    TRUE ~ as.numeric(NA)
  )
}

cereal_avail <- function() {

}

# Cost (18 points)

#'compute milk cost points
#' @param lowfat_milk_price the price of low fat milk
#' @param whole_milk_price the price of whole milk
#' @return the NEMS-S points associated with milk price
#' @examples
#' lowfat_milk_price <- rnorm(10,2.8,.5)
#' whole_milk_price <- rnorm(10,3.1,.3)
#' milk_cost(lowfat_milk_price, whole_milk_price)
milk_cost <- function(lowfat_milk_price, whole_milk_price){
  case_when(
    # 2 points if low fat is less expensive than whole
    lowfat_milk_price - whole_milk_price < 0 ~ 2,
    # 1 point if low fat and whole are the same
    lowfat_milk_price == whole_milk_price ~ 1,
    # -1 if whole milk is less expensive
    lowfat_milk_price - whole_milk_price > 0 ~ -1,
    TRUE ~ as.numeric(NA)
  )
}

#'compute ground beef cost points
#' @param lean_beef_price the price of lean ground beef
#' @param regular_beef_price the price of normal fat ground beef
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

#'compute wieners cost points
#' @param lean_wieners_price the price of hot dogs with less fat (specific fat content?)
#' @param regular_wieners_price the price of normal hot dogs
#' @return the NEMS-S points associated with wiener price
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

#'compute frozen dinners cost points
#' @param healthier_frozen_dinners_price the price of healthier option for frozen dinners
#' @param regular_frozen_dinners_price the price of regular fat frozen dinners
#' @return the NEMS-S points associated with frozen dinners price
#' @examples
#' healthier_frozen_dinners_price <- rnorm(10,4.5,.5)
#' regular_frozen_dinners_price <- rnorm(10,5.1,.3)
frozen_dinners_cost <- function(healthier_frozen_dinners_price, regular_frozen_dinners_price){
  case_when(
    # -1 point if healthier option is more expensive
    healthier_frozen_dinners_price - regular_frozen_dinners_price > 0 ~ -1,
    # 2 points if there are 1 or two options
    # need some clarification on what this code should be
    TRUE ~ as.numeric(NA)
  )
}

#'compute baked goods cost points
#' @param healthier_baked_goods_price the price of lower fat baked goods
#' @param regular_baked_goods_price the price of normal fat baked goods
#' @return the NEMS-S points associated with baked goods price
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

#'compute soda cost points
#' @param diet_soda_price the price regular soda
#' @param regular_soda_price the price of healthier soda
#' @return the NEMS_S points associated with soda
#' @examples
#' diet_soda_price <- rnorm(10,4.1,.5)
#' regular_soda_price <- rnorm(10,4.1,.3)
soda_cost <- function(diet_soda_price, regular_soda_price){
  case_when(
    # 2 points if diet soda is less expensive
    diet_soda_price - regular_soda_price < 0 ~ 2,
    TRUE ~ as.numeric(NA)
  )
}

#'compute juice drinks cost points
#' @param healthier_juice_drinks_price the cost of 100% juice drinks
#' @param regular_juice_drinks_price the cost of non 100% juice drinks
#' @return the NEMS_S points associated with juice drinks
#' @examples
#' healthier_juice_drinks_price <- rnorm(10,4.1,.5)
#' regular_juice_drinks_price <- rnorm(10,4.1,.3)
juice_drinks_cost <- function(healthier_juice_drinks_price, regular_juice_drinks_price){
  case_when(
    # -1 if 100% juice drink is more expensive
    healthier_juice_drinks_price - regular_juice_drinks_price > 0 ~ -1
  )
}

#'compute bread cost points
#' @param whole_wheat_bread_price the cost of whole wheat bread
#' @param white_bread_price the cost of white bread
#' @return the NEMS-S points associated with bread cost
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

#'compute chips cost points
#' @param baked_chips_price the price of baked chips
#' @param regular_chips_price the price of regular chips
#' @return the NEMS-S points associated with chips
#' @examples
#' baked_chips_price <- rnorm(10,2.5,.5)
#' regular_chips_price <- rnorm(10,2.8,.3)
chips_cost <- function(baked_chips_price, regular_chips_price){
  case_when(
    # 2 points if baked chips are less expensive than regular chips
    baked_chips_price - regular_chips_price < 0 ~ 2,
    # -1 points if baked chips are more expensive than regular chips
    baked_chips_price - regular_chips_price > 0 ~ -1,
    TRUE ~ as.numeric(NA)
  )
}

#'compute cereal cost points
#' @param healthier_cereal_price the price of healthier cereal (<7g of sugar per serving)
#' @param regular_cereal_price the price of regular cereal (>7g of sugar per serving)
#' @return the NEMS-S points associated with cereal cost
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

