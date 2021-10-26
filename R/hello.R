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
milk_avail <- function() {

}

fruit_avail <- function() {

}

vegetable_avail <- function() {

}

ground_beef_avail <- function() {

}

wieners_avail <- function() {

}

frozen_dinners_avail <- function() {

}

baked_goods_avail <- function() {

}

drinks_avail <- function() {

}

bread_avail <- function() {

}

chips_avail <- function() {

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
    lean_wieners_price - regular_wieners_price > 0 ~ -1
    # 2 points if healthier option is less expensive
    lean_wieners_price - regular_wieners_price < 0 ~ 2
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
frozen_dinners_cost <- function(){
  case_when(
    # -1 point if healthier option is more expensive
    healthier_frozen_dinners_price - regular_frozen_dinners_price > 0 ~ -1
    # 2 points if there are 1 or two options
    # need some clarification on what this code should be
    TRUE ~ as.numeric(NA)
  )

}

baked_goods_cost <- function(){

}

drinks_cost <- function(){

}

bread_cost <- function(){

}

chips_cost <- function(){

}

cereal_cost <- function(){

}

cost_score <- function(){

}

# quality (6 points)

