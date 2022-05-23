library(haven)
library(tidyverse)
library(gsubfn)

clean_data <- read_nemss("~/Transpo Research/Nutrition Environments/SPSS Data/NEMS-S+Grocery+Store+Data+Collection+Tool+-+V2_April+1%2C+2022_14.23.sav")

score <- calculate_score(clean_data, detail = FALSE)

