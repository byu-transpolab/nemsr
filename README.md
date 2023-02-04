
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nemsr

<!-- badges: start -->

[![R-CMD-check](https://github.com/byu-transpolab/nemsr/workflows/R-CMD-check/badge.svg)](https://github.com/byu-transpolab/nemsr/actions)
<!-- badges: end -->

The goal of nemsr is to enable researchers to quickly calculate NEMS-S
scores for individual grocery stores using a Qualtrics-based NEMS-S
survey developed at BYU.

## Installation

<!--
You can install the released version of nemsr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nemsr")
```
-->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("byu-transpolab/nemsr")
```

## Example

The NEMS-S data should come from a Qualtrics survey collected by a
trained surveyor. Download the data from Qualtrics in the `.sav` format
(for SPSS). Then you can read the data into R with the `read_nemss`
function.

``` r
library(nemsr)

## basic example code
file <- system.file("extdata", "example_nems.sav", package = "nemsr")
(nemss <- read_nemss(file))
#> Warning in system2("/usr/bin/otool", c("-L", shQuote(DSO)), stdout = TRUE):
#> running command ''/usr/bin/otool' -L '/Library/Frameworks/R.framework/Resources/
#> library/tcltk/libs//tcltk.so'' had status 1
#> # A tibble: 10 × 83
#>    STORE_ID store…¹ pharm…² ethnic merch regis…³ self_…⁴ lowfa…⁵ dairy…⁶ lowfa…⁷
#>    <chr>    <chr>   <chr>   <chr>  <chr> <chr>   <chr>   <chr>   <chr>   <chr>  
#>  1 SL-014   Dollar… FALSE   FALSE  FALSE 4       0       <NA>    FALSE   ""     
#>  2 SL-013   Grocer… FALSE   TRUE   FALSE 3       0       FALSE   TRUE    ""     
#>  3 SL-015   Grocer… TRUE    FALSE  FALSE 9       5       TRUE    TRUE    "0"    
#>  4 SL-010   Dollar… FALSE   FALSE  FALSE 2       0       FALSE   TRUE    "0"    
#>  5 SL-016   Grocer… FALSE   TRUE   FALSE 3       0       <NA>    FALSE   ""     
#>  6 SL-017   Dollar… FALSE   FALSE  FALSE 2       0       FALSE   TRUE    ""     
#>  7 SL-019   Grocer… FALSE   TRUE   FALSE 5       4       TRUE    TRUE    ""     
#>  8 SL-018   Grocer… FALSE   FALSE  FALSE 12      7       TRUE    TRUE    "0"    
#>  9 SL-012   Dollar… FALSE   FALSE  FALSE 2       0       FALSE   TRUE    "2"    
#> 10 SL-011   Grocer… FALSE   TRUE   FALSE 2       0       FALSE   TRUE    "0"    
#> # … with 73 more variables: lowfat_quart <chr>, lowfat_half_gal <chr>,
#> #   lowfat_gal <chr>, whole_pint <chr>, whole_quart <chr>,
#> #   whole_half_gal <chr>, whole_gal <chr>, lowfat_quart_price <chr>,
#> #   lowfat_half_gal_price <chr>, lowfat_gal_price <chr>,
#> #   whole_quart_price <chr>, whole_half_gal_price <chr>, whole_gal_price <chr>,
#> #   lean_beef_varieties <chr>, lean_beef_price <chr>, regular_beef_price <chr>,
#> #   varieties_of_fruit <chr>, apple_price <chr>, banana_price <chr>, …
```

Once the data are in your R session, there are two basic things you
might like to do with them: a NEMS-S score and a market basket score.

### NEMS-S Score

The `calculate_nems_score()` function

``` r
calculate_nems_score(nemss)
```

### Market Basket

The `calculate_market_basket()` function computes the cost of a common
market basket at that store using the pricing data available weighted
according to the [Thrifty Food Plan,
2021](https://fns-prod.azureedge.us/sites/default/files/resource-files/TFP2021.pdf)

    #> # A tibble: 10 × 17
    #>    ID     grain_repl…¹ grain dairy…² dairy veget…³ veget…⁴ fruit…⁵ fruit soda_…⁶
    #>    <chr>         <dbl> <dbl>   <dbl> <dbl>   <dbl>   <dbl>   <dbl> <dbl>   <dbl>
    #>  1 SL-014            2  35.7       2  8.75       7    40.8       4  29.6       0
    #>  2 SL-013            1  40.8       0 20.1        7    40.8       4  29.6       0
    #>  3 SL-015            0  30.0       0 16.5        1    44.2       0  32.0       0
    #>  4 SL-010            1  40.8       0 21.5        7    40.8       4  29.6       0
    #>  5 SL-016            2  35.7       2  8.75       4    34.3       1  24.6       0
    #>  6 SL-017            1  35.8       0 21.5        7    40.8       4  29.6       0
    #>  7 SL-019            1  45.1       0 16.5        1    28.2       1  23.9       0
    #>  8 SL-018            0  23.1       0 16.4        1    51.6       0  20.7       0
    #>  9 SL-012            2  35.7       0 21.5        7    40.8       4  29.6       0
    #> 10 SL-011            1  38.2       0 17.4        2    27.1       1  20.6       0
    #> # … with 7 more variables: soda <dbl>, beef_replacements <dbl>, beef <dbl>,
    #> #   frozen_dinner_replacements <dbl>, frozen_dinner <dbl>,
    #> #   total_replacements <dbl>, total <dbl>, and abbreviated variable names
    #> #   ¹​grain_replacements, ²​dairy_replacements, ³​vegetable_replacements,
    #> #   ⁴​vegetable, ⁵​fruit_replacements, ⁶​soda_replacements

For details on this function, its methods, and return information, see
`?calculate_market_basket()`
