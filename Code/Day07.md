Day 7
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.3     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(here)
```

    ## here() starts at C:/Users/steph/Documents/GitHub/advent-of-code-2021

``` r
read_pop <- function(fn){
  read_file(here("Input", fn)) %>%
    str_split(",", simplify = TRUE) %>%
    as.vector() %>%
    as.integer()
}

test_pos <-  read_pop("test_07.txt")
input_pos <- read_pop("input_07.txt")
```

# Part 1

``` r
best_pos <- function(x){
  f <- function(a, x){
    sum(abs(a-x))
  }
  optimize(f, range(x), x=x)  
}
best_pos(test_pos)
```

    ## $minimum
    ## [1] 2.000017
    ## 
    ## $objective
    ## [1] 37.00003

``` r
best_pos(input_pos)
```

    ## $minimum
    ## [1] 346
    ## 
    ## $objective
    ## [1] 359648

# Part 2

``` r
best_pos2 <- function(x){
  fcost <- function(a, x){
    diff <- abs(a-x)
    cost <- diff*(diff+1)/2
    sum(cost)
  }
  
  best_nonint <- optimize(fcost, range(x), x=x)  
  print(best_nonint)
  min(fcost(floor(best_nonint$minimum), x),
      fcost(ceiling(best_nonint$minimum), x))
}

best_pos2(test_pos)
```

    ## $minimum
    ## [1] 4.7
    ## 
    ## $objective
    ## [1] 167.55

    ## [1] 168

``` r
best_pos2(input_pos)
```

    ## $minimum
    ## [1] 497.404
    ## 
    ## $objective
    ## [1] 100727842

    ## [1] 100727924
