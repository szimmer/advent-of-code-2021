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
library(index0)
```

``` r
read_dig <- function(fn){
  df1 <- read_delim(
    here("Input", fn),
    delim="|",
    col_names = c("X", "Y"),
    col_types = cols(.default=col_character()),
    trim_ws = TRUE)
  
  dfx <- df1 %>%
    select(Value=X) %>%
    mutate(Row=row_number()) %>%
    separate_rows(Value) %>%
    group_by(Row) %>%
    mutate(Digit=row_number()) %>%
    ungroup() %>%
    select(Row, Digit, Value) %>%
    mutate(Type="Dig10")
  
  dfy <- df1 %>%
    select(Value=Y) %>%
    mutate(Row=row_number()) %>%
    separate_rows(Value) %>%
    group_by(Row) %>%
    mutate(Digit=row_number()) %>%
    ungroup() %>%
    select(Row, Digit, Value) %>%
    mutate(Type="Output")
  
  bind_rows(dfx, dfy)
}

test_dig <-  read_dig("test_08.txt")
input_dig <- read_dig("input_08.txt")
```

# Part 1

``` r
digs <- as.index0(c("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"))
str_length(digs)
```

    ##  [1] 6 2 5 5 4 5 6 3 7 6

``` r
count_1478 <- function(digdat){
  dig_1478 <- digs[c(1,4,7,8)]
  n_1478 <- str_length(dig_1478)
  digdat %>%
    filter(Type=="Output") %>%
    mutate(
      ndig=str_length(Value),
      val_1478=ndig %in% n_1478
    ) %>%
    summarise(ans=sum(val_1478))
}
count_1478(test_dig)
```

    ## # A tibble: 1 x 1
    ##     ans
    ##   <int>
    ## 1    26

``` r
count_1478(input_dig)
```

    ## # A tibble: 1 x 1
    ##     ans
    ##   <int>
    ## 1   255
