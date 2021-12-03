Day 3
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
input <- read_table(here("Input", "input_03.txt"), 
                     col_names = c("value"))
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   value = col_character()
    ## )

``` r
input
```

    ## # A tibble: 1,000 x 1
    ##    value       
    ##    <chr>       
    ##  1 110001101000
    ##  2 111011011100
    ##  3 100001101100
    ##  4 111011011010
    ##  5 001001101100
    ##  6 100001111100
    ##  7 110110101101
    ##  8 110010110000
    ##  9 111101010011
    ## 10 101101010110
    ## # ... with 990 more rows

``` r
test <- read_table(here("Input", "test_03.txt"), 
                     col_names = c("value"))
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   value = col_character()
    ## )

``` r
test
```

    ## # A tibble: 12 x 1
    ##    value
    ##    <chr>
    ##  1 00100
    ##  2 11110
    ##  3 10110
    ##  4 10111
    ##  5 10101
    ##  6 01111
    ##  7 00111
    ##  8 11100
    ##  9 10000
    ## 10 11001
    ## 11 00010
    ## 12 01010

# Part 1

``` r
read_diag_report <- function(report){
  ndig <- report %>% summarise(ndig=max(str_length(value))) %>% pull(ndig)
  
  meandigits <- report %>%
    separate(value, into=str_c("X", 1:ndig), sep=1:(ndig-1), convert=TRUE) %>%
    summarise(
      across(starts_with("X"), mean)
    )
  
  gamma_value <- meandigits %>%
    mutate(
      across(starts_with("X"), round),
      across(starts_with("X"), as.character)
      ) %>%
    unite("gamma", starts_with("X"), sep="") %>%
    mutate(
      gamma=strtoi(gamma, base=2)
    ) %>%
    pull(gamma)
  
  epsilon_value <- meandigits %>%
    mutate(
      across(starts_with("X"), round),
      across(starts_with("X"), ~1-.x),
      across(starts_with("X"), as.character)
      ) %>%
    unite("epsilon", starts_with("X"), sep="") %>%
    mutate(
      epsilon=strtoi(epsilon, base=2)
    ) %>%
    pull(epsilon)
  
  tibble(gamma=gamma_value, epsilon=epsilon_value) %>%
    mutate(ans=gamma*epsilon)
} 

read_diag_report(test)
```

    ## # A tibble: 1 x 3
    ##   gamma epsilon   ans
    ##   <int>   <int> <int>
    ## 1    22       9   198

``` r
read_diag_report(input)
```

    ## # A tibble: 1 x 3
    ##   gamma epsilon     ans
    ##   <int>   <int>   <int>
    ## 1  1565    2530 3959450

# Part 2

``` r
filter_i <- function(inp, i, type=c("oxy", "co2")){
  extdig <- inp %>%
    mutate(inpi=str_sub(value, i, i))
  
  digtab <- extdig %>% count(inpi)
  
  if (nrow(digtab)==2){
    n0 <- digtab %>% filter(inpi=="0") %>% pull(n)
    n1 <- digtab %>% filter(inpi=="1") %>% pull(n)
    if (n0 > n1){
      comp <- "mostly0"
    } else if (n1 > n0){
      comp <- "mostly1"
    } else{
      comp <- "tie"
    }
  } else {
    if (digtab$inpi=="0"){
      comp <- "mostly0"
    } else{
      comp <- "mostly1"
    }
  }
  
  if (type=="oxy"){
    if (comp %in% c("mostly1", "tie")){
      sel <- "1"
    } else{
      sel <- "0"
    }
  } else if (type=="co2"){
    if (nrow(digtab)==1){
      sel <- digtab$inpi
    } else if (comp %in% c("mostly1", "tie")){
      sel <- "0"
    } else{
      sel <- "1"
    }
    
  }
  
  extdig %>%
    filter(inpi==sel) %>%
    select(-inpi)
}

rating <- function(report){
  ndig <- report %>% summarise(ndig=max(str_length(value))) %>% pull(ndig)
  
  tmpreport_oxy <- report
  tmpreport_co2 <- report
  
  for (i in 1:ndig){
    tmpreport_oxy <- filter_i(tmpreport_oxy, i, "oxy")
    tmpreport_co2 <- filter_i(tmpreport_co2, i, "co2")
  }
  
  tibble(oxy=strtoi(tmpreport_oxy$value, base=2),
         co2=strtoi(tmpreport_co2$value, base=2)) %>%
    mutate(ans=oxy*co2)
  
}

rating(test)
```

    ## # A tibble: 1 x 3
    ##     oxy   co2   ans
    ##   <int> <int> <int>
    ## 1    23    10   230

``` r
rating(input)
```

    ## # A tibble: 1 x 3
    ##     oxy   co2     ans
    ##   <int> <int>   <int>
    ## 1  2039  3649 7440311
