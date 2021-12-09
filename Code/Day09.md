Day 8
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
read_flows <- function(fn){
  tabinit <- read_table(here("Input", fn), 
                        col_names = FALSE, 
                        col_types = cols(.default = col_character()))
  nc <- tabinit %>% slice(1) %>% pull(X1) %>% str_length()
  tabinit %>%
    separate(X1, into=str_c("Y", 1:nc), sep=1:(nc-1), convert = TRUE) %>%
    as.matrix()
}
flows_test <- read_flows("test_09.txt")
flows_input <- read_flows("input_09.txt")
```

# Part 1

``` r
calc_risk <- function(flows){
  nr <- nrow(flows)
  nc <- ncol(flows)
  checknbr <- function(r, c){
    n1 <- ifelse(between(c-1, 1, nc), flows[r, c-1], NA_real_)
    n2 <- ifelse(between(c+1, 1, nc), flows[r, c+1], NA_real_)
    n3 <- ifelse(between(r-1, 1, nr), flows[r-1, c], NA_real_)
    n4 <- ifelse(between(r+1, 1, nr), flows[r+1, c], NA_real_)
    nbrs <- c(n1, n2, n3, n4)
    if (all(flows[r,c]<nbrs, na.rm=TRUE)){
      return(flows[r, c])
    } else{
      return(NA)
    }
  }
  crossing(r=1:nr, c=1:nc) %>%
    rowwise() %>%
    mutate(
      lowpoint=checknbr(r, c),
      Risk=lowpoint+1
    ) %>%
    ungroup() %>%
    summarise(
      ans=sum(Risk, na.rm=TRUE)
    )
  
}

calc_risk(flows_test)
```

    ## # A tibble: 1 x 1
    ##     ans
    ##   <dbl>
    ## 1    15

``` r
calc_risk(flows_input)
```

    ## # A tibble: 1 x 1
    ##     ans
    ##   <dbl>
    ## 1   572
