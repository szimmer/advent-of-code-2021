Day 1
================
Stephanie Zimmer
12/1/2021

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
input1 <- read_table(here("Input", "input_01.txt"), col_names = "value")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   value = col_double()
    ## )

``` r
input1
```

    ## # A tibble: 2,000 x 1
    ##    value
    ##    <dbl>
    ##  1   157
    ##  2   148
    ##  3   149
    ##  4   146
    ##  5   144
    ##  6   145
    ##  7   162
    ##  8   163
    ##  9   164
    ## 10   166
    ## # ... with 1,990 more rows

``` r
input1 %>%
  mutate(Inc=lag(value)<value) %>%
  summarise(
    Answer1=sum(Inc, na.rm=TRUE)
  )
```

    ## # A tibble: 1 x 1
    ##   Answer1
    ##     <int>
    ## 1    1548

``` r
library(RcppRoll)

tibble(
  value3 = roll_sum(input1$value, 3)  
) %>%
  mutate(Inc=lag(value3)<value3) %>%
  summarise(
    Answer2=sum(Inc, na.rm=TRUE)
  )
```

    ## # A tibble: 1 x 1
    ##   Answer2
    ##     <int>
    ## 1    1589
