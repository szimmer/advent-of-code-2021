Day 2
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
input2 <- read_table(here("Input", "input_02.txt"), 
                     col_names = c("direction", "value"))
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   direction = col_character(),
    ##   value = col_double()
    ## )

``` r
input2
```

    ## # A tibble: 1,000 x 2
    ##    direction value
    ##    <chr>     <dbl>
    ##  1 forward       4
    ##  2 down          8
    ##  3 down          3
    ##  4 down          1
    ##  5 forward       8
    ##  6 up            6
    ##  7 down          4
    ##  8 forward       2
    ##  9 down          4
    ## 10 down          6
    ## # ... with 990 more rows

# Part 1

``` r
input2 %>%
  mutate(
    ymove=case_when(
      direction=="down"~value,
      direction=="up"~-value,
      TRUE ~ 0
    ),
    xmove=case_when(
      direction=="forward"~value,
      TRUE ~ 0
    )
  ) %>%
  summarise(
    depth=sum(ymove),
    horiz=sum(xmove)
  ) %>%
  mutate(
    res=depth*horiz
  )
```

    ## # A tibble: 1 x 3
    ##   depth horiz     res
    ##   <dbl> <dbl>   <dbl>
    ## 1   908  1815 1648020

# Part 2

``` r
input2 %>%
  mutate(
    aimmove=case_when(
      direction=="down"~value,
      direction=="up"~-value,
      TRUE ~ 0
    ),
    aim=cumsum(aimmove),
    xmove=case_when(
      direction=="forward"~value,
      TRUE ~ 0
    ),
    ymove=xmove*aim
  ) %>%
  summarise(
    depth=sum(ymove),
    horiz=sum(xmove)
  ) %>%
  mutate(
    res=depth*horiz
  )
```

    ## # A tibble: 1 x 3
    ##    depth horiz        res
    ##    <dbl> <dbl>      <dbl>
    ## 1 969597  1815 1759818555
