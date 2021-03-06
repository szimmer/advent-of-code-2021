Day 6
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

test_initpop <-  read_pop("test_06.txt")
input_initpop <- read_pop("input_06.txt")
```

# Part 1

``` r
gen_pop_next <- function(x){
  n_new <- x %>% filter(timer==0) %>% pull(n)
  
  newx <- x %>% mutate(timer=if_else(timer>0, timer-1, 6)) %>%
    add_row(timer=8, n=n_new)
  
  newx %>% count(timer, wt=n)
}

gen_pop <- function(initpop, days){
  x <- table(initpop) %>% as_tibble() %>% 
    rename(timer=initpop) %>%
    mutate(timer=as.integer(timer))
  
  for (i in 1:days){
    x <- gen_pop_next(x)
  }
  npop <- x %>% pull(n) %>% sum()
  return(list(pop=x, npop=npop))
}

test18 <- gen_pop(test_initpop, 18)
test80 <- gen_pop(test_initpop, 80)
test18$npop
```

    ## [1] 26

``` r
test80$npop
```

    ## [1] 5934

``` r
inp80 <- gen_pop(input_initpop, 80)
inp80$npop
```

    ## [1] 365131

# Part 2

``` r
test256 <- gen_pop(test_initpop, 256)
test256$npop
```

    ## [1] 26984457539

``` r
inp256 <- gen_pop(input_initpop, 256)
formatC(inp256$npop, digits=13)
```

    ## [1] " 1650309278600"
