Day 10
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
read_nav <- function(fn){
  read_table(here("Input", fn), 
             col_names = "X1", 
             col_types = cols(.default = col_character())) %>%
    pull(X1)
}
nav_test <- read_nav("test_10.txt")
nav_input <- read_nav("input_10.txt")
```

# Part 1

``` r
calc_corrupt_pts <- function(x){
  done <- FALSE
  xprev <- x
  while (!done){
    xnext <- str_remove_all(xprev, "\\(\\)|\\{\\}|\\[\\]|\\<\\>")
    if (xnext==xprev){
      done <- TRUE
    } else{
      xprev <- xnext
    }
  }
  
  xclosed <- str_remove_all(xnext, "\\(|\\{|\\[|\\<")
  if (str_length(xclosed)>0){
    if (str_sub(xclosed, 1, 1)==")"){
      points <- 3
    } else if (str_sub(xclosed, 1, 1)=="]"){
      points <- 57
    } else if (str_sub(xclosed, 1, 1)=="}"){
      points <- 1197
    } else if (str_sub(xclosed, 1, 1)==">"){
      points <- 25137
    }
  } else{
    points <- 0
  }
  return(points)  
}

nav_test %>% map_dbl(calc_corrupt_pts) %>% sum()
```

    ## [1] 26397

``` r
nav_input %>% map_dbl(calc_corrupt_pts) %>% sum()
```

    ## [1] 216297

# Part 2

``` r
calc_incomplete_pts <- function(x){
  done <- FALSE
  xprev <- x
  while (!done){
    xnext <- str_remove_all(xprev, "\\(\\)|\\{\\}|\\[\\]|\\<\\>")
    if (xnext==xprev){
      done <- TRUE
    } else{
      xprev <- xnext
    }
  }
  
  xclosed <- str_remove_all(xnext, "\\(|\\{|\\[|\\<")
  if (str_length(xclosed)>0) return(NA)
  
  xcomp <- stringi::stri_reverse(xnext)
  xcvals <- xcomp %>%
    str_replace_all("\\(", "1") %>%
    str_replace_all("\\[", "2") %>%
    str_replace_all("\\{", "3") %>%
    str_replace_all("\\<", "4") 
  xvals <- str_split(xcvals, "", simplify = TRUE) %>% as.numeric()
  
  score <- 0
  for (i in 1:length(xvals)){
    score <- 5*score+xvals[i]
  }
  return(score)
}

nav_test %>% map_dbl(calc_incomplete_pts) %>% median(na.rm=TRUE)
```

    ## [1] 288957

``` r
nav_input %>% map_dbl(calc_incomplete_pts) %>% median(na.rm=TRUE)
```

    ## [1] 2165057169
