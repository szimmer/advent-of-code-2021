Day 4
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
read_coords <- function(fn){
  dat_in <- read_csv(
    here("Input", fn), 
    col_names=c("x1", "mid", "y2"), 
    col_types = cols(
      x1 = col_integer(),
      mid = col_character(),
      y2 = col_integer()
    ))
  
  dat_in %>%
    separate(mid, into=c("y1", "x2"), convert = TRUE)
  
}

test_coord <- read_coords("test_05.txt")
input_coord <- read_coords("input_05.txt")
```

# Day 1

``` r
make_field <- function(coord){
  coord_hv <- coord %>%
    filter(x1==x2|y1==y2)
  make_point_set <- function(x1, y1, x2, y2){
    if (x1==x2){
      y <- y1:y2 %>% sort
      x <- rep(x1, length(y))
      data.frame(x=x, y=y)
    } else{
      x <- x1:x2 %>% sort
      y <- rep(y1, length(x))
      data.frame(x=x, y=y)
    }
  }
  
  coord_set <-
    coord_hv %>%
    rowwise() %>%
    mutate(ps=list(make_point_set(x1,y1,x2, y2))) %>%
    unnest(col=ps) %>%
    count(x, y)
  
  # p <- coord_set %>%
  #   ggplot(aes(x=x, y=y, label=n)) +
  #   geom_text() 
  # 
  # print(p)
  
  coord_set %>%
    filter(n>=2) %>%
    nrow()
}

make_field(test_coord)
```

    ## [1] 5

``` r
make_field(input_coord)
```

    ## [1] 5197
