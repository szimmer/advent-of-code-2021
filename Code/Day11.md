Day 11
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
read_oct <- function(fn){
  tabinit <- read_table(here("Input", fn), 
                        col_names = FALSE, 
                        col_types = cols(.default = col_character()))
  nc <- tabinit %>% slice(1) %>% pull(X1) %>% str_length()
  m <- tabinit %>%
    separate(X1, into=str_c("Y", 1:nc), sep=1:(nc-1), convert = TRUE) %>%
    as.matrix()
  colnames(m) <- NULL
  return(m)
}
oct_test <- read_oct("test_11.txt")
oct_input <- read_oct("input_11.txt")
```

# Part 1

``` r
flash_dance <- function(oct, steps){
  # Initialize
  flashcount <- 0
  nr <- nrow(oct)
  nc <- ncol(oct)
  
  for (step in 1:steps){
    # Increase by 1
    oct <- oct+1
    flashprev <- flash <- matrix(0, nrow=nrow(oct), ncol=ncol(oct))
    
    # Find flashes
    flash[oct>9] <- 1
    
    while (any(flash != flashprev)){
      find <- which(flash!=flashprev, arr.ind = TRUE)
      flashprev <- flash
      for (i in 1:nrow(find)){
        nb_rows <- max(find[i, 1]-1, 1):min(find[i, 1]+1, nr)
        nb_cols <- max(find[i, 2]-1, 1):min(find[i, 2]+1, nc)
        oct[nb_rows, nb_cols] <- oct[nb_rows, nb_cols] + 1
      }
      flash[oct>9] <- 1
    }
    
    oct[oct>9] <- 0
    flashcount <- flashcount + sum(flash)
  }
  return(list(flashcount=flashcount, field=oct))
}

flash_dance(oct_test, 100)
```

    ## $flashcount
    ## [1] 1656
    ## 
    ## $field
    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    ##  [1,]    0    3    9    7    6    6    6    8    6     6
    ##  [2,]    0    7    4    9    7    6    6    9    1     8
    ##  [3,]    0    0    5    3    9    7    6    9    3     3
    ##  [4,]    0    0    0    4    2    9    7    8    2     2
    ##  [5,]    0    0    0    4    2    2    9    8    9     2
    ##  [6,]    0    0    5    3    2    2    2    8    7     7
    ##  [7,]    0    5    3    2    2    2    2    9    6     6
    ##  [8,]    9    3    2    2    2    2    8    9    6     6
    ##  [9,]    7    9    2    2    2    8    6    8    6     6
    ## [10,]    6    7    8    9    9    9    8    7    6     6

``` r
flash_dance(oct_input, 100)
```

    ## $flashcount
    ## [1] 1732
    ## 
    ## $field
    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    ##  [1,]    5    7    1    1    1    1    2    0    0     9
    ##  [2,]    7    1    1    1    1    1    2    3    3     2
    ##  [3,]    5    1    1    1    1    1    1    1    1     1
    ##  [4,]    5    1    1    1    1    1    1    1    1     1
    ##  [5,]    5    1    1    1    1    1    1    1    1     5
    ##  [6,]    5    1    1    1    1    1    1    1    5     3
    ##  [7,]    4    1    1    1    1    1    6    4    3     2
    ##  [8,]    3    6    3    3    2    5    3    2    2     2
    ##  [9,]    3    6    0    0    7    5    4    4    3     8
    ## [10,]    3    0    0    0    0    4    0    0    8     6

# Part 2

``` r
flash_dance_all <- function(oct){
  # Initialize
  step <- flashcount <- 0
  nr <- nrow(oct)
  nc <- ncol(oct)
  
  allflash <- FALSE
  
  while (!allflash){
    # Increase by 1
    oct <- oct+1
    flashprev <- flash <- matrix(0, nrow=nrow(oct), ncol=ncol(oct))
    
    # Find flashes
    flash[oct>9] <- 1
    
    while (any(flash != flashprev)){
      find <- which(flash!=flashprev, arr.ind = TRUE)
      flashprev <- flash
      for (i in 1:nrow(find)){
        nb_rows <- max(find[i, 1]-1, 1):min(find[i, 1]+1, nr)
        nb_cols <- max(find[i, 2]-1, 1):min(find[i, 2]+1, nc)
        oct[nb_rows, nb_cols] <- oct[nb_rows, nb_cols] + 1
      }
      flash[oct>9] <- 1
    }
    
    oct[oct>9] <- 0
    step <- step + 1
    if (all(flash==1)) return(step)
  }
  
}

flash_dance_all(oct_test)
```

    ## [1] 195

``` r
flash_dance_all(oct_input)
```

    ## [1] 290
