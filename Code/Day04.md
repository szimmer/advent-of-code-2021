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
# input <- read_table(here("Input", "input_03.txt"), 
#                     col_names = c("value"))
# input
# test <- read_table(here("Input", "test_03.txt"), 
#                    col_names = c("value"))
# test



read_bingo <- function(fn){
  calls <- scan(file=here("Input", fn), nlines=1, sep=",")
  nl_file <- R.utils::countLines(here("Input", fn)) %>% as.numeric()
  ncards <- (nl_file-1) / 6
  read_card <- function(cardnum){
    startrow <- 3+(cardnum-1)*6
    scan(here("Input", fn), nlines=5, skip=startrow-1, quiet=TRUE) %>%
      matrix(nrow=5, byrow=TRUE)
  }
  cards <- map(1:ncards, read_card)
  return(list(calls=calls, cards=cards))
}

test_cc <- read_bingo("test_04.txt")
input_cc <- read_bingo("input_04.txt")
```

# Day 1

``` r
play_bingo <- function(cc){
  calls <- cc$calls
  cards <- cc$cards
  punches <- vector("list", length(cards))
  for (i in 1:length(punches)){
    punches[[i]] <- matrix(FALSE, 5, 5)
  }
  bingo_check <- rep(FALSE, length(cards))
  
  for (i in 1:length(calls)){
    for (player in 1:length(cards)){
      punch <- which(cards[[player]]==calls[[i]])
      if (length(punch)>0){
        punches[[player]][punch] <- TRUE
      }
      if( any(c(apply(punches[[player]], 1, sum),
                apply(punches[[player]], 2, sum))==5)){
        bingo_check[player] <- TRUE
      }
    }
    if (any(bingo_check)){
      break
    }
  }
  winner <- which(bingo_check)
  lastcall <- calls[i]
  winnercard <- cards[[winner]]
  winnerpunch <- punches[[winner]]
  unmarkedsum <- sum(winnercard[!winnerpunch])
  return(list(card=winnercard, punches=winnerpunch,
              lastcall=lastcall, unmarkedsum=unmarkedsum,
              ans=lastcall*unmarkedsum))
}

play_bingo(test_cc)
```

    ## $card
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   14   21   17   24    4
    ## [2,]   10   16   15    9   19
    ## [3,]   18    8   23   26   20
    ## [4,]   22   11   13    6    5
    ## [5,]    2    0   12    3    7
    ## 
    ## $punches
    ##       [,1]  [,2]  [,3]  [,4]  [,5]
    ## [1,]  TRUE  TRUE  TRUE  TRUE  TRUE
    ## [2,] FALSE FALSE FALSE  TRUE FALSE
    ## [3,] FALSE FALSE  TRUE FALSE FALSE
    ## [4,] FALSE  TRUE FALSE FALSE  TRUE
    ## [5,]  TRUE  TRUE FALSE FALSE  TRUE
    ## 
    ## $lastcall
    ## [1] 24
    ## 
    ## $unmarkedsum
    ## [1] 188
    ## 
    ## $ans
    ## [1] 4512

``` r
play_bingo(input_cc)
```

    ## $card
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   60   31   29   49   72
    ## [2,]   89   41    5   79   22
    ## [3,]   58   28   90   76   95
    ## [4,]   93   45   14   47   37
    ## [5,]   65   25    7   59   62
    ## 
    ## $punches
    ##       [,1] [,2]  [,3]  [,4]  [,5]
    ## [1,] FALSE TRUE FALSE FALSE FALSE
    ## [2,] FALSE TRUE FALSE FALSE FALSE
    ## [3,] FALSE TRUE FALSE FALSE FALSE
    ## [4,] FALSE TRUE FALSE FALSE FALSE
    ## [5,] FALSE TRUE FALSE FALSE FALSE
    ## 
    ## $lastcall
    ## [1] 45
    ## 
    ## $unmarkedsum
    ## [1] 1108
    ## 
    ## $ans
    ## [1] 49860

# Day 2

``` r
play_bingo_last <- function(cc){
  calls <- cc$calls
  cards <- cc$cards
  punches <- vector("list", length(cards))
  for (i in 1:length(punches)){
    punches[[i]] <- matrix(FALSE, 5, 5)
  }
  bingo_check <- rep(FALSE, length(cards))
  lastcard <- NULL
  for (i in 1:length(calls)){
    for (player in 1:length(cards)){
      punch <- which(cards[[player]]==calls[[i]])
      if (length(punch)>0){
        punches[[player]][punch] <- TRUE
      }
      if( any(c(apply(punches[[player]], 1, sum),
                apply(punches[[player]], 2, sum))==5)){
        bingo_check[player] <- TRUE
      }
      
      if (all(bingo_check) & is.null(lastcard)){
        lastcard <- player
        break
      }
    }
    if (!is.null(lastcard)){
      break
    }
    
  }
  lastcall <- calls[i]
  losercard <- cards[[lastcard]]
  loserpunch <- punches[[lastcard]]
  unmarkedsum <- sum(losercard[!loserpunch])
  return(list(card=losercard, punches=loserpunch,
              lastcall=lastcall, unmarkedsum=unmarkedsum,
              ans=lastcall*unmarkedsum))
}

play_bingo_last(test_cc)
```

    ## $card
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    3   15    0    2   22
    ## [2,]    9   18   13   17    5
    ## [3,]   19    8    7   25   23
    ## [4,]   20   11   10   24    4
    ## [5,]   14   21   16   12    6
    ## 
    ## $punches
    ##       [,1]  [,2] [,3]  [,4]  [,5]
    ## [1,] FALSE FALSE TRUE  TRUE FALSE
    ## [2,]  TRUE FALSE TRUE  TRUE  TRUE
    ## [3,] FALSE FALSE TRUE FALSE  TRUE
    ## [4,] FALSE  TRUE TRUE  TRUE  TRUE
    ## [5,]  TRUE  TRUE TRUE FALSE FALSE
    ## 
    ## $lastcall
    ## [1] 13
    ## 
    ## $unmarkedsum
    ## [1] 148
    ## 
    ## $ans
    ## [1] 1924

``` r
play_bingo_last(input_cc)
```

    ## $card
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    5   89   55   46   96
    ## [2,]   67   22   95   82   56
    ## [3,]   61   94   84   99   28
    ## [4,]   71   70   16   57   63
    ## [5,]   98   92   86   73   83
    ## 
    ## $punches
    ##       [,1] [,2]  [,3]  [,4]  [,5]
    ## [1,]  TRUE TRUE  TRUE FALSE  TRUE
    ## [2,] FALSE TRUE  TRUE  TRUE  TRUE
    ## [3,]  TRUE TRUE  TRUE  TRUE  TRUE
    ## [4,]  TRUE TRUE  TRUE  TRUE FALSE
    ## [5,]  TRUE TRUE FALSE  TRUE  TRUE
    ## 
    ## $lastcall
    ## [1] 94
    ## 
    ## $unmarkedsum
    ## [1] 262
    ## 
    ## $ans
    ## [1] 24628
