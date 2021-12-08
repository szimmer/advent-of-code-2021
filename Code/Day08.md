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

# Day 2

``` r
diglen <- as.index0(str_length(digs))

decode_output_i <- function(digdat, i){
  possdigits <- digdat %>%
    filter(Row==i, Type=="Dig10") %>%
    pull(Value)
  possdigits_list <- map(possdigits, str_split, "", simplify=TRUE) %>%
    map(as.vector)
  
  ndig <- str_length(possdigits)
  ValueNumber=case_when(
    ndig==diglen[1]~1,
    ndig==diglen[4]~4,
    ndig==diglen[7]~7,
    ndig==diglen[8]~8,
    TRUE ~ NA_real_
  )
  a <- setdiff(possdigits_list[[which(ValueNumber==7)]],
               possdigits_list[[which(ValueNumber==1)]])
  bd <- setdiff(possdigits_list[[which(ValueNumber==4)]],
                possdigits_list[[which(ValueNumber==1)]])
  cf <- setdiff(possdigits_list[[which(ValueNumber==4)]],
                bd)
  
  
  # 2 has c only, 5 and 6 have f only
  check_cfonly <- function(x){
    both <- (cf[1] %in% x) & (cf[2] %in% x)
    one <- (cf[1] %in% x) | (cf[2] %in% x)
    one & !both
  }
  
  cfonly <- map_lgl(possdigits_list, check_cfonly)
  
  posslist_256 <- possdigits_list[cfonly]
  poss_256 <- possdigits[cfonly]
  check_256_1 <- map_lgl(posslist_256, ~cf[1] %in% .x)
  
  if (sum(check_256_1)==1){
    val_56 <- poss_256[!check_256_1]
    val_2 <- poss_256[check_256_1]
  } else{
    val_56 <- poss_256[check_256_1]
    val_2 <- poss_256[!check_256_1]
  }
  
  val_5 <- val_56[str_length(val_56)==diglen[5]]
  val_6 <- val_56[str_length(val_56)==diglen[6]]
  
  ValueNumber[which(possdigits==val_2)] <- 2
  ValueNumber[which(possdigits==val_5)] <- 5
  ValueNumber[which(possdigits==val_6)] <- 6
  
  e <- setdiff(possdigits_list[[which(ValueNumber==6)]],
               possdigits_list[[which(ValueNumber==5)]])
  
  if (cf[1] %in% possdigits_list[[which(ValueNumber==2)]]){
    c <- cf[1]
    f <- cf[2]
  } else{
    c <- cf[2]
    f <- cf[1]
  }
  
  # now know a, bd, c, f, e
  # still don't know 0, 3, 9
  
  ValueNumber[which(is.na(ValueNumber) & ndig==5)] <- 3
  
  cont9 <- which(is.na(ValueNumber))
  
  diff9_1 <- setdiff(possdigits_list[[cont9[1]]],
                     possdigits_list[[which(ValueNumber==3)]])
  
  if (length(diff9_1)==1){
    ValueNumber[cont9[1]] <- 9
    ValueNumber[cont9[2]] <- 0
  } else{
    ValueNumber[cont9[2]] <- 9
    ValueNumber[cont9[1]] <- 0
    
  }
  
  alphabetize_strings <- function(x){
    x %>%
      map(str_split, "", simplify=TRUE) %>%
      map(as.vector) %>%
      map(sort) %>%
      map_chr(str_c, collapse="")
  }
  
  possdigits_ord <- alphabetize_strings(possdigits)
  output_ord <- digdat %>%
    filter(Row==i, Type!="Dig10") %>%
    pull(Value) %>%
    alphabetize_strings()
  
  ValueNumber[match(output_ord, possdigits_ord)] %>%
    str_c(collapse="") %>%
    as.numeric()
  
}

decode_output <- function(digdat){
  Rows <- digdat %>% distinct(Row) %>% pull()
  vals <- Rows %>% map_dbl(decode_output_i, digdat=digdat)
  sum(vals)
}

decode_output(test_dig)
```

    ## [1] 61229

``` r
decode_output(input_dig)
```

    ## [1] 982158
