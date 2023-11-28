library("tidyverse")
library("testthat")

#to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2.
input <- readLines("input/day01.txt") %>% as.numeric()

fuel_finder <- function(x){
  floor(x/3)-2
}

expect_equal(fuel_finder(12), 2)
expect_equal(fuel_finder(14), 2)
expect_equal(fuel_finder(1969), 654)
expect_equal(fuel_finder(100756), 33583)

map_dbl(input, fuel_finder) %>% sum()

fuel_reducer <- function(x){
  fuel_tracker <- c()
  fuel <- fuel_finder(x)
  
  while(fuel > 0){
    fuel_tracker <- c(fuel_tracker, fuel)
    fuel <- fuel_finder(fuel)
  }
  
  sum(fuel_tracker)
}


expect_equal(fuel_reducer(14), 2)
expect_equal(fuel_reducer(1969), 966)
expect_equal(fuel_reducer(100756), 50346)

map_dbl(input, fuel_reducer) %>% sum()
