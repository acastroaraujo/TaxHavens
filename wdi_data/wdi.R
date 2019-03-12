# This code gets us a WDI dataset using the `WDI` package. There are two main
# outputs here:
#
# - `wdi_data`, which can take a while to load, so I suggest saving it through
# the `saveRDS(wdi_data, "wdi_data.rds")` function.
#
# - `wdi_varlist` provides the whole set of variables which I thought were most
# relevant. This one is important because otherwise there's no easy way of
# knowing what, for example, `BN.KLT.DINV.DRS.GDP.ZS` means.

library(tidyverse)
library(WDI)
library(countrycode)

## First step: find relevant variable codes and store them as a string.
#WDIsearch("...TAX", field = "indicator") %>% View() 
code_regex <- c("DINV", "FRM.COR", "^NY.GDP", "^RL", "^GV", "...TAX")
wdi_varlist <- map(code_regex, WDIsearch, field = "indicator")
indicator_names <- map(wdi_varlist, ~.[ , 1]) %>% unlist() ## many indicators

## Second step: load data (this might take a while). Also, only 87 indicators survive the process.
wdi_data <- WDI(country = "all", indicator = indicator_names,
start = 2000, end = 2017, extra = FALSE) %>% as_data_frame()

## Third step: get country and continent names
wdi_data$country <- countrycode(wdi_data$iso2c, origin = "iso2c", 
destination = "country.name")
wdi_data$continent <- countrycode(wdi_data$country, origin = "country.name", 
destination = "continent")

## Fourth step: Clean up and organize
wdi_data <- wdi_data %>% 
filter(!is.na(country)) %>% 
select(country, continent, year, everything())

## Fith step: make wdi_varlist into useful format
wdi_varlist <- data_frame(regex = code_regex, info = wdi_varlist) %>%  
mutate(info = map(info, as_data_frame)) %>% 
unnest()

## Fith step: save data
readr::write_rds(wdi_data, "wdi_data.RDS")
readr::write_rds(wdi_varlist, "wdi_varlist.RDS")
