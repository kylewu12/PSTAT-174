library(fpp3)
library(feasts)
library(fable)
library(seasonal)
library(tidyverse)
library(tsibble)
library(MASS)
library(gridExtra)


cpi_newcar <- cpi_newcar %>%
  mutate(Month = yearmonth(DATE)) %>%
  as_tsibble(index = Month) %>%
  filter(DATE > "1992-12-01") %>%
  fiter(DATE < "2022-12-01")

c_newcar$DATE = as.Date(c_newcar$DATE)
c_newcar$CUUR0000SETA01 <- ts(c_newcar$CUUR0000SETA01, start = c(1960,01), frequency = 12)
plot(c_newcar$CUUR0000SETA01)

cpi_newcar_ts <- autoplot(cpi_newcar, CUSR0000SETA01) +
  labs(title = "CPI of New Cars", y = "Index: 1982-1984 = 100")

cpi_newcar_ts



### Prime Rate Loans

bank_rate <- bank_rate %>%
  mutate(Month = yearmonth(DATE)) %>%
  as_tsibble(index = Month) %>%
  filter(DATE > "1992-12-01") %>%
  fiter(DATE < "2022-12-01")

bank_rate_ts <- autoplot(bank_rate, MPRIME) +
  labs(title = "Bank Prime Loan Rate", y = "%")


### Total Vehicle Sales


total_vehicles <- total_vehicles %>%
  mutate(Month = yearmonth(DATE)) %>%
  as_tsibble(index = Month) %>%
  filter(DATE > "1992-12-01") %>%
  fiter(DATE < "2022-12-01")

total_vehicles_ts <- autoplot(total_vehicles, TOTALNSA) +
  labs(title = "Total Vehicle Sales", y = "Thousands of Units")




### Domestic Auto Production

auto_production <- auto_production %>%
  mutate(Month = yearmonth(DATE)) %>%
  as_tsibble(index = Month) %>%
  filter(DATE > "1992-12-01") %>%
  fiter(DATE < "2022-12-01")

auto_production_ts <- autoplot(auto_production, DAUPNSA) +
  labs(title = "Domestic Auto Production", y = "Thousands of Units")



### GAS CPI

fuel_prices <- fuel_prices %>%
  mutate(Month = yearmonth(DATE)) %>%
  as_tsibble(index = Month) %>%
  filter(DATE > "1992-12-01") %>%
  fiter(DATE < "2022-12-01")


fuel_prices_ts <- autoplot(fuel_prices, CUSR0000SETB01) +
  labs(title = "Gas CPI", y = "Index 1982-1984 = 100")

purrr::reduce(list(bank_rate, auto_production, total_vehicles, cpi_newcar, fuel_prices), dplyr::left_join, by = "DATE")


