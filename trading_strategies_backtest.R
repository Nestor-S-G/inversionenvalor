library(tidyverse)
library(tidyquant)
library(timetk)
library(tibbletime)
library(scales)
library(highcharter)
library(broom)
library(PerformanceAnalytics)

### Importa data

symbols <- c("^GSPC", "^IRX")

prices <-
  tq_get(symbols,
         get = "stock.prices",
         from = "1990-01-01")
head(prices)

prices %>%
  filter(symbol == "^GSPC") %>%
  hchart(.,
         hcaes(x = date, y = adjusted),
         type = "line") %>%
  hc_title(text = "GSPC prices")
  
## Calculate returns and quick stats
  
returns <- prices %>%
  select(symbol, date, adjusted) %>%
  spread(symbol, adjusted) %>%
  rename(sp500 = "^GSPC", treas = "^IRX") %>%
  
  #the mutate function adds a column - very important for adding layers to our data
  mutate(sp500_returns = log(sp500) - log(lag(sp500)),
        daily_treas = (1 + (treas/100)) ^(1/252) - 1)
  
head(returns)

prices %>%
  tq_performance(Ra = sp500_returns,
                 performance_fun = table.Stats) %>%
    
## Moving averages: our strategy
  
roll_mean_50 <-
  rollify(mean, window = 50)

roll_mean_200 <-
  rollify(mean, window = 200)

prices %>%
  select(symbol, date, adjusted)
  spread(symbol, adjusted)
  rename(sp500 = "^GSPC", treas = "^IRX" )
  
  mutate(sma_200 = roll_mean_200(sp500),
         sma_50 = roll_mean_50(sp500))
  na.omit()
  tail(5)
  
## Let's get systematic
  
prices %>%
  select(symbol, date, adjusted) %>%
  spread(symbol, adjusted) %>%
  rename(sp500 = "^GSPC", treas = "^IRX") %>%
  mutate(sp500_returns = log(sp500) - log(lag(sp500)),
         daily_treas = (1 + (treas/100)) ^(1/252) - 1,
         sma_200 = roll_mean_200(sp500),
         sma_50 = roll_mean_50(sp500))
  na.omit()
  #Add our logic or signal
  mutate(signal = ifelse(sma_50 > sma_200, 1, 0),
         trend_returns = ifelse(lag(signal)) == 1, (signal * sp500_returns), daily_treas) %>%
  select(-treas, -sp500)
    
prices %>%
  select(symbol, date, adjusted) %>%
  spread(symbol, adjusted) %>%
  rename(sp500 = "^GSPC", treas = "^IRX") %>%
  mutate(
          sp500_returns = log(sp500) - log(lag(sp500)),
          daily_treas = (1 + (treas/100)) ^(1/252) - 1,
          sma_200 = roll_mean_200(sp500),
          sma_50 = roll_mean_50(sp500))
na.omit()
mutate(signal = ifelse(sma_50 > sma_200, 1, 0),
       buy_hope_returns = (.9*sp500_returns) + (.1*daily_treas),
       trend_returns = ifelse(lag(signal)) == 1, (signal * sp500_returns), daily_treas) %>%
select(date, trend_returns, buy_hope_returns)
na.omit()

sma_trend_results <-
prices %>%
  select(symbol, date, adjusted) %>%
  spread(symbol, adjusted) %>%
  rename(sp500 = "^GSPC", treas = "^IRX") %>%
  mutate(sma_200 = roll_mean_200(sp500),
         sma_50 = roll_mean_50(sp500),
         signal = ifelse(sma_50 > sma_200, 1, 0),
         sp500_returns = log(sp500) - log(lag(sp500)),
         daily_treas = (1 + (treas/100)) ^(1/252) - 1,
         buy_hope_returns = (.9*sp500_returns) + (.1*daily_treas),
         trend_returns = ifelse(lag(signal)) == 1, (signal * sp500_returns), daily_treas) %>%
  na.omit()
  mutate(trend_growth = accumulate (1 + trend_returns, '*'),
         buy_hope_growth = accumulate(1 + buy_hope_returns, '*'))
         
sma_trend_results %>% tail()

#Visualize returns

sma_trend_results %>%
  select(date, trend_growth, buy_hope_growth)
  gather(strategy, growth, -date)
  hchart(., hcaes(x = date, y = growth, group = strategy),
         type = "line") %>%

#table stats of our strategies

sma_trend_results %>%
  select(date, trend_returns, buy_hope_returns)
  gather(strategy, returns, -date)
  group_by(strategy)
  tq_performance(Ra = returns,
                 performance_fun = table.Stats())
  t()
  knitr::kable()
         
  
## Part 2: Adding another layer of logic
  
trend_z_results <-
prices %>%  
  select(symbol, date, adjusted) %>%
  spread(symbol, adjusted) %>%
  rename(sp500 = "^GSPC", treas = "^IRX") %>%
  mutate(sma_200 = roll_mean_200(sp500),
         sma_50 = roll_mean_50(sp500),
         signal = ifelse(sma_50 > sma_200, 1, 0),
         sp500_returns = log(sp500) - log(lag(sp500)),
         daily_treas = (1 + (treas/100)) ^(1/252) - 1)
  na.omit()
  mutate(trend_signal = ifelse(sma_50 > sma_200, 1, 0),
         z_spread = (sp500 - sma_200),
         z_score = (z_spread - mean(z_spread))/sd(z_spread),
         z_signal = ifelse(
                            lag(z_score, 1) < -.05 &
                            lag(z_score, 2) < -.05 &
                            lag(z_score, 3) < -.05,
                            0, 1),
         trend_z_returns = ifelse(lag(trend_signal) == 1 &
                                    z_signal == 1,
                                    (trend_signal * sp500_returns), daily_treas),
         trend_returns = ifelse(lag(trend_signal) == 1,
                                (trend_signal * sp500_returns), daily_treas),
         buy_hope_returns = (.9 * sp500_returns) + (.1 * daily_treas))
  select(date, trend_signal, z_signal, buy_hope_returns, trend_returns, trend_z_returns, daily_treas)
  na.omit()
  mutate(
         trend_growth = acumulate(1 + trend_returns, '*'),
         trend_z_growth = acumulate(1 + trend_z_returns, '*'),
         buy_hope_growth = acumulate(1 + buy_hope_returns, '*'))
  
  trend_z_results %>% tail()