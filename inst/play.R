library(tidyverse)
library(devtools)
library(quantmod)
library(RQuantLib)
library(ragtop)
load_all()

theme_set(theme_minimal() +
            theme(strip.text = element_text(size = 7, hjust = 0)))

omx <- read.csv2("data/omxc25.csv", check.names = F)

# prices from Yahoo, all omxc25 stocks, back from 2015
omxPrices <- omx$Symbol %>%
  map(getSymbols, from = "2015-01-01", to = date_t, auto.assign = FALSE)

# Clean and convert from xts object to tibble
omxClean <- omxPrices %>%
  map(as.data.frame) %>%
  map(rownames_to_column, "date") %>%
  map(as_tibble) %>%
  map_dfr(pivot_longer, -date) %>%
  mutate(date = as.Date(date)) %>%
  separate(name, into = c("company", "metric"), sep = ".CO.") %>%
  mutate(company = paste0(company, ".CO")) %>%
  left_join(omx, c("company" = "Symbol"))

# Novo example
date_t  <-  Sys.Date()

mPrice <- getSymbols("NOVO-B.CO",
                     from = date_t - 365,
                     to = date_t,
                     auto.assign = FALSE)

vPrice <- as.numeric(mPrice[, 6])  # adjusted closing prices to numeric
vReturns <- diff(log(vPrice))      # log calculate returns in %
dSigma_daily <- sd(vReturns)       # standard deviation
dSigma <- dSigma_daily * sqrt(250) # annual standard deviation (250 trading days in a year)
dS <- vPrice[251]                  # latest stock price
dK <- 790                          # strike Price
drf <- 0.01                        # Risk-free Interest Rate
iT <- as.numeric((as.Date("2022-02-18") - date_t) / 365)  # time to Expiration (in fraction of years)

price1 <- BlackScholes(dS = dS,
                       dK = dK ,
                       iT = iT,
                       dSigma = dSigma,
                       type = "C")

price2 <- RQuantLib::EuropeanOption(type = "call",
                                    underlying = dS,
                                    strik = dK,
                                    dividendYield =  0.0,
                                    riskFreeRate = drf,
                                    maturity = iT,
                                    volatility = dSigma)

price3 <- ragtop::blackscholes(callput = 1,
                               S0 = dS,
                               K = dK,
                               r = drf,
                               time = iT,
                               vola = dSigma,
                               dividends = NULL)

# Compare implementations
price1
price2$value
price3$Price

# Plot
omxClean %>%
  filter(str_detect(metric, "Adjusted")) %>%
  ggplot(aes(x = date, y = value, color = `Company Name`)) +
  geom_line(show.legend = F) +
  scale_y_continuous(labels = NULL) +
  facet_wrap(vars(`Company Name`), scale = "free_y") +
  labs(x = "", y = NULL)





