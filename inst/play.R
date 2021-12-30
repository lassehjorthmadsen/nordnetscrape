library(tidyverse)
library(devtools)
library(quantmod)
library(RQuantLib)
library(ragtop)
load_all()

theme_set(theme_minimal() +
            theme(strip.text = element_text(size = 7, hjust = 0)))

omxc25_companies <- read.csv2("data/omxc25.csv", check.names = F)

date_t      <-  Sys.Date()
date_t_1    <-  Sys.Date() - 365

omx_prices_file <- "data-raw/omx_prices.rds"

if (file.exists(omx_prices_file)) {

  omx_prices <- readRDS(omx_prices_file)

  } else {

  # prices from Yahoo
  omx_prices <- omxc25_companies$Symbol %>%
    map(getSymbols, from = "2015-01-01", to = date_t, auto.assign = FALSE)

  omx_clean <- omx_prices %>%
    map(as.data.frame) %>%
    map(rownames_to_column, "date") %>%
    map(as_tibble) %>%
    map_dfr(pivot_longer, -date) %>%
    mutate(date = as.Date(date)) %>%
    separate(name, into = c("company", "metric"), sep = ".CO.") %>%
    mutate(company = paste0(company, ".CO")) %>%
    left_join(omxc25_companies, c("company" = "Symbol"))

  saveRDS(omx_clean, omx_prices_file)
}



mPrice  <-  getSymbols("NOVO-B.CO", from = "2015-11-28", to = date_t, auto.assign = FALSE)

vPrice <- as.numeric(mPrice[,6])   # adjusted closing prices to numeric
vReturns <- diff(log(vPrice)[-1])  # log Calculate Returns in %
dSigma_daily <- sd(vReturns)       # standard deviation
dSigma <- dSigma_daily * sqrt(250) # annual standard deviation (250 trading days in a year)
dS <- vPrice[249]                  # latest stock price
dK <- 700                          # strike Price
drf <- 0.01                        # Risk-free Interest Rate
iT <- 0.25                         # time to Expiration( In fraction of years)

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

# Plot
omx_clean %>%
  filter(str_detect(metric, "Adjusted")) %>%
  ggplot(aes(x = date, y = value, color = `Company Name`)) +
  geom_line(show.legend = F) +
  scale_y_continuous(labels = NULL) +
  facet_wrap(vars(`Company Name`), scale = "free_y") +
  labs(x = "", y = NULL)





