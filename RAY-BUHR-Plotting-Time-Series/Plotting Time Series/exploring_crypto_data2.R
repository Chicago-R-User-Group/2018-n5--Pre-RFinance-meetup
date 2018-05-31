library(riingo)
library(dplyr)
library(ggplot2)
library(lubridate)
library(anomalize)
library(forecast)
library(tidyquant)
library(sweep)

crypto_prices <- riingo_crypto_prices(
  ticker = c(
    "btcusd",
    # "xrpusd",
    # "xrpbtc",
    # "ethbtc",
    "ethusd",
    # "bchbtc",
    # "bchusd",
    # "ltcbtc",
    "ltcusd"
  ),
  start_date = "2017-01-01",
  end_date = "2018-05-21",
  raw = TRUE,
  resample_frequency = "1day",
  exchanges = "POLONIEX"
)

crypto_prices <- crypto_prices %>%
  mutate_at(
    .vars = vars(ticker, baseCurrency, quoteCurrency),
    .funs = funs(factor)
  )

crypto_anomalies <- crypto_prices %>%
  group_by(ticker) %>%
  prep_tbl_time() %>%
  time_decompose(close, method = "twitter", trend = "2 months") %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose()

crypto_anomalies %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= "2017-04-01") %>%
  filter(str_detect(ticker, "usd")) %>%
  plot_anomalies(time_recomposed = TRUE,
                 ncol = 1, alpha_dots = 0.25) +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::pretty_breaks(5),
                     labels = scales::dollar) +
  labs(title = "Daily Crypto Prices",
       subtitle = "Twitter + GESD Methods")


crypto_prices %>%
  # group_by(ticker) %>%
  prep_tbl_time() %>%
  time_decompose(close) %>%
  anomalize(remainder, method = "gesd") %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= "2017-04-01") %>%
  plot_anomaly_decomposition() +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::pretty_breaks(5),
                     labels = scales::dollar) +
  labs(title = "Daily BTC-USD Price",
       subtitle = "Decomposition")

wks <- seq(
  as.Date("2015-03-01"),
  as.Date("2018-04-01"),
  by = "1 week")

btc_tbl <- crypto_prices %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  select(date, price = close) %>%
  filter(date %in% wks)

btc_ts <- timetk::tk_ts(btc_tbl)

fit <- btc_ts %>% tbats() %>% forecast() %>% autoplot()
sw_augment(fit_arima, timetk_idx = TRUE) %>%
  ggplot(aes(x = index, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual diagnostic") +
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%y") +
  theme_tq()

fcast_arima <- forecast(fit_arima, h = 12)
fcast_tbl <- sw_sweep(fcast_arima, timetk_idx = TRUE)
fcast_tbl %>%
  ggplot(aes(x = index, y = price, color = key)) +
  # 95% CI
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
              fill = "#D5DBFF", color = NA, size = 0) +
  # 80% CI
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key),
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  # Prediction
  geom_line() +
  geom_point() +
  # Aesthetics
  labs(title = "Bitcoin Price Forecast: ARIMA",
       x = "", y = "Price in USD") +
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()

crypto_prices %>%
  ggplot(aes(x=date, y=close, color=baseCurrency)) +
  geom_line() +
  facet_wrap(~baseCurrency, scales = "free_y")

crypto_prices %>%
  ggplot(aes(x=date, y=volume, color=baseCurrency)) +
  geom_line() +
  facet_wrap(~baseCurrency, scales = "fixed")

crypto_prices %>%
  ggplot(aes(x=date, y=volumeNotional, color=baseCurrency)) +
  geom_line() +
  facet_wrap(~baseCurrency, scales = "fixed")

crypto_ts_df <- crypto_prices %>%
  select(currency = baseCurrency, date, price = close) %>%
  spread(key = "currency", value = "price")

btc_prices <- crypto_prices %>%
  filter(baseCurrency == "btc") %>%
  select(currency = baseCurrency, price = close, date)

btc_ts <- ts(btc_prices$price, frequency = 1)

plot(btc_ts,
     ylab = "Closing Price in USD",
     xlab = "Time in Days Since 2017-01-01")

btc_xts <- as.xts(crypto_ts_df$btc, order.by = crypto_ts_df$date)
plot(btc_xts, main = "BTC Closing Price in USD by Day")

crypto_ts <- as.xts(crypto_ts_df[, 2:4], order.by = crypto_ts_df$date)
plot(crypto_ts, panels = )

data("USAccDeaths")
plot(USAccDeaths)
plot(decompose(USAccDeaths))
usa <- as.xts(USAccDeaths)
dusa <- sw_tidy_decomp(decompose(USAccDeaths))
dusa_xts <- as.xts(dusa[, -1], order.by = dusa$index)
plot(dusa_xts, multi.panel = 5, yaxis.same = FALSE,
     main = "Decomposition of USAccDeaths")

dusa %>%
  gather(key = "decomposition", value = "price", -index) %>%
  mutate(index = as.Date(index)) %>%
  ggplot(aes(x=index, y=price, color=decomposition)) +
  geom_line() +
  scale_x_date(labels = scales::date_format("%h-%y"),
               breaks = scales::date_breaks("1 year")) +
  facet_wrap(~decomposition, scales = "free_y", ncol = 1) +
  theme(legend.position = "none")

source("theme_autumn.R")

dusa %>%
  gather(key = "decomposition", value = "price", -index) %>%
  mutate(index = as.Date(index)) %>%
  ggplot(aes(x=index, y=price, color=decomposition)) +
  geom_line() +
  scale_x_date(labels = scales::date_format("%h-%y"),
               breaks = scales::date_breaks("1 year")) +
  facet_wrap(~decomposition, scales = "free_y", ncol = 1) +
  theme_autumn() +
  scale_color_autumn()
