library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(lubridate)

#READ IN STOCK DATA
df <- read.csv('PATH_TO_STOCK_CSV.csv')    # REPLACE THIS WITH YOUR OWN CSV FILE

colnames(df) <- c("date","open","high","close","low","volume","num_trades","w.avg.price")
df$date <- as.POSIXct(df$date, format = "%Y.%m.%d %H:%M:%S")
df <- select(df, -num_trades, -w.avg.price)

#SELECT DATE RANGE
df <- filter(df, date >= "2017-01-01 00:00:00")
df <- filter(df, date <= "2018-01-01 00:00:00")

#REMOVE FIRST AND LAST 30 MINUTES
df <- filter(df, (hour(date) >= 10 & (hour(date) <= 15 & minute(date) < 30)))

candles <- 2   # How many minutes per candlestick
#CREATES A MULTIPLE DIVISIBLE BY CANDLESTICK PERIODS
if ((length(df$close) %% candles) == 1){
  df <- df[1:(length(df$close)-(length(df$close) %% candles)),]
}

df$group <- sort(rep(1:(length(df$close)/candles),candles))

df <- df %>% group_by(group) %>% summarise(date = max(date), open = first(open), high = max(high), close = last(close), low = min(low), volume = sum(volume))
df <- df %>% as.data.frame() %>% select(-group)

mavg <- function(close,b){
  # close - array of closing val
  # b - integer b for number of data points
  desti <- matrix(0,length(close),1)
  for (i in b:length(close)) {
    desti[i] = mean(close[(i-b+1):i])
  }
  return(desti)
}

emaf <- function(df,m){
  # close - array of closing values
  # m - integer m for number of data points
  weight <- (2 / (m + 1))
  current = df$index[1]
  n <- length(unlist(select(filter(df, index == current), close)))
  if(current < m){
    ema <- rep(NA,n)
  } else if(current == m) {
    data <- select(filter(df, index >= (current - m)), close)
    data <- as.data.frame(matrix(unlist(t(data)), byrow = F, n, m+1))
    ema <- rowSums(data) / m
  } else {
    data <- select(filter(df, index == current), close)
    data_ema <- select(filter(df, index == (current - 1)), ema)
    ema <- (data$close - (data_ema * weight)) + data_ema
  }
  return(ema)
}

emaf_macd <- function(df,m){
  # close - array of closing values
  # m - integer m for number of data points
  weight <- (2 / (m + 1))
  current = df$index[1]
  n <- length(unlist(select(filter(df, index == current), macd_fast)))
  if(current < m){
    macd_ema <- rep(NA,n)
  } else if(current == m) {
    data <- select(filter(df, index >= (current - m)), macd_fast)
    data <- as.data.frame(matrix(unlist(t(data)), byrow = F, n, m+1))
    macd_ema <- rowSums(data) / m
  } else {
    data <- select(filter(df, index == current), macd_fast)
    data_ema <- select(filter(df, index == (current - 1)), macd_fast)
    macd_ema <- (data$macd_fast - (data_ema * weight)) + data_ema
  }
  return(macd_ema)
}

emaf_vector <- function(V, b){
  desti <- matrix(0,length(V),1)
  weight <- (2 / (b + 1))
  desti[b] = mean(V[1:b])
  for (i in (b+1):length(V)) {
    desti[i] = (((V[i] - desti[(i-1)]) * weight) + desti[(i-1)])
  }
  return(desti)
}

emaf_a_b_cross <- function(df,a,b) {
  ema_a_short <- emaf_vector(df$close, a)
  ema_b_long <- emaf_vector(df$close, b)
  emaf_buy <-  matrix(0,length(df$close),1)
  emaf_sell <-  matrix(0,length(df$close),1)
  for (i in (b+1):length(df$close)) {
    if ((ema_a_short[(i-1)] < ema_b_long[(i-1)]) & (ema_a_short[(i)] > ema_b_long[(i)])){
      emaf_buy[i] <- 1
    } else if ((ema_a_short[(i-1)] >= ema_b_long[(i-1)]) & (ema_a_short[(i)] <= ema_b_long[(i)])){
      emaf_sell[i] <- 1
    }
  }
  emaf_buy[is.na(emaf_buy)] <- 0
  emaf_sell[is.na(emaf_sell)] <- 0
  return(cbind(emaf_buy,emaf_sell))
}

stochf <- function(df,b){
  # close - array of closing values
  # b - integer b for number of data points
  close <- df$close
  desti <- matrix(0,length(close),1)
  for (i in b:length(close)) {
    low <- min(close[(i-b+1):i])
    high <- max(close[(i-b+1):i])
    desti[i] = (((close[i]-low)/(high-low))*100)
  }
  desti <- emaf_vector(desti,13)
  desti[is.na(desti)] <- 100
  return(desti)
}

bollinger_bands <- function(df){
  ema21 <- emaf_vector(df$close, 21)
  delta_upper <- matrix(0,length(close),1)
  delta_lower <- matrix(0,length(close),1)
  for (i in 42:length(df$close)) {
    delta_upper[i] <- df$close[i] - (ema21[i] + (2 * sd(ema21[(i-20):i])))
    delta_lower[i] <- df$close[i] - (ema21[i] - (2 * sd(ema21[(i-20):i])))
  }
  return(cbind(delta_lower,delta_upper))
}

momentum <- function(df,b){
  # close - full quick table
  # b - number of candles back
  close <- df$close
  desti <- matrix(0,length(close),1)
  for (i in b:length(close)) {
    desti[i] = close[i] - close[i-b+1]
  }
  return(desti)
}

rate_of_change <- function(df,b){
  # close - full quick table
  # b - number of candles back
  close <- df$close
  desti <- matrix(0,length(close),1)
  for (i in b:length(close)) {
    desti[i] = close[i] / close[i-b+1]
  }
  return(desti)
}

mac_d <- function(df){
  # df - full quick table
  df$ema12 <- emaf_vector(df$close,12)
  df$ema26 <- emaf_vector(df$close,26)
  fast_macd <- matrix(0,length(df$close),1)
  for (i in 26:length(df$close)) {
    fast_macd[i] <- df$ema12[i] - df$ema26[i]
  }
  signal <- emaf_vector(fast_macd,9)
  macd_histogram <- fast_macd - signal
  macd_histogram[(0:35)] <- NA
  macd_b <- matrix(0,length(df$close),1)
  macd_s <- matrix(0,length(df$close),1)
  # MACD Strategy:
  for (i in 39:length(df$close)){
    if (all(macd_histogram[(i-3):(i-1)] < 0) & (macd_histogram[i] > 0)) {
      macd_b[i] <- 1
    } else if (macd_histogram[(i-1)] > macd_histogram[(i)]) {
      macd_s[i] <- 1
    }
  }
  return(cbind(macd_b,macd_s))
}

williamspr <- function(df, b){
  close <- df$close
  desti <- matrix(0,length(close),1)
  for (i in b:length(close)) {
    desti[i] = (max(close[(i-b+1):i]) - close[i]) / (max(close[(i-b+1):i]) - min(close[(i-b+1):i]))
    if (is.nan(desti[i])){
      desti[i] <- 1
    }
  }
  desti <- emaf_vector(desti,13)
  return(desti)
}

RSI <- function(df, b){
  # b as 13
  desti <- matrix(0,length(df$close),1)
  for (i in b:length(df$close)) {
    boolean <- df$close[(i-b+1):i] > df$open[(i-b+1):i]
    denom <- abs(mean(df$open[(i-b+1):i][!boolean] - df$close[(i-b+1):i][!boolean]))
    if (denom == 0 | is.na(denom)){
      denom <- 0.01
    }
    desti[i] <- ((mean(df$close[(i-b+1):i][boolean] - df$open[(i-b+1):i][boolean])) / denom)
  }
  #desti[is.nan(desti)] <- 100
  #desti <- emaf_vector(desti,3)
  return((100 - (100/(1+desti))))
}

elder_ray <- function(df){
  bullp <- matrix(0,length(df$close),1)
  bearp <- matrix(0,length(df$close),1)
  ema13 <- emaf_vector(df$close,13)
  bullp <- df$high - ema13
  bearp <- df$low - ema13
  #for (i in 13:length(close)) {
  #  bullp[i] = df$high[i] - df$ema13[i]
  #  bearp[i] = df$low[i] - df$ema13[i]
  #}
  # Elder Ray Strategy
  elder_b <- matrix(0,length(df$close),1)
  elder_s <- matrix(0,length(df$close),1)
  for (i in 2:length(df$close)) {
    if ((bullp[(i-1)] < bullp[(i)]) & (bearp[(i-1)] < bearp[(i)]) & (bullp[i] < 0) & (bearp[i] < 0)){
      elder_b[i] <- 1
    } else if ((bullp[(i-1)] > bullp[(i)]) & (bearp[(i-1)] > bearp[(i)])){
      elder_s[i] <- 1
    }
  }
  return(cbind(elder_b,elder_s))
}

forceindex <- function(df){
  desti <- matrix(0,length(df$close),1)
  for (i in 2:length(df$close)) {
    desti[i] <- df$volume[i] * (df$close[i] - df$close[i-1])
  }
  desti <- emaf_vector(desti,13)
  return(desti)
}

accumulation_dist <- function(df){
  desti <- (df$close - df$open) / (df$high - df$low)
  desti <- emaf_vector(desti,3)
  return(desti)
}

vwapf <- function(df){
  desti <- matrix(0,length(df$close),1)
  typical <- (df$volume * ((df$high + df$low + df$close) / 3))
  desti[1] <- (typical[1] / df$volume[1])
  c_typical <- matrix(0,length(df$close),1)
  c_volume <- matrix(0,length(df$close),1)
  c_typical[1] <- typical[1]
  c_volume[1] <- df$volume[1]
  for (i in 2:length(df$close)) {
      c_typical[i] <- c_typical[i-1] + typical[i]
      c_volume[i] <- c_volume[i-1] + df$volume[i]
  }
  desti <- c_typical / c_volume
  #VWAP Strategy
  vwap_b <- matrix(0,length(df$close),1)
  for (i in 4:length(df$close)) {
    if (all(df$close[(i-3):(i-1)] < desti[(i-3):(i-1)]) & (df$close[i] > desti[i])){
      vwap_b[i] <- 1
    }
  }
  return(vwap_b)
}


# Japanese Reversal Patterns
# variables:
hRB <- 0.2  #hammerReversalBody
hRT <- 0.05  #hammerReversalThresh
tweezerSession = 20
tweezerThreshold = .001

trendf <- function(df){
  #returns 1 for up and 0 for down
  close <- df$close
  desti <- matrix(0,length(close),1)
  for (i in 3:length(close)) {
    desti[i] <- (3*(close[(i-2)] + (2*close[(i-1)]) + (3*close[(i)])) - (sum(close[(i-2:i)])*6)) / ((3*sum((close[(i-2:i)])^2)) - sum(close[(i-2:i)])^2)
  }
  ifelse(desti>0,1,0)
  return(desti)
}

dojif <- function(df){
  dj <- ifelse(df$open == df$close, 1, 0)
  return(dj)
}

hammertime <- function(df){
  open <- df$open
  close <- df$close
  high <- df$high
  low <- df$low
  d <- high - low
  d[d==0] <- 0.01
  ht <- ifelse((((abs(open - close) / d) < hRB) & ((((high - max(open, close))/d) < hRT) | (((min(open, close) - low)/d < hRT)))) == TRUE, 1, 0)
  return(ht)
}

engulfing <- function(df){
  # use parameter trend from current trendf oputput (trendf must be run before this function, it's output as an argument)
  open <- df$open
  close <- df$close
  high <- df$high
  low <- df$low
  buy <- matrix(0,length(close),1)
  sell <- matrix(0,length(close),1)
  for (i in 3:length(close)) {
    if (((open[(i-1)]<close[(i)])&(close[(i-1)]>open[(i)])) | ((open[(i-1)]>close[(i)])&(close[(i-1)]<open[(i)]))){
      buy[i] <- 1
      sell[i] <- 0
    } else if (((open[(i-1)]>close[(i)])&(close[(i-1)]<open[(i)])) | ((open[(i-1)]<close[(i)])&(close[(i-1)]>open[(i)]))){
      buy[i] <- 0
      sell[i] <- 1
    } else {
      buy[i] <- 0
      sell[i] <- 0
    }
  }
  return(cbind(buy,sell))
}

engulfing_2 <- function(df){
  # use parameter trend from current trendf oputput (trendf must be run before this function, it's output as an argument)
  open <- df$open
  close <- df$close
  high <- df$high
  low <- df$low
  buy <- matrix(0,length(close),1)
  sell <- matrix(0,length(close),1)
  for (i in 3:length(close)) {
    size_logic <- (((high[i-1] > high[i]) & (low[i-1] < low[i])) | ((high[i-1] < high[i]) & (low[i-1] > low[i])))
    if((size_logic == TRUE)&((open[(i-1)]>close[(i-1)])&(close[(i)]>open[(i)]))){
      buy[i] <- 1
      sell[i] <- 0
    } else if ((size_logic == TRUE)&((open[(i-1)]<close[(i-1)])&(close[(i)]<open[(i)]))){
      buy[i] <- 0
      sell[i] <- 1
    } else {
      buy[i] <- 0
      sell[i] <- 0
    }
  }
  return(cbind(buy,sell))
}

start_time <- Sys.time()
  
a <- as.data.frame(emaf_a_b_cross(df,13,21))
df$ema_cross_buy <- a$V1
df$ema_cross_sell <- a$V2
df$stoch <- stochf(df,14)
b <- as.data.frame(bollinger_bands(df))
df$bollinger_lower <- b$delta_lower
df$bollinger_upper <- b$delta_upper
df$moment <- momentum(df,12)
df$roc <- rate_of_change(df,12)
d <- as.data.frame(mac_d(df))
df$macd_buy <- d$V1
df$macd_sell <- d$V2
df$wpr <- williamspr(df, 12)
df$rsi <- RSI(df,13)
e <- as.data.frame(elder_ray(df))
df$elder_buy <- e$V1
df$elder_sell <- e$V2
df$vwap_buy <- vwapf(df)
df$fi <- forceindex(df)
df$ad <- accumulation_dist(df)
df$trend <- trendf(df)
df$doji <- dojif(df)
df$hammer <- hammertime(df)
f <- as.data.frame(engulfing(df))
df$engulfing_buy <- f$V1
df$engulfing_sell <- f$V2
g <- as.data.frame(engulfing_2(df))
df$engulfing_2_buy <- g$V1
df$engulfing_2_sell <- g$V2
  
end_time <- Sys.time()
print(end_time - start_time)

# write.csv(df, file = "AAPL_2017_2018_5MIN.csv")





