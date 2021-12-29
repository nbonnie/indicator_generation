# Common Indicator Generation in R
#### R functions for common simple quantitative analysis.

Functions included:
- Simple moving average
- Exponential moving average
- Stochastics
- Bollinger bands
- Momentum
- ROC
- MACD
- Williams PR
- RSI
- Elder Ray
- VWAP
- Force Index
- Accumulation Distribution
- Trend (generalized)
- doji pattern
- Hammer reversal pattern
- Engulfing Buy / Sell pattern

## Usage & Setup
Read in your stock data on line 8. To utilize all functions, your data will need the following columns:
 - Data / Time
 - Open
 - Close
 - High
 - Low
 - Volume

Level 1 data like this is very standard, and should not be difficult to obtain for free.

Set the column names according to your data on line 10. If your data-time data is not in standard POSIXct format, you'll need to change the format on line 11.

You can focus in on a specific time range using the filters on lines 15-16, alternatively you could delete these lines to use all available data.

I opted to remove data during the first and last 30 minutes of trading, which is typically when professional high-frequency trading occurs. This occurs on line 19.

Finally, you can set the length of candlesticks you want to analyze. This value is in minutes on line 21. 




