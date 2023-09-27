library(quantmod)
library(tidyquant) #same as quantmod
library(tidyverse)
library(data.table)
library(ggplot2)
library(timeSeries)
library(forecast)
library(tseries)
library(gridExtra)
library(dplyr)
library(tidyr)
library(zoo)
library(xts)
library(caret)
library(repr)
library(TTR)
library(lubridate)
library(caTools)
library(lmtest)
library(moments)
library(car)
library(sandwich)
library(urca)
library(Metrics)
library(ROCR)# forprediction
library(patchwork)
library(margins)
library(pROC)#for Roc  curve
library(randomForest)

# Stock values are stated in terms of the closing price and the adjusted closing price.
# The closing price is the raw price, which is just the cash value of the last transacted price before the market closes. 
# The adjusted closing price factors in anything that might affect the stock price after the market closes.

##############################################################################################################

# Load securities data from Yahoo!

myStocks <-lapply(c("RELIANCE.NS", "TCS.NS","HDFCBANK.NS","ICICIBANK.NS","HINDUNILVR.NS","ITC.NS","INFY.NS","SBIN.NS","BHARTIARTL.NS","HDFC.NS"),
                  function(x) {getSymbols(x, 
                                          src = "yahoo",
                                          from = "2018/01/02", 
                                          to = "2023/05/31",
                                          periodicity = "daily",
                                          auto.assign=FALSE)} )
names(myStocks) <- c("REL", "TCS","HDFCBANK","ICICIBANK","HINDUNILVR","ITC","INFY","SBI","AIRTEL","HDFC")
head(myStocks$TCS)
class(myStocks$REL)

# Plot the adjusted price for various securities

a=ggplot(myStocks$REL, aes(x = index(myStocks$REL), y = myStocks$REL[,6])) + geom_line(color = "darkblue") +
  ggtitle("Reliance prices series") + xlab("Date") + ylab("Adj Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

b=ggplot(myStocks$TCS, aes(x = index(myStocks$TCS), y = myStocks$TCS[,6])) + geom_line(color = "red") +
  ggtitle("TCS prices series") + xlab("Date") + ylab("Adj Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

c=ggplot(myStocks$HDFCBANK, aes(x = index(myStocks$HDFCBANK), y = myStocks$HDFCBANK[,6])) + geom_line(color = "green") +
  ggtitle("HDFC Bank prices series") + xlab("Date") + ylab("Adj Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

d=ggplot(myStocks$ICICIBANK, aes(x = index(myStocks$ICICIBANK), y = myStocks$ICICIBANK[,6])) + geom_line(color = "violet") +
  ggtitle("ICICIBANK prices series") + xlab("Date") + ylab("Adj Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

e=ggplot(myStocks$HINDUNILVR, aes(x = index(myStocks$HINDUNILVR), y = myStocks$HINDUNILVR[,6])) + geom_line(color = "black") +
  ggtitle("HINDUNILVR prices series") + xlab("Date") + ylab("Adj Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

f=ggplot(myStocks$ITC, aes(x = index(myStocks$ITC), y = myStocks$ITC[,6])) + geom_line(color = "darkgreen") +
  ggtitle("ITC prices series") + xlab("Date") + ylab("Adj Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

g=ggplot(myStocks$ITC, aes(x = index(myStocks$INFY), y = myStocks$INFY[,6])) + geom_line(color = "maroon") +
  ggtitle("INFY prices series") + xlab("Date") + ylab("Adj Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

h=ggplot(myStocks$ITC, aes(x = index(myStocks$SBI), y = myStocks$SBI[,6])) + geom_line(color = "pink") +
  ggtitle("SBI prices series") + xlab("Date") + ylab("Adj Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

i=ggplot(myStocks$ITC, aes(x = index(myStocks$AIRTEL), y = myStocks$AIRTEL[,6])) + geom_line(color = "orange") +
  ggtitle("AIRTEL prices series") + xlab("Date") + ylab("Adj Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

j=ggplot(myStocks$HDFC, aes(x = index(myStocks$HDFC), y = myStocks$HDFC[,6])) + geom_line(color = "purple") +
  ggtitle("HDFC prices series") + xlab("Date") + ylab("Adj Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

combined_plot <- grid.arrange(a,b,c,d,e,f,g,h,i,j, ncol = 2)


# View whole data of a list

head(myStocks$TCS,1360)
myStocks[1]

# Obtain the Nifty 50 Index data

market<-lapply(c("^NSEI"),
               function(x) {getSymbols(x, 
                                       src = "yahoo",
                                       from = "2018/01/01", 
                                       to = "2023/05/31",
                                       periodicity = "daily",
                                       auto.assign=FALSE)} )
names(market) <- ("Nifty")

# Check for NA

sum(is.na((market$Nifty$NSEI.Adjusted)))
head(market$Nifty$NSEI.Adjusted)
head(market$Nifty)

# Plot the returns of the Nifty 50 Index

ggplot(market$Nifty, aes(x = index(market$Nifty), y = market$Nifty[,6])) + geom_line(color = "purple") +
  ggtitle("NIFTY50 index prices series") + xlab("Date") + ylab("Adj Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

############################################################################################################

# Create an object with adjusted values for Stocks

adjustedPrices <- lapply(myStocks, Ad)
adjustedPrices <- do.call(merge, adjustedPrices)
names(adjustedPrices)<-c("REL", "TCS","HDFCBANK","ICICIBANK","HINDUNILVR","ITC","INFY","SBI","BHaratiAIRTEL","HDFC")
head(adjustedPrices)
sum(is.na(adjustedPrices$TCS))

# Create an object with adjusted values for Market Index (Nifty 50 Index)

adjustedPrices_mar <- lapply(market, Ad)
adjustedPrices_mar <- do.call(merge, adjustedPrices_mar)
names(adjustedPrices_mar)<-("Nifty_adj")

# Replace missing value with last known value

adjustedPrices_mar_fill<-na.locf(adjustedPrices_mar)#library zoo

# Check for NA (again)

sum(is.na((adjustedPrices_mar_fill)))# there are no na left

# Calculate returns 

# return = (adjustedPrices of day/adjustedPrices of previous day)-1

stockReturns <- Return.calculate(adjustedPrices)%>%na.omit()

# Add column for cumulative sum

stockReturns$Cum_Ret_Rel<-cumsum(stockReturns$REL)
stockReturns$Cum_Ret_TCS<-cumsum(stockReturns$TCS)
stockReturns$Cum_Ret_HDFCBank<-cumsum(stockReturns$HDFCBANK)
stockReturns$Cum_Ret_ICICIBANK<-cumsum(stockReturns$ICICIBANK)
stockReturns$Cum_Ret_HINDUNILVR<-cumsum(stockReturns$HINDUNILVR)
stockReturns$Cum_Ret_ITC<-cumsum(stockReturns$ITC)
stockReturns$Cum_Ret_INFY<-cumsum(stockReturns$INFY)
stockReturns$Cum_Ret_SBI<-cumsum(stockReturns$SBI)
stockReturns$Cum_Ret_BHaratiAIRTEL<-cumsum(stockReturns$BHaratiAIRTEL)
stockReturns$Cum_Ret_HDFC<-cumsum(stockReturns$HDFC)
head(stockReturns)
sum(is.na((stockReturns)))

       
# Visualize  securities' returns

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$REL*100)) + geom_line(color = "darkblue") +
  ggtitle("% Reliance return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$Cum_Ret_Rel*100)) + geom_line(color = "darkblue") +
  ggtitle("% Reliance cum return") + xlab("Date") + ylab("% Return")+ 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

ggplot(stockReturns, aes(x =index(stockReturns) , y = stockReturns$TCS*100)) + geom_line(color = "red") +
  ggtitle("% TCS return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

ggplot(stockReturns, aes(x =index(stockReturns) , y = stockReturns$Cum_Ret_TCS * 100)) + geom_line(color = "red") +
  ggtitle("% TCS cum return") + xlab("Date") +ylab("% Return")+  
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$HDFCBANK * 100)) + geom_line(color = "green") +
  ggtitle("% HDFC Bank return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$Cum_Ret_HDFCBank * 100)) + geom_line(color = "green") +
  ggtitle("% HDFC Bank cum return") + xlab("Date") + ylab("% Return")+ 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$ICICIBANK * 100)) + geom_line(color = "violet") +
  ggtitle("% ICICIBANK return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$Cum_Ret_ICICIBANK * 100)) + geom_line(color = "violet") +
  ggtitle("% ICICIBANK cum return") + xlab("Date") + ylab("% Return")+  
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$HINDUNILVR * 100)) + geom_line(color = "black") +
  ggtitle("% HINDUNILVR return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$Cum_Ret_HINDUNILVR * 100)) + geom_line(color = "black") +
  ggtitle("% HINDUNILVR cum return") + xlab("Date") +ylab("% Return")+ 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$ITC * 100)) + geom_line(color = "darkgreen") +
  ggtitle("% ITC return") + xlab("Date") + ylab("% Return") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$Cum_Ret_ITC * 100)) + geom_line(color = "darkgreen") +
  ggtitle("% ITC cum return") + xlab("Date") + ylab("% Return")+ 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

ggplot(stockReturns, aes(x = index(stockReturns), y =stockReturns$INFY * 100)) + geom_line(color = "maroon") +
  ggtitle("% INFY return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

ggplot(stockReturns, aes(x = index(stockReturns), y =stockReturns$Cum_Ret_INFY * 100)) + geom_line(color = "maroon") +
  ggtitle("% INFY cum return") + xlab("Date") + ylab("% Return")+ 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$SBI * 100)) + geom_line(color = "grey") +
  ggtitle("% SBI return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$Cum_Ret_SBI * 100)) + geom_line(color = "grey") +
  ggtitle("% SBI cum return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$BHaratiAIRTEL * 100)) + geom_line(color = "orange") +
  ggtitle("% BharatiAirtel return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$Cum_Ret_BHaratiAIRTEL * 100)) + geom_line(color = "orange") +
  ggtitle("% BharatiAirtel cum return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$HDFC * 100)) + geom_line(color = "purple") +
  ggtitle("% HDFC return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

ggplot(stockReturns, aes(x = index(stockReturns), y = stockReturns$Cum_Ret_HDFC * 100)) + geom_line(color = "purple") +
  ggtitle("% HDFC cum return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

head(stockReturns[,1:10]*100,5)
tail(stockReturns[,1:10]*100,5)

# Return on Market Index (Nifty 50 Index)

marketReturns <- Return.calculate(adjustedPrices_mar_fill)%>%na.omit()
head(marketReturns)

ggplot(marketReturns, aes(x = index(marketReturns), y = marketReturns$Nifty_adj * 100)) + geom_line(color = "darkblue") +
  ggtitle("%Nifty50 return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

market<-marketReturns
market$cum_nifty<- cumsum(market$Nifty_adj)

ggplot(marketReturns, aes(x = index(marketReturns), y = market$cum_nifty * 100)) + geom_line(color = "darkblue") +
  ggtitle("%Nifty50 cum return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

head(marketReturns*100,5)
tail(marketReturns*100,5)

# Equal-weighted portfolio returns

stockReturns1 <- Return.calculate(adjustedPrices)%>%na.omit()# to make a time series object containing adjusted close price of 10 security
portReturns <- Return.portfolio(stockReturns1, c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))
head(portReturns*100,5)
tail(portReturns*100,5)

# Cumulative returns for the portfolio

portReturns1 <- portReturns
portReturnsCum <- cumsum(portReturns1)

# Plot the portfolio return and compare it with the Nifty 50 Index returns

ggplot(portReturns, aes(x = index(portReturns), y = portReturns*100)) + geom_line(color = "darkblue") +
  ggtitle("% Portfolio return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

ggplot(portReturnsCum, aes(x = index(portReturnsCum), y = portReturnsCum*100)) + geom_line(color = "darkblue") +
  ggtitle("% Portfolio cum return") + xlab("Date") + ylab("% Return") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "years")

# This above is without periodic rebalancing. We can consider rebalancing, done monthly. 

portReturnsRebalanced <- Return.portfolio(stockReturns1, c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1), rebalance_on = "months")
table.Stats(portReturnsRebalanced)
head(portReturnsRebalanced*100)

# Table for portfolio without and with rebalancing 

allPortReturns <- cbind(portReturns, portReturnsRebalanced)
colnames(allPortReturns) <- c("Non-Rebalanced", "Monthly Rebalanced")
Portfolio_combined_table<-table.AnnualizedReturns(allPortReturns)  #or use(allPortReturns, Rf = 0.1/252) for 20%rf

head(Portfolio_combined_table)

# Get cumulative portfolio returns

portReturnsRebalanced1 <- Return.portfolio(stockReturns1, c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1), rebalance_on = "months",wealth.index = "T")
table.Stats(portReturnsRebalanced1)
head(portReturnsRebalanced1)
tail(portReturnsRebalanced1)

# Contribution of all stock to portfolio

port_contr<-Return.portfolio(stockReturns1,c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1),rebalance_on = "months",contribution = TRUE)
port_contr_wealth<-Return.portfolio(stockReturns1,c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1),rebalance_on = "months",wealth.index= TRUE,contribution = TRUE)

# Monthly returns Table 

port_monthly<-apply.monthly(portReturnsRebalanced,Return.cumulative)
port_monthly_table<-table.CalendarReturns(port_monthly,as.perc = FALSE,digit=7)#for getting percentage value use true
head(port_monthly_table)

# Test and train - Regression Problem using equal weighted portfolio return as depended variable 

# Create and bind portfolio return (rebalanced) and market return in a single data frame

portReturns_dtfrm<-as.data.frame(portReturns)
marketReturns_dtfrm<-as.data.frame(marketReturns)
data1<-cbind(portReturns_dtfrm,marketReturns_dtfrm)
data1<-cbind(date=rownames(data1),data1) # Copy index column to new column
rownames(data1) <- NULL # Change the index or row names to numbers
data1$date<-as.Date(data1$date)
str(data1)
dim(data1)
head(data1)

qplot(x=data1$portfolio.returns, y=data1$Nifty_adj,
      data=data1, na.rm=TRUE,
      main="correlation",
      xlab="portfolio_return", ylab="market return")
cor(data1$portfolio.returns,data1$Nifty_adj)

# Create training data and test data

set.seed(11) # Get same value each time
ind = sample.split(Y = data1$portfolio.returns, SplitRatio = 0.8) # https://people.duke.edu/~rnau/three.htm

# Training data

train = data1[ind,]
dim(train)
head(train)

# Test data

test = data1[!ind,]
dim(test)
head(test)

# Linear regression

adf.test(data1$portfolio.returns) # Check portfolio.return series is stationary or not (p<0.05 reject null, which is 'series is not stationary')
adf.test(data1$Nifty_adj)

# If stationary, use linear model

slr=lm(portfolio.returns~Nifty_adj,data =train )
model= summary(slr)
model
S(slr)

# Residual diagnostics

densityPlot(model$residuals)
densityPlot(rstudent(slr))
qqPlot(slr, id=(list(n=4)))
outlierTest(slr)

# Check heteroscedasticity

par(mfrow=c(2,2)) # Four charts in one panel
plot(slr,ask=FALSE) # Set ask=FALSE to automatically display each plot without waiting for user confirmation 
par(mfrow=c(1,1))

# NCV Test and Breusch-Pagan Test 

ncvTest(slr) # Test error with fitted values; null hypothesis is 'heteroscedasticity does not exist'

bptest(slr, studentize = F)
bptest(slr, studentize = T) # can also use bptest(slr) as 'studendize' will be 'true' by default

# Auto correlation in errors: Durbin-Watson Test and Breusch-Godfrey test for first-order autocorrelation

dwtest(slr)

bgtest(slr,order = 1) # order=1 implies of lag of 1 time period

# Prediction with SLR

Predict<-predict(slr,test)
summary(Predict)
head(Predict)

plot(index(test), test$portfolio.returns, xlab="index", ylab="Returns",
     main="Predicted vs Actual Returns", col="red" ,lwd=1, type="l")

lines(index(test), Predict, col="blue", type="l")

legend("bottomright", c("Actual Return", "Predicted Returns"),
       fill=c("red", "blue"))
cor(test$portfolio.returns,Predict)

# Naive model

Naive= lm(portfolio.returns ~ 1, data= train)#no independent variable
summary(Naive)

Predict_Naive= predict(Naive, test)

# Make a dataframe

Error= data.frame(matrix(nrow=4, ncol=2))
colnames(Error)=c("Naive", "Market")
rownames(Error)=c("RMSE", "SMAPE", "RMSLE", "Average")

Error$Naive[1]=rmse(Predict_Naive,test$portfolio.returns)
Error$Market[1]=rmse(Predict,test$portfolio.returns)

Error$Naive[2]=smape(Predict_Naive,test$portfolio.returns )
Error$Market[2]=smape(Predict,test$portfolio.returns )

Error$Naive[3]=rmsle(Predict_Naive,test$portfolio.returns )
Error$Market[3]=rmsle(Predict,test$portfolio.returns )

Error$Naive[4]=mean(Error$Naive[1:3])
Error$Market[4]=mean(Error$Market[1:3])

Error

# Create 'updown' variable, 1 for positive return and 0 for negative return of Portfolio return

data2<-data1 |> mutate(updown = ifelse(portfolio.returns>0, 1, 0))

# Create training data and test data

set.seed(11)
indd = sample.split(Y = data2$updown, SplitRatio = 0.8)

# Training data

train1 = data2[indd,]
dim(train1)
head(train1)

# Test data

test1 = data2[!indd,]
dim(test1)
head(test1)

# Check distibution of Updown variable

prop.table(table(data2$updown))*100
prop.table(table(test1$updown))*100
prop.table(table(train1$updown))*100

# Linear model

linear=lm(updown~Nifty_adj,data = train1)
summary(linear)

linear$fitted.values # View fitted value

# Threshold of 0.4

fitted.results<-ifelse(linear$fitted.values>0.4,1,0)#find fitted in data= train1
CM_linear4<-confusionMatrix(as.factor(fitted.results),as.factor(train1$updown))
print(CM_linear4)
Performance_4= tibble(Threshold= 0.4, Accuracy= CM_linear4$overall["Accuracy"],
                      Sensitivity=CM_linear4$byClass["Sensitivity"],
                      Specificity=CM_linear4$byClass["Specificity"])

# Threshold of 0.5

fitted.results<-ifelse(linear$fitted.values>0.5,1,0)#find fitted in data= train1
CM_linear5<-confusionMatrix(as.factor(fitted.results),as.factor(train1$updown))
print(CM_linear5)
Performance_5= tibble(Threshold= 0.5, Accuracy= CM_linear5$overall["Accuracy"],
                      Sensitivity=CM_linear5$byClass["Sensitivity"],
                      Specificity=CM_linear5$byClass["Specificity"])

# Threshold of 0.6

fitted.results=ifelse(linear$fitted.values>0.6, 1, 0)
CM_linear6=confusionMatrix(as.factor(fitted.results), as.factor(train1$updown))
print(CM_linear6)
Performance_6= tibble(Threshold= 0.6, Accuracy= CM_linear6$overall["Accuracy"],
                      Sensitivity=CM_linear6$byClass["Sensitivity"],
                      Specificity=CM_linear6$byClass["Specificity"])

# Threshold of 0.7

fitted.results=ifelse(linear$fitted.values>0.7, 1, 0)
CM_linear7=confusionMatrix(as.factor(fitted.results), as.factor(train1$updown))
print(CM_linear7)
Performance_7= tibble(Threshold= 0.7, Accuracy= CM_linear7$overall["Accuracy"],
                      Sensitivity=CM_linear7$byClass["Sensitivity"],
                      Specificity=CM_linear7$byClass["Specificity"])

Linear_Performance= rbind(Performance_4, Performance_5, Performance_6, Performance_7)
Linear_Performance$Class="Linear"
Linear_Performance

# Logit model 

logit=glm(formula(linear),data = train1,family = binomial("logit"))
summary(logit)
margins(logit) 

logit_naive<-glm(updown ~ 1,data = train1,family = binomial("logit"))
pseudoRsqr<- 1-(logLik(logit)/logLik(logit_naive))
pseudoRsqr

logit$fitted.values

# Threshold of 0.4

fitted.results=ifelse(logit$fitted.values>0.4, 1, 0)
CM_logit4=confusionMatrix(as.factor(fitted.results), as.factor(train1$updown))
print(CM_logit4)
Performance_4= tibble(Threshold= 0.4, Accuracy= CM_logit4$overall["Accuracy"],
                      Sensitivity=CM_logit4$byClass["Sensitivity"],
                      Specificity=CM_logit4$byClass["Specificity"])

# Threshold of 0.5

fitted.results=ifelse(logit$fitted.values>0.5, 1, 0)
CM_logit5=confusionMatrix(as.factor(fitted.results), as.factor(train1$updown))
print(CM_logit5)
Performance_5= tibble(Threshold= 0.5, Accuracy= CM_logit5$overall["Accuracy"],
                      Sensitivity=CM_logit5$byClass["Sensitivity"],
                      Specificity=CM_logit5$byClass["Specificity"])

# Threshold of 0.6

fitted.results=ifelse(logit$fitted.values>0.6, 1, 0)
CM_logit6=confusionMatrix(as.factor(fitted.results), as.factor(train1$updown))
print(CM_logit6)
Performance_6= tibble(Threshold= 0.6, Accuracy= CM_logit6$overall["Accuracy"],
                      Sensitivity=CM_logit6$byClass["Sensitivity"],
                      Specificity=CM_logit6$byClass["Specificity"])

# Threshold of 0.7

fitted.results=ifelse(logit$fitted.values>0.7, 1, 0)
CM_logit7=confusionMatrix(as.factor(fitted.results), as.factor(train1$updown))
print(CM_logit7)
Performance_7= tibble(Threshold= 0.7, Accuracy= CM_logit7$overall["Accuracy"],
                      Sensitivity=CM_logit7$byClass["Sensitivity"],
                      Specificity=CM_logit7$byClass["Specificity"])


Logit_Performance= rbind(Performance_4, Performance_5, Performance_6, Performance_7)
Logit_Performance$Class="Logit"
Logit_Performance

# Probit model

probit=glm(formula(linear),data = train1,family = binomial("probit"))
summary(probit)
margins(probit) 

probit_naive<-glm(updown ~ 1,data = train1,family = binomial("probit"))
pseudoRsqr1<- 1-(logLik(probit)/logLik(probit_naive))
pseudoRsqr1

probit$fitted.values

# Threshold of 0.4

fitted.results=ifelse(probit$fitted.values>0.4, 1, 0)
CM_probit4=confusionMatrix(as.factor(fitted.results), as.factor(train1$updown))
print(CM_probit4)
Performance_4= tibble(Threshold= 0.4, Accuracy= CM_probit4$overall["Accuracy"],
                      Sensitivity=CM_probit4$byClass["Sensitivity"],
                      Specificity=CM_probit4$byClass["Specificity"])

# Threshold of 0.5

fitted.results=ifelse(probit$fitted.values>0.5, 1, 0)
CM_probit5=confusionMatrix(as.factor(fitted.results), as.factor(train1$updown))
print(CM_probit5)
Performance_5= tibble(Threshold= 0.5, Accuracy= CM_probit5$overall["Accuracy"],
                      Sensitivity=CM_probit5$byClass["Sensitivity"],
                      Specificity=CM_probit5$byClass["Specificity"])

# Threshold of 0.6

fitted.results=ifelse(probit$fitted.values>0.6, 1, 0)
CM_probit6=confusionMatrix(as.factor(fitted.results), as.factor(train1$updown))
print(CM_probit6)
Performance_6= tibble(Threshold= 0.6, Accuracy=CM_probit6$overall["Accuracy"],
                      Sensitivity=CM_probit6$byClass["Sensitivity"],
                      Specificity=CM_probit6$byClass["Specificity"])

# Threshold of 0.7

fitted.results=ifelse(probit$fitted.values>0.7, 1, 0)
CM_probit7=confusionMatrix(as.factor(fitted.results), as.factor(train1$updown))
print(CM_probit7)
Performance_7= tibble(Threshold= 0.7, Accuracy= CM_probit7$overall["Accuracy"],
                      Sensitivity=CM_probit7$byClass["Sensitivity"],
                      Specificity=CM_probit7$byClass["Specificity"])

Probit_Performance= rbind(Performance_4, Performance_5, Performance_6,Performance_7)
Probit_Performance$Class="Probit"
Probit_Performance

# Model performance

Perfomance<-tibble(rbind.data.frame(Linear_Performance,Logit_Performance,Probit_Performance))
Perfomance

P1<-ggplot(Perfomance,aes(factor(Threshold),Accuracy))+geom_point(aes(colour=Class))+
  xlab('Threshold')+ylab('Specificity')
P2<-ggplot(Perfomance,aes(factor(Threshold),Accuracy))+geom_point(aes(colour=Class))+
  xlab('Threshold')+ylab('Sensitivity')
P3<-ggplot(Perfomance,aes(factor(Threshold),Accuracy))+geom_point(aes(colour=Class))+
  xlab('Threshold')+ylab('Accuracy')
P1/P2/P3 #library patchwork

# ROC

par(mfrow=c(3,1), bg="lightyellow") # set count of figures in a single plot, and background color

# Linear model

pr<- ROCR::prediction(linear$fitted.values,train1$updown)
prf<-performance(pr, measure = "tpr",x.measure = "fpr")
plot(prf)
abline(0,1, lwd=2, lty=2)

auc<-performance(pr, measure = "auc") # Area under curve (above the 0.5 line)
auc@y.values[1]

# Logit model

pr<-ROCR::prediction(logit$fitted.values,train1$updown)
prf<-performance(pr, measure = "tpr",x.measure = "fpr")
plot(prf)
abline(0,1, lwd=2, lty=2)

auc<-performance(pr, measure = "auc") # Area under curve (above the 0.5 line)
auc@y.values[1]

# Probit model

pr<-ROCR::prediction(probit$fitted.values,train1$updown)
prf<-performance(pr, measure = "tpr",x.measure = "fpr")
plot(prf)
abline(0,1, lwd=2, lty=2)

auc<-performance(pr, measure = "auc") # Area under curve (above the 0.5 line)
auc@y.values[1]
par(mfrow=c(1,1), bg="white")

# Test and train - Regression Problem using rebalanced equal weighted portfolio return as depended variable 

# Create and bind portfolio return (rebalanced) and market return in a single data frame

portReturns_dtfrm1<-as.data.frame(portReturnsRebalanced)
marketReturns_dtfrm1<-as.data.frame(marketReturns)
data3<-cbind(portReturns_dtfrm1,marketReturns_dtfrm1)
data3<-cbind(date=rownames(data3),data3)#copy index column to new column
rownames(data3) <- NULL#to change the index or row names to numbers
data3$date<-as.Date(data3$date)
str(data3)
dim(data3)
head(data3)

qplot(x=data3$portfolio.returns, y=data3$Nifty_adj,
      data=data3, na.rm=TRUE,
      main="correlation",
      xlab="portfolio_return", ylab="market return")
cor(data3$portfolio.returns,data3$Nifty_adj)

# Create training data and test data

set.seed(11) # Get same value each time
ind1 = sample.split(Y = data3$portfolio.returns, SplitRatio = 0.8)  

# Training data

train2 = data3[ind1,]
dim(train2)
head(train2)

# Test data

test2 = data3[!ind1,]
dim(test2)
head(test2)

# Linear regression

adf.test(data3$portfolio.returns) # Check portfolio.return series is stationary or not (p<0.05 reject null, which is 'series is not stationary')
adf.test(data3$Nifty_adj)

# If stationary, use linear model

slr1=lm(portfolio.returns~Nifty_adj,data =train2 )
model= summary(slr1)
model
S(slr1)

# Residual diagnostics

densityPlot(model$residuals)
densityPlot(rstudent(slr1))
qqPlot(slr1, id=(list(n=4)))
outlierTest(slr1)

# Check heteroscedasticity

par(mfrow=c(2,2)) # Four charts in one panel
plot(slr1,ask= FALSE) # Set ask=FALSE to automatically display each plot without waiting for user confirmation
par(mfrow=c(1,1))

# NCV Test and Breusch-Pagan Test

ncvTest(slr1) # Test error with fitted values; null hypothesis is 'heteroscedasticity does not exist'

bptest(slr1, studentize = F)
bptest(slr1, studentize = T) # can also use bptest(slr) as 'studendize' will be 'true' by default

# Auto correlation in errors: Durbin-Watson Test and Breusch-Godfrey test for first-order autocorrelation

dwtest(slr1)

bgtest(slr1,order = 1) # order=1 implies of lag of 1 time period

# Prediction with SLR

Predict1<-predict(slr1,test2)
summary(Predict1)
head(Predict1)

plot(index(test2), test2$portfolio.returns, xlab="index", ylab="Returns",
     main="Predicted vs Actual Returns", col="red" ,lwd=1, type="l")

lines(index(test), Predict1, col="blue", type="l")

legend("topleft", c("Actual Return", "Predicted Returns"),
       fill=c("red", "blue"))
cor(test2$portfolio.returns,Predict1)

# Naive model

Naive1= lm(portfolio.returns ~ 1, data= train2)#no independent variable
summary(Naive1)

Predict_Naive1= predict(Naive1, test2)

# Make a dataframe

Error1= data.frame(matrix(nrow=4, ncol=2))
colnames(Error1)=c("Naive", "Market")
rownames(Error1)=c("RMSE", "SMAPE", "RMSLE", "Average")

Error1$Naive[1]=rmse(Predict_Naive1,test2$portfolio.returns)
Error1$Market[1]=rmse(Predict1,test2$portfolio.returns)

Error1$Naive[2]=smape(Predict_Naive1,test2$portfolio.returns )
Error1$Market[2]=smape(Predict1,test2$portfolio.returns )

Error1$Naive[3]=rmsle(Predict_Naive1,test2$portfolio.returns )
Error1$Market[3]=rmsle(Predict,test2$portfolio.returns )

Error1$Naive[4]=mean(Error1$Naive[1:3])
Error1$Market[4]=mean(Error1$Market[1:3])

Error1

# Create 'updown' variable, 1 for positive return and 0 for negative return of Portfolio return

data4<-data3 |> mutate(updown = ifelse(portfolio.returns>0, 1, 0))

# Create training data and test data

set.seed(11)
indd1 = sample.split(Y = data4$updown, SplitRatio = 0.8)

# Training data

train3 = data4[indd1,]
dim(train3)
head(train3)

# Test data

test3 = data4[!indd1,]
dim(test3)
head(test3)

# Check distibution of Updown variable

prop.table(table(data4$updown))*100
prop.table(table(test3$updown))*100
prop.table(table(train3$updown))*100

# Linear model

linear1=lm(updown~Nifty_adj,data = train3)
summary(linear1)

linear1$fitted.values#view the fitted value of train1

# Threshold of 0.4

fitted.results<-ifelse(linear1$fitted.values>0.4,1,0)#find fitted in data= train1
CM_linear4R<-confusionMatrix(as.factor(fitted.results),as.factor(train3$updown))
print(CM_linear4R)
Performance_4R= tibble(Threshold= 0.4, Accuracy= CM_linear4R$overall["Accuracy"],
                      Sensitivity=CM_linear4R$byClass["Sensitivity"],
                      Specificity=CM_linear4R$byClass["Specificity"])

# Threshold of 0.5

fitted.results<-ifelse(linear1$fitted.values>0.5,1,0)#find fitted in data= train1
CM_linear5R<-confusionMatrix(as.factor(fitted.results),as.factor(train3$updown))
print(CM_linear5R)
Performance_5R= tibble(Threshold= 0.5, Accuracy= CM_linear5R$overall["Accuracy"],
                      Sensitivity=CM_linear5R$byClass["Sensitivity"],
                      Specificity=CM_linear5R$byClass["Specificity"])

# Threshold of 0.6

fitted.results=ifelse(linear1$fitted.values>0.6, 1, 0)
CM_linear6R=confusionMatrix(as.factor(fitted.results), as.factor(train3$updown))
print(CM_linear6R)
Performance_6R= tibble(Threshold= 0.6, Accuracy= CM_linear6R$overall["Accuracy"],
                      Sensitivity=CM_linear6R$byClass["Sensitivity"],
                      Specificity=CM_linear6R$byClass["Specificity"])

# Threshold of 0.7

fitted.results=ifelse(linear1$fitted.values>0.7, 1, 0)
CM_linear7R=confusionMatrix(as.factor(fitted.results), as.factor(train3$updown))
print(CM_linear7R)
Performance_7R= tibble(Threshold= 0.7, Accuracy= CM_linear7R$overall["Accuracy"],
                      Sensitivity=CM_linear7R$byClass["Sensitivity"],
                      Specificity=CM_linear7R$byClass["Specificity"])

Linear_Performance1= rbind(Performance_4R, Performance_5R, Performance_6R, Performance_7R)
Linear_Performance1$Class="Linear"
Linear_Performance1

# Logit model 

logit1=glm(formula(linear),data = train3,family = binomial("logit"))
summary(logit1)
margins(logit1)

logit_naive1<-glm(updown ~ 1,data = train3,family = binomial("logit"))
pseudoRsqr2<- 1-(logLik(logit1)/logLik(logit_naive1))
pseudoRsqr2

logit1$fitted.values

# Threshold of 0.4

fitted.results=ifelse(logit1$fitted.values>0.4, 1, 0)
CM_logit4R=confusionMatrix(as.factor(fitted.results), as.factor(train3$updown))
print(CM_logit4R)
Performance_4R= tibble(Threshold= 0.4, Accuracy= CM_logit4R$overall["Accuracy"],
                      Sensitivity=CM_logit4R$byClass["Sensitivity"],
                      Specificity=CM_logit4R$byClass["Specificity"])

# Threshold of 0.5

fitted.results=ifelse(logit1$fitted.values>0.5, 1, 0)
CM_logit5R=confusionMatrix(as.factor(fitted.results), as.factor(train3$updown))
print(CM_logit5R)
Performance_5R= tibble(Threshold= 0.5, Accuracy= CM_logit5R$overall["Accuracy"],
                      Sensitivity=CM_logit5R$byClass["Sensitivity"],
                      Specificity=CM_logit5R$byClass["Specificity"])

# Threshold of 0.6

fitted.results=ifelse(logit1$fitted.values>0.6, 1, 0)
CM_logit6R=confusionMatrix(as.factor(fitted.results), as.factor(train3$updown))
print(CM_logit6R)
Performance_6R= tibble(Threshold= 0.6, Accuracy= CM_logit6R$overall["Accuracy"],
                      Sensitivity=CM_logit6R$byClass["Sensitivity"],
                      Specificity=CM_logit6R$byClass["Specificity"])

# Threshold of 0.7

fitted.results=ifelse(logit1$fitted.values>0.7, 1, 0)
CM_logit7R=confusionMatrix(as.factor(fitted.results), as.factor(train3$updown))
print(CM_logit7R)
Performance_7R= tibble(Threshold= 0.7, Accuracy= CM_logit7R$overall["Accuracy"],
                      Sensitivity=CM_logit7R$byClass["Sensitivity"],
                      Specificity=CM_logit7R$byClass["Specificity"])


Logit_Performance1= rbind(Performance_4R, Performance_5R, Performance_6R, Performance_7R)
Logit_Performance1$Class="Logit"
Logit_Performance1

# Probit model

probit1=glm(formula(linear),data = train3,family = binomial("probit"))
summary(probit1)
margins(probit) 

probit_naive1<-glm(updown ~ 1,data = train3,family = binomial("probit"))
pseudoRsqr3<- 1-(logLik(probit1)/logLik(probit_naive1))
pseudoRsqr3

probit1$fitted.values

# Threshold of 0.4

fitted.results=ifelse(probit1$fitted.values>0.4, 1, 0)
CM_probit4R=confusionMatrix(as.factor(fitted.results), as.factor(train3$updown))
print(CM_probit4R)
Performance_4R= tibble(Threshold= 0.4, Accuracy= CM_probit4R$overall["Accuracy"],
                      Sensitivity=CM_probit4R$byClass["Sensitivity"],
                      Specificity=CM_probit4R$byClass["Specificity"])

# Threshold of 0.5

fitted.results=ifelse(probit1$fitted.values>0.5, 1, 0)
CM_probit5R=confusionMatrix(as.factor(fitted.results), as.factor(train3$updown))
print(CM_probit5R)
Performance_5R= tibble(Threshold= 0.5, Accuracy= CM_probit5R$overall["Accuracy"],
                      Sensitivity=CM_probit5R$byClass["Sensitivity"],
                      Specificity=CM_probit5R$byClass["Specificity"])

# Threshold of 0.6

fitted.results=ifelse(probit1$fitted.values>0.6, 1, 0)
CM_probit6R=confusionMatrix(as.factor(fitted.results), as.factor(train3$updown))
print(CM_probit6R)
Performance_6R= tibble(Threshold= 0.6, Accuracy=CM_probit6R$overall["Accuracy"],
                      Sensitivity=CM_probit6R$byClass["Sensitivity"],
                      Specificity=CM_probit6R$byClass["Specificity"])

# Threshold of 0.7

fitted.results=ifelse(probit1$fitted.values>0.7, 1, 0)
CM_probit7R=confusionMatrix(as.factor(fitted.results), as.factor(train3$updown))
print(CM_probit7R)
Performance_7R= tibble(Threshold= 0.7, Accuracy= CM_probit7R$overall["Accuracy"],
                      Sensitivity=CM_probit7R$byClass["Sensitivity"],
                      Specificity=CM_probit7R$byClass["Specificity"])


Probit_Performance1= rbind(Performance_4R, Performance_5R, Performance_6R,Performance_7R)
Probit_Performance1$Class="Probit"
Probit_Performance1

# Model performance

Perfomance1<-tibble(rbind.data.frame(Linear_Performance1,Logit_Performance1,Probit_Performance1))
Perfomance1

P1<-ggplot(Perfomance1,aes(factor(Threshold),Accuracy))+geom_point(aes(colour=Class))+
  xlab('Threshold')+ylab('Specificity')
P2<-ggplot(Perfomance1,aes(factor(Threshold),Accuracy))+geom_point(aes(colour=Class))+
  xlab('Threshold')+ylab('Sensitivity')
P3<-ggplot(Perfomance1,aes(factor(Threshold),Accuracy))+geom_point(aes(colour=Class))+
  xlab('Threshold')+ylab('Accuracy')
P1/P2/P3 #library patchwork

# ROC

par(mfrow=c(3,1), bg="lightyellow")#set no figures in a single plot and background colour)

# Linear model

pr<- ROCR::prediction(linear1$fitted.values,train3$updown)
prf<-performance(pr, measure = "tpr",x.measure = "fpr")
plot(prf)
abline(0,1, lwd=2, lty=2)

auc<-performance(pr, measure = "auc") # Area under curve (above the 0.5 line)
auc@y.values[1]

# Logit model

pr<-ROCR::prediction(logit1$fitted.values,train3$updown)
prf<-performance(pr, measure = "tpr",x.measure = "fpr")
plot(prf)
abline(0,1, lwd=2, lty=2)

auc<-performance(pr, measure = "auc") # Area under curve (above the 0.5 line)
auc@y.values[1]

# Probit model

pr<-ROCR::prediction(probit1$fitted.values,train3$updown)
prf<-performance(pr, measure = "tpr",x.measure = "fpr")
plot(prf)
abline(0,1, lwd=2, lty=2)

auc<-performance(pr, measure = "auc") # Area under curve (above the 0.5 line)
auc@y.values[1]

