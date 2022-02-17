library(readxl)
library(fpp3)
library(fpp2)
library(fable)

# ----FHFA index data-----

# fhfa_indexdata <- read_excel("~/Downloads/FHFA_HPI.xls", skip = 10)
# 
# #fhfa_indexdata <- fhfa_indexdata[,c(1,20)]
# fhfa_indexdata <- clean_names(fhfa_indexdata)
# fhfa_indexdata$observation_date <- as.Date(fhfa_indexdata$observation_date,format = "%Y %b")
# fhfa_tsibble <- as_tsibble(fhfa_indexdata, index = observation_date)
# fhfa_tsibble <- fhfa_indexdata %>% mutate(observation_date = yearmonth(observation_date)) %>% as_tsibble(index = observation_date)
# fhfa_ts <- ts(fhfa_tsibble[,-1], start = c(1992, 1), end = c(2021,9), frequency = 12)
# rm(fhfa_indexdata)
# 
# hpi.train.tsibble <- fhfa_tsibble %>% filter(year(observation_date) <= 2015)
# #fhfa_train <- window(fhfa_ts, start = c(1992,1), end = c(2016,1))

#------Median House price data-----
pricedata <- read_excel("~/Downloads/House_Price_1970.xls", skip = 10)

pricedata <- pricedata[,c(1,4)]
pricedata <- na.omit(pricedata)
pricedata$observation_date <- as.Date(pricedata$observation_date)
price_tsibble <- as_tsibble(pricedata, index = observation_date)
price_tsibble <- pricedata %>% mutate(observation_date = yearquarter(observation_date)) %>% as_tsibble(index = observation_date)
price_ts <- ts(price_tsibble[,-1], start = c(1970, 1), end = c(2021,3), frequency = 4)
rm(pricedata)

# price.train.tsibble <- price_tsibble %>% filter(year(observation_date) <= 2015)
# price.test.tsibble <- price_tsibble %>% filter(year(observation_date) > 2015)
#------housing starts data---------
startsdata <- read_excel("~/Downloads/House_Starts_NSA.xls", skip = 10)

#Clean up starts data
startsdata$observation_date <- as.Date(startsdata$observation_date)
#Make a tsibble of it
starts_tsibble <- as_tsibble(startsdata, index = observation_date)
starts_tsibble <- starts_tsibble %>% mutate(observation_date = yearmonth(observation_date)) %>% 
  as_tsibble(index = observation_date)
starts_ts <- ts(starts_tsibble[,-1], start = c(1991, 1), end = c(2021,9), frequency = 12)
rm(startsdata)
starts.train.tsibble <- starts_tsibble %>% filter(year(observation_date) <= 2015)

#------ house supply data -----------
supplydata <- read_excel("~/Downloads/Houses_Supply_NSA.xls", skip = 10)
#clean it up
supplydata$observation_date <- as.Date(supplydata$observation_date)
#Make a tsibble of it
supply_tsibble <- as_tsibble(supplydata, index = observation_date)
supply_tsibble <- supplydata %>% mutate(observation_date = as.Date(observation_date, format = "%Y %b")) %>% 
  as_tsibble(index = observation_date)
supply_ts <- ts(supply_tsibble[,-1], start = c(1991, 1), end = c(2021,10), frequency = 12)
rm(supplydata)
supply.train.tsibble <- supply_tsibble %>% filter(year(observation_date) <= 2015)

# --------------------
# ------ EDA ---------
# --------------------
tsdisplay(price_ts)
tsdisplay(starts_ts)
tsdisplay(train_d)
tsdisplay(supply_ts)

autoplot(price_tsibble, .vars = adj_price) +
  xlab("") + ylab("Median Price") + 
  ggtitle("Median House Price, Adjusted for Inflation") +
  scale_x_yearquarter(breaks = "4 years") +
  theme(axis.text.x = element_text(angle=60))

# 
# autoplot(starts_tsibble) + 
#   xlab("") + ylab("Thousands of Units") + 
#   ggtitle("New Housing Units Started in the US, Total Units") + 
#   # scale_x_yearmonth(breaks = as.Date(
#   #   c("2008-01-01","2010-01-01", "2015-01-01", "2016-01-01", "2020-01-01", "2021-01-01"))) +
#   theme(axis.text.x = element_text(angle=60)) +
#   scale_x_yearmonth(date_breaks = "1 year")
# 
# autoplot(supply_tsibble) + 
#   xlab("") + ylab("Month's Supply") + 
#   ggtitle("Monthly Supply of Houses in the US") +
#   scale_x_yearmonth(breaks = as.Date(c("2010-01-01", "2015-01-01", "2020-01-01", "2021-01-01"))) +
#   theme(axis.text.x = element_text(angle=60))
# 
# 
# 
# cbind("Months' supply of houses" = supply_ts, "House Price Index" = fhfa_ts,
#       "Housing starts (1000s of units)" = starts_ts) %>%
#   autoplot(facets=TRUE) +
#   xlab("Year") + ylab("") +
#   ggtitle("Measuring the health of the US housing market")





gglagplot(supply_ts)
gglagplot(starts_ts)
gglagplot(price_tsibble$adj_price)
ggseasonplot(starts_ts)
ggseasonplot(supply_ts)
ggseasonplot(price_ts)
ggsubseriesplot(supply_ts)
ggsubseriesplot(price_ts)
ggsubseriesplot(starts_ts)

#-----Decomposed-----

#-----X11 decomposition-----
#price data
price_ts %>% seas(x11="") -> pricefit 
autoplot(pricefit) + ggtitle("X11 decomposition of median house price, inflation-adjusted")


autoplot(price_ts, series="Data") + 
  autolayer(trendcycle(pricefit), series="Trend") + 
  autolayer(seasadj(pricefit), series="Seasonally Adjusted") + 
  xlab("Year") + ylab("Median Price") + 
  ggtitle("Median House Price in US") + 
  scale_colour_manual(values=c("gray","blue","red"), breaks=c("Data","Seasonally Adjusted","Trend"))


ndiffs(price_ts)


#----Supply data----
supply_ts %>% seas(x11="") -> supplyfit 
autoplot(supplyfit) + ggtitle("X11 decomposition of housing unit supply, in month's supply")


autoplot(supply_ts, series="Data") + 
  autolayer(trendcycle(supplyfit), series="Trend") + 
  autolayer(seasadj(supplyfit), series="Seasonally Adjusted") + 
  xlab("Year") + ylab("Months' Supply Available") + 
  ggtitle("Monthly Housing Supply") + 
  scale_colour_manual(values=c("gray","blue","red"), breaks=c("Data","Seasonally Adjusted","Trend"))

#-----Housing starts data-----  
breaks=c("Data","Seasonally Adjusted","Trend"))
starts_ts %>% seas(x11="") -> startsfit 
autoplot(startsfit) + ggtitle("X11 decomposition of housing unit starts, in thousands of units")

autoplot(starts_ts, series="Data") + 
  autolayer(trendcycle(startsfit), series="Trend") + 
  autolayer(seasadj(startsfit), series="Seasonally Adjusted") + 
  xlab("Year") + ylab("Thousands of Units") + 
  ggtitle("Housing Construction Projects Started") + 
  scale_colour_manual(values=c("gray","blue","red"), breaks=c("Data","Seasonally Adjusted","Trend"))


#----------------------------


#-----Trying out models-----

#----ETS----

price_train <- ts(price_tsibble[,-1], start = c(1970, 1), end = c(2004,4), frequency = 4)
price_test <-  ts(price_tsibble[,-1], start = c(2005, 1), end = c(2021,3), frequency = 4)




lambda.price <- BoxCox.lambda(price_train)

ets.price <- ets(price_train, model="MZZ", damped=T, alpha=NULL,
               beta=NULL, gamma=NULL, phi=NULL, lambda=lambda.price,
               biasadj=T, additive.only=NULL, restrict=F,
               allow.multiplicative.trend=T)

summary(ets.price)
checkresiduals(ets.price)

ets.price.fc <- ets.price %>% forecast(h= 24)

ets.price.fc %>% forecast(h= 16) %>%
  autoplot() +
  labs(title = "US Inflation-adjusted Median House Price", y = "USD") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_y_continuous(labels=comma)


accuracy(ets.price.fc, price_test)  




#----------------------------

#-----Differencing------
#does our data need to be differenced for the ARIMA model we're gonna use?

library(urca)
price_train %>% ur.kpss() %>% summary()   #is it stationary?

#how many differences are needed?
ndiffs(price_train)
nsdiffs(price_train)


#-----Differencing------
library(seasonal)

train_d <- price_train %>% diff(lag = 2, differences = 1)

autoplot(train_d)




#taking first AND seasonal difference 
#supply
cbind("Month's supply" = supply_ts, "Logs" = log(supply_ts),
      "Seasonally\n differenced logs" = diff(log(supply_ts),12),
      "Doubly\n differenced logs" = diff(diff(log(supply_ts),12),1)) %>%
  autoplot(facets=T) +      #facets=T gives you three separate graphs insteaf of all lines on one
  xlab("Year") + ylab("") +
  ggtitle("Monthly US Housing Supply")



#-----Modeling ARIMA for price-----

# fit1 <- Arima(price_diff, order=c(0,1,3), seasonal=c(0,1,2))
# checkresiduals(fit1)
# 
fit1 <- auto.arima(price_train, stepwise = F)
summary(fit1)
checkresiduals(fit1)

# fit2 <- Arima(price_train, order = c(2,1,0), seasonal = list(order = c(1,0,1), period = 2))  #winner for non logged
# summary(fit2)
# checkresiduals(fit2)

#not logged!
fit5 <- Arima(price_train, order = c(2,1,0), seasonal = list(order = c(1,0,1), period = 4))  #winner for non logged
summary(fit5)
checkresiduals(fit5) #residuals are independent

# 3 tells us that we need to take into account the Y value at 2 lags from a given time point t.
# 1 tells us that the time series is not stationary, so we need to take a first-order difference.
# 0 tells us that this model takes into account the error term from 0 preceding/lagged values.

arima.fc <- fit5 %>% forecast(h=40)
arima.fc %>%
  autoplot() +
  labs(title="Median US House Price, Inflation-Adjusted",y="Price") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_y_continuous(labels = comma)

accuracy(arima.fc, price_test) 



# rwd.fc <- fit1 %>% forecast(h=39)
# rwd.fc %>%
#   autoplot() +
#   labs(title="Median US House Price, Inflation-Adjusted",y="log Price") +
#   guides(colour = guide_legend(title = "Forecast"))
# 
# accuracy(rwd.fc, price_test) 



#-----logged model------
# fit4 <- Arima(log(price_train), order = c(3,1,0), seasonal = list(order = c(2,0,0), period = 4)) #winner
# summary(fit4)
# checkresiduals(fit4)
# 
# 
# autoplot(log(price_train))






#----ARIMA for housing supply-----
supply_train <- ts(supply_tsibble[,-1], start = c(1991, 1), end = c(2015,12), frequency = 12)
supply_test <-  ts(supply_tsibble[,-1], start = c(2016, 1), end = c(2021,10), frequency = 12)

supplyfit1 <- auto.arima(price_diff, stepwise = F)
summary(supplyfit1)



#-----holt-winter forecast-----
supplysub <- window(supply_ts, start=2015)
supplyfit <- hw(supplysub,seasonal="additive")
supplyfit <- hw(supplysub,seasonal="multiplicative", damped = T) 
autoplot(supplysub) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) + 
  autolayer(fit2, series="HW multiplicative forecasts",PI=FALSE) +
  xlab("Year") + ylab("Months' Supply") + 
  ggtitle("Monthly Housing Supply") + guides(colour=guide_legend(title="Forecast"))

rm(fit1,fit2)


#-----Forecasting------

#training
arima.fc <- fit5 %>% forecast(h=24)
arima.fc %>%
  autoplot() +
  labs(title="Median US House Price, Inflation-Adjusted",y="Price") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_y_continuous(labels=comma)

accuracy(arima.fc, log(price_test)) 








#evaluating performance
autoplot(price_ts) +
  autolayer(ets.price.fc, series="ETS", PI=FALSE) +
  autolayer(arima.fc, series="SARIMA", PI=FALSE) +
  scale_y_continuous(labels=comma) +
  labs(title="Evaluating Forecast Accuracy:\nMedian US House Price",y="Price")





#------forecasting into the future------
wholefit <- auto.arima(price_ts, stepwise = F)

wholefit <- Arima(price_ts, order = c(2,1,0), seasonal = list(order = c(1,0,1), period = 4)) 
summary(wholefit)
checkresiduals(wholefit)
autoplot(price_ts)


lambda.whole <- BoxCox.lambda(price_ts)

whole.ets <- ets(price_ts, model="AAA", damped=T, alpha=NULL,
                 beta=NULL, gamma=NULL, phi=NULL, lambda=lambda.whole,
                 biasadj=T, additive.only=NULL, restrict=F,
                 allow.multiplicative.trend=T)

summary(whole.ets)
checkresiduals(whole.ets)
#whole dataset
whole.arima.fc <- wholefit %>% forecast(h=24)
whole.arima.fc %>% 
  autoplot() +
  labs(title="ARIMA(2,1,0)(1,0,1)[4] Forecast\nMedian US House Price, Inflation-Adjusted", y="Price") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_y_continuous(labels=comma)

#whole dataset
whole.ets.fc <- whole.ets %>% forecast(h=24)
whole.ets.fc %>% 
  autoplot() +
  labs(title="ETS(A,Ad,A) Forecast of Median US House Price", y="Price") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_y_continuous(labels=comma)






autoplot(price_ts) +
  autolayer(whole.arima.fc, series="SARIMA", PI=FALSE) +
  autolayer(whole.ets.fc, series="ETS", PI=FALSE) +
  scale_y_continuous(labels=comma) +
  labs(title="Forecast of Median US House Price", y="Price")


#-----STLF-----
# stl.price <- stl(price_train, t.window = 2, s.window = "periodic",
#            robust=TRUE)
# stl.fc <- stl.price %>% forecast(method="rwdrift") %>%
#   autoplot() + ylab("Median price")
# stl.fc
# 
# accuracy(stl.fc, price_test)
# checkresiduals