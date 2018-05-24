library("dplyr")
library("ggplot2")
library("tseries")
library("forecast")

#Read the Superstore file
global <- read.csv("Global Superstore.csv")
str(global)

#Check for NA's

# Check for duplicates


#lets convert the order date to date formate and extract the year and month from the date
global$Order.Date <- as.Date(global$Order.Date,format = "%d-%m-%Y")
global$Order.Date.Month <- format(global$Order.Date,"%Y-%m")

#lets check the different Market and Segments. We should have told 21 market segments 
levels(global$Market)
levels(global$Segment)

#After we combine Market and segment we get 21 unique market segments
global$marketsegment <- as.factor(paste(global$Segment,global$Market,sep = '_'))
levels(global$marketsegment)

# Now lets calculate the mostprofitable market segements from the 21 available
most_profitable <- global %>% group_by(marketsegment = marketsegment) %>% summarize(Sales = sum(Sales),Quantity = sum(Quantity),Profit = sum(Profit))

#Visually represent to see the most profitable marketsegments
bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5), 
                  legend.position="none")
ggplot(most_profitable,aes(x=marketsegment,Profit)) + geom_bar(stat = "identity") + bar_theme  
# we clearly see Consumer_Apac = 222817.560
# and Consumer_EU = 188687.707 are the 2 most profitable marketsegments

# Raster library helps us to find coeff of variations so that we can identify the consistent profitable segments
library("raster")
str(most_profitable)

most_profitable_coeffvariation <- global %>% 
  group_by(marketsegment = marketsegment, Month = Order.Date.Month) %>% 
  summarize(M.Profit = sum(Profit))

# cv() is the function from library raster which provides us the coefficient of variation
coeffofvar <- aggregate(M.Profit ~ marketsegment,most_profitable_coeffvariation,function(x){
  Coeff = cv(x)
})
ggplot(coeffofvar,aes(x=marketsegment,M.Profit)) + geom_bar(stat = "identity") + bar_theme

##Consumer_EU = 62.43052,Consumer_APAC = 63.21323
# We choose Consumer_EU and Consumer_APAC as they have least variation 


########################################Consumer_EU Sales ###################################
# Filter the global dataset for Consumer_EU
Consumer_EU_Sales <- filter(global,global$marketsegment == 'Consumer_EU')
#Aggregate the total sales for each month
Consumer_EU_Sales_agg <- Consumer_EU_Sales %>% 
  group_by(Month = Order.Date.Month) %>%
  summarize(Sales = sum(Sales))  

Consumer_EU_Sales_agg$Month <- gsub("-","",Consumer_EU_Sales_agg$Month)
timevals_in <- 0
#Sequencing the months 
for (i in 1:nrow(Consumer_EU_Sales_agg)){
  timevals_in[i] <- i
}
Consumer_EU_Sales_agg$timevals_in <- timevals_in
indata <- Consumer_EU_Sales_agg[1:42,]
# converting to timeseries and plotting the timeseries 
Consumer_EU_Sales_agg_ts <- ts(indata$Sales)                         
plot(Consumer_EU_Sales_agg_ts)


#Smoothing the series - Moving Average Smoothing
w <-1
Consumer_EU_Sales_smoothedseries <- stats::filter(Consumer_EU_Sales_agg_ts, 
                                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                                  method='convolution', sides=2)

#Smoothing left end of the time series

diff <- Consumer_EU_Sales_smoothedseries[w+2] - Consumer_EU_Sales_smoothedseries[w+1]
for (i in seq(w,0,-1)) {
  Consumer_EU_Sales_smoothedseries[i] <- Consumer_EU_Sales_smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(Consumer_EU_Sales_agg_ts)
diff <- Consumer_EU_Sales_smoothedseries[n-w] - Consumer_EU_Sales_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  Consumer_EU_Sales_smoothedseries[i] <- Consumer_EU_Sales_smoothedseries[i-1] + diff
}

#Plot the smoothed time series

lines(Consumer_EU_Sales_smoothedseries, col="red", lwd=2)

#######################Classical Decomposition Consumer_EU SALES####################################################
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

Consumer_EU_Sales_smootheddf <- as.data.frame(cbind(timevals_in[1:42], as.vector(Consumer_EU_Sales_smoothedseries)))
colnames(Consumer_EU_Sales_smootheddf) <- c('Month', 'Sales')

Consumer_EU_Sales_smootheddf$Sales <- as.numeric(Consumer_EU_Sales_smootheddf$Sales)
lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
            + Month, data=Consumer_EU_Sales_smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)

lines(timevals_in[1:42], global_pred, col='blue', lwd=2)

local_pred <- Consumer_EU_Sales_agg_ts-global_pred

plot(local_pred, col='blue', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- Consumer_EU_Sales_agg[43:48,]

timevals_out <- outdata$timevals_in

str(outdata)
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales)
MAPE_class_dec

# we got MAPE = 29.09711

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(Consumer_EU_Sales_smoothedseries, col = "black")
lines(class_dec_pred, col = "red")

### We also have to predict for month from 49 to 54
a = 49
timevals_out2 = vector()
for (i in 1:6){
  timevals_out2[i] <- a
  a = a + 1
}

global_pred_out2 <- predict(lmfit,data.frame(Month = timevals_out2))
summary(global_pred_out2)
# these are our Sales predictions for the next 6 months
global_pred_out2

#######################END Classical Decomposition Consumer_EU SALES####################################################


####################### ARIMA Consumer_EU SALES######################################################################

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(Consumer_EU_Sales_agg_ts)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- Consumer_EU_Sales_agg_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 12)

#We compare the 1st 6 predecited values with the outdata$Sales to find the MAPE. we also have the next 6 months predicted from 49:54
MAPE_auto_arima <- accuracy(fcast_auto_arima[1:6]$pred,outdata$Sales)
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Consumer_EU_Sales_agg_ts, col = "black")
lines(auto_arima_pred, col = "red")


##############################END ARIMA Consumer_EU SALES#############################################################

##############################Consumer_EU Quantity###############################
########****************************

str(Consumer_EU_Sales)
Consumer_EU_Quantity <- Consumer_EU_Sales %>% group_by(Month = Order.Date.Month) %>% summarize(Quantity = sum(Quantity))
Consumer_EU_Quantity$Month <- gsub("-","",Consumer_EU_Quantity$Month)                                                                                       
Consumer_EU_Quantity$timevals_in <- timevals_in

indata_1 <- ts(Consumer_EU_Quantity[1:42,]$Quantity)
Consumer_EU_Quantity_ts <- indata_1
plot(Consumer_EU_Quantity_ts)
w <-1
Consumer_EU_Quantity_smoothedseries <- stats::filter(indata_1, 
                                                     filter=rep(1/(2*w+1),(2*w+1)), 
                                                     method='convolution', sides=2)

#Smoothing left end of the time series

diff <- Consumer_EU_Quantity_smoothedseries[w+2] - Consumer_EU_Quantity_smoothedseries[w+1]
for (i in seq(w,0,-1)) {
  Consumer_EU_Quantity_smoothedseries[i] <- Consumer_EU_Quantity_smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(Consumer_EU_Quantity_smoothedseries)
diff <- Consumer_EU_Quantity_smoothedseries[n-w] - Consumer_EU_Quantity_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  Consumer_EU_Quantity_smoothedseries[i] <- Consumer_EU_Quantity_smoothedseries[i-1] + diff
}

#Plot the smoothed time series

lines(Consumer_EU_Quantity_smoothedseries, col="red", lwd=2)


Consumer_EU_Quantity_smootheddf <- as.data.frame(cbind(timevals_in[1:42], as.vector(Consumer_EU_Quantity_smoothedseries)))
colnames(Consumer_EU_Quantity_smootheddf) <- c('Month', 'Quantity')

Consumer_EU_Quantity_smootheddf$Quantity <- as.numeric(Consumer_EU_Quantity_smootheddf$Quantity)

##############################Classical Decomposition Consumer_EU Quantity###############################

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
            + Month, data=Consumer_EU_Quantity_smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)

lines(timevals_in[1:42], global_pred, col='blue', lwd=2)

local_pred <- Consumer_EU_Quantity_ts-global_pred

plot(local_pred, col='blue', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- Consumer_EU_Quantity[43:48,]

timevals_out <- outdata$timevals_in

str(outdata)
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Quantity)
MAPE_class_dec

# we got MAPE = 29.09711

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(Consumer_EU_Quantity_smoothedseries, col = "black")
lines(class_dec_pred, col = "red")

### We also have to predict for month from 49 to 54
a = 49
timevals_out2 = vector()
for (i in 1:6){
  timevals_out2[i] <- a
  a = a + 1
}

global_pred_out2 <- predict(lmfit,data.frame(Month = timevals_out2))

global_pred_out2
##############################End Classical Decomposition Consumer_EU Quantity###############################

####################### ARIMA Consumer_EU Quantity######################################################################

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(Consumer_EU_Quantity_ts)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- Consumer_EU_Quantity_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Quantity)
MAPE_auto_arima

#MAPE 30.13319

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Consumer_EU_Quantity_ts, col = "black")
lines(auto_arima_pred, col = "red")


##############################END ARIMA Consumer_EU Quantity#############################################################
#************************************************************************************************************************

#****************************APAC*************************************************************

Consumer_APAC_Sales <- filter(global,global$marketsegment == 'Consumer_APAC')
Consumer_APAC_Sales_agg <- Consumer_APAC_Sales %>% 
  group_by(Month = Order.Date.Month) %>%
  summarize(Sales = sum(Sales))  
Consumer_APAC_Sales_agg$Month <- gsub("-","",Consumer_APAC_Sales_agg$Month)
timevals_in <- 0
for (i in 1:nrow(Consumer_APAC_Sales_agg)){
  timevals_in[i] <- i
}
Consumer_APAC_Sales_agg$timevals_in <- timevals_in
indata <- Consumer_APAC_Sales_agg[1:42,]
Consumer_APAC_Sales_agg_ts <- ts(indata$Sales)                         
plot(Consumer_APAC_Sales_agg_ts)


#Smoothing the series - Moving Average Smoothing

w <-1
Consumer_APAC_Sales_smoothedseries <- stats::filter(Consumer_APAC_Sales_agg_ts, 
                                                    filter=rep(1/(2*w+1),(2*w+1)), 
                                                    method='convolution', sides=2)

#Smoothing left end of the time series

diff <- Consumer_APAC_Sales_smoothedseries[w+2] - Consumer_APAC_Sales_smoothedseries[w+1]
for (i in seq(w,0,-1)) {
  Consumer_APAC_Sales_smoothedseries[i] <- Consumer_APAC_Sales_smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(Consumer_APAC_Sales_agg_ts)
diff <- Consumer_APAC_Sales_smoothedseries[n-w] - Consumer_APAC_Sales_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  Consumer_APAC_Sales_smoothedseries[i] <- Consumer_APAC_Sales_smoothedseries[i-1] + diff
}

#Plot the smoothed time series

lines(Consumer_APAC_Sales_smoothedseries, col="red", lwd=2)

#######################Classical Decomposition Consumer_APAC SALES####################################################
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

Consumer_APAC_Sales_smootheddf <- as.data.frame(cbind(timevals_in[1:42], as.vector(Consumer_APAC_Sales_smoothedseries)))
colnames(Consumer_APAC_Sales_smootheddf) <- c('Month', 'Sales')

Consumer_APAC_Sales_smootheddf$Sales <- as.numeric(Consumer_APAC_Sales_smootheddf$Sales)
lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
            + Month, data=Consumer_APAC_Sales_smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)

lines(timevals_in[1:42], global_pred, col='blue', lwd=2)

local_pred <- Consumer_APAC_Sales_agg_ts-global_pred

plot(local_pred, col='blue', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- Consumer_APAC_Sales_agg[43:48,]

timevals_out <- outdata$timevals_in

str(outdata)
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales)
MAPE_class_dec

# we got MAPE = 26.89676

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(Consumer_APAC_Sales_smoothedseries, col = "black")
lines(class_dec_pred, col = "red")

### We also have to predict for month from 49 to 54
a = 49
timevals_out2 = vector()
for (i in 1:6){
  timevals_out2[i] <- a
  a = a + 1
}

global_pred_out2 <- predict(lmfit,data.frame(Month = timevals_out2))
summary(global_pred_out2)


#######################END Classical Decomposition Consumer_APAC SALES####################################################


####################### ARIMA Consumer_APAC SALES######################################################################

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(Consumer_APAC_Sales_agg_ts)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- Consumer_APAC_Sales_agg_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Sales)
MAPE_auto_arima

# 27.68952
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Consumer_APAC_Sales_agg_ts, col = "black")
lines(auto_arima_pred, col = "red")


##############################END ARIMA Consumer_APAC SALES#############################################################

##############################Consumer_APAC Quantity###############################
########****************************

str(Consumer_APAC_Sales)
Consumer_APAC_Quantity <- Consumer_APAC_Sales %>% group_by(Month = Order.Date.Month) %>% summarize(Quantity = sum(Quantity))
Consumer_APAC_Quantity$Month <- gsub("-","",Consumer_APAC_Quantity$Month)                                                                                       
Consumer_APAC_Quantity$timevals_in <- timevals_in

indata_1 <- ts(Consumer_APAC_Quantity[1:42,]$Quantity)
Consumer_APAC_Quantity_ts <- indata_1
plot(Consumer_APAC_Quantity_ts)
w <-1
Consumer_APAC_Quantity_smoothedseries <- stats::filter(indata_1, 
                                                       filter=rep(1/(2*w+1),(2*w+1)), 
                                                       method='convolution', sides=2)

#Smoothing left end of the time series

diff <- Consumer_APAC_Quantity_smoothedseries[w+2] - Consumer_APAC_Quantity_smoothedseries[w+1]
for (i in seq(w,0,-1)) {
  Consumer_APAC_Quantity_smoothedseries[i] <- Consumer_APAC_Quantity_smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(Consumer_APAC_Quantity_smoothedseries)
diff <- Consumer_APAC_Quantity_smoothedseries[n-w] - Consumer_APAC_Quantity_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  Consumer_APAC_Quantity_smoothedseries[i] <- Consumer_APAC_Quantity_smoothedseries[i-1] + diff
}

#Plot the smoothed time series

lines(Consumer_APAC_Quantity_smoothedseries, col="red", lwd=2)


Consumer_APAC_Quantity_smootheddf <- as.data.frame(cbind(timevals_in[1:42], as.vector(Consumer_APAC_Quantity_smoothedseries)))
colnames(Consumer_APAC_Quantity_smootheddf) <- c('Month', 'Quantity')

Consumer_APAC_Quantity_smootheddf$Quantity <- as.numeric(Consumer_APAC_Quantity_smootheddf$Quantity)

##############################Classical Decomposition Consumer_APAC Quantity###############################

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
            + Month, data=Consumer_APAC_Quantity_smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)

lines(timevals_in[1:42], global_pred, col='blue', lwd=2)

local_pred <- Consumer_APAC_Quantity_ts-global_pred

plot(local_pred, col='blue', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- Consumer_APAC_Quantity[43:48,]

timevals_out <- outdata$timevals_in

str(outdata)
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Quantity)
MAPE_class_dec

# we got MAPE = 29.09711

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(Consumer_APAC_Quantity_smoothedseries, col = "black")
lines(class_dec_pred, col = "red")

### We also have to predict for month from 49 to 54
a = 49
timevals_out2 = vector()
for (i in 1:6){
  timevals_out2[i] <- a
  a = a + 1
}

global_pred_out2 <- predict(lmfit,data.frame(Month = timevals_out2))

##############################End Classical Decomposition Consumer_APAC Quantity###############################

####################### ARIMA Consumer_APAC Quantity######################################################################

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(Consumer_APAC_Quantity_ts)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- Consumer_APAC_Quantity_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Quantity)
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Consumer_APAC_Quantity_ts, col = "black")
lines(auto_arima_pred, col = "red")
