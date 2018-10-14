library("ggplot2")
library("reshape2")
library("TTR")

# Read in the data
data <- read.csv('C:\\Users\\drjui\\DJIA_table.csv', stringsAsFactors = FALSE)

# Make 'Date' column a Date object to make train/test splitting easier
data$Date <- as.Date(data$Date)

# 1
# difference between open and close
dataLinePlot <- data
dataLinePlot$High <- NULL
dataLinePlot$Low <- NULL
dataLinePlot$Volume <- NULL
dataLinePlot$Adj.Close <- NULL

test_data_long <- melt(dataLinePlot, id="Date")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Date, y=value, colour=variable)) +
  geom_line()

# 2
# Close Difference
dataCD <- numeric(length=nrow(data)-1)
for(i in 2:nrow(data))
  dataCD[i] <- data$Close[i] - data$Close[i-1]

Pdata2 <- data.frame(Date=data$Date, CD=dataCD)

ggplot(data=Pdata2) +
  geom_line(aes(x=Date, y=CD))

# 3
# Standard Deviation of (Close Differences in each month)

monthCD <- split(Pdata2, format(Pdata2$Date, "%Y-%m"))

monthCDsd <- numeric(length=length(monthCD)-1)  # only one data(day) in 2016-7

for(i in 1:(length(monthCD)-1))  
  monthCDsd[i] <- mean(monthCD[[i]]$CD)

dfMonthCDsd <- data.frame(Date=(names(monthCD))[-length(monthCD)], SD=monthCDsd)

ggplot(data=dfMonthCDsd) +
  geom_line(aes(x=Date, y=SD, group=1)) +
  geom_hline(yintercept = mean(dfMonthCDsd$SD[1:nrow(dfMonthCDsd)-1]), color="blue") +
  labs(title="Monthly Standard Deviation of Closing Price Difference",
       x="Month",
       y="Standard Deviation") +
  theme_bw()


# 4

OpenCloseDiff <- numeric(length=nrow(data))

for(i in 1:nrow(data))
  OpenCloseDiff[i] <- data$Close[i] - data$Open[i]

dfOpenCloseDiff <- data.frame(Date=data$Date, OCD=OpenCloseDiff)

monthOCD <- split(dfOpenCloseDiff, format(dfOpenCloseDiff$Date, "%Y-%m"))

monthOCDsd <- numeric(length=length(monthOCD)-1)

for(i in 1:(length(monthOCD)-1))
  monthOCDsd[i] <- mean(monthOCD[[i]]$OCD)

dfMonthOCDsd <- data.frame(Date=(names(monthOCD))[-length(monthOCD)], SD=monthOCDsd)

ggplot(data=dfMonthOCDsd) +
  geom_line(aes(x=Date, y=SD, group=1)) +
  geom_hline(yintercept = mean(dfMonthOCDsd$SD[1:nrow(dfMonthOCDsd)-1]), color="blue") +
  labs(title="Monthly Standard Deviation of OCD",
       x="Month",
       y="Standard Deviation") +
  theme_bw()


# 5

#ggplot(data=data) +
#  geom_line(aes(x=Date, y=Volume))

dfVol <- data.frame(Date=data$Date, Volume=data$Volume)
monthVol <- split(dfVol, format(dfVol$Date, "%Y-%m"))

monthVolmean <- numeric(length(monthVol))

for(i in 1:length(monthVol))
  monthVolmean[i] <- mean(monthVol[[i]]$Volume)

dfmonthVolmean <- data.frame(Month=names(monthVol), VolMean=monthVolmean)

ggplot(data=dfmonthVolmean) +
  geom_line(aes(x=Month, y=VolMean, group=1))


# 6

ggplot(data=data) +
  geom_line(aes(x=Date, y=Adj.Close)) +
  theme_bw()

# 7

k <- 0
for(i in 2:nrow(data)){
  k <- k + data$Open[i] - data$Close[i-1]
}

meanClosetoOpen <- k / nrow(data)

# 8
# the 50 days which has the largest Closing Difference
head(Pdata2[with(Pdata2,order(-CD)),], 50)
# the 50 days which has the smallest Closing Difference
head(Pdata2[with(Pdata2,order(CD)),], 50)

# 9
tsdfMonthCDsd <- ts(dfMonthCDsd$SD , start=c(2008, 8), end=c(2016, 6), frequency=12) 
tsdfMonthCDsdComponent <- decompose(tsdfMonthCDsd)
plot(tsdfMonthCDsdComponent)
