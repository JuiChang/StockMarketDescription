library(tm)
library(RWeka)
library(magrittr)
library(Matrix)
library(glmnet)
library(ROCR)
library(ggplot2)

# Read in the data
data <- read.csv('./input/StockMarket/Combined_News_DJIA.csv', stringsAsFactors = FALSE)

# Make 'Date' column a Date object to make train/test splitting easier
data$Date <- as.Date(data$Date)

# Combine headlines into one text blob for each day and add sentence separation token
data$all <- paste(data$Top1, data$Top2, data$Top3, data$Top4, data$Top5, data$Top6,
                  data$Top7, data$Top8, data$Top9, data$Top10, data$Top11, data$Top12, 
                  data$Top13, data$Top14, data$Top15, data$Top16, data$Top17, data$Top18,
                  data$Top19, data$Top20, data$Top21, data$Top22, data$Top23, data$Top24,
                  data$Top25, sep=' <s> ')

# Get rid of those pesky b's and backslashes 
data$all <- gsub('b"|b\'|\\\\|\\"', "", data$all)

# Get rid of all punctuation except headline separators
data$all <- gsub("([<>])|[[:punct:]]", "\\1", data$all)

# Reduce to only the three columns we need. 
data <- data[, c('Date', 'Label', 'all')]

####################### shift the labels ####################
# PREVIOUS:(auc=0.5258177) 
#             if close value of 2/25 is less than 2/24
#             then the Label of 2/25 = 0
# problem:  x and y of a observation(day) are not much related
# NOW:(auc=0.5402666)      
#             if close value of 2/25 is less than 2/24
#             then the Label of 2/24 = 0
# solve: x and y of a observation(day) has more relation
# can think that y=1 means x(the news) is positive, on the contrary, x is negative
for(i in 1:(nrow(data)-1)){
  data$Label[i] <- data$Label[i+1]
}
#############################################################
options(mc.cores=1)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

control <- list(
  tokenize=BigramTokenizer,
  bounds = list(global = c(20, 500)),
  
  # ME
  stopwords = c(stopwords(kind = 'SMART'), '<s>')
  
)

# https://stackoverflow.com/questions/42604439/document-term-matrix-in-r-bigram-tokenizer-not-working
dtm <- VCorpus(VectorSource(data$all)) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  DocumentTermMatrix(control=control)