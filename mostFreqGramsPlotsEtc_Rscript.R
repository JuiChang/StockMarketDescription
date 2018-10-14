# The upper half code, data processing, was modified from Troy Walters's kernel
# https://www.kaggle.com/captcalculator/stock-prediction-with-r-glmnet-and-tm-packages

library(tm)
library(RWeka)
library(magrittr)
library(Matrix)
library(glmnet)
library(ROCR)
library(ggplot2)

# Read in the data
#setwd("C:\\Users\\drjui\\Combined_News_DJIA.csv")
data <- read.csv('C:\\Users\\drjui\\Combined_News_DJIA.csv', stringsAsFactors = FALSE)

#data <- read.csv('.\\input\\StockMarket\\Combined_News_DJIA.csv', stringsAsFactors = FALSE)

# Make 'Date' column a Date object to make train/test splitting easier
data$Date <- as.Date(data$Date)

# Combine headlines into one text blob for each day and add sentence separation token
data$all <- paste(data$Top1, data$Top2, data$Top3, data$Top4, data$Top5, data$Top6,
                  data$Top7, data$Top8, data$Top9, data$Top10, data$Top11, data$Top12, 
                  data$Top13, data$Top14, data$Top15, data$Top16, data$Top17, data$Top18,
                  data$Top19, data$Top20, data$Top21, data$Top22, data$Top23, data$Top24,
                  data$Top25, sep=' ')

# Get rid of those pesky b's and backslashes 
data$all <- gsub('b"|b\'|\\\\|\\"', "", data$all)

# Get rid of all punctuation except headline separators
data$all <- gsub("([<>])|[[:punct:]]", "\\1", data$all)

# Reduce to only the three columns we need. 
data <- data[, c('Date', 'Label', 'all')]


# shift the labels
for(i in 1:(nrow(data)-1))
  data$Label[i] <- data$Label[i+1]


for(i in 1:nrow(data))   # for bigram is better......
  data$all[i] <- removeWords(data$all[i], c(stopwords(kind = 'SMART'), 'the'))


options(mc.cores=1)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

control <- list(
  
  # make it comment if single grams are what u want
  tokenize=BigramTokenizer,
  
  bounds = list(global = c(100, 600)),  # singleGram:200, bigram 50           
  # bound: Terms that appear in less documents than the lower bound bounds$global[1]
  # or in more documents than the upper bound bounds$global[2] are discarded.
  
  stopwords = c(stopwords(kind = 'SMART'), '<s>')  # for single gram is better......
  
)

dtm <- VCorpus(VectorSource(data$all)) %>%  # Corpus can not work
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  DocumentTermMatrix(control=control)



freqGrams <- numeric(length=length(dtm$dimnames$Terms))
gramsLabelSum <- numeric(length=length(dtm$dimnames$Terms))
LSFvalue <- numeric(length=length(dtm$dimnames$Terms)) # gramsLabelSum/freqGrams
# all of the three has length 387


# all left words (duplicated among days, in a day isn't duplicated):
# max(dtm$i) is 1989 : which day
# max(dtm$j) is 387: which word (word ID)
# max(dtm$v) is 15: how many times in the day
# all of the three has length 141684 {EACH DAY EACH WORD}
for(k in 1:length(dtm$i)){  # the loop word by word 141684 iters {EACH DAY EACH WORD}
  
  #freqGrams: max 387, each word
  #freqGrams[dtm$j[k]] <- freqGrams[dtm$j[k]] + dtm$v[k]
  #freqGrams[dtm$j[k]] <- freqGrams[dtm$j[k]] + 1
  # gramsLabelSum: max 387, each word
  #gramsLabelSum[dtm$j[k]] <- gramsLabelSum[dtm$j[k]] + data$Label[dtm$i[k]]
  
  #freqGrams: max 387, each word
  freqGrams[dtm$j[k]] <- freqGrams[dtm$j[k]] + 1
  # gramsLabelSum: max 387, each word
  if (data$Label[dtm$i[k]] > 0) {
    gramsLabelSum[dtm$j[k]] <- gramsLabelSum[dtm$j[k]] + 1
  }else{
    gramsLabelSum[dtm$j[k]] <- gramsLabelSum[dtm$j[k]] - 0
  }
  
  
}


# Most frequent grams
freqGramIndex <- rev(tail(order(freqGrams), 30)) #!!! but the values are different from Ctrl+F in Excel...

dfFreqGrams <- data.frame(Gram=dtm$dimnames$Terms[freqGramIndex], 
                          Count=freqGrams[freqGramIndex])

ggplot(data=dfFreqGrams, aes(x=Gram, y=Count)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_x_discrete(limits = dfFreqGrams$Gram[order(dfFreqGrams$Count)]) +
  labs(title="Most frequent grams in the news titles") +
  geom_text(aes(label=Count), vjust=0) +
  theme_bw()

# find out how many documents in the DocumentTermMatrix (dtm) includes the word : 'german'
# sum(dtm$j == which(dtm$dimnames$Terms == 'german'))

for(i in 1:length(dtm$dimnames$Terms)) {
  LSFvalue[i] <- gramsLabelSum[i]/freqGrams[i]
  #LSFvalue[i] <- gramsLabelSum[i]
}
  

# high LSF grams
highLSFgramsIndex <- rev(tail(order(LSFvalue), 20))

dfhighLSFgrams <- data.frame(Gram=dtm$dimnames$Terms[highLSFgramsIndex], 
                             LSFvalue=LSFvalue[highLSFgramsIndex])

ggplot(data=dfhighLSFgrams, aes(x=Gram, y=LSFvalue)) +
  geom_bar(stat="identity") +
 coord_flip() +
  scale_x_discrete(limits = dfhighLSFgrams$Gram[order(dfhighLSFgrams$LSFvalue)]) +
  labs(title="Highest LSFvalue grams in the news titles") +
  theme_bw()

# low LSF grams
lowLSFgramsIndex <- head(order(LSFvalue), 20)

dflowLSFgrams <- data.frame(Gram=dtm$dimnames$Terms[lowLSFgramsIndex], 
                            LSFvalue=LSFvalue[lowLSFgramsIndex])

ggplot(data=dflowLSFgrams, aes(x=Gram, y=LSFvalue)) +
  geom_bar(stat="identity") +                                                          
  coord_flip() +
  scale_x_discrete(limits = dflowLSFgrams$Gram[rev(order(dflowLSFgrams$LSFvalue))]) +  
  labs(title="Lowest LSFvalue grams in the news titles") +
  theme_bw()


print(gramsLabelSum[highLSFgramsIndex])
print(freqGrams[highLSFgramsIndex])

print(gramsLabelSum[lowLSFgramsIndex])
print(freqGrams[lowLSFgramsIndex])
