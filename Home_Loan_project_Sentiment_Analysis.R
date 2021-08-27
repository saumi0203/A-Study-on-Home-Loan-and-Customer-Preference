#library(twitteR) ### for fetching the tweets
library(plyr) ## for breaking the data into manageable pieces
#library(ROAuth) # for R authentication
library(stringr) # for string processing
library(ggplot2) # for plotting the results
library(tidyverse)
library(tidytext)
#library(glue)
library(stringr)
#library(devtools)
library(dplyr)
library(tidyr)
#install.packages("sentiments")
library(syuzhet)
library(sentimentr) 
#library(sentiments)
#if (packageVersion("devtools") < 1.6) {
#  install.packages("devtools")
#}
#devtools::install_github("hadley/lazyeval")
#devtools::install_github("hadley/dplyr")
################http://blog.kaggle.com/2017/10/05/data-science-101-sentiment-analysis-in-r-tutorial/###
posText <- read.delim("D:/input_for_text_analytics_11th_sem/positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim("D:/input_for_text_analytics_11th_sem/negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
pos.words = c(posText, 'upgrade')
neg.words = c(negText, 'wtf', 'wait', 'waiting','epicfail', 'mechanical')

#my_SBI_data <- read.delim("D:/input_for_text_analytics_11th_sem/SBI_1.txt",sep =" ", dec = " ")

SBI <- readLines("D:/input_for_text_analytics_11th_sem/SBI.txt")
HDFC <- readLines("D:/input_for_text_analytics_11th_sem/HDFC.txt")
PNB <- readLines("D:/input_for_text_analytics_11th_sem/PNB.txt")

########################Sentiment Analysis for SBI#########################
#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word
        sentence = cleanTweets(sentence)
        sentence <- tolower(sentence)
        wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}

#load pos,neg statements
afinn_list <- read.delim(file='D:/input_for_text_analytics_11th_sem/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- negText 
posTerms <- posText
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")  

#Calculate score on each tweet

#SBI_txt = sapply(SBI,function(x) x$getText())

#function to clean data
cleanTweets = function(SBI)
{
tweets_cl = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",SBI)
tweets_cl = gsub("http[^[:blank:]]+", "", tweets_cl)
tweets_cl = gsub("@\\w+", "", tweets_cl)
tweets_cl = gsub("[ \t]{2,}", "", tweets_cl)
tweets_cl = gsub("^\\s+|\\s+$", "", tweets_cl)
tweets_cl = gsub("[[:punct:]]", " ", tweets_cl)
tweets_cl = gsub("[^[:alnum:]]", " ", tweets_cl)
tweets_cl <- gsub('\\d+', '', tweets_cl)
return(tweets_cl)
}

Result <- as.data.frame(sentimentScore(SBI, vNegTerms, negTerms, posTerms, vPosTerms))

Result$'2' = as.numeric(Result$'2')
Result$'3' = as.numeric(Result$'3')
Result$'4' = as.numeric(Result$'4')
Result$'5' = as.numeric(Result$'5')
counts = c(sum(Result$'2'),sum(Result$'3'),sum(Result$'4'),sum(Result$'5'))
names = c("Worst","BAD","GOOD","VERY GOOD")
mr = list(counts,names)

colors = c("red", "yellow", "green", "violet")
barplot(mr[[1]], main="Review for SBI", xlab="Number of comments",legend=mr[[2]],col=colors)

library(wordcloud)
tweets_cl = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",SBI)
tweets_cl = gsub("http[^[:blank:]]+", "", tweets_cl)
tweets_cl = gsub("@\\w+", "", tweets_cl)
tweets_cl = gsub("[ \t]{2,}", "", tweets_cl)
tweets_cl = gsub("^\\s+|\\s+$", "", tweets_cl)
tweets_cl = gsub("[[:punct:]]", " ", tweets_cl)
tweets_cl = gsub("[^[:alnum:]]", " ", tweets_cl)
tweets_cl <- gsub('\\d+', '', tweets_cl)

wordcloud(tweets_cl)

########################Sentiment Analysis for HDFC#########################
#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word
        sentence = cleanTweets(sentence)
        sentence <- tolower(sentence)
        wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}

#load pos,neg statements
afinn_list <- read.delim(file='D:/input_for_text_analytics_11th_sem/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- negText 
posTerms <- posText
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")  

#Calculate score on each tweet

#function to clean data
cleanTweets = function(HDFC)
{
tweets_cl1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",HDFC)
tweets_cl1 = gsub("http[^[:blank:]]+", "", tweets_cl1)
tweets_cl1 = gsub("@\\w+", "", tweets_cl1)
tweets_cl1 = gsub("[ \t]{2,}", "", tweets_cl1)
tweets_cl1 = gsub("^\\s+|\\s+$", "", tweets_cl1)
tweets_cl1 = gsub("[[:punct:]]", " ", tweets_cl11)
tweets_cl1 = gsub("[^[:alnum:]]", " ", tweets_cl1)
tweets_cl1 <- gsub('\\d+', '', tweets_cl1)
return(tweets_cl1)
}

Result <- as.data.frame(sentimentScore(HDFC, vNegTerms, negTerms, posTerms, vPosTerms))


Result$'2' = as.numeric(Result$'2')
Result$'3' = as.numeric(Result$'3')
Result$'4' = as.numeric(Result$'4')
Result$'5' = as.numeric(Result$'5')
counts = c(sum(Result$'2'),sum(Result$'3'),sum(Result$'4'),sum(Result$'5'))
names = c("Worst","BAD","GOOD","VERY GOOD")
mr = list(counts,names)

colors = c("red", "yellow", "green", "violet")
barplot(mr[[1]], main="Review for HDFC", xlab="Number of comments",legend=mr[[2]],col=colors)

library(wordcloud)
tweets_cl1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",HDFC)
tweets_cl1 = gsub("http[^[:blank:]]+", "", tweets_cl1)
tweets_cl1 = gsub("@\\w+", "", tweets_cl1)
tweets_cl1 = gsub("[ \t]{2,}", "", tweets_cl1)
tweets_cl1 = gsub("^\\s+|\\s+$", "", tweets_cl1)
tweets_cl1 = gsub("[[:punct:]]", " ", tweets_cl1)
tweets_cl1 = gsub("[^[:alnum:]]", " ", tweets_cl1)
tweets_cl1 <- gsub('\\d+', '', tweets_cl1)

wordcloud(tweets_cl1)

########################Sentiment Analysis for PNB#########################
#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word
        sentence = cleanTweets(sentence)
        sentence <- tolower(sentence)
        wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}

#load pos,neg statements
afinn_list <- read.delim(file='D:/input_for_text_analytics_11th_sem/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- negText 
posTerms <- posText
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")  

#Calculate score on each tweet

#function to clean data
cleanTweets = function(PNB)
{
tweets_cl2 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",PNB)
tweets_cl2 = gsub("http[^[:blank:]]+", "", tweets_cl2)
tweets_cl2 = gsub("@\\w+", "", tweets_cl2)
tweets_cl2 = gsub("[ \t]{2,}", "", tweets_cl2)
tweets_cl2 = gsub("^\\s+|\\s+$", "", tweets_cl2)
tweets_cl2 = gsub("[[:punct:]]", " ", tweets_cl2)
tweets_cl2 = gsub("[^[:alnum:]]", " ", tweets_cl2)
tweets_cl2 <- gsub('\\d+', '', tweets_cl2)
return(tweets_cl2)
}

Result <- as.data.frame(sentimentScore(PNB, vNegTerms, negTerms, posTerms, vPosTerms))

Result$'2' = as.numeric(Result$'2')
Result$'3' = as.numeric(Result$'3')
Result$'4' = as.numeric(Result$'4')
Result$'5' = as.numeric(Result$'5')
counts = c(sum(Result$'2'),sum(Result$'3'),sum(Result$'4'),sum(Result$'5'))
names = c("Worst","BAD","GOOD","VERY GOOD")
mr = list(counts,names)

colors = c("red", "yellow", "green", "violet")
barplot(mr[[1]], main="Review for PNB", xlab="Number of comments",legend=mr[[2]],col=colors)

library(wordcloud)
tweets_cl2 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",PNB)
tweets_cl2 = gsub("http[^[:blank:]]+", "", tweets_cl2)
tweets_cl2 = gsub("@\\w+", "", tweets_cl2)
tweets_cl2 = gsub("[ \t]{2,}", "", tweets_cl2)
tweets_cl2 = gsub("^\\s+|\\s+$", "", tweets_cl2)
tweets_cl2 = gsub("[[:punct:]]", " ", tweets_cl2)
tweets_cl2 = gsub("[^[:alnum:]]", " ", tweets_cl2)
tweets_cl2 <- gsub('\\d+', '', tweets_cl2)

wordcloud(tweets_cl2)
#http://uc-r.github.io/sentiment_analysis
#http://uc-r.github.io/tidy_text
#http://blog.kaggle.com/2017/10/05/data-science-101-sentiment-analysis-in-r-tutorial/
#https://www.tidytextmining.com/sentiment.html#sentiment-analysis-with-inner-join
#http://www.bernhardlearns.com/2017/04/sentiment-analysis-with-r-and-tidytext.html
#https://www.r-bloggers.com/using-tidytext-to-make-sentiment-analysis-easy/
############Text HighLight#####https://github.com/trinker/sentimentr#############

library(magrittr)
library(dplyr)
set.seed(2)

SBI %>%
    filter(number %in% sample(unique(number), 3)) %>%
    mutate(review = get_sentences(review)) %$%
    sentiment_by(review, number) %>%
    highlight()

install.packages("data.table")
library(data.table)
dat <- SBI
setDT(dat)


dat[, gr:={gr= paste(person, time); cumsum(c(TRUE, gr[-1]!= gr[-.N]))}]
dat <- dat[, list(person=person[1L], time=time[1L], dialogue=paste(dialogue,
collapse = ' ')), by = gr][,gr:= NULL][,
dialogue_split := get_sentences(dialogue)][]
(sent_dat <- with(dat, sentiment_by(dialogue_split, list(person, time))))
## Not run:

sent_dat <- sentiment_by(SBI)
highlight(sent_dat)


SBI %>% extract_sentiment_terms()
HDFC %>% extract_sentiment_terms()
PNB %>% extract_sentiment_terms()


SBI %>% 
  sentiment_by(by = NULL) %>%
  highlight(SBI)

####################24th March#########################

sentiment_by('I am not very happy', by = NULL)

sentiment('I am not very happy. He is very happy')

'My life has become terrible since I met you and lost
 money' %>% extract_sentiment_terms()


'My life has become terrible since I met you and lost money. But I still have got a little hope left in me' %>% 
  sentiment_by(by = NULL) %>%
  highlight()

#################################################
data=read.csv(file.choose(),header=T, stringsAsFactors = F,na.strings = "")
dim(data)
str(data)


library(sentimentr)
sent_agg <- with(data, sentiment_by(data$Feedback))
head(sent_agg)


par(mfrow=c(2,1))
with(data, hist(Rating))
with(sent_agg, hist(ave_sentiment))

mean(data$Rating)

mean(sent_agg$ave_sentiment)


best_reviews <- slice(Data, top_n(sent_agg, 10, ave_sentiment)$element_id)
with(best_reviews, sentiment_by(data$Feedback)) %>% highlight()
highlight(with(best_reviews, sentiment_by(Data$Feedback)))




worst_reviews <- slice(Data, top_n(sent_agg, 10, -ave_sentiment)$element_id)
with(worst_reviews, sentiment_by(data$Feedback)) %>% highlight()
??top_n
##################Extract sentiment terms#######################

extract_sentiment_terms(data$Feedback)

extract_sentiment_terms(SBI)

extract_sentiment_terms(HDFC)

Data=read.csv(file.choose(),header=T,stringsAsFactors = F,na.strings = "")

highlight(sentiment_by(Data$Feedback))

###########Comparison Cloud###############

Positive <- readLines("D:/input_for_text_analytics_11th_sem/Positive.txt")
Negative <- readLines("D:/input_for_text_analytics_11th_sem/Negative.txt")
Neutral <- readLines("D:/input_for_text_analytics_11th_sem/Neutral.txt")

clean.text = function(x)
{
   # tolower
   x = tolower(x)
   # remove rt
   x = gsub("rt", "", x)
   # remove at
   x = gsub("@\\w+", "", x)
   # remove punctuation
   x = gsub("[[:punct:]]", "", x)
   # remove numbers
   x = gsub("[[:digit:]]", "", x)
   # remove links http
   x = gsub("http\\w+", "", x)
   # remove tabs
   x = gsub("[ |\t]{2,}", "", x)
   # remove blank spaces at the beginning
   x = gsub("^ ", "", x)
   # remove blank spaces at the end
   x = gsub(" $", "", x)
   return(x)
}

# clean texts

pos_clean = clean.text(Positive)
neg_clean = clean.text(Negative)
neu_clean = clean.text(Neutral)

pos = paste(pos_clean, collapse=" ")
neg = paste(neg_clean, collapse=" ")
neu = paste(neu_clean, collapse=" ")

# put everything in a single vector
all = c(pos, neg, neu)
# remove stop-words

all = removeWords(all,c(stopwords("english")))

# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)

# add column names
colnames(tdm) = c("Positive", "Negative", "Neutral")

# comparison cloud

comparison.cloud(tdm, random.order=FALSE, 
colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
title.size=1.5, max.words=500)

##############Finding Best Reviews################
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)


library(ggplot2)

Data=read.csv(file.choose(),header=T,stringsAsFactors = F,na.strings = "")
head(Data)
library(sentimentr)
sent_agg <- with(Data, sentiment_by(Data$Feedback))
head(sent_agg)


par(mfrow=c(2,1))
with(data, hist(Rating))
with(sent_agg, hist(ave_sentiment))

mean(data$Rating)

mean(sent_agg$ave_sentiment)

#SBI<- as.data.frame(SBI)
best_reviews <- slice(Data, top_n(sent_agg, 5, ave_sentiment)$element_id)
with(best_reviews, sentiment_by(SBI)) %>% highlight()
highlight(with(best_reviews, sentiment_by(Data$Feedback)))




worst_reviews <- slice(Data, top_n(sent_agg,  -5)$element_id)
with(worst_reviews, sentiment_by(data$Feedback)) %>% highlight()



