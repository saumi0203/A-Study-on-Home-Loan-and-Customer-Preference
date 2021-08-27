#install.packages("devtools")
require(devtools)
#install_url("http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz")
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("sentimentr")
#install.packages("RWeka")
#install.packages("RSentiment")
library("RWeka")
library("Rstem")
library("RSentiment")
#install.packages("sentiment")
library("sentimentr")
library(ggplot2)
library("wordcloud")
library("RColorBrewer")
library("tm")
library("SnowballC")
library("plyr")
#install.packages("dplyr")
library("dplyr")
#install.packages("RCurl")
library(RCurl)
#library("plyr")
#library("plyr")
#library(sentiment)
#require(sentiment)
#ls("package:sentiment")
#############Text Analytics from Kaggle 
https://www.kaggle.com/rtatman/tutorial-sentiment-analysis-in-r/notebook##########################
################################
install.packages("tidyverse")
library(tidyverse)
#install.packages("tidytext")
library(tidytext)
#install.packages("glue")
library(glue)
library(stringr)
library(tibble)
library(tidyr)
#install.packages("readr")
library(readr)
library(purrr)
# get a list of the files in the input directory
files <- list.files("D:/input")

# stick together the path to the file & 1st file name
fileName <- glue("D:/input/", files[1], sep = "")
# get rid of any sneaky trailing spaces
fileName <- trimws(fileName)

# read in the new file
fileText <- glue(read_file(fileName))
# remove any dollar signs (they're special characters in R)
fileText <- gsub("\\$", "", fileText) 

# tokenize
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)

# get the sentiment from the first text: 
tokens %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) 

# write a function that takes the name of a file and returns the # of postive
# sentiment words, negative sentiment words, the difference & the normalized difference
GetSentiment <- function(file){
    # get the file
    fileName <- glue("D:/input/", file, sep = "")
    # get rid of any sneaky trailing spaces
    fileName <- trimws(fileName)

    # read in the new file
    fileText <- glue(read_file(fileName))
    # remove any dollar signs (they're special characters in R)
    fileText <- gsub("\\$", "", fileText) 

    # tokenize
    tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)

    # get the sentiment from the first text: 
    sentiment <- tokens %>%
      inner_join(get_sentiments("bing")) %>% # pull out only sentimen words
      count(sentiment) %>% # count the # of positive & negative words
      spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
      mutate(sentiment = positive - negative) %>% # # of positive words - # of negative owrds
      mutate(file = file) %>% # add the name of our file
      mutate(year = as.numeric(str_match(file, "\\d{4}"))) %>% # add the year
      mutate(president = str_match(file, "(.*?)_")[2]) # add president

    # return our sentiment dataframe
    return(sentiment)
}


GetSentiment(files[1])

# file to put our output in
sentiments <- data_frame()

# get the sentiments for each file in our datset
for(i in files){
    sentiments <- rbind(sentiments, GetSentiment(i))
}

# disambiguate Bush Sr. and George W. Bush 
# correct president in applicable rows
hdfc <- sentiments %>% 
  filter(bank == "HDFC") %>% # get rows where the bank is named "HDFC"...

# remove incorrect rows
#sentiments <- anti_join(sentiments, sentiments[sentiments$bank == "HDFC" & sentiments$year < 2000, ])

# add corrected rows to data_frame 
sentiments <- full_join(sentiments, hdfc)

# summerize the sentiment measures
summary(sentiments)

# plot of sentiment over time & automatically choose a method to model the change
ggplot(sentiments, aes(x = as.numeric(year), y = sentiment)) + 
  geom_point(aes(color = bank))+ # add points to our plot, color-coded by president
  geom_smooth(method = "auto") # pick a method & fit a model

# plot of sentiment by president
ggplot(sentiments, aes(x = bank, y = sentiment, color = bank)) + 
  geom_boxplot() # draw a boxplot for each president

# is the difference between parties significant?
# get democratic presidents & add party affiliation
democrats <- sentiments %>%
    filter(president == c("Clinton","Obama")) %>%
    mutate(party = "D")

# get democratic presidents & party add affiliation
republicans <- sentiments %>%
    filter(president != "Clinton" & president != "Obama") %>%
    mutate(party = "R")

# join both
byParty <- full_join(democrats, republicans)

# the difference between the parties is significant
t.test(democrats$sentiment, republicans$sentiment)

# plot sentiment by party
ggplot(byParty, aes(x = party, y = sentiment, color = party)) + geom_boxplot() + geom_point()
###########################################
###Get the data
data <- read.csv(file.choose(),header=T, stringsAsFactors = F,na.strings = "")
###########1st Feb,2018, Emotion Classification###########
df <- data.frame(data)
textdata <- df[df$Feedback, ]
textdata = gsub("[[:punct:]]", "", textdata)
textdata = gsub("[[:digit:]]", "", textdata)
textdata = gsub("http\\w+", "", textdata)
textdata = gsub("[ \t]{2,}", "", textdata)
textdata = gsub("^\\s+|\\s+$", "", textdata)
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
textdata = sapply(textdata, try.error)
textdata = textdata[!is.na(textdata)]
names(textdata) = NULL

class_emo = classify_emotion(textdata, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(textdata, algorithm="bayes")
polarity = class_pol[,4]
sent_df = data.frame(text=textdata, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
ggplot(sent_df, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
labs(x="emotion categories", y="")
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")

################Word Cloud 1st Feb###########
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = textdata[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE,
                 title.size = 1.5)

#############################################
################Preprocessing###################
Corpus <- Corpus(VectorSource(data$Feedback))
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus, removeNumbers)
Corpus <- tm_map(Corpus, tolower)
myStopwords <- c(stopwords('english'))
Corpus <- tm_map(Corpus, removeWords, myStopwords)
Corpus <- tm_map(Corpus, stripWhitespace)
Corpus <- tm_map(Corpus, stemDocument)
################Document Term Matrix###################
dtm<- DocumentTermMatrix(Corpus)
dtm
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
################Plotting the term frequencies###################
library(ggplot2)
wf <- data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq>30), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
################Word Cloud###################
wordcloud(names(freq), freq, min.freq=30,colors=brewer.pal(3,"Dark2"))
################Word Cloud###################
#This will make all words lower case, and remove any non-alphanumeric characters
data$Feedback <- tolower(data$Feedback)
data$Feedback<- gsub("[^0-9A-Za-z///' ]", "", data$Feedback)

Corpus <- Corpus(VectorSource(data$Feedback))
#remove puctuation
Corpus <- tm_map(Corpus, removePunctuation)
#remove numbers
Corpus <- tm_map(Corpus, removeNumbers)
#strip white space
Corpus <- tm_map(Corpus, stripWhitespace)
#remove stopwords
myStopwords <- c(stopwords('english'))
Corpus <- tm_map(Corpus, removeWords, myStopwords)

## Loading required package: methods
## Loading required package: RColorBrewer

wordcloud(Corpus, scale=c(5,.5), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

####################Sentiment Data##########################
sentimentdata <- sentimentr::sentiment(data$Feedback)

## Warning in `[.data.table`(dat[, `:=`(indx, wc < 1), by = c("id",
## "sentences", : with=FALSE ignored, it isn't needed when using :=. See ?':='
## for examples.

writeLines("td, th { padding : 6px } th { background-color : brown ; color : white; border : 1px solid white; } td { color : brown ; border : 1px solid brown }", con = "mystyle.css")

df1 <- knitr::kable(head(data[order(sentimentdata$sentiment,decreasing = T),c(1)], 25), format = "html")
row.names(df1) <- NULL
print(df1, row.names = F)

writeLines("td, th { padding : 6px } th { background-color : brown ; color : white; border : 1px solid white; } td { color : brown ; border : 1px solid brown }", con = "mystyle.css")
df2 <- knitr::kable(head(data[order(sentimentdata$sentiment),c(1)], 25), format = "html")
print(df2, row.names = F)

sents <- sentiment(data$Feedback)
slog <- sents[,mean(sentiment),by=element_id] #Sentiment by log
slog$cumsumsent <- cumsum(x = slog$V1) # Cumulative sum
#slog$prov<-"New testament"
#Partbook <- slog$prov
#grep(pattern = "The book of the generation of Jesus",x = bible) Start New testament
#Partbook[1:76646] <- "Old Testament"
#slog$Part <- Partbook
ggplot(slog,aes(x = element_id,y = cumsumsent,col=V1))+
  geom_point()+
  ggtitle("Customer Feedback: Cumulative sentiment")+
  xlab("Sentences")+ylab("Cumulative Sentiment")


corpus1 = Corpus(VectorSource(list(data$Feedback)))
corpus1= tm_map(corpus1, removePunctuation)
corpus1 = tm_map(corpus1, content_transformer(tolower))
corpus1 = tm_map(corpus1, removeNumbers) 
corpus1 = tm_map(corpus1, stripWhitespace)
corpus1 = tm_map(corpus1, removeWords, stopwords('english'))

dtm_customerfeedback = DocumentTermMatrix(VCorpus(VectorSource(corpus1$Feedback)))
freq_customerfeedback<- colSums(as.matrix(dtm_customerfeedback))

sentiments_customerfeedback= calculate_sentiment(freq_customerfeedback)
#sentiments_customerfeedback= cbind(sentiments_bible, as.data.frame(freq_customerfeedback))
#??calculate_sentiment
#sent_pos_bible = sentiments_bible[sentiments_bible$sentiment == 'Positive',]
#sent_neg_bible = sentiments_bible[sentiments_bible$sentiment == 'Negative',]

#############6th Aug######################
#Create Corpus

Corpuss <- Corpus(VectorSource(data$Rating))
#inspect a particular document
writeLines(as.character(Corpuss[[30]]))

getTransformations()

#create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))})

docs <- tm_map(Corpuss ,toSpace, "-")
docs <- tm_map(docs, toSpace, ":")

#Remove punctuation – replace punctuation marks with ” “
docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "-")

#Transform to lower case (need to wrap in content_transformer)
docs <- tm_map(docs,content_transformer(tolower))

#Strip digits (std transformation, so no need for content_transformer)
docs <- tm_map(docs, removeNumbers)

#remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))

#Strip whitespace (cosmetic?)
docs <- tm_map(docs, stripWhitespace)

writeLines(as.character(docs[[30]]))


#load library
library(SnowballC)
#Stem document
docs <- tm_map(docs,stemDocument)
writeLines(as.character(docs[[30]]))


docs <- tm_map(docs, content_transformer(gsub), pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub), pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub), pattern = "team-", replacement = "team")


dtm <- DocumentTermMatrix(docs)

inspect(dtm[,])
freq <- colSums(as.matrix(dtm))

#length should be total number of terms
length(freq)

#create sort order (descending)
ord <- order(freq,decreasing=TRUE)


#inspect most frequently occurring terms
freq[head(ord)]

#inspect least frequently occurring terms
freq[tail(ord)] 

dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20),
bounds = list(global = c(3,27))))

dtmr

freqr <- colSums(as.matrix(dtmr))
#length should be total number of terms
length(freqr)


#findFreqTerms(dtmr,lowfreq=20)

#findAssocs(dtmr,"Feedback",0.6)

####Word Frequency####

wf=data.frame(term=names(freqr),occurrences=freqr)
library(ggplot2)
p <- ggplot(subset(wf, freqr>0), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freqr),freqr, min.freq=1)

#…add color
#wordcloud(names(freqr),freqr,min.freq=1,colors=brewer.pal(6,"Dark2"))

#########Topic Modelling#########

#load topic models library

#install.packages("topicmodels")
library(topicmodels)

#Set parameters for Gibbs sampling
burnin <- 5000
iter <- 1000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 10

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
op=write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 10 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))
ldaOut.terms 
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))


#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))


#############Sentiment Analysis from Rbloggers##############

install.packages("RTextTools")

library(RTextTools)
library(e1071)

train=data[1:44,]
test=data[45:68,]

# build dtm
matrix= create_matrix(data[,3], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE)

# train the model
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:44,], as.factor(data[1:44,3]) )


# test the validity
mat[45:68,3]

predicted = predict(classifier, as.factor(data[45:68,3]))
#predicted = predict(classifier, mat[22:36,])
 predicted
table(data[45:68, 3], predicted)
recall_accuracy(data[45:68, 2], predicted)
recall_accuracy(data[45:68, 3], predicted)

# build the data to specify response variable, training set, testing set.
container = create_container(matrix, as.numeric(as.factor(data[,3])),
                             trainSize=1:44, testSize=45:68,virgin=FALSE)


models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

results = classify_models(container, models)


# accuracy table
table(as.numeric(as.factor(data[45:68, 3])), results[,"FORESTS_LABEL"])
table(as.numeric(as.factor(data[45:68, 3])), results[,"MAXENTROPY_LABEL"])

# recall accuracy
recall_accuracy(as.numeric(as.factor(data[45:68, 3])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(data[45:68, 3])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(data[45:68, 3])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(data[45:68, 3])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(data[45:68, 3])), results[,"SVM_LABEL"])


# model summary
analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)
analytics@ensemble_summary

N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")


###########Classify Emotions###########
install.packages("devtools")
library("devtools")
library("plyr")
library("ggplot2")
library("wordcloud")
library("RColorBrewer")
library("tm")
library("SnowballC")
library(RTextTools)
library(sentiment)
df <- data.frame(data$Feedback)
textdata <- df[df$data, ]
textdata = gsub("[[:punct:]]", "", textdata)
textdata = gsub("[[:punct:]]", "", textdata)
textdata = gsub("[[:digit:]]", "", textdata)
textdata = gsub("http\\w+", "", textdata)
textdata = gsub("[ \t]{2,}", "", textdata)
textdata = gsub("^\\s+|\\s+$", "", textdata)
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
textdata = sapply(textdata, try.error)
textdata = textdata[!is.na(textdata)]
names(textdata) = NULL
??classify_emotion
class_emo = classify_emotion(textdata, algorithm="bayes",prior=1.0)
emotion = class_emo[,2]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(textdata, algorithm="bayes")
polarity = class_pol[,4]
 

sent_df = data.frame(text=textdata, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


ggplot(sent_df, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
labs(x="emotion categories", y="")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")

emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)

for (i in 1:nemo)
{
  tmp = textdata[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE,
                 title.size = 1.5)

###########From Businessethics Project#############

df <- data.frame(data$Feedback)

#Create the corpus
Corpus = Corpus(VectorSource(data$Feedback))
#Specific Stop words list
myStopList = c("thats","can","have.","https.","su.","amp","amp.","mrudueuuu", "since","thats","goes","will","eduaubdedubuaeduaubdedubuaeduaubdedubuaeduaubdedubuaddemonetization")
#Create the document term matrix
doctermMatrix = DocumentTermMatrix(Corpus,control = list(removePunctuation = TRUE,
stopwords = c("have...",myStopList, stopwords("english")),
removeNumbers = TRUE, tolower = TRUE))
#Transpose of DTM i.e. TDM
termDocMatrix = TermDocumentMatrix(Corpus,control = list(removePunctuation = TRUE,
stopwords = c("have...",myStopList, stopwords("english")),
removeNumbers = TRUE, tolower = TRUE))
#Remove the sparse terms
dtms <- removeSparseTerms(doctermMatrix, 0.1)
#Organize terms by their frequency
freq = sort(colSums(as.matrix(doctermMatrix)))
wordFreq = data.frame(word=names(freq), freq=freq)
#Find out the words which have frequency greater than 50
wordFreqMax = subset(wordFreq, freq >= 10)
#Plot
p = ggplot(subset(wordFreqMax, freq>2), aes(word, freq), fill = "magenta")
p = p + geom_bar(stat="identity")
p = p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Create the Word Cloud
set.seed(142)
wordcloud(names(freq),freq, max.words =120,min.freq=100,scale=c(4,.5),
random.order = FALSE,rot.per=.5,colors=brewer.pal(6, "Dark2"))

################Pie chart##########
D=c("positive","neutral","negative")
mytable <- table(data$Sentiment)
 lbls <- paste(names(mytable), "\n", mytable, sep="")
 pie(mytable, labels = lbls, 
    main="Pie Chart of Sentiment\n (with sample sizes)",col=rainbow(length(D)))


###########################################################################

##################Cluster Analysis##########################
library(tm)
#Transform to lower case
docs <- tm_map(docs$Feedback,content_transformer(tolower))
#remove potentiallyy problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern," ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "•")
docs <- tm_map(docs, toSpace, "•    ")
docs <- tm_map(docs, toSpace, "-")
#docs <- tm_map(docs, toSpace, """)
#docs <- tm_map(docs, toSpace, """)
#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)

#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
writeLines(as.character(docs[[30]]))
docs <- tm_map(docs,stemDocument)

docs <- tm_map(docs, content_transformer(gsub),pattern = "organiz", replacement = "organise")
docs <- tm_map(docs, content_transformer(gsub), pattern = "organis", replacement = "organise")
docs <- tm_map(docs, content_transformer(gsub), pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub), pattern = "inenterpris", replacement = "enterprise")
docs <- tm_map(docs, content_transformer(gsub), pattern = "team-", replacement = "team")

myStopwords <- c("can", "say","one","way","use",
               "also","howev","tell","will",
                  "much","need","take","tend","even",
                  "like","particular","rather","said",
                "get","well","make","ask","come","end",
                  "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","’ve ",
                 "’re ")
#remove custom stopwords
docs <- tm_map(docs, removeWords, myStopwords)

dtm <- DocumentTermMatrix(docs)
#print a summary
dtm

#convert dtm to matrix
m <- as.matrix(dtm)
#write as csv file (optional)
write.csv(m,file="dtmEight2Late.csv")
#shorten rownames for display purposes
rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
                      substring(rownames(m), nchar(rownames(m))-12,nchar(rownames(m))-4))
#compute distance between document vectors
d <- dist(m)


#run hierarchical clustering using Ward’s method
groups <- hclust(d,method="ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=-1)

library(cluster)

plot(hclusters, which.plots = 2, main = "", sub = "", xlab = "")
##############################Kmeans##########

#I think there are 5 main topics: Data Science, Web Analytics, R, Julia, Wordpress
kmeans5<- kmeans(d, 5)

#Merge cluster assignment back to keywords
kw_with_cluster <- as.data.frame(cbind(s$'Natural Search Keyword', kmeans5$cluster))
names(kw_with_cluster) <- c("keyword", "kmeans5")

#Make df for each cluster result, quickly "eyeball" results
cluster1 <- subset(kw_with_cluster, subset=kmeans5 == 1)
cluster2 <- subset(kw_with_cluster, subset=kmeans5 == 2)
cluster3 <- subset(kw_with_cluster, subset=kmeans5 == 3)
cluster4 <- subset(kw_with_cluster, subset=kmeans5 == 4)
cluster5 <- subset(kw_with_cluster, subset=kmeans5 == 5)

############################################

## create a term document matrix
dtm <- DocumentTermMatrix(docs)
inspect(dtm[,])

findFreqTerms(dtm,2)
#findAssocs(dtm, "good", .4)
#washington  secretari  political     reagan republican      white      regan 
#      1.00       0.49       0.46       0.45       0.45       0.42       0.41 
#staff strategist 
#0.41       0.41 



## do tfxidf
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf[,])

## do document clustering

### k-means (this uses euclidean distance)
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)

### don't forget to normalize the vectors so Euclidean makes sense
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)


### cluster into 10 clusters
cl <- kmeans(m_norm, 10)
cl

table(cl$cluster)

### show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl$cl)

findFreqTerms(dtm[cl$cluster==1], 50)
inspect(reuters[which(cl$cluster==1)])

## hierarchical clustering
install.packages("proxy")
library(proxy)

### this is going to take 4-ever (O(n^2))
d <- dist(m, method="cosine")
hc <- hclust(d, method="average")
plot(hc)

cl <- cutree(hc, 36)
table(cl)
findFreqTerms(dtm, 10)

############Topic Modelling in R bloggers###############
library("tm")

library("wordcloud")
library("slam")
library("topicmodels")
#Load Text
Con <- read.csv("D://SBI_Feedback.csv")
con=Con$Feedback

# pre-processing:
con<- gsub("'", "", con)  # remove apostrophes
con<- gsub("[[:punct:]]", " ", con)  # replace punctuation with space
con<- gsub("[[:cntrl:]]", " ", con)  # replace control characters with space
con<- gsub("^[[:space:]]+", "", con) # remove whitespace at beginning of documents
con<- gsub("[[:space:]]+$", "", con) # remove whitespace at end of documents
con<- tolower(con)  # force to lowercase

# tokenize on space and output as a list:
doc.list <- strsplit(con, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:

stop_words <- stopwords("SMART")
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table) 


# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
install.packages("lda")
library(lda)

set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1 

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

Customerfeedback <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

install.packages("LDAvis")
library(LDAvis)
#install.packages("proxy")
load.packages(proxy)
install.packages("servr")
library(servr)



# create the JSON object to feed the visualization:
json <- createJSON(phi = Customerfeedback$phi, 
                   theta = Customerfeedback$theta, 
                   doc.length = Customerfeedback$doc.length, 
                   vocab = Customerfeedback$vocab, 
                   term.frequency = Customerfeedback$term.frequency)

s=serVis(json)
s

### Faster Way of doing LDA 

corpusLDA <- lexicalize(docs)
r=require(lda)

ldaModel=lda.collapsed.gibbs.sampler(corpusLDA$documents,K=10,vocab=corpusLDA$vocab,burnin=9999,num.iterations=1000,alpha=1,eta=0.1)
top.words <- top.topic.words(ldaModel$topics, 5, by.score=TRUE)
print(top.words)
############POS Tagging in R#####################
## Natural Language Processing: POS tagging

#install.packages("pattern.nlp")
install.packages("devtools")
library(devtools)
install_github("bnosac/pattern.nlp")


library(pattern.nlp)
ceta_tagged <- mapply(article.id = ceta$article.id, content = ceta$txt, FUN=function(article.id, content){
  out <- pattern_pos(x = content, language = "english", core = TRUE)
  out$article.id <- rep(article.id, times = nrow(out))
  out
}, SIMPLIFY = FALSE)  
ceta_tagged <- rbindlist(ceta_tagged)

## Take only nouns
ceta_nouns <- subset(ceta_tagged, word.type %in% c("NN") & nchar(word.lemma) > 2)

## All data in 1 list
ceta <- list(ceta_txt = txt, ceta = ceta, ceta_tagged = ceta_tagged, ceta_nouns = ceta_nouns)

## Look at POS tags
library(lattice)
barchart(sort(table(ceta$ceta_tagged$word.type)), col = "lightblue", xlab = "Term frequency", 
         main = "Parts of Speech Tag term frequency\n in CETA treaty")

############Co-Occurance############
install.packages("ggraph")
library(ggraph)
library(ggforce)
library(igraph)
library(tidytext)

## Take only nouns
word_cooccurences <- pair_count(data=data$Feedback, sort = TRUE)
set.seed(123456789)
head(word_cooccurences, 70) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8, col = "darkgreen") +
  ggtitle(sprintf("\n%s", "CETA treaty\nCo-occurrence of nouns")) +
  theme_void()
##########Word Occurance Network############
install.packages("semnet")
library(semnet)
terms <- unique(unlist(sapply(ceta_topic_terms, names)))
cooc <- coOccurenceNetwork(dtm[, terms])
cooc <- simplify(cooc)
plot(cooc, vertex.size=V(cooc)$freq / 20, edge.arrow.size=0.5)
