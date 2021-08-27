Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_161')
#install.packages("rJava")
library(rJava)
library(ggplot2)
#install.packages("digest")
library(gridExtra)
library(digest)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
#install.packages("wordcloud2")
library(wordcloud2)
library(NLP)
library(RColorBrewer)
library(stringr)
library(sentimentr)
library(RSentiment)


options(jupyter.plot_mimetypes = "image/png")

data=read.csv(file.choose(),header=T, stringsAsFactors = F,na.strings = "")
dim(data)
str(data)
data$Bank <-as.factor(data$Bank)
data$Sentiment <-as.factor(data$Sentiment)
data$Negativereason <-as.factor(data$Negativereason)
data$Feedback <-as.factor(data$Feedback)

str(data)

##Proportion of positive,negative and neutral feedback
prop.table(table(data$Sentiment))

# generate a dataframe for plotting in ggplot2
smallData = as.data.frame(prop.table(table(data$Sentiment)))
colnames(smallData) = c('Sentiment', 'Frequency')
smallData

# create blank theme for pie chart, otherwise it looks awful in my opinion
blank_theme = theme_minimal() + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = 'bold') )

gbar = ggplot(smallData, aes(x = Sentiment, y = Frequency, fill = Sentiment))
gpie = ggplot(smallData, aes(x = "", y = Frequency, fill = Sentiment))

plot1 = gbar + geom_bar(stat = 'identity') + ggtitle("Overall Sentiment") + 
        theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1),
              axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -1))

plot2 = gpie + geom_bar(stat = 'identity') + coord_polar("y", start = 0) + blank_theme +
        theme(axis.title.x = element_blank()) + geom_text(aes(y = Frequency/3 + c(0, cumsum(Frequency)[-length(Frequency)]),
        label = round(Frequency, 2)), size = 4) + ggtitle('Overall Sentiment')

grid.arrange(plot1, plot2, ncol = 1, nrow = 2)



##Proportion of feedback per bank

prop.table(table(data$Bank))

# dataframe for plotting in ggplot
smallData1 = as.data.frame(prop.table(table(data$Bank)))
colnames(smallData1) = c('Bank','Frequency')
smallData1

gbar = ggplot(smallData1, aes(x = Bank, y = Frequency, fill = Bank))
gbar + geom_bar(stat = 'identity') + scale_fill_brewer() + ggtitle('Percentage of Feedbacks per Bank') +
guides(fill = FALSE) + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1))

#Proportion of various sentiment sentiments per Bank

prop.table(table(data$Sentiment, data$Bank))

# dataframe for ggplot
smallData2 = as.data.frame(prop.table(table(data$Sentiment, data$Bank)))
colnames(smallData2) = c('Sentiment', 'Bank', 'Percentage_Feedbacks')

gbar = ggplot(smallData2, aes(x = Bank, y = Percentage_Feedbacks, fill = Sentiment)) + ggtitle('Proportion of Feedbacks per Bank') +
theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_text(vjust = -1))

plot1 = gbar + geom_bar(stat = 'identity')
plot2 = gbar + geom_bar(stat = 'identity', position = 'fill')

grid.arrange(plot1, plot2, ncol = 1, nrow = 2)

#Reasons for negative sentiment feedbacks

# dataframe for ggplot
smallData3 = as.data.frame(prop.table(table(data$Negativereason)))
colnames(smallData3) = c('Reason', 'Frequency')
smallData3 = smallData3[-1, ] # remove first raw as it has no reason specified
smallData3

g = ggplot(smallData3, aes(x = Reason, y = Frequency)) + 
geom_bar(stat = 'identity', fill = '#0073C2FF')
g = g + ggtitle('Reasons for Not Recommending')
g = g + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_text(vjust = -0.1),
              axis.text.x = element_text(angle = 30, size = 10, vjust = 1))
##################################
g=g+ geom_text(
  aes(label = Frequency), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 3.5
)
##################################
g+coord_flip()
 


#Reasons for negative sentiment per bank

# dataframe for ggplot
smallData4 = as.data.frame(prop.table(table(data$Negativereason, data$Bank)))
colnames(smallData3) = c('Reason', 'Bank')
smallData4 = smallData4[-1, ] # remove first raw as it has no reason specified
smallData4

AXIS = subset(data, Bank == 'AXIS')
HDFC = subset(data, Bank == 'HDFC')
ICICI = subset(data, Bank == 'ICICI')
LIC = subset(data, Bank == 'LIC')
PNB = subset(data, Bank == 'PNB')
SBI = subset(data, Bank == 'SBI')

g1 = ggplot(as.data.frame(prop.table(table(AXIS$Negativereason))),
 aes(x = Var1, y = Freq)) +
geom_bar(stat = 'identity', fill = '#EFC000FF')
g1 = g1 + ggtitle('AXIS: ')
g1 = g1 + theme(plot.title = element_text(size = 14, face = 'bold', 
vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, 
size = 10, vjust = 1))

g2 = ggplot(as.data.frame(prop.table(table(HDFC$Negativereason))), aes(x = Var1, y = Freq)) +
geom_bar(stat = 'identity', fill = '#EFC000FF')
g2 = g2 + ggtitle('HDFC:')
g2 = g2 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g3 = ggplot(as.data.frame(prop.table(table(ICICI$Negativereason))), aes(x = Var1, y = Freq)) +
geom_bar(stat = 'identity', fill = '#EFC000FF')
g3 = g3 + ggtitle('ICICI:')
g3 = g3 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g4 = ggplot(as.data.frame(prop.table(table(LIC$Negativereason))), aes(x = Var1, y = Freq)) +
geom_bar(stat = 'identity', fill = '#EFC000FF')
g4 = g4 + ggtitle('LIC:')
g4 = g4 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g5 = ggplot(as.data.frame(prop.table(table(PNB$Negativereason))), aes(x = Var1, y = Freq)) +
geom_bar(stat = 'identity', fill = '#EFC000FF')
g5 = g5 + ggtitle('PNB:')
g5 = g5 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g6 = ggplot(as.data.frame(prop.table(table(SBI$Negativereason))), aes(x = Var1, y = Freq)) +
geom_bar(stat = 'identity', fill = '#EFC000FF')
g6 = g6 + ggtitle('SBI:')
g6 = g6 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

grid.arrange(g1, g2, ncol = 1, nrow = 2)

grid.arrange(g3, g4, ncol = 1, nrow = 2)

grid.arrange(g5, g6, ncol = 1, nrow = 2)

grid.arrange(g1+coord_flip(), g2+coord_flip(), ncol = 2, nrow=1)

#############Polarity###########################

library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
#install.packages('plotly')
library(plotly)

# plot distribution of polarity
# using sentiment package to classify emotions
emotions <- classify_emotion(data$Feedback, algorithm='bayes')

# using sentiment package to classify polarities
polarities = classify_polarity(data$Feedback, algorithm='bayes')

df = data.frame(text=data$Feedback, emotion=emotions[,'BEST_FIT'],
                     polarity=polarities[,'BEST_FIT'], stringsAsFactors=FALSE)
df[is.na(df)] <- "N.A."


# plot the emotions
plot_ly(df, x=~polarity, type="histogram",
        marker = list(color = c('magenta', 'gold',
                                'lightblue'))) %>%
  layout(yaxis = list(title='Count'), title="Sentiment Analysis: Polarity")     
##################################################
# Remove the @ bit of the text of the feedback
data$Feedback = gsub("^@\\w+ *", "", data$Feedback)
head(data)

# divide feedbacks in 2 dataframes according to positive or negative sentiment
positive = subset(data, Sentiment == 'positive')
negative = subset(data, Sentiment == 'negative')
neutral = subset(data, Sentiment == 'neutral')

dim(positive); dim(negative); dim(neutral)


##Determine word frequency and build cloud of words for each sentiment

# these words appear quite frequently in tweets and in my opinion are not informative,
# so I will remove them"
wordsToRemove = c('get','must','cant', 'can', 'now', 'just', 'will', 'dont', 'ive', 'got', 'much')

# generate a function to analyse corpus text
analyseText = function(text_to_analyse){
    # analyse text and generate matrix of words
    # Returns a dataframe containing 1 tweet per row, one word per column
    # and the number of times the word appears per tweet
    CorpusTranscript = Corpus(VectorSource(text_to_analyse))
    CorpusTranscript = tm_map(CorpusTranscript, tolower)
    #CorpusTranscript = tm_map(CorpusTranscript, PlainTextDocument)
    CorpusTranscript = tm_map(CorpusTranscript, removePunctuation)
    CorpusTranscript = tm_map(CorpusTranscript, removeWords, wordsToRemove)
    CorpusTranscript = tm_map(CorpusTranscript, removeWords, stopwords("english"))
    CorpusTranscript = DocumentTermMatrix(CorpusTranscript)
    CorpusTranscript = removeSparseTerms(CorpusTranscript, 0.97) # keeps a matrix 97% sparse
    CorpusTranscript = as.data.frame(as.matrix(CorpusTranscript))
    colnames(CorpusTranscript) = make.names(colnames(CorpusTranscript))
    return(CorpusTranscript)
}

# analysis of negative feedbacks
words = analyseText(negative$Feedback)
dim(words)

# sum the number of times each word appears in total accross all negative tweets.
freqWords_neg = colSums(words)
freqWords_neg = freqWords_neg[order(freqWords_neg, decreasing = T)]
head(freqWords_neg)

# analysis of positive feedbacks
words1 = analyseText(positive$Feedback)
dim(words1)

freqWords_pos = colSums(words1)
freqWords_pos = freqWords_pos[order(freqWords_pos, decreasing = T)]
head(freqWords_pos)

# analysis of neutral feedbacks
words2 = analyseText(neutral$Feedback)
dim(words2)

freqWords_neu = colSums(words2)
freqWords_neu = freqWords_neu[order(freqWords_neu, decreasing = T)]
head(freqWords_neu)

# word clouds
par(mfrow = c(1,3))

wordcloud(freq = as.vector(freqWords_neg), words = names(freqWords_neg),random.order = FALSE,
          random.color = FALSE, colors = brewer.pal(9, 'Reds')[4:9])

wordcloud(freq = as.vector(freqWords_pos), words = names(freqWords_pos),random.order = FALSE,
          random.color = FALSE, colors = brewer.pal(9, 'BuPu')[4:9])

wordcloud(freq = as.vector(freqWords_neu), words = names(freqWords_neu),random.order = FALSE,
          random.color = FALSE, colors = brewer.pal(9, 'Greens')[4:9])


# generate a function to analyse corpus text and return a document term matrix instead of dataframe
# we can perform further analysis on document term matrices
analyseText2 = function(text_to_analyse){
    # analyse text and generate matrix of words
    # Returns a dtm containing 1 tweet per row, one word per column
    # and the number of times the word appears per tweet
    CorpusTranscript = Corpus(VectorSource(text_to_analyse))
    CorpusTranscript = tm_map(CorpusTranscript, tolower)
    #CorpusTranscript = tm_map(CorpusTranscript, PlainTextDocument, lazy = T)
    CorpusTranscript = tm_map(CorpusTranscript, removePunctuation)
    CorpusTranscript = tm_map(CorpusTranscript, removeWords, wordsToRemove)
    CorpusTranscript = tm_map(CorpusTranscript, removeWords, stopwords("english"))
    CorpusTranscript = DocumentTermMatrix(CorpusTranscript)
    CorpusTranscript = removeSparseTerms(CorpusTranscript, 0.97) # keeps a matrix 97% sparse
    
    return(CorpusTranscript)
}


words_neg = analyseText2(negative$Feedback)
# find words correlated with the ones mentioned below (correlation at 70%)
findAssocs(words_neg, c("interest", 'processing', 'charges', 'service'), .07)

words_pos = analyseText2(positive$Feedback)
findAssocs(words_pos, c("Interest", 'processing', 'documentation', 'service'), .07)

##To further understand the associations between words, we can make clustering analysis of words

# hierarchical clustering
d = dist(t(as.matrix(words_neg)), method = 'euclidean')
fit = hclust(d = d, method = 'ward.D')

#fancy plot
op = par(bg = "#DDE3CA")
plot(fit, col = "#487AA1", col.main = "#45ADA8", col.lab = "#7C8071", main = 'Negative Sentiment', xlab = '',
     col.axis = "#F38630", lwd = 3, lty = 3, sub = "", hang = -1, axes = FALSE)
# add axis
axis(side = 2, at = seq(0, 400, 100), col = "#F38630", labels = FALSE, 
     lwd = 2)
# add text in margin
mtext(seq(0, 100, 10), side = 2, at = seq(0, 100, 10), line = 1, 
      col = "#A38630", las = 2)

##In the dendrogram, words that are linked by short arms are highly associated

plot.new()
plot(fit, hang=-1, main = 'Negative Sentiment', xlab = '')
rect.hclust(fit, k=3, border="red")

#########Hierarchical Clustering Cut number determination##########

#install.packages("factoextra")
#install.packages("cluster")
#install.packages("NbClust")
library(factoextra)
library(cluster)
library(NbClust)

############Elbow method########################

fviz_nbclust(t(as.matrix(words_neg)), FUN = hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

################silhouette method############
fviz_nbclust(t(as.matrix(words_neg)), hcut, method = "silhouette",
             hc_method = "complete")
###########Gap statistic for hierarchical clustering###########
set.seed(123)
gap_stat <- clusGap(t(as.matrix(words_neg)), FUN = hcut,nstart = 25,K.max = 10, B = 50)
# Plot gap statistic
fviz_gap_stat(gap_stat)

################Using Nbclust#####################
#nb <- NbClust(t(as.matrix(words_neg)), distance = "euclidean", min.nc = 2,
# max.nc = 67,method = "complete", index ="all")

??NbClust
###############################################################
# positive sentiment feedback
d = dist(t(as.matrix(words_pos)), method = 'euclidean')
fit = hclust(d = d, method = 'ward.D')

#fancy plot
op = par(bg = "#DDE3CA")
plot(fit, col = "#487AA1", col.main = "#45ADA8", col.lab = "#7C8071", main = 'Positive Sentiment', xlab = '',
     col.axis = "#F38630", lwd = 3, lty = 3, sub = "", hang = -1, axes = FALSE)
# add axis
axis(side = 2, at = seq(0, 400, 100), col = "#F38630", labels = FALSE, 
     lwd = 2)
# add text in margin
mtext(seq(0, 100, 10), side = 2, at = seq(0, 100, 10), line = 1, 
      col = "#A38630", las = 2)

##In the dendrogram, words that are linked by short arms are highly associated

plot.new()
plot(fit, hang=-1, main = 'Positive Sentiment', xlab = '')
rect.hclust(fit, k=3, border="red")

#########Hierarchical Clustering Cut number determination##########

#install.packages("factoextra")
#install.packages("cluster")
#install.packages("NbClust")
library(factoextra)
library(cluster)
library(NbClust)

############Elbow method########################

fviz_nbclust(t(as.matrix(words_pos)), FUN = hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

################silhouette method############
fviz_nbclust(t(as.matrix(words_pos)), hcut, method = "silhouette",
             hc_method = "complete")
###########Gap statistic for hierarchical clustering###########
set.seed(123)
gap_stat <- clusGap(t(as.matrix(words_pos)), FUN = hcut,nstart = 25,K.max = 10, B = 50)
# Plot gap statistic
fviz_gap_stat(gap_stat)

######################################################

#################Sentiment Analysis###################
library(syuzhet)
library(RSentiment)
library(sentimentr)
library(plyr)
library(ggplot2)
library(reshape2)
#install.packages("quanteda")
library(koRpus)
library(quanteda)
library(tm)

#Get Sentiments

data["SentimentSYValues"] = sapply(data$Feedback,
 function(x) get_sentiment(as.character(x), method="syuzhet"))

data["SentimentBingValues"] = sapply(data$Feedback,
 function(x) get_sentiment(as.character(x), method="bing"))

data["SentimentAfinnValues"] = sapply(data$Feedback,
 function(x) get_sentiment(as.character(x), method="afinn"))

#data["SentimentNrcValues"] = sapply(data$Feedback,
#function(x) get_sentiment(as.character(x), method="nrc"))

#Find the overall sentiments of the data

#DataSentiVal = data[order(data$Bank),c("Feedback","Bank","SentimentSYValues",
#"SentimentBingValues","SentimentNrcValues","SentimentAffinValues")]

#overallSentiments = sum(data[c("SentimentSYValues","SentimentBingValues","SentimentNrcValues","SentimentAffinValues"),])
#summarySentiments = summary(twitterDataordSentiVal[,c("SentimentSYValues","SentimentBingValues","SentimentNrcValues","SentimentAffinValues")])

#Plot of sentiments value
bingSentiment = table(sign(data$SentimentBingValues))
sySentiment = table(sign(data$SentimentSYValues))

colors = c("red", "green", "violet", "orange", "blue", "pink", "cyan")

plot(bingSentiment,col=colors,xlab="Sentiment - Bing", ylab = "Count" )
plot(sySentiment,col=colors,xlab="Sentiment - SY", ylab = "Count" )

#Plot sentiments with passing time
plot(data$SentimentSYValues,col=colors,ylim=range(c(5,-5)), type = "s" ,xlab="Sentiment - syuzhet", ylab = "Emotional Valance")
plot(data$SentimentBingValues,col=colors,xlab="Sentiment - Bing",type = "s", ylab = "Emotional Valance")


ft_SY_values = get_transformed_values(
data$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="h",
main ="Sentiment Analysis",
xlab = "Time",
ylab = "Emotional Valence",
col = "green"
)

par(new = TRUE)
ft_Bing_values = get_transformed_values(
data$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
scale_vals = F,
scale_range = T
)
plot(
ft_Bing_values,
type ="l",
main ="Sentiment Analysis",
xlab = "Time",
ylab = "Emotional Valence",
col = "blue"
)



par(new = TRUE)
ft_Bing_values = get_transformed_values(
data$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
scale_vals = F,
scale_range = T
)
plot(
ft_Bing_values,
type ="l",
main ="Sentiment Analysis",
xlab = "Time",
ylab = "Emotional Valence",
col = "red"
)


## Word specific text sentiments - Analysis of effect of Home Loan around few specific
## words/factors according to people


#Factor - Interest Rate, processing time, service
irFactData = data[grep("interest rate",data$Feedback),]
pFactData = data[grep("processing",data$Feedback),]
sFactData = data[grep("service",data$Feedback),]


#Factor - sbi,hdfc,pnb,lic,axis,icici
sbiFactData = data[grep("SBI",data$Feedback),]
hdfcFactData = data[grep("HDFC",data$Feedback),]
iciciFactData = data[grep("ICICI",data$Feedback),]
licFactData = data[grep("LIC",data$Feedback),]
axisFactData = data[grep("AXIS",data$Feedback),]
pnbFactData = data[grep("PNB",data$Feedback),]


#Plot factor specific graphs by BING
ft_SY_values = get_transformed_values(
irFactData$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)

plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "red"
)


par(new = TRUE)
ft_SY_values = get_transformed_values(
pFactData$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "green"
)


par(new = TRUE)
ft_SY_values = get_transformed_values(
sFactData$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "violet"
)



par(new = TRUE)
ft_SY_values = get_transformed_values(
sbiFactData$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "orange"
)
par(new = TRUE)
ft_SY_values = get_transformed_values(
hdfcFactData$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "blue"
)
par(new = TRUE)
ft_SY_values = get_transformed_values(
pnbFactData$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "pink"
)
par(new = TRUE)
ft_SY_values = get_transformed_values(
iciciFactData$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "cyan"
)


par(new = TRUE)
ft_SY_values = get_transformed_values(
licFactData$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "magenta"
)
par(new = TRUE)
ft_SY_values = get_transformed_values(
axisFactData$SentimentBingValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "chocolate"
)



##Plot on basis of SY analysis

ft_SY_values = get_transformed_values(
irFactData$SentimentSYValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)

plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "red"
)


par(new = TRUE)
ft_SY_values = get_transformed_values(
pFactData$SentimentSYValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "green"
)


par(new = TRUE)
ft_SY_values = get_transformed_values(
sFactData$SentimentSYValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "violet"
)



par(new = TRUE)
ft_SY_values = get_transformed_values(
sbiFactData$SentimentSYValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "orange"
)
par(new = TRUE)
ft_SY_values = get_transformed_values(
hdfcFactData$SentimentSYValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "blue"
)
par(new = TRUE)
ft_SY_values = get_transformed_values(
pnbFactData$SentimentSYValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "pink"
)
par(new = TRUE)
ft_SY_values = get_transformed_values(
iciciFactData$SentimentSYValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "cyan"
)


par(new = TRUE)
ft_SY_values = get_transformed_values(
licFactData$SentimentSYValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "magenta"
)
par(new = TRUE)
ft_SY_values = get_transformed_values(
axisFactData$SentimentSYValues,
low_pass_size = 3,
x_reverse_len = 500,
padding_factor = 2,
scale_vals = F,
scale_range = T
)
plot(
ft_SY_values,
type ="l",
main ="Opinion/Sentiment on Factors",
xlab = "Time",
ylab = "Emotional Valence",
col = "chocolate"
)


###############################################
#############Word cloud########################

library(quanteda)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(tm)
#install.packages("openNLP")
#library(openNLP)
library(NLP)
library(stringr)

#Create the corpus
Corpus <- Corpus(VectorSource(data$Feedback))
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus, removeNumbers)
Corpus <- tm_map(Corpus, tolower)
myStopwords <- c(stopwords('english'),'the','they','and','was')
Corpus <- tm_map(Corpus, removeWords, myStopwords)
Corpus <- tm_map(Corpus, stripWhitespace)
Corpus <- tm_map(Corpus, stemDocument)
################Document Term Matrix###################
dtm<- DocumentTermMatrix(Corpus)
dtm
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]

###################Term Document Matrix######################

tdm<- TermDocumentMatrix(Corpus)
tdm

#Remove the sparse terms
dtms <- removeSparseTerms(dtm, 0.1)
#Organize terms by their frequency
freq = order(colSums(as.matrix(dtm)), decreasing = T)
wordFreq = data.frame(word=names(freq), freq=freq)
#Find out the words which have frequency greater than 50
wordFreqMax = subset(wordFreq, freq >= 30)


#Plot
p = ggplot(subset(wordFreqMax, freq>20), aes(word, freq), fill = "blue")
p = p + geom_bar(stat="identity")
p = p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


#Create the Word Cloud
set.seed(142)
wordcloud(names(freq),freq, max.words =120,min.freq=20,scale=c(4,.5),
random.order = FALSE,rot.per=.5,colors=brewer.pal(6, "Dark2"))

#install.packages("Rstem")
#install.packages("C:/sentiment_0.2.tar.gz", repos = NULL, type="source")
library(Rstem)
library(sentiment)
# using sentiment package to classify emotions
emotions <- classify_emotion(data$Feedback, algorithm='bayes')
emotion = emotions[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_polarity= classify_polarity(data$Feedback, algorithm="bayes")
# get polarity best fit
polarity = class_polarity[,4]

# using sentiment package to classify polarities
polarities = classify_polarity(data$Feedback, algorithm='bayes')

df = data.frame(text=data$Feedback, emotion=emotions[,'BEST_FIT'],
                     polarity=polarities[,'BEST_FIT'], stringsAsFactors=FALSE)
df[is.na(df)] <- "N.A."

data_df = data.frame(text=data$Feedback, emotion=emotion,
polarity=polarity, stringsAsFactors=FALSE)

ggplot(data_df, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion))+xlab("Emotions Categories")
 + ylab("Tweet Count")+
ggtitle("Sentiment Analysis of Feedbacks on Emotions")

ggplot(data_df, aes(x=polarity)) +
geom_bar(aes(y=..count.., fill=polarity))+xlab("Polarities") + ylab("Feedback Count")+ggtitle("Sentiment Analysis of Feedbacks on Polarity")



emos = levels(factor(data_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
tmp = data$Feedback[emotion == emos[i]]
emo.docs[i] = paste(tmp, collapse=" ")
}
 
# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
 
# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
scale = c(3,.5), random.order = FALSE, title.size = 1.5)


#https://rhandbook.wordpress.com/tag/sentiment-analysis-using-r/




