install.packages("stringr")
install.packages("ggplot2")
setwd("C:/Users/PUNEETMATHUR/Desktop/MISC/Sentiment_Analysis")

library(stringr)
library(ggplot2)
source('sentiment.r')
library("stats")

positives<- loadpos()

negatives<- loadneg()

hillaryemails2<- scan("h_onlywords.csv", what='character')

sentiment_score<- scoring(hillaryemails2,positives,negatives)
sum(hillaryemails2)
positives
negatives


sentence= gsub('[[:punct:]]','',hillaryemails2)
sentence= gsub('[[:cntrl:]]','',sentence)
sentence= gsub('\\d+','',sentence)
sentence=tolower(sentence)
word.list= str_split(sentence,'\\s+')
words= unlist(word.list)
allwords= c(negatives,positives)
  pos.matches= match(words,allwords)
pos.matches


pos_score<- posscoring(hillaryemails2,positives,negatives)
pos_score

neg_score<- negscoring(hillaryemails2,positives,negatives)
neg_score
total_words<- totalwords(hillaryemails2,positives,negatives)
total_words
neg_percent<- (100/total_words) * neg_score
neg_percent
pos_percent<- (100/total_words) * pos_score
pos_percent


scores<- c(neg_score,pos_score,total_words)
colores= c("red","green","blue")

titles<-cbind("Hillary Clinton's Public Mail Sentiment Analysis Score: ", sentiment_score)
barplot(scores, main=titles, names.arg=c( "Negative Sentiment", "Positive Sentiment","Total valid words"), xlab="Sentiments", ylab="Number of Words", col=colores) 

barplot(scores, main="Hillary Clinton's Public Mail Sentiment Analysis", names.arg=c( "Negative Sentiment", "Positive Sentiment"), xlab="Sentiments", ylab="Number of Words",label=scores, col=heat.colors(2)) 
per<- c(pos_percent,neg_percent)
pie(per, labels=c("Positive Sentiments","Negative Sentiments"), main=titles)
lines(scores)