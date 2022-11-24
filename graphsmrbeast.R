library(dplyr)
library(readr)
library(devtools)
library(Rcpp)
library(AnomalyDetection)
library(ggplot2)
library(tm)
library(stringi)
library(lattice)
library(udpipe)
library(wordcloud)
library(SnowballC)
library(readr)
library(cowplot)
library(tidyverse)
library(stringr)
upload <- read_csv("upload.csv")

#creating wordcloud for mr. beast's titles with numbers
titles_n <- read_file('mrbeast_titles.txt')

tdocs<-Corpus(VectorSource(titles_n))

## pre-process the texts
tdocs <- tm_map(tdocs, function(x) stri_replace_all_regex(x, "<.+?>", " "))
tdocs <- tm_map(tdocs, function(x) stri_replace_all_fixed(x, "\n", " "))

# consider all raw text
tdocs <- tm_map(tdocs, PlainTextDocument)
# remove punctuation
tdocs <- tm_map(tdocs, removePunctuation)
# set capital letters to lower letters
tdocs <- tm_map(tdocs, content_transformer(tolower)) 
# remove common words ("the", "and")
tdocs <- tm_map(tdocs, removeWords, stopwords("english"))

tdtm<-DocumentTermMatrix(tdocs)
ttdm<-TermDocumentMatrix(tdocs)

tmywords<-tdtm$dimnames$Terms

tdtm$dimnames$Docs<-as.character(c(1:length(text)))
rowTotals<- apply(tdtm , 1, sum) 
tdtm.new <- tdtm[rowTotals> 0,]

tdictionary<-tdtm$dimnames$Terms

tfreq <- colSums(as.matrix(tdtm))
tord <- order(tfreq,decreasing=TRUE)
t_dict <- as.data.frame(tfreq[tord])

tm <- as.matrix(tdtm)
tv <- sort(colSums(tm), decreasing=TRUE)
t_myNames <- names(tv)
t_dtmnew <- data.frame(word=t_myNames, freq=tv)
layout(matrix(c(1, 2), nrow=1), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Most Common Words and Numbers Used in Mr. Beast's Video Titles")
wordcloud(t_dtmnew$word, family = "serif", font = 6, colors=colorRampPalette(brewer.pal(9,"Greens"))(32)[seq(8,32,6)], t_dtmnew$freq, min.freq = 20)

#best videos via views
upload <- upload[ order(upload$items.statistics.viewCount, decreasing = TRUE ),] #making sure it is in highest to lowest order
best_viddf_views<-data.frame(upload$snippet.title[1:10],upload$items.statistics.viewCount[1:10]) ##capturing only top ten videos

plot2 <- ggplot(data = best_viddf_views, mapping = aes(x = reorder(as.factor(upload.snippet.title.1.10.), -upload.items.statistics.viewCount.1.10.), y = upload.items.statistics.viewCount.1.10.)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  labs(x = "YouTube Titles", y = 'View Count') +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#best videos via likes
upload <- upload[ order(upload$items.statistics.likeCount, decreasing = TRUE ),] #making sure it is in highest to lowest order
best_viddf_likes <- data.frame(upload$snippet.title[1:10], upload$items.statistics.likeCount[1:10]) ##capturing only top ten videos

plot3 <- ggplot(data = best_viddf_likes, mapping = aes(x = reorder(as.factor(upload.snippet.title.1.10.),-upload.items.statistics.likeCount.1.10.), y = upload.items.statistics.likeCount.1.10.)) +
  geom_bar(stat = 'identity', fill = 'pink') +
  labs(x = "YouTube Titles", y = 'Like Count') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#best videos via comments
upload <- upload[ order(upload$items.statistics.commentCount, decreasing = TRUE ),] #making sure it is in highest to lowest order
best_viddf_comments <- data.frame(upload$snippet.title[1:10], upload$items.statistics.commentCount[1:10]) ##capturing only top ten videos

plot1 <- ggplot(data = best_viddf_comments, mapping = aes(x = reorder(as.factor(upload.snippet.title.1.10.),-upload.items.statistics.commentCount.1.10.), y = upload.items.statistics.commentCount.1.10.)) +
  geom_bar(stat = 'identity', fill = 'lightgreen') +
  labs(x = "YouTube Titles", y = 'Comment Count') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_grid(plot2, plot3,plot1, ncol = 3, nrow = 1)

##Creating word cloud of top 10 best performing videos 
bindedvids <- cbind(best_viddf_likes$upload.snippet.title.1.10.,best_viddf_comments$upload.snippet.title.1.10.,best_viddf_views$upload.snippet.title.1.10.)
top <- read_file('top10vid_titles.txt')

tdocs<-Corpus(VectorSource(top))

## pre-process the texts
tdocs <- tm_map(tdocs, function(x) stri_replace_all_regex(x, "<.+?>", " "))
tdocs <- tm_map(tdocs, function(x) stri_replace_all_fixed(x, "\n", " "))

# consider all raw text
tdocs <- tm_map(tdocs, PlainTextDocument)
# remove punctuation
tdocs <- tm_map(tdocs, removePunctuation)
# set capital letters to lower letters
tdocs <- tm_map(tdocs, content_transformer(tolower)) 
# remove common words ("the", "and")
tdocs <- tm_map(tdocs, removeWords, stopwords("english"))

tdtm<-DocumentTermMatrix(tdocs)
ttdm<-TermDocumentMatrix(tdocs)

tmywords<-tdtm$dimnames$Terms

tdtm$dimnames$Docs<-as.character(c(1:length(text)))
rowTotals<- apply(tdtm , 1, sum) 
tdtm.new <- tdtm[rowTotals> 0,]

tdictionary<-tdtm$dimnames$Terms

tfreq <- colSums(as.matrix(tdtm))
tord <- order(tfreq,decreasing=TRUE)
t_dict <- as.data.frame(tfreq[tord])

tm <- as.matrix(tdtm)
tv <- sort(colSums(tm), decreasing=TRUE)
t_myNames <- names(tv)
t_dtmnew <- data.frame(word=t_myNames, freq=tv)
layout(matrix(c(1, 2), nrow=1), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Most Common Words and Numbers Used in Mr. Beast's Top 10 Video Titles")
wordcloud(t_dtmnew$word, family = "serif", font = 6, colors=colorRampPalette(brewer.pal(9,"Greens"))(32)[seq(8,32,6)], t_dtmnew$freq)