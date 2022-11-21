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

#creating word cloud for Mr Beast's Video Titles
titles <- read_file('mrbeast_titles.txt')

tdocs<-Corpus(VectorSource(titles))

## pre-process the texts
tdocs <- tm_map(tdocs, function(x) stri_replace_all_regex(x, "<.+?>", " "))
tdocs <- tm_map(tdocs, function(x) stri_replace_all_fixed(x, "\n", " "))

# consider all raw text
tdocs <- tm_map(tdocs, PlainTextDocument)
# remove punctuation
tdocs <- tm_map(tdocs, removePunctuation)
# remove numbers and symbols
tdocs <- tm_map(tdocs, removeNumbers)
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
wordcloud(t_dtmnew$word, family = "serif", font = 6, colors=colorRampPalette(brewer.pal(9,"Greens"))(32)[seq(8,32,6)], t_dtmnew$freq, min.freq = 10)

#plot1 correlation coefficient chart
panel.cor <- function(x, y, digits = 4, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.5, txt)
}
pairs(upload[,3:6], lower.panel = panel.cor)



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

plot_grid(plot2, plot3, ncol = 2, nrow = 1)
