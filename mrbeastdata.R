#packages
library(httr)
library(jsonlite)
library(here)
library(dplyr)
library(readr)
library(devtools)
library(Rcpp)
library(dplyr)
library(ggplot2)
library(tm)
library(stringi)
library(lattice)
library(udpipe)

##
##This file does not run unless you put your own API key in it. 
##


key <- (FILL IN WITH YOUR OWN API KEY) #api key
MrBeastChannelID<-"UCX6OQ3DkcsbYNE6H8uQQuVA" #channel id
base<- "https://www.googleapis.com/youtube/v3/" #baseurl

# Construct the API call: https://www.yuichiotsuka.com/youtube-data-extract-r/ 
api_params <- 
  paste(paste0("key=", key),  
        paste0("id=", MrBeastChannelID), 
        "part=snippet,contentDetails,statistics",
        sep = "&")
api_call <- paste0(base, "channels", "?", api_params)
api_result <- GET(api_call)
json_result <- httr::content(api_result, "text", encoding="UTF-8")

#format channel information from API into dataframe
channel.json <- fromJSON(json_result, flatten = T)
channel.df <- as.data.frame(channel.json)

#take 'upload' playlist ID from the channel dataframe in order to get video information
playlist_id <- channel.df$items.contentDetails.relatedPlaylists.uploads

#temporary variables
nextPageToken <- ""
upload.df <- NULL
pageInfo <- NULL

#go through pages to get all the video information
while (!is.null(nextPageToken)) {
  # Construct the API call
  api_params <- 
    paste(paste0("key=", key), 
          paste0("playlistId=", playlist_id), 
          "part=snippet,contentDetails",
          "maxResults=50",
          sep = "&")
  
  # Add the page token for page 2 onwards
  if (nextPageToken != "") {
    api_params <- paste0(api_params,
                         "&pageToken=",nextPageToken)
  }
  
  api_call <- paste0(base, "playlistItems", "?", api_params)
  api_result <- GET(api_call)
  json_result <- httr::content(api_result, "text", encoding="UTF-8")
  upload.json <- fromJSON(json_result, flatten = T)
  
  nextPageToken <- upload.json$nextPageToken
  pageInfo <- upload.json$pageInfo
  
  curr.df <- as.data.frame(upload.json$items)
  if (is.null(upload.df)) {2
    upload.df <- curr.df
  } else {
    upload.df <- bind_rows(upload.df, curr.df)
  }
}

#upload.df is the dataframe that contains all the video information (titles, description, when it was posted)

#Now, I need to get the video statistics such as the likes, view count, and comment count.

#create empty vectors to be the column names
items.id <- c()
items.statistics.viewCount <- c()
items.statistics.likeCount <- c()
items.statistics.commentCount <- c()

for (x in 1:730){ #loop through all 730 Mr Beast videos
  vid_api_params <- 
    paste(paste0("key=", key),  
          paste0("id=", upload.df$contentDetails.videoId[x]), 
          "part=statistics",
          sep = "&")
  vid_api_call <- paste0(base, "videos", "?", vid_api_params)
  vid_api_result <- GET(vid_api_call)
  vid_json_result <- httr::content(vid_api_result, "text", encoding="UTF-8")
    vid.json <- fromJSON(vid_json_result, flatten = T)
    vid.df <- as.data.frame(vid.json) 
    
    #If there are any variables within the row that are blank, say they are NA
    
    if("items.statistics.likeCount" %in% colnames(vid.df) == FALSE){ 
      items.statistics.likeCount[x] <- NA
    }
    else if("items.id" %in% colnames(vid.df) == FALSE)
      {
    items.id[x] <- NA
    }
    else if("items.statistics.viewCount" %in% colnames(vid.df) == FALSE){
    items.statistics.viewCount[x] <- NA
    }
    else if ('items.statistics.commentCount'%in% colnames(vid.df) == FALSE){
      items.statistics.commentCount[x] <- NA
    }
    else{
      items.id[x] <- vid.df$items.id[1] #if all the variables within the row is present, then add that variable into the vector
      items.statistics.commentCount[x] <- vid.df$items.statistics.commentCount[1]
      items.statistics.viewCount[x] <- vid.df$items.statistics.viewCount[1]
      items.statistics.likeCount[x] <- vid.df$items.statistics.likeCount[1]
    }
    
    }

vid.df <- cbind(items.id, items.statistics.viewCount,items.statistics.likeCount, items.statistics.commentCount) #bind the vector to get a matrix
vid.df[is.na(vid.df)] <- 0 #make all NA into 0

upload.df <- cbind(upload.df,vid.df) #combine the video information (titles, description, when it was posted) and video statistics (likes, view count, and comment count)
upload.df <- select(upload.df,items.id, items.statistics.viewCount,items.statistics.likeCount, items.statistics.commentCount,snippet.publishedAt,snippet.description,snippet.title,snippet.playlistId)
  
write.csv(upload.df, file = "upload.csv")

