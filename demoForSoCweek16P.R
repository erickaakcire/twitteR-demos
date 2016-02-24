library("twitteR")
library("rio")

setup_twitter_oauth("YourCredentials")

#Search
searchTerm <- "to:hillaryclinton"
tweets <- searchTwitter(searchTerm, n=5000, lang = "en")
dfT <- twListToDF(tweets)

#retrieve user timeline
userName <- "HillaryClinton"
userTl <- userTimeline(userName,n=3200, includeRts = TRUE)
dfT <- twListToDF(userTl)

#time histogram
firstDate <- min(dfT$created)
lastDate <- max(dfT$created)
toHist <- dfT$created
titleHist <- paste("Tweets from: ",firstDate," to: ",lastDate,sep="")
hist(toHist, col="wheat", breaks = 100, main=titleHist, xlab = "Time", freq=TRUE)
remove(firstDate,lastDate,titleHist,toHist)


#Top Hashtags - http://www.r-bloggers.com/using-r-to-find-obamas-most-frequent-twitter-hashtags/

vec1 = dfT$text

extract.hashes = function(vec){
  
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  
  hash.matches = gregexpr(pattern = hash.pattern,
                          text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  return(df)
}

dat = head(extract.hashes(vec1),20)
dat2 = transform(dat,tag = reorder(tag,freq))

#plot it
library(ggplot2)
p = ggplot(dat2, aes(x = tag, y = freq)) + geom_bar(fill = "blue", stat="identity")
titleP <- paste("Hashtag frequencies",sep="")
p + coord_flip() + labs(title = titleP)

#clean up
remove(dat,dat2,i,p,titleP,vec1,extract.hashes)

#tweet source
#clean the link from the status source field
dfT$sourceC <- sub("<a href=\".*\">","",dfT$statusSource)
dfT$sourceC <- sub("</a>","",dfT$sourceC)
source.table <- table(dfT$sourceC)
source.table.sort <- sort(source.table, decreasing = FALSE)
source.table.sort
source.table.p <- round(prop.table(source.table.sort),2) *100
source.table.p
#clean up
remove(source.table,source.table.sort,source.table.p)

#find tweets by - source
search <- "Twitter for iPhone"
tweetFound <- dfT[which(dfT$sourceC==search),]
head(tweetFound$tweet, n=10)
remove(search,tweetFound)

#find words in tweets
searchInTweet <- "bill" #regexp, case insenstive
dfT$match1 <- grepl(searchInTweet, dfT$text, ignore.case = "TRUE")
match.table <- table(dfT$match1)
match.table.sort <- sort(match.table, decreasing = FALSE)
match.table.sort
match.table.p <- round(prop.table(match.table.sort),2) *100
match.table.p
#find matching tweets
tweetFound <- dfT[which(dfT$match1==TRUE),]
head(tweetFound$text, n=10)
#clean
remove(match.table,match.table.p,match.table.sort,tweetFound)

#export
#strip out all tabs and new lines from the tweet field
dfT$tweetC <- gsub("\t"," ",dfT$text)
dfT$tweetC <- gsub("\n"," ",dfT$tweetC)

#add a link to the tweet
dfT$linkToTweet <- paste("http://twitter.com/",dfT$screenName,"/status/",dfT$id,sep="")

#subset only fields to export
myvars <- c("tweetC","created","linkToTweet","retweetCount","isRetweet","favoriteCount" ,"id","sourceC", "replyToSN", "truncated","replyToSID","replyToUID","screenName","longitude", "latitude")
dataExport <- dfT[myvars]
#
fileName <- paste(userName,".tsv",sep="")
fileName <- paste(searchTerm,".tsv",sep="")

export(dataExport,fileName)

remove(dataExport,fileName,myvars)
