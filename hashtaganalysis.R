#RESTART R session!
#this method avoids explicit handshake
#had to set up a developer account through Twitter
#create new app
#used my wordpress as my website
#callback URL WAS: http://127.0.0.1:1410

#install RCurl package, twitteR, httr, syuzhet
library(RCurl)
library(twitteR)
library(httr)
library(tm)
library(syuzhet)
library(wordcloud)

consumerKey <- "ZKPNweFiEevZ44LunMTm7Zdvb"
consumerSecret <- "7xivTtVRJZ6yz5IfMOYtn9fXXm6AwGy2drWHbpNXqPvbcDPpRi"
accessToken <- "4175840296-Q0lpwHThWNalIsQlOLNuatvC5HzbAeNhtKYLqOD"
accessTokenSecret <- "5ZEXZNlwfTo3MgYNabAAa8yzIyDWAsZ1sIJJnMr5anW4g"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
#searching for hashhtags in english...can specify other langs
tweets = searchTwitter("#BlackLivesMatter", n=1000, lang = "en")
#store the tweets in a dataframe
tweets.df = twListToDF(tweets)

#now we clean the tweets
tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)

tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub="")

#getting sentiment scores using NRC dictionary
emotions <- get_nrc_sentiment(tweets.df$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing= TRUE)])

#VISUALIZE EMOTIONS FROM TEH nrc SENTIMENTS
library(plotly)
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: #BlackLivesMatter")

# Create comparison word cloud data
#we can see which word contributes which emotion
wordcloud_tweet = c(
  paste(tweets.df$text[emotions$anger > 0], collapse=" "),
  paste(tweets.df$text[emotions$anticipation > 0], collapse=" "),
  paste(tweets.df$text[emotions$disgust > 0], collapse=" "),
  paste(tweets.df$text[emotions$fear > 0], collapse=" "),
  paste(tweets.df$text[emotions$joy > 0], collapse=" "),
  paste(tweets.df$text[emotions$sadness > 0], collapse=" "),
  paste(tweets.df$text[emotions$surprise > 0], collapse=" "),
  paste(tweets.df$text[emotions$trust > 0], collapse=" ")
)
# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet))

# remove punctuation, convert every word in lower case and remove stop words

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)

# create document term matrix

tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

# column name binding
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)
