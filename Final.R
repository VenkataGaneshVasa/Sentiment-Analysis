library("twitteR")
library("ROAuth")
library("tm")
library("stringr")
library("RColorBrewer")                                          
library("wordcloud")

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem") #for downloading the certificate

## Twitter authentication
apiKey <-  "Nejxm88eaJ6F9D5sfI7VojuzF"
apiSecret <- "grWBLLxTj4ZscRbIjPo0mirYEayDcjbJV7w6cgXY8o8kP8uu6V"
accessToken <- "1097661052945195009-Yv0YYpx6930nycbMYcerxucPUfyAcV"
accessTokenSecret <- "IlYSKWWS6Bnjd2VgUZb9P8i1lMLvAZwgb5TKaAE8rGQKL"
setup_twitter_oauth(apiKey,
                    apiSecret,
                    accessToken,
                    accessTokenSecret) 

tweets <- searchTwitter("IPLAuction", n=100) # Max we can have 1500 tweets
tweets
class(tweets)

#######################################################################################################
#Cleaning the data

df=do.call("rbind",lapply(tweets,as.data.frame))

class(df)

df 
View(df)
df$text # to derive only tweet

df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #remove emoticon, convert latin1 to ASCII and then substitute it with blank
df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text) #remove URL, here we used regular expression of URL, we replace it with nothing
sample <- df$text

head(sample)
View(sample)
#######################################################################################################################################################################
#Loading the word database

getwd()
setwd("D:/cp/Capstone")

Pos_Words=scan('D:/cp/Capstone/positive-words.txt',what='character',comment.char = ';') #what tells what kind of data stored, ; depicts comment

Neg_Words=scan('D:/cp/Capstone/negative-Words.txt',what='character',comment.char = ';')

#Adding words to positive and negative databases

Pos_Words=c(Pos_Words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader')
Neg_Words = c(Neg_Words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')

############################################################################################################################################################################
#Lexical Analysis

score.sentiment = function(sentences, Pos_Words, Neg_Words, .progress='none') #Function for lexical analysis
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, Pos_Words, Neg_Words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence) #Convert punctuation to nothing
    sentence = gsub('[[:cntrl:]]','',sentence)  #Convert cntrl to nothing
    sentence = gsub('\\d+','',sentence)  #removes decimal number
    sentence = gsub('\n','',sentence)    #removes new lines
    sentence = tolower(sentence) # Convert sentences to lowercase
    word.list = str_split(sentence, '\\s+') #For this we need stringr package
    words = unlist(word.list)  #changes a list to character vector
    pos.matches = match(words, Pos_Words) #matching one word of words with Postive words database, if matches then return data else na 
    neg.matches = match(words, Neg_Words)
    pos.matches = !is.na(pos.matches) #remove na data, contains only positive 
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1 = c(score, pp, nn)
    return (list1)
  }, Pos_Words, Neg_Words)
  score_new = lapply(list, `[[`, 1) #Extract the first column from returned list
  pp1 = lapply(list, `[[`, 2) #Stores positive scores
  nn1 = lapply(list, `[[`, 3) #Stores negative scores
  
  scores.df = data.frame(score = score_new, text=sentences)
  positive.df = data.frame(Positive = pp1, text=sentences)
  negative.df = data.frame(Negative = nn1, text=sentences)
  
  list_df = list(scores.df, positive.df, negative.df)
  return(list_df)
}

require(plyr)
result=score(sample,Pos_Words,Neg_Words)

result
require(reshape)

# Clean the tweets and returns merged data frame
result = score.sentiment(sample, Pos_Words, Neg_Words)

library(reshape)
test1=result[[1]] #Stores the score that is merged and tweets
test2=result[[2]] #stores the positive scores and tweets
test3=result[[3]] #stores the negative scores and tweets

#Creating three different data frames for Score, Positive and Negative
#Removing text column i.e tweet from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL

#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
qq1=melt(q1, var='Score') #It melts the score in one column

qq2=melt(q2, var='Positive')
qq3=melt(q3, var='Negative') 

qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL

#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

#Merging three data frames into one
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
View(table_final)
table_final

######################################################################################################################
#Positive Percentage

#Renaming
posSc=table_final$Positive
negSc=table_final$Negative

#Adding column
table_final$PosPercent = posSc/ (posSc+negSc)*100

#Replacing Nan(Not a number when divided by 0) with zero
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp

#Negative Percentage

#Adding column
table_final$NegPercent = negSc/ (posSc+negSc)*100

#Replacing Nan with zero
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0
table_final$NegPercent = nn
View(table_final)
table_final

#Histogram
hist(table_final$Positive, col=rainbow(10))
hist(table_final$Negative, col=rainbow(10))
hist(table_final$Score, col=rainbow(10))

#Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
labels <- c("Positive", "Negative")

library(plotrix)
#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis")

ref_text = sapply(tweets, function(x) x$getText()) #sapply returns a vector 
df <- do.call("rbind", lapply(tweets, as.data.frame)) #lapply returns a list
ref_text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
str(ref_text) #gives the summary/internal structure of an R object

library(tm) #tm: text mining
ref_corpus <- Corpus(VectorSource(ref_text)) #corpus is a collection of text documents
ref_corpus
inspect(ref_corpus[1])

docs<-ref_corpus

#clean text
library(wordcloud)
ref_clean <- tm_map(ref_corpus, removePunctuation)
ref_clean <- tm_map(ref_clean, removeWords, stopwords("english"))
ref_clean <- tm_map(ref_clean, removeNumbers)
ref_clean <- tm_map(ref_clean, stripWhitespace)
wordcloud(ref_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(1.5,1))

##################################################################################################
# top-trends

#assuming input = Ottawa

a_trends = availableTrendLocations()
a_trends
View(a_trends)
#woeid = a_trends[which(a_trends$name=="Ottawa"),3] # 3 is serial number of ottawa
locat_trend = getTrends(woeid = 2295414)
trends = locat_trend[1:2]

#To clean data and remove Non English words: 
dat <- cbind(trends$name)
dat2 <- unlist(strsplit(dat, split=", "))
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
dat4 <- dat2[-dat3]
dat4
View(dat4)
#top-10-hashtags of a particular user(BarackObama)

library(twitteR)
tw = userTimeline("narendramodi", n = 1000)
tw = twListToDF(tw) #Covert list to data frame
tw

vec1 = tw$text
vec1
#Extract the hashtags:

hash.pattern = "#[[:alpha:]]+"
have.hash = grep(x = vec1, pattern = hash.pattern) #stores the indices of the tweets which have hashes

hash.matches = gregexpr(pattern = hash.pattern,
                        text = vec1[have.hash]) #Matching hash with hash.pattern
extracted.hash = regmatches(x = vec1[have.hash], m = hash.matches) #the actual hashtags are stored here

df = data.frame(table(tolower(unlist(extracted.hash)))) #dataframe formed with var1(hashtag), freq of hashtag
colnames(df) = c("tag","freq")
df = df[order(df$freq,decreasing = TRUE),] #for ordering the frequencies 


dat = head(df,50)
dat2 = transform(dat,tag = reorder(tag,freq)) #reorder it so that highest freq is at the top


library(ggplot2)

p = ggplot(dat2, aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "blue")+theme_gray()
q=p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the Narendra Modi")+theme_grey()

q
