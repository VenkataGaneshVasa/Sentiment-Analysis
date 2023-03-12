options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
install.packages(c('ROAuth','RCurl'))
install.packages("RColorBrewer")
install.packages("tm")
install.packages("wordcloud")
install.packages('base64enc')
install.packages('ROAuth')
install.packages('plyr')
install.packages('stringr')
install.packages('twitteR')
install.packages("installr") 

install.packages("htmltools")
library(installr) # install+load installr
#updateR()  updating R.
install.packages("twitteR")
install.packages("streamR")
require('ROAuth')
require('RCurl')
library(twitteR)
install.packages(c("devtools", "rjson", "bit64", "httr"))
#RESTART R session!
install.packages("githubinstall")
library(httr)
library(twitteR)
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
apiKey <-  "Nejxm88eaJ6F9D5sfI7VojuzF"
apiSecret <- "grWBLLxTj4ZscRbIjPo0mirYEayDcjbJV7w6cgXY8o8kP8uu6V"

twitCred <- OAuthFactory$new(
  consumerKey = apiKey, 
  consumerSecret = apiSecret,
  requestURL = reqURL,
  accessURL = accessURL, 
  authURL = authURL
)

twitCred$handshake(
  cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")
)

registerTwitterOAuth(twitCred)





###############################





download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
# Set constant requestURL
requestURL <- "https://api.twitter.com/oauth/request_token"
# Set constant accessURL
accessURL <- "https://api.twitter.com/oauth/access_token"
# Set constant authURL
authURL <- "https://api.twitter.com/oauth/authorize"
apiKey <-  "Nejxm88eaJ6F9D5sfI7VojuzF"
apiSecret <- "grWBLLxTj4ZscRbIjPo0mirYEayDcjbJV7w6cgXY8o8kP8uu6V"
accessToken <- "1097661052945195009-Yv0YYpx6930nycbMYcerxucPUfyAcV"
accessTokenSecret <- "IlYSKWWS6Bnjd2VgUZb9P8i1lMLvAZwgb5TKaAE8rGQKL"
setup_twitter_oauth(apiKey,
                    apiSecret,
                    accessToken,
                    accessTokenSecret) 
Objectname <- searchTwitter("SSMB", n=50, lang=NULL)

Objectname
