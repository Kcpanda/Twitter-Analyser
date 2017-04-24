library(twitteR)
library(RJSONIO)
library(bitops)
library(RCurl)
library(ROAuth)
library(shiny)
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

credential<- OAuthFactory$new(consumerKey="Enter your consumer key",
                   consumerSecret="Enter the consumer secret key",
                   requestURL="https://api.twitter.com/oauth/request_token",
                   accessURL="https://api.twitter.com/oauth/access_token",
                   authURL="https://api.twitter.com/oauth/authorize")

credential$handshake(cainfo="cacert.pem")
api_key <- "enter api_key or consumer key"

api_secret <- "enter api_secret or consumer_secret"

access_token <- "enter the access token"

access_token_secret <- "enter access token secret"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

save(credential, file="twitterauthentication.Rdata")



