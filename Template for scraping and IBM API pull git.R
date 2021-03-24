#Packages Needed 
library(stringr)
library(twitteR)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)
library(rtweet)
library(plyr)
library(httr)

# 1. Grab tweets

twitter_api_key <- "token"
api_key_secret <- "token"
bearer_token <- "token"
access_token <- "token"
acess_token_secret <- "token"

create_token(app= "username", consumer_key = twitter_api_key, consumer_secret = api_key_secret,
             access_token = access_token, access_secret = acess_token_secret)

stephtweets <- get_timeline("StephenCurry30", n = 3200)

# 2. Emojis

library(textclean)
stephtweets$emoji.text <- textclean::replace_emoji(stephtweets$text)

# 3. clnTxt with Rwatsonpi package

library(rwatsonpi)
stephtweets$clean.emoji.text <- clnTxt(stephtweets$emoji.text)

# 4. Cat functions for JSON

content <- "content"
qj <- "\""
space <- " "
comma <- ","



cat.content <- function (df){
  cat(qj,content,qj,": ", qj, as.character(df$clean.emoji.text[i]), qj, comma, sep = "")
}


cat.contenttype<- function (){
  cat(qj, "contenttype", qj, ":", space, qj, "text/plain", qj, comma, sep = "")
}


cat.created <- function(df){
  cat(qj, "created", qj, ": ", df$created_at[i], comma, sep = "" )
}


cat.id <- function(df){
  cat(qj, "id", qj, ": ",qj, df$user_id[i], qj, comma, sep = "")
}


cat.language <- function(df){
  cat(qj, "language", qj, ": ", qj, df$lang[i], qj, sep = "")
}
i = 1
cat.language(stephtweets)

cat.all <- function(df){
  if(i !=1){
    cat(",")
  }
  cat("{")
  #cat("\n")
  cat.content(df)
  #cat("\n")
  cat.contenttype()
  #cat("\n")
  cat.created(df)
  #cat("\n")
  cat.id(df)
  #cat("\n")
  cat.language(df)
  #cat("\n")
  cat("}")
}



sqj <- "\'"

first.part <- function(){
  cat("{")
  #cat("\n")
  cat(qj,"contentItems", qj , ":", " [", sep = "")
}
first.part()

end.part <- function(){
  cat("]}")
  cat(sqj)
  cat(")")
}
end.part()

capture.output(for(i in 1:5){
  cat.all(stephtweets)
})
# 5. Make stephtweets into .json
steph.json <- NULL
steph.json <- capture.output(as.character(
  first.part(),
  for(i in 1:1000){
    cat.all(stephtweets)
  },
  end.part()
))


# 6. Write IBM pull function and keys for IBM

library(httr)
api_key = "Tj4Y-bfKv_6IIf7NhubDpm6SAG8M2W22tQfbO2YVCBu1"
user_url = "https://api.us-south.personality-insights.watson.cloud.ibm.com/instances/7a03f7cd-ce72-4dc5-b755-c81a6b5a4aba"
wurl <- "https://api.us-south.personality-insights.watson.cloud.ibm.com/instances/7a03f7cd-ce72-4dc5-b755-c81a6b5a4aba"
watsonAPI = function(tweets_json,user_url,api_key){
  body = tweets_json
  
  result = POST(paste0(user_url,'/v3/profile?version=2017-10-13&consumption_preferences=true&raw_scores=true'),
                authenticate("apikey",api_key),
                add_headers("Content-Type" ="application/json",
                            "Accept"="application/json"),
                encode="json",
                config(verbose=T),
                body=body)
  
  return_json = content(result)
  
  return(return_json)
}


# 7. Pull from IBM

steph.results <- NULL
steph.results = watsonAPI(steph.json, wurl, api_key)

steph.results.compare = watsonAPI(steph.json, wurl, api_key)

steph.results.compare.again = watsonAPI(steph.json, wurl, api_key)
