
library(dplyr)
library(plyr)
not.enough <- NULL
for(i in 1:length(tweets)){
  if(nrow(tweets[[i]]) < 100){
    not.enough <- rbind(not.enough, i)
  }
}

good.set <- NULL


not.enough
for(i in 1:length(tweets))){
  trim.tweets <- NULL
  trim.tweets <- tweets[[i]][,c(1,3,4,5,32)]
  trim.tweets$emoji.text <- textclean::replace_emoji(trim.tweets$text)
  trim.tweets$clean.emoji.text <- clnTxt(trim.tweets$emoji.text)
  trim.tweets.json <- NULL
  trim.tweets.json <- capture.output(as.character(
    first.part(),
    for(i in 1:length(trim.tweets$clean.emoji.text)){
      cat.all(trim.tweets)
    },
    end.part()
  ))
  trim.tweets.result <- watsonAPI(trim.tweets.json,wurl,api_key)
  good.set <- rbind.fill(good.set, data.frame(trim.tweets$screen_name[1],
                                              trim.tweets.result[[3]][[1]][[4]],
                                              trim.tweets.result[[3]][[2]][[4]],
                                              trim.tweets.result[[3]][[3]][[4]],
                                              trim.tweets.result[[3]][[4]][[4]],
                                              trim.tweets.result[[3]][[5]][[4]],
                                              trim.tweets.result[[3]][[1]][[5]],
                                              trim.tweets.result[[3]][[2]][[5]],
                                              trim.tweets.result[[3]][[3]][[5]],
                                              trim.tweets.result[[3]][[4]][[5]],
                                              trim.tweets.result[[3]][[5]][[5]]))
}

