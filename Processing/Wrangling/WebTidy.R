library(stringr)#For text cleaning
library(dplyr)
library(readr)
library(tidyverse)
library(text)#Enables use of HuggingFace models for text analysis(NOTE: This file takes about 30mins to run, because
#the AI models are large, and are parsing through reasonably many reviews)
textrpp_install()
textrpp_initialize(save_profile = TRUE)


#Loading in raw files which we created in the WebCrawl program
REED_RAW <- read_csv("Data/RawTxt/REED_UNIGO_RAW.csv")

OHIO_RAW <- read_csv("Data/RawTxt/OHIO_UNIGO_RAW.csv")

UCF_RAW <- read_csv("Data/RawTxt/UCF_UNIGO_RAW.csv")

#The below function takes in a raw data set of the form we designed in WebCrawl.R, and outputs a tidy version,
#With parsable text reviews, sentiments, sentiment scores, and categories. 

processing_func <- function(df){
  
  #The line below and the function below it clean the raw text so that it can be parsed later on by the HuggingFace
  #NLP models. 
  
  colnames(df)[2] <- "Reviews"
  
  clean_text <- function(txt) {
    txt <- gsub("\\s+", " ", txt)  
    txt <- trimws(txt) 
    txt <- gsub("\n", " ", txt)  
    txt <- gsub("[[:punct:]]", "", txt) 
    txt <- gsub("http[s]?://\\S+", "", txt)
    txt <- tolower(txt)
   
    words <- strsplit(txt, split = "\\s+")[[1]] 
    
   
    #This small condition is a hotfix required by used of the HuggingFace NLPs, which have an effective word limit
    #of 250 for each request(review).
    
    if (length(words) >250) {
      return(NA) 
    }
    return(txt)
  }
  
  
  df <-df %>% 
    
    #Select the text, clean it, and filter out those which exceed the word limit.
    
    select(Reviews)%>%
    mutate(Reviews = map_chr(Reviews, clean_text))%>%
    filter(is.na(Reviews)==F)
  
  
  #textZeroShot() is from the 'text' package, and classifies text(each review) by predefined category. Here, we are effectively
  #categorizing each review programmatically. This adds a useful facet to our text analysis afforts. The exact model 
  #used is the value associated with the 'model' parameter. the function returns a dataframe of categories associated with each 
  #review, and in the liens of code below, we clean it to obtain the category which has the highest probability of being
  #the "true" classification. 
  
  categories <- textZeroShot(df$Reviews, 
                             c("professors","community","food","courses","finanical","outcomes","diversity"), 
                             model="facebook/bart-large-mnli")
  
  
  categories<-categories%>%
    select(sequence, labels_x_1)%>%
    head(1250)
  
  colnames(categories)[2] <- "Label"
  
  colnames(categories)[1] <- "Reviews"
  
  #textClassify() simply facilitates a sentiment analysis program which classifies each review as positive or negative
  #with a score from 0 to 5(when working with these scores in the shiny app, we negate all negative scores, so that they
  # can have an intuitive effect on averages and the like).
  
  sentiments <- textClassify(df$Reviews, model="textattack/bert-base-uncased-yelp-polarity",
                             return_incorrect_results = TRUE)
  
  sentiments<- sentiments %>%
    mutate(label_x = ifelse(label_x == "LABEL_1", "Positive","Negative"))%>%
    head(1250)
  
  colnames(sentiments)[1] <- "Sentiment"
  colnames(sentiments)[2] <- "SentimentSC"
  
  
  TIDYdf <- cbind(sentiments, categories)
  
  return(TIDYdf)
  
}

#And once the Raw datasets are processed, the tidy ones are written to the TidyText folder. 

REED_TIDY <- processing_func(REED_RAW)
write.csv(REED_TIDY, file.path("Data/TidyText", "REED_TIDY.csv"))

OHIO_TIDY <- processing_func(OHIO_RAW)
write.csv(OHIO_TIDY, file.path("Data/TidyText", "OHIO_TIDY.csv"))

UCF_TIDY <- processing_func(UCF_RAW)
write.csv(UCF_TIDY, file.path("Data/TidyText", "UCF_TIDY.csv"))

