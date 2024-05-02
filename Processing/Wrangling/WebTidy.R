library(stringr)
library(dplyr)
library(readr)
library(tidyverse)
library(text)
textrpp_install()
textrpp_initialize(save_profile = TRUE)



REED_RAW <- read_csv("~/Documents/GitHub/project-02-rosa1/Data/RawTxt/REED_UNIGO_RAW.csv")

processing_func <- function(df){
  
  colnames(df)[2] <- "Reviews"
  
  clean_text <- function(txt) {
    txt <- gsub("\\s+", " ", txt)  
    txt <- trimws(txt) 
    txt <- gsub("\n", " ", txt)  
    txt <- gsub("[[:punct:]]", "", txt) 
    txt <- gsub("http[s]?://\\S+", "", txt)
    txt <- tolower(txt)
   
    words <- strsplit(txt, split = "\\s+")[[1]] 
    
   
    if (length(words) >250) {
      return(NA) 
    }
    return(txt)
  }
  
  
  df <-df %>% 
    select(Reviews)%>%
    mutate(Reviews = map_chr(Reviews, clean_text))%>%
    filter(is.na(Reviews)==F)
  
  
  categories <- textZeroShot(df$Reviews, 
                             c("professors","community","food","courses","finanical","outcomes","diversity"), 
                             model="facebook/bart-large-mnli")
  
  
  categories<-categories%>%
    select(sequence, labels_x_1)
  
  colnames(categories)[2] <- "Label"
  
  colnames(categories)[1] <- "Reviews"
  
  sentiments <- textClassify(df$Reviews, model="textattack/bert-base-uncased-yelp-polarity",
                             return_incorrect_results = TRUE)
  
  sentiments<- sentiments %>%
    mutate(label_x = ifelse(label_x == "LABEL_1", "Positive","Negative"))
  
  colnames(sentiments)[1] <- "Sentiment"
  colnames(sentiments)[2] <- "SentimentSC"
  
  
  TIDYdf <- cbind(sentiments, categories)
  
  return(TIDYdf)
  
}

REED_TIDY <- processing_func(REED_RAW)
write.csv(REED_TIDY, file.path("~/Documents/GitHub/project-02-rosa1/Data/TidyText", "REED_TIDY.csv"))

