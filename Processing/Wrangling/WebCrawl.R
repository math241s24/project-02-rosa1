
library(Rcrawler)
library(tidyverse)


# REED CRAWL ########################################
Rcrawler(Website = "https://www.unigo.com/colleges/reed-college",
         crawlUrlfilter = c("/describe-how-your-school-looks-to-someone-whos-never-seen-it",
                            "/describe-the-students-at-your-school",
                            "/heres-your-chance-say-anything-about-your-college",
                            "/what-is-your-overall-opinion-of-this-school"),
         ExtractXpathPat = c("//div[@class = 'col-md-9']/p"),
         MaxDepth = 4,
         no_cores=4,
         no_conn=4,
         ManyPerPattern = TRUE,
         RequestsDelay = 5,
         saveOnDisk = FALSE)

REED_UNIGO_RAW <- do.call("rbind", DATA) %>% data.frame %>% unnest(V2) %>% select(V2)%>% distinct()

REED_UNIGO_RAW <- apply(REED_UNIGO_RAW,2,as.character)
write.csv(REED_UNIGO_RAW, file.path("~/Documents/GitHub/project-02-rosa1/Data/RawTxt", "REED_UNIGO_RAW.csv"))

# OHIO STATE UNI CRAWL ########################################
Rcrawler(Website = "https://www.unigo.com/colleges/ohio-state-university-main-campus",
         crawlUrlfilter = c("/describe-how-your-school-looks-to-someone-whos-never-seen-it",
                            "/describe-the-students-at-your-school",
                            "/heres-your-chance-say-anything-about-your-college",
                            "/what-is-your-overall-opinion-of-this-school"),
         ExtractXpathPat = c("//div[@class = 'col-md-9']/p"),
         MaxDepth = 4,
         no_cores=4,
         no_conn=4,
         ManyPerPattern = TRUE,
         RequestsDelay = 5,
         saveOnDisk = FALSE)

OHIO_UNIGO_RAW <- do.call("rbind", DATA) %>% data.frame %>% unnest(V2) %>% distinct(V2)

OHIO_UNIGO_RAW <- apply(OHIO_UNIGO_RAW,2,as.character)
write.csv(OHIO_UNIGO_RAW, file.path("~/Documents/GitHub/project-02-rosa1/Data/RawTxt", "OHIO_UNIGO_RAW.csv"))


# UCF CRAWL ########################################
Rcrawler(Website = "https://www.unigo.com/colleges/university-of-central-florida",
         crawlUrlfilter = c("/describe-how-your-school-looks-to-someone-whos-never-seen-it",
                            "/describe-the-students-at-your-school",
                            "/heres-your-chance-say-anything-about-your-college",
                            "/what-is-your-overall-opinion-of-this-school"),
         ExtractXpathPat = c("//div[@class = 'col-md-9']/p"),
         MaxDepth = 4,
         no_cores=4,
         no_conn=4,
         ManyPerPattern = TRUE,
         RequestsDelay = 5,
         saveOnDisk = FALSE)

UCF_UNIGO_RAW <- do.call("rbind", DATA) %>% data.frame %>% unnest(V2) %>% distinct(V2)

UCF_UNIGO_RAW <- apply(UCF_UNIGO_RAW,2,as.character)
write.csv(UCF_UNIGO_RAW, file.path("~/Documents/GitHub/project-02-rosa1/Data/RawTxt", "UCF_UNIGO_RAW.csv"))




