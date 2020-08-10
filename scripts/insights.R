{
  library(openxlsx)
  library(stringr)
  library(dplyr)
  library(tm)
  library(ggplot2)
  library(lubridate)
}

#loading functions
source("scripts/functions.R")

######################################################################################


## PRE PROCESSING

#loading data
reviews_bank <- read.xlsx("data_scientist_-_rma.xlsx", 2)
retained_bank <- read.xlsx("data_scientist_-_rma.xlsx", 3, detectDates = TRUE)

#filling sentiment column based on star.rating column
reviews_bank[reviews_bank$Star.Rating == 1 | reviews_bank$Star.Rating == 2, "Sentiment"] <- "Negative"
reviews_bank[reviews_bank$Star.Rating == 3, "Sentiment"] <- "Neutral"
reviews_bank[reviews_bank$Star.Rating == 4 | reviews_bank$Star.Rating == 5, "Sentiment"] <- "Positive"

# deleting emoticons
reviews_bank$Review.Text <- gsub("[^\x01-\xFF]", "", reviews_bank$Review.Text)

# replacing 1000 by 1
retained_bank[6:14][retained_bank[,6:14] == 1000] <- 1

# convering columns format
retained_bank[,4:14] <- sapply(retained_bank[,4:14], as.numeric)
retained_bank$Date <- as.Date(retained_bank$Date)

# deleting wrong dates
retained_bank <- filter(retained_bank,!is.na(retained_bank$Date))


# INSGIHTS OVER COMPETITORS

concorrentes <- c("Nubank","BB","Banco do Brasil", "Bradesco", "Santander", "Banco Inter","\\bXP\\b")

competitors_mentions_boolean <- data.frame(
  sapply(concorrentes, function(x) {
    grepl(x, reviews_bank$Review.Text, ignore.case = TRUE)
  })
)

competitors_mentions_index <- unlist(
  unname(
    apply(
      competitors_mentions_boolean, 2, function(x) which(x)
    )
  )
)

competitors_mentions <- reviews_bank[competitors_mentions_index,]

apply(competitors_mentions_boolean, 2, function(x) which(x))

summary(competitors_mentions$Star.Rating)
boxplot(competitors_mentions$Star.Rating)

write.csv2(competitors_mentions,'powerbi_data/competitors_mentions.csv')


# INSIGHTS OVER REVIEWS

# deleting NAs and keeping only pt reviews
# language accuracy is low, so I kept reviews written in any langugage

#review_and_sentiment <- reviews_bank[!is.na(reviews_bank$Review.Text) & reviews_bank$Reviewer.Language=='pt', c("Review.Text","Sentiment")]
review_and_sentiment <- reviews_bank[!is.na(reviews_bank$Review.Text), c("Review.Text","Sentiment")]

# setting apart sentiment reviews
positive_review <- review_and_sentiment[review_and_sentiment$Sentiment=="Positive",]
neutral_review <- review_and_sentiment[review_and_sentiment$Sentiment=="Neutral",]
negative_review <- review_and_sentiment[review_and_sentiment$Sentiment=="Negative",]

# building corpus and summing
positive_corpus <- corpus_function(positive_review$Review.Text)
neutral_corpus <- corpus_function(neutral_review$Review.Text)
negative_corpus <- corpus_function(negative_review$Review.Text)

write.csv2(positive_corpus,'positive_corpus.csv', row.names = FALSE)
write.csv2(neutral_corpus,'neutral_corpus.csv', row.names = FALSE)
write.csv2(negative_corpus,'negative_corpus.csv', row.names = FALSE)

# getting the context in which the word is used
positive_most_used <- positive_corpus[order(positive_corpus[,2], decreasing = T)[1:10], 1]
neutral_most_used <- neutral_corpus[order(neutral_corpus[,2], decreasing = T)[1:10], 1]
negative_most_used <- negative_corpus[order(negative_corpus[,2], decreasing = T)[1:10], 1]


kwics <- list()
whole_review <- list()

# getting kwic for each word as well as the whole review
for (j in 1:length(negative_most_used)){
  
  kwics[[j]] <- data.frame(kwic(text = tolower(negative_review[,1]), keyword = negative_most_used[j], reach = 2), stringsAsFactors = FALSE)
  kwics[[j]]$word <- negative_most_used[j]
  whole_review[[j]] <- negative_review[grepl(negative_most_used[j],negative_review[,1]), 1]
  
}

write.csv2(bind_rows(kwics),'negative_kwic.csv',row.names = FALSE)


# INSIGHTS OVER OGANIC PERFORMANCE

summary(retained_bank[retained_bank$Acquisition.Channel == "Organic", "Visitor-to-Installer.conversion.rate"])

boxplot(retained_bank$`Visitor-to-Installer.conversion.rate`~Acquisition.Channel,data=retained_bank, main="Installer Conversion vs. Acquisition",
        xlab="Acquisition Channel", ylab="Visitor-to-Installer.conversion.rate")

boxplot(retained_bank$`Installer-to-30.days.retention.rate`~Acquisition.Channel,data=retained_bank, main="Installer Conversion vs. Acquisition",
        xlab="Acquisition Channel", ylab="Visitor-to-Installer.conversion.rate",)

df <- select(retained_bank,c("Date","Acquisition.Channel","Acquisition.Channel","Installer-to-1.day.retention.rate","Installer-to-7.days.retention.rate",
                                "Installer-to-15.days.retention.rate","Installer-to-30.days.retention.rate"))

df <- retained_bank %>%
  mutate(month = format(Date, "%m")) %>%
  group_by(Acquisition.Channel) %>%
  summarise('1_day' = mean(`Installer-to-1.day.retention.rate`),
            '7_days' = mean(`Installer-to-7.days.retention.rate`),
            '15_days'= mean(`Installer-to-15.days.retention.rate`),
            '30_days'= mean(`Installer-to-30.days.retention.rate`)
            )

