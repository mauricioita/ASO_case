{
  library(openxlsx)
  library(caTools)
  library(randomForest)
  library(xgboost)
  library(caret)
}

source("scripts/functions.R")


# PRE PROCESSING

#loading data
reviews_bank <- read.xlsx("data_scientist_-_rma.xlsx", 2)
retained_bank <- read.xlsx("data_scientist_-_rma.xlsx", 3, detectDates = TRUE)

#filling sentiment column based on star.rating column
reviews_bank[reviews_bank$Star.Rating == 1 | reviews_bank$Star.Rating == 2, "Sentiment"] <- "Negative"
reviews_bank[reviews_bank$Star.Rating == 3, "Sentiment"] <- "Neutral"
reviews_bank[reviews_bank$Star.Rating == 4 | reviews_bank$Star.Rating == 5, "Sentiment"] <- "Positive"

# deleting emoticons
reviews_bank$Review.Text <- gsub("[^\x01-\xFF]", "", reviews_bank$Review.Text)

# removing NA reviews
review_and_sentiment <- reviews_bank[!is.na(reviews_bank$Review.Text), c("Review.Text","Sentiment")]

# replacing 1000 by 1
retained_bank[6:14][retained_bank[,6:14] == 1000] <- 1

# convering columns format
retained_bank[,4:14] <- sapply(retained_bank[,4:14],as.numeric)
retained_bank$Date <- as.Date(retained_bank$Date)

# deleting wrong dates
retained_bank <- filter(retained_bank,!is.na(retained_bank$Date))


### NATURAL LANGUAGE PROCESSING WITH RANDOM FORESTS TO PREDICT SENTIMENT ###

corpus = VCorpus(VectorSource(review_and_sentiment$Review.Text), readerControl = list(language="pt"))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords(kind = "portuguese"))
corpus = tm_map(corpus, stripWhitespace)

dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
dataset$Sentiment = review_and_sentiment$Sentiment

dataset$Sentiment = factor(dataset$Sentiment, labels = c(1,2,3), levels = c("Positive","Neutral","Negative"))


# Splitting the dataset into the Training set and Test set
split = sample.split(dataset$Sentiment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
classifier = randomForest(x = training_set[-904],
                          y = training_set$Sentiment,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-904])

# Making the Confusion Matrix
cm = table(test_set[, 904], y_pred)

teste <- subset(review_and_sentiment, split == FALSE)
teste$pred_200 <- y_pred


# measuring model performance with k-fold cross validation 
folds = createFolds(training_set$Sentiment, k = 100)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = randomForest(x = training_fold[-904],
                            y = training_fold$Sentiment,
                            ntree = 10)
  y_pred = predict(classifier, newdata = test_fold[-904])
  cm = table(test_fold[, 904], y_pred)
  accuracy = (cm[1,1] + cm[2,2] + cm[3,3]) / (cm[1,1] + cm[2,2] + cm[3,3] + cm[1,2] + cm[2,1] + cm[1,3] + cm[2,3] + cm[3,1] + cm[3,2])
  return(accuracy)
})

accuracy = mean(as.numeric(cv))


### XGBOOST TO PREDICT 15 AND 30-DAYS RETATION RATE ###

retained_bank$Acquisition.Channel = as.numeric(factor(retained_bank$Acquisition.Channel,
                                   levels = c("Third-party referrers", "Other", "Tracked channels (UTM)", "Organic"),
                                   labels = c(1, 2, 3, 4)))

dataset = retained_bank[,c(3,4,5,7,14)] #30 days
dataset = retained_bank[,c(3,4,5,7,12)] #15 days
 
# Splitting the dataset into the Training set and Test set
split = sample.split(dataset$`Installer-to-30.days.retention.rate`, SplitRatio = 0.8) #30 days
split = sample.split(dataset$`Installer-to-15.days.retention.rate`, SplitRatio = 0.8) #15 days

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting XGBoost to the Training set
#classifier = xgboost(data = as.matrix(training_set[-5]), label = training_set$`Installer-to-30.days.retention.rate`, nrounds = 15)
classifier = xgboost(data = as.matrix(training_set[-5]), label = training_set$`Installer-to-15.days.retention.rate`, nrounds = 15)

y_pred = predict(classifier, newdata = as.matrix(test_set[-5]))

# measuring accuracy
folds = createFolds(training_set$`Installer-to-15.days.retention.rate`, k = 100)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-5]), label = training_set$`Installer-to-15.days.retention.rate`, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-5]))
  cm = table(test_fold[, 5], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))


# Applying Grid Search to find the best parameters
classifier = caret::train(y = training_set$`Installer-to-15.days.retention.rate`, x = as.matrix(training_set[-5]), method = 'xgbTree')
classifier
classifier$bestTune
