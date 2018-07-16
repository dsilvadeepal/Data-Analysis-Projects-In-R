library(readr)
library(caTools)
library(e1071)
library(rpart)
library(rpart.plot)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(stringr)
library(randomForest)
Message <- read.csv("spam.csv", stringsAsFactors = F)
str(Message)
Message$X <- NULL
Message$X.1 <- NULL
Message$X.2 <- NULL
names(Message) <- c("Tag","messages")
levels(as.factor(Message$Tag))

#Changing the Labels
Message$Tag[Message$Tag == "ham"] <- "non-spam"
Message$Tag[Message$Tag == "spam"] <- "spam"
Message$Tag <- factor(Message$Tag)

#Seperating the message into words
cloudOfWords <- Corpus(VectorSource(Message$messages))
cloudOfWords <- tm_map(cloudOfWords, tolower)
cloudOfWords <- tm_map(cloudOfWords, removePunctuation)
cloudOfWords <- tm_map(cloudOfWords, removeWords, c(stopwords("english")))
cloudOfWords <- tm_map(cloudOfWords, stripWhitespace)
cloudOfWords <- tm_map(cloudOfWords, stemDocument)

#Extracting the Frequent words
frequency <- DocumentTermMatrix(cloudOfWords)
findFreqTerms(frequency, lowfreq = 200)
ScatteredWords <- removeSparseTerms(frequency, 0.995)
ScatteredWords <- as.data.frame(as.matrix(ScatteredWords))
colnames(ScatteredWords) <- make.names(colnames(ScatteredWords))
str(ScatteredWords)
ScatteredWords$Tag <- Message$Tag

#Distribution of Data in test and train
set.seed(16107608)
split <- sample.split(ScatteredWords$Tag, SplitRatio = 0.75)
train <- subset(ScatteredWords, split == T)
test <- subset(ScatteredWords, split == F)

#Decision Tree algorithm
DecisionTree <- rpart(Tag ~ ., data = train, method = "class", minbucket = 35)
prp(DecisionTree)
DecisionTree <- predict(DecisionTree, test, type = "class")
table(test$Tag, DecisionTree)
rpart.accuracy.table <- as.data.frame(table(test$Tag, DecisionTree))
print(paste("Decision Tree Accuracy is ", 100*round(((rpart.accuracy.table$Freq[1]+rpart.accuracy.table$Freq[4])/nrow(test)), 4), "%"))

#Random Forest Algorithm
RdmForest <-randomForest(Tag ~ ., data = train, importance=TRUE,ntree=35)
RdmForest = predict(RdmForest, test)
table(test$Tag, RdmForest)
randomForest.accuracy.table <- as.data.frame(table(test$Tag, RdmForest))
print(paste("randomForest accuracy is ", 100*round(((randomForest.accuracy.table$Freq[1]+randomForest.accuracy.table$Freq[4])/nrow(test)), 4), "%"))

#Support Vector Machine algorithm
Sprtvm <- svm(Tag ~ ., data = train, kernel = "linear", cost = 0.1, gamma = 0.1)
Sprtvm <- predict(Sprtvm, test)
table(test$Tag, Sprtvm)
svm.accuracy.table <- as.data.frame(table(test$Tag, Sprtvm))
print(paste("SVM accuracy is ", 100*round(((svm.accuracy.table$Freq[1]+svm.accuracy.table$Freq[4])/nrow(test)), 4), "%"))