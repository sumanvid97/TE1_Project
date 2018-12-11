library(caret)

#give the complete location of the csv file
#data <- read.csv('~/Google Drive/Lectures - IITB/Semester 5/CE 310 Transport I/Project/daydata.csv', header=T, na.strings=c(""))
data <- read.csv('~/Google Drive/Lectures - IITB/Semester 5/CE 310 Transport I/Project/nightdata.csv', header=T, na.strings=c(""))

sapply(data, function(x) sum(is.na(x))) #checks for missing data (none)
n <- length(data$Accept)

set.seed(1226)

index <- sample(1:nrow(data),round(0.75*nrow(data))) # index for data splitting
train <- data[index,] # training set
test <- data[-index,] # test set

# 10-fold 3 times repeated cross-validation 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# grid for tuning the model
#nnetGrid = expand.grid(size = seq(3, 10, 1), decay = seq(0.01, 0.1, 0.01))
nnetGrid = expand.grid(size = 6, decay = 0.05) 

annmodel <- train( Accept ~ Speed..km.hr. + Spatial.Gap, data = train, method = "nnet",
                trControl=trctrl, preProcess = c("center", "scale"), 
                tuneGrid = nnetGrid)

prediction <- predict(annmodel, test) # predicting the gap acceptance from test dataset

accuracy = mean(prediction == test$Accept) # accuracy on the test set

# Confusion Matrix
cm <- confusionMatrix(prediction, test$Accept, mode = "prec_recall")
z <- cm$table[1, 1] #correct negatives
m <- cm$table[1, 2] #misses
f <- cm$table[2, 1] #false alarms
h <- cm$table[2, 2] #hits

# Skill Scores for Gap Acceptance Prediction
bias <- (f+h)/(m+h) # Bias
prec <- h/(h+f) # Precision
recall <- h/(m+h) # Recall
fmeasure <- 2*(prec*recall)/(prec + recall) # F-measure
far <- f/(f+h) # False Alarm Ratio
hss <- 2*(z*h - f*m)/((z+f)*(f+h) + (m+h)*(z+m)) # Heidke???s Skill Score


