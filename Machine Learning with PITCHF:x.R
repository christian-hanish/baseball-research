# TOPIC: Performing Pitch Classification Using PITCHF/x Data
# METHOD: K-Nearest Neighbors Machine Learning Algorithm
# DATE: Fall 2019 (By Christian A. Hanish)

# Import data
pitchclassificationtrain <- read.csv("/Users/christianhanish/Desktop/New York Yankees/pitchclassificationtrain.csv")
pitchclassificationtest <- read.csv("/Users/christianhanish/Desktop/New York Yankees/pitchclassificationtest.csv")

# Display contents of data frame
str(pitchclassificationtrain)
str(pitchclassificationtest)


# -----Begin: pre-processing for the kNN algorithm-------------------------

# Convert pitch type to factor
pitchclassificationtrain$type <- as.factor(pitchclassificationtrain$type)

# Check conversion
str(pitchclassificationtrain)

# Display pitch type frequency
table(pitchclassificationtrain$type)

# Display first six observations
head(pitchclassificationtrain)

# Display summary of data
summary(pitchclassificationtrain[,c(1:11)])

# Create normalization function
normalize <- function(x) {
  + return( (x - min(x)) / (max(x) - min(x)) ) }

# Apply normalization function to training set (excluding year id because it would return NaN in testing set)
pitchclassificationtrain_n <- as.data.frame(lapply(pitchclassificationtrain[,c(2, 4:11)], normalize))

# Check for normalization of training set
str(pitchclassificationtrain_n)
summary(pitchclassificationtrain_n)

# Apply normalization function to testing set (excluding year id because it would return NaN)
pitchclassificationtest_n <- as.data.frame(lapply(pitchclassificationtest[,c(2, 4:11)], normalize))

# Check for normalization of testing set
str(pitchclassificationtest_n)
summary(pitchclassificationtest_n)

# -----End: pre-processing for the kNN algorithm-------------------------


# -----Begin: machine learning with kNN algorithm-------------------------

# Create new normalized training and testing sets
pitch_train <- pitchclassificationtrain_n
pitch_test <- pitchclassificationtest_n

# Determine training target feature (type)
pitch_train_target <- pitchclassificationtrain[, 12]

# Load class package
require(class)

# Determine k value (rule of thumb: sqrt of total observations in training set; preferably odd)
sqrt(12354)

# Increase limit of max.print to ensure all observations are predicted
options(max.print = 100000)

# Create kNN algorithm
pitch_knn <- knn(train = pitch_train, test = pitch_test, cl = pitch_train_target, k = 111)

# Run kNN algorithm
pitch_knn

# Export predictions as csv file
write.csv(pitch_knn, file = "ExportedPitchKNN.csv")

# -----End: machine learning with kNN algorithm-------------------------


# ----- Begin: testing accuracy of kNN algorithm on testing set-------------------------

# Divide training set into training and testing subsets using 75:25 ratio rule
pitch_train_train <- pitchclassificationtrain_n[1:7985,]
pitch_train_test <- pitchclassificationtrain_n[7986:10647,]

# Determine target features for training and testing subsets (type)
pitch_train_train_target <- pitchclassificationtrain[1:7985, 12]
pitch_train_test_target <- pitchclassificationtrain[7986:10647, 12]

# Load class package
require(class)

# Determine k value (rule of thumb: sqrt of total observations in training set; preferably odd)
sqrt(10647)

# Increase limit of max.print to ensure all observations are predicted
options(max.print = 100000)

# Create kNN algorithm of testing set
pitch_knn_accuracy <- knn(train = pitch_train_train, test = pitch_train_test, cl = pitch_train_train_target, k = 103)

# Run kNN algorithm on testing set
pitch_knn_accuracy

# Check accuracy of kNN algorithm on testing set (x axis: pitch type predictions, y axis: actual pitch types)
table(pitch_train_test_target, pitch_knn_accuracy)

# ----- End: testing accuracy of kNN algorithm on testing set-------------------------

