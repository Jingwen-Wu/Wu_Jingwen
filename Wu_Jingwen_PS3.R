#把系统分配给R语言的内存使用上限调到100G
memory.limit(102400)

library(readr)
wine <- read_csv("wine.csv")
View(wine)


#2(a)
# Data pre-processing

#set a high_quality column as Y
wine$high_quality = 0
wine$high_quality[wine$quality > 5] = 1
wine_high_qulaity = subset(wine, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar",
                                              "chlorides","free.sulfur.dioxide","total.sulfur.dioxide",
                                              "density","pH","sulphates","alcohol","high_quality"))

# Random sampling into training, validation, and testing
set.seed(123)
training.rows <- sample(1:nrow(wine_high_qulaity), nrow(wine_high_qulaity)*0.6)
wine_quality_training = wine_high_qulaity[training.rows,]
wine_quality_vt = wine_high_qulaity[-training.rows,]

validation.rows <- sample(1:nrow(wine_quality_vt),nrow(wine_quality_vt)*0.5)
wine_quality_validation = wine_quality_vt[validation.rows,]
wine_quality_testing = wine_quality_vt[-validation.rows,]

# Check whether the rows of training+validation+testing equals to the total number
nrow(wine_high_qulaity) == nrow(wine_quality_training)+nrow(wine_quality_validation)+
  nrow(wine_quality_testing)


#(i) Logistic Regression Model
# Basic logistic regression model trained on training set
# glm:generalize linear model; family=binomial:use logistic regression
model_lrg = glm(high_quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
              chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
              density+pH+sulphates+alcohol, 
            data=wine_quality_training,family=binomial)
summary(model_lrg)

# Make predictions on the validation set
# The type="response" option tells R to output probabilities of the form P(Y = 1|X), 
#                                 as opposed to other information such as the logit.
predictVal_lrg = predict(model_lrg, type="response", newdata = wine_quality_validation)

# Analyze predictions
summary(predictVal_lrg)

# Confusion matrix for threshold of 0.5
table(wine_quality_validation$high_quality, predictVal_lrg > 0.5)

# Calculate the overall accuracy
accuracy_lrg = (159+581)/(159+176+64+581)
accuracy_lrg


# (ii)k−NN model
# Create the training data set
# Scaling:(x-mean(x))/sd(x)
f_a = (wine_quality_training$fixed.acidity-mean(wine_quality_training$fixed.acidity))/sd(wine_quality_training$fixed.acidity)
v_a = (wine_quality_training$volatile.acidity-mean(wine_quality_training$volatile.acidity))/sd(wine_quality_training$volatile.acidity)
c_a = (wine_quality_training$citric.acid-mean(wine_quality_training$citric.acid))/sd(wine_quality_training$citric.acid)
r_s = (wine_quality_training$residual.sugar-mean(wine_quality_training$residual.sugar))/sd(wine_quality_training$residual.sugar)
chl = (wine_quality_training$chlorides-mean(wine_quality_training$chlorides))/sd(wine_quality_training$chlorides)
f_s_d = (wine_quality_training$free.sulfur.dioxide-mean(wine_quality_training$free.sulfur.dioxide))/sd(wine_quality_training$free.sulfur.dioxide)
t_s_d = (wine_quality_training$total.sulfur.dioxide-mean(wine_quality_training$total.sulfur.dioxide))/sd(wine_quality_training$total.sulfur.dioxide)
den = (wine_quality_training$density-mean(wine_quality_training$density))/sd(wine_quality_training$density)
ph = (wine_quality_training$pH-mean(wine_quality_training$pH))/sd(wine_quality_training$pH)
sul = (wine_quality_training$sulphates-mean(wine_quality_training$sulphates))/sd(wine_quality_training$sulphates)
alc = (wine_quality_training$alcohol-mean(wine_quality_training$alcohol))/sd(wine_quality_training$alcohol)
h_q = as.factor(wine_quality_training$high_quality)

knn_train=data.frame(f_a,v_a,c_a,r_s,chl,f_s_d,t_s_d,den,ph,sul,alc)
knn_training=data.frame(f_a,v_a,c_a,r_s,chl,f_s_d,t_s_d,den,ph,sul,alc,h_q)

# Create the validation data set
# Scaling:(x-mean(x))/sd(x)
f_a = (wine_quality_validation$fixed.acidity-mean(wine_quality_validation$fixed.acidity))/sd(wine_quality_validation$fixed.acidity)
v_a = (wine_quality_validation$volatile.acidity-mean(wine_quality_validation$volatile.acidity))/sd(wine_quality_validation$volatile.acidity)
c_a = (wine_quality_validation$citric.acid-mean(wine_quality_validation$citric.acid))/sd(wine_quality_validation$citric.acid)
r_s = (wine_quality_validation$residual.sugar-mean(wine_quality_validation$residual.sugar))/sd(wine_quality_validation$residual.sugar)
chl = (wine_quality_validation$chlorides-mean(wine_quality_validation$chlorides))/sd(wine_quality_validation$chlorides)
f_s_d = (wine_quality_validation$free.sulfur.dioxide-mean(wine_quality_validation$free.sulfur.dioxide))/sd(wine_quality_validation$free.sulfur.dioxide)
t_s_d = (wine_quality_validation$total.sulfur.dioxide-mean(wine_quality_validation$total.sulfur.dioxide))/sd(wine_quality_validation$total.sulfur.dioxide)
den = (wine_quality_validation$density-mean(wine_quality_validation$density))/sd(wine_quality_validation$density)
ph = (wine_quality_validation$pH-mean(wine_quality_validation$pH))/sd(wine_quality_validation$pH)
sul = (wine_quality_validation$sulphates-mean(wine_quality_validation$sulphates))/sd(wine_quality_validation$sulphates)
alc = (wine_quality_validation$alcohol-mean(wine_quality_validation$alcohol))/sd(wine_quality_validation$alcohol)
h_q = as.factor(wine_quality_validation$high_quality)

knn_valid=data.frame(f_a,v_a,c_a,r_s,chl,f_s_d,t_s_d,den,ph,sul,alc)
knn_validation=data.frame(f_a,v_a,c_a,r_s,chl,f_s_d,t_s_d,den,ph,sul,alc,h_q)

# Test the overall accuracy on the testing set.
accuracy_knn = function(actual, predicted) {
  return(mean(actual == predicted))
}

# load the package 'class'
library(class)

#Make predictions on validation set using kNN
predicted_validation = knn(train = knn_train, test = knn_valid, cl = knn_training$h_q, k = 5)
high_quality_validation = knn_validation$h_q

#Confusion Matrix
table(high_quality_validation, predicted_validation)

#Calculate the accuracy of kNN
accuracy_knn(actual = high_quality_validation,predicted = predicted_validation)


# Choose KNN model
# Create the testing data set
# Scaling:(x-mean(x))/sd(x)
f_a = (wine_quality_testing$fixed.acidity-mean(wine_quality_testing$fixed.acidity))/sd(wine_quality_testing$fixed.acidity)
v_a = (wine_quality_testing$volatile.acidity-mean(wine_quality_testing$volatile.acidity))/sd(wine_quality_testing$volatile.acidity)
c_a = (wine_quality_testing$citric.acid-mean(wine_quality_testing$citric.acid))/sd(wine_quality_testing$citric.acid)
r_s = (wine_quality_testing$residual.sugar-mean(wine_quality_testing$residual.sugar))/sd(wine_quality_testing$residual.sugar)
chl = (wine_quality_testing$chlorides-mean(wine_quality_testing$chlorides))/sd(wine_quality_testing$chlorides)
f_s_d = (wine_quality_testing$free.sulfur.dioxide-mean(wine_quality_testing$free.sulfur.dioxide))/sd(wine_quality_testing$free.sulfur.dioxide)
t_s_d = (wine_quality_testing$total.sulfur.dioxide-mean(wine_quality_testing$total.sulfur.dioxide))/sd(wine_quality_testing$total.sulfur.dioxide)
den = (wine_quality_testing$density-mean(wine_quality_testing$density))/sd(wine_quality_testing$density)
ph = (wine_quality_testing$pH-mean(wine_quality_testing$pH))/sd(wine_quality_testing$pH)
sul = (wine_quality_testing$sulphates-mean(wine_quality_testing$sulphates))/sd(wine_quality_testing$sulphates)
alc = (wine_quality_testing$alcohol-mean(wine_quality_testing$alcohol))/sd(wine_quality_testing$alcohol)
h_q = as.factor(wine_quality_testing$high_quality)

knn_test=data.frame(f_a,v_a,c_a,r_s,chl,f_s_d,t_s_d,den,ph,sul,alc)
knn_testing=data.frame(f_a,v_a,c_a,r_s,chl,f_s_d,t_s_d,den,ph,sul,alc,h_q)

#Make predictions on testing set using kNN
predicted_testing = knn(train = knn_valid, test = knn_test, cl = knn_validation$h_q, k = 5)
high_quality_testing = knn_testing$h_q
 
#Calculate the accuracy of kNN
accuracy_knn(actual = high_quality_testing,predicted = predicted_testing)




# 2(b)
# Re-split the testing set 
set.seed(8)
training.rows <- sample(1:nrow(wine_high_qulaity), nrow(wine_high_qulaity)*0.7)
wine_quality_training = wine_high_qulaity[training.rows,]
wine_quality_testing = wine_high_qulaity[-training.rows,]


##Cross Validation for K-nn Model

#Load necessary packages for Cross Validation
library(caret)
library(e1071)

# Create the training data set
# Scaling:(x-mean(x))/sd(x)
f_a = (wine_quality_training$fixed.acidity-mean(wine_quality_training$fixed.acidity))/sd(wine_quality_training$fixed.acidity)
v_a = (wine_quality_training$volatile.acidity-mean(wine_quality_training$volatile.acidity))/sd(wine_quality_training$volatile.acidity)
c_a = (wine_quality_training$citric.acid-mean(wine_quality_training$citric.acid))/sd(wine_quality_training$citric.acid)
r_s = (wine_quality_training$residual.sugar-mean(wine_quality_training$residual.sugar))/sd(wine_quality_training$residual.sugar)
chl = (wine_quality_training$chlorides-mean(wine_quality_training$chlorides))/sd(wine_quality_training$chlorides)
f_s_d = (wine_quality_training$free.sulfur.dioxide-mean(wine_quality_training$free.sulfur.dioxide))/sd(wine_quality_training$free.sulfur.dioxide)
t_s_d = (wine_quality_training$total.sulfur.dioxide-mean(wine_quality_training$total.sulfur.dioxide))/sd(wine_quality_training$total.sulfur.dioxide)
den = (wine_quality_training$density-mean(wine_quality_training$density))/sd(wine_quality_training$density)
ph = (wine_quality_training$pH-mean(wine_quality_training$pH))/sd(wine_quality_training$pH)
sul = (wine_quality_training$sulphates-mean(wine_quality_training$sulphates))/sd(wine_quality_training$sulphates)
alc = (wine_quality_training$alcohol-mean(wine_quality_training$alcohol))/sd(wine_quality_training$alcohol)
h_q = as.factor(wine_quality_training$high_quality)

knn_train=data.frame(f_a,v_a,c_a,r_s,chl,f_s_d,t_s_d,den,ph,sul,alc)
knn_training=data.frame(f_a,v_a,c_a,r_s,chl,f_s_d,t_s_d,den,ph,sul,alc,h_q)

# Create the testing data set
# Scaling:(x-mean(x))/sd(x)
f_a = (wine_quality_testing$fixed.acidity-mean(wine_quality_testing$fixed.acidity))/sd(wine_quality_testing$fixed.acidity)
v_a = (wine_quality_testing$volatile.acidity-mean(wine_quality_testing$volatile.acidity))/sd(wine_quality_testing$volatile.acidity)
c_a = (wine_quality_testing$citric.acid-mean(wine_quality_testing$citric.acid))/sd(wine_quality_testing$citric.acid)
r_s = (wine_quality_testing$residual.sugar-mean(wine_quality_testing$residual.sugar))/sd(wine_quality_testing$residual.sugar)
chl = (wine_quality_testing$chlorides-mean(wine_quality_testing$chlorides))/sd(wine_quality_testing$chlorides)
f_s_d = (wine_quality_testing$free.sulfur.dioxide-mean(wine_quality_testing$free.sulfur.dioxide))/sd(wine_quality_testing$free.sulfur.dioxide)
t_s_d = (wine_quality_testing$total.sulfur.dioxide-mean(wine_quality_testing$total.sulfur.dioxide))/sd(wine_quality_testing$total.sulfur.dioxide)
den = (wine_quality_testing$density-mean(wine_quality_testing$density))/sd(wine_quality_testing$density)
ph = (wine_quality_testing$pH-mean(wine_quality_testing$pH))/sd(wine_quality_testing$pH)
sul = (wine_quality_testing$sulphates-mean(wine_quality_testing$sulphates))/sd(wine_quality_testing$sulphates)
alc = (wine_quality_testing$alcohol-mean(wine_quality_testing$alcohol))/sd(wine_quality_testing$alcohol)
h_q = as.factor(wine_quality_testing$high_quality)

knn_test=data.frame(f_a,v_a,c_a,r_s,chl,f_s_d,t_s_d,den,ph,sul,alc)
knn_testing=data.frame(f_a,v_a,c_a,r_s,chl,f_s_d,t_s_d,den,ph,sul,alc,h_q)

# 6-fold cross validation
trControl <- trainControl(method  = "cv", number = 6)

fit_cv <- train(h_q ~ ., method = "knn", tuneGrid = expand.grid(k = 1:10),metric="Accuracy", 
                trControl = trControl,data=knn_training)


fit_cv

# The best model is k=1.
#Make predictions on testing set using kNN
predicted_testing = knn(train = knn_train, test = knn_test, cl = knn_training$h_q, k = 1)
high_quality_testing = knn_testing$h_q

#Calculate the accuracy of kNN
accuracy_knn(actual = high_quality_testing,predicted = predicted_testing)
