# loading required Libraries
library(tidyverse)  # data manipulation
library(data.table) # fast file reading
library(caret)      # rocr analysis
library(rpart.plot) # decision tree plotting
library(caTools)    # Contains several basic utility functions like split, moving etc. 
library(class)      # Print a Vector of class names
library(C50)        # Generate decision Tree
library(funModeling) # imports libraries like ggplot2,rpart,dplyr,Hmisc,tidyr,RColorBrewer,reshape2
library(gmodels)    # Display the frequencies and relative frequencies of observations
library(psych)      # Used to check correlation between two variables
library(epiDisplay) # Data exploration and result presentation
library(plyr)       # Split the data apart , a common data manipulation step
library(gridExtra)  # Work with grid graphic objects,provides useful extension to the grid system.

# Loading the Data-set including a parameter 'sep' by semicolon';'.

Bank <- read.csv("D:/Semester-2-CourseWork/Assignment-DataSets/Task1/Bank.csv", header=TRUE, sep=";")
dim(Bank) # Shape of Dataset
attach(Bank) # By attaching the dataset we don't need to declare the dataset each time for performing actions.
head(Bank) # Top 5 records
View(Bank) # Bottom 5 records
nrow(Bank) # No of rows in Dataset
sum(duplicated(Bank)) # Checking Duplicate rows
sum(!complete.cases(Bank)) # No of Rows containing missing values
str(Bank) # Structure of Dataset
prop.table(table(Bank$y)) # Observe that the dataset predicted outcome (y) is skewed towards 'no' with over 88%.

summary(Bank$age) # Gives statistical Analysis of the data

########################################### Visualizing using different plot ##################################################

plt = ggplot(Bank)

age1 = plt + geom_histogram(aes(x=age),color="black", fill="yellow", binwidth = 5) +
  ggtitle('Age Distribution (red line is Mean line)') +
  ylab('Count') +
  xlab('Age') +
  geom_vline(aes(xintercept = mean(age), color = "red")) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  theme(legend.position = "none")

age2 = plt + geom_boxplot(aes(x='', y=age),color="black", fill="violet") +
  ggtitle('Age Boxplot') +
  ylab('Age')

grid.arrange(age1, age2, ncol = 2)

# Note : Box plot of age describes essentially the same statistics but we can see outliers above the age of 65.

# Outliers treatment for Age using quartiles.

bp = boxplot(Bank$age,col="red",main = "Box Plot of Age before Outliers Treatment")

x <- Bank$age
QRT <- quantile(x, probs=c(.25, .75), na.rm = T)
IR <- 1.5 * IQR(x, na.rm = T)
Q3 <- IR + QRT[2]
Q1 <- QRT[1] - IR
Bank <- Bank[which(Bank$age>Q1 & Bank$age<Q3),]

bp1 = boxplot(Bank$age,col="red",main = "Box Plot of Age after Outliers Treatment")

# Age Vs Subscription
Age_mean <- Bank %>% group_by(y) %>% summarise(grp.mean=mean(age))

ggplot (Bank, aes(x=age)) + 
  geom_histogram(color = "orange", fill = "orange", binwidth = 5) +
  facet_grid(cols=vars(y)) + 
  ggtitle('Age Distribution by Subscription') + ylab('Count') + xlab('Age') +
  scale_x_continuous(breaks = seq(0,100,5)) +
  geom_vline(data=Age_mean, aes(xintercept=grp.mean), color="red", linetype="dashed")

# Job
count(Bank,'job')
tab1(Bank$job, sort.group = "decreasing", cum.percent = TRUE)

# Job Vs Subscription
ggplot(data = Bank, aes(x=job, fill=y)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription Vs Job") +
  xlab(" Job") +
  guides(fill=guide_legend(title="Term Deposit Subscription"))

# Marital Status Vs Subscription
ggplot(data = Bank, aes(x=marital, fill=y)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription Vs Marital") +
  xlab(" Marital Status") +
  guides(fill=guide_legend(title="Term Deposit Subscription"))

# Job Vs Education
ggplot(data = Bank, aes(x=job, fill=education)) +
  geom_bar() +
  ggtitle("Job Vs Education") +
  xlab(" ") +
  guides(fill=guide_legend(title="Education Level"))

# Education Vs Subscription
ggplot(data = Bank, aes(x=education, fill=y)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription Vs Education Level") +
  xlab(" Education Level") +
  guides(fill=guide_legend(title="Term Deposit Subscription"))


# Histogram of Balance
ggplot (Bank, aes(x=balance)) +
  geom_histogram(color = "black", fill = "blue4") +
  ggtitle('Histogram Of Balance')

# Month Vs Subscription
df <- table(Bank$month, Bank$y)
tab <- as.data.frame(prop.table(df, 2))
colnames(tab) <-  c("month", "y", "perc")

ggplot(data = tab, aes(x = month, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) + 
  xlab("Month")+
  ylab("Percent")

#Duration Vs Subscription
Duration_mean <- Bank %>% group_by(y) %>% summarise(grp2.mean=mean(duration))

plt_duration <- ggplot(Bank, aes(x=duration, fill = y)) +
  geom_histogram(binwidth = 10) +
  facet_grid(cols = vars(y)) +
  coord_cartesian(xlim = c(0,5000), ylim = c(0,400))

plt_duration + geom_vline(data = Duration_mean, aes(xintercept = grp2.mean), color = "blue", linetype = "dashed")

# BarChart of Contact
barchart(Bank$contact,horizontal = F , col = "chartreuse" , main = "Barchart of Contact")

# Correlation plot for duration,p-days,previous,day,campaign
library(corrplot)
Bank_Correlation <- select_if(Bank, is.numeric) %>% cor()
corrplot(Bank_Correlation, method = "number")

# Subscription based on Number of Contacts during the Campaign
ggplot(data=Bank, aes(x=campaign, fill = y))+
  geom_histogram()+
  facet_grid(cols=vars(y)) +
  ggtitle("Subscription Vs Number of Contacts during the Campaign")+
  xlab("Number of Contacts during the Campaign")+
  xlim(c(min=1,max=25)) +
  guides(fill=guide_legend(title="Term Deposit Subscription"))

########################################## Decision Tree Model Evaluation ###########################################

Bank <- read.csv("D:/Semester-2-CourseWork/Assignment-DataSets/Task1/Bank.csv", header=TRUE, sep=";")
attach(Bank)
# Shuffling the data
Bank_rand <- Bank[order(runif(45211)), ]
str(Bank_rand)

# splitting the data
Bank_train <- Bank_rand[1:39000, ]
Bank_test  <- Bank_rand[39001:45211, ]

# checking the proportion of class variable
prop.table(table(Bank_rand$y))
prop.table(table(Bank_train$y))
prop.table(table(Bank_test$y))

# Step 3: Training a model on the data

Bank_model <- C5.0(Bank_train[, -17], Bank_train$y)

model <- rpart(Bank_train$y ~ ., data = Bank_train,
               control = rpart.control(cp = 0, maxdepth = 3))

# Plot Decision Tree
rpart.plot(model, box.palette = "auto", digits = -3)


# Display detailed information about the tree
summary(Bank_model)

# Step 4: Evaluating model performance
# Test data accuracy
test_res <- predict(Bank_model, Bank_test)
test_acc <- mean(Bank_test$y == test_res)
test_acc

# Cross tabulation of predicted v/s actual classes
CrossTable(Bank_test$y, test_res, dnn = c('Actual Subscription', 'Predicted Subscription'))

# On Training Dataset
train_res <- predict(Bank_model, Bank_train)
train_acc <- mean(Bank_train$y == train_res)
train_acc

CrossTable(Bank_train$y, train_res, dnn = c('Actual Subscription', 'Predicted Subscription'))

############ Creating dummy variables for each categorical variables for creating Logistic Regression Model to Compare Accuracy ########

job<- model.matrix(~job, data = Bank)[,-1]
marital = model.matrix(~marital, data = Bank)[,-1]
education = model.matrix(~education, data = Bank)[,-1]
default = model.matrix(~default, data = Bank)[,-1]
housing = model.matrix(~housing, data = Bank)[,-1]
loan = model.matrix(~loan, data = Bank)[,-1]
contact = model.matrix(~contact, data = Bank)[,-1]
month = model.matrix(~month, data = Bank)[,-1]
poutcome = model.matrix(~poutcome, data = Bank)[,-1]

# Combining the created dummy variables above and the remaining variables and created new dataset as 'bank_new'
bank_new = cbind(Bank$age, job,marital,education,default, Bank$balance, housing,loan,contact, Bank$day, month, Bank[,12:15],poutcome)
y = model.matrix(~y, data = Bank)[,-1]
Bank = cbind (bank_new, y)


####################################### Logistic Regression Model Evaluation ########################################

model <- glm(y ~ ., data = Bank, family = "binomial") 
summary(model)

# To calculate the odds ratio manually we are going to take exp of coef(model)
exp(coef(model))

# Checking Prediction on model validation
prob <- predict(model, Bank, type = "response")
prob

# Data Partitioning
set.seed(1234)
n <-  nrow(Bank)
n1 <-  n * 0.85
n2 <-  n - n1
train_index <-  sample(1:n, n1)
train <- Bank[train_index, ]
test <-  Bank[-train_index, ]

# Train the model using Training data
finalmodel <- glm(y ~ ., data = train, family = "binomial")
summary(finalmodel)

# Prediction on test data
prob_test <- predict(model, newdata = test, type = "response")
prob_test

# Confusion matrix 
confusion <- table(prob_test > 0.5, test$y)
confusion

# Model Accuracy on Test Data
Accuracy <- sum(diag(confusion)/sum(confusion))
Accuracy # 0.9047479

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL

pred_values <- ifelse(prob_test > 0.5, 1, 0)

# Creating new column to store the above values
test[,"prob"] <- prob_test
test[,"pred_values"] <- pred_values

table(test$y, test$pred_values)

# Compare the model performance on Train data
# Prediction on test data
prob_train <- predict(model, newdata = train, type = "response")
prob_train

# Confusion matrix 
confusion_train <- table(prob_train > 0.5, train$y)
confusion_train

# Model Accuracy on Train Data
Acc_train <- sum(diag(confusion_train)/sum(confusion_train))
Acc_train # 0.9013245

# Calculating Additional metrics below for precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity

# ROC Curve is used to evaluate the betterness of the logistic model the more the area under ROC curve,the better is the model 
# We will not only use ROC curve for logistic regression but it can apply for any classification techniques.

rocrpred <- prediction(prob, Bank$y)
rocrperf <- performance(rocrpred, 'tpr', 'fpr')
str(rocrperf)
plot(rocrperf, colorize=T, text.adj=c(-0.2,1.7))
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]], fpr=rocrperf@x.values, tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off, 6)

# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff, desc(TPR))
View(rocr_cutoff)

