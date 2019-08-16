##############################################################
# Author: Vishal Bakshi
# Project: Predict chances of release for marijuana possession
# Date: August 16th 2019
##############################################################

##install packages(to run this code) if required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")

##load libraries
library(tidyverse)
library(caret)


##Download and read "arrest records" dataset
arrest_data <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/carData/Arrests.csv")

##Analyze dataset attributes
str(arrest_data)

##Remove Column X  from dataset - this is simply the sequence no. of records
##and not useful for prediction/analysis
arrest_data <- arrest_data[,-1]

##Check for NAs in the dataset
summary(arrest_data)

##Partition dataset in to training and test sets
##Training set is 80% of dataset and test set is 20% of dataset

#set the seed to 1 - if using R 3.5 or earlier, use `set.seed(1)` instead
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = arrest_data$released, times = 1, p = 0.2, list = FALSE)
training_set <- arrest_data[-test_index,]
test_set <- arrest_data[test_index,]

#Let's run some simple visualizations by age, year and "previous checks" for
#quick insights
hist(training_set$age)
hist(training_set$year)
hist(training_set$checks)


############################
#Data Analysis
############################
##Now we will analyze data for some insights and store it in "insights" table
##For each of the insight calculation below, we calculate probability of a person
##not getting released if he/she met certain criteria
##For example: person is white, black, male, female etc.

#Check the probability of not getting released if person's colour is black
x1 <- sum(training_set$colour == "Black")
x2 <- sum(training_set$colour == "Black" & training_set$released == "No")
per <- x2/x1
insights <- data_frame(Criteria = "Person is Black", Probability_Not_Released = per)

#Check the probability of not getting released if person's colour is white
x1 <- sum(training_set$colour == "White")
x2 <- sum(training_set$colour == "White" & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                              data_frame(Criteria = "Person is White",  
                                         Probability_Not_Released = per))

#Check the probability of not getting released if person is male
x1 <- sum(training_set$sex == "Male")
x2 <- sum(training_set$sex == "Male" & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person is Male",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person is female
x1 <- sum(training_set$sex == "Female")
x2 <- sum(training_set$sex == "Female" & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person is Female",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person is employed
x1 <- sum(training_set$employed == "Yes")
x2 <- sum(training_set$employed == "Yes" & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person is Employed",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person is unemployed
x1 <- sum(training_set$employed == "No")
x2 <- sum(training_set$employed == "No" & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person is Not Employed",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person is canadian citizen
x1 <- sum(training_set$citizen == "Yes")
x2 <- sum(training_set$citizen == "Yes" & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person is Citizen",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person is non-citizen
x1 <- sum(training_set$citizen == "No")
x2 <- sum(training_set$citizen == "No" & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person is Non-Citizen",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person has 0 checks
x1 <- sum(training_set$checks == 0)
x2 <- sum(training_set$checks == 0 & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person has 0 checks",  
                                 Probability_Not_Released = per))


#Check the probability of not getting released if person has atleast 1 check
x1 <- sum(training_set$checks > 0)
x2 <- sum(training_set$checks > 0 & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person has atleast 1 check",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person has atleast 2 checks
x1 <- sum(training_set$checks > 1)
x2 <- sum(training_set$checks > 1 & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person has atleast 2 checks",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person has atleast 3 checks
x1 <- sum(training_set$checks > 2)
x2 <- sum(training_set$checks > 2 & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person has atleast 3 checks",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person has atleast 4 checks
x1 <- sum(training_set$checks > 3)
x2 <- sum(training_set$checks > 3 & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person has atleast 4 checks",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person has atleast 5 checks
x1 <- sum(training_set$checks > 4)
x2 <- sum(training_set$checks > 4 & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person has atleast 5 checks",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person is less than 18 
#years old i.e. he/she is not an adult
x1 <- sum(training_set$age < 18 )
x2 <- sum(training_set$age < 18 & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person's age is less than 18 years",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person is between 18 and 30 years old
x1 <- sum(training_set$age >=18 & training_set$age <= 30 )
x2 <- sum(training_set$age >= 18 & training_set$age <= 30 & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person's age is between 18 and 30 years",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released if person age is more than 30
x1 <- sum(training_set$age > 30)
x2 <- sum(training_set$age > 30 & training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "Person's age is more than 30 years",  
                                 Probability_Not_Released = per))

#Check the probability of not getting released across entire training set
x1 <- length(training_set$released)
x2 <- sum(training_set$released == "No")
per <- x2/x1
insights <- bind_rows(insights,
                      data_frame(Criteria = "All persons in entire training set",  
                                 Probability_Not_Released = per))

#####################
#End of Data Analysis
#####################

####################
#Modeling Methods
####################
##Now we will run some models and store the "accuracy" of each model
##in "accuracy_results" table

#Method 1 - Baseline model - since majority of people were released, predict 
#all as released against test dataset
pred <- "Yes"
accur <- sum(pred == test_set$released)/length(test_set$released)
accuracy_results <- data_frame(method = "Baseline - Predict all persons as released", Accuracy = accur)


#Method 2 - Predict using glm model across all predictors
train_glm <- train(released ~ ., method = "glm", data = training_set)
y_hat_glm <- predict(train_glm, test_set, type = "raw")
accur <- confusionMatrix(y_hat_glm, test_set$released)$overall[["Accuracy"]]
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(method="glm - all predictors",  
                                         Accuracy = accur))

#Method 3 - Predict using glm model across predictors - colour and checks
train_glm <- train(released ~ colour + checks, method = "glm", data = training_set)
y_hat_glm <- predict(train_glm, test_set, type = "raw")
accur <- confusionMatrix(y_hat_glm, test_set$released)$overall[["Accuracy"]]
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(method="glm - colour + checks",  
                                         Accuracy = accur))

#Method 4 - Predict using glm model across predictors - colour, 
#citizen, sex, age, checks
train_glm <- train(released ~ colour + citizen + checks + sex + age, method = "glm", data = training_set)
y_hat_glm <- predict(train_glm, test_set, type = "raw")
accur <- confusionMatrix(y_hat_glm, test_set$released)$overall[["Accuracy"]]
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(method="glm - color + citizen + sex + age + checks",  
                                         Accuracy = accur))

#Method 5 - Predict using knn model across all predictors
train_knn <- train(released ~ ., method = "knn", data = training_set)
y_hat_knn <- predict(train_knn, test_set, type = "raw")
accur <- confusionMatrix(y_hat_knn, test_set$released)$overall[["Accuracy"]]
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(method="knn - all predictors",  
                                         Accuracy = accur))

########################################################
#Print the Data Analysis and Model Peroformance Results
########################################################

#Output the results for insights table
insights %>% knitr::kable()

#Output the accuracy results for all models and show the best model 
#with highest accuracy value
accuracy_results %>% knitr::kable()
print(c("The highest accuracy is for knn model using all predictors. The accuracy value is:", accur))











