#Cascading approach to the Wifi problem

#Loading packages ----

pacman::p_load(dplyr,caret,ggplot2,tidyr,utils,matrixStats,sf,viridis, 
               graphics,ranger,plotly,FNN,h2o)




#load the training and validation data set ----

raw_training_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/trainingData.csv')

raw_validation_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/validationData.csv')




#Preprocessing ----

#First we create the first cascading approach to the modelling, observe the errors and preprocess 
#as we get feedback from the problem. first general cascading approach with simple ranger model
#as it showed the best resutls. Given the variables, target predicted variable will be floor 
#as we update the dtable with predictions of information that might help the model find the floor
#more accurately

#Pre processing Ideas

#1. remove first look useless variables 
#2. transform missing values to '0'
#3. standarize the wap measures
#4. Principal component analysis
#5. apply changes to data set depending on PCA results



#Ranger cascade modelling ----

#First we create the training and test samples

intraining_raw <- createDataPartition(raw_training_data_set$SPACEID,
                                      p=0.70,
                                      list = FALSE) 

training_raw <- raw_training_data_set[intraining_raw,]

testing_raw <- raw_training_data_set[-intraining_raw,]


#We train our first model to predict longitude first as is the most important feature, this will
#help us define what building we are in and to choose the latitude better

#longitude prediction

#training set

# training_random_forest_longitude <- ranger(formula = training_raw[,521]~.,
#                                             data = training_raw[,1:520],
#                                             verbose = T)
# 
# saveRDS(training_random_forest_longitude,'training_random_forest_longitude.rds')

training_random_forest_longitude <- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/training_random_forest_longitude.rds')

random_forest_training_longitude_predictions <- predict(training_random_forest_longitude,testing_raw)

#postResample(random_forest_training_longitude_predictions$predictions,testing_raw[,521]) 

#train first try

#      RMSE  Rsquared       MAE 
# 8.6417893 0.9951338 5.3803643  

#

 
#validation set

# validation_random_forest_longitude <- ranger(formula = raw_training_data_set[,521]~.,
#                                             data = raw_training_data_set[,1:520],
#                                             verbose = T)
# 
# saveRDS(validation_random_forest_longitude,'validation_random_forest_longitude.rds')

validation_random_forest_longitude <- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/validation_random_forest_longitude.rds')   

random_forest_validation_longitude_predictions <- predict(validation_random_forest_longitude,raw_validation_data_set)

#postResample(random_forest_validation_longitude_predictions$predictions,raw_validation_data_set[,521]) 

#first try ranger over validation set 

# RMSE   Rsquared        MAE 
# 12.1269590  0.9901161  8.6797150 

#Accuracy decreases. given the importance of the errors on cascading models, will re try after
#pre processing

#validation error analysis 


error_table_longitude <- data.frame(predicted = random_forest_validation_longitude_predictions$predictions,
                                    real = raw_validation_data_set[,521])


error_table_longitude <- error_table_longitude %>%  mutate(real_error = (real-predicted),
                                  percentual_error = (real-predicted)/real)


ggplot(error_table_longitude, aes(x = real, y = real_error)) +
  geom_point(colour = 'blue')

ggplot(error_table_longitude, aes(x = real, y = percentual_error)) +
  geom_point(colour = 'blue')

ggplot(error_table_longitude, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')

ggplot(error_table_longitude, aes(x = real_error)) +
  geom_histogram(colour = 'blue')


#first try
#Biggest percentual error seems to be happening on the third building but there isn't
#a high bias between the buildings, outliers present in all three. Pretty normal 
#distribution for the error too. Cause for higher RMSE seems to be the outliers


#latitude prediction  

#we create the data sets that we are going to cascade and with
                            
#cascade_validation_data_set <- cbind(raw_validation_data_set[,1:520],error_table_longitude$predicted)




#adding longitude predictions



