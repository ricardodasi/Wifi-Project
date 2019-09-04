
#Loading packages ----

pacman::p_load(dplyr,caret,ggplot2,tidyr,utils,matrixStats,sf,viridis, 
               graphics,ranger,plotly,FNN,h2o,plyr,anchors, psycho, data.table,
               stats)





#load the training and validation data set ----

raw_training_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/trainingData.csv')

raw_validation_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/validationData.csv')

#creating cascading data set

cascading_train_data_set <- raw_training_data_set 

cascading_validation_data_set <- raw_validation_data_set


#Preprocessing ----

#First we create the first cascading approach to the modelling, observe the errors and preprocess 
#as we get feedback from the problem. first general cascading approach with simple ranger model
#as it showed the best resutls. Given the variables, target predicted variable will be floor 
#as we update the dtable with predictions of information that might help the model find the floor
#more accurately

#Pre processing Ideas

#1. removing zero var variables  (data cleaning

#removing zero var variables

cascading_train_data_set <- 
  cascading_train_data_set[,-which(apply(cascading_train_data_set,2,var)==0)]

#removing variables that are not going to be used

cascading_train_data_set <- 
  cascading_train_data_set[,-c(474,473,472,471,470)]

#2. transform missing values to '0' 

#Verified that the data set did not contain already values assigned to '0'

# list_of_waps <- stack(raw_training_data_set[,1:520])
# 
# list_of_waps <- list_of_waps %>% filter(values < 100 )

#replaced missing values and changing magnitude

cascading_train_data_set[,1:465] <- cascading_train_data_set[,1:465] + 105

cascading_validation_data_set[,1:520] <- cascading_validation_data_set[,1:520] + 105


cascading_train_data_set <- replace.value( data = cascading_train_data_set,
                                          names = colnames(cascading_train_data_set[1:465]),
                                          from = 205 ,
                                          to  = 0,
                                          verbose = FALSE)



cascading_validation_data_set <- replace.value( data = cascading_validation_data_set,
                                          names = colnames(cascading_validation_data_set[1:520]),
                                          from = 205 ,
                                          to  = 0,
                                          verbose = FALSE)


#3. standarize the wap measures 



#we do this by row, given that each user behaviour with the wap measures might be different
#depending on the kind of cellphone and we want them all in the same scale so the model 
#can recognize the position by waps and not specific measures


# cascading_train_data_set[,1:465]<- transpose(as.data.frame(apply(cascading_train_data_set[,1:465],
#                                                                   1,
#                                                                   function(x){standardize(as.numeric(as.vector(x)))})))
# 
# 
# names(cascading_train_data_set[,1:465]) <- names(raw_training_data_set[,1:465])
# 
# 
# cascading_validation_data_set[,1:520]<- transpose(as.data.frame(apply(cascading_validation_data_set[,1:520],
#                                                                  1,
#                                                                  function(x){standardize(as.numeric(as.vector(x)))})))
# 
# names(cascading_validation_data_set [,1:520]) <- names(cascading_validation_data_set [,1:520])




#4. Principal component analysis



#5. apply changes to data set depending on PCA results




#6. Changing variable catergories

cascading_train_data_set$BUILDINGID <- as.factor(cascading_train_data_set$BUILDINGID)

cascading_train_data_set$FLOOR <- as.factor(cascading_train_data_set$FLOOR)

cascading_validation_data_set$BUILDINGID <- as.factor(cascading_validation_data_set$BUILDINGID)

cascading_validation_data_set$FLOOR <- as.factor(cascading_validation_data_set$FLOOR)



#exploration ----

#exploring the distribution of the dependant variables

#Building

ggplot(raw_training_data_set, aes(x = BUILDINGID)) +
  geom_histogram(colour = 'blue')

ggplot(raw_validation_data_set, aes(x = BUILDINGID)) +
  geom_histogram(colour = 'blue')

#training set has more observations of building 2, while validation set has more 
#observations of building 0

ggplot(raw_training_data_set, aes(x = FLOOR)) +
  geom_histogram(colour = 'blue')

ggplot(raw_validation_data_set, aes(x = FLOOR)) +
  geom_histogram(colour = 'blue')

#training set floors are fairly equal except for the 4th floor wich is only present
#in building 2 so it is good. observations by floor are more evenly distributed in
#validation set 

ggplot(raw_training_data_set, aes(x = LONGITUDE)) +
  geom_histogram(colour = 'blue',
                 bins = 70)

ggplot(raw_validation_data_set, aes(x = LONGITUDE)) +
  geom_histogram(colour = 'blue',
                 bins = 70)

#longitude distributions are uneven along both data sets

ggplot(raw_training_data_set, aes(x = LATITUDE)) +
  geom_histogram(colour = 'blue',
                 bins = 70)

ggplot(raw_validation_data_set, aes(x = LATITUDE)) +
  geom_histogram(colour = 'blue',
                 bins = 70)

#latitude distributions are uneven along both data sets


#will consider transforming to even out ddistribution for predicted data


#variance analysis of dependable balance for cascading order selection

#Building variance

var(raw_training_data_set$BUILDINGID)

#var 0.6941213

var(raw_validation_data_set$BUILDINGID)

#var 0.6660828

#longitude variance

var(raw_training_data_set$LONGITUDE)

#var 15228.06

var(raw_validation_data_set$LONGITUDE)

#var 14450.28


#latitude variance

var(raw_training_data_set$LATITUDE)

#var 4480.051

var(raw_validation_data_set$LATITUDE)

#var 4938.266

#Floor variance 

var(raw_training_data_set$FLOOR)

#var 1.495919

var(raw_validation_data_set$FLOOR)

#var 1.003659

#plotting the distribution by building of the points 

ggplot(raw_training_data_set,aes(x=LONGITUDE, y=LATITUDE, colour = BUILDINGID))+
  geom_point()


#we can observe that Longitude has a higher variance than Latitude, and that more of the
#Longitude is shared by the buildings than Longitude, so it would be wise to predict by
#Latitude first 


#checking overlapping in variables between buildings

# 
# ggplot(raw_training_data_set, aes(x = LATITUDE, fill = BUILDINGID)) + 
#   geom_density(alpha = 0.2)
# 
# ggplot(raw_training_data_set, aes(LATITUDE, fill = BUILDINGID, colour = )) + 
#   geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
# 
# 


#Ranger cascade modelling ----

#Predicting building ----

#given tha building showed the best results, we will start to cascade through this variable

# random_forest_building_model <-  ranger(formula = cascading_train_data_set[,469]~.,
#                                            data = cascading_train_data_set[,1:465],
#                                            verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_building_model ,'random_forest_building_model.rds')

random_forest_building_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_model.rds')

random_forest_building_prediction <- 
  predict(random_forest_building_model,cascading_validation_data_set)

confusionMatrix(random_forest_building_prediction$predictions,cascading_validation_data_set$BUILDINGID)

#First approach

# Confusion Matrix and Statistics
#             Reference
#Prediction   0   1   2
#         0 536   0   0
#         1   0 307   0
#         2   0   0 268
# 
# Overall Statistics
# 
# Accuracy : 1          
# 95% CI : (0.9967, 1)
# No Information Rate : 0.4824     
# P-Value [Acc > NIR] : < 2.2e-16  
# 
# Kappa : 1


#Model is very good at predicting building over the validation set. Adding predictions to the 
#data set

cascading_train_data_set$predicted_building <- random_forest_building_model$predictions

cascading_validation_data_set$predicted_building <- random_forest_building_prediction$predictions



#predicting Latitude without building separation (preprocessing done) ----

# random_forest_latitude_model <-  ranger(formula = cascading_train_data_set[,467]~.,
#                                           data = cascading_train_data_set[,c(1:465,470)],
#                                           verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_latitude_model,'random_forest_latitude_model.rds')

# random_forest_latitude_model <- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_latitude_model.rds')
# 
# random_forest_latitude_prediction <- predict(random_forest_latitude_model,cascading_validation_data_set)
# 
# postResample(random_forest_latitude_prediction$predictions,cascading_validation_data_set$LATITUDE)
# 
# 
# 
# #Error analysis 
# 
# latitude_error <- data.frame(real_latitude = cascading_validation_data_set$LATITUDE,
#                               predicted_latitude = random_forest_latitude_prediction$predictions,
#                               real_error = (cascading_validation_data_set$LATITUDE - random_forest_latitude_prediction$predictions),
#                               percentual_error = (cascading_validation_data_set$LATITUDE - random_forest_latitude_prediction$predictions)/cascading_validation_data_set$LATITUDE,
#                               real_building = cascading_validation_data_set$BUILDINGID)
# 
# #error plot 
# 
# ggplot(latitude_error, aes(x = real_latitude,
#                            y = real_error,
#                            color = real_building,
#                            shape = real_building)) +geom_point()
# 
# 
# ggplot(latitude_error, aes(x = real_latitude,
#                            y = percentual_error,
#                            color = real_building,
#                            shape = real_building)) + geom_point()
# 
# 
# ggplot(latitude_error, aes(x = percentual_error)) +
#  geom_histogram(colour = 'blue')
# 
# ggplot(latitude_error, aes(x = real_error)) +
#  geom_histogram(colour = 'blue')
# 


#first try 

#     RMSE Rsquared      MAE 
# 9.512063 0.982877 6.431550

#error looks normally distributed, and model fails more around the edges, going to predict individually
#Latitude predictions improve in regards to the original individual latitude predictions


#predicting latitude with model separation ----

#separatig data sets by predicted building

#train

cascading_train_data_set_predicted_building_0 <- cascading_train_data_set %>% filter(predicted_building == 0) 

cascading_train_data_set_predicted_building_1 <- cascading_train_data_set %>% filter(predicted_building == 1) 

cascading_train_data_set_predicted_building_2 <- cascading_train_data_set %>% filter(predicted_building == 2) 

#test 

cascading_validation_data_set_predicted_building_0 <- cascading_validation_data_set %>% filter(predicted_building == 0) 

cascading_validation_data_set_predicted_building_1 <- cascading_validation_data_set %>% filter(predicted_building == 1) 

cascading_validation_data_set_predicted_building_2 <- cascading_validation_data_set %>% filter(predicted_building == 2) 


#predicting latitude for building 0


# random_forest_building_0_latitude_model <-  ranger(formula = cascading_train_data_set_predicted_building_0[,467]~.,
#                                                       data = cascading_train_data_set_predicted_building_0[,c(1:465,470)],
#                                                       verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_building_0_latitude_model,'random_forest_building_0_latitude_model.rds')

random_forest_building_0_latitude_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_0_latitude_model.rds')

random_forest_latitude_prediction_building_0 <- 
  predict(random_forest_building_0_latitude_model,cascading_validation_data_set_predicted_building_0)

postResample(random_forest_latitude_prediction_building_0$predictions,
             cascading_validation_data_set_predicted_building_0$LATITUDE)


#error analysis 

latitude_error_building_0 <- 
  data.frame(real_latitude = cascading_validation_data_set_predicted_building_0$LATITUDE,
                                         predicted_latitude = random_forest_latitude_prediction_building_0$predictions,
                                         real_error = (cascading_validation_data_set_predicted_building_0$LATITUDE - random_forest_latitude_prediction_building_0$predictions),
                                         percentual_error = (cascading_validation_data_set_predicted_building_0$LATITUDE - random_forest_latitude_prediction_building_0$predictions)/cascading_validation_data_set_predicted_building_0$LATITUDE,
                                         real_building = cascading_validation_data_set_predicted_building_0$BUILDINGID)
                                        

#Error plots 

ggplot(latitude_error_building_0, aes(x = real_latitude,
                                       y = real_error, 
                                       color = real_building, 
                                       shape = real_building)) +geom_point()


ggplot(latitude_error_building_0, aes(x = real_latitude,
                                       y = percentual_error, 
                                       color = real_building, 
                                       shape = real_building)) + geom_point()


ggplot(latitude_error_building_0, aes(x = real_error)) +
  geom_histogram(colour = 'blue')

ggplot(latitude_error_building_0, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')




#First try 

#      RMSE  Rsquared       MAE 
# 6.3964868 0.9628311 4.4033578 


#Error distribution looks normal and is low in value, some outliers present


#addind predictions to the data set


#train
cascading_train_data_set_predicted_building_0$predicted_latitude <- 
  random_forest_building_0_latitude_model$predictions


#validation 

cascading_validation_data_set_predicted_building_0$predicted_latitude <-
  random_forest_latitude_prediction_building_0$predictions
  




#predicting latitude for building 1


# random_forest_building_1_latitude_model <-  ranger(formula = cascading_train_data_set_predicted_building_1[,467]~.,
#                                                       data = cascading_train_data_set_predicted_building_1[,c(1:465,470)],
#                                                       verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_building_1_latitude_model,'random_forest_building_1_latitude_model.rds')

random_forest_building_1_latitude_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_1_latitude_model.rds')

random_forest_latitude_prediction_building_1 <- 
  predict(random_forest_building_1_latitude_model,cascading_validation_data_set_predicted_building_1)

postResample(random_forest_latitude_prediction_building_1$predictions,
             cascading_validation_data_set_predicted_building_1$LATITUDE)

#error analysis 


latitude_error_building_1 <- 
  data.frame(real_latitude = cascading_validation_data_set_predicted_building_1$LATITUDE,
                                        predicted_latitude = random_forest_latitude_prediction_building_1$predictions,
                                        real_error = (cascading_validation_data_set_predicted_building_1$LATITUDE - random_forest_latitude_prediction_building_1$predictions),
                                        percentual_error = (cascading_validation_data_set_predicted_building_1$LATITUDE - random_forest_latitude_prediction_building_1$predictions)/cascading_validation_data_set_predicted_building_1$LATITUDE,
                                        real_building = cascading_validation_data_set_predicted_building_1$BUILDINGID)


#error plots

ggplot(latitude_error_building_1, aes(x = real_latitude,
                                      y = real_error, 
                                      color = real_building, 
                                      shape = real_building)) +geom_point()


ggplot(latitude_error_building_1, aes(x = real_latitude,
                                      y = percentual_error, 
                                      color = real_building, 
                                      shape = real_building)) + geom_point()


ggplot(latitude_error_building_1, aes(x = real_error)) +
  geom_histogram(colour = 'blue')

ggplot(latitude_error_building_1, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')




#first try

#       RMSE   Rsquared        MAE 
# 12.4898909  0.8806299  8.3555360 


#error seems to have a normal distribution, seems like most of the issue with the latitude
#prediction from building 1 comes from an outlier. However erors still go up to 40,
#need to check if I can improve this model


#addind predictions to the data set


#train
cascading_train_data_set_predicted_building_1$predicted_latitude <- 
  random_forest_building_1_latitude_model$predictions


#validation 

cascading_validation_data_set_predicted_building_1$predicted_latitude <-
  random_forest_latitude_prediction_building_1$predictions




#predicting latitude for building 2

# random_forest_building_2_latitude_model <-  ranger(formula = cascading_train_data_set_predicted_building_2[,467]~.,
#                                                       data = cascading_train_data_set_predicted_building_2[,c(1:465,470)],
#                                                       verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_building_2_latitude_model,'random_forest_building_2_latitude_model.rds')

random_forest_building_2_latitude_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_2_latitude_model.rds')

random_forest_latitude_prediction_building_2 <- 
  predict(random_forest_building_2_latitude_model,cascading_validation_data_set_predicted_building_2)

postResample(random_forest_latitude_prediction_building_2$predictions,
             cascading_validation_data_set_predicted_building_2$LATITUDE)

#Error analysis

latitude_error_building_2 <- 
  data.frame(real_latitude = cascading_validation_data_set_predicted_building_2$LATITUDE,
                                        predicted_latitude = random_forest_latitude_prediction_building_2$predictions,
                                        real_error = (cascading_validation_data_set_predicted_building_2$LATITUDE - random_forest_latitude_prediction_building_2$predictions),
                                        percentual_error = (cascading_validation_data_set_predicted_building_2$LATITUDE - random_forest_latitude_prediction_building_2$predictions)/cascading_validation_data_set_predicted_building_2$LATITUDE,
                                        real_building = cascading_validation_data_set_predicted_building_2$BUILDINGID)




#error plot 

ggplot(latitude_error_building_2, aes(x = real_latitude,
                                      y = real_error, 
                                      color = real_building, 
                                      shape = real_building)) +geom_point()


ggplot(latitude_error_building_2, aes(x = real_latitude,
                                      y = percentual_error, 
                                      color = real_building, 
                                      shape = real_building)) + geom_point()


ggplot(latitude_error_building_2, aes(x = real_error)) +
  geom_histogram(colour = 'blue')

ggplot(latitude_error_building_2, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')




#first try 

#       RMSE   Rsquared        MAE 
# 10.3206706  0.8896295  7.3126992 



#addind predictions to the data set


#train

cascading_train_data_set_predicted_building_2$predicted_latitude <- 
  random_forest_building_2_latitude_model$predictions


#validation 

cascading_validation_data_set_predicted_building_2$predicted_latitude <-
  random_forest_latitude_prediction_building_2$predictions




#error seems to have a normal distribution, seems like most of the issue with the latitude
#prediction from building 1 comes from an outlier. However erors still go up to 40,
#need to check if I can improve this model




#Predicting longitude  without building separation----


# random_forest_longitude_model <-  ranger(formula = cascading_train_data_set[,466]~.,
#                                           data = cascading_train_data_set[,c(1:465,470)],
#                                           verbose = T,importance = 'impurity')
# 
# random_forest_longitude_prediction <- predict(random_forest_longitude_model,cascading_validation_data_set)
# 
# postResample(random_forest_longitude_prediction$predictions,cascading_validation_data_set$LONGITUDE)


#Error analysis

# first try without preprocessing or added columns

#       RMSE   Rsquared        MAE 
# 12.1378244  0.9900806  8.6629902 

#error is not on acceptable levels 

# longitude_error <- data.frame(real_longitude = cascading_validation_data_set$LONGITUDE,
#                               predicted_longitude = random_forest_longitude_prediction$predictions,
#                               real_error = (cascading_validation_data_set$LONGITUDE - random_forest_longitude_prediction$predictions),
#                               percentual_error = (cascading_validation_data_set$LONGITUDE - random_forest_longitude_prediction$predictions)/cascading_validation_data_set$LONGITUDE,
#                               real_building = cascading_validation_data_set$BUILDINGID)


#error plots 


# ggplot(longitude_error, aes(x = real_longitude,
#                             y = real_error, 
#                             color = real_building, 
#                             shape = real_building)) +geom_point()


# ggplot(longitude_error, aes(x = real_longitude,
#                             y = percentual_error, 
#                             color = real_building, 
#                             shape = real_building)) + geom_point()

#biggest error zones seem to be located around the building edges, also 
#behaviour for the third building seems to be slightly different in regards to the
#error than the other buildings

# ggplot(longitude_error, aes(x = percentual_error)) +
#   geom_histogram(colour = 'blue')

# ggplot(longitude_error, aes(x = real_error)) +
#   geom_histogram(colour = 'blue')


#error distribution seems fairly normal, some outliers that are represented by the 
#errors of building 3 in the previous plots

#adding predicted column to prediction to see if it changes the behaviour of the error 

# Second try (adding predicted building column information)

#       RMSE   Rsquared        MAE 
# 11.4166885  0.9911152  8.3323240 

#result is positive, improving all the metrics slightly


# Error plots 

#same behaviour was presented, normal distributions and different outliers for different buildings

#will approach the data set now with pre processing to try to get better error metrics as the 
#error level is not acceptable


#Third  try

#removed columns with zero variance, metrics get slightly worse for for RMSE and Rsquared,
#but it gets better for the MAE

#       RMSE   Rsquared        MAE 
# 11.6957163  0.9906599  8.3197928 


# error distribution looks fairly unafected

#Fourth try 

#transformed the scale to positive, and switched the missing values to 0, error improves.
#greater improvement in the MAE 

#       RMSE   Rsquared        MAE 
# 10.1747198  0.9930389  6.9410499 


# error distribution looks fairly unafected

#5th try 

#removed rows with  zero variance, model gets worse at predicting, but will use this to test
#the effect of row standarization

#       RMSE   Rsquared        MAE 
# 11.2722751  0.9913017  8.1540464 


#6th try 

#standarized by rows to level the observations,it generated worse results

#       RMSE   Rsquared        MAE 
# 14.5362869  0.9859827  9.2444654 

#will try separating the buildigns as the biggest errors seem to be happening at the edges of
#buildings. Will not work with standardized data for the time being, as I cannot
#work under the precepts that I can remove real user observations for the working
#app. returned to 4th try preprocessing.






#Predicting longitude  with building model separation ---- 


#predicting longitude for building 0

# random_forest_building_0_longitude_model <-  
#   ranger(formula = cascading_train_data_set_predicted_building_0[,466]~.,
#           data = cascading_train_data_set_predicted_building_0[,c(1:465,470,471)],
#           verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_building_0_longitude_model,
#         'random_forest_building_0_longitude_model.rds')

random_forest_building_0_longitude_model <-
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_0_longitude_model.rds')

random_forest_longitude_prediction_building_0 <-
  predict(random_forest_building_0_longitude_model,cascading_validation_data_set_predicted_building_0)

postResample(random_forest_longitude_prediction_building_0$predictions,
             cascading_validation_data_set_predicted_building_0$LONGITUDE)


# error analysis

#residuals analysis


longitude_error_building_0 <-
  data.frame(real_longitude = cascading_validation_data_set_predicted_building_0$LONGITUDE,
              predicted_longitude = random_forest_longitude_prediction_building_0$predictions,
              real_error = (cascading_validation_data_set_predicted_building_0$LONGITUDE - random_forest_longitude_prediction_building_0$predictions),
              percentual_error = (cascading_validation_data_set_predicted_building_0$LONGITUDE - random_forest_longitude_prediction_building_0$predictions)/cascading_validation_data_set_predicted_building_0$LONGITUDE,
              real_building = cascading_validation_data_set_predicted_building_0$BUILDINGID)


#Error plots 

ggplot(longitude_error_building_0, aes(x = real_longitude,
                            y = real_error, 
                            color = real_building, 
                            shape = real_building)) +geom_point()


ggplot(longitude_error_building_0, aes(x = real_longitude,
                            y = percentual_error, 
                            color = real_building, 
                            shape = real_building)) + geom_point()


ggplot(longitude_error_building_0, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')

ggplot(longitude_error_building_0, aes(x = real_error)) +
  geom_histogram(colour = 'blue')


#first try

#      RMSE  Rsquared       MAE 
# 7.5685890 0.9321235 5.3802557

#while rsqueared value does get lowered, we do get to see a good improvement in the values for 
#the RMSE and the MAE. better results 

#error distribution seems to have gotten more condensed and more normal, it is failing on one predicted
#building. 


#second try (adding the predictions for latitude)


#      RMSE  Rsquared       MAE 
# 7.5036861 0.9332621 5.2561520 

#predictions get slightly better for building 0, biggets error keeps coming from the point
#that predicts wrongly for bulding 0 when it belongs to building 1


#adding predictions 

#addind predictions to the data set


#train

cascading_train_data_set_predicted_building_0$predicted_longitude <- 
  random_forest_building_0_longitude_model$predictions


#validation 

cascading_validation_data_set_predicted_building_0$predicted_longitude<-
  random_forest_longitude_prediction_building_0$predictions





#predicting longitude for building 1


# random_forest_building_1_longitude_model <-  
#   ranger(formula = cascading_train_data_set_predicted_building_1[,466]~.,
#           data = cascading_train_data_set_predicted_building_1[,c(1:465,470,471)],
#           verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_building_1_longitude_model,'random_forest_building_1_longitude_model.rds')

random_forest_building_1_longitude_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_1_longitude_model.rds')

random_forest_longitude_prediction_building_1 <- 
  predict(random_forest_building_1_longitude_model,cascading_validation_data_set_predicted_building_1)

postResample(random_forest_longitude_prediction_building_1$predictions,cascading_validation_data_set_predicted_building_1$LONGITUDE)


#error analysis


#residuals 


longitude_error_building_1 <- 
  data.frame(real_longitude = cascading_validation_data_set_predicted_building_1$LONGITUDE,
                                         predicted_longitude = random_forest_longitude_prediction_building_1$predictions,
                                         real_error = (cascading_validation_data_set_predicted_building_1$LONGITUDE - random_forest_longitude_prediction_building_1$predictions),
                                         percentual_error = (cascading_validation_data_set_predicted_building_1$LONGITUDE - random_forest_longitude_prediction_building_1$predictions)/cascading_validation_data_set_predicted_building_1$LONGITUDE,
                                         real_building = cascading_validation_data_set_predicted_building_1$BUILDINGID)

#error plot

ggplot(longitude_error_building_1, aes(x = real_longitude,
                                       y = real_error, 
                                       color = real_building, 
                                       shape = real_building)) +geom_point()


ggplot(longitude_error_building_1, aes(x = real_longitude,
                                       y = percentual_error, 
                                       color = real_building, 
                                       shape = real_building)) + geom_point()


ggplot(longitude_error_building_1, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')

ggplot(longitude_error_building_1, aes(x = real_error)) +
  geom_histogram(colour = 'blue')


#first try

#       RMSE   Rsquared        MAE 
# 12.4177050  0.9293016  7.1664181 

#error distribution has gotten a lot more focused around zero, however, the increase in the RMSE and 
#the error plot is showing that there is an outlier which is pushin the RMSE up.

#second try

# RMSE   Rsquared        MAE 
# 12.5660442  0.9272868  7.1901881 

#results get slightly worse, error distribution looks similar, not a big change. 



#addind predictions to the data set


#train

cascading_train_data_set_predicted_building_1$predicted_longitude <- 
  random_forest_building_1_longitude_model$predictions


#validation 

cascading_validation_data_set_predicted_building_1$predicted_longitude<-
  random_forest_longitude_prediction_building_1$predictions




#predicting longitude for building 2


# random_forest_building_2_longitude_model <- 
#   ranger(formula = cascading_train_data_set_predicted_building_2[,466]~.,
#             data = cascading_train_data_set_predicted_building_2[,c(1:465,470,471)],
#             verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_building_2_longitude_model,'random_forest_building_2_longitude_model.rds')

random_forest_building_2_longitude_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_2_longitude_model.rds')

random_forest_longitude_prediction_building_2 <- 
  predict(random_forest_building_2_longitude_model,cascading_validation_data_set_predicted_building_2)

postResample(random_forest_longitude_prediction_building_2$predictions,
             cascading_validation_data_set_predicted_building_2$LONGITUDE)



# error analysis


longitude_error_building_2 <- 
  data.frame(real_longitude = cascading_validation_data_set_predicted_building_2$LONGITUDE,
                                         predicted_longitude = random_forest_longitude_prediction_building_2$predictions,
                                         real_error = (cascading_validation_data_set_predicted_building_2$LONGITUDE - random_forest_longitude_prediction_building_2$predictions),
                                         percentual_error = (cascading_validation_data_set_predicted_building_2$LONGITUDE - random_forest_longitude_prediction_building_2$predictions)/cascading_validation_data_set_predicted_building_2$LONGITUDE,
                                         real_building = cascading_validation_data_set_predicted_building_2$BUILDINGID)


#Error plots 

ggplot(longitude_error_building_2, aes(x = real_longitude,
                                       y = real_error, 
                                       color = real_building, 
                                       shape = real_building)) +geom_point()


ggplot(longitude_error_building_2, aes(x = real_longitude,
                                       y = percentual_error, 
                                       color = real_building, 
                                       shape = real_building)) + geom_point()


ggplot(longitude_error_building_2, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')

ggplot(longitude_error_building_2, aes(x = real_error)) +
  geom_histogram(colour = 'blue')



#first try

#       RMSE   Rsquared        MAE 
# 12.2110073  0.8792003  9.4621427 


#all the stadistics for the error metrics get worse, as we can see the distribution of the error has
#expanded, the model is not  effectively predicting the longitude for the building 2, will need
#to improve the data or the model.

#second try 

#       RMSE   Rsquared        MAE 
# 11.6723716  0.8894872  8.8932410 

#has the best increase in performance from all the buildings, error distribution imptoved, however,
#the the spread on the scatter is still big, looking for better performance. 



#train

cascading_train_data_set_predicted_building_2$predicted_longitude <- 
  random_forest_building_2_longitude_model$predictions


#validation 

cascading_validation_data_set_predicted_building_2$predicted_longitude<-
  random_forest_longitude_prediction_building_2$predictions



#predicting latitude 2 ----

#we try to re predict latitude with the additional information for testing

#predicting building 0

# random_forest_building_0_latitude_model_2 <-  
#   ranger(formula = cascading_train_data_set_predicted_building_0[,467]~.,
#           data = cascading_train_data_set_predicted_building_0[,c(1:465,470,472)],
#           verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_building_0_latitude_model_2,'random_forest_building_0_latitude_model_2.rds')

random_forest_building_0_latitude_model_2 <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_0_latitude_model_2.rds')

random_forest_latitude_prediction_2_building_0 <- 
  predict(random_forest_building_0_latitude_model_2,cascading_validation_data_set_predicted_building_0)

postResample(random_forest_latitude_prediction_2_building_0$predictions,
             cascading_validation_data_set_predicted_building_0$LATITUDE)


#error analysis 

latitude_error_building_0_2 <- 
  data.frame(real_latitude = cascading_validation_data_set_predicted_building_0$LATITUDE,
             predicted_latitude = random_forest_latitude_prediction_2_building_0$predictions,
             real_error = (cascading_validation_data_set_predicted_building_0$LATITUDE - random_forest_latitude_prediction_2_building_0$predictions),
             percentual_error = (cascading_validation_data_set_predicted_building_0$LATITUDE - random_forest_latitude_prediction_2_building_0$predictions)/cascading_validation_data_set_predicted_building_0$LATITUDE,
             real_building = cascading_validation_data_set_predicted_building_0$BUILDINGID)


#Error plots 

ggplot(latitude_error_building_0_2, aes(x = real_latitude,
                                      y = real_error, 
                                      color = real_building, 
                                      shape = real_building)) +geom_point()


ggplot(latitude_error_building_0_2, aes(x = real_latitude,
                                      y = percentual_error, 
                                      color = real_building, 
                                      shape = real_building)) + geom_point()


ggplot(latitude_error_building_0_2, aes(x = real_error)) +
  geom_histogram(colour = 'blue')

ggplot(latitude_error_building_0_2, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')


#first try

#     RMSE  Rsquared       MAE 
# 6.2402254 0.9650211 4.2003817 

#vs previous 

#      RMSE  Rsquared       MAE 
# 6.3964868 0.9628311 4.4033578 

#results get slightly better for building 0. error distribution still the same


#adding results


#train
cascading_train_data_set_predicted_building_0$predicted_latitude_2 <- 
  random_forest_building_0_latitude_model_2$predictions


#validation 

cascading_validation_data_set_predicted_building_0$predicted_latitude_2 <-
  random_forest_latitude_prediction_2_building_0$predictions






#predicting building 1

# random_forest_building_1_latitude_model_2 <-
#   ranger(formula = cascading_train_data_set_predicted_building_1[,467]~.,
#           data = cascading_train_data_set_predicted_building_1[,c(1:465,470,472)],
#           verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_building_1_latitude_model_2,'random_forest_building_1_latitude_model_2.rds')

random_forest_building_1_latitude_model_2 <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_1_latitude_model_2.rds')

random_forest_latitude_prediction_2_building_1 <- 
  predict(random_forest_building_1_latitude_model_2,
          cascading_validation_data_set_predicted_building_1)

postResample(random_forest_latitude_prediction_2_building_1$predictions,
             cascading_validation_data_set_predicted_building_1$LATITUDE)


#first try 

#       RMSE   Rsquared        MAE 
# 12.7288869  0.8760342  8.3456268 

#vs 

#       RMSE   Rsquared        MAE 
# 12.6664116  0.8768631  8.2715442 


#adding results


#train
cascading_train_data_set_predicted_building_1$predicted_latitude_2 <- 
  random_forest_building_1_latitude_model_2$predictions


#validation 

cascading_validation_data_set_predicted_building_1$predicted_latitude_2 <-
  random_forest_latitude_prediction_2_building_1$predictions



#predicting building 2

random_forest_building_2_latitude_model_2 <-
  ranger(formula = cascading_train_data_set_predicted_building_2[,467]~.,
          data = cascading_train_data_set_predicted_building_2[,c(1:465,470,472)],
          verbose = T,importance = 'impurity')

saveRDS(random_forest_building_2_latitude_model_2,'random_forest_building_2_latitude_model_2.rds')

random_forest_building_2_latitude_model_2 <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_2_latitude_model_2.rds')

random_forest_latitude_prediction_2_building_2 <- 
  predict(random_forest_building_2_latitude_model_2,
          cascading_validation_data_set_predicted_building_2)

postResample(random_forest_latitude_prediction_2_building_2$predictions,
             cascading_validation_data_set_predicted_building_2$LATITUDE)



#first try 

# RMSE   Rsquared        MAE 
# 10.4015248  0.8877199  7.3135684 

#vs 

#first try 

#       RMSE   Rsquared        MAE 
# 10.3206706  0.8896295  7.3126992 

#adding results


#train
cascading_train_data_set_predicted_building_2$predicted_latitude_2 <- 
  random_forest_building_2_latitude_model_2$predictions


#validation 

cascading_validation_data_set_predicted_building_2$predicted_latitude_2 <-
  random_forest_latitude_prediction_2_building_2$predictions



#predicting floor (with model separation)----


#predicting building 0


random_forest_building_0_floor_model <-
  ranger(formula = cascading_train_data_set_predicted_building_0[,468]~.,
          data = cascading_train_data_set_predicted_building_0[,c(1:465,470,473,472)],
          verbose = T,importance = 'impurity')

saveRDS(random_forest_building_0_floor_model,
        'random_forest_building_0_floor_model.rds')

random_forest_building_0_floor_model <-
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_0_floor_model.rds')

random_forest_floor_prediction_building_0 <-
  predict(random_forest_building_0_floor_model,cascading_validation_data_set_predicted_building_0)

confusionMatrix(random_forest_floor_prediction_building_0$predictions,
             cascading_validation_data_set_predicted_building_0$FLOOR)

#Error analysis


#First try

# Confusion Matrix and Statistics
# 
#                      Reference
# Prediction   0   1   2   3   4
#          0  74   1   1   0   0
#          1   3 205   3   0   0
#          2   1   2 160   2   0
#          3   0   0   1  83   0
#          4   0   0   0   0   0
# 
# Overall Statistics
# 
# Accuracy : 0.9739          
# 95% CI : (0.9566, 0.9856)
# No Information Rate : 0.3881          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9631       


#second try (with new latitude)

# Confusion Matrix and Statistics
# 
#                      Reference
# Prediction   0   1   2   3   4
#          0  73   1   0   0   0
#          1   3 205   3   0   0
#          2   2   2 162   2   0
#          3   0   0   0  83   0
#          4   0   0   0   0   0
# 
# Overall Statistics
# 
# Accuracy : 0.9757         
# 95% CI : (0.9589, 0.987)
# No Information Rate : 0.3881         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.9657

#gets a little bit better 



#predicting building 1

random_forest_building_1_floor_model <-
  ranger(formula = cascading_train_data_set_predicted_building_1[,468]~.,
          data = cascading_train_data_set_predicted_building_1[,c(1:465,470,473,472)],
          verbose = T,importance = 'impurity')

saveRDS(random_forest_building_1_floor_model,
        'random_forest_building_1_floor_model.rds')

random_forest_building_1_floor_model <-
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_1_floor_model.rds')

random_forest_floor_prediction_building_1 <-
  predict(random_forest_building_1_floor_model,
          cascading_validation_data_set_predicted_building_1)


confusionMatrix(random_forest_floor_prediction_building_1$predictions,
                cascading_validation_data_set_predicted_building_1$FLOOR)


#Error analysis

#first try

# Confusion Matrix and Statistics

#                 Reference
# Prediction  0  1  2  3  4
#          0 23  1  0  0  0
#          1  2 96  1  0  0
#          2  5 42 83  4  0
#          3  0  4  3 43  0
#          4  0  0  0  0  0
# 
# Overall Statistics
# 
# Accuracy : 0.798           
# 95% CI : (0.7487, 0.8415)
# No Information Rate : 0.4658          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7088   


#second try

# Confusion Matrix and Statistics
# 
#                 Reference
# Prediction  0  1  2  3  4
#          0 22  1  0  0  0
#          1  3 96  1  0  0
#          2  4 40 84  4  0
#          3  1  6  2 43  0
#          4  0  0  0  0  0
# 
# Overall Statistics
# 
# Accuracy : 0.798           
# 95% CI : (0.7487, 0.8415)
# No Information Rate : 0.4658          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7087   

#results get slightly better


#predicting building 2


# random_forest_building_2_floor_model <-
#   ranger(formula = cascading_train_data_set_predicted_building_2[,468]~.,
#           data = cascading_train_data_set_predicted_building_2[,c(1:465,470,473,472)],
#           verbose = T,importance = 'impurity')
# 
# 
# saveRDS(random_forest_building_2_floor_model,
#         'random_forest_building_2_floor_model.rds')

random_forest_building_2_floor_model <-
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_building_2_floor_model.rds')

random_forest_floor_prediction_building_2 <-
  predict(random_forest_building_2_floor_model,
          cascading_validation_data_set_predicted_building_2)

confusionMatrix(random_forest_floor_prediction_building_2$predictions,
                cascading_validation_data_set_predicted_building_2$FLOOR)

#Error analysis

#first try

# Confusion Matrix and Statistics
# 
#                      Reference
# Prediction   0   1   2   3   4
#          0  21   0   0   0   1
#          1   3 109   1   0   0
#          2   0   2  47   0   0
#          3   0   0   6  39   9
#          4   0   0   0   1  29
# 
# Overall Statistics
# 
# Accuracy : 0.9142         
# 95% CI : (0.874, 0.9448)
# No Information Rate : 0.4142         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.8832   

#second try

# Confusion Matrix and Statistics
# 
#                      Reference
# Prediction   0   1   2   3   4
#          0  21   0   0   0   1
#          1   3 109   2   0   0
#          2   0   2  47   0   0
#          3   0   0   5  39  10
#          4   0   0   0   1  28
# 
# Overall Statistics
# 
# Accuracy : 0.9104          
# 95% CI : (0.8697, 0.9418)
# No Information Rate : 0.4142          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.878  




