#Cascading approach to the Wifi problem

#Loading packages ----

pacman::p_load(dplyr,caret,ggplot2,tidyr,utils,matrixStats,sf,viridis, 
               graphics,ranger,plotly,FNN,h2o,plyr,anchors, robustHD)




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

#1. remove first look useless variables -> modelling with only chosen variables, not necesary

#2. transform missing values to '0' 

#Verified that the data set did not contain already values assigned to '0'

# list_of_waps <- stack(raw_training_data_set[,1:520])
# 
# list_of_waps <- list_of_waps %>% filter(values < 100 ) 

# replaced missing values and changing magnitude

cascading_train_data_set[,1:520] <- cascading_train_data_set[,1:520] + 105

cascading_validation_data_set[,1:520] <- cascading_validation_data_set[,1:520] + 105


cascading_train_data_set <- replace.value( data = cascading_train_data_set, 
                                           names = colnames(cascading_train_data_set[1:520]),
                                           from = 205 ,
                                           to  = 0,
                                           verbose = FALSE)

cascading_validation_data_set <- replace.value( data = cascading_validation_data_set, 
                                           names = colnames(cascading_validation_data_set[1:520]),
                                           from = 205 ,
                                           to  = 0,
                                           verbose = FALSE)


cascading_train_data_set[,1:520] <- cascading_train_data_set[,1:520] + 105

#3. standarize the wap measures 

View(standarize(cascading_train_data_set][1,1:520]))


#we do this by row, given that each user behaviour with the wap measures might be different
#depending on the kind of cellphone and we want them all in the same scale so the model 
#can recognize the position by waps and not specific measures









#4. Principal component analysis



#5. apply changes to data set depending on PCA results







#Changing variable catergories

cascading_train_data_set$BUILDINGID <- as.factor(raw_training_data_set$BUILDINGID)

cascading_train_data_set$FLOOR <- as.factor(raw_training_data_set$FLOOR)

cascading_validation_data_set$BUILDINGID <- as.factor(raw_validation_data_set$BUILDINGID)

cascading_validation_data_set$FLOOR <- as.factor(raw_validation_data_set$FLOOR)



#Ranger cascade modelling ----

#Predicting building ----

#given tha building showed the best results, we will start to cascade through this variable

# random_forest_building_model <-  ranger(formula = cascading_train_data_set[,524]~.,
#                                            data = cascading_train_data_set[,1:520],
#                                            verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_building_model,'random_forest_building_model.rds')

random_forest_building_model <- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/random_forest_building_model.rds')

random_forest_building_prediction <- predict(random_forest_building_model,cascading_validation_data_set)

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



#Predicting longitude ----


random_forest_longitude_model <-  ranger(formula = cascading_train_data_set[,521]~.,
                                          data = cascading_train_data_set[,c(1:520,530)],
                                          verbose = T,importance = 'impurity')

random_forest_longitude_prediction <- predict(random_forest_longitude_model,cascading_validation_data_set)

postResample(random_forest_longitude_prediction$predictions,cascading_validation_data_set$LONGITUDE)


#Error analysis

# first try without preprocessing or added columns

#       RMSE   Rsquared        MAE 
# 12.1378244  0.9900806  8.6629902 

#error is not on acceptable levels 

longitude_error <- data.frame(real_longitude = cascading_validation_data_set$LONGITUDE,
                              predicted_longitude = random_forest_longitude_prediction$predictions,
                              real_error = (cascading_validation_data_set$LONGITUDE - random_forest_longitude_prediction$predictions),
                              percentual_error = (cascading_validation_data_set$LONGITUDE - random_forest_longitude_prediction$predictions)/cascading_validation_data_set$LONGITUDE,
                              real_building = cascading_validation_data_set$BUILDINGID)


#error plots 


ggplot(longitude_error, aes(x = real_longitude,
                            y = real_error, 
                            color = real_building, 
                            shape = real_building)) +geom_point()


ggplot(longitude_error, aes(x = real_longitude,
                            y = percentual_error, 
                            color = real_building, 
                            shape = real_building)) + geom_point()

#biggest error zones seem to be located around the building edges, also 
#behaviour for the third building seems to be slightly different in regards to the
#error than the other buildings

ggplot(longitude_error, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')

ggplot(longitude_error, aes(x = real_error)) +
  geom_histogram(colour = 'blue')


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






