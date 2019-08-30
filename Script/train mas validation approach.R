#Train plus validation approach 

#Loading packages ----

pacman::p_load(dplyr,caret,ggplot2,tidyr,utils,matrixStats,sf,viridis, 
               graphics,ranger,plotly,FNN,h2o,plyr,anchors, psycho, data.table,
               stats)


#load the training and validation data set ----

raw_training_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/trainingData.csv')

raw_validation_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/validationData.csv')

#creating cascading data set

full_raw_data_set <- rbind(raw_training_data_set,raw_validation_data_set)



#preprocessing ----

#removing variables that are not going to be used

full_raw_data_set <- 
  full_raw_data_set[,-c(474,473,472,471,470)]




#creating train and test ---- 

intraining_raw <- createDataPartition(full_raw_data_set$FLOOR,
                                      p=0.70,
                                      list = FALSE) 

training_raw <- raw_training_data_set[intraining_raw,]

testing_raw <- raw_training_data_set[-intraining_raw,]




