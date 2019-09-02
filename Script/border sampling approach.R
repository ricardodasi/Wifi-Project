#Sampled borders approach

#Loading packages ----

pacman::p_load(dplyr,caret,ggplot2,tidyr,utils,matrixStats,sf,viridis, 
               graphics,ranger,plotly,FNN,h2o,plyr,anchors, psycho, data.table,
               stats)


#load the training and validation data set ----

cascading_train_data_set<- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/trainingData.csv')

cascading_validation_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/validationData.csv')


#preprocessing ----


# 1. changing factor categories

cascading_train_data_set$BUILDINGID <- as.factor(cascading_train_data_set$BUILDINGID)

cascading_train_data_set$FLOOR <- as.factor(cascading_train_data_set$FLOOR)

cascading_validation_data_set$BUILDINGID <- as.factor(cascading_validation_data_set$BUILDINGID)

cascading_validation_data_set$FLOOR <- as.factor(cascading_validation_data_set$FLOOR)


# 2. checking waps with problem measures 

#waps shouldnt have measures between -30 and 0


waps_values <- stack(cascading_train_data_set[,1:520])

waps_values <- waps_values %>%  filter(values != 100) 

ggplot(waps_values,aes(x = values)) + geom_histogram()

#we can see that there are measures that go up to 0 from -30, these need to be removed

problem_waps <-  waps_values %>%  filter(values <=1,values>= -30)


#65 problem waps out of the 520

problem_waps <- problem_waps %>%  
  group_by(ind) %>% 
  summarise(n = count(ind))

#different frequencies depending to the wap, exploring measures in relation to the users


waps_with_user <- cascading_train_data_set[,c(1:520,527,528)]


waps_with_user[which(waps_with_user[,1:520]>=-30 & waps_with_user[,1:520]<=0 ),]
