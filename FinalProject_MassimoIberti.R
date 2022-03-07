#-------------------------------------------------------------------------------
#  CLASSIFICATION OF STARS
#
#  MASSIMO IBERTI
#
#-------------------------------------------------------------------------------

#This report was created as part of the assignment for the Capstone course of HarvardX's Data Science Professional Certificate.

#The aim of this project is to built a classification model for astronomical objects based on the characteristics
#of the spectrum of their light


#-------------------------------------------------------------------------------
# DATASET
#-------------------------------------------------------------------------------



#The dataset that we are taking into consideration can be found in 
#https://www.kaggle.com/fedesoriano/stellar-classification-dataset-sdss17
#It contains data released by the Sloan Digital Sky Survey (SDSS).

# 
# load libraries needed in the calculations
#

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
library(corrplot) 
library(randomForest) 
library(tidyverse)
library(caret)
library(data.table)
library(rpart)
library(gridExtra)
library(knitr)


# the code below downloads the zip archive from github and unzip it
# it populates the star_classification dataset


dl <- tempfile()
download.file("https://github.com/miberti/mlearning-stars/raw/main/archive_Stellar.zip", dl)
star_classification <- read.csv(unzip(dl, "star_classification.csv"))
rm(dl)


#We will first start from the investigation of the dataset
#The dataset contains 100000 observations and 18 columns.\

#The columns are:
  
# obj_ID     : Identifier of the Stellar object in the database
# alpha      : Right Ascension angle (at J2000 epoch)
# delta      : Declination angle (at J2000 epoch)
# u          : Ultraviolet filter in the photometric system
# g          : Green filter in the photometric system
# r          : Red filter in the photometric system
# i          : Near Infrared filter in the photometric system
# z          : Infrared filter in the photometric system
# run_ID     : Run Number used to identify the specific scan
# rerun_ID   : Rerun Number to specify how the image was processed
# cam_col    : Camera column to identify the scanline within the run
# field_ID   : Field number to identify each field
# spec_obj_ID: Unique ID used for optical spectroscopic objects
# class      : The Class of the Stellar Object
# redshift   : Redshift value based on the increase in wavelength
# plate      : Plate ID, identifies each plate in SDSS
# MJD        : Modified Julian Date, used to indicate when a given piece of SDSS data was taken
# fiber_ID   : fiber ID that identifies the fiber that pointed the light at the focal plane in each observation


#str(star_classification)
# We will not work with all the coulmns, so we remove the columns which are not useful


star_classification <- star_classification %>% mutate(class = as.factor(class)) %>%
  select(obj_ID,class,alpha,delta,u,g,r,i,z,redshift)
str(star_classification)


#-------------------------------------------------------------------------------
# ANALYSIS
#-------------------------------------------------------------------------------


# We will start plotting the amount of records for each class of objects.
# Histogram of the classes

ggplot(star_classification, aes(x=class,fill=class)) + geom_histogram(stat="count")

# We will now study the distribution in space of our data. 
# Distribution of the observation, in celestial coordinates alpha and delta
# only 4000 point

star_classification[ sample(nrow(star_classification),4000),] %>%
  ggplot(aes(x= alpha ,y=delta,color=class)) + geom_point(size = 0.6 )


# We will now study the distribution of the spectral features of our data

# redshift : redshift
# u : Ultraviolet filter
# g : Green filter
# r : Red filter
# i : Near Infrared filter
# z : Infrared filter

# with a boxplot, we want to have a glimps of the distribution of the spectral variables among the classes

brs  <- ggplot(star_classification, aes(class,redshift,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
bi <- ggplot(star_classification, aes(class,i,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
br  <- ggplot(star_classification, aes(class,r,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
bu  <- ggplot(star_classification, aes(class,u,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
bg  <- ggplot(star_classification, aes(class,g,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
bz  <- ggplot(star_classification, aes(class,z,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
grid.arrange(brs,bi,br,bu, bg , bz, nrow = 2)


# Outlier recognition 
#-------------------------------------------------------------------------------


# from the graph above we can see that we have a outliers
# We recognize the presence of an outlier from the difference between the standard dev and the MAD

df <- summarise(star_classification,Feature = "Ultraviolet Filter", Column = "(u)" ,
                Mean= mean(u) ,"StandardDev" = sd(u) ,Median = median(u), "MedianAbsDev" =  mad(u))
df <- rbind(df,summarise(star_classification,Feature = "Green", Column = "(g)" ,
                         Mean= mean(g) ,"StandardDev" = sd(g) ,Median = median(g), "MedianAbsDev" =  mad(g)))
df <- rbind(df,summarise(star_classification,Feature = "Infrared", Column = "(z)" ,
                         Mean= mean(z) ,"StandardDev" = sd(z) ,Median = median(z), "MedianAbsDev" =  mad(z)))
df
 
# find the outlier 

star_classification[star_classification$g < -1000,] %>% select(obj_ID, class, u, g, z) 

#The values of u,g and z for the star with obj_ID = 1237648703521095936 are very different from their respective means
#The point is an outlier, so we remove it


star_classification <- 
  star_classification[star_classification$obj_ID != 1237648703521095936, ]

# after removing the outlier, we plot again the boxplot of the spacial features

brs  <- ggplot(star_classification, aes(class,redshift,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
bi <- ggplot(star_classification, aes(class,i,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
br  <- ggplot(star_classification, aes(class,r,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
bu  <- ggplot(star_classification, aes(class,u,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
bg  <- ggplot(star_classification, aes(class,g,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
bz  <- ggplot(star_classification, aes(class,z,fill=class)) + geom_boxplot()+ theme(legend.position = "none")
grid.arrange(brs,bi,br,bu, bg , bz, nrow = 2)


# Correlation
#-------------------------------------------------------------------------------


# The correlation is a measure of how much two variables grow together. 
# In this case, we are interested in the spectral variables. The result is a matrix 6x6
star_classification %>% select(redshift,u,g,r,i,z) %>% cor 


#-------------------------------------------------------------------------------
#
#
# THE MODELS
#
#
#-------------------------------------------------------------------------------

# We are going to train now the models

# At first introduce a convenient variables
star_classification <- star_classification %>%
  mutate(isSTAR = as.factor(ifelse( as.character(class) =="STAR",1,0)),
         isQSO = as.factor(ifelse( as.character(class) =="QSO",1,0)),
         isGALAXY = as.factor(ifelse( as.character(class) =="GALAXY",1,0)))

# Divide the set into test set and train set randomly
# 10%test set 90% train set
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use set.seed(1)
test_index <-  createDataPartition(y = star_classification$class, times = 1, p = 0.1, list = FALSE)
train_set <- star_classification[-test_index,]
test_set <- star_classification[test_index,]



#-------------------------------------------------------------------------------
# Two Step Liner Model
#-------------------------------------------------------------------------------

# We call two step models, those models that require more than one training step and where usually the second model 
# provide a classification which is build upon the result of the first one

# For later use, it will be convenient We will first divide the set 
train_set_filtered <- train_set %>% filter(as.character(class) !="STAR")
test_set_filtered  <- test_set  %>% filter(as.character(class) !="STAR")

# the filtered sets will be useful when training the step 2 of the model
# to have an idea of the distribution of the galaxies and the quasars we plot a sample of 4000 points

train_set_filtered[ sample(nrow(train_set_filtered),4000),] %>%
  ggplot(aes(redshift,u )) +geom_point(aes(color = class,alpha = 0.1 ))


# Step 1
#-------------------------------------------------------------------------------

# The first step we train a model capable of separating the stars from the rest of the objects

fit_star_glm <-  glm(  isSTAR ~ redshift + i + r + u + g + z , data = train_set , family = "binomial")

# with this step, we are able to separate quite well the stars from the other objects.
# 

y_star <- predict(fit_star_glm, test_set ,type = "response")
y_star <-  as.factor( ifelse(y_star >= 0.5,1,0) )  # declare a star if the probability is above .5


# the accuracy of the first step of the model is very high
confusionMatrix( y_star , test_set$isSTAR)$overall["Accuracy"]
 


# Step 2
#-------------------------------------------------------------------------------

# The second step is again linear. Here we want to start from the train set
# and select only the galaxies and the quasars
# then we train a linear model on this subset
# this means that the model will focus only on the part of the data that is most difficult to classify

start_time <- Sys.time() #take starting time
fit_galaxy_glm <- glm( isGALAXY ~ redshift + i + r + u + g + z , data = train_set_filtered, family = "binomial"  ) # train part
end_time <- Sys.time() #take ending time
comp_time_m1 <- end_time - start_time # difference to evaluate performance of the algoritm


# and we can test the accuracy of the model on the filtered subset

hat_galaxy <- predict(fit_galaxy_glm, test_set_filtered, type = "response")
hat_galaxy <- as.factor(ifelse(hat_galaxy >= 0.5,1,0))
confusionMatrix( hat_galaxy, test_set_filtered$isGALAXY)

# Step 1 + Step 2
#------------------------------------------------------------------------------- 

# The model consist into separating the stars at first, 
#and then, in case the object is not classified as a star, separate the galaxies and the quasars
# we will define the dataset

y_model1  <- test_set %>% 
  mutate(p_star = predict(fit_star_glm, newdata = . ,type = "response"))  %>%
  mutate(p_galaxy = predict(fit_galaxy_glm,newdata = .  , type = "response")) %>% 
  mutate(pred=ifelse(p_star>=0.5,"STAR",ifelse(p_galaxy>=0.5,"GALAXY","QSO")))%>% 
  mutate(pred = as.factor(pred) ) 

# Let's see the perfeormance of the first model

cm_m1<-confusionMatrix( y_model1$pred, y_model1$class)
cm_m1


#-------------------------------------------------------------------------------
# TWO STEP RANDOM FOREST MODEL
#-------------------------------------------------------------------------------

# the second model is a two- step model as well. 

#Step 1
#-------------------------------------------------------------------------------

# The first part is identical to the one of the previous model, i.e. training a linear model to separate the stars
# we already did in the first algorithm, 

# Step 2
#-------------------------------------------------------------------------------

# Reduce dimensionality with Principal component decomposition

pca <- train_set_filtered %>% prcomp( ~ redshift + i + r + u + g + z , data =.) # we use the train set without the stars
pca
summary(pca)

#From the summary above we can see that the first two components are responsible
#for more than 95% of the variability in the data and the the first three are responsible for 98%

#add the new coordinate to the train set
train_set_filtered <- cbind(train_set_filtered,pca$x[,1:6])

# we can plot the first two components
train_set_filtered[ sample(nrow(train_set_filtered),4000),] %>% ggplot(aes(PC1,PC2 )) +geom_point(aes(color = isGALAXY,alpha = 0.5))


#  the following code might take several minutes to complete
start_time <- Sys.time()#take starting time
fit_galaxy_rf <- randomForest( isGALAXY ~ PC1 + PC2 + PC3, data = train_set_filtered)  # train step 2
end_time <- Sys.time()#take ending time
comp_time_m2 <- end_time - start_time # difference to evaluate performance of the algoritm

# We are now ready to test the algorithm to see the performance
# We have to change the coordinates also in the test set. To do so, we use the rotation matrix that pca has provided


test_set_newcoords <- test_set[,c("redshift","i","r","u","g","z")] %>% as.matrix(.)%>% 
  sweep( . , 2,pca$center) %*% # subtract the mean
  pca$rotation %>% as.data.frame(.) # rotate
test_set <- cbind(test_set,test_set_newcoords) # bind the coordinates 



# the prediction according to the second model

y_model2 <- test_set %>%
  mutate(p_star = predict(fit_star_glm, newdata = . ,type = "response")) %>% # separate stars - step 1
  mutate(p_galaxy = predict(fit_galaxy_rf,newdata = . )) %>% # separate galaxy - step 2
  mutate(pred=ifelse(p_star>=0.5,"STAR", ifelse(as.character(p_galaxy)=="1","GALAXY","QSO")))%>% # predict
  mutate(pred = as.factor(pred) )

# And we can now find the confusion matrix to evaluate the model
cm_m2 <- confusionMatrix(y_model2$pred, y_model2$class)
cm_m2

#-------------------------------------------------------------------------------
# Model 3 - Bonus model
#-------------------------------------------------------------------------------

start_time <- Sys.time()#take starting time
fit_rf_test <- randomForest( class ~ redshift + i+r+u+g+z , data = train_set ) 
end_time <- Sys.time()#take end time
comp_time_rf <- end_time - start_time # difference to evaluate performance of the algoritm


# Evaluate the performance of the full random matrix model

y_hat_rf_all <- predict(fit_rf_test,newdata = test_set  )
cm_rf <-confusionMatrix(y_hat_rf_all, test_set$class)
cm_rf


#-------------------------------------------------------------------------------
# SUMMARY
#-------------------------------------------------------------------------------

# We present a summary of al the results

models <- c("Two-step linear model", 
            "Two-step linear and random forest model", 
            "Random forest model")
accuracy <- c(cm_m1$overall["Accuracy"],cm_m2$overall["Accuracy"],cm_rf$overall["Accuracy"])
comptime <- c(comp_time_m1,comp_time_m2,comp_time_rf)
data.frame("Model"=models,"Accuracy"=accuracy,"Computation time"=comptime)
