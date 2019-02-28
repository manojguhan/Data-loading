#Objective :- To build a Machine Learning Model

# Setting up the Working Directory
setwd("D:/Data Analytics/Second/PDA/Data")

#install the xlsx package to load the file inside
install.packages("xlsx")
library(xlsx)

#Load the Input Data as a dataframe
Input_data <-  read.xlsx("Crimes.xlsx", sheetName = "Crimes")

#Check if Any Na's are there in Input_data?
ifelse(nrow((na.omit(Input_data))) == nrow(Input_data), "No NA's Found", "NA's Found")

  for (j in 1:(nrow(Converted_Input_mat_df))) 
  {
    Year = (Converted_Input_mat_df[j,3])
    Month  = (Converted_Input_mat_df[j,4])
    TotalCrimes = (Converted_Input_mat_df[j,5])
    if (Year == 1990)
  
        {
          
      Req_MonthValue = ReqMonthValue + Month
      Req_TotalCrimes = Req_TotalCrimes + TotalCrimes
      
      }
    
      }


Average_crimes_perMonth_1990 = round( Req_TotalCrimes/Req_MonthValue )



for (j in 1:(nrow(Converted_Input_mat_df))) 
{
  Year = (Converted_Input_mat_df[j,3])
  Month  = (Converted_Input_mat_df[j,4])
  TotalCrimes = (Converted_Input_mat_df[j,5])
  if (Year == 1990)
    
  {
    
    Req_MonthValue = ReqMonthValue + Month
    Req_TotalCrimes = Req_TotalCrimes + TotalCrimes
    
  }
  
}
df <- data.frame(
  V1 = c(1,3,3,5,5,6),
  V2 = c(19,19,38,19,38,19),
  V3 = c(1,3,1,7,2,10)
)





#Encoding the data 
Cateogorical_Code_df <- read.table(text = "
Category Code
GP	0
MS	1
F	0
M	1
U	0
R	1
GT3	0
LE3	1
A	0
T	1
at_home	0
health	1
other	2
services	3
teacher	4
course 0
home	1
reputation	2
mother	0
father	1
yes	0
no	1
Portuguese 0
Maths 1
", header = TRUE, stringsAsFactors = FALSE)

Converted_Input_mat_df <- Input_mat_df
Converted_Input_mat_df$school <- as.character(Converted_Input_mat_df$school)
Converted_Input_mat_df$sex <- as.character(Converted_Input_mat_df$sex)
Converted_Input_mat_df$age <- as.character(Converted_Input_mat_df$age)
Converted_Input_mat_df$address<- as.character(Converted_Input_mat_df$address)
Converted_Input_mat_df$famsize<- as.character(Converted_Input_mat_df$famsize)
Converted_Input_mat_df$Pstatus<- as.character(Converted_Input_mat_df$Pstatus)
Converted_Input_mat_df$Mjob<- as.character(Converted_Input_mat_df$Mjob)
Converted_Input_mat_df$Fjob<- as.character(Converted_Input_mat_df$Fjob)
Converted_Input_mat_df$reason<- as.character(Converted_Input_mat_df$reason)
Converted_Input_mat_df$guardian<- as.character(Converted_Input_mat_df$guardian)
Converted_Input_mat_df$schoolsup<- as.character(Converted_Input_mat_df$schoolsup)
Converted_Input_mat_df$famsup<- as.character(Converted_Input_mat_df$famsup)
Converted_Input_mat_df$paid<- as.character(Converted_Input_mat_df$paid)
Converted_Input_mat_df$activities<- as.character(Converted_Input_mat_df$activities)
Converted_Input_mat_df$nursery<- as.character(Converted_Input_mat_df$nursery)
Converted_Input_mat_df$higher<- as.character(Converted_Input_mat_df$higher)
Converted_Input_mat_df$internet<- as.character(Converted_Input_mat_df$internet)
Converted_Input_mat_df$romantic<- as.character(Converted_Input_mat_df$romantic)

for (i in 1:(ncol(Converted_Input_mat_df)))
  {
    for (j in 1:(nrow(Converted_Input_mat_df))) 
      {
        ReqValue = (Converted_Input_mat_df[j,i])
        for (k in 1:(nrow(Cateogorical_Code_df))) 
        {
          ReqValue2 = (Cateogorical_Code_df[k,1])
          ReqValue3 =  (Cateogorical_Code_df[k,2])
          if (ReqValue == ReqValue2)
          {
            Converted_Input_mat_df[j,i] <- ReqValue3
          }
        }
      }
  }


str(Converted_Input_mat_df)
for (i in 1:(ncol(Converted_Input_mat_df)))
{
  if (typeof(Converted_Input_mat_df[,i]) == "character")
  {
    Converted_Input_mat_df[,i] <- as.factor(Converted_Input_mat_df[,i])
  }
}

str(Converted_Input_mat_df)


missingLevelsToNA<-function(object,data){
  
  #Obtain factor predictors in the model and their levels ------------------
  
  factors<-(gsub("[-^0-9]|as.factor|\\(|\\)", "",names(unlist(object$xlevels))))
  factorLevels<-unname(unlist(object$xlevels))
  modelFactors<-as.data.frame(cbind(factors,factorLevels))
  
  
  #Select column names in your data that are factor predictors in your model -----
  
  predictors<-names(data[names(data) %in% factors])
  
  
  #For each factor predictor in your data if the level is not in the model set the value to NA --------------
  
  for (i in 1:length(predictors)){
    found<-data[,predictors[i]] %in% modelFactors[modelFactors$factors==predictors[i],]$factorLevels
    if (any(!found)) data[!found,predictors[i]]<-NA
  }
  
  data
  
}



install.packages("mlbench")
library(mlbench)
install.packages("caret",dependencies = TRUE)
install.packages("stringi")
library(caret)


# prepare training scheme
GLM_control <- trainControl(method="repeatedcv", number=10, repeats=10)

# train the model
BestFeaturesModel <- train(G3~., data=Converted_Input_mat_df, method="glm", preProcess="scale", trControl=GLM_control)

# estimate variable importance
importance <- varImp(BestFeaturesModel, scale=FALSE)

# summarize importance
print(importance)
Importance_df <- do.call(rbind.data.frame, (importance))
Importance_df <- head(Importance_df,-2)

# plot importance
plot(importance)



#Random Sampling - Regression
set.seed(007)
smp_size <- floor(0.7 * nrow(Converted_Input_mat_df))
train_ind <- sample(seq_len(nrow(Converted_Input_mat_df)), size = smp_size)
Random_Sampling_RegressionTraining <- Converted_Input_mat_df[train_ind, ]
Random_Sampling_RegressionTesting <- Converted_Input_mat_df[-train_ind, ]

#Stratified Sampling :- Regression
install.packages("caTools")
library(caTools)
train_ind = sample.split(Converted_Input_mat_df$G3, SplitRatio=0.7)
Stratified_Sampling_RegressionTraining = Converted_Input_mat_df[ train_ind,]
Stratified_Sampling_RegressionTesting  = Converted_Input_mat_df[!train_ind,]

#Random Sampling - Classification
Random_Sampling_ClassificationTraining <- Random_Sampling_RegressionTraining
Random_Sampling_ClassificationTraining$G3 <- ifelse(Random_Sampling_ClassificationTraining$G3 < 10 ,0,1)
Random_Sampling_ClassificationTesting<- Random_Sampling_RegressionTesting
Random_Sampling_ClassificationTesting$G3 <- ifelse(Random_Sampling_ClassificationTesting$G3 < 10 ,0,1)

Random_Sampling_ClassificationTraining$G3 <- as.character(Random_Sampling_ClassificationTraining$G3)
Random_Sampling_ClassificationTraining$G3 <- as.factor(Random_Sampling_ClassificationTraining$G3)
Random_Sampling_ClassificationTesting$G3 <- as.character(Random_Sampling_ClassificationTesting$G3)
Random_Sampling_ClassificationTesting$G3 <- as.factor(Random_Sampling_ClassificationTesting$G3)

#Stratified Sampling - Classification
Stratified_Sampling_ClassificationTraining <- Stratified_Sampling_RegressionTraining
Stratified_Sampling_ClassificationTraining$G3 <- ifelse(Stratified_Sampling_ClassificationTraining$G3 < 10 ,0,1)
Stratified_Sampling_ClassificationTesting<- Stratified_Sampling_RegressionTesting
Stratified_Sampling_ClassificationTesting$G3 <- ifelse(Stratified_Sampling_ClassificationTesting$G3 < 10 ,0,1)

Stratified_Sampling_ClassificationTraining$G3 <- as.character(Stratified_Sampling_ClassificationTraining$G3)
Stratified_Sampling_ClassificationTraining$G3 <- as.factor(Stratified_Sampling_ClassificationTraining$G3)
Stratified_Sampling_ClassificationTesting$G3 <- as.character(Stratified_Sampling_ClassificationTesting$G3)
Stratified_Sampling_ClassificationTesting$G3 <- as.factor(Stratified_Sampling_ClassificationTesting$G3)


#Top 10 Best Features + dependent Variable only
Random_Sampling_RegressionTraining_BestFeatures <- Random_Sampling_RegressionTraining [,c(
"G2",
"absences",
"famrel",
"G1",
"activities",
"age",
"school",
"schoolsup",
"Walc",
"romantic", 
"G3"
)]

Random_Sampling_RegressionTesting_BestFeatures <- Random_Sampling_RegressionTesting [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]

Stratified_Sampling_RegressionTraining_BestFeatures <- Stratified_Sampling_RegressionTraining [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]


Stratified_Sampling_RegressionTesting_BestFeatures <- Stratified_Sampling_RegressionTesting [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]


Random_Sampling_ClassificationTraining_BestFeatures <- Random_Sampling_ClassificationTraining [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]



Random_Sampling_ClassificationTesting_BestFeatures <- Random_Sampling_ClassificationTesting [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]



Stratified_Sampling_ClassificationTraining_BestFeatures <- Stratified_Sampling_ClassificationTraining [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]



Stratified_Sampling_ClassificationTesting_BestFeatures <- Stratified_Sampling_ClassificationTesting [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]


library(ggplot2)

IGP <- ggplot(data=Converted_Input_mat_df, aes(x=Converted_Input_mat_df$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("G3 Vs Number of Students - Available Data") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light()+ylim(0,60) +
  theme(plot.title = element_text(hjust = 0.5))


RRTR <- ggplot(data=Random_Sampling_RegressionTraining, aes(x=Random_Sampling_RegressionTraining$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Random Sampling - Regression Training ") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light() +ylim(0,60)+
  theme(plot.title = element_text(hjust = 0.5))


RRTE <- ggplot(data=Random_Sampling_RegressionTesting, aes(x=Random_Sampling_RegressionTesting$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Random Sampling - Regression Testing ") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light() +ylim(0,60)+
  theme(plot.title = element_text(hjust = 0.5))

SRTR <- ggplot(data=Stratified_Sampling_RegressionTraining, aes(x=Stratified_Sampling_RegressionTraining$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Stratified Sampling - Regression Training") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light()+ylim(0,60)+
  theme(plot.title = element_text(hjust = 0.5))

SRTE <- ggplot(data=Stratified_Sampling_RegressionTesting, aes(x=Stratified_Sampling_RegressionTesting$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Stratified Sampling - Regression Testing") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light()+ylim(0,60)+
  theme(plot.title = element_text(hjust = 0.5))


RCTR <- ggplot(data=Random_Sampling_ClassificationTraining, aes(x=Random_Sampling_ClassificationTraining$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Random Sampling - Classification Training ") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light() +ylim(0,260)+
  theme(plot.title = element_text(hjust = 0.5))


RCTE <- ggplot(data=Random_Sampling_ClassificationTesting, aes(x=Random_Sampling_ClassificationTesting$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Random Sampling - Classification Testing ") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light() +ylim(0,260)+
  theme(plot.title = element_text(hjust = 0.5))

SCTR <- ggplot(data=Stratified_Sampling_ClassificationTraining, aes(x=Stratified_Sampling_ClassificationTraining$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Stratified Sampling - Classification Training") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light()+ylim(0,260)+
  theme(plot.title = element_text(hjust = 0.5))

SCTE <- ggplot(data=Stratified_Sampling_ClassificationTesting, aes(x=Stratified_Sampling_ClassificationTesting$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Stratified Sampling - Classification Testing") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light()+ylim(0,260)+
  theme(plot.title = element_text(hjust = 0.5))


library(grid)

# Move to a new page
grid.newpage()

# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 5, ncol = 2)))

# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

# Arrange the plots
print(IGP, vp = define_region(row = 1, col = 1:2))   # Span over two columns
print(RRTR, vp = define_region(row = 2, col = 1))
print(RRTE, vp = define_region(row = 2, col = 2))
print(SRTR, vp = define_region(row = 3, col = 1))
print(SRTE, vp = define_region(row = 3, col = 2))
print(RCTR, vp = define_region(row = 4, col = 1))
print(RCTE, vp = define_region(row = 4, col = 2))
print(SCTR, vp = define_region(row = 5, col = 1))
print(SCTE, vp = define_region(row = 5, col = 2))

#Simple Linear Regression
RandomSampling_Simple_lr_model_Mat <- lm(Random_Sampling_RegressionTraining$G3 ~ G2, # regression formula
                              data=Random_Sampling_RegressionTraining)

RandomSampling_Simple_lr_predict_Mat <- predict.lm(RandomSampling_Simple_lr_model_Mat, newdata=Random_Sampling_RegressionTesting)

StratifiedSampling_Simple_lr_model_Mat <- lm(Stratified_Sampling_RegressionTraining$G3 ~ G2, # regression formula
                                         data=Stratified_Sampling_RegressionTraining)

StratifiedSampling_Simple_lr_predict_Mat <- predict.lm(StratifiedSampling_Simple_lr_model_Mat, newdata=Stratified_Sampling_RegressionTesting)


#Multiple Linear Regression
RandomSampling_Multiple_lr_model_mat <- lm(Random_Sampling_RegressionTraining$G3 ~., # regression formula
                                     data=Random_Sampling_RegressionTraining)

RandomSampling_Multiple_lr_model_predict_mat <- predict.lm(RandomSampling_Multiple_lr_model_mat, newdata=Random_Sampling_RegressionTesting)

RandomSampling_MultipleBestFeatures_lr_model_mat <- lm(Random_Sampling_RegressionTraining_BestFeatures$G3 ~., # regression formula
                                     data=Random_Sampling_RegressionTraining_BestFeatures)

RandomSampling_MultipleBestFeatures_lr_model_predict_mat <- predict.lm(RandomSampling_MultipleBestFeatures_lr_model_mat, newdata=Random_Sampling_RegressionTesting_BestFeatures)



Stratified_Multiple_lr_model_mat <- lm(Stratified_Sampling_RegressionTraining$G3 ~., # regression formula
                                       data=Stratified_Sampling_RegressionTraining)

Stratified_Sampling_RegressionTesting <-missingLevelsToNA(Stratified_Multiple_lr_model_mat, Stratified_Sampling_RegressionTesting)

Stratified_Multiple_lr_model_mat_predict <- predict.lm(Stratified_Multiple_lr_model_mat, newdata=Stratified_Sampling_RegressionTesting)


StratifiedSampling_MultipleBestFeatures_lr_model <- lm(Stratified_Sampling_RegressionTraining_BestFeatures$G3 ~., # regression formula
                                                   data=Stratified_Sampling_RegressionTraining_BestFeatures)

StratifiedSampling_MultipleBestFeatures_lr_model_predict <- predict.lm(StratifiedSampling_MultipleBestFeatures_lr_model, newdata=Random_Sampling_RegressionTesting_BestFeatures)


#Random Forest
install.packages("randomForest")
library(randomForest)
library(mlbench)
install.packages("e1071")
library(e1071)
library(caret)

#Random forset
RandomSampling_RandomForest_Mat <- randomForest(G3 ~.,data=Random_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
RandomSampling_RandomForest_Mat_Pred <- predict(RandomSampling_RandomForest_Mat, newdata = Random_Sampling_ClassificationTesting)
confusionMatrix(RandomSampling_RandomForest_Mat_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_RandomForest_Mat_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")


StratifiedSampling_RandomForest_Mat <- randomForest(G3 ~.,data=Stratified_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
StratifiedSampling_RandomForest_Mat_Pred <- predict(StratifiedSampling_RandomForest_Mat, newdata = Stratified_Sampling_ClassificationTesting)
confusionMatrix(StratifiedSampling_RandomForest_Mat_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_RandomForest_Mat_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")

RandomSampling_RandomForest_BestFeatures_Mat <- randomForest(G3 ~.,data=Random_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
RandomSampling_RandomForest_BestFeatures_Mat_Pred <- predict(RandomSampling_RandomForest_BestFeatures_Mat, newdata = Random_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(RandomSampling_RandomForest_BestFeatures_Mat_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_RandomForest_BestFeatures_Mat_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


StratifiedSampling_RandomForest_BestFeatures_Mat <- randomForest(G3 ~.,data=Stratified_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
StratifiedSampling_RandomForest_BestFeatures_Mat_Pred <- predict(StratifiedSampling_RandomForest_BestFeatures_Mat, newdata = Stratified_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(StratifiedSampling_RandomForest_BestFeatures_Mat_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_RandomForest_BestFeatures_Mat_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


#NaiveBayes

RandomSampling_NaiveBayes_Mat <- naiveBayes(G3 ~.,data=Random_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
RandomSampling_NaiveBayes_Mat_Pred <- predict(RandomSampling_NaiveBayes_Mat, newdata = Random_Sampling_ClassificationTesting)
confusionMatrix(RandomSampling_NaiveBayes_Mat_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_NaiveBayes_Mat_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")


StratifiedSampling_NaiveBayes_Mat <- naiveBayes(G3 ~.,data=Stratified_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
StratifiedSampling_NaiveBayes_Mat_Pred <- predict(StratifiedSampling_NaiveBayes_Mat, newdata = Stratified_Sampling_ClassificationTesting)
confusionMatrix(StratifiedSampling_NaiveBayes_Mat_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_NaiveBayes_Mat_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")

RandomSampling_NaiveBayes_BestFeatures_Mat <- naiveBayes(G3 ~.,data=Random_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
RandomSampling_NaiveBayes_BestFeatures_Mat_Pred <- predict(RandomSampling_NaiveBayes_BestFeatures_Mat, newdata = Random_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(RandomSampling_NaiveBayes_BestFeatures_Mat_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_NaiveBayes_BestFeatures_Mat_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


StratifiedSampling_NaiveBayes_BestFeatures_Mat <- naiveBayes(G3 ~.,data=Stratified_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
StratifiedSampling_NaiveBayes_BestFeatures_Mat_Pred <- predict(StratifiedSampling_NaiveBayes_BestFeatures_Mat, newdata = Stratified_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(StratifiedSampling_NaiveBayes_BestFeatures_Mat_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_NaiveBayes_BestFeatures_Mat_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


#SVM Model
RandomSampling_Svm_Mat <- svm(G3 ~.,data=Random_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
RandomSampling_Svm_Mat_Pred <- predict(RandomSampling_Svm_Mat, newdata = Random_Sampling_ClassificationTesting)
confusionMatrix(RandomSampling_Svm_Mat_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_Svm_Mat_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")


StratifiedSampling_Svm_Mat <- svm(G3 ~.,data=Stratified_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
StratifiedSampling_Svm_Mat_Pred <- predict(StratifiedSampling_Svm_Mat, newdata = Stratified_Sampling_ClassificationTesting)
confusionMatrix(StratifiedSampling_Svm_Mat_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_Svm_Mat_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")

RandomSampling_Svm_BestFeatures_Mat <- svm(G3 ~.,data=Random_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
RandomSampling_Svm_BestFeatures_Mat_Pred <- predict(RandomSampling_Svm_BestFeatures_Mat, newdata = Random_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(RandomSampling_Svm_BestFeatures_Mat_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_Svm_BestFeatures_Mat_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


StratifiedSampling_Svm_BestFeatures_Mat <- svm(G3 ~.,data=Stratified_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
StratifiedSampling_Svm_BestFeatures_Mat_Pred <- predict(StratifiedSampling_Svm_BestFeatures_Mat, newdata = Stratified_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(StratifiedSampling_Svm_BestFeatures_Mat_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_Svm_BestFeatures_Mat_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")

#Ensemble to be done now ---------------------------------------------
tuneParams <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final'
)

#Ensemble - Boosting

library(fastAdaboost)
Stratified_Sampling_AdaBoosting_Mat <- train(G3~., Stratified_Sampling_ClassificationTraining, method ='adaboost',trControl = tuneParams, tunelength = 3)
StratifiedSampling_AdaBoosting_Mat_Pred <- predict(Stratified_Sampling_AdaBoosting_Mat, newdata = Stratified_Sampling_ClassificationTesting)
confusionMatrix(StratifiedSampling_AdaBoosting_Mat_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_AdaBoosting_Mat_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")


Stratified_Sampling_BestFeatures_AdaBoosting_Mat <- train(G3~., Stratified_Sampling_ClassificationTraining_BestFeatures, method ='adaboost',trControl = tuneParams, tunelength = 3)
StratifiedSampling_BestFeatures_AdaBoosting_Mat_Pred <- predict(Stratified_Sampling_BestFeatures_AdaBoosting_Mat, newdata = Stratified_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(StratifiedSampling_BestFeatures_AdaBoosting_Mat_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_BestFeatures_AdaBoosting_Mat_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


## Now doing the Same for Portuguese...........................................
Converted_Input_por_df <- Input_Por_df
Converted_Input_por_df$school <- as.character(Converted_Input_por_df$school)
Converted_Input_por_df$sex <- as.character(Converted_Input_por_df$sex)
Converted_Input_por_df$age <- as.character(Converted_Input_por_df$age)
Converted_Input_por_df$address<- as.character(Converted_Input_por_df$address)
Converted_Input_por_df$famsize<- as.character(Converted_Input_por_df$famsize)
Converted_Input_por_df$Pstatus<- as.character(Converted_Input_por_df$Pstatus)
Converted_Input_por_df$Mjob<- as.character(Converted_Input_por_df$Mjob)
Converted_Input_por_df$Fjob<- as.character(Converted_Input_por_df$Fjob)
Converted_Input_por_df$reason<- as.character(Converted_Input_por_df$reason)
Converted_Input_por_df$guardian<- as.character(Converted_Input_por_df$guardian)
Converted_Input_por_df$schoolsup<- as.character(Converted_Input_por_df$schoolsup)
Converted_Input_por_df$famsup<- as.character(Converted_Input_por_df$famsup)
Converted_Input_por_df$paid<- as.character(Converted_Input_por_df$paid)
Converted_Input_por_df$activities<- as.character(Converted_Input_por_df$activities)
Converted_Input_por_df$nursery<- as.character(Converted_Input_por_df$nursery)
Converted_Input_por_df$higher<- as.character(Converted_Input_por_df$higher)
Converted_Input_por_df$internet<- as.character(Converted_Input_por_df$internet)
Converted_Input_por_df$romantic<- as.character(Converted_Input_por_df$romantic)

for (i in 1:(ncol(Converted_Input_por_df)))
{
  for (j in 1:(nrow(Converted_Input_por_df))) 
  {
    ReqValue = (Converted_Input_por_df[j,i])
    for (k in 1:(nrow(Cateogorical_Code_df))) 
    {
      ReqValue2 = (Cateogorical_Code_df[k,1])
      ReqValue3 =  (Cateogorical_Code_df[k,2])
      if (ReqValue == ReqValue2)
      {
        Converted_Input_por_df[j,i] <- ReqValue3
      }
    }
  }
}


str(Converted_Input_por_df)
for (i in 1:(ncol(Converted_Input_por_df)))
{
  if (typeof(Converted_Input_por_df[,i]) == "character")
  {
    Converted_Input_por_df[,i] <- as.factor(Converted_Input_por_df[,i])
  }
}






# prepare training scheme
GLM_control <- trainControl(method="repeatedcv", number=10, repeats=10)

# train the model
BestFeaturesModel <- train(G3~., data=Converted_Input_por_df, method="glm", preProcess="scale", trControl=GLM_control)

# estipore variable importance
importance <- varImp(BestFeaturesModel, scale=FALSE)

# summarize importance
print(importance)
Importance_df <- do.call(rbind.data.frame, (importance))
Importance_df <- head(Importance_df,-2)

# plot importance
plot(importance)



#Random Sampling - Regression
set.seed(007)
smp_size <- floor(0.7 * nrow(Converted_Input_por_df))
train_ind <- sample(seq_len(nrow(Converted_Input_por_df)), size = smp_size)
Random_Sampling_RegressionTraining <- Converted_Input_por_df[train_ind, ]
Random_Sampling_RegressionTesting <- Converted_Input_por_df[-train_ind, ]

#Stratified Sampling :- Regression
train_ind = sample.split(Converted_Input_por_df$G3, SplitRatio=0.7)
Stratified_Sampling_RegressionTraining = Converted_Input_por_df[ train_ind,]
Stratified_Sampling_RegressionTesting  = Converted_Input_por_df[!train_ind,]

#Random Sampling - Classification
Random_Sampling_ClassificationTraining <- Random_Sampling_RegressionTraining
Random_Sampling_ClassificationTraining$G3 <- ifelse(Random_Sampling_ClassificationTraining$G3 < 10 ,0,1)
Random_Sampling_ClassificationTesting<- Random_Sampling_RegressionTesting
Random_Sampling_ClassificationTesting$G3 <- ifelse(Random_Sampling_ClassificationTesting$G3 < 10 ,0,1)

Random_Sampling_ClassificationTraining$G3 <- as.character(Random_Sampling_ClassificationTraining$G3)
Random_Sampling_ClassificationTraining$G3 <- as.factor(Random_Sampling_ClassificationTraining$G3)
Random_Sampling_ClassificationTesting$G3 <- as.character(Random_Sampling_ClassificationTesting$G3)
Random_Sampling_ClassificationTesting$G3 <- as.factor(Random_Sampling_ClassificationTesting$G3)

#Stratified Sampling - Classification
Stratified_Sampling_ClassificationTraining <- Stratified_Sampling_RegressionTraining
Stratified_Sampling_ClassificationTraining$G3 <- ifelse(Stratified_Sampling_ClassificationTraining$G3 < 10 ,0,1)
Stratified_Sampling_ClassificationTesting<- Stratified_Sampling_RegressionTesting
Stratified_Sampling_ClassificationTesting$G3 <- ifelse(Stratified_Sampling_ClassificationTesting$G3 < 10 ,0,1)

Stratified_Sampling_ClassificationTraining$G3 <- as.character(Stratified_Sampling_ClassificationTraining$G3)
Stratified_Sampling_ClassificationTraining$G3 <- as.factor(Stratified_Sampling_ClassificationTraining$G3)
Stratified_Sampling_ClassificationTesting$G3 <- as.character(Stratified_Sampling_ClassificationTesting$G3)
Stratified_Sampling_ClassificationTesting$G3 <- as.factor(Stratified_Sampling_ClassificationTesting$G3)


#Top 10 Best Features + dependent Variable only
Random_Sampling_RegressionTraining_BestFeatures <- Random_Sampling_RegressionTraining [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]

Random_Sampling_RegressionTesting_BestFeatures <- Random_Sampling_RegressionTesting [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]

Stratified_Sampling_RegressionTraining_BestFeatures <- Stratified_Sampling_RegressionTraining [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]


Stratified_Sampling_RegressionTesting_BestFeatures <- Stratified_Sampling_RegressionTesting [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]


Random_Sampling_ClassificationTraining_BestFeatures <- Random_Sampling_ClassificationTraining [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]



Random_Sampling_ClassificationTesting_BestFeatures <- Random_Sampling_ClassificationTesting [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]



Stratified_Sampling_ClassificationTraining_BestFeatures <- Stratified_Sampling_ClassificationTraining [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]



Stratified_Sampling_ClassificationTesting_BestFeatures <- Stratified_Sampling_ClassificationTesting [,c(
  "G2",
  "absences",
  "famrel",
  "G1",
  "activities",
  "age",
  "school",
  "schoolsup",
  "Walc",
  "romantic", 
  "G3"
)]



IGP <- ggplot(data=Converted_Input_por_df, aes(x=Converted_Input_por_df$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("G3 Vs Number of Students - Available Data") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light()+ylim(0,60) +
  theme(plot.title = element_text(hjust = 0.5))


RRTR <- ggplot(data=Random_Sampling_RegressionTraining, aes(x=Random_Sampling_RegressionTraining$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Random Sampling - Regression Training ") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light() +ylim(0,60)+
  theme(plot.title = element_text(hjust = 0.5))


RRTE <- ggplot(data=Random_Sampling_RegressionTesting, aes(x=Random_Sampling_RegressionTesting$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Random Sampling - Regression Testing ") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light() +ylim(0,60)+
  theme(plot.title = element_text(hjust = 0.5))

SRTR <- ggplot(data=Stratified_Sampling_RegressionTraining, aes(x=Stratified_Sampling_RegressionTraining$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Stratified Sampling - Regression Training") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light()+ylim(0,60)+
  theme(plot.title = element_text(hjust = 0.5))

SRTE <- ggplot(data=Stratified_Sampling_RegressionTesting, aes(x=Stratified_Sampling_RegressionTesting$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Stratified Sampling - Regression Testing") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light()+ylim(0,60)+
  theme(plot.title = element_text(hjust = 0.5))


RCTR <- ggplot(data=Random_Sampling_ClassificationTraining, aes(x=Random_Sampling_ClassificationTraining$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Random Sampling - Classification Training ") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light() +ylim(0,260)+
  theme(plot.title = element_text(hjust = 0.5))


RCTE <- ggplot(data=Random_Sampling_ClassificationTesting, aes(x=Random_Sampling_ClassificationTesting$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Random Sampling - Classification Testing ") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light() +ylim(0,260)+
  theme(plot.title = element_text(hjust = 0.5))

SCTR <- ggplot(data=Stratified_Sampling_ClassificationTraining, aes(x=Stratified_Sampling_ClassificationTraining$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Stratified Sampling - Classification Training") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light()+ylim(0,260)+
  theme(plot.title = element_text(hjust = 0.5))

SCTE <- ggplot(data=Stratified_Sampling_ClassificationTesting, aes(x=Stratified_Sampling_ClassificationTesting$G3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust= -0.5 )+ ggtitle("Stratified Sampling - Classification Testing") +
  xlab("G3 Marks") + ylab("Number of Students")+theme_light()+ylim(0,260)+
  theme(plot.title = element_text(hjust = 0.5))


# Move to a new page
grid.newpage()

# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 5, ncol = 2)))

# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

# Arrange the plots
print(IGP, vp = define_region(row = 1, col = 1:2))   # Span over two columns
print(RRTR, vp = define_region(row = 2, col = 1))
print(RRTE, vp = define_region(row = 2, col = 2))
print(SRTR, vp = define_region(row = 3, col = 1))
print(SRTE, vp = define_region(row = 3, col = 2))
print(RCTR, vp = define_region(row = 4, col = 1))
print(RCTE, vp = define_region(row = 4, col = 2))
print(SCTR, vp = define_region(row = 5, col = 1))
print(SCTE, vp = define_region(row = 5, col = 2))

#Simple Linear Regression
RandomSampling_Simple_lr_model_por <- lm(Random_Sampling_RegressionTraining$G3 ~ G2, # regression formula
                                         data=Random_Sampling_RegressionTraining)

RandomSampling_Simple_lr_predict_por <- predict.lm(RandomSampling_Simple_lr_model_por, newdata=Random_Sampling_RegressionTesting)

StratifiedSampling_Simple_lr_model_por <- lm(Stratified_Sampling_RegressionTraining$G3 ~ G2, # regression formula
                                             data=Stratified_Sampling_RegressionTraining)

StratifiedSampling_Simple_lr_predict_por <- predict.lm(StratifiedSampling_Simple_lr_model_por, newdata=Stratified_Sampling_RegressionTesting)


#Multiple Linear Regression
RandomSampling_Multiple_lr_model_por <- lm(Random_Sampling_RegressionTraining$G3 ~., # regression formula
                                           data=Random_Sampling_RegressionTraining)

RandomSampling_Multiple_lr_model_predict_por <- predict.lm(RandomSampling_Multiple_lr_model_por, newdata=Random_Sampling_RegressionTesting)

RandomSampling_MultipleBestFeatures_lr_model_por <- lm(Random_Sampling_RegressionTraining_BestFeatures$G3 ~., # regression formula
                                                       data=Random_Sampling_RegressionTraining_BestFeatures)

RandomSampling_MultipleBestFeatures_lr_model_predict_por <- predict.lm(RandomSampling_MultipleBestFeatures_lr_model_por, newdata=Random_Sampling_RegressionTesting_BestFeatures)



Stratified_Multiple_lr_model_por <- lm(Stratified_Sampling_RegressionTraining$G3 ~., # regression formula
                                       data=Stratified_Sampling_RegressionTraining)

Stratified_Sampling_RegressionTesting <-missingLevelsToNA(Stratified_Multiple_lr_model_por, Stratified_Sampling_RegressionTesting)

Stratified_Multiple_lr_model_por_predict <- predict.lm(Stratified_Multiple_lr_model_por, newdata=Stratified_Sampling_RegressionTesting)


StratifiedSampling_MultipleBestFeatures_lr_model <- lm(Stratified_Sampling_RegressionTraining_BestFeatures$G3 ~., # regression formula
                                                       data=Stratified_Sampling_RegressionTraining_BestFeatures)

StratifiedSampling_MultipleBestFeatures_lr_model_predict <- predict.lm(StratifiedSampling_MultipleBestFeatures_lr_model, newdata=Random_Sampling_RegressionTesting_BestFeatures)


#Random Forest

RandomSampling_RandomForest_por <- randomForest(G3 ~.,data=Random_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
RandomSampling_RandomForest_por_Pred <- predict(RandomSampling_RandomForest_por, newdata = Random_Sampling_ClassificationTesting)
confusionMatrix(RandomSampling_RandomForest_por_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_RandomForest_por_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")


StratifiedSampling_RandomForest_por <- randomForest(G3 ~.,data=Stratified_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
StratifiedSampling_RandomForest_por_Pred <- predict(StratifiedSampling_RandomForest_por, newdata = Stratified_Sampling_ClassificationTesting)
confusionMatrix(StratifiedSampling_RandomForest_por_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_RandomForest_por_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")

RandomSampling_RandomForest_BestFeatures_por <- randomForest(G3 ~.,data=Random_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
RandomSampling_RandomForest_BestFeatures_por_Pred <- predict(RandomSampling_RandomForest_BestFeatures_por, newdata = Random_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(RandomSampling_RandomForest_BestFeatures_por_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_RandomForest_BestFeatures_por_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


StratifiedSampling_RandomForest_BestFeatures_por <- randomForest(G3 ~.,data=Stratified_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
StratifiedSampling_RandomForest_BestFeatures_por_Pred <- predict(StratifiedSampling_RandomForest_BestFeatures_por, newdata = Stratified_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(StratifiedSampling_RandomForest_BestFeatures_por_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_RandomForest_BestFeatures_por_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


#NaiveBayes

RandomSampling_NaiveBayes_por <- naiveBayes(G3 ~.,data=Random_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
RandomSampling_NaiveBayes_por_Pred <- predict(RandomSampling_NaiveBayes_por, newdata = Random_Sampling_ClassificationTesting)
confusionMatrix(RandomSampling_NaiveBayes_por_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_NaiveBayes_por_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")


StratifiedSampling_NaiveBayes_por <- naiveBayes(G3 ~.,data=Stratified_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
StratifiedSampling_NaiveBayes_por_Pred <- predict(StratifiedSampling_NaiveBayes_por, newdata = Stratified_Sampling_ClassificationTesting)
confusionMatrix(StratifiedSampling_NaiveBayes_por_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_NaiveBayes_por_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")

RandomSampling_NaiveBayes_BestFeatures_por <- naiveBayes(G3 ~.,data=Random_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
RandomSampling_NaiveBayes_BestFeatures_por_Pred <- predict(RandomSampling_NaiveBayes_BestFeatures_por, newdata = Random_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(RandomSampling_NaiveBayes_BestFeatures_por_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_NaiveBayes_BestFeatures_por_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


StratifiedSampling_NaiveBayes_BestFeatures_por <- naiveBayes(G3 ~.,data=Stratified_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
StratifiedSampling_NaiveBayes_BestFeatures_por_Pred <- predict(StratifiedSampling_NaiveBayes_BestFeatures_por, newdata = Stratified_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(StratifiedSampling_NaiveBayes_BestFeatures_por_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_NaiveBayes_BestFeatures_por_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


#SVM Model
RandomSampling_Svm_por <- svm(G3 ~.,data=Random_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
RandomSampling_Svm_por_Pred <- predict(RandomSampling_Svm_por, newdata = Random_Sampling_ClassificationTesting)
confusionMatrix(RandomSampling_Svm_por_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_Svm_por_Pred, Random_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")


StratifiedSampling_Svm_por <- svm(G3 ~.,data=Stratified_Sampling_ClassificationTraining,importance = TRUE, ntree = 2000)
StratifiedSampling_Svm_por_Pred <- predict(StratifiedSampling_Svm_por, newdata = Stratified_Sampling_ClassificationTesting)
confusionMatrix(StratifiedSampling_Svm_por_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_Svm_por_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")

RandomSampling_Svm_BestFeatures_por <- svm(G3 ~.,data=Random_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
RandomSampling_Svm_BestFeatures_por_Pred <- predict(RandomSampling_Svm_BestFeatures_por, newdata = Random_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(RandomSampling_Svm_BestFeatures_por_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(RandomSampling_Svm_BestFeatures_por_Pred, Random_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


StratifiedSampling_Svm_BestFeatures_por <- svm(G3 ~.,data=Stratified_Sampling_ClassificationTraining_BestFeatures,importance = TRUE, ntree = 2000)
StratifiedSampling_Svm_BestFeatures_por_Pred <- predict(StratifiedSampling_Svm_BestFeatures_por, newdata = Stratified_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(StratifiedSampling_Svm_BestFeatures_por_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_Svm_BestFeatures_por_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")


tuneParams <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final'
)

#Ensemble - Boosting

Stratified_Sampling_AdaBoosting_por <- train(G3~., Stratified_Sampling_ClassificationTraining, method ='adaboost',trControl = tuneParams, tunelength = 3)
StratifiedSampling_AdaBoosting_por_Pred <- predict(Stratified_Sampling_AdaBoosting_por, newdata = Stratified_Sampling_ClassificationTesting)
confusionMatrix(StratifiedSampling_AdaBoosting_por_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_AdaBoosting_por_Pred, Stratified_Sampling_ClassificationTesting$G3, mode = "prec_recall", positive="0")


Stratified_Sampling_BestFeatures_AdaBoosting_Por <- train(G3~., Stratified_Sampling_ClassificationTraining_BestFeatures, method ='adaboost',trControl = tuneParams, tunelength = 3)
StratifiedSampling_BestFeatures_AdaBoosting_Por_Pred <- predict(Stratified_Sampling_BestFeatures_AdaBoosting_Por, newdata = Stratified_Sampling_ClassificationTesting_BestFeatures)
confusionMatrix(StratifiedSampling_BestFeatures_AdaBoosting_Por_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="1")
confusionMatrix(StratifiedSampling_BestFeatures_AdaBoosting_Por_Pred, Stratified_Sampling_ClassificationTesting_BestFeatures$G3, mode = "prec_recall", positive="0")









