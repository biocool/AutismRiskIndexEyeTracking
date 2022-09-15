#In this script I'm goign to do some feature importance 
library(caret)
library(TH.data)
library(Boruta)

data("GlaucomaM", package = "TH.data")
trainData <- GlaucomaM
head(trainData)
View(trainData)

#using Boruta package ==================================
library(Boruta)
boruta_output <- Boruta(recentDxJ_dxCode ~ ., data=trainData, doTrace=0)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort
imps2[order(-imps2$meanImp), ]
# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  
save(boruta_output, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/boruta_output")

final_boruta_output <- colMeans(boruta_output$ImpHistory)
final_boruta_output [final_boruta_output == -Inf] <- 0
write.csv(final_boruta_output,
          file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/final_boruta_output.csv")


#using caret package ==================================
#train() the desired model using the caret package.
#Then, use varImp() to determine the feature importances.

library(caret)
ET_subjects_having_all_five <- 
  read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/All_ET_Combination/12345_weka.csv")
dim(ET_subjects_having_all_five)

ET_subjects_having_all_five[ET_subjects_having_all_five=="?"] <- NA

View(ET_subjects_having_all_five)
trainData <- ET_subjects_having_all_five
set.seed(100)
#converting to numeric 
temp <- apply(ET_subjects_having_all_five[,c(1:52)], 2, as.numeric)
temp <- as.data.frame(temp)
temp$recentDxJ_dxCode <- as.factor(ET_subjects_having_all_five$recentDxJ_dxCode)
dim(temp)

ET_subjects_having_all_five <- temp

rPartMod <- train(recentDxJ_dxCode ~ ., data=temp, na.action = na.roughfix, method="rpart")
View(temp)

train(formula,
      dataset,
      method = "C5.0",
      na.action = na.pass)


rpartImp <- varImp(rPartMod)
print(rpartImp)
plot(rpartImp, top = 20, main='Variable Importance')
save(rpartImp, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/CART_using_package_rpart")
write.csv(rpartImp$importance, 
          file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/CART_using_package_rpart.csv")


# Train an RRF model and compute variable importance.

rrfMod <- train(recentDxJ_dxCode ~ ., data=temp, na.action = na.roughfix, method="RRF")
rrfImp <- varImp(rrfMod, scale=F)
rrfImp
plot(rrfImp, top = 20, main='Variable Importance')
save(rrfImp, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/RegularizedRandomForest")
write.csv(rrfImp$importance, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/RegularizedRandomForest.csv")
View(rrfImp$importance)

#a loop for doing feature selection using a list of ML methods in caret=================

classifier_vctr <- c ("treebag", "logicBag", "bagFDA", "bartMachine", "brnn", "blackboost", 
   "J48", "C5.0", "enet", "randomGLM", "fda", "gaussprRadial", "gamLoess", 
   "glmnet", "kknn", "svmLinearWeights2","lda",  "mlp", "stepQDA", "qda", "rqlasso")

feature_importance_res <- ftr_imprtnce_caret(classifier_vctr)


#trained_model_list <- list()
i <- 21 #loop index 
#varImp_list <- list()
while (i <= length(classifier_vctr)) 
{
  
  print("===============================")
  print(i)
  print("out of:")
  print(length(classifier_vctr))
  print(classifier_vctr[i])
  #training a new model and saving it
  trained_model_list[[i]] <- 
    train(recentDxJ_dxCode ~ ., data=temp, na.action = na.roughfix, method=classifier_vctr[i])
  #saving the feature importance results
  varImp_list[[i]] <- varImp(trained_model_list[[i]], scale=F)
  i <- i + 1
}

save(varImp_list, 
     file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/varImp_list")
View(varImp_list)


no_of_feat_selection_method <- length(varImp_list)
for (i in c(1:no_of_feat_selection_method)) 
{
  file_name_f_write <- paste("/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/varImp_list", 
                             as.character(i), ".csv", sep = "")
  write.csv(varImp_list[[i]]$importance, file = file_name_f_write)
  
}


#rfe from the caret package

print(head(trainData))
str(trainData)


set.seed(100)
options(warn=-1)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 10,
                   verbose = FALSE)

#size shows the subset size of the features that should be examined as a subset
lmProfile <- rfe(x=trainData[, -53], y=trainData$recentDxJ_dxCode, sizes = c(4:25), 
                 rfeControl = ctrl)

lmProfile$optVariables
write.csv(lmProfile$optVariables, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/rfe_optVars.csv")


#lasso on my data ================

library(glmnet)
trainData <- ET_subjects_having_all_five
View(trainData)
#NA imputaion
trainData <- na.roughfix(trainData)
x <- as.matrix(trainData[,-53]) # all X vars
y <- as.double(as.matrix(ifelse(trainData[, 53]=='nonASD', 0, 1))) # Only Class
# Fit the LASSO model (Lasso: Alpha = 1)
set.seed(100)
cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results
plot(cv.lasso)



#The best lambda value is stored inside 'cv.lasso$lambda.min'.
# plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)

# See all contributing variables
#A high positive or low negative implies more important is that variable.
lasso_ET_res <- df_coef[df_coef[, 1] != 0, ]
View(lasso_ET_res)
save(lasso_ET_res, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/lasso_ET_res")

write.csv(lasso_ET_res,
          file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/lasso_ET_res.csv")



#Ensemble feature selection=============
feature_scores_total <- 
  read.csv("/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/FeatureSelectionResults4ProcessingWithR.csv")
#all columns with odds indices are feature names and other columns are scores 
dim(feature_scores_total)
View(feature_scores_total)

#removing leading and tailing whitespaces 
feature_scores_total <- 
  apply(feature_scores_total, 2, trimws)

#extracting the feature values
feature_scores_withoutSchemeNames <- extract_only_feature_scores(feature_scores_matrix = feature_scores_total, 
                            no_of_ftr = 52, ftr_names_vctr = colnames(trainData)[-53] )

feature_scores_proccessed <- apply(feature_scores_withoutSchemeNames, 2, ftr_score_scaling)

write.csv(feature_scores_proccessed,
          file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/feature_scores_proccessed.csv")

feature_scores_proccessed <- 
  read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/feature_scores_proccessed.csv")
rownames(feature_scores_proccessed) <- feature_scores_proccessed[,1]
feature_scores_proccessed <- feature_scores_proccessed[,-1]
View(feature_scores_proccessed)


No_of_feature_selection_methods<- 28
final_fature_score <- rowSums(feature_scores_proccessed)
final_fature_score <- sort(final_fature_score, decreasing = TRUE)
percent_from_max_possible_score <- final_fature_score / (No_of_feature_selection_methods * 100)
View(cbind(final_fature_score, percent_from_max_possible_score))
write.csv(cbind(final_fature_score, percent_from_max_possible_score), 
          file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/FinalEnsembleFeatureSelection2.csv")


#viz on the feature_scores_proccessed

data_frame_4_ensemble_ftr_selection <- matrix(nrow = 1, ncol = 2)
colnames(data_frame_4_ensemble_ftr_selection) <- c("Feature","Importance_Score")
#View(cbind(rownames(feature_scores_proccessed), feature_scores_proccessed[,1]))
#View(feature_scores_proccessed)


#reshaping the feature selection for violin plot 
for(i in c(1:No_of_feature_selection_methods))
{
  new_ftr_selection_score <- cbind(rownames(feature_scores_proccessed), feature_scores_proccessed[,i])
  data_frame_4_ensemble_ftr_selection <- rbind(data_frame_4_ensemble_ftr_selection, new_ftr_selection_score)
}

View(data_frame_4_ensemble_ftr_selection)
View(feature_scores_proccessed)
#removing the first row (NA)
data_frame_4_ensemble_ftr_selection <- data_frame_4_ensemble_ftr_selection[-1,]
data_frame_4_ensemble_ftr_selection <- as.data.frame(data_frame_4_ensemble_ftr_selection)
colnames(data_frame_4_ensemble_ftr_selection) <- c("Feature","Importance_Score")
library(ggplot2)
pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/FeatureSelectionBasedonAllFiveTest/boxplot.pdf",   # The directory you want to save the file in
    width = 40, # The width of the plot in inches
    height = 40) # The height of the plot in inches
p <- ggplot(data = data_frame_4_ensemble_ftr_selection, aes(x = Feature, y = Importance_Score, fill = Feature))
data_frame_4_ensemble_ftr_selection$Importance_Score <- as.numeric(data_frame_4_ensemble_ftr_selection$Importance_Score)
p + geom_boxplot()
dev.off()

#extract_only_feature_scores========
#this fucntion tries to get a matrix which contains the results of several feature selection methods 
#(all columns with odds indices are feature names and other columns are scores), and return a mtrix that contains
#the scores only
#INPUT: feature_scores_matrixL: a matrix of severla feature selection schemes; 
#no_of_ftr: number of features in the original dataset that was used in feature selection algorithms
#ftr_names_vctr: a vector of the feature names that can be used for 
extract_only_feature_scores <- function(feature_scores_matrix, no_of_ftr, ftr_names_vctr)
{
  #no_of_ftr <- dim(feature_scores_matrix)[1]
  ftr_names <- ftr_names_vctr #colnames(trainData)[-53]
  #the below matrix would be the final matrix
  ftr_scoring_matrix <- matrix(nrow = no_of_ftr, ncol = 1)
  rownames(ftr_scoring_matrix) <- ftr_names
  
  col_no <- dim(feature_scores_matrix)[2]
  #i is the counter for columns of the input feature selection schemes (feature_scores_matrix)
  i <- 1
  #j is the counter for columns of the final matrix (ftr_scoring_matrix)
  j <- 1
  while (i < col_no ) 
  {
    #browser()
    #i and i+1 is the index of one feature scoreing scheme, i and i+1 shows names and scores, respectively. 
    #extract scores from the next feature scoring scheme and save it in the matrix
    for (ftr in rownames(ftr_scoring_matrix)) 
    {
      #browser()
      #if there are no scoring for this feature then it should be zero
      score_4_this_ftr <- 0
      if (ftr %in% feature_scores_matrix[,i]) 
      {
        #index of the current feature (ftr) in the current feature scoring scheme (feature_scores_matrix[,i])
        ftr_indx <- which(feature_scores_matrix[,i] == ftr)
        score_4_this_ftr <- feature_scores_matrix[ftr_indx, i+1]
      }
      #save the score in the final matrix
      #negative values usually means important features
      ftr_scoring_matrix[ftr, j] <- abs(as.numeric(score_4_this_ftr))
    }
    #preparing the counter for the next feature 
    i <- i + 2
    #adding a new col to the final matrix 
    ftr_scoring_matrix <- cbind(ftr_scoring_matrix, rep(NA, no_of_ftr))
    ##preparing the index for the next feature in the final matrix
    j <- j + 1
  }
  #removing the last col of the matrix (it is NA)
  ftr_scoring_matrix <- ftr_scoring_matrix[,-j]
  return(ftr_scoring_matrix)

}


#=================Function for running a lot of feature importance based on different classifiers
#INPUT: this function gives a list of classifiers as the input and return the important features based on those calssifier 
#as methods in "train" function from caret package and finally returen a list of two lists:
#list of trained models, and list of feature importance
ftr_imprtnce_caret <- function(classifier_vctr, data)
{
  trained_model_list <- list()
  i <- 0 #loop index 
  varImp_list <- list()
  for(classifier in classifier_vctr)
  {
    i <- i + 1
    print("===============================")
    print(i)
    print("out of:")
    print(length(classifier_vctr))
    print(classifier)
    #training a new model and saving it
    trained_model_list[[i]] <- 
      train(target_var ~ ., data = trainData, na.action = na.roughfix, method=classifier)
    #saving the feature importance results
    varImp_list[[i]] <- varImp(trained_model_list[[i]], scale=F)

  }
  return(list(TraindeModelsLis = trained_model_list, FeatureImportanceResultsList = varImp_list))
}







#feature score scaling===========
#this function tries to scale the input vector in such a way that maximum score be 100
ftr_score_scaling <- function(ftr_scor_vctr)
{
  max_score <- max(ftr_scor_vctr)
  scaling_factor <- 100/max_score
  ftr_scor_vctr_scaled <- ftr_scor_vctr * scaling_factor
  return(ftr_scor_vctr_scaled)
}



#excep





