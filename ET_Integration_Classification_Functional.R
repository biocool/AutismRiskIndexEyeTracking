
#in this script I'll try to do classification mainly on the ET data using caret package 
writing_path_data <-  
  "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/09-10-2021-Update on Missing Fixations for Tobii Project 1-3/FiveET_Test/ProcessedUsingR/ReadyForR/All_ET_Combination/4Grant/"
writing_path_figures <- 
  "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/"

library(caret)
library(skimr)
library(AppliedPredictiveModeling)
library(corrplot)
library(ComplexHeatmap)
library(heatmaply)
library(tidyr)
library(EnvStats) 

#=====reading the data ========
#this function gives a full path for a csv file and read it and after some basic preprocessing returns the resulting data.frame
#INPUT: file.path: is the full path for the csv ET file
#the class col in the file MUST has this name "recentDxJ_dxCode"
#OUTPUT: the data frame that all features were converted to numeric and the class was converted to factor and also each row has name
read_ET_data <- function(file.path)
{
  #browser()
  #we assume that the first col in the csv is sbjID and so the first col after reading will be the first ET feature 
  ET.data <- 
    read.csv(file.path, 
             row.names = 1)
  #converting features to numeric
  class.index <- dim(ET.data)[2]
  ET.data[,-class.index] <- apply(ET.data[,-class.index],2,as.numeric)
  #converting
  ET.data$recentDxJ_dxCode <- 
    as.factor(ET.data$recentDxJ_dxCode)
  return(ET.data)
}

full.path <- "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/09-10-2021-Update on Missing Fixations for Tobii Project 1-3/FiveET_Test/ProcessedUsingR/ReadyForR/All_ET_Combination/12345_weka.csv"

ET.data <- read_ET_data(file.path = full.path)



#=======driverFunction========
driverFunction <- function(file_path, writing_path_data, writing_path_figures, strTag)
{
  #!!!!!#!#!#!#
  strTag <- "AllFive"
  ET.data <- read_ET_data(full.path)#OK!
  
  preprocessing.res.list <- do.preprocessing(ET.data = ET.data, 
                                             writing_path_data = writing_path_data, 
                                             writing_path_figures = writing_path_figures, strTag=strTag)#OK!!!!
  #the below element is ready for classification
  ET.data.processed <- preprocessing.res.list[[1]]
  #the below element is learned.imputation.model that will be used to do the imputation on the unseen data
  imputation.model <- preprocessing.res.list[[2]]
  #the below element is ET.data.imputed that will be used to do extract min and max for normalizing unseen data
  ET.data.imputed <- preprocessing.res.list[[3]]
  save(preprocessing.res.list, file = paste(writing_path_data, strTag, ".preprocessing.res.list.R.List", sep = ""))
  write.csv(ET.data.imputed, file = paste(writing_path_data, strTag, "ET.data.imputed.Normalized.csv", sep = ""))

  #NOTE #NOTE: for grant purpose I didn't use very stringent method: 
  #I used all for normalization and imputation (I did the data spliting after preprocessing)
  dataSplitRes <- 
    do.dataSplit(ET.data = ET.data.processed)
  #View(dataSplitRes)
  trainData <- 
    dataSplitRes[[1]]
  validationData <- 
    dataSplitRes[[2]]
  testData <- 
    dataSplitRes[[3]]
  #!!!!=====
  #using all data as train
  do.preparing.4.classification.res <- do.preparing.4.classification(no.Of.Classifier = 50)
  my.trainControl <- do.preparing.4.classification.res[[1]]
  selected.Classifiers.to.be.trained <- do.preparing.4.classification.res[[2]]
  View(selected.Classifiers.to.be.trained)
  #removing xgbDART
  selected.Classifiers.to.be.trained <- selected.Classifiers.to.be.trained[-9]
  selected.Classifiers.to.be.trained <- selected.Classifiers.to.be.trained[-6]
  ORFlog
  #HERE
  trained.classifiers.list2 <- do.classification(ET.data.train = ET.data.processed, my.trainControl = my.trainControl, 
                                                  selected.Classifiers.to.be.trained = selected.Classifiers.to.be.trained[c(1:10)],
                                                  metric = "oneSE", selectionFunction = "ROC")
  
  trained.classifiers.list3 <- do.classification(ET.data.train = ET.data.processed, my.trainControl = my.trainControl, 
                                                 selected.Classifiers.to.be.trained = selected.Classifiers.to.be.trained[c(11:20)],
                                                 metric = "oneSE", selectionFunction = "Spec")
  
  trained.classifiers.list4 <- do.classification(ET.data.train = ET.data.processed, my.trainControl = my.trainControl, 
                                                 selected.Classifiers.to.be.trained = selected.Classifiers.to.be.trained[c(21:30)],
                                                 metric = "oneSE", selectionFunction = "Spec")
  
  vctr.to.be.subsampled <- selected.Classifiers.to.be.trained[c(31:40)]
  
  trained.classifiers.list5 <- do.classification(ET.data.train = ET.data.processed, my.trainControl = my.trainControl, 
                                                 selected.Classifiers.to.be.trained = selected.Classifiers.to.be.trained[c(31:40)],
                                                 metric = "oneSE", selectionFunction = "Spec")
  vctr.to.be.subsampled <- 
    selected.Classifiers.to.be.trained[c(41:51)]
  vctr.to.be.subsampled <- 
    vctr.to.be.subsampled[c(2,4,6,8,9,10,11)]
  trained.classifiers.list6 <- 
    do.classification(ET.data.train = ET.data.processed, my.trainControl = my.trainControl, 
                                                 selected.Classifiers.to.be.trained = vctr.to.be.subsampled,
                                                 metric = "oneSE", selectionFunction = "Spec")
  
  trained.classifiers.list2[[1]] <- trained.classifiers.list2[[1]]
  trained.classifiers.list2[[2]] <- trained.classifiers.list2[[2]]
  trained.classifiers.list2[[3]] <- trained.classifiers.list2[[3]]
  
  trained.classifiers.list2[[4]] <- trained.classifiers.list3[[1]]
  trained.classifiers.list2[[5]] <- trained.classifiers.list3[[2]]
  
  trained.classifiers.list2[[6]] <- trained.classifiers.list4[[1]]
  trained.classifiers.list2[[7]] <- trained.classifiers.list4[[2]]
  trained.classifiers.list2[[8]] <- trained.classifiers.list4[[3]]
  trained.classifiers.list2[[9]] <- trained.classifiers.list4[[4]]

  trained.classifiers.list2[[10]] <- trained.classifiers.list6[[1]]
  trained.classifiers.list2[[11]] <- trained.classifiers.list6[[2]]
  trained.classifiers.list2[[12]] <- trained.classifiers.list6[[3]]
  
  
  names(trained.classifiers.list2)[c(1:3)]<- names(trained.classifiers.list2)
  names(trained.classifiers.list2)[c(4,5)]<- names(trained.classifiers.list3)
  names(trained.classifiers.list2)[c(6:9)]<- names(trained.classifiers.list4)
  names(trained.classifiers.list2)[c(10:12)]<- names(trained.classifiers.list6)
  trained.classifiers.list.final2 <- trained.classifiers.list2
  
  
  
  #dev.off()
  do.feature.importance.analysis(set.of.trained.classifiers = trained.classifiers.list.final2, trainData =  trainData, strTag = "AllFive2", 
                                 writing_path_data = writing_path_data, writing_path_figures = writing_path_figures)
 save(trained.classifiers.list.final2,file = paste(writing_path_data, strTag, ".LearnedModels.R.List2") )
}

#=====do.preprocessing===========
do.preprocessing <- function(ET.data, writing_path_data, writing_path_figures, strTag )
{
  print("data_summary_ET")
  skimmed.ET.data <- data_summary_ET(data = ET.data, writing_path_data = writing_path_data, strTag = strTag) #OK!
  print("remove.ZeroVarFeatures")
  ET.data <- remove.ZeroVarFeatures(ET.data = ET.data)#OK!
  print("do.HighNA.features.elimination")
  ET.data <- do.HighNA.features.elimination(ET.data)#OK!
  print("do.imputation")
  imputation.res <- do.imputation(ET.data)#OK!
  ET.data.imputed <- imputation.res[[1]]
  imputation.model <- imputation.res[[2]]
  #OK!
  print("do.normalization")
  normalization.res <- do.normalization(ET.data.imputed) # Standardize: method = c("center", "scale") or #0-1 Normalize:  method="range" (default)
  ET.data.Normalized <- normalization.res[[1]]
  normalization.model <- normalization.res[[2]]
  print("do.feature.plotting")
  #Here
  do.feature.plotting(ET.data.imputed,  writing_path_figures, strTag = strTag)#OK!
  do.feature.plotting(ET.data.Normalized,  writing_path_figures, strTag = paste(strTag,"Normalized", sep = ""))#OK!
  print("do.corr.plotting")
  do.corr.plotting(ET.data.imputed, writing_path_figures, strTag = strTag )#OK
  do.corr.plotting(ET.data.Normalized, writing_path_figures, strTag = paste(strTag,"Normalized", sep = ""))#OK
  print("outlire.detection.and.removal")
  ##NOTE: "outlire.detection.and.removal" function assumes that there is a column named "recentDxJ_dxCode" with two values: "nonASD", and "ASD"
  ET.ready.4.classificatoin <- outlire.detection.and.removal(ET.data.Normalized, writing_path_data, writing_path_figures, strTag = "sag" )#OK
  
  preProcessingRes <- list(ET.Data.ready.4.classificatoin = ET.ready.4.classificatoin, 
                           learned.imputation.model = imputation.model, ET.data.imputed.4.having.details.2.normalizing.unseen.data = ET.data.imputed)
  return(preProcessingRes)
  
}

#========do.HighNA.features.elimination========
#in this function I tried to eliminate all features that have NA percent > thrsh
do.HighNA.features.elimination <- function(ET.data, thrsh = 0.1)
{
  #the last col is the lable
  no.of.instances <- (dim(ET.data)[1]) 
  ET.data.is.na <- apply(ET.data, 2, is.na)
  no.of.NA.in.features <- colSums(ET.data.is.na)
  na.percent <- no.of.NA.in.features / no.of.instances
  #flagging the col that should be removed
  to.be.removed.features.indices <- (na.percent > thrsh)
  ET.data.eliminated.highlyNA.features <- ET.data[,!to.be.removed.features.indices]
  no.of.removed.features <- sum(to.be.removed.features.indices)
  print("no.of.removed.features:" )
  print(no.of.removed.features)
  return(ET.data.eliminated.highlyNA.features)
}

#do.imputation===========
#This function does the imputation and returens the imputation model for applying on the unseen data
#INPUT: ET.data: an ET dataset that should be imputed;  impute.method: method that will pass to the "preProcess" function from caret package 
#OUTPUT: a list 
do.imputation <- function(ET.data, impute.method = "bagImpute")
{
  library(caret)
  set.seed(100)
  
  ET.data.imputed <- ET.data
  class.index <- dim(ET.data)[2]
  print("learning impute model")
  imput_by_bagedTree_model <- preProcess(ET.data[,-class.index], method = impute.method)
  print("Applying learned impute model")
  ET.data.imputed[,-class.index] <- predict(imput_by_bagedTree_model, ET.data[,-class.index])
  #View(ET.data.imputed)
  
  res.list <- list(ET.data.imputed = ET.data.imputed,
                   imputationModel = imput_by_bagedTree_model)
  return(res.list)
 
}

#do.feature.plotting=============
#this function does the feature plottig using three different ways
do.feature.plotting <- function(ET.data,  writing_path_figures, strTag)
{
  #View(all_five_test)
  class.index <- dim(ET.data)[2]
  full.path.4.fig <- paste(writing_path_figures, strTag, ".FeaturePlot.Pairs.pdf", sep = "")
  pdf(file = full.path.4.fig, width = 80, height = 80 )
  
  x <- featurePlot(x = ET.data[,-class.index],
              y = ET.data$recentDxJ_dxCode,
              plot = "pairs",
              auto.key = list(columns = 2)
  )
  plot(x)
  dev.off()
  print("pairs feature plotting has been done!")
  #plot = "box" # scales = list(y = list(relation="free"), x = list(rot = 90)),
  full.path.4.fig <- paste(writing_path_figures, strTag, "FeaturePlot.box.pdf", sep = "")
  pdf(file = full.path.4.fig, width = 40, height = 40 )
  
  x <- featurePlot(x = ET.data[,-class.index],
              y = ET.data[,class.index],
              plot = "box", scales = list(y = list(relation="free"), x = list(rot = 90)),
              auto.key = list(columns = 2)
  )
  plot(x)
  dev.off()
  print("box feature plotting has been done!")
  
  full.path.4.fig <- paste(writing_path_figures, strTag, "FeaturePlot.density.pdf", sep = "")
  pdf(file = full.path.4.fig, width = 40, height = 40 )
  
  x <- featurePlot(x = ET.data[,-class.index],
              y = ET.data[,class.index],
              plot = "density",
              auto.key = list(columns = 2)
  )
  plot(x)
  dev.off()
  print("density feature plotting has been done!")
  message("Plotting done!")
}


#outlire.detection.and.removal===============
#NOTE: this function assumes that there is a column named "recentDxJ_dxCode" with two values: "nonASD", and "ASD"
outlire.detection.and.removal <- function(ET.data, writing_path_data, writing_path_figures, strTag )
{
  print("Doing Outlier removal using Cook distance")
  #for using cooks distance outlier removal, it is needed to have a fitted linear regression model.
  #so first we should convert the classes to numeric classes and then fit an lm model
  data_4_lm_fitting <- ET.data
  data_4_lm_fitting$recentDxJ_dxCode <-  as.character(data_4_lm_fitting$recentDxJ_dxCode)
  data_4_lm_fitting$recentDxJ_dxCode[data_4_lm_fitting$recentDxJ_dxCode == "nonASD"] <- 0
  data_4_lm_fitting$recentDxJ_dxCode[data_4_lm_fitting$recentDxJ_dxCode == "ASD"] <- 1  
  print("Fitting linear model")
  lm_fitted_model <- 
    lm(recentDxJ_dxCode ~. , data = data_4_lm_fitting)
  #coock distance is a multivariate method=======
  print("computing  cooks.distance ")
  cooksd <- cooks.distance(lm_fitted_model)
  influential <- cooksd[(cooksd > (3 * mean(cooksd, na.rm = TRUE)))]

  influential_instances <- names(influential)
  not_influential_instances <- !(rownames(ET.data) %in% influential_instances)
  ET.data <- ET.data[not_influential_instances,]
  message(paste("The number of removed outliers based on coocks distance: ", 
                as.character(length(influential_instances))))
  
  #writing the outlier removal to H.D.D
  file.name.4.writing <- paste(writing_path_data, strTag, "Influential.based.on.coocks.dist.csv", sep = "")
  write.csv(influential, file = file.name.4.writing)
  
  full.path.4.fig <- paste(writing_path_figures, strTag, "Influential.based.on.coocks.dist.pdf", sep = "")
  print("Plotting cooks distance results")
  pdf(full.path.4.fig)
  plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") # plot cook's distance
  abline(h = 3*mean(cooksd, na.rm=T), col="red") # add cutoff line
  text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd > 3*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
  dev.off()
  
  #saving 4 panel figure as a summarizer of the fitted lm model
  print("saving 4 panel figure as a summarizer of the fitted lm model")
  full.path.4.fig <- paste(writing_path_figures, strTag, "lmFitted.dist.pdf", sep = "")
  pdf(full.path.4.fig)
  par(mfrow = c(2, 2))
  plot(lm_fitted_model)
  dev.off()
  
  #Rosner’s test=======
  print("Rosner’s test")
  library(EnvStats) 
  #k is the number of expected outlier 
  #this is univarate method
  
  #-1 : droping the class
  no.of.features <- (dim(ET.data)[2])-1
  no.of.instances <- (dim(ET.data)[1])
  #applying Rosner’s test based on each feature 
  outlier.multi.set <- c()
  for (i in c(1:no.of.features)) 
  {
    test <- rosnerTest(ET.data[,i], k = 10 )
    outlier_index <- test$all.stats$Obs.Num[which(test$all.stats$Outlier == TRUE)]
    new.outlier.instances <- row.names(ET.data)[outlier_index]
    outlier.multi.set <- c(outlier.multi.set, new.outlier.instances)
  }
  
  outlier.fer.based.on.Rosners.test <- as.data.frame(table(outlier.multi.set))
  colnames(outlier.fer.based.on.Rosners.test) <- c("SubjectId","Freq")
  file.name.4.writing <- paste(writing_path_data, strTag, ".OutlierRosners.test.csv", sep = "")
  
  
  #If an instance be outlier in more than 10 percent of the features it will be considered as outlier
  thrsh.4.considernig.as.outLier <- round(0.10*(no.of.features))
  index4outliers<- outlier.fer.based.on.Rosners.test$Freq>= thrsh.4.considernig.as.outLier
  outLierSubjIDs.Rosners.test <-outlier.fer.based.on.Rosners.test$SubjectId[index4outliers]
  not.OutlierInstances <- !(rownames(ET.data) %in% outLierSubjIDs.Rosners.test)
  write.csv(outLierSubjIDs.Rosners.test, file = file.name.4.writing)
  ET.data <- ET.data[not.OutlierInstances,]
  #ET.data <- ET.data[not.OutlierInstances,]
  message(paste("The number of removed outliers based on Rosner’s test: ", 
                as.character(length(outLierSubjIDs.Rosners.test))))
  
  #Hampel filter======
  print("Hampel filter")
  outlier.multi.set <- c()
  for (i in c(1:no.of.features)) 
  {
    lower_bound <- median(ET.data[,i]) - 3 * mad(ET.data[,i], constant = 1)
    upper_bound <- median(ET.data[,i]) + 3 * mad(ET.data[,i], constant = 1)
    outlier_ind <- which(ET.data[,i] < lower_bound | ET.data[,i] > upper_bound)
    outLierSubjects <- rownames(ET.data)[outlier_ind]
    outlier.multi.set <- c(outlier.multi.set, outLierSubjects)
  }
  outlier.based.on.Hampel.filter <- as.data.frame(table(outlier.multi.set))
  colnames(outlier.based.on.Hampel.filter) <- c("SubjectId","Freq")
  #View(outlier.based.on.Hampel.filter)
  file.name.4.writing <- paste(writing_path_data, strTag, ".OutlierHampel.Filter.csv", sep = "")
  #If an instance be outlier in more than 30 percent of the features it will be considered as outlier
  thrsh.4.considernig.as.outLier <- round(0.30*(no.of.features))
  index4outliers<- outlier.based.on.Hampel.filter$Freq>= thrsh.4.considernig.as.outLier
  outLierSubjIDs.Hampel.filter <- outlier.based.on.Hampel.filter$SubjectId[index4outliers]
  not.OutlierInstances <- !(rownames(ET.data) %in% outLierSubjIDs.Hampel.filter)
  ET.data <- ET.data[not.OutlierInstances,]
  write.csv(outLierSubjIDs.Hampel.filter, file = file.name.4.writing)
  message(paste("The number of removed outliers based on Hampel Filter: ", 
                as.character(length(outLierSubjIDs.Hampel.filter))))
  message("Outlier Removal Done!!")
  #===========
  return(ET.data)
}

#remove.ZeroVarFeatures===================
#removing zero Var features 
remove.ZeroVarFeatures <- function(ET.data)
{
  #near zero var analysis
  nzv <- nearZeroVar(ET.data, saveMetrics= TRUE)
  #View(nzv$nzv)
  zero_var_features <- sum(nzv$nzv)
  print("numebr of near zero var features:")
  print(zero_var_features)
  ET.data <- ET.data[,!nzv$nzv]
  return(ET.data)
}

#do.preparing.4.classification=====================
do.preparing.4.classification <- function(no.Of.Classifier)
{
  fitControl <- setting.the.trainControl()
  selceted.classifiers.vctr <- do.selecting.a.diverse.set.of.classifier(no.Of.Classifier)
  res.list <-list(fitControl = fitControl, selceted.classifiers.vctr = selceted.classifiers.vctr)
  return(res.list)
}

#setting.the.trainControl===========
setting.the.trainControl<- function()
{
  #setting the trainControl function parameter: repeated CV; downsampling; 
  set.seed(100)
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10,
    returnData = TRUE,
    classProbs = TRUE,
    returnResamp = "all",
    summaryFunction = twoClassSummary,
    sampling = "down",
    savePredictions = "final"
  )
  return(fitControl)
}


#do.selecting.a.diverse.set.of.classifier==========
#this function selects and returns the #noClassifier out of 190 different classification models that are most diverse regarding to 
#"C5.0Tree" classifier. --> (pls)
do.selecting.a.diverse.set.of.classifier <- function(noClassifier)
{
  all_caret_classifier <- read.csv("/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/Archive/08-23-2021/Modified/Final/AfterPreprocessing/WEKA_Ready/caret/tag_data.csv", row.names = 1)
  all_caret_classifier <- as.matrix(all_caret_classifier)
  #View(all_caret_classifier)
  ## Select only models for regression
  classificationModels <- all_caret_classifier[all_caret_classifier[,"Classification"] == 1,]
  #View(classificationModels)
  all <- 1:nrow(classificationModels)
  
  start <- grep("(pls)", rownames(classificationModels), fixed = TRUE)
  pool <- all[all != start]

  ## Select 4 model models by maximizing the Jaccard
  ## dissimilarity between sets of models
  nextMods <- maxDissim(classificationModels[start,,drop = FALSE], 
                        classificationModels[pool, ], 
                        method = "Jaccard",
                        n = noClassifier)
  
  library(stringr)
  model_names <- rownames(classificationModels)[c(start, nextMods)]
  model_names <- str_replace(model_names, pattern  = "\\(",  replacement = ";")
  model_names <- str_replace(model_names, pattern  = "\\)",  replacement = ";")
  model_names <- str_split_fixed(string = model_names, pattern = ";", n = 2)
  #dim(model_names)
  model_names <- str_replace(string = model_names[,2], pattern = ";", replacement = "")
  #View(model_names)
  return(model_names)
  
}


#do.normalization===========
#This function does the normalization and returens the normalization model for applying on the unseen data
#INPUT: ET.data: an ET dataset that should be normalized;  normalizatio.method: method that will pass to the "preProcess" function from caret package 
do.normalization <- function(ET.data, normalizatio.method = "range")
{
  ET.Normalized <- ET.data
  class.index <- dim(ET.data)[2]
  normalization.model <- preProcess(ET.data[,-class.index], method= normalizatio.method)
  ET.Normalized[,-class.index] <- predict(normalization.model, ET.data[,-class.index])
  res.list <- list(ET.Normalized = ET.Normalized, normalization.model = normalization.model)
  return(res.list)
}


#do.corr.plotting=========
do.corr.plotting <- function(ET.data, writing_path_figures, strTag)
{
  
  class.index <- dim(ET.data)[2]
  data.4.corr.plot <- ET.data[,-class.index]
  data.4.corr.plot.cor.matrix <- cor(data.4.corr.plot)
  data.4.corr.plot.cor.matrix <- as.matrix(data.4.corr.plot.cor.matrix)
  #View(data.4.corr.plot.cor.matrix)
  full.path.4.fig <- paste(writing_path_figures, strTag, "FeaturePlot.CorrPlotMix.pdf", sep = "")
  pdf(file = full.path.4.fig, width = 40, height = 40 )
  corrplot.mixed(data.4.corr.plot.cor.matrix)
  dev.off()
  full.path.4.fig <- paste(writing_path_figures, strTag, "HeatMap.pdf", sep = "")
  pdf(file = full.path.4.fig, width = 40, height = 40 )
  heatmap(data.4.corr.plot.cor.matrix)
  dev.off()
  full.path.4.fig <- paste(writing_path_figures, strTag, "FeaturePlot.CorrPlotwithClusters.pdf", sep = "")
  pdf(file = full.path.4.fig, width = 40, height = 40 )
  corrplot( data.4.corr.plot.cor.matrix, method = 'pie', 
            type="upper", diag=FALSE, tl.cex = 0.5, tl.col = "black", tl.srt = 45, outline = TRUE, order = "hclust", hclust.method = "complete")
  
  dev.off()
  message("Correlation Plotting is Done!")
  
}


#data summary========
#this function tries to summarize the data using the skimr package 
data_summary_ET <- function(data, writing_path_data,  strTag = NA, apply_as_muneric = FALSE)
{
  library(skimr)
  #data_summary_ET(ET.data, writing_path_data, strTag)
  if (apply_as_muneric) 
  {
    data <- apply(data, 2, as.numeric)
  }
  #browser()
  skimmed <- skim_to_wide(data)
  wrtinig_path_fullName <- paste(writing_path_data, strTag, "_datasammary.csv",sep = "" )
  write.csv(skimmed, wrtinig_path_fullName)
  return(skimmed)
}




#do.dataSplit========
#this functoin returns a lsit of three data
#discovery.dataSet (80% of the original dataset)
#trainDataSet (80% of the discovery dataset)
#validation.dataSet (20% of the discovery dataset)
#test.data (20% of the original dataset)
#Finally, the three last datasets would be returened
do.dataSplit <- function(ET.data)
{
  set.seed(100)
  discoveryIndex <- 
    createDataPartition(y = ET.data$recentDxJ_dxCode, p = 0.8, list = FALSE, times = 1)
  discovery.dataSet <- ET.data[discoveryIndex,]
  test.data <- ET.data[-discoveryIndex,]
  set.seed(200)
  trainIndex <- 
    createDataPartition(y = discovery.dataSet$recentDxJ_dxCode, p = 0.8, list = FALSE, times = 1)
  trainDataSet <- discovery.dataSet[trainIndex,]
  validation.dataSet <- discovery.dataSet[-trainIndex,]
  dim(discovery.dataSet)
  dim(trainDataSet)
  dim(validation.dataSet)
  dim(test.data)
  #View(discovery.dataSet)
  data.split.res.list <- list(train = trainDataSet, 
                              validation = validation.dataSet, 
                              test = test.data)
  #intersect(rownames(validation.dataSet), rownames(trainDataSet))
  #intersect(rownames(validation.dataSet), rownames(test.data))
  #intersect(rownames(trainDataSet), rownames(test.data))
  return(data.split.res.list)
}


#===do.classification========
#class feature must be "recentDxJ_dxCode"
#INPUT: 
#"ET.data.train" the ET data frame 
#"my.trainControl" a train.control object that was created using function  trainControl
#"selected.Classifiers.to.be.trained" a vector of model names in caret package 
#"metric" a metric that will be used a an objective function for selecting the best classifier (to be optimized performance measure)
#"selectionFunction" a method that will be used in the "train" functoin for selecting the best classifier
#OUTPUT: "trained_model_lits" a list of trained classifier using the data and provided measures  
do.classification <- function(ET.data.train, my.trainControl, 
                              selected.Classifiers.to.be.trained, metric = "Spec",
                            selectionFunction = "oneSE")
{

  trained_model_lits <- list()
  i <- 1
  for(model in selected.Classifiers.to.be.trained)
  {
    #browser()
    tryCatch(
      expr = 
        { i <- i +  1
          print(paste("Model:", model))
          print(i)
          set.seed(100)
          trained_model_lits[[model]] <- train(recentDxJ_dxCode ~ ., data = ET.data.train, 
                                               method = model, 
                                               trControl = my.trainControl, 
                                               verbose = FALSE,
                                               selectionFunction = selectionFunction,
                                               metric = metric)
          print("Done with:")
          print(model)
          #model_index <- model_index + 1
        },
      error = function(e)
      {
        #message("sag")
        print("Got an error with:")
        print(model)
      }
    )
  }
  return(trained_model_lits)
}    
  

#a2c5t
#y4w9w



#do.feature.importance.analysis========
#this function extract the "feature importance" scores form a list of trained models
#"trainData" will be used only for ROC based feature importance analysis
do.feature.importance.analysis <- function(set.of.trained.classifiers, strTag, trainData , writing_path_data, writing_path_figures)
{
  print("Feature Importance Analysis:")
  #computing varable importance  
  print("computing varable importance")
  varImp.list <- list()
  for (trainedModel in set.of.trained.classifiers) 
  {
    #this variable "trainedModel$modelInfo$label" gives the trained model name
    # varImp(trainedModel) extracts the "feature importance" scores
    varImp.list[[trainedModel$modelInfo$label]] <- varImp(trainedModel)
  }
  writing_path_data <- paste(writing_path_data, strTag, ".featureImportance.R.list")
  #saving to the H.D.D
  save(varImp.list, file = writing_path_data)
  
  #computing one another feature importance based on ROC
  roc_imp <- filterVarImp(x = trainData[,-ncol(trainData)], y = trainData[,ncol(trainData)])
  normalization.model.4.feature.importance <- preProcess(roc_imp, method= "range")
  roc_imp.normalized <- predict(normalization.model.4.feature.importance, roc_imp)

  #making a dataframe from all "feature importance" scores=======
  #initializing with ROC importance 
  feature.scores.data.frame <- as.data.frame(cbind(featureNames = rownames(roc_imp.normalized), ROC = roc_imp.normalized$ASD))
  rownames(feature.scores.data.frame) <- feature.scores.data.frame$featureNames
  #View(feature.scores.data.frame
  feature.scores.data.frame <- feature.scores.data.frame[sort(feature.scores.data.frame$featureNames),]
  #beacuse ROC score of 0.5 means random! finally, I decided to do the normalization on ROC scores. 
  #View(feature.scores.data.frame)
  colnames.4.feature.scores.data.frame <- colnames(feature.scores.data.frame)
  #the below counter will be used for extracting the names of the learned models
  loop.index <- 1 
  model.var.imortant.Names <- names(varImp.list)
  print("Extracting Feature Importance")
  for (featureImportance in varImp.list) 
  {
    #adding feature name as a new column
    featureImportance.to.be.sorted.based.onfeature.name <- 
      cbind(featureNames = rownames(featureImportance$importance), featureImportance$importance)
    #sorting based on feature name to have all the feature importance score in the same order
    #View(featureImportance.sorted.based.onfeature.name)
    featureImportance.sorted.based.onfeature.name <- 
      featureImportance.to.be.sorted.based.onfeature.name[sort(featureImportance.to.be.sorted.based.onfeature.name$featureNames),]
    #second colum is the overall feature importance 
    #We don't need to normalize the importance score because automatically they are normalized in [0,100]
    #we just need to devide the scores by 100 to be scaled in [0,1]
    #the ROC importance score is autamically normalized in [0.1]
    final.featureImportance <- featureImportance.sorted.based.onfeature.name[,2]/100#scaling the scores to be in [0,1]
    feature.scores.data.frame <- cbind(feature.scores.data.frame, final.featureImportance)
    #"featureImportance$model" is the model's name
    #i'm using this for col name of the final data frame to have a meaningful naming
    colnames.4.feature.scores.data.frame <- c(colnames.4.feature.scores.data.frame, model.var.imortant.Names[loop.index])
    loop.index <- loop.index + 1
  }
  
  colnames(feature.scores.data.frame) <- 
    colnames.4.feature.scores.data.frame
  no.of.feature.importance.methods <- length(colnames.4.feature.scores.data.frame)
  #View(feature.scores.data.frame)
  print("Writing Results to the H.D.D")
  write.csv(feature.scores.data.frame, file = paste(writing_path_data, strTag, ".FeatureImportance.csv", sep = ""))
  #normalizing the final summed scores=============
  #the first col is the feature names
  feature.scores.data.frame[,-1] <- apply(feature.scores.data.frame[,-1], 2, as.numeric)
  #View(feature.scores.data.frame)
  feature.scores.data.frame.row.sum <- rowSums(feature.scores.data.frame[,-1])
  feature.scores.data.frame.row.SD <- apply(feature.scores.data.frame[,-1], 1, sd)
  #(ncol(feature.scores.data.frame) - 1) is the number of var.importance methods
  final.feature.importance.normalized.by.No.of.featureimportance.methods <- 
    feature.scores.data.frame.row.sum / (ncol(feature.scores.data.frame) - 1)
  #only two didigits after point
  final.feature.importance.normalized.by.No.of.featureimportance.methods <- 
    signif(final.feature.importance.normalized.by.No.of.featureimportance.methods, digits = 1)
  
  #View(final.feature.importance.normalized.by.No.of.featureimportance.nethods)
  #converting to data frame for ggplot
  final.feature.importance.normalized.by.No.of.featureimportance.methods.data.frame <- 
    as.data.frame(cbind(featureName = (names(final.feature.importance.normalized.by.No.of.featureimportance.methods)), ImportanceScore =final.feature.importance.normalized.by.No.of.featureimportance.methods))
  
  final.feature.importance.normalized.by.No.of.featureimportance.methods.sorted.data.frame <-  final.feature.importance.normalized.by.No.of.featureimportance.methods.data.frame
  
  final.feature.importance.normalized.by.No.of.featureimportance.methods.sorted.data.frame$featureName <- 
    factor(final.feature.importance.normalized.by.No.of.featureimportance.methods.sorted.data.frame$featureName, 
           levels = final.feature.importance.normalized.by.No.of.featureimportance.methods.sorted.data.frame$featureName[order(final.feature.importance.normalized.by.No.of.featureimportance.methods.sorted.data.frame$ImportanceScore)])
  
  write.csv(final.feature.importance.normalized.by.No.of.featureimportance.methods.sorted.data.frame, file = paste(writing_path_data, strTag, ".FeatureImportanceSummary.csv", sep = ""))
  
  
  #visualization based on feature importance ===========================
  print("Ploting feature importance")
  no.of.features <- length(final.feature.importance.normalized.by.No.of.featureimportance.methods)

  #par(mar=c(10,3,1,1))
  pdf(paste(writing_path_figures, strTag, ".FeatureImportancePlot.pdf",  sep = ""), width = 15, height = 10)
  
  p2 <- ggplot(final.feature.importance.normalized.by.No.of.featureimportance.methods.data.frame, 
               aes(x = featureName, y = ImportanceScore))
  
  p2 + geom_bar(stat="identity", width=0.5, fill=rainbow(no.of.features), alpha = 0.6) + 
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle=45, hjust=1)) + ggtitle(paste(strTag, "Feature Importance (Features Are Sorted by ET Test)" ,"; No. of Ftr. Importance Methods:" , as.character(no.of.feature.importance.methods)))
  
  #plotting based on sorted features 
  p1 <- ggplot(final.feature.importance.normalized.by.No.of.featureimportance.methods.sorted.data.frame, 
               aes(x = reorder(featureName, ImportanceScore), y = ImportanceScore))
  
  p1 + geom_bar(stat="identity", width=0.5,  alpha = 0.6) +
    theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1)) +
    ggtitle(paste(strTag, "Feature Importance (Features Are Sorted by Importance Score)","; No. of Ftr. Importance Methods:" , as.character(no.of.feature.importance.methods))) + xlab("Feature Name")
  dev.off()
}



inspecting.the.results <- function(list.of.trained.classifiers, writing_path_data)
{
  
  #list.of.trained.classifiers <- trained.classifiers.list.final2
  no.of.predictions <- nrow(list.of.trained.classifiers[[1]]$pred)
  all.classifier.prediction.results.data.frame <- as.data.frame(matrix(nrow = no.of.predictions, ncol = 1))
  colnames(all.classifier.prediction.results.data.frame) <-"Nothing"
  #extracting the predictions 
  loop.index <- 1
  for (trained.model in list.of.trained.classifiers) 
  {
    #extracting themodel predictions
    vctr.4.colnames <- c()
    #sorting based on the rwo index
    model.predictions <- trained.model$pred[order(trained.model$pred$rowIndex),]
    #View(model.predictions)
    
    obs.pred.cols <- c(which(colnames(model.predictions) == "obs"),which(colnames(model.predictions) == "pred"))
    obs.and.pred.labels <- model.predictions[,obs.pred.cols]
    
    ASD.prob.col <- c(which(colnames(model.predictions) == "ASD"))
    ASD.prob <- model.predictions[,ASD.prob.col]
    
    #View(ASD.prob)
    modelName <- names(list.of.trained.classifiers)[loop.index]
    vctr.4.colnames <- c(vctr.4.colnames, paste(modelName, ".obs", sep = ""), paste(modelName, ".pred", sep = ""), paste(modelName, ".ASD.prob", sep = ""))
    main.measures.4.this.model <- cbind(obs.and.pred.labels, ASD.prob)
    
    colnames(main.measures.4.this.model) <- c(paste(modelName, ".obs", sep = ""), paste(modelName, ".pred", sep = ""), paste(modelName, ".ASD.prob", sep = ""))
    #View(main.measures.4.this.model)
    all.classifier.prediction.results.data.frame <- cbind(all.classifier.prediction.results.data.frame, main.measures.4.this.model)
    #View(all.classifier.prediction.results.data.frame)
    loop.index <- loop.index + 1
  }
  
  View(temp)
  all.classifier.prediction.results.data.frame <- all.classifier.prediction.results.data.frame[,-1]
  write.csv(all.classifier.prediction.results.data.frame, file = paste(writing_path_data, strTag,"all.classifier.prediction.results.data.frame.csv" ,sep = ""))
  
  
  View(all.classifier.prediction.results.data.frame)
  
  
  predicted.label.col.index.vctr <- seq(2,ncol(all.classifier.prediction.results.data.frame),3)
  all.classifier.only.prediction.labels <- all.classifier.prediction.results.data.frame[,predicted.label.col.index.vctr]
  View(all.classifier.only.prediction.labels)
  actual.label <- all.classifier.prediction.results.data.frame[,1]
  View(actual.label)
  no.of.predictions <- nrow(all.classifier.only.prediction.labels)
  
  comparing.to.actula.label.data.frame <- as.data.frame(matrix(nrow = no.of.predictions, ncol = 1))
  
  for (i in c(1:ncol(all.classifier.only.prediction.labels))) 
  {
    comparision.res.Boolean <- (actual.label == all.classifier.only.prediction.labels[,i])
    comparing.to.actula.label.data.frame <- cbind(comparing.to.actula.label.data.frame, comparision.res.Boolean)
  }
  
  comparing.to.actula.label.data.frame <- comparing.to.actula.label.data.frame[,-1]
  
  View(comparing.to.actula.label.data.frame)
  comparing.to.actula.label.data.frame <- as.data.frame(comparing.to.actula.label.data.frame)
  comparing.to.actula.label.data.frame[is.na(comparing.to.actula.label.data.frame)] <- 0
  comparing.to.actula.label.data.frame <- apply(comparing.to.actula.label.data.frame, 2, as.numeric)
  ensemble.opinion <- rowSums(comparing.to.actula.label.data.frame)
  View(ensemble.opinion)
  hist(ensemble.opinion)
  sum(ensemble.opinion>=6)/length(ensemble.opinion)
  write.csv(cbind(comparing.to.actula.label.data.frame, Class.Label = as.character(all.classifier.prediction.results.data.frame[,1])), paste(writing_path_data, strTag, "4.Ensemble.Learning.0.1.csv", sep = ""))
  
  View(comparing.to.actula.label.data.frame)
  comparing.to.actula.label.data.frame.cor <- cor(comparing.to.actula.label.data.frame)
  dev.new()
  Heatmap(comparing.to.actula.label.data.frame.cor)
  
  
  all.classifier.only.prediction.labels.backup <- all.classifier.only.prediction.labels
  all.classifier.only.prediction.labels[is.na(all.classifier.only.prediction.labels)] <- 0
  all.classifier.only.prediction.labels[all.classifier.only.prediction.labels == "ASD"] <- 1
  all.classifier.only.prediction.labels[all.classifier.only.prediction.labels == "nonASD"] <- 0
  write.csv(cbind((all.classifier.only.prediction.labels), Class.Label = as.character(all.classifier.prediction.results.data.frame[,1])), paste(writing_path_data, strTag, "4.Ensemble.Learning.PredictedLabel.csv", sep = ""))
  View(all.classifier.only.prediction.labels)
  
  predicted.Prob.col.index.vctr <- seq(3,ncol(all.classifier.prediction.results.data.frame),3)
  all.classifier.only.prediction.Prob <- all.classifier.prediction.results.data.frame[,predicted.Prob.col.index.vctr]
  View(all.classifier.only.prediction.Prob)
  all.classifier.only.prediction.Prob [is.na(all.classifier.only.prediction.Prob)] <- 0.5
  write.csv(cbind((all.classifier.only.prediction.Prob), Class.Label = as.character(all.classifier.prediction.results.data.frame[,1])), paste(writing_path_data, strTag, "4.Ensemble.Learning.all.classifier.only.prediction.Prob.csv", sep = ""))
  
  all.classifier.only.prediction.Prob.and.Actual.Label <- cbind((all.classifier.only.prediction.Prob), Class.Label = as.character(all.classifier.prediction.results.data.frame[,1]))
  
  View(all.classifier.only.prediction.Prob)
  
  #playing with threshold to have a better specificity 
  all.classifier.only.prediction.Prob.4.threshold.selection <- all.classifier.only.prediction.Prob
  View(all.classifier.only.prediction.Prob.4.threshold.selection)
  prob.thrsh <- 0.4
  all.classifier.only.prediction.Prob.4.threshold.selection[all.classifier.only.prediction.Prob.4.threshold.selection>=prob.thrsh] <- 1
  all.classifier.only.prediction.Prob.4.threshold.selection[all.classifier.only.prediction.Prob.4.threshold.selection<prob.thrsh] <- 0
  #View(all.classifier.only.prediction.Prob.4.threshold.selection)
  ensemble.opinion.based.on.threshold <- rowSums(all.classifier.only.prediction.Prob.4.threshold.selection)
  ensemble.opinion.based.on.threshold.with.label <- cbind(ensemble.opinion.based.on.threshold, Class.Label = as.character(all.classifier.prediction.results.data.frame[,1]))
  #View(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision)
  
  pcnt.of.ASD.vote.thrsh <- 11
  ensemble.opinion.based.on.threshold.with.label.4.enemble.decision <- ensemble.opinion.based.on.threshold.with.label
  ensemble.opinion.based.on.threshold.with.label.4.enemble.decision <- as.data.frame(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision)
  ensemble.opinion.based.on.threshold.with.label.4.enemble.decision$ensemble.opinion.based.on.threshold [ensemble.opinion.based.on.threshold.with.label.4.enemble.decision$ensemble.opinion.based.on.threshold>= pcnt.of.ASD.vote.thrsh] <- "ASD"
  ensemble.opinion.based.on.threshold.with.label.4.enemble.decision$ensemble.opinion.based.on.threshold [ensemble.opinion.based.on.threshold.with.label.4.enemble.decision$ensemble.opinion.based.on.threshold< pcnt.of.ASD.vote.thrsh] <- "nonASD"
  View(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision)
  ensemble.opinion.based.on.threshold.with.label.4.enemble.decision.only.ASD <- subset(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision, ensemble.opinion.based.on.threshold.with.label.4.enemble.decision$Class.Label == "ASD")
  ensemble.opinion.based.on.threshold.with.label.4.enemble.decision.only.nonASD <- subset(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision, ensemble.opinion.based.on.threshold.with.label.4.enemble.decision$Class.Label == "nonASD")
  dim(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision.only.ASD)
  dim(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision.only.nonASD)
  #View(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision.only.ASD)
  sum(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision.only.ASD[,1] ==ensemble.opinion.based.on.threshold.with.label.4.enemble.decision.only.ASD[,2])/(nrow(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision.only.ASD))
  sum(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision.only.nonASD[,1] ==ensemble.opinion.based.on.threshold.with.label.4.enemble.decision.only.nonASD[,2])/(nrow(ensemble.opinion.based.on.threshold.with.label.4.enemble.decision.only.nonASD))
  
  
  
  #check instance based classification ===============
  
  
  subjectID.4.pred.data.frame <- (rownames(list.of.trained.classifiers[[1]]$trainingData)[sort(list.of.trained.classifiers[[1]]$pred$rowIndex)])
  all.classifier.only.prediction.Prob.4.threshold.selection$subjectID <- subjectID.4.pred.data.frame
  all.classifier.only.prediction.Prob.4.threshold.selection.with.subjectID <- all.classifier.only.prediction.Prob.4.threshold.selection
  
  View(all.classifier.only.prediction.Prob.4.threshold.selection.with.subjectID)
  
  all.classifier.only.prediction.Prob.4.threshold.selection.with.subjectID$Class.Label <- as.character(all.classifier.prediction.results.data.frame[,1])
  write.csv(all.classifier.only.prediction.Prob.4.threshold.selection.with.subjectID, 
            file = paste(writing_path_data, strTag, "all.classifier.only.prediction.Prob.4.threshold.selection.with.subjectID.csv", sep = ""))
  dev.new()
  
  l <- ncol(all.classifier.only.prediction.Prob.4.threshold.selection.with.subjectID)
  comparing.to.actula.label.data.frame.Bool..with.subjectID <- cbind(comparing.to.actula.label.data.frame, all.classifier.only.prediction.Prob.4.threshold.selection.with.subjectID[,c(l-1,l)])
  View(comparing.to.actula.label.data.frame.Bool..with.subjectID)
  l <- ncol(comparing.to.actula.label.data.frame.Bool..with.subjectID)
  #"1-" beacuse we want to count misclassificatoin rate 
  mis.classified.no <- rowSums(1-(comparing.to.actula.label.data.frame.Bool..with.subjectID[,c(1:(l-2))]))
  View(mis.classified.no)
  comparing.to.actula.label.data.frame.Bool..with.subjectID$mis.classified.no <- mis.classified.no
  View(comparing.to.actula.label.data.frame.Bool..with.subjectID)
  september.LWR
  comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData <- merge(x = comparing.to.actula.label.data.frame.Bool..with.subjectID, 
           y = september.LWR, by.x = "subjectID", by.y = "subjectid")
  dim(comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData)
  comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.leastOneTime <- subset(comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData, comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData$mis.classified.no >= 6)
  comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.hardInstances <- subset(comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData, comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData$mis.classified.no >= 7)
  comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.VeryhardInstances <- subset(comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData, comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData$mis.classified.no >= 8)
  comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.SuperhardInstances <- subset(comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData, comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData$mis.classified.no >= 11)
  hard.instance.list <- list(comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.leastOneTime,
       comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.hardInstances,
       comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.VeryhardInstances,
       comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.SuperhardInstances
       )
  
  save(hard.instance.list, file = paste(writing_path_data ,strTag , "hard.instance.list.R.list", sep = ""))
  
  
  table(comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.SuperhardInstances$recentDxJ_dxCode)
  
  table(comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.leastOneTime$recentDxJ_dxCode)
  
  table(comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.VeryhardInstances$recentDxJ_dxCode)
  
  table(comparing.to.actula.label.data.frame.Bool..with.subjectID.withPhenoData.misclassified.at.hardInstances$recentDxJ_dxCode)
  
  #
  no_of_djx_changes()
  
  
  
}





  
  
  
  
