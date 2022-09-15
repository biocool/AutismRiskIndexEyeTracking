#In this script I'm trying to explain the trained treebag classifier in a simple way, 
#and use new metrics that Karen's mentioned and see the effects and performance: just Z-scores for sacc/sec in geo and soc, and also pcntfix
#for computing the z-scores she would like to use only non-ASD kids for computing the 


setwd("/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/New Results 12.3.2021/")
temp <- loadRData("trained.models.10foldCV.list.12.15.2021")

library(rattle)

#loading the trained models================
#the below model was trained for the ACE grants
bagtreeTrainedModel.forACEgrants.AllFiveET.Tests.12.3.2021 <- 
  loadRData("/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/09-10-2021-Update on Missing Fixations for Tobii Project 1-3/FiveET_Test/ProcessedUsingR/ReadyForR/All_ET_Combination/4Grant/WholeData.as.Train/bagtreeTrainedModel")
View(bagtreeTrainedModel.forACEgrants.AllFiveET.Tests.12.3.2021)
length(bagtreeTrainedModel.forACEgrants.AllFiveET.Tests.12.3.2021$finalModel$mtrees)#25

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


#reading data for all five tests===========
path.4.reading.the.data <- "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/09-10-2021-Update on Missing Fixations for Tobii Project 1-3/FiveET_Test/ProcessedUsingR/ReadyForR/All_ET_Combination/"
full.path <- paste(path.4.reading.the.data, "4GrantAllFiveET.data.imputed.Normalized.csv", sep = "")
allFiveET.data.imputed.Normalized.12.6.2021 <- read_ET_data(file.path = full.path)
dim(allFiveET.data.imputed.Normalized.12.6.2021)#321  56
View(allFiveET.data.imputed.Normalized.12.6.2021)




#bagtree visualization==================
library(RColorBrewer)
i <- 10
pdf(paste("sampletree.viz.using.fancyRpartPlot", as.character(i), ".pdf", sep = ""))
fancyRpartPlot(bagtreeTrainedModel.forACEgrants.AllFiveET.Tests.12.3.2021$finalModel$mtrees[[i]]$btree,  
               palettes =  "YlOrRd")
dev.off()
View(bagtreeTrainedModel.forACEgrants.AllFiveET.Tests.12.3.2021$finalModel$param)

brewer.pal(3, "RdBu")
#reading the original train data for all five tests
path.4.reading.the.data <- 
  "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/09-10-2021-Update on Missing Fixations for Tobii Project 1-3/FiveET_Test/ProcessedUsingR/ReadyForR/All_ET_Combination/"

full.path <- paste(path.4.reading.the.data, "4GrantAllFiveET.data.imputed.Normalized.csv", sep = "")
GrantAllFiveET.data.imputed.Normalized <- read.csv(file = full.path)
dim(GrantAllFiveET.data.imputed.Normalized)#321  57
View(GrantAllFiveET.data.imputed.Normalized)
table(GrantAllFiveET.data.imputed.Normalized$recentDxJ_dxCode)
#ASD nonASD 
#195    126 
#the above table shows that we had more ASD than non-ASD
#but, I did undersampling in the training and therefor in each bag we have ~50%-50% data.

#feature importance=======
feature.importance.bagged.tree <- varImp(bagtreeTrainedModel.forACEgrants.AllFiveET.Tests.12.3.2021)
View(feature.importance.bagged.tree$importance)
# Overall (20 out of 55)
# ET1_NoOfSaccadsRatio       100.00
# ET1_FixationGeo             87.84
# ET1_NoOfSaccadesGeo         87.60
# ET8_FixationGeo             58.91
# ET8_NoOfSaccadesGeo         50.79
# ET7_NoOfSaccadsRatio        42.10
# ET1_SaccadesPerSecondSoc    41.01
# ET8_NoOfSaccadsRatio        40.94
# ET7_FixationGeo             33.42
# ET7_NoOfSaccadesSoc         30.07
# ET1_NoOfSaccadesSoc         27.36
# ET2_FixationGeo             26.86
# ET2_TotalFixationDuration   22.62
# ET2_NoOfSaccadsRatio        21.23
# ET1_SaccadesPerSecondGeo    21.03
# ET3_SaccadesPerSecondSoc    21.03
# ET2_SaccadesPerSecondSoc    19.27
# ET7_NoOfSaccadesGeo         18.46
# ET1_TotalFixationDuration   17.62
# ET7_PcntFixEyes             17.35




#IDEA: using sum(feature score) / no. of. features in each ET to detect the ET importance 

#another idea: do feature type importance analysis; for example we can conclude that saccads ratio is the most important feature...
#for this idea we should integrate all the feature importance scores for a single feature type (like pcntfixation; saccads ratio; ...) across all five ET

#All five ET are contributing to the final models 
all_ET.NoPoor.12.6.2021 <- 
  read_ET_data(file.path ="/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/09-10-2021-Update on Missing Fixations for Tobii Project 1-3/FiveET_Test/ProcessedUsingR/ReadyForR/Final_Final_ET_Merged.csv" )
dim(all_ET.NoPoor.12.6.2021)#2377   64
View(all_ET.NoPoor.12.6.2021)
save(all_ET.NoPoor.12.6.2021, file = "all_ET.NoPoor.12.6.2021")
write.csv(all_ET.NoPoor.12.6.2021,
          file = "all_ET.NoPoor.12.6.2021.csv")

#computing confusion matrix for the trained bagedtree============
bagtree.TrainingRes <- 
  read.csv("/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/New Results 12.3.2021/bagtree.TrainingRes.csv")
bagtree.TrainingRes$pred <- as.factor(bagtree.TrainingRes$pred)
bagtree.TrainingRes$obs <- as.factor(bagtree.TrainingRes$obs)
x <- confusionMatrix(data = bagtree.TrainingRes$pred, 
                reference  = bagtree.TrainingRes$obs, positive = "ASD")
View(x)
View(bagtree.TrainingRes)

#doing cross validation =======
#loading cross validation results from grant 
learned.models.usingAllFiveTests.321Samples.Repeated10fCV <- 
  loadRData("/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/09-10-2021-Update on Missing Fixations for Tobii Project 1-3/FiveET_Test/ProcessedUsingR/ReadyForR/All_ET_Combination/4Grant AllFive .LearnedModels.R.List2")
View(learned.models.usingAllFiveTests.321Samples.Repeated10fCV)

#data set :
allFiveET.data.imputed.Normalized.12.6.2021
my.train.control <- setting.the.trainControl()

metric <- "Spec"
selectionFunction <- "oneSE"

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 10,
  returnData = TRUE,
  classProbs = TRUE,
  returnResamp = "all",
  summaryFunction = twoClassSummary,
  sampling = "down",
  savePredictions = "final"
)

trained_treebag.5fCV.12.7.2021 <- train(recentDxJ_dxCode ~ ., data = allFiveET.data.imputed.Normalized.12.6.2021, 
                                     method = "treebag", 
                                     trControl = fitControl, 
                                     verbose = FALSE,
                                     selectionFunction = selectionFunction,
                                     metric = metric)

trained_rda.5fCV.12.7.2021 <- train(recentDxJ_dxCode ~ ., data = allFiveET.data.imputed.Normalized.12.6.2021, 
                                        method = "rda", 
                                        trControl = fitControl, 
                                        verbose = FALSE,
                                        selectionFunction = selectionFunction,
                                        metric = metric)

save(trained_rda.5fCV.12.7.2021, file = "trained_rda.5fCV.12.7.2021")
varImp(trained_rda.5fCV.12.7.2021)
# only 20 most important variables are shown (out of 55)
#Importance 
# ET1_FixationGeo              100.00
# ET1_NoOfSaccadesGeo           97.51
# ET1_NoOfSaccadsRatio          95.06
# ET8_FixationGeo               93.43
# ET8_NoOfSaccadesGeo           91.21
# ET8_NoOfSaccadsRatio          88.99
# ET7_FixationGeo               81.03
# ET7_NoOfSaccadsRatio          80.98
# ET7_NoOfSaccadesSoc           79.30
# ET2_FixationGeo               78.50
# ET1_SaccadesPerSecondSoc      76.83
# ET8_SaccadesPerSecondSoc      72.65
# ET2_NoOfSaccadsRatio          71.64
# ET2_NoOfSaccadesGeo           69.19
# ET3_FixationGeo               67.59
# ET2_SaccadesPerSecondSoc      60.00
# ET3_NoOfSaccadsRatio          58.71
# ET7_NoOfSaccadesGeo           52.37
# ET3_NoOfSaccadesGeo           51.78
# ET3_NoOfSaccadesSoc           49.82

#selcting a random subset with the non-sig different clinical characteristics========
#In this section I'm going to write a function to randomly extract a subset of a bigger set of subjects
#in such a way that there is no significant difference in important clinical measures 
#including: sex, age, djx, ADOS severity, Mullen elc, vineland abc, vinalnd social, vinaland communicatoin

#the below package seems a great one to do that!
library("BalancedSampling")

head(allFiveET.data.imputed.Normalized.12.6.2021)

length(intersect(lwr.sept.2021.freeze$subjectid, rownames(allFiveET.data.imputed.Normalized.12.6.2021)))
temp.lwr <- read.csv("/Users/apple/Desktop/Eric/Research/Karen/All Clinical Data/LWR /LWReport_September_09012021_FreezeFreeze.csv")
length(intersect(temp.lwr$subjectid, rownames(allFiveET.data.imputed.Normalized.12.6.2021)))
dim(allFiveET.data.imputed.Normalized.12.6.2021)

lwr.sept.2021.freeze.subset.with.five.ET <- 
  subset(lwr.sept.2021.freeze,
         lwr.sept.2021.freeze$subjectid %in% rownames(allFiveET.data.imputed.Normalized.12.6.2021))
dim(lwr.sept.2021.freeze.subset.with.five.ET)#314 446
lwr.sept.2021.freeze.subset.with.five.ET <- 
  as.data.frame(lwr.sept.2021.freeze.subset.with.five.ET)

#a subset of important clinical measures to be used for selecting a balanced random subset 
phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled <- 
  cbind(lwr.sept.2021.freeze.subset.with.five.ET$recentDxJ_dxCode ,
  lwr.sept.2021.freeze.subset.with.five.ET$gender,
  lwr.sept.2021.freeze.subset.with.five.ET$DxJ_agemo_1,
  lwr.sept.2021.freeze.subset.with.five.ET$ados_CoSoTotRRTot_1,
  lwr.sept.2021.freeze.subset.with.five.ET$vine_AdapBehav_DomStd_1,
  lwr.sept.2021.freeze.subset.with.five.ET$vine_ComTotal_DomStd_1,
  lwr.sept.2021.freeze.subset.with.five.ET$vine_SocTotal_DomStd_1,
  lwr.sept.2021.freeze.subset.with.five.ET$mullen_ELC_Std_1,
  lwr.sept.2021.freeze.subset.with.five.ET$mullen_ELT_1,
  lwr.sept.2021.freeze.subset.with.five.ET$mullen_RLT_1
  )

rownames(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled)<- 
  lwr.sept.2021.freeze.subset.with.five.ET$subjectid

allFiveET.data.imputed.Normalized.12.6.2021.with.values.in.lwr <- 
  subset(allFiveET.data.imputed.Normalized.12.6.2021,
         rownames(allFiveET.data.imputed.Normalized.12.6.2021) %in% lwr.sept.2021.freeze$subjectid)
dim(allFiveET.data.imputed.Normalized.12.6.2021.with.values.in.lwr)#314  56
rownames(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled) <- 
  lwr.sept.2021.freeze.subset.with.five.ET$subjectid
#double checking 
sum(rownames(allFiveET.data.imputed.Normalized.12.6.2021.with.values.in.lwr)%in%
      rownames(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled))#314
#DONE!

dim(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled)#314   8
colnames(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled) <- 
  c("DJX", "gender", "Age.First.Visit", "ADOS.COSO.RR.Tot",
    "vine_AdapBehav", "vine_ComTotal", "vine_SocTotal", "mullen_ELC")

View(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled)
phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled<- 
  as.data.frame(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled)
#NOTE!!!:
#it's better to use average age of five ET instead of "age at DX1" 

#having a backup of the original pheno data 
backup <- 
  phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled
#converting all features to numeric 
#beacuse the "lpm2" function from "BalancedSampling" only accepts numeric values
phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled$DJX[
  phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled$DJX != "ASD"] <- 0

phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled$DJX[
  phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled$DJX == "ASD"] <- 1
table(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled$DJX)
# 0   1 
# 124 190 

phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled$gender[
  phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled$gender != "M"] <- 0

phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled$gender[
  phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled$gender == "M"] <- 1
table(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled$gender)
# 0   1 
# 93 221 


phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled <- 
  apply(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled, 2, as.double)
apply(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled, 2, typeof)
#all are double!
dim(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled)
prob.vctr <- rep(50/314, 314)
x <- lpm2(prob = prob.vctr, 
          x = phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled)
View(x)

#balanced random subsampleing =========
#the below subset of the phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled should be balanced in terms of 4 features 
#compared to the original dataset
lwr.balnced.subset.4.five.ET.to.be.used.as.IndepTest <- 
  phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled[x,]
View(lwr.balnced.subset.4.five.ET.to.be.used.as.IndepTest)

#assigning corresponding subject IDs to the rownames of the subsampled dataset
rownames(lwr.balnced.subset.4.five.ET.to.be.used.as.IndepTest) <- 
  rownames(phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled)[x]

#comparing the subset with the original dataset=========
#applying t.test on all columns
i <- 1
for (i in c(1:ncol(lwr.balnced.subset.4.five.ET.to.be.used.as.IndepTest))) 
{
  comp.t.test.rest <- t.test(lwr.balnced.subset.4.five.ET.to.be.used.as.IndepTest[,i],
         phenoData.4.allFiveET.data.imputed.Normalized.12.6.2021.to.be.balanced.undersampleled[,i])
  print(comp.t.test.rest$p.value)
  
}
#[1] 0.8423521
# [1] 0.7399521
# [1] 0.675228
# [1] 0.9571096
# [1] 0.8361338
# [1] 0.5561256
# [1] 0.5428628
# [1] 0.3663109

#performance assesment using indep test set============
#the below variable is the original dataset for five ET
#first, I should drop the features that are not based on pcnt fix; and saccads
#second, I should delete the indep.test data from the original data 
#third, doing training and CV
#forth indep testing performance measure estimation
dim(allFiveET.data.imputed.Normalized.12.6.2021)#321  56
View(colnames(allFiveET.data.imputed.Normalized.12.6.2021))
View(lwr.balnced.subset.4.five.ET.to.be.used.as.IndepTest)
#Step one was done before!
#Step2:
allFiveET.data.imputed.Normalized.training.12.14.2021 <- 
  subset(allFiveET.data.imputed.Normalized.12.6.2021, 
       !(rownames(allFiveET.data.imputed.Normalized.12.6.2021)%in%rownames(lwr.balnced.subset.4.five.ET.to.be.used.as.IndepTest)))
dim(allFiveET.data.imputed.Normalized.training.12.14.2021)#271  56

allFiveET.data.imputed.Normalized.IndepTest.12.14.2021 <- 
  subset(allFiveET.data.imputed.Normalized.12.6.2021, 
         (rownames(allFiveET.data.imputed.Normalized.12.6.2021)%in%rownames(lwr.balnced.subset.4.five.ET.to.be.used.as.IndepTest)))
dim(allFiveET.data.imputed.Normalized.IndepTest.12.14.2021)#50 56

#pickling
# save(allFiveET.data.imputed.Normalized.training.12.14.2021,
#      file = "allFiveET.data.imputed.Normalized.training.12.14.2021")
# save(allFiveET.data.imputed.Normalized.IndepTest.12.14.2021,
#      file = "allFiveET.data.imputed.Normalized.IndepTest.12.14.2021")

#extracting the name of the classifiers=============
#the below list contains trained classifiers. So, it means that this classifiers can be run again
learned.models.usingAllFiveTests.321Samples.Repeated10fCV
names(learned.models.usingAllFiveTests.321Samples.Repeated10fCV)
models.in.caret.2.be.trained.name.vctr <- 
  names(learned.models.usingAllFiveTests.321Samples.Repeated10fCV)

#====Doing CV==============

my.train.control.12.14.2021 <- 
  setting.the.trainControl()

trained.models.10foldCV.list <- 
  do.classification(ET.data.train = allFiveET.data.imputed.Normalized.training.12.14.2021, my.trainControl = my.train.control.12.14.2021, 
                  selected.Classifiers.to.be.trained = models.in.caret.2.be.trained.name.vctr)

length(trained.models.10foldCV.list)
length(models.in.caret.2.be.trained.name.vctr)
View(trained.models.10foldCV.list)
save(trained.models.10foldCV.list,
     file="trained.models.10foldCV.list.12.15.2021")

#========IndepTest========
i <- 1

i<-i+1
indep.test.pred.results.12.14.2021 <- predict.train(object = trained.models.10foldCV.list[["pda"]], 
              newdata = allFiveET.data.imputed.Normalized.IndepTest.12.14.2021)
#dim(indep.test.pred.results.12.14.2021)
confusionMatrix(data = indep.test.pred.results.12.14.2021, 
                allFiveET.data.imputed.Normalized.IndepTest.12.14.2021$recentDxJ_dxCode)


View(trained.models.10foldCV.list)
#writing details of caret and ROC in onenote!!!!!!
save(indep.test.pred.results.12.14.2021,
     file = "indep.test.pred.results.12.14.2021")

View(indep.test.pred.results.12.14.2021)
#=======adding some ados and csbs questions==============









