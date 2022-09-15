#classifier based for combining different clinical scores 

#!!! Karen: remove question 23 

library(caret)
library(rpart.plot)
View(ep_CSBS_Merged_4_numericalStatistical_analysis)
View(ep_CSBS_Merged_4_numericalStatistical_analysis)
train_data <- ep_CSBS_Merged_4_numericalStatistical_analysis[,-c(1,3)] #removing subject ID and gender
train_data <- train_data [,-c(2:5)] #removing ados and age 
View(train_data)


#rpart=======
cvCtrl <- trainControl(summaryFunction = twoClassSummary,
                       classProbs = TRUE)
traned.dt <- train(recentDxJ_dxCode ~ ., data = train_data, method="rpart", trControl = cvCtrl)

plot(traned.dt$finalModel, uniform=TRUE,
     main="Classification Tree")
text(traned.dt$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
suppressMessages(library(rattle))
rpart.plot(traned.dt$finalModel)
#cp          ROC        Sens       Spec     
#0.04359673  0.6593379  0.4811281  0.7822955

#set tuneLength = 30 for checking more models 
cvCtrl <- trainControl(summaryFunction = twoClassSummary,
                       classProbs = TRUE)

View(train_data_2)
rpartTune <- train(recentDxJ_dxCode ~ ., data = train_data_2, method="rpart",
                   tuneLength = 30,
                   metric = "ROC",
                   trControl = cvCtrl)
#                   cp           ROC        Sens       Spec 
# Prev (with Q23): 0.007422719  0.6810796  0.4958864  0.8005401

train_data_2 <- train_data[,-24] #removing question 23 as Karen said
View(train_data_2)
rpartTune_2 <- train(recentDxJ_dxCode ~ ., data = train_data_2, method="rpart",
                   tuneLength = 30,
                   metric = "ROC",
                   trControl = cvCtrl)


rpart.plot(rpartTune_2$finalModel)

#cp           ROC        Sens       Spec 
#0.0148454  0.67776  0.51686  0.77429


#testing  different subset of features ============


t <- train_data[, c("recentDxJ_dxCode", temp[[2000]])]

traned.dt <- train(recentDxJ_dxCode ~ ., data = t, method="rpart")

#plot(traned.dt$finalModel, uniform=TRUE,     main="Classification Tree")
#text(traned.dt$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
suppressMessages(library(rattle))
rpart.plot(traned.dt$finalModel)




#!report
dev.off()
jpeg(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/DT.jpg", res=200)
fancyRpartPlot(traned.dt$finalModel)
dev.off()


jpeg(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/DT_new.jpg", res=200)
rpart.plot(traned.dt$finalModel)

dev.off()


#all combination of columns==========
View(ep_CSBS_Merged_4_numericalStatistical_analysis)
#tmp would be a list of all combination of colnames with length 3
temp <- combn(names(ep_CSBS_Merged_4_numericalStatistical_analysis[,c(8:31)]),3,simplify=FALSE)
#we can use temp elements as colnames to extracting the corresponding columns 
t <- ep_CSBS_Merged_4_numericalStatistical_analysis[, temp[[1]]]
t <- ep_CSBS_Merged_4_numericalStatistical_analysis[, temp[[2]]]
View(temp[[2024]])
length(temp)

#EYE TRACKING TESTS========================================

ET_1_GeoPref1 <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/1-Final GeoPref MasterSheet.csv")
ET_2_ComplexSocialGeoPref2 <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/2-Complex Social GeoPref Mastersheet.csv")
ET_3_OutdoorPeerPlayMastersheet <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/3-Outdoor Peer Play Mastersheet.csv")

ET_7_MotheresevsTrafficMaster <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/7-QL Motherese vs Traffic Master Sheet.csv")
ET_8_MotheresevsTechnoSpaceMasterSheet <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/8-LK Motherese vs Techno Space Master Sheet.csv")
#ET_9_LBMotheresevsFlatAffect <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/9-LB Motherese vs Flat Affect Master Sheet.csv")

#!!! Report
dim(ET_1_GeoPref1) #3474  518 --> 3474   9
dim(ET_2_ComplexSocialGeoPref2) #472  31 -->  472  16
dim(ET_3_OutdoorPeerPlayMastersheet)#541  32 --> 541  14
dim(ET_7_MotheresevsTrafficMaster)#810 497 --> 810 13
dim(ET_8_MotheresevsTechnoSpaceMasterSheet)#634 493 --> 634 15  ??? New: 744 15
#dim(ET_9_LBMotheresevsFlatAffect)#485 507 --> 485 46



# preprocessing========================================================================================================================================================================================================================
#1- NA raplacement

ET_1_GeoPref1 <- preprocess_NA_replacement(ET_1_GeoPref1)
ET_2_ComplexSocialGeoPref2 <- preprocess_NA_replacement(ET_2_ComplexSocialGeoPref2)
ET_3_OutdoorPeerPlayMastersheet <- preprocess_NA_replacement(ET_3_OutdoorPeerPlayMastersheet)

ET_7_MotheresevsTrafficMaster <- preprocess_NA_replacement(ET_7_MotheresevsTrafficMaster)
ET_8_MotheresevsTechnoSpaceMasterSheet <- preprocess_NA_replacement(ET_8_MotheresevsTechnoSpaceMasterSheet)
#ET_9_LBMotheresevsFlatAffect <- preprocess_NA_replacement(ET_9_LBMotheresevsFlatAffect)


View(ET_3_OutdoorPeerPlayMastersheet)
#2 subsetting based on missing value percent
ET_1_GeoPref1 <- ET_1_GeoPref1[,na.percent.4Dataframe_indexvector(ET_1_GeoPref1, 20)]
ET_2_ComplexSocialGeoPref2 <- ET_2_ComplexSocialGeoPref2[,na.percent.4Dataframe_indexvector(ET_2_ComplexSocialGeoPref2, 20)]
ET_3_OutdoorPeerPlayMastersheet <- ET_3_OutdoorPeerPlayMastersheet[,na.percent.4Dataframe_indexvector(ET_3_OutdoorPeerPlayMastersheet, 20)]
ET_7_MotheresevsTrafficMaster <- ET_7_MotheresevsTrafficMaster[,na.percent.4Dataframe_indexvector(ET_7_MotheresevsTrafficMaster, 20)]
ET_8_MotheresevsTechnoSpaceMasterSheet <- ET_8_MotheresevsTechnoSpaceMasterSheet[,na.percent.4Dataframe_indexvector(ET_8_MotheresevsTechnoSpaceMasterSheet, 20)]
#ET_9_LBMotheresevsFlatAffect <- ET_9_LBMotheresevsFlatAffect[,na.percent.4Dataframe_indexvector(ET_9_LBMotheresevsFlatAffect, 20)]

#correcting column name in such a way that shows the origin of the column

colnames(ET_1_GeoPref1) <- element.Wise.pasting(vctr = colnames(ET_1_GeoPref1), str = "ET1GeoPref" )
colnames(ET_2_ComplexSocialGeoPref2) <- element.Wise.pasting(vctr = colnames(ET_2_ComplexSocialGeoPref2), str = "ET2ComplexSocia" )
colnames(ET_3_OutdoorPeerPlayMastersheet) <- element.Wise.pasting(vctr = colnames(ET_3_OutdoorPeerPlayMastersheet), str = "ET3_OutdoorPeerPlay" )
colnames(ET_7_MotheresevsTrafficMaster) <- element.Wise.pasting(vctr = colnames(ET_7_MotheresevsTrafficMaster), str = "ET7MotheresevsTraffic" )
colnames(ET_8_MotheresevsTechnoSpaceMasterSheet) <- element.Wise.pasting(vctr = colnames(ET_8_MotheresevsTechnoSpaceMasterSheet), str = "ET8MotheresevsTechno" )
#colnames(ET_9_LBMotheresevsFlatAffect) <- element.Wise.pasting(vctr = colnames(ET_9_LBMotheresevsFlatAffect), str = "ET9LBMotherese" )
View(ET_2_ComplexSocialGeoPref2)

#selecting longitudinal based on the date of the visit (select first visit)
#sorting based on SubjectId and then based on AgeMo
#View(ET_1_GeoPref1)
#selecting only the first row for non-unique sbjID =====================


View(ET_2_ComplexSocialGeoPref2)
dim(ET_1_GeoPref1)
ET_1_GeoPref1 <- select.only.first.visit(ET_1_GeoPref1,1,2 )
ET_2_ComplexSocialGeoPref2 <- select.only.first.visit(ET_2_ComplexSocialGeoPref2,1,2 )
ET_3_OutdoorPeerPlayMastersheet <- select.only.first.visit(ET_3_OutdoorPeerPlayMastersheet,1,2 )
ET_7_MotheresevsTrafficMaster <- select.only.first.visit(ET_7_MotheresevsTrafficMaster,1,2 )
ET_8_MotheresevsTechnoSpaceMasterSheet <- select.only.first.visit(ET_8_MotheresevsTechnoSpaceMasterSheet,1,2 )
length(ET_1_GeoPref1$ET1GeoPrefSubject.ID)
length(unique(ET_1_GeoPref1$ET1GeoPrefSubject.ID))

View(ET_1_GeoPref1)
View(ET_2_ComplexSocialGeoPref2)


#removing those with vision problem================================= 

which(colnames(ET_1_GeoPref1)=="ET1GeoPrefVision.Abnormalities.Notes")
dim(ET_1_GeoPref1)
#ET_1_GeoPref1 <- vision_problem_removed(ET_1_GeoPref1, 3)
#dim(ET_1_GeoPref1)

dim(ET_2_ComplexSocialGeoPref2)# 437  10
which(colnames(ET_2_ComplexSocialGeoPref2)=="ET2ComplexSociaVision.Abnormalities")
ET_2_ComplexSocialGeoPref2 <- vision_problem_removed(ET_2_ComplexSocialGeoPref2, 5)
dim(ET_2_ComplexSocialGeoPref2)# 421  10


dim(ET_3_OutdoorPeerPlayMastersheet) #491  10
which(colnames(ET_3_OutdoorPeerPlayMastersheet)=="ET3_OutdoorPeerPlayVision")
ET_3_OutdoorPeerPlayMastersheet <- vision_problem_removed(ET_3_OutdoorPeerPlayMastersheet, 5)
dim(ET_3_OutdoorPeerPlayMastersheet)#474  10

dim(ET_7_MotheresevsTrafficMaster)#617  13
which(colnames(ET_7_MotheresevsTrafficMaster)=="ET7MotheresevsTrafficVision.Abnormalities")
ET_7_MotheresevsTrafficMaster <- vision_problem_removed(ET_7_MotheresevsTrafficMaster, 3)
dim(ET_7_MotheresevsTrafficMaster)#601  13

dim(ET_8_MotheresevsTechnoSpaceMasterSheet)#578  15
which(colnames(ET_8_MotheresevsTechnoSpaceMasterSheet)=="ET8MotheresevsTechnoVision.Abnormalities")
ET_8_MotheresevsTechnoSpaceMasterSheet <- vision_problem_removed(ET_8_MotheresevsTechnoSpaceMasterSheet, 4)
dim(ET_8_MotheresevsTechnoSpaceMasterSheet)#578  15


#which(colnames(ET_9_LBMotheresevsFlatAffect)=="ET9LBMothereseVision.Abnormalities")
#ET_9_LBMotheresevsFlatAffect <- vision_problem_removed(ET_9_LBMotheresevsFlatAffect, 3)

#removing based on data quality 
#for ET1, ET7, ET8, and ET9 all data have good or moderate quality 
View(ET_3_OutdoorPeerPlayMastersheet)
View(ET_2_ComplexSocialGeoPref2)


dim(ET_2_ComplexSocialGeoPref2)# 437  10
ET_2_ComplexSocialGeoPref2 <- subset(ET_2_ComplexSocialGeoPref2, 
                                     !(ET_2_ComplexSocialGeoPref2$ET2ComplexSociaData.Quality == "Poor" | ET_2_ComplexSocialGeoPref2$ET2ComplexSociaData.Quality == "Unknown")
)
dim(ET_2_ComplexSocialGeoPref2) #303  10


dim(ET_3_OutdoorPeerPlayMastersheet)#474  10
ET_3_OutdoorPeerPlayMastersheet <- subset(ET_3_OutdoorPeerPlayMastersheet, 
                                          !(ET_3_OutdoorPeerPlayMastersheet$ET3_OutdoorPeerPlayData.Quality == "Poor" | ET_3_OutdoorPeerPlayMastersheet$ET3_OutdoorPeerPlayData.Quality == "Unknown")
)
dim(ET_3_OutdoorPeerPlayMastersheet)#384  10


#!!! removing  vision and data qulaity columns
View(ET_3_OutdoorPeerPlayMastersheet)
#ET_1_GeoPref1 <- ET_1_GeoPref1[,-3]
ET_2_ComplexSocialGeoPref2 <- ET_2_ComplexSocialGeoPref2[,-c(3:5)]
ET_3_OutdoorPeerPlayMastersheet <- ET_3_OutdoorPeerPlayMastersheet[,-c(3:5)]
ET_7_MotheresevsTrafficMaster <- ET_7_MotheresevsTrafficMaster[,-3]
ET_8_MotheresevsTechnoSpaceMasterSheet <- ET_8_MotheresevsTechnoSpaceMasterSheet[,-c(3,4)]
#ET_9_LBMotheresevsFlatAffect <- ET_9_LBMotheresevsFlatAffect[,-3]




#removing some specific rows 
View(ET_1_GeoPref1)
ET_1_GeoPref1 <- ET_1_GeoPref1[order(ET_1_GeoPref1$ET1GeoPrefAge.at.ET),] 
ET_1_GeoPref1 <- ET_1_GeoPref1[-c(1:5),]
which(ET_1_GeoPref1$ET1GeoPrefAge.at.ET == 0)#119 165
which(ET_1_GeoPref1$ET1GeoPrefSubject.ID == "V7S2Y")#251 #this subject has empty age
ET_1_GeoPref1 <- ET_1_GeoPref1 [-c(119, 165, 251),]
dim(ET_1_GeoPref1)


View(ET_2_ComplexSocialGeoPref2)
which(ET_2_ComplexSocialGeoPref2$ET2ComplexSociaSubject.ID == "0")#1
which(ET_2_ComplexSocialGeoPref2$ET2ComplexSociaSubject.ID == "R")#135
which(ET_2_ComplexSocialGeoPref2$ET2ComplexSociaET.Age == 0)
ET_2_ComplexSocialGeoPref2 <- ET_2_ComplexSocialGeoPref2 [-c(1, 135),]
dim(ET_2_ComplexSocialGeoPref2)


View(ET_3_OutdoorPeerPlayMastersheet)
ET_3_OutdoorPeerPlayMastersheet <- ET_3_OutdoorPeerPlayMastersheet[order(ET_3_OutdoorPeerPlayMastersheet$ET3_OutdoorPeerPlayET.Age),]
ET_3_OutdoorPeerPlayMastersheet <- ET_3_OutdoorPeerPlayMastersheet [-c(1:8),] # no valid age
dim(ET_3_OutdoorPeerPlayMastersheet)

#regarding the age feature ET7 and ET8 don't have problem. 



#!!! converting to numeric
View(ET_7_MotheresevsTrafficMaster)
ET_1_GeoPref1[,2:length(ET_1_GeoPref1[1,])] <- apply(ET_1_GeoPref1[,2:length(ET_1_GeoPref1[1,])], 2, as.numeric)
ET_2_ComplexSocialGeoPref2[,2:length(ET_2_ComplexSocialGeoPref2[1,])] <- apply(ET_2_ComplexSocialGeoPref2[,2:length(ET_2_ComplexSocialGeoPref2[1,])], 2, as.numeric)
ET_3_OutdoorPeerPlayMastersheet[,3:length(ET_3_OutdoorPeerPlayMastersheet[1,])] <- apply(ET_3_OutdoorPeerPlayMastersheet[,3:length(ET_3_OutdoorPeerPlayMastersheet[1,])], 2, as.numeric) # for this data age has some problems I skipped converting age

ET_7_MotheresevsTrafficMaster[,2:length(ET_7_MotheresevsTrafficMaster[1,])] <- apply(ET_7_MotheresevsTrafficMaster[,2:length(ET_7_MotheresevsTrafficMaster[1,])], 2, as.numeric)
ET_8_MotheresevsTechnoSpaceMasterSheet[,2:length(ET_8_MotheresevsTechnoSpaceMasterSheet[1,])] <- apply(ET_8_MotheresevsTechnoSpaceMasterSheet[,2:length(ET_8_MotheresevsTechnoSpaceMasterSheet[1,])], 2, as.numeric)
#ET_9_LBMotheresevsFlatAffect[,2:length(ET_9_LBMotheresevsFlatAffect[1,])] <- apply(ET_9_LBMotheresevsFlatAffect[,2:length(ET_9_LBMotheresevsFlatAffect[1,])], 2, as.numeric)






#ET_2_ComplexSocialGeoPref2 <- ET_2_ComplexSocialGeoPref2[-1,] # removing the first record without subject ID
#ET_3_OutdoorPeerPlayMastersheet <- ET_3_OutdoorPeerPlayMastersheet[-1,]

#saving data on HDD
write.csv(ET_1_GeoPref1, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/ET_1_GeoPref1.csv")
write.csv(ET_2_ComplexSocialGeoPref2, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/ET_2_ComplexSocialGeoPref2.csv")
write.csv(ET_3_OutdoorPeerPlayMastersheet, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/ET_3_OutdoorPeerPlayMastersheet.csv")
write.csv(ET_7_MotheresevsTrafficMaster, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/ET_7_MotheresevsTrafficMaster.csv")
write.csv(ET_8_MotheresevsTechnoSpaceMasterSheet, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/ET_8_MotheresevsTechnoSpaceMasterSheet.csv")
#write.csv(ET_9_LBMotheresevsFlatAffect, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/ET_9_LBMotheresevsFlatAffect.csv")

library(ggVennDiagram)
ET_sbjIDs<- list(ET_1_GeoPref1= ET_1_GeoPref1$ET1GeoPrefSubject.ID,
     ET_2_ComplexSocialGeoPref2= ET_2_ComplexSocialGeoPref2$ET2ComplexSociaSubject.ID,
     ET_3_OutdoorPeerPlayMastersheet= ET_3_OutdoorPeerPlayMastersheet$ET3_OutdoorPeerPlaySubject.ID,
     ET_7_MotheresevsTrafficMaster= ET_7_MotheresevsTrafficMaster$ET7MotheresevsTrafficSubject.ID,
     ET_8_MotheresevsTechnoSpaceMasterSheet= ET_8_MotheresevsTechnoSpaceMasterSheet$ET8MotheresevsTechnoSubject.ID)

ET_sbjIDs <- list(GeoPref= ET_1_GeoPref1$ET1GeoPrefSubject.ID,
                 ComplexGeoPref= ET_2_ComplexSocialGeoPref2$ET2ComplexSociaSubject.ID,
                 MothTechno= ET_8_MotheresevsTechnoSpaceMasterSheet$ET8MotheresevsTechnoSubject.ID,
                 MothTraffic= ET_7_MotheresevsTrafficMaster$ET7MotheresevsTrafficSubject.ID,
                 OutdPlay=ET_3_OutdoorPeerPlayMastersheet$ET3_OutdoorPeerPlaySubject.ID
)
library(ggVennDiagram)
library(ggplot2)
ggVennDiagram(ET_sbjIDs, label_alpha = 0) + scale_fill_gradient(low="white", high = "white")
ET12<- union(ET_1_GeoPref1$ET1GeoPrefSubject.ID, ET_2_ComplexSocialGeoPref2$ET2ComplexSociaSubject.ID)
ET34<- union(ET_8_MotheresevsTechnoSpaceMasterSheet$ET8MotheresevsTechnoSubject.ID,
             ET_7_MotheresevsTrafficMaster$ET7MotheresevsTrafficSubject.ID)

ET1234 <- union(ET12, ET34)
ET12345 <- union(ET1234, ET_3_OutdoorPeerPlayMastersheet$ET3_OutdoorPeerPlaySubject.ID)
length(ET12345) #2457

#!!! Report 
dim(ET_1_GeoPref1) #2331    9 ==New: 2336    9
dim(ET_2_ComplexSocialGeoPref2)#209   7 ==New:303  10
dim(ET_3_OutdoorPeerPlayMastersheet)#187   7 == New:384  10
dim(ET_7_MotheresevsTrafficMaster)#601  12 ==New:601  13
dim(ET_8_MotheresevsTechnoSpaceMasterSheet)#493  13 ==New: 571  15 
#dim(ET_9_LBMotheresevsFlatAffect)#400  36 ==New:

Final_ET_1_GeoPref1 <- ET_1_GeoPref1
Final_ET_2_ComplexSocialGeoPref2 <- ET_2_ComplexSocialGeoPref2
Final_ET_3_OutdoorPeerPlayMastersheet <- ET_3_OutdoorPeerPlayMastersheet
Final_ET_7_MotheresevsTrafficMaster <- ET_7_MotheresevsTrafficMaster
Final_ET_8_MotheresevsTechnoSpaceMasterSheet <- ET_8_MotheresevsTechnoSpaceMasterSheet
#Final_ET_9_LBMotheresevsFlatAffect <- ET_9_LBMotheresevsFlatAffect

#computing intersect of all 6 different ET test
x1 <- intersect(Final_ET_1_GeoPref1$ET1GeoPrefSubject.ID, Final_ET_2_ComplexSocialGeoPref2$ET2ComplexSociaSubject.ID)
x2 <- intersect(Final_ET_3_OutdoorPeerPlayMastersheet$ET3_OutdoorPeerPlaySubject.ID, Final_ET_7_MotheresevsTrafficMaster$ET7MotheresevsTrafficSubject.ID)
x3 <- Final_ET_8_MotheresevsTechnoSpaceMasterSheet$ET8MotheresevsTechnoSubject.ID
y <- intersect(x1, x2)
z <- intersect(x3, y)
#!!! Report
length(z)#134 #224


#merging ET data========================================================================================================================================================================================================================
#removing age 
View(Final_ET_1_GeoPref1)
Final_ET_1_GeoPref1 <- Final_ET_1_GeoPref1[, -2]
View(Final_ET_2_ComplexSocialGeoPref2)
Final_ET_2_ComplexSocialGeoPref2<- Final_ET_2_ComplexSocialGeoPref2[, -2]
View(Final_ET_3_OutdoorPeerPlayMastersheet)
Final_ET_3_OutdoorPeerPlayMastersheet<- Final_ET_3_OutdoorPeerPlayMastersheet[, -2]
View(Final_ET_7_MotheresevsTrafficMaster)
Final_ET_7_MotheresevsTrafficMaster <- Final_ET_7_MotheresevsTrafficMaster[, -2]
View(Final_ET_8_MotheresevsTechnoSpaceMasterSheet)
Final_ET_8_MotheresevsTechnoSpaceMasterSheet <- Final_ET_8_MotheresevsTechnoSpaceMasterSheet[, -2]
#View(Final_ET_9_LBMotheresevsFlatAffect)
Final_ET_9_LBMotheresevsFlatAffect <- Final_ET_9_LBMotheresevsFlatAffect[, -2]


x1 <- merge(x= Final_ET_1_GeoPref1, 
           y= Final_ET_2_ComplexSocialGeoPref2, by.x = "ET1GeoPrefSubject.ID", by.y = "ET2ComplexSociaSubject.ID", all= TRUE)

x2 <- merge(x= Final_ET_3_OutdoorPeerPlayMastersheet, 
            y= Final_ET_7_MotheresevsTrafficMaster, by.x = "ET3_OutdoorPeerPlaySubject.ID", by.y = "ET7MotheresevsTrafficSubject.ID", all = TRUE)

x3 <- Final_ET_8_MotheresevsTechnoSpaceMasterSheet
#x3 <- merge(x= Final_ET_8_MotheresevsTechnoSpaceMasterSheet, 
            #y= Final_ET_9_LBMotheresevsFlatAffect, by.x = "ET8MotheresevsTechnoSubject.ID", by.y = "ET9LBMothereseSubject.ID", all = TRUE)
View(x1)

y <- merge(x= x1, 
            y= x2, by.x = "ET1GeoPrefSubject.ID", by.y = "ET3_OutdoorPeerPlaySubject.ID", all= TRUE)

Final_ET_Merged <- merge(x= y, 
           y= x3, by.x = "ET1GeoPrefSubject.ID", by.y = "ET8MotheresevsTechnoSubject.ID", all= TRUE)

dim(Final_ET_Merged)
#!!! Report 
#2448   72
#New: 2457   53

x1 <- union(Final_ET_1_GeoPref1$ET1GeoPrefSubject.ID, Final_ET_2_ComplexSocialGeoPref2$ET2ComplexSociaSubject.ID)
x2 <- union(Final_ET_3_OutdoorPeerPlayMastersheet$ET3_OutdoorPeerPlaySubject.ID, Final_ET_7_MotheresevsTrafficMaster$ET7MotheresevsTrafficSubject.ID)
x3 <- Final_ET_8_MotheresevsTechnoSpaceMasterSheet$ET8MotheresevsTechnoSubject.ID
y <- union(x1, x2)
z <- union(x3, y)

#!!!report total unique subjects 
length(z)#2457

library(summarytools)
x <- dfSummary(Final_ET_Merged)
View(x)

#index_vector <- na.percent.4Dataframe_indexvector(data = Final_ET_Merged, 30)
#Final_ET_Merged_NA30Percent <- Final_ET_Merged[,index_vector]
#dim(Final_ET_Merged_NA30Percent)

#View(Final_ET_Merged_NA30Percent)

#selecting those subjects with at least three different eye tracking tests 
IsInET1 <- as.numeric(Final_ET_Merged$ET1GeoPrefSubject.ID %in% Final_ET_1_GeoPref1$ET1GeoPrefSubject.ID)
IsInET2 <- as.numeric(Final_ET_Merged$ET1GeoPrefSubject.ID %in% Final_ET_2_ComplexSocialGeoPref2$ET2ComplexSociaSubject.ID)
IsInET3 <- as.numeric(Final_ET_Merged$ET1GeoPrefSubject.ID %in% Final_ET_3_OutdoorPeerPlayMastersheet$ET3_OutdoorPeerPlaySubject.ID)
IsInET7 <- as.numeric(Final_ET_Merged$ET1GeoPrefSubject.ID %in% Final_ET_7_MotheresevsTrafficMaster$ET7MotheresevsTrafficSubject.ID)
IsInET8 <- as.numeric(Final_ET_Merged$ET1GeoPrefSubject.ID %in% Final_ET_8_MotheresevsTechnoSpaceMasterSheet$ET8MotheresevsTechnoSubject.ID)
#IsInET9 <- as.numeric(Final_ET_Merged$ET1GeoPrefSubject.ID %in% Final_ET_9_LBMotheresevsFlatAffect$ET9LBMothereseSubject.ID)

Final_Final_ET_Merged <- cbind(IsInET1, IsInET2, IsInET3, IsInET7, IsInET8, Final_ET_Merged)
dim(Final_Final_ET_Merged) #2457   58
View(Final_Final_ET_Merged)

#!!! select only those 100+ subjects with all tests 

No_of_ET_Test_Participation <- IsInET1 + IsInET2 + IsInET3 + IsInET7 + IsInET8 
View(No_of_ET_Test_Participation)

Final_Final_ET_Merged <- cbind(No_of_ET_Test_Participation, Final_Final_ET_Merged)
View(Final_Final_ET_Merged)
hist(Final_Final_ET_Merged$No_of_ET_Test_Participation)

#adding diagnosis from extendet LWR file 
!!! here 
rownames(Final_Final_ET_Merged) <- Final_Final_ET_Merged$ET1GeoPrefSubject.ID

index_vector <- LWR_CSBS_Added_PreNatalAdded_inffeedQAdded_ET_Added$subjectid %in% Final_Final_ET_Merged$ET1GeoPrefSubject.ID
sum(index_vector)  #2386


dim(Final_Final_ET_Merged) #2457   59


#Probably we can do better because the new version of the LWR may have a larger intersection with the merged ET file 
Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR <- subset(
  Final_Final_ET_Merged, Final_Final_ET_Merged$ET1GeoPrefSubject.ID %in% LWR_CSBS_Added_PreNatalAdded_inffeedQAdded_ET_Added$subjectid)
dim(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR)#2386 out of 2457 


x1 <- LWR_CSBS_Added_PreNatalAdded_inffeedQAdded_ET_Added$recentDxJ_dxCode[index_vector]#diagnosis 
x2 <- LWR_CSBS_Added_PreNatalAdded_inffeedQAdded_ET_Added$subjectid[index_vector]#will be used for double checking 
length(x1)
length(x2)

View(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR)

temp <- Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR[x2, ]
View(temp)
temp <- cbind(x2, x1, temp)
sum(rownames(temp) == x2) == length(rownames(temp))#double checking 
colnames(temp)[c(1,2)] <- c("SubjectID", "DJX")
temp <- temp[,-10] # removing redundant subject Id column
Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR <- temp
Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR$DJX <- edit_DJX(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR$DJX)

#Constructing label ASD vs. nonASD 
Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR <- cbind(ASD_nonASD_classLabel= Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR$DJX, Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR)
View(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR)
Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR$ASD_nonASD_classLabel[Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR$ASD_nonASD_classLabel != "ASD"] <- "nonASD"
table(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR$ASD_nonASD_classLabel)

dim(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR) #2381 81 #New 2386   61
#!!! Report 
#   ASD nonASD 
#1153   1228 

#ASD nonASD 
#1035   1351 


#selecting a subset with at least 3 ET test=====
table(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR$No_of_ET_Test_Participation)
save(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MyProcessedData/Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR")
View(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR)
#!!! Report 
#1    2    3    4    5    6 
#1738  108  180  169   97   89 



#preparing data for WEKA ==========================================================================
Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA <- Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR
View(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA)
Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA <- cbind(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA, Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA$ASD_nonASD_classLabel) #adding lable as the last column
Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA <- Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA[, -c(1:4)]

Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA[is.na(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA)]<-"?"

#write.csv(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA.csv")

#Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA.csv")
View(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA)
colSums(as.numeric(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA[,c(2:6)]))
#generating all possible subsets 

#remove SANITY column !!!

#removing ET9 features 
which(colnames(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA) == "ET8MotheresevsTechnoPercent.Fix_Mouth")#44 the last ET8 index
length(colnames(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA))
colnames(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA)[45]
colnames(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA)[77]#last ET9 feature
Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA <- Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA[,-c(45:77)]
View(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA)
writing_path <- "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/NewCharlene/OnlyImportantFeatures-KarenSelected/"
weka_all_subset_indices_list <- all_subset(c(1:5))
#each element of this list is a vector which we can use for selecting those subjects who participated in specific ET tests
L <- length(weak_all_subset_indices_list)
for (i in c(1:L)) 
{
  print(weka_all_subset_indices_list[[i]])
  Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA[,weka_all_subset_indices_list[[i]]]
  k <- length(weka_all_subset_indices_list[[i]])
  index_vector <- Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA[,weka_all_subset_indices_list[[i]][1]]
  if (k>1) #product of all elements of corresponding cols of Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA to generate the final index vector
  {
    for (j in c(1:k)) 
    {
      index_vector <- index_vector * Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA[,weka_all_subset_indices_list[[i]][j]]
    }
  }
  index_vector <- index_vector == 1
  temp <- Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_4_WEKA[index_vector,-c(1:6)]#the first six columns are related to indices vectors 
  #writing temp on HDD
  str_4_filename <- paste(weka_all_subset_indices_list[[i]], collapse = '')
  str_4_filename <- paste(writing_path, str_4_filename, "_weka.csv", sep = "")
  write.csv(temp, file = str_4_filename)
  print(str_4_filename, dim(temp))
}



Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_ThreeET_test <- subset(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR, Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR$NoET_Test_Participation >= 3)
dim(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_ThreeET_test) #501 116
table(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_ThreeET_test$ASD_nonASD_classLabel)
#!!! Report
#ASD nonASD 
#332    169 


#doing some unsupervised analysis ==============================================================================================================================================================================================

#
library(Hmisc)
x <- Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_ThreeET_test[,c(11:116)]
View(x)
result<-rcorr((as.matrix(x)))
View(result$r)#correlations
temp <- result$r[rownames(x),]
View(result$P)#p-values for correlations
View(result$n)#number of observations used for each correlation

View(x)
#!!! Report
Heatmap(result$r)


#write.csv(Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_ThreeET_test, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MyProcessedData/Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_ThreeET_test.csv")

cvCtrl <- trainControl(summaryFunction = twoClassSummary,
                       classProbs = TRUE)

train_data_2 <- Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_ThreeET_test[,c(1,11:116)]
View(train_data_2)
colnames(train_data_2)[1]
rpartTune <- train(ASD_nonASD_classLabel ~ ., data = train_data_2, method="rpart",
                   tuneLength = 30,
                   metric = "ROC",
                   na.action  = na.pass,
                   trControl = cvCtrl)


suppressMessages(library(rattle))
rpart.plot(rpartTune$finalModel)
rpartTune$results




train_data_2$ASD_nonASD_classLabel[train_data_2$ASD_nonASD_classLabel == "ASD"] <- "1"
train_data_2$ASD_nonASD_classLabel[train_data_2$ASD_nonASD_classLabel == "nonASD"] <- "0"

rpartTune2 <- train(ASD_nonASD_classLabel ~ ., data = train_data_2, method="rpart",
                   tuneLength = 30,
                   metric='Spec',
                   na.action  = na.pass,
                   trControl = cvCtrl)

rpart.plot(rpartTune2$finalModel)
rpartTune2$results


View(train_data_2)

#WEKA feature importance ==========================================================================================================================================================================
WEKA_Feature_Importance_Summary <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Report/Feature Importance Summary.csv")
View(WEKA_Feature_Importance_Summary)
library("ggVennDiagram")

feature_importance_list <- list( Relief = WEKA_Feature_Importance_Summary$Relief, 
                                 Cor = WEKA_Feature_Importance_Summary$Correlation, 
                                 InfoGain=WEKA_Feature_Importance_Summary$InfoGain, 
                                 CFS=WEKA_Feature_Importance_Summary$CFS_Subset)
ggVennDiagram(feature_importance_list, label_alpha = 0) + scale_fill_gradient(low="white", high = "blue")

x1 <- intersect(WEKA_Feature_Importance_Summary$Relief,  
                WEKA_Feature_Importance_Summary$Correlation)
x2 <- intersect(WEKA_Feature_Importance_Summary$InfoGain, 
                WEKA_Feature_Importance_Summary$CFS_Subset)

common_features <- intersect(x1, x2)
length(common_features)
View(common_features)


#Undersampling major class for WEKA ==========================================
WEKA_26commonFtr <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MyProcessedData/Final_Final_ET_Merged_subset_with_diagnosis_in_FinalLWR_ThreeET_test-4WEKA_26commonFtr.csv")
View(WEKA_26commonFtr)
table(WEKA_26commonFtr$ASD_nonASD_classLabel)


WEKA_26commonFtr_ASD_Subset <- subset(WEKA_26commonFtr, WEKA_26commonFtr$ASD_nonASD_classLabel == "ASD" )
WEKA_26commonFtr_nonASD_Subset <- subset(WEKA_26commonFtr, WEKA_26commonFtr$ASD_nonASD_classLabel == "nonASD" )
dim(WEKA_26commonFtr_ASD_Subset)#332  27
dim(WEKA_26commonFtr_nonASD_Subset)#169  27

#geberating random subset of ASD subjects 
WEKA_26commonFtr_balancedDataSet_list <- list()


View(index_vector)
number_of_balanced_dataSet4WEKA <- 10

for (i in c(1:number_of_balanced_dataSet4WEKA)) 
{
  index_vector <- sample(c(1:332),size = 169)
  tempDataSet<- rbind(WEKA_26commonFtr_ASD_Subset[index_vector,], 
        WEKA_26commonFtr_nonASD_Subset)
  file_path = paste("/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MyProcessedData/", as.character(i), "balanced_dataSet4WEKA.csv", sep = "")
  write.csv(tempDataSet, file = file_path)
}

