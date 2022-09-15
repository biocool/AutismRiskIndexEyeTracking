#check age effect and ethnicity ...

read_data_all_five_ET <- function()
{
  all_five_test <- 
    read.csv("/Users/apple/Desktop/Eric/Research/Karen/Classifier/Data/MostUpdated/09-10-2021-Update on Missing Fixations for Tobii Project 1-3/FiveET_Test/ProcessedUsingR/ReadyForR/All_ET_Combination/12345_weka.csv", 
             row.names = 1)
  #all_five_test[all_five_test=="?"] <- NA
  all_five_test[,c(1:52)] <- apply(all_five_test[,c(1:52)],2,as.numeric)
  all_five_test$recentDxJ_dxCode <- 
    as.factor(all_five_test$recentDxJ_dxCode)
  return(all_five_test)
}

#percent fixation L vs R===============
all_five_test <- read_data_all_five_ET()
dim(all_five_test)
View(all_five_test)

all_five_test$subID <- rownames(all_five_test)

geoRmean <- rowMeans(cbind(all_five_test$ET1_FixationGeo,#R
                           all_five_test$ET2_FixationGeo,#R
                           all_five_test$ET7_FixationGeo#R
))

geoLmean <- rowMeans(cbind(all_five_test$ET3_FixationGeo, #L
                           all_five_test$ET8_FixationGeo#L
)
)


socLmean <- rowMeans(cbind(100-all_five_test$ET1_FixationGeo,#R
                           100-all_five_test$ET2_FixationGeo,#R
                           100-all_five_test$ET7_FixationGeo#R
))

socRmean <- rowMeans(cbind(100-all_five_test$ET3_FixationGeo, #L
                           100-all_five_test$ET8_FixationGeo#L
)
)



t.test(socLmean, geoLmean)
#p-value < 2.2e-16
t.test(socRmean, geoRmean)
#p-value < 2.2e-16
t.test(socLmean, geoLmean, paired = TRUE)
#p-value < 2.2e-16


t.test(geoRmean, geoLmean)
#Welch Two Sample t-test
#data:  geoRmean and geoLmean
#t = -2.7821, df = 722.18, p-value = 0.005541
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -7.445427 -1.284809
#sample estimates:
#  mean of x mean of y 
#35.20453  39.56964 

t.test(geoRmean, geoLmean, paired = TRUE)
#	Paired t-test

#data:  geoRmean and geoLmean
#t = -5.2459, df = 362, p-value = 2.652e-07
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -6.001463 -2.728773
#sample estimates:
#  mean of the differences 
#-4.365118 

diffR.L <- geoRmean - geoLmean


pcntFixationDiffAnalysis.datFrame <- 
  cbind(geoRmean, geoLmean, socRmean, socLmean,
      diffR.L, 
      recentDxJ_dxCode = all_five_test$recentDxJ_dxCode)

View(pcntFixationDiffAnalysis.datFrame)
pcntFixationDiffAnalysis.datFrame <- as.data.frame(pcntFixationDiffAnalysis.datFrame)
pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode[pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode == "1"]<- "ASD"
pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode[pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode == "2"]<- "nonASD"

table(pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode)

library(ggplot2)
p <- ggplot(pcntFixationDiffAnalysis.datFrame, aes(y = geoRmean , x = recentDxJ_dxCode, fill =  recentDxJ_dxCode))
p+geom_violin() + geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.2, notch = TRUE) 


p <- ggplot(pcntFixationDiffAnalysis.datFrame, aes(y = geoLmean , x = recentDxJ_dxCode, fill =  recentDxJ_dxCode))
p+geom_violin() + geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.2, notch = TRUE) 


p <- ggplot(pcntFixationDiffAnalysis.datFrame, aes(y = diffR.L , x = recentDxJ_dxCode, fill =  recentDxJ_dxCode))
p+geom_violin() + geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.2, notch = TRUE) 

#preparing for a comprehensive visualization 



#c(pcntFixationDiffAnalysis.datFrame$geoRmean, pcntFixationDiffAnalysis.datFrame$geoLmean, pcntFixationDiffAnalysis.datFrame$diffR.L)
Pcnt.Fix.Geo.R.L <- c(pcntFixationDiffAnalysis.datFrame$geoRmean, pcntFixationDiffAnalysis.datFrame$geoLmean, 
                      pcntFixationDiffAnalysis.datFrame$socRmean,  pcntFixationDiffAnalysis.datFrame$socLmean)  
  
Category <- c(element.Wise.pasting(vctr = pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode, "Geo.R.Mean.PcntFix." ), 
element.Wise.pasting(vctr = pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode, "Geo.L.Mean.PcntFix." ),
element.Wise.pasting(vctr = pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode, "Soc.R.Mean.PcntFix." ),
element.Wise.pasting(vctr = pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode, "Soc.L.Mean.PcntFix." )
)


Djx <- c(c(pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode, pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode),
         c(pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode, pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode))

L.vs.R <- c(rep("Right", length( pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode)),
            rep("Left", length( pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode)),
            rep("Right", length( pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode)),
            rep("Left", length( pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode))
            )

Soc.Vs.Geo <- c(rep("Geo", length( pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode)),
                          rep("Geo", length( pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode)),
                          rep("Soc", length( pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode)),
                          rep("Soc", length( pcntFixationDiffAnalysis.datFrame$recentDxJ_dxCode))
)



R.L.Pref.4Viz.data.frame <- as.data.frame(cbind(Pcnt.Fix.Geo.R.L, Category, L.vs.R, Soc.Vs.Geo, Djx))
R.L.Pref.4Viz.data.frame$Pcnt.Fix.Geo.R.L <- as.numeric(R.L.Pref.4Viz.data.frame$Pcnt.Fix.Geo.R.L)
R.L.Pref.4Viz.data.frame$Djx[R.L.Pref.4Viz.data.frame$Djx == "1"]<- "ASD"
R.L.Pref.4Viz.data.frame$Djx[R.L.Pref.4Viz.data.frame$Djx == "2"]<- "nonASD"

View(R.L.Pref.4Viz.data.frame)

p <- ggplot(R.L.Pref.4Viz.data.frame, 
             aes(x = Category, y = Pcnt.Fix.Geo.R.L, fill = Djx))
pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/LeftRightPref/Soc.vs.Geo.Total.pdf")
p+geom_violin() + geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.2, notch = TRUE) 
dev.off()

pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/LeftRightPref/AllCategory.pdf")
p <- ggplot(R.L.Pref.4Viz.data.frame, R.L.Pref.4Viz.data.frame, 
            mapping =  aes(x = Djx, y = Pcnt.Fix.Geo.R.L, fill = Category))
p+geom_violin() + 
  geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.4, notch = TRUE) 
dev.off()


pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/LeftRightPref/AllCategoryBoxplot.pdf")

p +   geom_boxplot(alpha=0.8,  notch = TRUE) #+ geom_point( size = 1, shape = 21, position = position_jitterdodge()) 
dev.off()




pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/LeftRightPref/Left_vs_Right.pdf")
p <- ggplot(R.L.Pref.4Viz.data.frame, 
            mapping =  aes(x = L.vs.R, y = Pcnt.Fix.Geo.R.L, fill = L.vs.R))

p+geom_violin() + geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.4, notch = TRUE) 
dev.off()


pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/LeftRightPref/Left_vs_Right_ASD_nonASD.pdf")
p <- ggplot(R.L.Pref.4Viz.data.frame, 
            mapping =  aes(x = Soc.Vs.Geo, y = Pcnt.Fix.Geo.R.L, fill = L.vs.R))

p+geom_violin() + geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.4, notch = TRUE) 
dev.off()

p <- ggplot(R.L.Pref.4Viz.data.frame, 
            mapping =  aes(x = Soc.Vs.Geo, y = Pcnt.Fix.Geo.R.L, fill = Soc.Vs.Geo))

p+geom_violin() + geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.4, notch = TRUE) 


p <- ggplot(R.L.Pref.4Viz.data.frame, 
            mapping =  aes(x = Djx, y = Pcnt.Fix.Geo.R.L, fill = L.vs.R))

p+geom_violin() + geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.4, notch = TRUE) 


#Male vs Female

library(stats)
library(dplyr)
library(rstatix)

stat.test <- R.L.Pref.4Viz.data.frame %>%
  pairwise_t_test(Pcnt.Fix.Geo.R.L ~ Category)
View(stat.test)
hist(diffR.L)


stat.test2 <- R.L.Pref.4Viz.data.frame %>%
  pairwise_t_test(Pcnt.Fix.Geo.R.L ~ Category)
View(stat.test2)
write.csv(stat.test2, file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/LeftRightPref/stat.test2.csv")

#Checking for gender bias ===============
lwrGende_subjectID <- cbind(gender = sara_cleaned_lwr$gender, subjectid = sara_cleaned_lwr$subjectid)
lwrGende_subjectID <- as.data.frame(lwrGende_subjectID)

View(all_five_test)
merge(all_five_test)

all_five_test_withGender <- merge(x = all_five_test, y = lwrGende_subjectID, 
      by.x = "subID", by.y = "subjectid")

length(unique(all_five_test$subID))


R.L.Pref.4Viz.data.frame$gender <- c(all_five_test_withGender$gender,
                                     all_five_test_withGender$gender,
                                     all_five_test_withGender$gender,
                                     all_five_test_withGender$gender)

dim(R.L.Pref.4Viz.data.frame)

pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/LeftRightPref/Left_vs_Right_Gender.pdf")

p <- ggplot(R.L.Pref.4Viz.data.frame, 
            mapping =  aes(x = L.vs.R, y = Pcnt.Fix.Geo.R.L, fill = gender))

p+geom_violin() + geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.4, notch = TRUE) 
dev.off()

#che3cking for age effect======================================

lwrage_subjectID <- cbind(Age = sara_cleaned_lwr$ados_ageMo_1, subjectid = sara_cleaned_lwr$subjectid)
lwrage_subjectID <- as.data.frame(lwrage_subjectID)
lwrage_subjectID$Age <- as.numeric(lwrage_subjectID$Age)
#imnputing those ages that are NULL or 0
lwrage_subjectID$Age[is.na(lwrage_subjectID$Age)|lwrage_subjectID$Age==0] <- mean(na.omit(lwrage_subjectID$Age))


all_five_test_withAge <- merge(x = all_five_test_withGender, y = lwrage_subjectID, 
                                  by.x = "subID", by.y = "subjectid")

length(unique(all_five_test_withAge$Age.x))


R.L.Pref.4Viz.data.frame$Age <- c(all_five_test_withAge$Age.x,
                                     all_five_test_withAge$Age.x,
                                     all_five_test_withAge$Age.x,
                                     all_five_test_withAge$Age.x)
hist(R.L.Pref.4Viz.data.frame$Age)
range(R.L.Pref.4Viz.data.frame$Age)

R.L.Pref.4Viz.data.frame$AgeBin <- rep(NA, length(R.L.Pref.4Viz.data.frame$Age))

#R.L.Pref.4Viz.data.frame$AgeBin[R.L.Pref.4Viz.data.frame$Age<=12] <- "0-12"
R.L.Pref.4Viz.data.frame$AgeBin[ R.L.Pref.4Viz.data.frame$Age<=24] <- "0-24"
R.L.Pref.4Viz.data.frame$AgeBin[R.L.Pref.4Viz.data.frame$Age>24] <- "24-45"
table(R.L.Pref.4Viz.data.frame$AgeBin)


library(ggplot2)
pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/LeftRightPref/Left_vs_Right_AgeBin.pdf")
p <- ggplot(R.L.Pref.4Viz.data.frame, aes(x = AgeBin, y = Pcnt.Fix.Geo.R.L, fill =  L.vs.R)) 
p + geom_violin() + geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.4, notch = TRUE) 
dev.off()


MoreDetailedCategory <- paste(R.L.Pref.4Viz.data.frame$Category, R.L.Pref.4Viz.data.frame$AgeBin)
R.L.Pref.4Viz.data.frame$MoreDetailedCategory <- MoreDetailedCategory


pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/LeftRightPref/Left_vs_Right_AllCategories.pdf")
p <- ggplot(R.L.Pref.4Viz.data.frame, aes(x = MoreDetailedCategory, y = Pcnt.Fix.Geo.R.L, fill =  MoreDetailedCategory)) 
p + geom_violin() + geom_point( size = 1, shape = 21, position = position_jitterdodge()) + geom_boxplot(alpha=0.8, width=0.4, notch = TRUE) 
dev.off()

#!!! check whether the ages are correct or not!
View()

write.csv(R.L.Pref.4Viz.data.frame, 
          file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/LeftRightPref/R.L.Pref.4Viz.data.frame.csv")


stat.test3 <- R.L.Pref.4Viz.data.frame %>%
  pairwise_t_test(Pcnt.Fix.Geo.R.L ~ MoreDetailedCategory)
View(stat.test3)
write.csv(stat.test3, file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/LeftRightPref/stat.test3.csv")


#same for saccads per second ===========