#in this script I'm goging to find some questions that are discriminating ASD vs nonASD (or ASD/Delyaed/TD)
#last update 03.30.2022

#some strange things:
#1- in the first question in eCSBS we don't have any 1 it starts from 2....
library(ComplexHeatmap)
library(circlize)
library(ggplot2)
library(corrplot)
library(vioplot)
library(ggpubr)
library(pheatmap)
library(ggstatsplot)
#I'm goging to save the results in the following folder 
setwd("/Users/apple/Desktop/Eric/Research/Karen/Classifier/CSBS/Results/")
########################################################################################################################################################################
#reading the source data files=======
eCSBS <- 
  read.csv("/Users/apple/Desktop/Eric/Research/Karen/IntegratingData4ML/CSBS/eCSBS/eCSBS_Mapped_Subjects_05122021.csv")
pCSBS <- 
  read.csv("/Users/apple/Desktop/Eric/Research/Karen/IntegratingData4ML/CSBS/pCSBS/pCSBS_Mapped_Subjects_05102021.csv")
LWR_for_CSBS <- 
  read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/IntegratingData4ML/FromSrinvasa/LWReport_01312021.csv")

dim(eCSBS)#877  57
dim(pCSBS)#2448   49
#View(eCSBS)#starts from 1
#View(pCSBS)#starts from 0
#NOTE: we should 

########################################################################################################################################################################
#computing the indicies#####
#which should be kept from each dataframe and extracting the subsets with useful information

#computing the indicies that show important feature in each file 
#pCSBS
pCSBC.index_vector <- c(which(colnames(pCSBS) == "SubjectId"), 
                        which(colnames(pCSBS) == "ChildName"),
                        which(colnames(pCSBS) == "AgeMo"),
                        which(colnames(pCSBS) == "Gender"),
                        c( which(colnames(pCSBS) == "Q01.KnowEmot"):which(colnames(pCSBS) == "Concerns"))
)
pCSBS_Subset <- 
  pCSBS[,pCSBC.index_vector]
#View(pCSBS_Subset)
dim(pCSBS_Subset)#2448   29

#the same for eCSBS
eCSBS.index_vector1 <- c(which(colnames(eCSBS) == "SubjectId"), 
                         which(colnames(eCSBS) == "subject_fullname"),
                         which(colnames(eCSBS) == "AgeMo"),
                         which(colnames(eCSBS) == "Gender"),
                         c( which(colnames(eCSBS) == "Q1_answer_code"):which(colnames(eCSBS) == "concerns"))
)

eCSBS_Subset<- eCSBS[,eCSBS.index_vector1]
#View(eCSBS_Subset)
dim(eCSBS_Subset)#877  29
length(intersect(eCSBS_Subset$SubjectId, pCSBS_Subset$SubjectId))#151
length(eCSBS_Subset$SubjectId)#877
length(unique(eCSBS_Subset$SubjectId))#481
length(pCSBS_Subset$SubjectId)#2448
length(unique(pCSBS_Subset$SubjectId))#1727

class(pCSBS_Subset$AgeMo)

pCSBS_Subset$AgeMo <- 
  as.numeric(pCSBS_Subset$AgeMo)#there are some NA values
#View(pCSBS_Subset$AgeMo)
eCSBS_Subset$AgeMo <- 
  as.numeric(eCSBS_Subset$AgeMo)
########################################################################################################################################################################
#Unique & Converting to numerical========== 

pCSBS_Subset <- 
  select.only.first.visit(data = pCSBS_Subset, 
                          index_for_unique_selection = 1, 
                          index_to_sort_based_on = 3)
# [1] "No. of removed records:"
# [1] 721
# [1] "Shrinkage percent: "
# [1] 0.7054739
dim(pCSBS_Subset)#1727   29

#converting those 24 first questions (excpet concern) to numeric
questions.cols.index.vctr.pCSBS_Subsets <- 
  c( which(colnames(pCSBS_Subset) == "Q01.KnowEmot"):(which(colnames(pCSBS_Subset) == "Concerns")-1))
pCSBS_Subset[,questions.cols.index.vctr.pCSBS_Subsets] <- 
  apply(pCSBS_Subset[,questions.cols.index.vctr.pCSBS_Subsets], 2, as.numeric)
typeof(pCSBS_Subset$Q23.BlocksCount)#double
#View(eCSBS_Subset)

eCSBS_Subset <- 
  select.only.first.visit(data = eCSBS_Subset, 
                          index_for_unique_selection = 1, 
                          index_to_sort_based_on = 3)
# [1] "No. of removed records:"
# [1] 396
# [1] "Shrinkage percent: "
# [1] 0.5484607
dim(eCSBS_Subset)#481  29

questions.cols.index.vctr.eCSBS_Subsets <- 
  c( which(colnames(eCSBS) == "Q1_answer_code"):(which(colnames(eCSBS) == "concerns")-1))
eCSBS_Subset[,questions.cols.index.vctr.pCSBS_Subsets] <- 
  apply(eCSBS_Subset[,questions.cols.index.vctr.pCSBS_Subsets], 2, as.numeric)
typeof(pCSBS_Subset$Q16.ConsCount)#"double"

#subtracting one from eCSBS_Subset elements to be in the same range with pCSBS
eCSBS_Subset[,questions.cols.index.vctr.pCSBS_Subsets] <- 
  eCSBS_Subset[,questions.cols.index.vctr.pCSBS_Subsets] - 1
View(eCSBS_Subset[,questions.cols.index.vctr.pCSBS_Subsets])

########################################################################################################################################################################
#merging eCSBS abd pSCBS========
length(intersect(eCSBS_Subset$SubjectId, pCSBS_Subset$SubjectId))#151

colnames(eCSBS_Subset) <- 
  colnames(pCSBS_Subset)

ep_CSBS_Merged <- 
  rbind(eCSBS_Subset, pCSBS_Subset)
dim(ep_CSBS_Merged)#2208   29
#View(ep_CSBS_Merged)

#View(eCSBS_Subset)
#View(pCSBS_Subset)

#deleting redundant subjects in merged CSBS
ep_CSBS_Merged <- 
  select.only.first.visit(data = ep_CSBS_Merged, index_for_unique_selection = 1, index_to_sort_based_on = 3)
# [1] "No. of removed records:"
# [1] 151
# [1] "Shrinkage percent: "
# [1] 0.9316123

dim(ep_CSBS_Merged)#2057   29

#adding  DJX to the merged CSBS ==============

#replacing all "NULL" to NA
ep_CSBS_Merged[ep_CSBS_Merged == "NULL"] <- NA
ep_CSBS_Merged[ep_CSBS_Merged == "-"] <- NA
ep_CSBS_Merged[ep_CSBS_Merged == ""] <- NA
View(ep_CSBS_Merged)

ep_CSBS_Merged <- 
  ep_CSBS_Merged[complete.cases(ep_CSBS_Merged[ , ]) ,]
dim(ep_CSBS_Merged) #1898   29 #NEW: 1761   29
sum(is.na(ep_CSBS_Merged))#0
View(ep_CSBS_Merged)

ep_CSBS_Merged_withOtherClinicalInfo <- merge(x= LWR_for_CSBS, 
                                              y= ep_CSBS_Merged, by.x = "ados_ados_subjectid", by.y = "SubjectId")

dim(ep_CSBS_Merged_withOtherClinicalInfo) #1728  507  #NEW: 1601  507
#View(ep_CSBS_Merged_withOtherClinicalInfo)

# extracting a subset of ep_CSBS_Merged_withOtherClinicalInfo for doing statistical analysis==============
which(colnames(ep_CSBS_Merged_withOtherClinicalInfo) == "AgeMo") #481
which(colnames(ep_CSBS_Merged_withOtherClinicalInfo) == "recentDxJ_dxCode") #403
which(colnames(ep_CSBS_Merged_withOtherClinicalInfo) == "subjectid")#2
which(colnames(ep_CSBS_Merged_withOtherClinicalInfo) == "ados_RRTot_1") #242
which(colnames(ep_CSBS_Merged_withOtherClinicalInfo) == "ados_CoSoTot_1") #241
which(colnames(ep_CSBS_Merged_withOtherClinicalInfo) == "ados_CoSoTotRRTot_1")#243 
which(colnames(ep_CSBS_Merged_withOtherClinicalInfo) == "weeks_Pregnancy")

View(ep_CSBS_Merged_withOtherClinicalInfo)
#save(ep_CSBS_Merged_withOtherClinicalInfo, file = "/Users/apple/Desktop/Eric/Research/Karen/Classifier/CSBS/ep_CSBS_Merged_withOtherClinicalInfo")
#cols 483:507 are CSBS questions (25 questions )
ep_CSBS_Merged_4_numericalStatistical_analysis <- 
  ep_CSBS_Merged_withOtherClinicalInfo[,c(2, 403,482,481, 241:243,  483:507)]
dim(ep_CSBS_Merged_4_numericalStatistical_analysis)#1728   32 #NEW:1601   32
View(ep_CSBS_Merged_4_numericalStatistical_analysis)  

#converitng the concern to 0/1=========
table(ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns)
# N  No   Y Yes 
# 626 256 685 161 
ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns[
  ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns == "No"] <- "N"
ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns[
  ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns == "Yes"] <- "Y"
table(ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns)
# N   Y 
# 882 846 
#New
# N   Y 
# 812 789 

ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns[
  ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns == "N"] <- 0

ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns[
  ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns == "Y"] <- 1

typeof(ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns)#"character"
ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns <- 
  as.numeric(ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns)
typeof(ep_CSBS_Merged_4_numericalStatistical_analysis$Concerns)#"double"
########################################################################################################################################################################
#correcting DxJ in such a way tha have only two categories ASD and nonASD==============
table(ep_CSBS_Merged_4_numericalStatistical_analysis$recentDxJ_dxCode)  
# ADHD                                                ASD 
# 4                                                629 
# ASD Features                                      ASD Features  
# 83                                                  1 
# DD                                            Dropped 
# 96                                                  2 
# DROPPED                                                FMD 
# 1                                                 18 
# GDD                                                 LD 
# 24                                                215 
# MD                                               NULL 
# 12                                                  3 
# Other                                         PrevASDTyp 
# 124                                                  1 
# PrevDDTyp                                         PrevLDDTyp 
# 7                                                 47 
# TD Tests typical; see diagnostic comments re: concern 
# 406                                                  1 
# Typ Sib ASD 
# 54 


#Delay
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="ASD Features"] <- "ASD_Features"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="ASD Features  "] <- "ASD_Features"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="ASD Features "] <- "ASD_Features"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="DD"] <- "Delay"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="GDD"] <- "Delay"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="MD"] <- "Delay"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="Other"] <- "Delay"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="ADHD"] <- "Delay"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="FMD"] <- "Delay"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="LD"] <- "Delay"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="ADHD"] <- "Delay"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="FMD"] <- "Delay"
#TD
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="PrevDDTyp"] <- "TD"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="Typ Sib ASD"] <- "TD"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="PrevASDTyp"] <- "TD"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="PrevLDDTyp"] <- "TD"
ep_CSBS_Merged_4_numericalStatistical_analysis[
  ep_CSBS_Merged_4_numericalStatistical_analysis=="Tests typical; see diagnostic comments re: concern"] <- "TD"

table(ep_CSBS_Merged_4_numericalStatistical_analysis$recentDxJ_dxCode)  
# ASD ASD_Features        Delay      Dropped      DROPPED         NULL           TD 
# 629           84          493            2            1            3          516 

ep_CSBS_Merged_4_numericalStatistical_analysis[ep_CSBS_Merged_4_numericalStatistical_analysis=="NULL"] <- NA
ep_CSBS_Merged_4_numericalStatistical_analysis[ep_CSBS_Merged_4_numericalStatistical_analysis=="Dropped"] <- NA
ep_CSBS_Merged_4_numericalStatistical_analysis[ep_CSBS_Merged_4_numericalStatistical_analysis=="DROPPED"] <- NA

ep_CSBS_Merged_4_numericalStatistical_analysis <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis[
    complete.cases(ep_CSBS_Merged_4_numericalStatistical_analysis),]
dim(ep_CSBS_Merged_4_numericalStatistical_analysis)#1722   32 #NEW:1595   32
table(ep_CSBS_Merged_4_numericalStatistical_analysis$recentDxJ_dxCode) 
ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022<- 
  ep_CSBS_Merged_4_numericalStatistical_analysis

table(ep_CSBS_Merged_4_numericalStatistical_analysis$recentDxJ_dxCode)  
# ASD ASD_Features        Delay           TD 
# 629           84          493          516 
save(ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022,
     file = "ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022")

ep_CSBS_Merged_4_numericalStatistical_analysis$recentDxJ_dxCode[
  ep_CSBS_Merged_4_numericalStatistical_analysis$recentDxJ_dxCode == "ASD_Features"] <-"Delay"
table(ep_CSBS_Merged_4_numericalStatistical_analysis$recentDxJ_dxCode)  
# ASD Delay    TD 
# 629   577   516

ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V2.ASDF.as.Delay <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis
save(ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V2.ASDF.as.Delay, 
     file = "ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V2.ASDF.as.Delay")
View(ep_CSBS_Merged_4_numericalStatistical_analysis)
apply(ep_CSBS_Merged_4_numericalStatistical_analysis, 2, typeof)
########################################################################################################################################################################
#correcting geneder field=========

table(ep_CSBS_Merged_4_numericalStatistical_analysis$Gender)
# 135          f          F F (assigne     Female          m          M       male       Male 
#   1         11        456          1          3         29       1199          5         17 

ep_CSBS_Merged_4_numericalStatistical_analysis$Gender[
  ep_CSBS_Merged_4_numericalStatistical_analysis$Gender =="Male"] <- "M"
ep_CSBS_Merged_4_numericalStatistical_analysis$Gender[
  ep_CSBS_Merged_4_numericalStatistical_analysis$Gender =="male"] <- "M"
ep_CSBS_Merged_4_numericalStatistical_analysis$Gender[
  ep_CSBS_Merged_4_numericalStatistical_analysis$Gender =="m"] <- "M"
ep_CSBS_Merged_4_numericalStatistical_analysis$Gender[
  ep_CSBS_Merged_4_numericalStatistical_analysis$Gender =="Female"] <- "F"
ep_CSBS_Merged_4_numericalStatistical_analysis$Gender[
  ep_CSBS_Merged_4_numericalStatistical_analysis$Gender =="f"] <- "F"
ep_CSBS_Merged_4_numericalStatistical_analysis$Gender[
  ep_CSBS_Merged_4_numericalStatistical_analysis$Gender =="F (assigne"] <- "F"
ep_CSBS_Merged_4_numericalStatistical_analysis$Gender[
  ep_CSBS_Merged_4_numericalStatistical_analysis$Gender =="135"] <- NA
table(ep_CSBS_Merged_4_numericalStatistical_analysis$Gender)
# F    M 
# 471 1250 

#Removing the element with NA as the gender
dim(ep_CSBS_Merged_4_numericalStatistical_analysis)#1722   32 #NEW:1595   32 
ep_CSBS_Merged_4_numericalStatistical_analysis <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis[
    complete.cases(ep_CSBS_Merged_4_numericalStatistical_analysis),
    ]
dim(ep_CSBS_Merged_4_numericalStatistical_analysis)#1721   32 #NEW:1594   32
ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V3.ASDF.as.Delay.GenderCorrected <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis
save(ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V3.ASDF.as.Delay.GenderCorrected,
     file = "ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V3.ASDF.as.Delay.GenderCorrected")
load("./ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V3.ASDF.as.Delay.GenderCorrected")
write.csv(ep_CSBS_Merged_4_numericalStatistical_analysis,
          file = "ep_CSBS_Merged_4_numericalStatistical_analysis.csv")
dim(ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V3.ASDF.as.Delay.GenderCorrected)#1594   32
########################################################################################################################################################################
#Functions for correlation=====
#computing the correlation between 25 CSBS question and three ADOS scores
compute_corr <- function(x, y)
{
  res <- cor(as.numeric(x), as.numeric(y))
  return(res)
}

#in this function it assumed that the indices for the 25 CSBS questions is 5+7:29+7
ADOS_CSBS_Cor<- function(input_data)
{
  cor_CSBS_with_ados <- c()
  for (i in c(1:25)) 
  {
    #browser()
    cor_with_ados_CoSoTot <- compute_corr(input_data[,i+7],input_data$ados_CoSoTot_1)
    cor_with_ados_RRTot <- compute_corr(input_data[,i+7],input_data$ados_RRTot_1)
    cor_with_ados_CoSoTotRRTot <- compute_corr(input_data[,i+7],input_data$ados_CoSoTotRRTot_1)
    current_cor_vector <- c(cor_with_ados_CoSoTot, 
                            cor_with_ados_RRTot,
                            cor_with_ados_CoSoTotRRTot)
    cor_CSBS_with_ados <- rbind(cor_CSBS_with_ados, current_cor_vector )
  }
  colnames(cor_CSBS_with_ados) <- 
    c("cor_with_ados_CoSoTot", "cor_with_ados_RRTot", "cor_with_ados_CoSoTotRRTot")
  rownames(cor_CSBS_with_ados) <- 
    colnames(ep_CSBS_Merged_4_numericalStatistical_analysis)[8:32]
  return(cor_CSBS_with_ados)
  
}
########################################################################################################################################################################
#Functions for ggplot====== 

#the below function would be used for adding N to each group 
give.n <-function(x)
{return(c(y=median(x)*0.8, label= length(x)))}# experiment with the multiplier to find the perfect position



########################################################################################################################################################################
#statistical analysis########
#checking the age diff between gender groups and DJX groups

#1- gender group####
#I'll do this in two ways: using more simple way; and more smart way using ggplot2

#more simple way====
#extracting ages for male and female 
ep_CSBS_Merged.male.age.vctr <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis$AgeMo[ep_CSBS_Merged_4_numericalStatistical_analysis$Gender == "M"]
ep_CSBS_Merged.female.age.vctr <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis$AgeMo[ep_CSBS_Merged_4_numericalStatistical_analysis$Gender != "M"]

#double checking
length(ep_CSBS_Merged.male.age.vctr)#1152
length(ep_CSBS_Merged.female.age.vctr)#442
table(ep_CSBS_Merged_4_numericalStatistical_analysis$Gender)
# F    M 
# 442 1152 

#testing diff in M/F ages
t.test(ep_CSBS_Merged.male.age.vctr, 
       ep_CSBS_Merged.female.age.vctr)
#t = 0.37367, df = 787.26, p-value = 0.7088

#extracting ages for male and female 
table(ep_CSBS_Merged_4_numericalStatistical_analysis$recentDxJ_dxCode)
# ASD Delay    TD 
# 581   524   489 

ep_CSBS_Merged.male.age.vctr <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis$AgeMo[
    ep_CSBS_Merged_4_numericalStatistical_analysis$recentDxJ_dxCode == "ASD"]
ep_CSBS_Merged.female.age.vctr <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis$AgeMo[
    ep_CSBS_Merged_4_numericalStatistical_analysis$Gender == "Delay"]
ep_CSBS_Merged.female.age.vctr <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis$AgeMo[
    ep_CSBS_Merged_4_numericalStatistical_analysis$Gender == "TD"]

#double checking
length(ep_CSBS_Merged.male.age.vctr)#1152
length(ep_CSBS_Merged.female.age.vctr)#442
table(ep_CSBS_Merged_4_numericalStatistical_analysis$Gender)
# F    M 
# 442 1152 

#testing diff in M/F ages
t.test(ep_CSBS_Merged.male.age.vctr, 
       ep_CSBS_Merged.female.age.vctr)
#t = 0.37367, df = 787.26, p-value = 0.7088

#more smart way======
pdf("Testing Age Difference in Gender Groups.pdf")
p <- 
  ggplot(ep_CSBS_Merged_4_numericalStatistical_analysis, 
         aes(y=AgeMo, x= Gender, fill=Gender)) +
  #geom_point( size = 1, shape = 21, position = position_jitterdodge())+
  geom_violin(alpha=0.9) + 
  xlab(paste("Quesion",as.character(i), sep = "")) + 
  stat_summary(fun.y = mean, geom="point",colour="blue", size=2)+
  stat_summary(fun.y = median, geom="point",colour="red", size=2)+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median)+
  ylab("Values")  + stat_compare_means(comparisons = list(c("M","F")), method = "t.test") + 
  scale_fill_manual(values=c("pink","skyblue"))

p + ggtitle("Testing Age Difference in Gender Groups \n(based on bubjects with CSBS data)") + #justifing to center
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#2- DJX group####
#I'm doing just using the smart way
my_comparisons <- 
  combn(c("ASD", "Delay", "TD"), 2, FUN = NULL, simplify = FALSE)
pdf("AgeDiff.DJX.Group.pdf")
p <- 
  ggplot(ep_CSBS_Merged_4_numericalStatistical_analysis, 
         aes(y=AgeMo, x= recentDxJ_dxCode, fill=recentDxJ_dxCode)) +
  #geom_point( size = 1, shape = 21, position = position_jitterdodge())+
  geom_violin(alpha=0.6) + 
  xlab(paste("DjxGroups")) + 
  stat_summary(fun.y = mean, geom="point",colour="blue", size=2)+
  stat_summary(fun.y = median, geom="point",colour="red", size=2)+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median)+
  ylab("Age (Months)")  + stat_compare_means(comparisons = my_comparisons, method = "t.test") + 
  scale_fill_manual(values=c("red","orange","green")) 

p + ggtitle("Testing Age Difference in DJX Groups \n(based on bubjects with CSBS data)") + #justifing to center
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


#correlation between three ados scores and all 25 questions======
cor_CSBS_with_ados_male_Female <- 
  ADOS_CSBS_Cor(input_data = ep_CSBS_Merged_4_numericalStatistical_analysis)
View(cor_CSBS_with_ados_male_Female)

cor_CSBS_with_ados_Male <- 
  ADOS_CSBS_Cor(subset(ep_CSBS_Merged_4_numericalStatistical_analysis, 
                       ep_CSBS_Merged_4_numericalStatistical_analysis$Gender == "M"))
cor_CSBS_with_ados_Female <- 
  ADOS_CSBS_Cor(subset(ep_CSBS_Merged_4_numericalStatistical_analysis, 
                       ep_CSBS_Merged_4_numericalStatistical_analysis$Gender == "F"))

View(cor_CSBS_with_ados_male_Female)

range(cor_CSBS_with_ados_male_Female) #-0.216  0.270 #NEW: -0.3164327  0.2539894
range(cor_CSBS_with_ados_Male) #-0.216  0.285 #NEW: -0.2981469  0.2251502
range(cor_CSBS_with_ados_Female) #-0.206  0.243 #NEW: -0.3535438  0.3141017
heatmap(as.matrix(cor_CSBS_with_ados_male_Female))

#VIZ simple heat map======
library(pheatmap)

pdf("SimpleHeatMap.OnlyCorrWithADOS.M.and.F.pdf")
pheatmap(as.matrix(cor_CSBS_with_ados_male_Female), display_numbers = T, 
         cluster_rows = F, cluster_cols = F, fontsize_number = 12, 
         main = "CSBS Cor. with Three ADOS scores\nAll subjects (M&F)")
dev.off()

pdf("SimpleHeatMap.OnlyCorrWithADOS.Females.pdf")
pheatmap(as.matrix(cor_CSBS_with_ados_Female), display_numbers = T, 
         cluster_rows = F, cluster_cols = F, fontsize_number = 12, 
         main = "CSBS Cor. with Three ADOS scores\nFemales")
dev.off()

pdf("SimpleHeatMap.OnlyCorrWithADOS.Males.pdf")
pheatmap(as.matrix(cor_CSBS_with_ados_Male), display_numbers = T, 
         cluster_rows = F, cluster_cols = F, fontsize_number = 12, 
         main = "CSBS Cor. with Three ADOS scores\nMales")
dev.off()



#!!!HERE
#corplots and Complex Heatmap==========
View(ep_CSBS_Merged_4_numericalStatistical_analysis)
x <- 
  apply(ep_CSBS_Merged_4_numericalStatistical_analysis[,5:32], 2, as.numeric)
View(x)
cor_CSBS_with_ados_and_itself <- cor(x)
View(cor_CSBS_with_ados_and_itself)
# !report
pdf("CorPlot_CSBS_with_ados_and_itself.Pie.pdf")
corrplot::corrplot(cor_CSBS_with_ados_and_itself,method = 'pie', 
                   type="upper",  diag=FALSE, tl.cex = 0.5, tl.col = "black", tl.srt = 45)
dev.off()
pdf("CorPlot_CSBS_with_ados_and_itself.Number.pdf")
corrplot::corrplot(cor_CSBS_with_ados_and_itself,method = 'number', 
                   type="upper",  diag=FALSE, tl.cex = 0.5, tl.col = "black", tl.srt = 45, 
                   number.cex= 14/ncol(cor_CSBS_with_ados_and_itself))
dev.off()

library(ComplexHeatmap)
#two different way of viz: first is a clustergram and the second one is just a simple heatmap with values
pdf("Clustergram_CSBS_with_ados_and_itself.Number.pdf")
Heatmap(cor_CSBS_with_ados_and_itself, column_split = 5, row_split = 5)
dev.off()

pdf("SimpleHeatMap_CSBS_with_ados_and_itself.Number.pdf", height = 20, width = 15)
pheatmap(as.matrix(cor_CSBS_with_ados_and_itself), display_numbers = T, 
         cluster_rows = F, cluster_cols = F, fontsize_number = 10, 
         main = "Corr. HeatMap 25 CSBS Questions ")
dev.off()


#t.test for individual questions=====
#in this section I'm trying to do the t.test on the individual questions 
#columns 8 through 32 are individual questions in CSBS

my_comparisons <- 
  combn(c("ASD", "Delay", "TD"), 2, FUN = NULL, simplify = FALSE)
i <- 8

#Run the following chunck of code in a loop fashion 
file.name.4.writing <- 
  paste(colnames(ep_CSBS_Merged_4_numericalStatistical_analysis)[i],"CSBS.Question.T.test", ".pdf", sep = "")
pdf(file = file.name.4.writing)
ggplot(ep_CSBS_Merged_4_numericalStatistical_analysis, 
       aes(y=ep_CSBS_Merged_4_numericalStatistical_analysis[,i], x= recentDxJ_dxCode, fill=recentDxJ_dxCode)) +
  #geom_point( size = 1, shape = 21, position = position_jitterdodge())+
  geom_violin(alpha=0.6) + 
  xlab(paste("DjxGroups")) + 
  stat_summary(fun.y = mean, geom="point",colour="blue", size=2)+
  stat_summary(fun.y = median, geom="point",colour="red", size=2)+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median)+
  ylab(colnames(ep_CSBS_Merged_4_numericalStatistical_analysis)[i])+ 
  stat_compare_means(comparisons = my_comparisons, method = "t.test")+ 
  scale_fill_manual(values=c("red","orange","green"))
dev.off()
i <- i + 1

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
######DOING THE SAME ANALYSIS BUT JUST USING <=24 MONTHS OLD KIDS##########
########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
#only age <=24 =======
table(ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V3.ASDF.as.Delay.GenderCorrected$Gender)

ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month <- 
  subset(ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V3.ASDF.as.Delay.GenderCorrected, 
         (ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V3.ASDF.as.Delay.GenderCorrected$AgeMo <= 24 & 
            ep_CSBS_Merged_4_numericalStatistical_analysis.3.8.2022.V3.ASDF.as.Delay.GenderCorrected$AgeMo >= 12)
  )

dim(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month)
dim(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month)#1404   32 #1345   32

#START here:
#I should subsample ASD (or all groups) to be matched in terms of age with TD groups


#1- gender group####
#I'll do this in two ways: using more simple way; and more smart way using ggplot2
#more smart way======

pdf("Testing Age Difference in Gender Groups.pdf")
p <- 
  ggplot(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month, 
         aes(y=AgeMo, x= Gender, fill=Gender)) +
  #geom_point( size = 1, shape = 21, position = position_jitterdodge())+
  geom_violin(alpha=0.9) + 
  xlab(paste("Quesion",as.character(i), sep = "")) + 
  stat_summary(fun.y = mean, geom="point",colour="blue", size=2) + 
  stat_summary(fun.y = median, geom="point",colour="red", size=2) + 
  stat_summary(fun.data = give.n, geom = "text", fun.y = median) + 
  ylab("Values")  + stat_compare_means(comparisons = list(c("M","F")), method = "t.test") + 
  scale_fill_manual(values=c("pink","skyblue"))

p + ggtitle("Testing Age Difference in Gender Groups \n(based on bubjects with CSBS data)") + #justifing to center
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#2- DJX group####
#I'm doing just using the smart way
my_comparisons <- 
  combn(c("ASD", "Delay", "TD"), 2, FUN = NULL, simplify = FALSE)
pdf("AgeDiff.DJX.Group.pdf")
p <- 
  ggplot(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month, 
         aes(y=AgeMo, x= recentDxJ_dxCode, fill=recentDxJ_dxCode)) +
  #geom_point( size = 1, shape = 21, position = position_jitterdodge())+
  geom_violin(alpha=0.6) + 
  xlab(paste("DjxGroups")) + 
  stat_summary(fun.y = mean, geom="point",colour="blue", size=2)+
  stat_summary(fun.y = median, geom="point",colour="red", size=2)+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median)+
  ylab("Age (Months)")  + stat_compare_means(comparisons = my_comparisons, method = "t.test") + 
  scale_fill_manual(values=c("red","orange","green")) 

p + ggtitle("Testing Age Difference in DJX Groups \n(based on bubjects with CSBS data)") + #justifing to center
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

dim(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month)#1345   32
View(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month)
#correlation between three ados scores and all 25 questions======
cor_CSBS_with_ados_male_Female <- 
  ADOS_CSBS_Cor(input_data = ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month)
View(cor_CSBS_with_ados_male_Female)

cor_CSBS_with_ados_Male <- 
  ADOS_CSBS_Cor(subset(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month, 
                       ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month$Gender == "M"))
cor_CSBS_with_ados_Female <- 
  ADOS_CSBS_Cor(subset(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month, 
                       ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month$Gender == "F"))

View(cor_CSBS_with_ados_male_Female)

range(cor_CSBS_with_ados_male_Female) #-0.216  0.270 #NEW: -0.3164327  0.2539894
range(cor_CSBS_with_ados_Male) #-0.216  0.285 #NEW: -0.2981469  0.2251502
range(cor_CSBS_with_ados_Female) #-0.206  0.243 #NEW: -0.3535438  0.3141017
heatmap(as.matrix(cor_CSBS_with_ados_male_Female))

#VIZ simple heat map======
library(pheatmap)

pdf("SimpleHeatMap.OnlyCorrWithADOS.M.and.F.pdf")
pheatmap(as.matrix(cor_CSBS_with_ados_male_Female), display_numbers = T, 
         cluster_rows = F, cluster_cols = F, fontsize_number = 12, 
         main = "CSBS Cor. with Three ADOS scores\nAll subjects (M&F)")
dev.off()

pdf("SimpleHeatMap.OnlyCorrWithADOS.Females.pdf")
pheatmap(as.matrix(cor_CSBS_with_ados_Female), display_numbers = T, 
         cluster_rows = F, cluster_cols = F, fontsize_number = 12, 
         main = "CSBS Cor. with Three ADOS scores\nFemales")
dev.off()

pdf("SimpleHeatMap.OnlyCorrWithADOS.Males.pdf")
pheatmap(as.matrix(cor_CSBS_with_ados_Male), display_numbers = T, 
         cluster_rows = F, cluster_cols = F, fontsize_number = 12, 
         main = "CSBS Cor. with Three ADOS scores\nMales")
dev.off()



#!!!HERE
#corplots and Complex Heatmap==========
View(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month)
x <- 
  apply(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month[,5:32], 2, as.numeric)
View(x)
cor_CSBS_with_ados_and_itself <- cor(x)
View(cor_CSBS_with_ados_and_itself)
# !report
pdf("CorPlot_CSBS_with_ados_and_itself.Pie.24Month.pdf")
corrplot::corrplot(cor_CSBS_with_ados_and_itself,method = 'pie', 
                   type="upper",  diag=FALSE, tl.cex = 0.5, tl.col = "black", tl.srt = 45)
dev.off()
pdf("CorPlot_CSBS_with_ados_and_itself.Number.24Month.pdf")
corrplot::corrplot(cor_CSBS_with_ados_and_itself,method = 'number', 
                   type="upper",  diag=FALSE, tl.cex = 0.5, tl.col = "black", tl.srt = 45, 
                   number.cex= 14/ncol(cor_CSBS_with_ados_and_itself))
dev.off()

library(ComplexHeatmap)
#two different way of viz: first is a clustergram and the second one is just a simple heatmap with values
pdf("Clustergram_CSBS_with_ados_and_itself.Number.24.Months.pdf")
Heatmap(cor_CSBS_with_ados_and_itself, column_split = 5, row_split = 5)
dev.off()

pdf("SimpleHeatMap_CSBS_with_ados_and_itself.Number.24.Months.pdf", height = 20, width = 15)
pheatmap(as.matrix(cor_CSBS_with_ados_and_itself), display_numbers = T, 
         cluster_rows = F, cluster_cols = F, fontsize_number = 10, 
         main = "Corr. HeatMap 25 CSBS Questions ")
dev.off()


#t.test for individual questions=====
#in this section I'm trying to do the t.test on the individual questions 
#columns 8 through 32 are individual questions in CSBS

my_comparisons <- 
  combn(c("ASD", "Delay", "TD"), 2, FUN = NULL, simplify = FALSE)
i <- 8

#Run the following chunck of code in a loop fashion 
file.name.4.writing <- 
  paste(colnames(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month)[i],"CSBS.Question.T.24Month.test", ".pdf", sep = "")
pdf(file = file.name.4.writing)
ggplot(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month, 
       aes(y=ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month[,i], x= recentDxJ_dxCode, fill=recentDxJ_dxCode)) +
  #geom_point( size = 1, shape = 21, position = position_jitterdodge())+
  geom_violin(alpha=0.6) + 
  xlab(paste("DjxGroups")) + 
  stat_summary(fun.y = mean, geom="point",colour="blue", size=2)+
  stat_summary(fun.y = median, geom="point",colour="red", size=2)+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median)+
  ylab(colnames(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month)[i])+ 
  stat_compare_means(comparisons = my_comparisons, method = "t.test")+ 
  scale_fill_manual(values=c("red","orange","green"))
dev.off()
i <- i + 1
i

View(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month)
save(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month,
     file = "ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month")
write.csv(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month,
          file = "ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.csv")


################################################################################################################################################################################
################################################################################################################################################################################
#matching============
################################################################################################################################################################################
################################################################################################################################################################################
#I'm trying to use the MatchIt library to balanced for age =========
#it didn't work well So I tried to do it by myself (removing rendom subjects)
library("MatchIt")
ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month$recentDxJ_dxCode <- 
  as.factor(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month$recentDxJ_dxCode)

ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month$
  m.out1 <- matchit(recentDxJ_dxCode ~ AgeMo, 
                    data = ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month,
                    method = "nearest", distance = "glm")

#Error: The treatment must be a binary variable.
ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month %>%
  filter(recentDxJ_dxCode!="Delay")
dim(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD)#895  32
table(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD$recentDxJ_dxCode)
# ASD  TD 
# 460 435
ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD$recentDxJ_dxCode <- 
  as.factor(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD$recentDxJ_dxCode)

m.out1 <- matchit(recentDxJ_dxCode ~ AgeMo, 
                  data = ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD,
                  method ="optimal", distance = "glm")

summary(m.out1, un = FALSE)

#View(m.out1)

temp <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD[m.out1$weights==1, ]
dim(temp)#870  33
table(temp$recentDxJ_dxCode)
# ASD  TD 
# 435 435 
x <- temp %>%
  filter(recentDxJ_dxCode == "ASD") %>%
  select(AgeMo)

y <- temp %>%
  filter(recentDxJ_dxCode != "ASD") %>%
  select(AgeMo)

t.test(x,y)

plot(m.out1, type = "jitter", interactive = FALSE)

#applying lm on all questions 
ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.4.lm <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month
rownames(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.4.lm) <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.4.lm$subjectid
ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.4.lm <-
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.4.lm[,-c(1,5:7)]
dim(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.4.lm)#1345   28
dim(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month)#1345   32
ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.4.lm$Gender <- 
  as.factor(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.4.lm$Gender)

fit1 <- lm(recentDxJ_dxCode ~ . , data = ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.4.lm)
#library(lmtest)
coeftest(fit1)
View(x)
#matching age by ramoving random old ASD (50) and young TD subjects (25)====
ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD$recentDxJ_dxCode
ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD$AgeMo
x <- ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD %>%
  filter(recentDxJ_dxCode == "ASD" & AgeMo >= 20)

y <- ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD %>%
  filter(recentDxJ_dxCode == "TD" & AgeMo <= 13)

dim(x)#89 33
dim(y)#205  33
x.2 <- 
  sample(x$subjectid, 50, replace = FALSE)

y.2 <- 
  sample(y$subjectid, 25, replace = FALSE)

ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD.trying.2.balance <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD %>%
  filter(!(subjectid %in%  union(y.2,x.2)))

dim(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD.trying.2.balance)
#820  33

temp <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD.trying.2.balance

x <- temp %>%
  filter(recentDxJ_dxCode == "ASD") %>%
  select(AgeMo)

y <- temp %>%
  filter(recentDxJ_dxCode != "ASD") %>%
  select(AgeMo)

t.test(x,y)

# Welch Two Sample t-test
# 
# data:  x and y
# t = 1.5881, df = 812.95, p-value = 0.1127
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.08854028  0.83884760
# sample estimates:
#   mean of x mean of y 
# 15.22976  14.85461 

#the below list is the list of matched ASD and TD 
subjectId.vctr.4.balanced.age.ASD.TD <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.ASD.nonASD.trying.2.balance$subjectid
length(subjectId.vctr.4.balanced.age.ASD.TD)#820

#removing 40 subjects from Delay group 
dim(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month)#1345   32
table(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month$recentDxJ_dxCode)
# ASD Delay    TD 
# 460   450   435 

x <- ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month %>%
  filter(recentDxJ_dxCode == "Delay" & AgeMo >= 20)


dim(x)#68 32

x.2 <- 
  sample(x$subjectid, 40, replace = FALSE)


ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.Delay.Subjects <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month %>%
  filter(!(subjectid %in% x.2 ) & recentDxJ_dxCode == "Delay")

dim(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.Delay.Subjects)
#410  32

selected.subjectIDs.ASD.TD.Delay <- 
  union(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.Delay.Subjects$subjectid,
        subjectId.vctr.4.balanced.age.ASD.TD)
length(selected.subjectIDs.ASD.TD.Delay)#1230

#testing final matched data set=======
ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month %>%
  filter(subjectid %in% selected.subjectIDs.ASD.TD.Delay)
dim(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge)

setwd("/Users/apple/Desktop/Eric/Research/Karen/Classifier/CSBS/Results/")
save(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge,
     file = "ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge")
#load("/Users/apple/Desktop/Eric/Research/Karen/Classifier/CSBS/Results/ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge")
View(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge)



i <- 8

#Run the following chunck of code in a loop fashion 
file.name.4.writing <- 
  paste(colnames(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge)[i],"CSBS.Question.T.24Month.test", ".pdf", sep = "")
pdf(file = file.name.4.writing)

#computing the effect sizes
temp.ASD.TD.Subset <- ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge %>%
  filter(recentDxJ_dxCode!="Delay") %>%
  droplevels() 
temp.ASD.TD.Subset <- 
  droplevels(temp.ASD.TD.Subset)
eff.size.ASD.TD <- 
  round(x = cohensD(temp.ASD.TD.Subset[i] ~ recentDxJ_dxCode, data = temp.ASD.TD.Subset) , digits = 2)
#x <- cohen.d(Q19.NameResp ~ recentDxJ_dxCode, data = temp.ASD.Delay.Subste)$estimate
#x$estimate
temp.ASD.Delay.Subste <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge %>%
  filter(recentDxJ_dxCode!="TD") %>%
  droplevels() 
temp.ASD.Delay.Subste <- 
  droplevels(temp.ASD.Delay.Subste)
eff.size.ASD.Delay <-
  round(x = cohensD(temp.ASD.Delay.Subste[i] ~ recentDxJ_dxCode, data = temp.ASD.Delay.Subste), digits = 2 )

temp.Delay.TD.Subset <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge %>%
  filter(recentDxJ_dxCode!="ASD") %>%
  droplevels() 
temp.Delay.TD.Subset <- 
  droplevels(temp.Delay.TD.Subset)
eff.size.Delay.TD <-
  round(x = cohensD(temp.Delay.TD.Subset[i] ~ recentDxJ_dxCode, data = temp.Delay.TD.Subset) , 2)

str.4.ggplot.title <- 
  paste("CohensD:  ", "ASD.TD: ", as.character(eff.size.ASD.TD), "; ",
        "ASD.Delay: ", as.character(eff.size.ASD.Delay), "; ",
        "Delay.TD: ", as.character(eff.size.Delay.TD), sep = ""
  )

ggplot(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge, 
       aes(y=ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge[,i], x= recentDxJ_dxCode, fill=recentDxJ_dxCode)) +
  #geom_point( size = 1, shape = 21, position = position_jitterdodge())+
  geom_violin(alpha=0.6) + 
  xlab(paste("DjxGroups")) + 
  stat_summary(fun.y = mean, geom="point",colour="blue", size=2)+
  stat_summary(fun.y = median, geom="point",colour="red", size=2)+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median)+
  ylab(colnames(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge)[i])+ 
  stat_compare_means(comparisons = my_comparisons, method = "t.test")+ 
  scale_fill_manual(values=c("red","orange","green"))+ggtitle(str.4.ggplot.title)



dev.off()
i <- i + 1
i

#checking the again======= 
#load("/Users/apple/Desktop/Eric/Research/Karen/Classifier/CSBS/Results/ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge")
p <- ggbetweenstats( 
  data  = ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge,
  x     = recentDxJ_dxCode,
  y     = AgeMo, p.adjust.method = "BH", effsize.type = "eta",
  title = "AgeDiff"
)
pdf("/Users/apple/Desktop/Eric/Research/Karen/Classifier/CSBS/Results/age.diff.pdf")
p + 
  ggplot2::scale_color_manual(values=c("red","orange","green"))+ggplot2::ggtitle("AgeDiff")
dev.off()

View(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge)

ep_CSBS_Merged.subset_24Month.matchedAge.AgeStatSummary <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge %>%
    group_by(recentDxJ_dxCode) %>% 
    summarise(Age.Mean = mean(AgeMo),
              Age.SD = sd(AgeMo),
              Age.Median = median(AgeMo),
              Age.Range = paste(as.character(range(AgeMo)[1]), as.character(range(AgeMo)[2]), sep = ","))
View(ep_CSBS_Merged.subset_24Month.matchedAge.AgeStatSummary)

write.csv(ep_CSBS_Merged.subset_24Month.matchedAge.AgeStatSummary,
           file = "./Results/CSBS/ep_CSBS_Merged.subset_24Month.matchedAge.AgeStatSummary.csv")

################################################################################################################################################################################################
#effect size ===============
#cohensD
#I'm goign to use this package "lsr"
#hereare some refrences for this package
#https://www.rdocumentation.org/packages/lsr/versions/0.5.2/topics/cohensD
#https://learningstatisticswithr.com/
#https://github.com/djnavarro/lsr
library("lsr")

dev.new()
pdf("Prematurity.pdf")
p <- ggstatsplot::ggbetweenstats(     data  = ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded,
                                      x     = recentDxJ_dxCode,
                                      y     = finalPrematureInfo, p.adjust.method = "BH", effsize.type = "eta",
                                      title = str.4.ggplot.title)
p 
dev.off()

library("effsize")
temp.ASD.TD.Subset <- ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge %>%
  filter(recentDxJ_dxCode!="Delay") %>%
  droplevels() 
temp.ASD.TD.Subset <- 
  droplevels(temp.ASD.TD.Subset)
cohensD(Q19.NameResp ~ recentDxJ_dxCode, data = temp.ASD.TD.Subset) 
cohen.d(Q19.NameResp ~ recentDxJ_dxCode, data = temp.ASD.TD.Subset) 

temp.ASD.Delay.Subste <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge %>%
  filter(recentDxJ_dxCode!="TD") %>%
  droplevels() 
temp.ASD.Delay.Subste <- 
  droplevels(temp.ASD.Delay.Subste)
cohensD(Q19.NameResp ~ recentDxJ_dxCode, data = temp.ASD.Delay.Subste) 
cohen.d(Q19.NameResp ~ recentDxJ_dxCode, data = temp.ASD.Delay.Subste) 

temp.Delay.TD.Subset <- 
  ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge %>%
  filter(recentDxJ_dxCode!="ASD") %>%
  droplevels() 
temp.Delay.TD.Subset <- 
  droplevels(temp.Delay.TD.Subset)
cohensD(Q19.NameResp ~ recentDxJ_dxCode, data = temp.Delay.TD.Subset) 

cohen.d(formula = Q19.NameResp ~ recentDxJ_dxCode, data = temp.Delay.TD.Subset)


x <-
  temp.Delay.TD.Subset %>%
  filter(recentDxJ_dxCode=="TD") 

y <-
  temp.Delay.TD.Subset %>%
  filter(recentDxJ_dxCode=="Delay") 

cohen.d(d = x$Q19.NameResp, f = y$Q19.NameResp)

typeof(x)

cohen.d(c(1:10), c(10:20))

################################################################################################################################################################################################
#prematurity===== 
dim(pCSBS)#2448   49
dim(eCSBS)#877  57
# View(pCSBS)
# View(eCSBS)

#extracting prematurity column from two CSBS data
pCSBS.premature.data <- pCSBS %>%
  select(SubjectId, Premature)
dim(pCSBS.premature.data)#2448    2
View(pCSBS.premature.data)
table(pCSBS.premature.data$Premature)
# -    N NULL    Y 
# 70 1767  180  431 
pCSBS.premature.data$Premature [pCSBS.premature.data$Premature== "-" |
                                  pCSBS.premature.data$Premature== "NULL"] <- NA
table(pCSBS.premature.data$Premature)
# N    Y 
# 1767  431 
View(pCSBS.premature.data)
#E6Y3N
pCSBS.premature.data$Premature [pCSBS.premature.data$Premature== "N"] <- 0
pCSBS.premature.data$Premature [pCSBS.premature.data$Premature== "Y"] <- 1
pCSBS.premature.data <- 
  select.only.first.visit(data = pCSBS.premature.data, 
                          index_for_unique_selection = 1, index_to_sort_based_on = 1)
# [1] "No. of removed records:"
# [1] 721
# [1] "Shrinkage percent: "
# [1] 0.7054739

eCSBS.premature.data <- eCSBS %>%
  select(SubjectId, weeks_Pregnancy)
dim(eCSBS.premature.data)#877   2
table(eCSBS.premature.data$weeks_Pregnancy)
eCSBS.premature.data$weeks_Pregnancy[26]
View(eCSBS.premature.data)
#           <24        25        26        27        28        29        30        31        32 
# 25         2         5         2         2        10         3         4         2        14 
# 33        34        35        36 37 to 40+ 
# 17        19        22        59       691

eCSBS.premature.data$weeks_Pregnancy [eCSBS.premature.data$weeks_Pregnancy== "" ] <- NA
sum(is.na(eCSBS.premature.data$weeks_Pregnancy))#25

eCSBS.premature.data$weeks_Pregnancy [eCSBS.premature.data$weeks_Pregnancy!= "37 to 40+" & 
                                        !is.na(eCSBS.premature.data$weeks_Pregnancy!= "37 to 40+") ] <- 1
eCSBS.premature.data$weeks_Pregnancy [eCSBS.premature.data$weeks_Pregnancy== "37 to 40+" ] <- 0


eCSBS.premature.data <- 
  select.only.first.visit(data = eCSBS.premature.data, 
                          index_for_unique_selection = 1, index_to_sort_based_on = 1)
# [1] "No. of removed records:"
# [1] 396
# [1] "Shrinkage percent: "
# [1] 0.5484607

#adding prematurity 
dim(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge)#1230   32
temp <- merge(x = ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge,
              y = eCSBS.premature.data, by.x = "subjectid", by.y = "SubjectId", all.x = TRUE
)
dim(temp)#1230   33

temp <- merge(x = temp,
              y = pCSBS.premature.data, by.x = "subjectid", by.y = "SubjectId", all.x = TRUE
)
dim(temp)#1230   34
View(temp)

#integrating two prematurity info
premature.vctr <- 
  if_else(condition = !is.na(temp$weeks_Pregnancy), 
          true = temp$weeks_Pregnancy, false = temp$Premature
  )  
length(premature.vctr)#
temp$finalPrematureInfo <- 
  premature.vctr


ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded <-
  temp
ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded$finalPrematureInfo <-
  as.numeric(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded$finalPrematureInfo)
save(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded,
     file = "ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded")
write.csv(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded,
          file = "ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded.csv")

table(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded$recentDxJ_dxCode,
      ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded$finalPrematureInfo)

# 0   1
# ASD   318  64
# Delay 313  74
# TD    303  84
View(pCSBS)
View(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded)
View(eCSBS)

pdf("Prematurity2.pdf")
ggplot(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded, 
       aes(y=finalPrematureInfo, x= recentDxJ_dxCode, fill=recentDxJ_dxCode)) +
  #geom_point( size = 1, shape = 21, position = position_jitterdodge())+
  geom_violin(alpha=0.6) + 
  xlab(paste("DjxGroups")) + 
  stat_summary(fun.y = mean, geom="point",colour="blue", size=2)+
  stat_summary(fun.y = median, geom="point",colour="red", size=2)+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median)+
  ylab("Prematurity")+ 
  stat_compare_means(comparisons = my_comparisons, method = "t.test")+ 
  scale_fill_manual(values=c("red","orange","green"))

dev.off()
table(ep_CSBS_Merged_4_numericalStatistical_analysis_subset_24Month.matchedAge.PrematurityAdded$recentDxJ_dxCode)

all_ET.NoPoor.CSBS.Added.5.11.2022.subset.HaveCSBS.HaveAllET

#fitting linear model on the WEKA normalized dataset to check the effect of the age and concern question=====
dim(all_ET.NoPoor.CSBS.Added.5.13.2022.subset.HaveCSBS.HaveAllET.4.WEKA.AgeAdded.MinMaxNormalizedcsv)#120  25

str(all_ET.NoPoor.CSBS.Added.5.13.2022.subset.HaveCSBS.HaveAllET.4.WEKA.AgeAdded.MinMaxNormalizedcsv)






