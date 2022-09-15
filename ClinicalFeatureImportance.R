#in this script I'm going to do ensemble feature importance analysis on various clinical data types
ados_t <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/All Clinical Data/Detailed Clinical Measures Srinivasa macarthur/ADOST.csv")
ados_mod1 <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/All Clinical Data/Detailed Clinical Measures Srinivasa macarthur/ADOS_Mod1.csv")
ados_mod2 <- read.csv(file = "/Users/apple/Desktop/Eric/Research/Karen/All Clinical Data/Detailed Clinical Measures Srinivasa macarthur/ADOS_Mod2.csv")

dim(ados_t)#1335   65
dim(ados_mod1)#671  57
dim(ados_mod2)#330  50
View(ados_t)

#!!! check with Cindy 
ados_t[is.na(ados_t)] <- 0
ados_mod1[is.na(ados_mod1)] <- 0
ados_mod2[is.na(ados_mod2)] <- 0

library(caret)
library(glmnet)
trainData <- ados_t
sum(is.na(ados_t))
View(trainData)
dim(trainData)

#lasso======
x <- as.matrix(trainData[,-c(1,65)]) # all X vars
#y <- as.double(as.matrix(ifelse(trainData[, 53]=='nonASD', 0, 1))) # Only Class
y <- trainData[,65]
# Fit the LASSO model (Lasso: Alpha = 1)

cv.lasso <- cv.glmnet(x, y,  alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results

pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/All Clinical Data/Detailed Clinical Measures Srinivasa macarthur/lassoADosT.pdf",   # The directory you want to save the file in
    width = 20, # The width of the plot in inches
    height = 20) # The height of the plot in inches

plot(cv.lasso)
dev.off()

cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)

# See all contributing variables
#A high positive or low negative implies more important is that variable.
lasso_ET_res <- df_coef[df_coef[, 1] != 0, ]
View(lasso_ET_res)

#Boruta===========
library(Boruta)
boruta_output <- Boruta(Overall_Total ~ ., data=trainData, doTrace=0)

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

pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/All Clinical Data/Detailed Clinical Measures Srinivasa macarthur/boruta_ADosT.pdf",   # The directory you want to save the file in
    width = 20, # The width of the plot in inches
    height = 20) # The height of the plot in inches

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  
dev.off() 

#regression======
fittel.lm <- lm(formula = target_var ~., data = trainData)
lm_summary <- summary(fittel.lm)
lm_summary$coefficients[,4]

#caret=========
library( randomForest )
classifier_vctr <- c ("treebag", "logicBag", "bagFDA", "bartMachine", "brnn", "blackboost", 
                      "J48", "C5.0", "enet", "fda", "gaussprRadial", "gamLoess", 
                      "glmnet", "kknn", "svmLinearWeights2","lda",  "mlp", "stepQDA", "qda", "rqlasso")

colnames(trainData)[65] <- "target_var"
trainData <- trainData[,-1] 
x <- train(target_var ~ ., data=trainData, method="logicBag")

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
    train(target_var ~ ., data=trainData, method=classifier)
  #saving the feature importance results
  varImp_list[[i]] <- varImp(trained_model_list[[i]], scale=F)
  
}



#=====comapring examiners' scores=================
examinor_ados_t <- cbind(ados_t$Examiner, ados_t$sex, ados_t$agemo , ados_t$Overall_Total, rep("ados_t", length( ados_t$Overall_Total)))

examinor_ados_mod1 <- cbind(ados_mod1$Examiner, 
                            ados_mod1$sex, 
                            ados_mod1$agemo,
                            ados_mod1$OverallTotal, 
                            rep("ados_mod1", length( ados_mod1$OverallTotal))
                            )

examinor_ados_mod2 <- cbind(ados_mod2$Examiner, 
                            ados_mod2$sex, 
                            ados_mod2$agemo, 
                            ados_mod2$OverallTotal, 
                            rep("ados_mod2", length( ados_mod2$OverallTotal))
                            )

examinor_ados_all <- rbind(examinor_ados_t, examinor_ados_mod1, examinor_ados_mod2)

colnames(examinor_ados_all) <- c("Examiner", "Sex", "AgeMonth", "OverallADOS", "ADOSModule")

examinor_ados_all <- as.data.frame(examinor_ados_all)

View(examinor_ados_all)

examinor_ados_all$AgeMonth <- as.numeric(examinor_ados_all$AgeMonth)
examinor_ados_all$OverallADOS <- as.numeric(examinor_ados_all$OverallADOS)


pdf(file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/Examinor1AOverallScoreGroupedByModule.pdf",   # The directory you want to save the file in
    width = 20, # The width of the plot in inches
    height = 20) # The height of the plot in inches
p <- ggplot(data = examinor_ados_all, aes(x = Examiner , y = OverallADOS, fill = ADOSModule)) 
p + geom_violin() +  geom_point( size = 1, shape = 21, position = position_jitterdodge())
dev.off()

View(examinor_ados_all)
range(examinor_ados_all$OverallADOS)

temp <- subset(examinor_ados_all, examinor_ados_all$Examiner == "Steven Arias")
dim(temp)
temp2 <- subset(examinor_ados_all, examinor_ados_all$Examiner == "Cindy Carter")
t.test(temp$OverallADOS, temp2$OverallADOS)
#p-value = 7.196e-05

View(table(examinor_ados_all$Examiner, examinor_ados_all$ADOSModule))

write.csv(examinor_ados_all, file = "/Users/apple/Desktop/Eric/Research/Karen/Manuscript Drafts /Report/examinor_ados_all.csv")
