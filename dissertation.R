#======= Required pAckages for the Codes
# install.packages("lattice")
# 




# rm(list=ls())
# data = read.csv("/Users/suchibratapatra/Desktop/Dissertation/maindata.csv")
# attach(data)
# names(data)
# data$male = as.factor(data$male)
# data$education = as.factor(data$education)
# data$currentSmoker = as.factor(data$currentSmoker)
# data$prevalentStroke = as.factor(data$prevalentStroke)
# data$prevalentHyp = as.factor(data$prevalentHyp)
# data$diabetes = as.factor(data$diabetes)
# data$TenYearCHD = as.factor(data$TenYearCHD)

# mymodel = glm(TenYearCHD ~ ., data = data, family=binomial(link ="logit"))
# summary(mymodel)
# # install.packages("caret")
# fitted_prob=fitted(mymodel)
# thresold = seq(0,1,0.001)
# accuracy = numeric(length(thresold))
# for(i in 1:length(thresold)) {
    # Y_hat = ifelse(fitted_prob <= thresold[i], 0, 1)
    # x = caret::confusionMatrix(as.factor(Y_hat),as.factor(data$TenYearCHD))
    # y = as.data.frame(x$overall)
    # accuracy[i] = y[1,1]
# }
# accuracy
# plot(thresold, accuracy, type="l")
# abline(v = thresold[which.max(accuracy)], col = "red", lty = 2)

# #thresold = thresold[which.max(accuracy)]
# Y_hat = ifelse(fitted_prob <= thresold, 0, 1)
# x = caret::confusionMatrix(as.factor(Y_hat),as.factor(data$TenYearCHD))
# detach(data)



rm(list=ls())
data = read.csv("/Users/suchibratapatra/Desktop/Dissertation/maindata.csv")
attach(data)
names(data)
data$male = as.factor(data$male)
data$education = as.factor(data$education)
data$currentSmoker = as.factor(data$currentSmoker)
data$prevalentStroke = as.factor(data$prevalentStroke)
data$prevalentHyp = as.factor(data$prevalentHyp)
data$diabetes = as.factor(data$diabetes)
data$TenYearCHD = as.factor(data$TenYearCHD)

mymodel = glm(TenYearCHD ~ ., data = data, family=binomial(link ="logit"))
summary(mymodel)
fitted_prob=fitted(mymodel)
#=============================================#
# Finding thresold by Optimising TPR*(1-FPR)  #
#=============================================#
thresold = seq(0,1,0.001)
TPR = numeric(length(thresold))
FPR = numeric(length(thresold))
Index = numeric(length(thresold))
for (i in 1:length(thresold)) {
    Y_hat = ifelse(fitted_prob <= thresold[i], 0, 1)
    conf_matrix = caret::confusionMatrix(as.factor(Y_hat),   as.factor(data$TenYearCHD))
    confusion_matrix = conf_matrix$table 
     TN = confusion_matrix[1, 1]  # True Negatives
     FP = confusion_matrix[1, 2]  # False Positives
     FN = confusion_matrix[2, 1]  # False Negatives
     TP = confusion_matrix[2, 2]  # True Positives
    TPR[i] = TP / (TP + FN)
    FPR[i] = FP / (TN+FP+FN+TP)
    Index[i] = TPR[i] *(1-FPR[i]) 
}
optimal_threshold = thresold[which.max(Index)]
plot(thresold, Index, type="l", xlab="Threshold", ylab="TPR*(1-FPR)")
abline(v = optimal_threshold, col = "red", lty = 2)
Y_hat_index = ifelse(fitted_prob <= optimal_threshold, 0, 1)
confusion_matrix_index = caret::confusionMatrix(as.factor(Y_hat_index),
as.factor(data$TenYearCHD))


#==== 
#Correlation Heatmaprm(list=ls())
rm(list=ls())
data = read.csv("/Users/suchibratapatra/Desktop/Dissertation/maindata.csv")
attach(data)
names(data)
library(ggplot2)
library(reshape2)

# Calculate correlation matrix
correlation_matrix <- cor(data)

# Melt correlation matrix
melted_correlation <- melt(correlation_matrix)

# Plot heatmap
ggplot(data = melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "Yellow", high = "Red", mid = "white", midpoint = 0,
                       limit = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed()



#this is source Code

