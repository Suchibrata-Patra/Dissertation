#======= Required Packages for the Codes
# install.packages("lattice")
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



# # rm(list=ls())
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
# fitted_prob=fitted(mymodel)
# #=============================================#
# # Finding thresold by Optimising TPR*(1-FPR)  #
# #=============================================#
# thresold = seq(0,1,0.001)
# TPR = numeric(length(thresold))
# FPR = numeric(length(thresold))
# Index = numeric(length(thresold))
# for (i in 1:length(thresold)) {
    # Y_hat = ifelse(fitted_prob <= thresold[i], 0, 1)
    # conf_matrix = caret::confusionMatrix(as.factor(Y_hat),   as.factor(data$TenYearCHD))
    # confusion_matrix = conf_matrix$table 
     # TN = confusion_matrix[1, 1]  # True Negatives
     # FP = confusion_matrix[1, 2]  # False Positives
     # FN = confusion_matrix[2, 1]  # False Negatives
     # TP = confusion_matrix[2, 2]  # True Positives
    # TPR[i] = TP / (TP + FN)
    # FPR[i] = FP / (TN+FP+FN+TP)
    # Index[i] = TPR[i] *(1-FPR[i]) 
# }
# optimal_threshold = thresold[which.max(Index)]
# plot(thresold, Index, type="l", xlab="Threshold", ylab="TPR*(1-FPR)")
# abline(v = optimal_threshold, col = "red", lty = 2)
# Y_hat_index = ifelse(fitted_prob <= optimal_threshold, 0, 1)
# confusion_matrix_index = caret::confusionMatrix(as.factor(Y_hat_index),
# as.factor(data$TenYearCHD))


#====
# Code for Plotting teh Correlation Heatmaps
#====== 
rm(list=ls())
data = read.csv("/Users/suchibratapatra/Desktop/Dissertation/maindata.csv")
library(ggplot2)
library(reshape2)

# Calculate correlation matrix
correlation_matrix = cor(data)

# Melt the correlation matrix for plotting
melted_correlation = melt(correlation_matrix)
# Plot heatmap with correlation coefficients
ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile() + 
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  scale_fill_gradient2(low = "#58390b", high = "#0f423c", midpoint = 0, limit = c(-1,1), name="Correlation", mid = "#FFFFFF") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  coord_fixed() +
  labs(x = "Variables", y = "Variables")


# = = = = = = 
# Plottting the VIF Values
# = = = = = =
# Remove existing objects and load the dataset

rm(list = ls())
data = read.csv("/Users/suchibratapatra/Desktop/Dissertation/maindata.csv")

# Load necessary libraries
library(car)
library(ggplot2)

# Fit the model
mymodel = glm(TenYearCHD ~ ., data = data, family = binomial(link = "logit"))

# Calculate VIF for each predictor in the model
vif_values = vif(mymodel)

# Transform VIF values into a data frame
vif_df = data.frame(Variable = names(vif_values), VIF = unname(vif_values))

# Create the bar chart using ggplot
ggplot(vif_df, aes(x = Variable, y = VIF, fill = VIF)) +
  geom_bar(stat = "identity", color = "steelblue") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Plotting of the VIF Values", y = "VIF Value", x = "") +
  coord_cartesian(ylim = c(0, max(vif_df$VIF) + 1)) + # Adjust y-axis limits
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle the x-axis text


# = = = = = = = = = = 
#Splitting the Dataset into Training and Testing Data
# = = = = = = = = = =
rm(list=ls())
set.seed(123)
data = read.csv("/Users/suchibratapatra/Desktop/Dissertation/maindata.csv")
attach(data)
# Define the proportion of data for training (e.g., 80%)
train_proportion = 0.8

# Number of rows in the data frame
num_rows = nrow(data)

# Randomly select row indices for the training set
train_indices = sample(num_rows, size = round(train_proportion * num_rows), replace = FALSE)

training_data = data[train_indices, ]
testing_data = data[-train_indices, ]

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



#Fitting the Complete Model



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





detach(data)




#install.packages("tidymodels")
library(readr)
library(tidymodels)

# Read the dataset and convert the target variable to a factor
data = read.csv("/Users/suchibratapatra/Desktop/Dissertation/maindata.csv")

# Plot job occupation against the target variable
ggplot(Ten, aes(job, fill = y)) +
    geom_bar() +
    coord_flip()
names(data)



