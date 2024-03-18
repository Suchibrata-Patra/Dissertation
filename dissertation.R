# ======================================================== #
#                    Final Code                            #
# ======================================================== #
rm(list=ls())
#install.packages("lattice")
# install.packages("caret")
#install.packages("pROC")
#install.packages("randomForest")
#install.packages("olsrr")
library(car)
library(caTools)
library(ggplot2)
library(pROC)
library(reshape2)
library(randomForest)
library(InformationValue)
library(olsrr)
set.seed(1234)

data = read.csv("/Users/suchibratapatra/Desktop/Dissertation/maindata.csv")
split = sample.split(data, SplitRatio = 0.8)
training_data = data[split, ]
testing_data = data[!split, ]




# = = = = = = = = = = = 
# Correlation Heatmap
# = = = = = = = = = = = 
correlation_matrix = cor(data)
melted_correlation = melt(correlation_matrix)
ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile() + 
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  scale_fill_gradient2(low = "#58390b", high = "#0f423c", midpoint = 0, limit = c(-1,1), name="Correlation", mid = "#FFFFFF") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  coord_fixed() +
  labs(x = "Variables", y = "Variables")


# = = = = = = = = = = = = =
# Converting into factors
# = = = = = = = = = = = = =
data$male = as.factor(data$male)
data$education = as.factor(data$education)
data$currentSmoker = as.factor(data$currentSmoker)
data$prevalentStroke = as.factor(data$prevalentStroke)
data$prevalentHyp = as.factor(data$prevalentHyp)
data$diabetes = as.factor(data$diabetes)
data$TenYearCHD = as.factor(data$TenYearCHD)


model = glm(TenYearCHD ~ ., data = training_data, family = binomial(link = "logit"))

# = = = = = = = = = = = = = 
# Plottting the VIF Values
# = = = = = = = = = = = = =

vif_values = vif(model)
vif_df = data.frame(Variable = names(vif_values), VIF = unname(vif_values))

# Create the bar chart using ggplot
ggplot(vif_df, aes(x = Variable, y = VIF, fill = VIF)) +
  geom_bar(stat = "identity", color = "steelblue") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Plotting of the VIF Values", y = "VIF Value", x = "") +
  coord_cartesian(ylim = c(0, max(vif_df$VIF) + 1)) + # Adjust y-axis limits
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle the x-axis text


	



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
#  Code for testing the significance of the predictor variables
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

x = summary(model)
y = x$coefficients
estimates = y[,1][-c(1,18)]
se = y[,2][-c(1,18)]
walds_t = estimates/sqrt(se)
significance = numeric(16)
for(i in 1:16){
	if(abs(walds_t[i])>1.96){
		significance[i]= "Not Significant"
	}else{
		significance[i]="Significant"
	}
}
data.frame(names(training_data),estimates,se,walds_t,significance)
#Findig Out the Odds Ratio
Odds_Ratio = exp(estimates)
p_values = 2*(1- qnorm(estimates)) ; p_values
data.frame(names(training_data),exp(Odds_Ratio),p_values)



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Selection of the Best Model
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 


# Full Model
fullmodel = glm(TenYearCHD ~ ., data = training_data, family = binomial(link = "logit"))
AIC_full_model = 2139.9


# =====================data
# Reduced Model -> 1
# =====================
x = summary(fullmodel)
y =	x$coefficients
z = which.max(as.vector(y[,4]))
reduced_data_1 = training_data[-z+1]
reduced_model_1 = glm(TenYearCHD ~ ., data = reduced_data_1, family = binomial(link = "logit"))
summary(reduced_model_1)
AIC_Reduced_Model_1 = 2138.2


# =====================
# Reduced Model -> 2
# =====================
x = summary(reduced_model_1)
y =	x$coefficients
z = which.max(as.vector(y[,4]))
reduced_data_2 = reduced_data_1[-z+1]
reduced_model_2 = glm(TenYearCHD ~ ., data = reduced_data_2, family = binomial(link = "logit"))
summary(reduced_model_2)
AIC_Reduced_Model_2 = 2136.3



# =====================
# Reduced Model -> 3
# =====================
x = summary(reduced_model_2)
y =	x$coefficients
z = which.max(as.vector(y[,4]))
reduced_data_3 = reduced_data_2[-z+1]
reduced_model_3 = glm(TenYearCHD ~ ., data = reduced_data_3, family = binomial(link = "logit"))
summary(reduced_model_3)
AIC_Reduced_Model_3 = 2134.7


# =====================
# Reduced Model -> 4
# =====================
x = summary(reduced_model_3)
y =	x$coefficients
z = which.max(as.vector(y[,4]))
reduced_data_4 = reduced_data_3[-z+1]
reduced_model_4 = glm(TenYearCHD ~ ., data = reduced_data_4, family = binomial(link = "logit"))
summary(reduced_model_4)
AIC_Reduced_Model_4 = 2133.2



# =====================
# Reduced Model -> 5
# =====================
x = summary(reduced_model_4)
y =	x$coefficients
z = which.max(as.vector(y[,4]))
reduced_data_5 = reduced_data_4[-z+1]
reduced_model_5 = glm(TenYearCHD ~ ., data = reduced_data_5, family = binomial(link = "logit"))
summary(reduced_model_5)
AIC_Reduced_Model_5 = 2131.6




# =====================
# Reduced Model -> 6
# =====================
x = summary(reduced_model_5)
y =	x$coefficients
z = which.max(as.vector(y[,4]))
reduced_data_6 = reduced_data_5[-z+1]
reduced_model_6 = glm(TenYearCHD ~ ., data = reduced_data_6, family = binomial(link = "logit"))
summary(reduced_model_6)
AIC_Reduced_Model_6 = 2130.1


# =====================
# Reduced Model -> 7
# =====================
x = summary(reduced_model_6)
y =	x$coefficients
z = which.max(as.vector(y[,4]))
reduced_data_7 = reduced_data_5[-z+1]
reduced_model_7 = glm(TenYearCHD ~ ., data = reduced_data_7, family = binomial(link = "logit"))
summary(reduced_model_7)
AIC_Reduced_Model_7 = 2130.4

Selected_Model = fullmodel



fitted_prob = fitted(Selected_Model)
 
#=============================================#
# Finding thresold by Optimising TPR*(1-FPR)  #
#=============================================#
TPR=array()
FPR=array()
Index = array()
k=1
p=seq(0.1,0.9,0.001)
for(i in p)
{
print(paste("Threshold = ",i))
Y.hat=ifelse(fitted_prob>i,1,0)
confusion_matrix = table(Y.hat,TenYearCHD)
print(confusion_matrix)
  TN = confusion_matrix[1, 1]  # True Negatives
  FP = confusion_matrix[1, 2]  # False Positives
  FN = confusion_matrix[2, 1]  # False Negatives
  TP = confusion_matrix[2, 2]  # True Positives
  TPR[k] = TP / (TP + FN)
  FPR[k] = FP / (TN + FP + FN + TP)
  Index[k] = TPR[k] * (1 - FPR[k]) 
  plot(FPR,TPR,type="l",main="Finding Optimum Model . . . ")
  k=k+1
 
}
optimum_thresold = p[which.max(Index)]


#===================================#
#Checking the Model Accuracy        #
#===================================#

binary_predictions = ifelse(fitted_prob > optimum_thresold, 1, 0)

# Calculate confusion matrix
confusion_matrix = table(binary_predictions, TenYearCHD)

# Print confusion matrix
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate accuracy
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 3)))

# Calculate precision
precision = confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
print(paste("Precision:", round(precision, 3)))

# Calculate recall (True Positive Rate)
recall = confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
print(paste("Recall (True Positive Rate):", round(recall, 3)))

# Calculate F1-score
F1_score = 2 * (precision * recall) / (precision + recall)
print(paste("F1-score:", round(F1_score, 3)))



  