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




# = = = = = = = = = = = #
#  Correlation Heatmap  #
# = = = = = = = = = = = #
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


# = = = = = = = = = = = = = #
#  Converting into factors  #
# = = = = = = = = = = = = = #
data$male = as.factor(data$male)
data$education = as.factor(data$education)
data$currentSmoker = as.factor(data$currentSmoker)
data$prevalentStroke = as.factor(data$prevalentStroke)
data$prevalentHyp = as.factor(data$prevalentHyp)
data$diabetes = as.factor(data$diabetes)
data$TenYearCHD = as.factor(data$TenYearCHD)


model = glm(TenYearCHD ~ ., data = training_data, family = binomial(link = "logit"))

#===============================#
#   Plottting the VIF Values    #
#===============================#

vif_values = vif(model)
vif_df = data.frame(Variable = names(vif_values), VIF = unname(vif_values))

ggplot(vif_df, aes(x = Variable, y = VIF, fill = VIF)) +
  geom_bar(stat = "identity", color = "steelblue") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Plotting of the VIF Values", y = "VIF Value", x = "") +
  coord_cartesian(ylim = c(0, max(vif_df$VIF) + 1)) + # Adjust y-axis limits
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle the x-axis text


	



#=========================================#
# Significance of the predictor variables #
#=========================================#
x = summary(fullmodel)
y = x$coefficients
estimates = y[,1][-1]
se = y[,2][-1]
walds_t = as.numeric(estimates/sqrt(se))
significance = numeric(15)
for(i in 1:15){
    if(abs(walds_t[i])>1.96){
        significance[i]= "Not Significant"
    }else{
        significance[i]="Significant"
    }
}
data.frame(names(training_data)[-16],walds_t,significance)
Odds_Ratio = exp(estimates)



#======================================#
# Selection of the Best Model          #
#======================================#

# ============
# Full Model
# ============
fullmodel = glm(TenYearCHD ~ ., data = training_data, family = binomial(link = "logit"))
AIC_full_model = 2139.9
summary(fullmodel)

# =====================
# Reduced Model -> 1
# =====================
x = summary(fullmodel);x
y =	x$coefficients
z = which.max(as.vector(y[,4]))
reduced_data_1 = training_data[-z+1]
reduced_model_1 = glm(TenYearCHD ~ ., data = reduced_data_1, family = binomial(link = "logit"))
summary(reduced_model_1)
AIC_Reduced_Model_1 = 2138


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
reduced_data_7 = reduced_data_6[-z+1]
reduced_model_7 = glm(TenYearCHD ~ ., data = reduced_data_7, family = binomial(link = "logit"))
summary(reduced_model_7)
AIC_Reduced_Model_7 = 2128.8



# =====================
# Reduced Model -> 8
# =====================
x = summary(reduced_model_7)
y =	x$coefficients
z = which.max(as.vector(y[,4]))
reduced_data_8 = reduced_data_7[-z+1]
reduced_model_8 = glm(TenYearCHD ~ ., data = reduced_data_8, family = binomial(link = "logit"))
summary(reduced_model_8)
AIC_Reduced_Model_7 = 2128



# =====================
# Reduced Model -> 9
# =====================
x = summary(reduced_model_8)
y =	x$coefficients
z = which.max(as.vector(y[,4]))
reduced_data_9 = reduced_data_8[-z+1]
reduced_model_9 = glm(TenYearCHD ~ ., data = reduced_data_9, family = binomial(link = "logit"))
summary(reduced_model_9)



Selected_Model = reduced_model_6
fitted_prob = fitted(Selected_Model)








#==============================================#
#Finding Out Odds ratio
#==============================================#
x = summary(reduced_model_8)
y = x$coefficients
estimates = y[,1][-1]
se = y[,2][-1]
Odds_Ratio = as.data.frame(exp(estimates))
Odds_Ratio


names(summary(fullmodel))





























































fitted_prob = predict(reduced_model_8)

#=============================================#
#Finding Goosness of Fit for the reduced Model#
#=============================================#
# Assuming fitted_prob contains the predicted probabilities and reduced_data_8$TenYearCHD contains the actual labels

thresholds = sort(fitted_prob)

# Initialize arrays to store TPR and FPR values
tpr_values = numeric(length(thresholds))
fpr_values = numeric(length(thresholds))

# Calculate TPR and FPR for each threshold
for (i in 1:length(thresholds)) {
  predicted_labels = ifelse(fitted_prob > thresholds[i], 1, 0)
  tp = sum(predicted_labels == 1 & reduced_data_8$TenYearCHD == 1)
  fp = sum(predicted_labels == 1 & reduced_data_8$TenYearCHD == 0)
  fn = sum(predicted_labels == 0 & reduced_data_8$TenYearCHD == 1)
  tn = sum(predicted_labels == 0 & reduced_data_8$TenYearCHD == 0)
  tpr_values[i] = tp / (tp + fn)
  fpr_values[i] = fp / (fp + tn)
}

# Now, let's plot the ROC curve using base R plotting functions
plot(fpr_values, tpr_values, type = "l", main = "ROC Curve", col = "GREY", lwd = 2, xlab = "FPR", ylab = "TPR")

# Add legend
legend("bottomright", legend = "ROC Curve", col = "blue", lty = 1, lwd = 2, bty = "n")

# Determine predicted labels based on the optimal threshold
predicted_labels = ifelse(fitted_prob > optimal_threshold, 1, 0)

# Calculate accuracy
accuracy = sum(predicted_labels == reduced_data_8$TenYearCHD) / length(reduced_data_8$TenYearCHD)

# Print accuracy
print(paste("Accuracy of the model at the optimal threshold:", round(accuracy, 3)))



#Finding out the AUC Value
roc_curve = roc(reduced_data_8$TenYearCHD, fitted_prob)
auc(roc_curve)
lines(fpr_values,fpr_values,lty=2)