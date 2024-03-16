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
# Final Code
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

# Fitting of the Logistic Regression Model 
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
plot(fitted_prob,type="h",main="Original vs Fitted")
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




















#  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#   						       	Previous Code
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

#--- DATA CLEANING ---#
#=====================#

rm(list=ls())
data=read.csv("literacy_data.csv",header=TRUE)
is.data.frame(data)
nrow(data)
apply(is.na(data),2,which)

# CLEANING
data1=na.omit(data)
apply(is.na(data1),2,which)
dim(data1)

#Recoding 2 as 0
attach(data1)
literacy1=replace(literacy,which(literacy==2),0)
gender1=replace(gender,which(gender==2),0)
data2=data.frame(literacy1,gender1,age)
dim(data2)

#part (i)
tab1=table(literacy1,gender1);tab1
#(a)
o1=7860/2018;o1
#(b)
o2=4955/3883;o2
#(c)
or=3.894945/1.276075;or

#part (ii)
model_logit=glm(literacy1~age,data2,
family=binomial(link="logit"));model_logit
fitted(model_logit)

#part (iii)
model_probit=glm(literacy1~age,data2,
family=binomial(link="probit"));model_probit
pi_hat_probit=fitted(model_probit)

#part (iv)
pi_hat_logit=as.vector(fitted(model_logit));pi_hat_logit
pi_hat_probit=as.vector(fitted(model_probit));pi_hat_probit

chisq_logit=sum(((literacy1-pi_hat_logit)^2)/(pi_hat_logit*(1-pi_hat_logit)))
chisq_probit=sum(((literacy1-pi_hat_probit)^2)/(pi_hat_probit*(1-pi_hat_probit)))
chisq_logit
chisq_probit
 
#part (v)

dt=data.frame(age,pi_hat_logit,pi_hat_probit)
dtt=dt[order(age),]

par(mfrow=c(2,1))
plot(y=dtt[,2],x=dtt[,1],type='l')
plot(y=dtt[,3],x=dtt[,1],type='l')

# part (vi)

library(caret)
library(InformationValue)
??caret
confusionMatrix(as.factor(literacy1),pi_hat_logit,threshold=0.5)

Y.hat=ifelse(pi_hat_logit>0.5,1,0)
caret::confusionMatrix(as.factor(Y.hat),as.factor(literacy1))

Y.hat=ifelse(pi_hat_logit>0.5,1,0)  
t1=table(Y.hat,literacy1);t1
TPR=t1[2,2]/(t1[2,2]+t1[1,2]);TPR
FPR=4859/(4859+1042);FPR
Sensitivity=TPR;Sensitivity
Specificity=1-FPR;Specificity
misc=1-TPR+FPR;misc

TPR=array()
FPR=array()
Sensitivity=array()
Specificity=array()
misc=array()

k=1
p=seq(0.1,0.8,0.1)
for(i in p)
{
print(paste("Threshold = ",i))
Y.hat=ifelse(pi_hat_logit>i,1,0)
t=table(Y.hat,literacy1)
print(t)
TPR[k]=t[2,2]/(t[2,2]+t[1,2])
FPR[k]=t[2,1]/(t[2,1]+t[1,1])
Sensitivity[k]=TPR[k]
Specificity[k]=1-FPR[k]
misc[k]=1-TPR[k]+FPR[k]
k=k+1
}

data.frame(p,TPR,FPR,Specificity,Sensitivity,misc,TPR*(1-FPR))



TPR=array()
FPR=array()
Sensitivity=array()
Specificity=array()
misclassification=array()

k=1
#p=seq(0.1,0.8,0.1)
p=unique(pi_hat_logit)
p=sort(p)
p=p[-length(p)]
p
for(i in p)
{
print(paste("Threshold = ",i))
Y.hat=ifelse(pi_hat_logit>i,1,0)
t=table(Y.hat,literacy1)
print(t)
TPR[k]=t[2,2]/(t[2,2]+t[1,2])
FPR[k]=t[2,1]/(t[2,1]+t[1,1])
Sensitivity[k]=TPR[k]
Specificity[k]=1-FPR[k]
misclassification[k]=1-TPR[k]+FPR[k]
k=k+1
}

p
misc
dtt=data.frame(p,TPR,FPR,Specificity,Sensitivity,misclassification,TPR*(1-FPR));dtt
xx=TPR*(1-FPR)
which(xx==max(xx))
dtt[66,]
xx
par(mfrow=c(1,2))
plot(y=TPR,x=1-FPR,type='l',xlim=c(0,1),main="ROC Curve")
plot(x=TPR,y=1-FPR,type='l',xlim=c(0,1),main="ROC Curve")




rm(list=ls())
data = read.csv("/Users/suchibratapatra/Desktop/Dissertation/maindata.csv")
data$male = as.factor(data$male)
data$education = as.factor(data$education)
data$currentSmoker = as.factor(data$currentSmoker)
data$prevalentStroke = as.factor(data$prevalentStroke)
data$prevalentHyp = as.factor(data$prevalentHyp)
data$diabetes = as.factor(data$diabetes)
data$TenYearCHD = as.factor(data$TenYearCHD)
mymodel = glm(TenYearCHD ~ ., data = data, family = binomial(link = "logit"))
summary(mymodel)



#Code for testing the significance of the predictor variables
x=summary(mymodel)
x$deviance
y=x$coefficients
y = as.data.frame(y)
estimates = y[,1][-c(1,18)]
se = y[,2][-c(1,18)]
walds_t = estimates/sqrt(se)
significance = numeric(16)
for(i in 1:16){
	if(walds_t[i]>=1.96){
		significance[i]= "***" ;
	}else{
		significance[i]="*"
	}
}
data.frame(names(data),estimates,se,walds_t,significance)
