# CA1- Data Analysis
# Heart Attack Analysis

# Heart Attack data set (heart.csv) contains information about patients and 
# their chance of having a heart attack. 

# Data Preparation 

# Read in the heart.csv data set into a new data frame called heart_attack
heart_attack <- read.csv("heart.csv")
str(heart_attack)

# See if this dataset has any missing data
incomplete_data <- heart_attack[!complete.cases(heart_attack),]
incomplete_data
#or visualize the data for missing variables
library("mice")
md.pattern((heart_attack))
# Both complete.cases and mice md.pattern reveal that there is no missing data
# in the dataset, so there is no need to worry about removing na's

# Display columns of heart_attack data frame
names(heart_attack)

# Remove 'oldpeak', and 'slp' from data frame seeing as they are not
# needed for analysis 
include_list <- heart_attack[c("age", "sex", "cp", "trtbps", "chol", "fbs", 
                               "restecg", "thalachh", "exng", "caa", "thall", "output")]

# Make the include list the new data frame (ha_cleaned)
# to work with for the rest of analysis
ha_cleaned <- include_list
names(ha_cleaned)

# Modify and update variable names for ha_cleaned data frame 
col_names <- c("Age", "Sex", "ChestPainType", 
               "RestingBloodPressure", 
               "Cholestoral", 
               "FastingBloodSugar(> 120 mg/dl)", 
               "RestingElectrocardiographicResults", 
               "MaximumHeartRateAchieved", 
               "ExerciseInducedAngina",
               "NumberofMajorVessels",
               "BloodDisorderThalassemia",
               "ChanceofHeartAttack")
colnames(ha_cleaned) <- col_names
names(ha_cleaned)

# Research Question 1: Does a person with fewer major vessels have an increased 
# chance of having a heart attack?

# H0: Number of major vessels has no effect on having a heart attack
# H1: Number of major vessels has an effect on having a heart attack

# The variables that will be used for analysis are 'Number of Major Vessels'
# and 'Chance of Heart Attack'.

# What type of variables are they?
str(ha_cleaned)
# 'Number of Major Vessels' is a continuous variable
# 'Chance of Heart Attack' is a categorical dichotomous variable

# Need to convert the 'Chance of Heart Attack' variable to 
# a categorical dichotomous variable
ha_cleaned$`ChanceofHeartAttack` <- 
  factor(ha_cleaned$`ChanceofHeartAttack`, labels = 
           c("Less of a Chance", "More of a Chance"))
str(ha_cleaned)


# Check linearity 
attach(ha_cleaned)
plot(NumberofMajorVessels, ChanceofHeartAttack, pch = "19", col = "red", 
     main = "Comparison of Number of Major Vessels with Chance of Heart Attack",
     xlab = "Number of Major Vessels", ylab = "Chance of Heart Attack")
# This does not tell much because the two variables are different types

# We can split the dicotomous variable into 2 and then examine the data
library("lattice")

histogram(~NumberofMajorVessels | ChanceofHeartAttack,
          data = ha_cleaned,
          main = "Comparison of Number of Major Vessels with 
          Chance of Heart Attack",
          xlab = "Number of Major Vessels",
          ylab = "Chance of Heart Attack")
# Visual analysis seems to indicate that the data is skewed
# Summarise the medians of the data to confirm that it is not normall dist.
tapply(NumberofMajorVessels, ChanceofHeartAttack, median)
# They seem to be off, the center point for 'Less of a Chance' is 1 where
# the center point for 'More of a Chance' is 0
# Let's confirm the distribution


# Quantile-quantile plot (Q-Q) plot allows us to check
# if the data is normally distributed or not
# Compare quantiles of both samples 

# is temp normally dist?
qqnorm(NumberofMajorVessels)
# Add line that represents normal dist.
qqline(NumberofMajorVessels, col = "red")
# Temp appears not to be normally distributed



# Is activity normally distributed? 
with(ha_cleaned, qqplot(NumberofMajorVessels[ChanceofHeartAttack == "More of a Chance"],
                        NumberofMajorVessels[ChanceofHeartAttack == "Less of a Chance"],
                          main = "Comparing 2 samples of activity data",
                          xlab = "Active temp = Yes",
                          ylab = "Active temp = No"))
#qqplot and qqnorm are the same things

# We can add normality line to the plot
# to help evalute normality 
with(beavers_data, {
  qqnorm(temp[activ == "No"],
         main = "Inactive data")
  qqline(temp[activ == "No"])})

with(beavers_data, {
  qqnorm(temp[activ == "Yes"],
         main = "Active data")
  qqline(temp[activ == "Yes"])})













# Check correlations
pairs(ha_cleaned)


agesex <- heart_attack(age, sex)




