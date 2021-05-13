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
# or visualize the data for missing variables
library("mice")
md.pattern((heart_attack))
# Both complete.cases and mice md.pattern reveal that there is no missing data
# in the dataset, so there is no need to worry about removing na's

# Display columns of heart_attack data frame
names(heart_attack)

# Remove all variable from data frame that are not needed to answer Research Q's
include_list <- heart_attack[c("sex", "exng", "caa", "cp", "chol", "output")]

# Make the include list the new data frame (ha_cleaned)
# to work with for the rest of analysis
ha_cleaned <- include_list
names(ha_cleaned)

# Modify and update variable names for ha_cleaned data frame 
col_names <- c("Sex", 
               "ExerciseInducedAngina",
               "NumberofMajorVessels",
               "ChestPainType", 
               "Cholesterol", 
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
# 'Number of Major Vessels' is a continuous interval variable
# 'Chance of Heart Attack' is a categorical dichotomous variable

# Need to convert the 'Chance of Heart Attack' variable to 
# a categorical dichotomous variable
ha_cleaned$`ChanceofHeartAttack` <- 
  factor(ha_cleaned$`ChanceofHeartAttack`, labels = 
           c("Less of a Chance", "More of a Chance"))
str(ha_cleaned)


# Check linearity/correlation
attach(ha_cleaned)
plot(NumberofMajorVessels, 
     ChanceofHeartAttack, 
     pch = "19", 
     col = "red", 
     main = "Comparison of Number of Major Vessels with Chance of Heart Attack",
     xlab = "Number of Major Vessels", 
     ylab = "Chance of Heart Attack")
# This does not tell much because the two variables are different types


# Instead, we can split the dichotomous variable into 2 & then examine the data
library("lattice")

histogram(~NumberofMajorVessels | ChanceofHeartAttack,
          data = ha_cleaned,
          main = "Distribution of Number of Major Vessels with 
          Chance of Heart Attack",
          xlab = "Number of Major Vessels",
          ylab = "Chance of Heart Attack")
# Visual analysis seems to indicate that the data is skewed to the right
# and not normally distributed

# Summarise the medians of the data to confirm that it is not normally distributed
tapply(NumberofMajorVessels, ChanceofHeartAttack, median)
# They seem to be off, the center point for both sides of the variable are different.
# The center point for 'Less of a Chance' is 1 
# Where the center point for 'More of a Chance' is 0
# Let's confirm the distribution

# Is NumberofMajorVessels normally distributed?
qqnorm(NumberofMajorVessels)
# Add line that represents normal distribution
qqline(NumberofMajorVessels, col = "red")
# NumberofMajorVessels appears not to be normally distributed


# Is ChanceofHeartAttack normally distributed? 
with(ha_cleaned, qqplot(NumberofMajorVessels[ChanceofHeartAttack == "More of a Chance"],
                        NumberofMajorVessels[ChanceofHeartAttack == "Less of a Chance"],
                          main = "Comparing Two Samples of Heart Attack Data",
                          xlab = "Less of a Chance",
                          ylab = "More of a Chance"))

# We can add normality line to the plot
# to help evalute normality 
with(ha_cleaned, {
  qqnorm(NumberofMajorVessels[ChanceofHeartAttack == "Less of a Chance"],
         main = "Less of a Chance")
  qqline(NumberofMajorVessels[ChanceofHeartAttack == "Less of a Chance"])})
# This reveals that the 'Less of a Chance' side of the 'ChanceofHeartAttack' 
# variable, is normally distributed


with(ha_cleaned, {
  qqnorm(NumberofMajorVessels[ChanceofHeartAttack == "More of a Chance"],
         main = "More of a Chance")
  qqline(NumberofMajorVessels[ChanceofHeartAttack == "More of a Chance"])})
# This reveals that the 'More of a Chance' side of the 'ChanceofHeartAttack' 
# variable, is not normally distributed

# Formal test of normality
# Shapiro-Wilks test
# P value tells us the chances that the sample comes from a normal distribution
# If p > 0.05 then, normally dist.
normality_test <- shapiro.test(ha_cleaned$NumberofMajorVessels)
normality_test$p.value
# p-value = 6.271126e-22 (0.000000000000000000000627)
# This is less than 0.05, so it is not normally distributed

# This test does not work on dicotomous variable
with(ha_cleaned, 
     tapply(NumberofMajorVessels, ChanceofHeartAttack, shapiro.test))
# p-value for 'Less of a Chance' is 4.997e-10 (0.0000000004997)
#This is less than 0.05, so it is not normally distributed
# p-value for 'More of a Chance' is 2.2e-16 (0.00000000000000022)
# This is less than 0.05, so it is not normally distributed

# Therefore results show that both variables, 'NumberofMajorVessels'
# and 'ChanceofHeartAttack', are not normally distributed.

# Now it is time to decide the test to use
# After consulting the chart and both variables being independent,
# the test that will be used is Wilcox test.
# Format of wilcox.test(dependent ~ independent)
wilcox.test(NumberofMajorVessels~ChanceofHeartAttack)
# cut-off = 0.05
# p-value = 1.841e-15 (0.000000000000001841)
# p-value < 0.05 so this indicates the Null (H0) hypothesis is
# rejected. Therefore, the number of major vessels has an effect on 
# having a heart attack. (p = 1.841e-15)



# Research Question 2: Are males more likely to experience heart
# attacks than females?

# H0: Likeliness of a heart attack does not differ by gender
# H1: Likeliness of a heart attack does differ by gender

# The variables that will be used for analysis are 'Sex'
# and 'Chance of Heart Attack'.

# What type of variables are they?
str(ha_cleaned)
# 'Sex' is a categorical dichotomous variable
# 'Chance of Heart Attack' is a categorical dichotomous variable (already 
# converted to this earlier)

# Need to convert the 'Sex' variable to a categorical dichotomous variable
ha_cleaned$`Sex` <- 
  factor(ha_cleaned$`Sex`, labels = 
           c("Female", "Male"))
str(ha_cleaned)

# Check linearity/correlation
attach(ha_cleaned)
plot(Sex, 
     ChanceofHeartAttack, 
     pch = "19", 
     col = "red", 
     main = "Comparison of Sex with Chance of Heart Attack",
     xlab = "Sex", 
     ylab = "Chance of Heart Attack")
detach(ha_cleaned)
# This does not tell much because the two variables are both categorical
# dichotomous so they aren't going to show a trend of linearity

# Instead, we can split the dichotomous variable into 2 & then examine the data
library("lattice")

histogram(~Sex| ChanceofHeartAttack,
          data = ha_cleaned,
          main = "Comparison of Sex with 
          Chance of Heart Attack",
          xlab = "Sex",
          ylab = "Chance of Heart Attack")
# Visual analysis seems to indicate that the data is skewed
# Summarise the medians of the data to confirm that it is not normall dist.
tapply(Sex, ChanceofHeartAttack, median)
# They seem to be off, the center point for 'Less of a Chance' is 1 where
# the center point for 'More of a Chance' is 0
# Let's confirm the distribution





# Research Question 3: Is having exercise induced angina a strong 
# indicator of a person’s likeliness of having a heart attack?

# H0: Exercise induced angina does not indicate likeliness of a heart attack
# H1: Exercise induced angina does indicate likeliness of a heart attack


# The variables that will be used for analysis are 'ExerciseInducedAngina'
# and 'Chance of Heart Attack'.

# What type of variables are they?
str(ha_cleaned)
# 'ExerciseInducedAngina' is a categorical dichotomous variable
# 'Chance of Heart Attack' is a categorical dichotomous variable (already 
# converted to this earlier)

# Need to convert the 'ExerciseInducedAngina' variable to a 
# categorical dichotomous variable
ha_cleaned$`ExerciseInducedAngina` <- 
  factor(ha_cleaned$`ExerciseInducedAngina`, labels = 
           c("No", "Yes"))
str(ha_cleaned)

# Check linearity/correlation
attach(ha_cleaned)
plot(ExerciseInducedAngina, 
     ChanceofHeartAttack, 
     pch = "19", 
     col = "red", 
     main = "Comparison of Number of Exercise Induced Angina with Chance of Heart Attack",
     xlab = "Exercise Induced Angina", 
     ylab = "Chance of Heart Attack")
detach(ha_cleaned)
# This does not tell much because the two variables are both categorical
# dichotomous so they aren't going to show a trend of linearity


histogram(~ExerciseInducedAngina| ChanceofHeartAttack,
          data = ha_cleaned,
          main = "Comparison of Exercise Induced Angina with 
          Chance of Heart Attack",
          xlab = "Exercise Induced Angina",
          ylab = "Chance of Heart Attack")
# Visual analysis seems to indicate that the data is skewed
# Summarise the medians of the data to confirm that it is not normall dist.
tapply(ExerciseInducedAngina, ChanceofHeartAttack, median)
# They seem to be off, the center point for 'Less of a Chance' is 1 where
# the center point for 'More of a Chance' is 0
# Let's confirm the distribution



# Research Question 4: What do the different chest pain types mean in terms
# of likeliness of a heart attack?

#H0: Chest pain types do not have a direct correlation in the likeliness of a heart attack
#H1: Chest pain types do have a direct correlation in the likeliness of a heart attack

# The variables that will be used for analysis are 'ChestPainType'
# and 'Chance of Heart Attack'.

# What type of variables are they?
str(ha_cleaned)
# 'ChestPainType' is a categorical nominal variable
# 'Chance of Heart Attack' is a categorical dichotomous variable (already 
# converted to this earlier)

# Need to convert the 'ChestPainType' variable to a categorical nominal variable
ha_cleaned$`ChestPainType` <- 
  factor(ha_cleaned$`ChestPainType`, labels = 
           c("Typical Angina", 
             "Atypical Angina", 
             "Non-Angina Pain", 
             "Asymptomatic"))
str(ha_cleaned)

# Check linearity/correlation 
attach(ha_cleaned)
plot(ChestPainType, 
     ChanceofHeartAttack, 
     pch = "19", 
     col = "red", 
     main = "Comparison of Number of Chest Pain Type with Chance of Heart Attack",
     xlab = "Chest Pain Type", 
     ylab = "Chance of Heart Attack")
detach(ha_cleaned)
# This does not tell much because the two variables are both categorical
# dichotomous so they aren't going to show a trend of linearity

histogram(~ChestPainType| ChanceofHeartAttack,
          data = ha_cleaned,
          main = "Comparison of Chest Pain Type with 
          Chance of Heart Attack",
          xlab = "Chest Pain Type",
          ylab = "Chance of Heart Attack")
# Visual analysis seems to indicate that the data is skewed
# Summarise the medians of the data to confirm that it is not normall dist.
tapply(ChestPainType, ChanceofHeartAttack, median)
# They seem to be off, the center point for 'Less of a Chance' is 1 where
# the center point for 'More of a Chance' is 0
# Let's confirm the distribution


# Research Question 5: Does a person with high cholesterol have a greater 
# risk of having a heart attack? 

# H0: High cholesterol is not associated with greater risk of having a heart attack
# H1: High cholesterol is associated with greater risk of having a heart attack

# The variables that will be used for analysis are 'Cholesterol'
# and 'Chance of Heart Attack'.

# What type of variables are they?
str(ha_cleaned)
# 'Cholesterol' is a continuous interval variable
# 'Chance of Heart Attack' is a categorical dichotomous variable ((already 
# converted to this earlier)
# No conversions needed

# Check linearity/correlation
attach(ha_cleaned)
plot(Cholesterol, 
     ChanceofHeartAttack, 
     pch = "19", 
     col = "red", 
     main = "Comparison of Cholesterol with Chance of Heart Attack",
     xlab = "Cholesterol", 
     ylab = "Chance of Heart Attack")
detach(ha_cleaned)
# This does not tell much because the two variables are different types

histogram(~Cholesterol| ChanceofHeartAttack,
          data = ha_cleaned,
          main = "Comparison of Cholesterol with 
          Chance of Heart Attack",
          xlab = "Cholesterol",
          ylab = "Chance of Heart Attack")
# Visual analysis seems to indicate that the data is skewed
# Summarise the medians of the data to confirm that it is not normall dist.
tapply(Cholesterol, ChanceofHeartAttack, median)
# They seem to be off, the center point for 'Less of a Chance' is 1 where
# the center point for 'More of a Chance' is 0
# Let's confirm the distribution


# Quantile-quantile plot (Q-Q) plot allows us to check
# if the data is normally distributed or not
# Compare quantiles of both samples 

# is temp normally dist?
qqnorm(Cholesterol)
# Add line that represents normal dist.
qqline(Cholesterol, col = "red")
# Temp appears not to be normally distributed

# Check correlations
pairs(ha_cleaned)


agesex <- heart_attack(age, sex)




