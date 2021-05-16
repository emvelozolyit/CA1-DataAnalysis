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
include_list <- heart_attack[c("age","sex", "trtbps", "caa", "chol", "output")]

# Make the include list the new data frame (ha_cleaned)
# to work with for the rest of analysis
ha_cleaned <- include_list
names(ha_cleaned)

# Modify and update variable names for ha_cleaned data frame 
col_names <- c("Age", 
               "Sex", 
               "RestingBloodPressure",
               "NumberofMajorVessels",
               "Cholesterol", 
               "ChanceofHeartAttack")
colnames(ha_cleaned) <- col_names
names(ha_cleaned)

--------------------------------------------------------------------------------

# RESEARCH QUESTION 1: Does a person with fewer major vessels have an increased 
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
# variable, is possibly normally distributed


with(ha_cleaned, {
  qqnorm(NumberofMajorVessels[ChanceofHeartAttack == "More of a Chance"],
         main = "More of a Chance")
  qqline(NumberofMajorVessels[ChanceofHeartAttack == "More of a Chance"])})
# This reveals that the 'More of a Chance' side of the 'ChanceofHeartAttack' 
# variable, is not normally distributed

# Formal test of normality
# Shapiro-Wilks test
# P value tells us the chances that the sample comes from a normal distribution
# If p > 0.05 then, normally distributed
normality_test <- shapiro.test(ha_cleaned$NumberofMajorVessels)
normality_test$p.value
# p-value = 6.271126e-22 (0.000000000000000000000627)
# This is less than 0.05, so it is not normally distributed

# This test does not work on dichotomous variable
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

--------------------------------------------------------------------------------

# Research Question 2: Are males more likely to experience heart
# attacks than females?

# H0: Males are not more likely to experience heart attacks than females
# H1: Males are more likely to experience heart attacks than females

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
# dichotomous so they aren't going to show a trend of linearity/normality 

# Instead, we can split the dichotomous variable into 2 & then examine the data
library("lattice")

histogram(~Sex| ChanceofHeartAttack,
          data = ha_cleaned,
          main = "Comparison of Sex with 
          Chance of Heart Attack",
          xlab = "Sex",
          ylab = "Chance of Heart Attack")
# Visual analysis sis hard to depict because again, both variables
# are categorical dichotomous 

# Because both variables are categorical dichotomous, the chi-squared test
# will be used to check if the two variables are independent
# Null hypothesis is that the two variables are independent
# Alternative hypothesis is that the two variables are correlated
# cut-off value is 0.05
chisq <- chisq.test(Sex, ChanceofHeartAttack)
chisq$p.value
# p-value = 1.876778e-06 (0.000001877)
# Reject Null and therefore there is a correlation between the two variables
# (p = 1.876778e-06)

--------------------------------------------------------------------------------

# Research Question 3: What does a person’s resting blood pressure say about
# their likeliness of having a heart attack?

# H0: Resting blood pressure is not correlated to the likeliness of a heart attack
# H1: Resting blood pressure is correlated to the likeliness of a heart attack


# The variables that will be used for analysis are 'Resting Blood Pressure'
# and 'Chance of Heart Attack'.

# What type of variables are they?
str(ha_cleaned)
# 'RestingBloodPressure' is a continuous interval variable
# 'Chance of Heart Attack' is a categorical dichotomous variable (already 
# converted to this earlier)
# No conversions needed

# Check linearity/correlation
attach(ha_cleaned)
plot(RestingBloodPressure, 
     ChanceofHeartAttack, 
     pch = "19", 
     col = "red", 
     main = "Comparison of Resting Blood Pressure with Chance of Heart Attack",
     xlab = "Resting Blood Pressure (mm Hg)", 
     ylab = "Chance of Heart Attack")
# This does not tell much because the two variables are different types


# Instead, we can split the dichotomous variable into 2 & then examine the data
library("lattice")
histogram(~RestingBloodPressure| ChanceofHeartAttack,
          data = ha_cleaned,
          main = "Comparison of Resting Blood Pressure with 
          Chance of Heart Attack",
          xlab = "Resting Blood Pressure (mm Hg)",
          ylab = "Chance of Heart Attack")
# Visual analysis seems to indicate that the data is skewed to the right
# Summarise the medians of the data to confirm that it is not normall dist.
tapply(RestingBloodPressure, ChanceofHeartAttack, median)
# The center point for 'Less of a Chance' and for 'More of a Chance' is both 130
# Let's confirm the distribution

# Is RestingBloodPressure normally distributed?
qqnorm(RestingBloodPressure, main = "Resting Blood Pressure (mm Hg) Distribution")
# Add line that represents normal distribution
qqline(RestingBloodPressure, col = "red")
# Resting Blood Pressure appears to not be normally distributed, although it 
# could possibly be argued that it is normally distributed.

# Is ChanceofHeartAttack normally distributed? 
with(ha_cleaned, qqplot(RestingBloodPressure[ChanceofHeartAttack 
                                             == "More of a Chance"],
                        RestingBloodPressure[ChanceofHeartAttack 
                                             == "Less of a Chance"],
                        main = "Comparing Two Samples of Heart Attack Data",
                        xlab = "Less of a Chance",
                        ylab = "More of a Chance"))

# We can add normality line to the plot
# to help evalute normality 
with(ha_cleaned, {
  qqnorm(RestingBloodPressure[ChanceofHeartAttack == "Less of a Chance"],
         main = "Less of a Chance")
  qqline(RestingBloodPressure[ChanceofHeartAttack == "Less of a Chance"])})
# This reveals that the 'Less of a Chance' side of the 'ChanceofHeartAttack' 
# variable, is not normally distributed


with(ha_cleaned, {
  qqnorm(RestingBloodPressure[ChanceofHeartAttack == "More of a Chance"],
         main = "More of a Chance")
  qqline(RestingBloodPressure[ChanceofHeartAttack == "More of a Chance"])})
# This reveals that the 'More of a Chance' side of the 'ChanceofHeartAttack' 
# variable, is normally distributed

# Formal test of normality
# Shapiro-Wilks test
# P value tells us the chances that the sample comes from a normal distribution
# If p > 0.05 then, normally dist.
normality_test <- shapiro.test(ha_cleaned$RestingBloodPressure)
normality_test$p.value
# p-value =  1.458097e-06 (0.00000146)
# This is less than 0.05, so it is confirmed that it is not normally distributed

# This test does not work on dichotomous variable
with(ha_cleaned, 
     tapply(RestingBloodPressure, ChanceofHeartAttack, shapiro.test))
# p-value for 'Less of a Chance' is 8.365e-05 (0.00008365)
#This is less than 0.05, so it is not normally distributed
# p-value for 'More of a Chance' is 0.0119
# This is also less than 0.05, so it is not normally distributed

# Therefore results show that 'RestingBloodPressure', 'Less of a Chance' of a 
# heart attack, and 'More of a Chance' of a heart attack are all
# not normally distributed. 

# Now it is time to decide the test to use
# After consulting the chart and both variables being independent,
# the test that will be used is Wilcox test.
# Format of wilcox.test(dependent ~ independent)
wilcox.test(RestingBloodPressure~ChanceofHeartAttack)
# cut-off = 0.05
# p-value = 0.03465
# p-value < 0.05 so this indicates the Null (H0) hypothesis is
# rejected. Therefore, resting blood pressure is correlated to the 
# likeliness of a heart attack

--------------------------------------------------------------------------------

# Research Question 4: Does a person’s age have any correlation with their 
# cholesterol levels?

# H0: A persons age does not have a direct correlation to cholesterol levels
# H1: A persons age does have a direct correlation to cholesterol levels

# The variables that will be used for analysis are 'Age' and 'Cholesterol' 

# What type of variables are they?
str(ha_cleaned)
# 'Age' is a continuous interval variable
# 'Cholesterol' is also a continuous interval variable
# No conversions need to be done

# Check linearity/correlation 
attach(ha_cleaned)
plot(Age, 
     Cholesterol, 
     pch = "19", 
     col = "red", 
     main = "Comparison of Number of Age and Cholesterol",
     xlab = "Age", 
     ylab = "Cholesterol (mg/dl)")
detach(ha_cleaned)
# It is hard to decipher if there is a correlation between these two variables
# However, if there is a correlation it is a positive one

# We can usea QQ plot to show the correlation between both vars
with(ha_cleaned,
     qqplot(Age, Cholesterol,
            main = "Correlation between Age and Cholesterol",
            xlab = "Age",
            ylab = "Cholesterol (mg/dl)"))

# Visualize the normality of the variables 
opar = par(no.readonly = TRUE)
# arrange the plots in 1 row by 2 cols
par(mfrow = c(1,2)) # two charts side by side
hist(Age, col = "red", main = "Distribution of Age", 
     xlab = "Age")
hist(Cholesterol, col = "red", main = "Distribution Cholesterol (mg/dl)",
     xlab = "Cholesterol (mg/dl)")
par = opar
# Distribution of Age looks like it might be normally distributed, but could
# also  be argued that it is not
# Distribution of Cholesterol looks skewed to the right
# However, still cannot make definite conclusions about the distributions 

# Create a normal QQ-plot of Age and Cholesterol values
# and we can add normality line
# which is important to evaluate normality

with(cars, {qqnorm (Age, 
                    main = "Normal QQ-Plot of Age Data",
                    xlab = "Theoretical Quantiles",
                    ylab = "Sample Quantiles")
  qqline(Age)
})

with(cars, {qqnorm (Cholesterol, 
                    main = "Normal QQ-Plot of Cholesterol Data",
                    xlab = "Theoretical Quantiles",
                    ylab = "Sample Quantiles")
  qqline(Cholesterol)
})

# Argue point either way about these variables normalizations
# Both the Age data and Cholesterol data have outliers far from line of fit, but
# the line also fits the data pretty well
# At this point, Distribution of Age plot shows that the data may be normally 
# distributed and the Normal QQ Plot of Age Data shows there is a likely
# chance it is not normally distributed due, to some outliers off the line. 
# As for Cholesterol, the Distribution of Cholesterol 
# appears to be skewed to the right, so not normally distributed, and the 
# Normal QQ Plot of Cholesterol shows there is a likely
# chance it is not normally distributed due, to some outliers off the line.


# We can run the formal test of normality (final decision)
# provided through widely used shapiro-wilks test
# pvalue tells us the chance that the sample comes from  a normal distribution
# if p < 0.05 then variable is not normally distributed
# if p > 0.05 then it is normally distributed
normality_test <- shapiro.test(Age)
normality_test$p.value
# pvalue is 0.005798359
# [1] 0.005798359 < 0.05 so it is in fact not normally distributed

normality_test <- shapiro.test(Cholesterol)
normality_test$p.value
# pvalue is 5.364848e-09 (0.00000000536) so it is also not normally distributed

# Both variables are not normally distributed
# will use the spearman's correlation coefficient test
attach(ha_cleaned)
cor.test(Age, Cholesterol, method = "spearman", exact = FALSE)
# pvalue = 0.0006099
# 0.0006099 < 0.05, therefore we reject H0, and accepts that persons age 
# does have a direct correlation to cholesterol levels

--------------------------------------------------------------------------------

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
     xlab = "Cholesterol (mg/dl)", 
     ylab = "Chance of Heart Attack")
# This does not tell much because the two variables are different types


# Instead, we can split the dichotomous variable into 2 & then examine the data
library("lattice")

histogram(~Cholesterol | ChanceofHeartAttack,
          data = ha_cleaned,
          main = "Distribution of Cholesterol with 
          Chance of Heart Attack",
          xlab = "Cholesterol (mg/dl)",
          ylab = "Chance of Heart Attack")
# Visual analysis seems to indicate that the 'Less of a Chance' of a 
# heart attack data is normally distributed. The 'More of a Chance' of a heart
# attack data seems to be skewed to the right and not normally distributed

# Summarise the medians of the data to confirm that it is not normally distributed
tapply(Cholesterol, ChanceofHeartAttack, median)
# They seem to be off, the center point for both sides of the variable are different.
# The center point for 'Less of a Chance' is 249 mg/dl
# Where the center point for 'More of a Chance' is 234 mg/dl
# Let's confirm the distribution

# Is Cholesterol normally distributed?
qqnorm(Cholesterol, main = "Cholesterol Distribution")
# Add line that represents normal distribution
qqline(Cholesterol, col = "red")
# Cholesterol appears to be normally distributed


# Is ChanceofHeartAttack normally distributed? 
with(ha_cleaned, qqplot(Cholesterol[ChanceofHeartAttack == "More of a Chance"],
                        Cholesterol[ChanceofHeartAttack == "Less of a Chance"],
                        main = "Comparing Two Samples of Heart Attack Data",
                        xlab = "Less of a Chance",
                        ylab = "More of a Chance"))

# We can add normality line to the plot
# to help evalute normality 
with(ha_cleaned, {
  qqnorm(Cholesterol[ChanceofHeartAttack == "Less of a Chance"],
         main = "Less of a Chance")
  qqline(Cholesterol[ChanceofHeartAttack == "Less of a Chance"])})
# This reveals that the 'Less of a Chance' side of the 'ChanceofHeartAttack' 
# variable, is normally distributed


with(ha_cleaned, {
  qqnorm(Cholesterol[ChanceofHeartAttack == "More of a Chance"],
         main = "More of a Chance")
  qqline(Cholesterol[ChanceofHeartAttack == "More of a Chance"])})
# This reveals that the 'More of a Chance' side of the 'ChanceofHeartAttack' 
# variable, is not normally distributed

# Formal test of normality
# Shapiro-Wilks test
# P value tells us the chances that the sample comes from a normal distribution
# If p > 0.05 then, normally dist.
normality_test <- shapiro.test(ha_cleaned$Cholesterol)
normality_test$p.value
# p-value = 5.364848e-09 (0.00000000536)
# This is less than 0.05, so it is actually not normally distributed

# This test does not work on dichotomous variable
with(ha_cleaned, 
     tapply(Cholesterol, ChanceofHeartAttack, shapiro.test))
# p-value for 'Less of a Chance' is 0.3792
#This is more than 0.05, so it is normally distributed
# p-value for 'More of a Chance' is 3.079e-09 (0.000000003079)
# This is less than 0.05, so it is not normally distributed

# Therefore results show that 'Cholesterol' is not normally distributed,
# 'Less of a Chance' of a heart attack is normally distributed, and 'More of 
# a Chance' of a heart attack is not normally distributed. 

# Now it is time to decide the test to use
# After consulting the chart and variables being both dependent and independent,
# the test that will be used is Wilcox test.
# Format of wilcox.test(dependent ~ independent)
wilcox.test(Cholesterol~ChanceofHeartAttack)
# cut-off = 0.05
# p-value = 0.03572 
# p-value < 0.05 so this indicates the Null (H0) hypothesis is
# rejected. Therefore, high cholesterol is associated with greater risk 
# of having a heart attack (p = 0.03572)




