# CA1- Data Analysis
# Heart Attack Analysis

# Heart Attack dataset (heart.csv) contains information about patients and 
# their chance of having a heart attack. 

# Data Preparation 

# Read in the heart.csv dataset into a new data frame called heart_attack
heart_attack <- read.csv("heart.csv")
str(heart_attack)

# See if this dataset has any missing data
incomplete_data <- heart_attack[!complete.cases(heart_attack),]
incomplete_data
#or visualize the data for missing variables
md.pattern((heart_attack))
# Both complete.cases and mice md.pattern reveal that there is no missing data
# in the dataset, so there is no need to worry about removing na's


# Check correlations
library(psych)

pairs.panels(heart_attack,
             smooth = TRUE, # IF TRUE, draws loess smooth
             scale = FALSE, # IF TRUE, scales the correlation text font
             density = TRUE, # IF TRUE, adds density plots and histograms
             ellipses = TRUE, #IF TRUE, draws ellipses
             method = "spearman", # Correlation method (pearson or kendall)
             pch = 21, #pch symbol
             lm = FALSE, #IF TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # IF TRUE, reports correlations
             jiggle = FALSE, # IF TRUE, data points are jittered
             factor = 2, #Jittering factor
             hist.col = 4, #Histogram color
             stars = TRUE, # Stars after numbers, means there is some type of correlation
             ci = TRUE) 
