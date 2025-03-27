## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

# Import necessary libraries.
library(ggplot2)
library(tidyverse)

# Read file and summary.
rev <- read.csv('turtle_reviews.csv', header = TRUE)
df_rev <- subset(rev, select = -c(language, platform))
colnames(df_rev)[3] <- 'remuneration'
colnames(df_rev)[4] <- 'spending_score'

# Start visualising 'loyalty_points' relationships.
# Start with a simple scatterplot.
# loyalty points vs spending score.
ggplot(df_rev, aes(x=loyalty_points, y=spending_score)) + 
  geom_point()

# loyalty points vs remuneration w/ fill as gender
ggplot(df_rev, aes(x=loyalty_points, y=remuneration, col = gender)) +
  geom_point() +
  theme_classic()

# scatterplot with linear regression line fitted for both genders.
ggplot(df_rev, aes(x=loyalty_points, y=remuneration, col = gender)) +
  geom_point() +
  
  geom_smooth(method = lm)

# With no standard error.
ggplot(df_rev, aes(x=loyalty_points, y=remuneration, col = gender)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE)

ggplot(df_rev, aes(x=loyalty_points, y=spending_score, col = gender)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE)

ggplot(df_rev, aes(x=loyalty_points, y=age, col = gender)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE)


# Count plot of educational background:
ggplot(df_rev, aes(x=education, color = education, fill = education)) +
  geom_bar()

# Checking outliers for remuneration and spending score:
# Boxplots:
boxplot(df_rev$remuneration, col='yellow',
        xlab='Remuneration')
boxplot(df_rev$spending_score, col='red',
        xlab='Spending Score')

# Histograms of various variables:
hist(df_rev$age, col = 'lightblue', main = 'Spread of Customers Ages',
     xlab = 'Ages of Customers', ylab = 'Count')
  
hist(df_rev$remuneration, col = 'blue', main = 'Spread of Customers Earnings',
     xlab = 'Earnings')

# Checking relationship between loyalty points and age:
ggplot(df_rev, aes(x=loyalty_points, y=age, col = gender)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE)

# Looking into possible correlations between the variables:
# New dataset with only numerical variables:
review_num <-subset(df_rev, select = -c(gender, education, review, summary))

cor(review_num)


# From the correlation table we can see that remuneration, 
# spending score seem to be significant for loyalty points.
# Multiple linear regression model with these independent variables
model_a <- lm(loyalty_points ~ remuneration + spending_score, 
              data=review_num)

summary(model_a)

# Checking if age can be a signifcant variable too.
model_b <- lm(loyalty_points ~ remuneration + spending_score
              + age, data=review_num)

summary(model_b)

# Age does seem to be significant as shown by the *** 
# in the summary and it does increase the r2 value as well.

# Going to make prediction based on both models.
pred_a <- predict(model_a, newdata = review_num, 
                      interval = 'confidence')

print(pred_a)

# Model b
pred_b <- predict(model_b, newdata = review_num, 
                       interval = 'confidence')
print(pred_b)

# Extra model for linear regression just with age.
model_c <- lm(loyalty_points ~ age, data = review_num)

# Plotting multiple regression:
# Installing packages.
install.packages("plotly")
install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")
library(ggiraph)
library(ggiraphExtra)
library(plyr)
library(plotly)

# Interactive linear regression with ggPredict.
ggPredict(model_b,se=TRUE,interactive=TRUE)

ggPredict(model_a, se=TRUE, interactive = TRUE)

ggPredict(model_c, se=TRUE, interactive=TRUE)

# Prediction plot:
#effect_plot(model_b, pred = pred_b, interval = TRUE, plot.points = TRUE)

# Plot predicted vs. actual values
ggplot(review_num, aes(x=predict(model_b), 
                       y=loyalty_points, 
                       color = loyalty_points)) + 
  geom_point() +
  geom_abline(intercept=0, slope=1, color = 'darkred') +
  labs(x='Predicted Values', 
       y='Actual Values', 
       title='Predicted vs. Actual Values')

