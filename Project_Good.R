### Importing various useful libraries

library(fastDummies)
library(corrplot)
library(lattice)
library(flexmix)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(olsrr)
library(DAAG)
library(moments)
library(forecast)

### Importing the data

life <- read.csv("/Users/zande/Desktop/Data_Science/Second_semester/Statistical_Learning/Project/expectancy.csv")

### Changing the names of the columns in the dataframe

colnames(life)[1] <- "country"
colnames(life)[2] <- "year"
colnames(life)[3] <- "status"
colnames(life)[4] <- "expectancy"
colnames(life)[5] <- "adult_mort"
colnames(life)[6] <- "infant_death"
colnames(life)[7] <- "alcohol"
colnames(life)[8] <- "perc_expend"
colnames(life)[9] <- "hepatitis_b"
colnames(life)[10] <- "measles"
colnames(life)[11] <- "bmi"
colnames(life)[12] <- "under5"
colnames(life)[13] <- "polio"
colnames(life)[14] <- "tot_expend"
colnames(life)[15] <- "diphtheria"
colnames(life)[16] <- "hiv"
colnames(life)[17] <- "gdp"
colnames(life)[18] <- "population"
colnames(life)[19] <- "thin10_19"
colnames(life)[20] <- "thin5_9"
colnames(life)[21] <- "income"
colnames(life)[22] <- "school"

### Cleaning the dataframe, deleting columns or rows if necessary, or simply substituting missing or wrong values if possible

df <- subset(life, select = -c(population))

ix <- which(is.na(df$expectancy))
df$expectancy[ix] <- (df$expectancy[ix -1] + df$expectancy[ix + 1]) / 2

df <- subset(df, country != "South Sudan")
ix <- which(is.na(df$alcohol))
df$alcohol[ix] <- df$alcohol[ix + 1]
df$alcohol[ix] <- df$alcohol[ix + 1]

ix <- which(is.na(df$adult_mort))
df$adult_mort[ix] <- (df$adult_mort[ix -1] + df$adult_mort[ix + 1]) / 2

df <- subset(df, country != "Sudan")
ix <- which(is.na(df$bmi))
df$bmi[ix] <- (df$bmi[ix -1] + df$bmi[ix + 1]) / 2

df <- subset(df, country != "Democratic People's Republic of Korea" & country != "Somalia")
df <- subset(df, country != "Iraq" | year > 2003)
ix <- which(is.na(df$tot_expend))
df$tot_expend[ix] <- df$tot_expend[ix + 1]

ix <- which(is.na(df$diphtheria))
df$diphtheria[ix] <- df$diphtheria[ix - 1]
df$diphtheria[ix] <- df$diphtheria[ix - 1]

df <- df[-c(which(is.na(df$gdp))),]

df <- df[-c(which(is.na(df$income))),]

df <- subset(df, (country != "Montenegro" | year > 2005))
ix <- which(is.na(df$polio))
df$polio[ix] <- df$polio[ix - 1]
df$polio[ix] <- df$polio[ix - 1]

df <- df[-c(which(is.na(df$hepatitis_b))),]

df <- dummy_cols(df, select_columns = 'status')
colnames(df)[22] <- "developed"
colnames(df)[23] <- "developing"
df <- subset(df, select = -c(status, developing))

df <- subset(df, select = -c(country))

### Creating histograms that could show us relevant information
### Creating a logarithmic or power scale, if it helped fit the data to a linear regression

hist(df$expectancy, main="Histogram for Life Expectancy", xlab="Life Expectancy", ylab = "Count", col="cornflowerblue")
skewness(df$expectancy)

hist(df$year, main="Histogram for Year", xlab="Year", ylab = "Count", col="cornflowerblue")
skewness(df$year)

hist(df$adult_mort, main="Histogram for Adult Mortality", xlab="Adult Mortality", ylab = "Count", col="cornflowerblue")
skewness(df$adult_mort)
df$adult_mort <- sqrt(df$adult_mort)
colnames(df)[3] <- "sqrt_adult_mort"
skewness(df$sqrt_adult_mort)
hist(df$sqrt_adult_mort, main="Histogram for Adult Mortality", xlab="sqrt(Adult Mortality)", ylab = "Count", col="cornflowerblue")

hist(df$infant_death, main="Histogram for Infant Deaths", xlab="Infant Deaths", ylab = "Count", col="cornflowerblue")
skewness(df$infant_death)
df$infant_death <- log1p(df$infant_death)
colnames(df)[4] <- "log_infant_death"
skewness(df$log_infant_death)
hist(df$log_infant_death, main="Histogram for Infant Deaths", xlab="log(Infant Deaths)", ylab = "Count", col="cornflowerblue")

hist(df$alcohol,main="Histogram for Alcohol", xlab="Alcohol", ylab = "Count", col="cornflowerblue")
skewness(df$alcohol) #Good

hist(df$perc_expend, main="Histogram for Percentage Expenditure", xlab="Percentage Expenditure", ylab = "Count", col="cornflowerblue")
skewness(df$perc_expend)
df$perc_expend <- log1p(df$perc_expend)
colnames(df)[6] <- "log_perc_expend"
skewness(df$log_perc_expend)
hist(df$log_perc_expend, main="Histogram for Percentage Expenditure", xlab="Percentage Expenditure", ylab = "Count", col="cornflowerblue")

hist(df$hepatitis_b, main="Histogram for Hepatitis B", xlab="Hepatitis B", ylab = "Count", col="cornflowerblue")
skewness(df$hepatitis_b)
df$hepatitis_b <- df$hepatitis_b^3
colnames(df)[7] <- "cube_hepatitis_b"
skewness(df$cube_hepatitis_b)
hist(df$cube_hepatitis_b, main="Histogram for Hepatitis B", xlab="Hepatitis B", ylab = "Count", col="cornflowerblue")

hist(df$measles, main="Histogram for Measles", xlab="Measles", ylab = "Count", col="cornflowerblue")
skewness(df$measles)
df$measles <- log1p(df$measles)
colnames(df)[8] <- "log_measles"
skewness(df$log_measles)
hist(df$log_measles, main="Histogram for Measles", xlab="Measles", ylab = "Count", col="cornflowerblue")

hist(df$bmi, main="Histogram for BMI", xlab="BMI", ylab = "Count", col="cornflowerblue")
skewness(df$bmi) # Good

hist(df$under5, main="Histogram for Under 5 Deaths", xlab="Under 5 Deaths", ylab = "Count", col="cornflowerblue")
skewness(df$under5)
df$under5 <- log1p(df$under5)
colnames(df)[10] <- "log_under5"
skewness(df$log_under5)
hist(df$log_under5, main="Histogram for Under 5 Deaths", xlab="Under 5 Deaths", ylab = "Count", col="cornflowerblue")

hist(df$polio, main="Histogram for Polio", xlab="Polio", ylab = "Count", col="cornflowerblue")
skewness(df$polio)
df$polio <- df$polio^4
colnames(df)[11] <- "pwr4_polio"
skewness(df$pwr4_polio)
hist(df$pwr4_polio, main="Histogram for Polio", lab="Polio", ylab = "Count", col="cornflowerblue")

hist(df$tot_expend, main="Histogram for Total Expenditure", xlab="Total Expenditure", ylab = "Count", col="cornflowerblue")
skewness(df$tot_expend) #Good

hist(df$diphtheria, main="Histogram for Diphtheria", xlab="Diphtheria", ylab = "Count", col="cornflowerblue")
skewness(df$diphtheria)
df$diphtheria <- df$diphtheria^4
colnames(df)[13] <- "pwr4_diphtheria"
skewness(df$pwr4_diphtheria)
hist(df$pwr4_diphtheria, main="Histogram for Diphtheria", xlab="Diphtheria", ylab = "Count", col="cornflowerblue")

hist(df$hiv, main="Histogram for HIV/AIDS", xlab="HIV/AIDS", ylab = "Count", col="cornflowerblue")
skewness(df$hiv)
df$hiv <- 1/df$hiv
colnames(df)[14] <- "recipr_hiv"
skewness(df$recipr_hiv)
hist(df$recipr_hiv, main="Histogram for HIV/AIDS", xlab="HIV/AIDS", ylab = "Count", col="cornflowerblue")

hist(df$gdp, main="Histogram for GDP", xlab="GDP", ylab = "Count", col="cornflowerblue")
skewness(df$gdp)
df$gdp <- log1p(df$gdp)
colnames(df)[15] <- "log_gdp"
skewness(df$log_gdp)
hist(df$log_gdp, main="Histogram for GDP", xlab="GDP", ylab = "Count", col="cornflowerblue")

hist(df$thin10_19, main="Histogram for Thinness 10-19 Years", xlab="Thinness 10-19 Years", ylab = "Count", col="cornflowerblue")
skewness(df$thin10_19)
df$thin10_19 <- log1p(df$thin10_19)
colnames(df)[16] <- "log_thin10_19"
skewness(df$log_thin10_19)
hist(df$log_thin10_19, main="Histogram for Thinness 10-19 Years", xlab="Thinness 10-19 Years", ylab = "Count", col="cornflowerblue")

hist(df$thin5_9, main="Histogram for Thinness 5-9 Years", xlab="Thinness 5-9 Years", ylab = "Count", col="cornflowerblue")
skewness(df$thin5_9)
df$thin5_9 <- log1p(df$thin5_9)
colnames(df)[17] <- "log_thin5_9"
skewness(df$log_thin5_9)
hist(df$log_thin5_9, main="Histogram for Thinness 5-9 Years", xlab="Thinness 5-9 Years", ylab = "Count", col="cornflowerblue")

hist(df$income, main="Histogram for ICOF", xlab="Income Composition Of Resources", ylab = "Count", col="cornflowerblue")
skewness(df$income)
df$income <- df$income^2
colnames(df)[18] <- "sqr_income"
skewness(df$sqr_income)
hist(df$sqr_income, main="Histogram for ICOF", xlab="Income Composition Of Resources", ylab = "Count", col="cornflowerblue")

hist(df$school, main="Histogram for Schooling", xlab="Schooling", ylab = "Count", col="cornflowerblue")
skewness(df$school) #Good

### Creating a couple of boxplot for variables that could be better understood this way

boxplot(df$expectancy ~ df$year, col="orange", border="brown")
boxplot(df$expectancy ~ df$developed, col = "orange", border = "brown")

### Splitting the data into a training and test set, the second one being the last year of the survey

df.test <- subset(df, year == 2015)
df.train <- subset(df, year != 2015)

### Computing a correlation matrix

corrmat <- cor(df.train)
corrplot(corrmat, type = "upper", method = 'circle', tl.col = 'black', cl.ratio = 0.2, col = COL2('PuOr', 10))

### Creating plots to show how the expectancy is correlated to some variables

plot(df.train$sqrt_adult_mort, df.train$expectancy)
abline(lm(expectancy ~ sqrt_adult_mort, data = df.train), col = 'red')

plot(df.train$log_under5, df.train$expectancy)
abline(lm(expectancy ~ log_under5, data = df.train), col = 'red')

plot(df.train$recipr_hiv, df.train$expectancy)
abline(lm(expectancy ~ recipr_hiv, data = df.train), col = 'red')

plot(df.train$sqr_income, df.train$expectancy)
abline(lm(expectancy ~ sqr_income, data = df.train), col = 'red')

plot(df.train$school, df.train$expectancy)
abline(lm(expectancy ~ school, data = df.train), col = 'red')

### Creating the model, in which all the variables were considered

model <- lm(expectancy ~ ., data = df.train)
summary(model)$adj.r.squared
summary(model)$coef

### Applying the backward propagation, in order to find the optimal model

model_back1 <- ols_step_backward_p(model, prem = 0.1, progress = TRUE, details = TRUE)
model_back1$adjr
model_back1$rmse
model_back1$model$coefficients
plot(model_back1)

model_back <- ols_step_backward_p(model, prem = 0.1, progress = TRUE, details = FALSE)
model_back$adjr
model_back$rmse
model_back$model$coefficients
plot(model_back)

### Predicting the results of the final model and checking the residuals

pred <- predict(model_back$model, df.test, se.fit = TRUE)
rmse <- accuracy(pred$fit, df.test$expectancy)[2]
diff <- df.test$expectancy - pred$fit
plot(diff)
abline(h = 0, col = "red")

