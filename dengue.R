#Set the directory
setwd("F:/Google Drive/UoD/Advanced Analytics/Data Sets")
#defining variables
climate <- read.csv("https://s3.amazonaws.com/drivendata/data/44/public/dengue_features_train.csv")
dengue <- read.csv("https://s3.amazonaws.com/drivendata/data/44/public/dengue_labels_train.csv")
test <- read.csv("F:/Google Drive/UoD/Advanced Analytics/Data Sets/dengue_features_test.csv")


sj_dengue <- dengue[dengue$city == "sj",]
dim(sj_dengue)
sj_climate <- climate[climate$city == "sj",]
dim(sj_climate)
dim(sj_dengue)
data <- cbind(sj_climate, sj_dengue)


#delete unneccesary columns
data <- data[, -c(1,5,6,25,26,27)]

#checking na rows
nrow(data[!complete.cases(data),])

#Handling NA in train data 
data[22] <- lapply(data[22], as.numeric)
sapply(data, function(x) sum(is.na(x)))
sum(is.na(data))
mean(is.na(data))
colMeans(is.na(data))
c_data <- na.omit(data)

#Handling NA in test data 
test <- test[, -c(4,5)]
test[22] <- lapply(test[22], as.numeric)
sapply(test, function(x) sum(is.na(x)))
sum(is.na(test))
mean(is.na(test))
colMeans(is.na(test))
c_test <- na.omit(test)



#Plotting
#Histogram of week of year

require(plotly)
library(plotly)
barplot_of_dengue <- plot_ly(
x = data$weekofyear,
y = data$total_cases,
name = "Barchart",
type = "bar"
) %>%
layout(title = "Dengue cases by week of year")

# Basic line plot with points
library(plotly)

p <- plot_ly(data, x = data$year, y = data$total_cases, type = 'scatter', mode = 'lines')



#first linear model

model1 <- lm(total_cases ~  
                      ndvi_se + ndvi_sw + precipitation_amt_mm + 
                      reanalysis_air_temp_k + reanalysis_avg_temp_k + 
                      reanalysis_dew_point_temp_k + reanalysis_max_air_temp_k + 
                      reanalysis_min_air_temp_k + reanalysis_precip_amt_kg_per_m2 + 
                      reanalysis_relative_humidity_percent + reanalysis_sat_precip_amt_mm + 
                      reanalysis_specific_humidity_g_per_kg + reanalysis_tdtr_k + 
                      station_avg_temp_c + station_diur_temp_rng_c + 
                      station_max_temp_c + station_min_temp_c + station_precip_mm, 
                    data = c_data)
#second linear model
model2 <- lm(total_cases ~ reanalysis_max_air_temp_k + 
                      reanalysis_specific_humidity_g_per_kg + station_diur_temp_rng_c + 
                      station_max_temp_c, data = c_data)

#finale linear model by automatic backward elimination 
library(MASS)
step <- stepAIC(model1, direction="backward", trace = "FALSE")
step$anova

model3 <- lm(total_cases ~ precipitation_amt_mm + reanalysis_avg_temp_k + 
            reanalysis_dew_point_temp_k + reanalysis_max_air_temp_k + 
            reanalysis_precip_amt_kg_per_m2 + reanalysis_specific_humidity_g_per_kg + 
            reanalysis_tdtr_k + station_diur_temp_rng_c + station_max_temp_c, data = c_data)

anova(model1, model2, model3)
par(mfrow = c(2,2))
plot(model3)

# load the libraries
library(caret)
library(klaR)

# make predictions
prediction <- predict(model3, c_test)
c_test$prediction <- prediction
actuals_preds <- data.frame(cbind(actuals=c_test$total_cases, predicteds=prediction)) 
correlation_accuracy <- cor(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
library(Metrics)
mse(actuals_preds$actuals, actuals_preds$predicteds)
require(ggplot2)
ggplot(test, aes(x = total_cases, y = predictedLin)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)

xyplot(data = actuals_preds, group = ind, auto.key = TRUE)
# summarize results
confusionMatrix(predictions$class, y_test)






time <- lm(c_data$total_cases ~ year + weekofyear + week_start_date, data=c_data)
prec <- predict(lm3, test, interval = 'confidence')
#Regfression tree
library(rpart)
library(rattle)
library(RColorBrewer)
library(rpart.plot)

rt.a1 <- rpart(data$total_cases ~ ., data = data[,4:23])
summary(rt.a1)

summary(c_data$total_cases)

p.rpart <- predict(rt.a1,data= test)
summary(p.rpart)

summary(data$total_cases)
