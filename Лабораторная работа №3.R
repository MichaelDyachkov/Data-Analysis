#Пользуясь примером из лекции файл (6.0.R) проанализируйте данные
#о возрасте и физ. характеристиках молюсков
#https://archive.ics.uci.edu/ml/datasets/abalone
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=TRUE, sep=",")
summary(data)
colnames(data)
colnames(data) <- c("sex", "length", "diameter", "height", 
                    "whole_weight", "shucked_weight",
                    "viscera_weight", "shell_weight", "rings")

colnames(data)
data$sex <- as.factor(data$sex)
par(mfrow=c(1,3)) 
hist(data$diameter, main = "Диаметр, мм")
hist(data$height, main = "Высота, мм")
hist(data$whole_weight, main = "Полный вес, гр")

#Видим ассиметрию https://en.wikipedia.org/wiki/Skewness
#и выбросы (от них нужно избавиться)

# выкидываем выбросы
boxplot(data$diameter)
outliers <- boxplot.stats(data$diameter)$out
data <- data[-which(data$diameter %in% outliers),]

boxplot(data$whole_weight)
outliers <- boxplot.stats(data$whole_weight)$out
data <- data[-which(data$whole_weight %in% outliers),]

boxplot(data$height)
outliers <- boxplot.stats(data$height)$out
data <- data[-which(data$height %in% outliers),]

#Визулизируем возможные зависимости
par(mfrow=c(1,2)) 
plot(data$diameter, data$whole_weight,'p',main = "Зависимость веса от диаметра")
plot(data$height, data$whole_weight,'p',main = "Зависимость веса от высоты")

#Хорошо видна зависимость, нужно её исследовать
#построить линейные модели при помощи функции lm, посмотреть их характеристики
#избавиться от выборосов, построить ещё модели и проверить их
#разделить массив данных на 2 случайные части
#подогнать модель по первой части
#спрогнозировать (функция predict) значения во второй части
#проверить качество прогноза
install.packages("caret")
# разделяем данные на 2 случайные части (выбросы были удалены выше)
library(caret)

train_index <- createDataPartition(data$sex, p = 0.5, list = FALSE)
df_train <- data[train_index, ]
df_test <- data[-train_index, ]

# создаем линейную модель
model <- lm(whole_weight ~ diameter + height, data = df_train)

# смотрим результаты
summary(model)

# генерируем предсказания
predictions <- predict(model, newdata = df_test)

# выводим различные метрики качества
install.packages("tidymodels")
install.packages("magrittr")
install.packages("dplyr")
library(magrittr)
library(dplyr)
# выводим различные метрики качества
library(broom)
glance(model) %>%
dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

