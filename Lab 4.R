#Дисперсионный анализ. Пример "Типы диет"

#Загрузим данные (папку data "Set As A Working Directory")
data = read.csv("data/diet.csv", row.names = 1)
summary(data)
#Ознакомимся со структурой и переименуем колонки, как нам удобно
#data/Diet_data_description.docx
#data/diet.csv

colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])

#Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight

#Проанализиуем есть ли различия по типам диет
boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#Проверим сбалансированные ли данные
table(data$diet.type)

#График групповых средних
install.packages("gplots")
library(gplots)
plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)

#Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
#тест на межгрупповые различия
fit <- aov(weight.loss ~ diet.type, data=data)
summary(fit)

#Попарные различия между средними значениями для всех групп
TukeyHSD(fit)

#Tukey honest significant differences test
install.packages("multcomp")
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")

#ЗАДАНИЕ

#Добавить проверку на выбросы и избавиться от них

data <- na.omit(data) 
data <- data[!(data$diet.type == "A" & data$weight.loss > 8), ]
data <- data[!(data$diet.type == "B" & data$weight.loss < -2), ]

 
#Проанализируем, зависит ли похудение от пола
boxplot(weight.loss ~ gender, data = data, col="light gray",
        ylab = "Weight loss (kg)", xlab = "Gender")
aggregate(data$weight.loss, by = list(data$gender), FUN = summary)

# 1 ANOVA
fit <- aov(weight.loss ~ gender, data = data)
summary(fit)


# 2 ANOVA
fit <- aov(weight.loss ~ diet.type + gender, data = data)
summary(fit)


# Графики зависимости похудения от пола и типа диеты
diet.types <- split(data, f = data$diet.type)
par(mfrow=c(1,3))
boxplot(weight.loss ~ gender, data = diet.types$A, ylim = c(-1, 10), main = "Diet type A")
boxplot(weight.loss ~ gender, data = diet.types$B, ylim = c(-1, 10), main = "Diet type B")
boxplot(weight.loss ~ gender, data = diet.types$C, ylim = c(-1, 10), main = "Diet type C")

# ANCOVA - проанализируем зависимость похудения от диеты, пола, роста, возраста
fit <- aov(weight.loss ~ diet.type + gender + height, data = data)
summary(fit)
fit <- aov(weight.loss ~ diet.type + gender + height + age, data = data)
summary(fit)
