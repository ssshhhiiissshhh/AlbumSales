# установка пакетов
install.packages("PerformanceAnalytics")
install.packages("car")
install.packages("tidyverse")
install.packages("gvlma")


# считываем данные. передаем первую строку таблицы, как названия переменных
sales <- read.table("Album Sales 2.dat",header=TRUE)

# основные статистические характеристики данных: 
# минимум, максимум, медиану, среднее и квантили для каждой переменной
summary(sales) 

# подключаем пакет для использования в текущей сессии
library(PerformanceAnalytics)

# из библиотеки PerformanceAnalytics
chart.Correlation(sales)

# строим модель, взяв все переменные
lmS<-lm(sales ~ adverts + airplay + attract, data = sales)

# детали модели, включая коэффициенты регрессии, их значимость и общую статистику модели
summary(lmS)

#графики
plot(lmS)

# проверяем распределение и дисперсию остатков
car::qqPlot(lmS)
car::residualPlot(lmS, type="response")


gvlma::gvlma(lmS)

coefficients(lmS)

# доверительные интервалы для всех независимых параметров модели (уровень значимости 0.05, по умолчанию)
confint(lmS)

ggstatsplot::ggcoefstats(lmS)

lmS.diag<-fortify(lmS)
lmS.diag
fortify(lmS)

# фактор инфляции дисперсии (VIF) — мера мультиколлинеарности
car::vif(lmS)

# по очереди удаляем по одной переменной, проверяя, улучшается ли модель (смотрим adjusted R^2)

lmS1<-lm(sales ~ adverts + airplay, data = sales)
summary(lmS1)

lmS2<-lm(sales ~ attract + airplay, data = sales)
summary(lmS2)

lmS3<-lm(sales ~ adverts + attract, data = sales)
summary(lmS3)

anova(lmS1, lmS)
anova(lmS2, lmS)
anova(lmS3, lmS)

