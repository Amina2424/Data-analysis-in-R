# Я буду не удалять, а комментировать вызываемые команды, так как Вы говорили, что висячие строки - red flag :)

# Импорт библиотек 
library(ggplot2)
library(nortest)  # Для теста Андерсона
library(sjstats)  # Mann-Whitney U Test
library(effsize)
library(corrplot)
library(MASS)   
library(compareGroups)
library(caret)
library(dplyr)
library(readr)
library(nnet)
library(pROC)
library(ggstatsplot)


                                         # ЗНАКОМСТВО С ДАННЫМИ

df = read.delim('heart_2020_cleaned.csv', sep = ',')

                          # 1. Определение типа переменных и предварительная фильтрация
#str(df)
# 319795 наблюдений и 18 признаков, из которых 4 являются числовыми (numeric, среди которых 1 -double и 3 integer), 
# 14 категориальными (символьный тип - character): 11 бинарные номинальные, 1 мультиноминальные и 2 порядковые

# проверка на пропущенные значения
# sum(is.na(df))
# 0 - в данных отсуствуют пропущенные значения

# провека на дупликаты
#sum(duplicated(df)) # [1] 18078 значений, их удаляю
df = df[!duplicated(df), ]
# 301717 rows

#общая статистика количественных данных
summary(df[c("BMI","PhysicalHealth","MentalHealth","SleepTime")])
#         BMI        PhysicalHealth    MentalHealth      SleepTime     
# Min.   :12.02   Min.   : 0.000   Min.   : 0.000   Min.   : 1.000  
# 1st Qu.:24.03   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 6.000  
# Median :27.41   Median : 0.000   Median : 0.000   Median : 7.000  
# Mean   :28.44   Mean   : 3.572   Mean   : 4.121   Mean   : 7.085  
# 3rd Qu.:31.65   3rd Qu.: 2.000   3rd Qu.: 4.000   3rd Qu.: 8.000  
# Max.   :94.85   Max.   :30.000   Max.   :30.000   Max.   :24.000  

# Наблюдения: при указанной медиане и среднем изначально была мысль, что данных распределены нормально, 
# но после я увидела максимальное число в каждом признаке и решила проверить гипотезу, что в данных имеются выбросы

# Анализ данных на выбросы
boxplot(df$BMI, main = "Outliners for BMI", col = "darkred")$out #7905 выбросов
boxplot(df$PhysicalHealth, main = "Outliners for Physical Health", col = "lightblue")$out #46136 выбросов
boxplot(df$SleepTime, main = "Outliners for Sleep Time", col = "green")$out #3542 выбросов
boxplot(df$MentalHealth, main = "Outliners for MentalHealth", col = "#B3EE3A")$out #38713 выбросов 

#для дальнейшей работы создам два списка с названиями столбцов отдельно категориальных и числовых переменных
numeric_cols <- names(df)[sapply(df, is.numeric)]
categorical_cols <- names(df)[sapply(df, is.character)]

# функция для удаления выбросов, выходящих за q1 и q3 квантили (ниже 25% и выше 75%)
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# применила к признакам, содержащим огромное количество выбросов: BMI и Mental Heath
df$BMI <- remove_outliers(df$"BMI")
boxplot(df$BMI, main = "Outliners for BMI", col = "darkred")$out
df$MentalHealth <- remove_outliers(df$MentalHealth)
boxplot(df$MentalHealth, main = "Outliners for MentalHealth", col = "#B3EE3A")$out 

sum(is.na(df)) # обновила пропущенные значения после выбросов и удалила 48618 наблюдений
df = na.omit(df)
# Итоговое число наблюдений:255233, 0 пропущенных значений и 0 дупликатов, уменьшено количество выбросов

#Для дальнейшего анализа все категориальные переменные переведу в факторы

df$AgeCategory <- factor(df$AgeCategory, 
                         levels = c("18-24", "25-29", "30-34", "35-39", 
                                    "40-44", "45-49", "50-54", "55-59", 
                                    "60-64", "65-69", "70-74", "75-79", "80 or older"), 
                         ordered = TRUE)
df$GenHealth <- factor(df$GenHealth, 
                       levels = c("Poor", "Fair", "Good", "Very good", "Excellent"), 
                       ordered = TRUE)
df$PhysicalActivity <- as.factor(df$PhysicalActivity)
df$Smoking <- as.factor(df$Smoking)
df$AlcoholDrinking <- as.factor(df$AlcoholDrinking)
df$Stroke <- as.factor(df$Stroke)
df$DiffWalking <- as.factor(df$DiffWalking)
df$Sex <- as.factor(df$Sex)
df$Diabetic <- as.factor(df$Diabetic)
df$Asthma <- as.factor(df$Asthma)
df$KidneyDisease <- as.factor(df$KidneyDisease)
df$SkinCancer <- as.factor(df$SkinCancer)
df$HeartDisease <- as.factor(df$HeartDisease)
df$GenHealth <- factor(df$GenHealth, 
                       levels = c("Poor", "Fair", "Good", "Very good", "Excellent"), 
                       ordered = TRUE)
df$Race <- as.factor(df$Race)

                                         # БАЗОВАЯ ВИЗУАЛИЗАЦИЯ

                                    # 1. Анализ числовых переменных

# Гистограммы для числовых переменных: беглая оценка для оценки распределения данных (по f(x) = F'(x))

hist(df$BMI, main = paste("Distribution of BMI"), freq=FALSE, ylim=c(0,0.08), xlab = 'BMI', col = '#698B22')
lines(density(df$BMI), col="red", lwd=2)
hist(df$PhysicalHealth, main = paste("Distribution of Physical Health"), freq=FALSE, ylim=c(0,0.08), xlab = 'Physical Health', col = '#B3EE3A')
lines(density(df$PhysicalHealth), col="red", lwd=2)
hist(df$SleepTime, main = paste("Distribution of Sleep Time"),  freq=FALSE, ylim=c(0,0.4), xlab = 'Sleep Time', col = '#6B8E23')
lines(density(df$SleepTime), col="red", lwd=2)
hist(df$MentalHealth, main = paste("Distribution of Mental Health"), freq=FALSE, ylim=c(0,0.4), xlab = 'Mental Health', col = '#4EEE94')
lines(density(df$MentalHealth), col="red", lwd=2)

# можно предположить на основе полученной визуализации, что данные распределены отлично от нормального
# стремится к нормальному распределению BMI

# Построю для общей картины в отчет все в одну уже не по плотности, а частоте (хотела еще повторить применение циклов в R)
par(mfrow = c(2, 2))
for (col in numeric_cols) {
  hist(df[[col]], main = paste("Distribution of", col), xlab = col, col = c('#698B22', '#6B8E23', '#B3EE3A','#4EEE94'))
}

# Визуальная проверка на нормальность данных при помощи QQ-plot
par(mfrow = c(2, 2))
for (col in numeric_cols) {
  qqnorm(df[[col]], main = paste("Q-Q Plot of", col))
  qqline(df[[col]], col = "red")
}

# Данные не ложатся на прямую, поэтому предположительно считаем, что данные отклоняются от нормального распределения
# Проверю ниже при помощи уже статистических тестов

                           # 2. Базовая визуализация категориальных переменных
# Барплоты отдельно для категориальных переменных: оценка сбалансированности данных внутри одного признака
for (col in categorical_cols) {
  print(ggplot(df, aes_string(x = col)) + 
          geom_bar(fill = '#87CEFA') + 
          labs(title = paste("Distribution of", col)) + theme_bw())
}

# Как видно из представленных результатов, все признаки кроме пола несбалансированы,что предполагает
# ограничения на анализ данных и возможные проблемы с анализом


                                # СТАТИСТИЧЕСКИЕ МЕТОДЫ АНАЛИЗА

                                # 1. Анализ числовых переменных
# при анализе числовых переменных важно проверить данные на нормальное распределение. Делается это
# при помощи двух шагов: QQ-plot и/или гистограмма (выше) и статистические тесты для проверки на нормальность

# Проверка на нормальность данных при помощи статистического теста

# Нулевая гипотеза (Н0): проверяемый признак (переменная) подчиняется закону нормального распределения
# Альтернативная гипотеза (Н1): переменная не подчиняется закону нормального распределения

# тест Андерсона-Дарлинга

# особенно чувствителен к отклонениям в хвостах распределения, что делает его полезным для обнаружения различий даже в экстремальных значениях

anderson_bmi <- ad.test(df$BMI) # A = 1390.7, p-value < 2.2e-16
anderson_PhysicalHealth <- ad.test(df$PhysicalHealth) # A = 61245, p-value < 2.2e-16
anderson_MentalHealth <- ad.test(df$MentalHealth) # A = 47931, p-value < 2.2e-16
anderson_SleepTime <- ad.test(df$SleepTime) # A = 8389.7, p-value < 2.2e-16

# тест Шапиро-Уилка не подошел из-за более 5к количества наблюдений в выборке, но подходит тест КС

# тест Колмагорова-Смирнова
KS_bmi <- ks.test(df$BMI, "pnorm", mean(df$BMI, na.rm = TRUE), sd(df$BMI, na.rm = TRUE)) # D = 0.061346, p-value < 2.2e-16
KS_PhysicalHealth <- ks.test(df$PhysicalHealth, "pnorm", mean(df$PhysicalHealth, na.rm = TRUE), sd(df$PhysicalHealth, na.rm = TRUE)) # D = 0.3824, p-value < 2.2e-16
KS_MentalHealth <- ks.test(df$MentalHealth, "pnorm", mean(df$MentalHealth, na.rm = TRUE), sd(df$MentalHealth, na.rm = TRUE)) # D = 0.17869, p-value < 2.2e-16
KS_SleepTime <- ks.test(df$SleepTime, "pnorm", mean(df$SleepTime, na.rm = TRUE), sd(df$SleepTime, na.rm = TRUE)) # D = 0.17869, p-value < 2.2e-16

# Согласно полученным результатам, мы отклоняем нулевую гипотезу во всех 4 случаях 
# принимаем альтернативную гипотезу: все четыре признака не подчиняются закону нормального распределения 
# п.с. Хотя мне показалось, что индекс массы находился близко к нормальному распределнию, но мы подтвердили
# при помощи стат теста, что нет, поэтому полезно комбинировать и визуализацию, и тесты

                               # 2. Корелляционный анализ между числовыми переменными

# Нулевая гипотеза (H0): среднее по BMI статистически не отличается между мужчинами и женщинами в выборке
# Альтернативная гипотеза (Н1): среднее по BMI статистически отличается между мужчинами и женщинами 

# Большая выборка, непараметрический тест Манна-Уитни, группы считаются независимыми 

wilcox.test(BMI ~ Sex, data = df, alternative = "two.sided", conf.level = 0.95)
# W = 7300283454, p-value < 2.2e-16

# согласно полученному результату мы отклоняем нулевую гипотезу и принимаем альтернативную, что 
# средние между двумя группами статитически значимо отличимы

# я решила посмотреть на средние в двух группах, но почему-то с установленной задачей ответ не сошелся

mean(df[df$Sex == 'Female','BMI']) # [1] 27.39322
mean(df[df$Sex == 'Male','BMI']) # [1] 28.15741

# построила график плотности, чтобы посмотреть, как распределены относительно друг друга две группы

ggplot(df, aes(x = BMI, fill = Sex)) +  
  geom_density(alpha = 0.5) + 
  labs(title="Body mass index distribution density among men and women", 
       y="Density", x = "Body mass index") + 
  theme_bw()

# согласно полученным данным, несмотря на полученный результат, я вижу, что среднее в двух группах 
# по массе тела практически не отличается. Возможно, при большом количестве наблюдений более маленькие изменения
# между группами принимаются за статистически значимые

# Коэффициент корреляции
# Для количественных переменных (BMI, SleepTime, PhysicalHealth, MentalHealth)

cor(df$BMI, df$MentalHealth, method = 'spearman') # -0.0243326
cor(df$BMI, df$SleepTime, method = 'spearman') # -0.05401905
cor(df$BMI, df$PhysicalHealth, method = 'spearman') # 0.05941798

ggstatsplot::ggcorrmat(
  data = df,
  type = "nonparametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue")
)

# Все три корреляции имеют значения, близкие к нулю, что говорит о слабых 
# или практически отсутствующих монотонных зависимостях между индексом массы тела (BMI) и остальными переменными

                               # 3. Анализ категориальных переменных
# Целевая переменная  - Heart Disease 
# Критерий выбора статистического теста: 2 сравниваемые группы, бинарные катег переменные с большой выборкой - критерий хи-квадрат Пирсона
# Критерий хи-квадрат согласия Пирсона по переменной Heart Disease

chi <- table(df$HeartDisease,df$AgeCategory) 
print(chisq.test(chi)) 
# X-squared = 16561, df = 12, p-value < 2.2e-16
chi2 <- table(df$HeartDisease,df$GenHealth) 
print(chisq.test(chi2)) 
# X-squared = 15872, df = 4, p-value < 2.2e-16
chi3 <- table(df$HeartDisease,df$Race) 
print(chisq.test(chi3))
# X-squared = 1039.8, df = 5, p-value < 2.2e-16
chi4 <- table(df$HeartDisease,df$Smoking) 
print(chisq.test(chi4)) 
# X-squared = 2750.6, df = 1, p-value < 2.2e-16
chi5 <- table(df$HeartDisease,df$Sex) 
print(chisq.test(chi5)) 
# X-squared = 1631, df = 1, p-value < 2.2e-16
chi6 <- table(df$HeartDisease,df$AlcoholDrinking) 
print(chisq.test(chi6))
# X-squared = 331.16, df = 1, p-value < 2.2e-16
chi7 <- table(df$HeartDisease,df$Diabetic) 
print(chisq.test(chi7)) 
# X-squared = 7629.8, df = 3, p-value < 2.2e-16
chi8 <- table(df$HeartDisease,df$GenHealth) 
print(chisq.test(chi2)) 
# X-squared = 15872, df = 4, p-value < 2.2e-16
chi9 <- table(df$HeartDisease,df$PhysicalActivity) 
print(chisq.test(chi9))
# X-squared = 1767.6, df = 1, p-value < 2.2e-16
chi10 <- table(df$HeartDisease,df$Asthma) 
print(chisq.test(chi10)) 
# X-squared = 196.45, df = 1, p-value < 2.2e-16
chi11 <- table(df$HeartDisease,df$SkinCancer) 
print(chisq.test(chi11)) 
# X-squared = 2129.6, df = 1, p-value < 2.2e-16
chi12 <- table(df$HeartDisease,df$KidneyDisease) 
print(chisq.test(chi12))
# X-squared = 4510.4, df = 1, p-value < 2.2e-16
chi13 <- table(df$HeartDisease,df$Stroke) 
print(chisq.test(chi13))
# X-squared = 8621, df = 1, p-value < 2.2e-16
chi14 <- table(df$HeartDisease,df$DiffWalking) 
print(chisq.test(chi14))
# X-squared = 8478, df = 1, p-value < 2.2e-16

# Все тесты дают очень низкие p-value (меньше 2.2e-16), что означает, что 
# все связи между сердечными заболеваниями и другими переменными статистически значимы

                          # 4. Сравнение количественного признака в трех и более групп

# Группирование происходит по признаку Race (категориальная номинальная, 3 и более групп)
# непараметрический аналог – критерий Краскела-Уолиса, который может
# быть использован в случае, если исследуемая величина не имеет
# нормального распределения или дисперсии не равны между группами и число сравниваемых групп три и более

# Применяем критерий Краскела-Уолиса для каждой количественной переменной
kruskal_results <- lapply(numeric_cols, function(var) {
  test <- kruskal.test(df[[var]] ~ df$Race)
  data.frame(Variable = var, P_Value = test$p.value)
})

# хочу для красоты результаты в таблицу
kruskal_results_df <- do.call(rbind, kruskal_results)
print(kruskal_results_df)

# Variable       P_Value
# 1            BMI  0.000000e+00
# 2 PhysicalHealth 1.093667e-143
# 3   MentalHealth  1.125231e-63
# 4      SleepTime 2.144990e-159

# Согласно полученным результатам, каждый из признаков указывает на наличие значимых различий 
# в показателях между группами по расе


                      # 5. Сравнение групп в данных по признаку заболеваний сердца

description <- descrTable(HeartDisease~Smoking+GenHealth+Race+Diabetic+Sex+DiffWalking+Stroke+KidneyDisease,data=df,
           Q1=0,
           Q3=1)

# согласно полученным результатам (я понимаю, что признаки несбалансированы, поэтому возможно резкое "заявление") я бы сделала следующие выводы:
# Люди с сердечными заболеваниями чаще курят (57.3%) по сравнению с теми, у кого нет заболеваний (39.2%), 
# имеют более низкое общее состояние здоровья (24.1% против 9.8%) 
# и чаще страдают от диабета (31.2% против 11%)
# мужчины чаще страдают от заболеваний (61.9% против 47.7% у женщин) - здесь может и правда, потому этот признак сбалансирован
# Люди с сердечными заболеваниями имеют больше проблем с движением (31.6% против 10.4%), 
# а также страдают от инсульта (14.9% против 2.61%) и заболеваний почек (11.6% против 2.84%).

                         # АНАЛИЗ ЦЕЛЕВОЙ ПЕРЕМЕННОЙ: ПРОДВИНУТАЯ ВИЗУАЛИЗАЦИЯ

# В свою работу я определяю целевую переменную Heart Disease
# Посмотрю от нее зависимость от некоторых категориальных переменных

ggplot(df, aes(x = HeartDisease, fill = Diabetic)) +
  geom_bar(position = "dodge") +
  theme_bw() +
  labs(title="Количество заболеваний сердца среди диабетиков и не диабетиков", y="Количество", x = "Заболевание сердца") + 
  theme(plot.title=element_text(size=10,
                                face="bold",
                                family="American Typewriter"),
        plot.caption=element_text(size=5,
                                   family="American Typewriter",
                                   face="italic", color = 'darkred'),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=8))

# преобладает группа отсутствующих диабет и без заболеваний сердца в обоих группах, наименьшие значения показаны во время беременности в обоих группах 

ggplot(df, aes(x = HeartDisease, fill = AlcoholDrinking)) + 
  geom_bar(position = "dodge") + labs(title = "Количество заболеваний сердца в зависимости от употребления алкоголя", x = "Курение", y = "Количество", fill = "Алкоголь") + theme_bw()
# в обоих группах среди некуряющих нет заболеваний сердца


ggplot(df, aes(x = HeartDisease, fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Количество заболеваний сердца среди мужчин и женщин", x = "Заболевания сердца", y = "Количество", fill = "Пол") + theme_bw()
# в полученных данных больше здоровых людей, практически одинаковое соотношение в двух случаях

ggplot(df, aes(x = HeartDisease, fill = KidneyDisease)) +
  geom_bar(aes(col = Diabetic),position = "dodge") +
  labs(title = "Количество заболеваний сердца среди диабета и заболеваний почек", x = "Заболевание сердца", y = "Количество", fill = "Болезнь почек") + theme_bw()

ggplot(df[df$HeartDisease == 'Yes', ], aes(x = HeartDisease, fill = KidneyDisease)) + geom_bar(position = "dodge") + theme_bw()

ggplot(df, aes(x = HeartDisease, fill = GenHealth)) +
  geom_bar(position = "dodge") +
  labs(title = "Количество заболеваний сердца по оценке здоровья",
       x = "Заболевание сердца", y = "Количество", fill = "Оценка здоровья") + theme_bw()
# люди с заболеваниями сердца в целом оценивают свое состояние здоровья хуже

ggplot(df[df$HeartDisease == 'Yes', ], aes(x = HeartDisease, fill = AgeCategory)) +
  geom_bar(position = "dodge") +
  labs(title = "Количество заболеваний сердца в зависимости от возрастной категории",
       x = "Заболевание сердца", y = "Количество", fill = "Возрастная категория") + 
  theme_bw()

# Прослеживается увеличение тенденции с возрастом к заболеванию сердца

ggplot(df, aes(x = HeartDisease, fill = Race)) +
  geom_bar(position = "dodge") +
  labs(title = "Количество заболеваний сердца в зависимости от расы",
       x = "Заболевание сердца", y = "Количество", fill = "Раса") + theme_bw()
# в обоих группах наиболее высокие показатели у белой расы. отдельно для заболеваний:


ggplot(df[df$HeartDisease == 'Yes', ], aes(x = HeartDisease, fill = Race)) +
  geom_bar(position = "dodge") +
  labs(title = "Количество заболеваний сердца в зависимости от расы",
       x = "Заболевание сердца", y = "Количество", fill = "Раса") + theme_bw()
# наименьший показатель заболевания у asian, наиболее белая

                      # Анализ между категориальными и количесвенными переменными

# Как было видно ранее, среди категориальных переменных близкое к равному число наблюдений представлено
# только у sex. В проверке гипотез я решила остановится именно на нем, но в добавочной визуализации хотелось
# посмотреть еще распределение массы тела от возраста и курения (сначала по отдельности, а потом в группе М и Ж)

# Распределение веса в зависимости от курения (использование box plot)
ggplot(df, aes(x = BMI, fill = Smoking)) +
  geom_boxplot() +
  labs(title = "Распределение веса в зависимости от курения") +
  theme_bw()
# средние примерно равны

# Распределение веса в зависимости от категории возраста (использование box plot)
ggplot(df, aes(x = BMI, fill = AgeCategory)) +
  geom_boxplot() +
  labs(title = "Распределение веса в зависимости от категории возраста") +
  theme_bw()

# Распределение количества сна от курения
ggplot(df, aes(x = SleepTime, fill = Smoking)) +
  labs(title = "Распределение времени в зависимости от курения") +
  geom_boxplot() +
  theme_bw()

# Для представленной целевой переменной можно построить box plot по признаку BMI

ggplot(df, aes(HeartDisease, y=BMI)) + geom_boxplot(outliers = T, outlier.colour = 'green') + theme_bw() +
  labs(title = 'Box plot of Heart Disease depending on BMI', y="BMI", x = "Heart Disease")

                                         # РЕГРЕССИОННЫЙ АНАЛИЗ
# решила посмотреть методом логистической регрессии, так как таргетная переменная - бинарная категориальная
# добавление таргета в виде фактора
df$HeartDisease_target = as.factor(ifelse(df$HeartDisease == "Yes", 1, 0))

# путем перебора признаков, выбрала более оптимальные (перепроверяла и по значимости, на всех был перебор)
fit3 <- glm(HeartDisease_target ~ AgeCategory + GenHealth + Diabetic + KidneyDisease + Stroke + DiffWalking, data = df, family = binomial)
summary(fit3)
# у меня после перебора переменной выходило площадь под кривой: 0.8199, но нулевое значение recall и переобучение модели
# решила отфильтровать признаки и выбрать наиболее значимые

features<-stepAIC(fit3,direction="both")
features<-c(names(features$coefficients)[-1],"HeartDisease_target")
print(features) 

# наиболее значимым признаком оказался AgeCat, решила остановится на нем

fit4<-glm(HeartDisease_target~AgeCategory, data = df, family = binomial)
summary(fit4)

predictions <- ifelse(fitted(fit4) > 0.5, 1, 0)
accuracy <- mean(predictions == df$HeartDisease_target)
print(accuracy) # 0.9128639 - точность, но она не очень информативна, так как все признаки внутри данных несбалансированы
# лучше смотреть на метрику recall = 0.13 - очень низкий, но наиболее показанный

# ROC-кривая
predicted_values <- fitted(fit4)

# ROC-кривая
roc_curve <- roc(df$HeartDisease_target, predicted_values)
plot(roc_curve, col="blue", main="ROC-кривая для логистической модели")
auc_value <- auc(df$HeartDisease_target, predicted_values)
print(auc_value)
# площадь под кривой: 0.7532393
# из всего анализа по переменным это лучший результат, что я смогла получить :( из важных был еще фактор BMI, 
# но у него рок аук вышел 0.5539181

conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(df$HeartDisease_target))

# Создаем матрицу ошибок
conf_matrix_table <- as.table(conf_matrix$table)

# Преобразование в датафрейм 
df_conf <- as.data.frame(conf_matrix_table)
colnames(df_conf) <- c("Predicted", "Actual", "Count")

# Тепловая карта для наглядности

ggplot(df_conf, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "#B3EE3A") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_bw()

pr<-par(no.readonly = T)
par(mfrow=c(2,2))
plot(fit4)

#  общий вывод - логистическая регресия показала нехорошие метрики в построении модели
# возможные причины: несбалансированность классов, нужен более подходящий подбор гиперпараметров
# возможно сама модель не подходит, так как из представленных предпосылок видно, что даже гомогед не выполняется
# я бы попробовала, будь у нас времени больше вне курса построить например деревья или поиграть с гиперпараметрами, учесть показатель что нон балансед
# но как это делать в R я до конца не поняла, поэтому оставила уже такими результаты :(
