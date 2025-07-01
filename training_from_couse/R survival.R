install.packages('survival')
install.packages('survminer')
library(ggplot2)
library(survival)
library(survminer)
library(dplyr)
# Рандомизированное исследование двух схем лечения рака легких. Это стандартный набор данных для анализа выживаемости.
trt: 1=стандартный 2=тестовый
Тип клеток: 1=плоскоклеточный, 2=мелкоклеточный, 3=адено, 4=крупноклеточный
время: время выживания
статус: статус цензурирования
karno: оценка производительности Карновского (100 = хорошо)
время диагностики: месяцы от диагноза до рандомизации
возраст: в годах
до: предшествующая терапия 0=нет, 10=да
# загружаем несколько наборов данных
data(cancer, package="survival")
glimpse(veteran)

# факторизация числовых переменных
vet <- mutate(veteran, AG = ifelse((age < 60), "LT60", "OV60"),
              AG = factor(AG),
              trt = factor(trt,labels=c("standard","test")),
              prior = factor(prior,labels=c("N0","Yes")))


# Построение кривых Каплана-Мейера
surv_object <- Surv(time = vet$time, event = vet$status)
fit <- survfit(surv_object ~ trt, data = vet)
summary(fit)
ggsurvplot(fit, data = vet, pval = TRUE)

# Оценка различий кривых с использованием логрангового теста
survdiff(surv_object ~ trt, data = vet)

# Регрессия Кокса
fit.coxph <- coxph(surv_object ~ trt + celltype + karno + diagtime + age + prior, 
                   data = vet)
ggforest(fit.coxph, data = vet)

# оценка зависимости коэффициентов уравнения от времени
tt <- cox.zph(fit.coxph) 
print(tt) 
plot(tt)


lung<-lung
# Time (Время): Это переменная, которая представляет собой время, 
# прошедшее с момента операции до наступления события (например, смерти) или до конца наблюдения. Измеряется в днях.
# Status (Статус): Это бинарная переменная, которая указывает, произошло ли событие (смерть) 
# к моменту последнего наблюдения (1 - событие не произошло, 2 - событие произошло).
# Age (Возраст): Возраст пациента на момент операции.
# Sex (Пол): Пол пациента (мужской или женский).
# Ph.ecog (Экологический индекс Физикал Хелзон): Это оценка физического состояния пациента 
# перед операцией (число от 0 до 4, где 0 - отсутствие симптомов, 4 - сильные симптомы).
# Ph.karno (Карно Индекс): Оценка общего состояния здоровья пациента перед операцией (число от 0 до 100, где 100 - отличное здоровье).
# Ph.karno.future (Будущий Карно Индекс): Предполагаемая оценка общего состояния здоровья пациента через два года после операции.

surv_obj <- with(lung, Surv(time, status))
surv_km <- survfit(surv_obj ~ 1, 
                   conf.type = "log-log",
                   data = lung)
# Построение кривой Каплан-Мейера
library(ggsurvfit)
ggsurvfit(surv_km, col = "#0984e3", size = 0.8)+  # Создает график кривой выживаемости surv_km - это объект функции survfit. col =  устанавливает цвет линии, а size = 0.8 задает размер.
  add_censor_mark(col = "#0984e3") + # Добавляет метки цензурированных данных на график. col = "#0984e3" устанавливает цвет для меток
  add_confidence_interval(fill = "#0984e3")+ # Добавляет доверительный интервал на график. fill = "#0984e3" задает цвет заливки для интервала
  add_risktable(risktable_stats = c("n.risk"), # Добавляет таблицу рисков. risktable_stats = c("n.risk") указывает, что нужно показать количество наблюдений на риске
                risktable_group = c( "auto")) + # risktable_group = c( "auto") автоматически определяет группы в данных
  add_risktable_strata_symbol(col = "#0984e3") + # Добавляет символы для групп на графике. col = "#0984e3" задает цвет символов.
  ylim(c(0, 1)) + # Устанавливает пределы по оси y на графике от 0 до 1, чтобы гарантировать, что все значения на графике остаются в пределах этого диапазона
  labs( x = "Days", size = 0.1) + # Устанавливает подписи осей и легенды на графике.
  theme_survminer() # красивая тема из пакета survminer
# Вывод таблицы дожития
summary(surv_km)
# Сравним общую выживаемость у мужчин и женщин
lung$surv_obj <- with(lung, Surv(time, status))
surv_km <- survfit2(surv_obj ~ sex, # меняем survfit на survfit2 из пакета ggsurvfit
                    # после тильды указываем группирующий столбик (пол)
                    conf.type = "log-log",
                    data = lung)
ggsurvfit(surv_km, size = 0.8)+ 
  add_censor_mark() + 
  add_confidence_interval()+ 
  add_risktable(risktable_stats = c("n.risk"), 
                risktable_group = c( "auto")) + 
  add_risktable_strata_symbol() + 
  ylim(c(0, 1)) + 
  labs( x = "Days", size = 0.1) + 
  theme_survminer() + 
  add_pvalue(size = 0.6) + # добавляет p значение, расчитанное тестом log-rank из ф-ции survfit2
  scale_color_manual(labels = c("Male", "Female"), values = c("#00b894", "#0984e3")) + # меняем в ручную названия наблюдений и прописываем цвета
  scale_fill_manual(labels = c("Male", "Female"), values = c("#00b894",  "#0984e3")) 

summary(surv_km, times = 800) # указываем конкретное время для таблицы дожития