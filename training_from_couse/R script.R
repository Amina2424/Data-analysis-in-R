
install.packages('survival')
install.packages('survminer')

library(survival)
library(survminer)
library(dplyr)

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
