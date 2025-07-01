# «birthwt.txt»  факторов на низкий вес новорожденных: age – возраст матери, 
# lwt – вес матери в кг., 
# race - раса (1 – европеоидная, 2 – негроидная, 3 – не указана), 
# smoke – курение табака, ht – наличие артериальной гипертензии. 

влияние этих факторов на вес новорожденных, с помощью функций из пакета ggplot2.
Примечание: для того чтобы повернуть оси координат на 180 градусов воспользуйтесь 
функциями scale_x_reverse() и scale_y_reverse()

library(ggplot2)
data <- read.delim('birthwt.txt', as.is = T) #факторы!!!!
data <- data.frame(
  data,
  Smoking = ifelse(data$smoke == 1, "Smoking", "No smoking"),
  Hypertony = ifelse(data$ht == 1, "Hypertonia", "No disease"),
  Race = ifelse(
    data$race == 1, 
    "europeoid", 
    ifelse(data$race == 2, 'negroid', 'NA')
  ),
  stringsAsFactors = FALSE
)


colnames(data)
# [1] "age"       "lwt"       "race"      "smoke"     "ht"        "bwt"       "Smoking"   "Hypertony" "Race"     

ggplot(data = data, aes(x=lwt, y=bwt)) + geom_point(col="blue", size = 3) + theme_bw()
#пока никакой зависимости не вижу, есть возможно положительный тренд, хочу его проверить

ggplot(data = data, aes(x=lwt, y=bwt)) + geom_point(col="blue", size = 3) + theme_bw() +
  geom_smooth(method="lm",col="firebrick")
#данные не ложатся на график, линия тренда плохо описывает данные
#попробую нелинейную зависимость 
ggplot(data = data, aes(x=lwt, y=bwt)) + geom_point(col="blue", size = 3) + theme_bw() +
geom_smooth(method="loess",col="firebrick")
#пока кажется что между данными нет зависимости

#трехмерная вариация: через цвет третью переменную можно добавить 

ggplot(data = data, aes(x=lwt, y=bwt)) + geom_point(aes(col = Smoking)) + theme_bw() 
# у некурящих распределение веса новорожденного находится выше, у курящих наблюдается меньший вес новорожденного 

ggplot(data = data, aes(x=lwt, y=bwt)) + geom_point(aes(col = Hypertony, size= age)) + theme_bw()
#зависимость не наблюдается, потому что выборка несбалансирована по признаку "гипертония"
ggplot(data = data, aes(x=lwt, y=bwt)) + geom_point(aes(col = Race), size = 2) + theme_bw() 
# у европеоидной дети рождаются примерно в диапазоне от 3 до 4 кг детки, у негроидной нет детей выше 3 кг детей

ggplot(data = data, aes(x=lwt, y=bwt)) + geom_point(aes(col = 'red', size= age)) + theme_bw() 
#меньше возраст = меньше вес новорожденного, больший вес = большой вес новорожденного

ggplot(data = data, aes(x=lwt, y=bwt)) + geom_point(aes(col = Smoking)) + theme_bw()  + labs(title="Влияние курения на вес новорожденных", subtitle="Как курение может влиять на вес новорожденных?", y="Вес ребенка", x="Вес матери", caption="У некурящих распределение веса новорожденного находится выше, у курящих наблюдается меньший вес новорожденного") +
  theme(plot.title=element_text(size=20,
                                face="bold",
                                family="American Typewriter"),
        plot.subtitle=element_text(size=11,
                                   family="American Typewriter",
                                   face="italic", color = 'darkred'),
        plot.caption=element_text(size=8, face = 'bold'),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10))
