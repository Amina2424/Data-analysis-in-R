В таблице «birthwt.txt» представлена информация о влиянии нескольких факторов на 
низкий вес новорожденных: age – возраст матери, lwt – вес матери в кг., race - раса (1 – 
                                                                                     европеоидная, 2 – негроидная, 3 – не указана), smoke – курение табака, ht – наличие 
артериальной гипертензии. Постройте один или несколько графиков, наилучшим образом 
иллюстрирующих влияние этих факторов на вес новорожденных, с помощью функций из 
пакета ggplot2. 


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

ggplot(data, aes(x=bwt, y=lwt)) + 
  geom_jitter(col="green", size=1.5,width=1,height=1) + 
  geom_smooth(method="lm", se=F,col="firebrick") + 
  labs(title="Оценка корелляции между весом матери и ребенка", 
       y="Вес матери", x="Вес ребенка") + theme_bw()
#данные не ложатся на график, линия тренда плохо описывает данные

ggplot(data = data, aes(x=lwt, y=bwt)) + geom_boxplot(aes(col = Hypertony, size= age),notch=T) + theme_bw()
#видим разные насечки на графике, что говрит нам о том, что выборка несбалансированна. таким образом нельзя выдвинуть достоверные результаты
data1 = subset(data, data$race != 3)

ggplot(data1, aes(bwt)) + geom_density(aes(fill=Race),alpha=0.8) + 
  labs(title="Race",fill="bwt") + theme_bw()
# в случае негоридной меньше вес чем европеоидной

ggplot(data, aes(x = Smoking, y = bwt)) + geom_boxplot(notch=T) + theme_bw()
ggplot(data, aes(bwt)) + geom_density(aes(fill = Smoking)) + theme_bw()
# no smoking выше масса тела новорожденных