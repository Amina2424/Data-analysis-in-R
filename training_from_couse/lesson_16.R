library(ggplot2)
tt <- read.delim('Death_rate2.txt', as.is = T) #открыть все данные в том типе данных, что о них есть
colnames(tt)<-c("Precipitations","Schooling","Pollution","Death_rate")

# разбиение на категории
tt<-data.frame(tt,Death_rate_cat=ifelse(tt$Death_rate>940,"High","Low"), 
                 Schooling_cat=ifelse(tt$Schooling>11,"High","Low"),stringsAsFactors = F)

# создание координатной плоскости 
ggplot(tt, aes(x=Precipitations, y=Pollution)) # aes - координатная плоскость 

#добавление точек geom_point()
ggplot(tt, aes(x=Precipitations, y=Pollution)) + geom_point()
#добавим цыет и размер
ggplot(tt, aes(x=Precipitations, y=Pollution)) + geom_point(col="red", size = 3)

# линия ренда (функция geom_smooth),  при помощи линейной регрессии (параметр method = “lm”)
ggplot(tt, aes(x=Precipitations, y=Pollution)) + geom_point(col="steelblue", size=3)+ 
  geom_smooth(method="lm",col="firebrick")
ggplot(tt, aes(x=Precipitations, y=Pollution)) + geom_point(col="steelblue", 
                                                            size=3)+ geom_smooth(method="loess",col="firebrick")
#трехмерная вариация: через цвет третью переменную можно добавить 

ggplot(tt, aes(x=Precipitations, y=Pollution)) + 
  geom_point(aes(col=Death_rate),size=3)

# по дефолту он низкие значение делает темнее, что не оч удобно воспринимать. решение -scale_colour_gradient(low="#",high=")

ggplot(tt, aes(x=Precipitations, y=Pollution)) + 
  geom_point(aes(col=Death_rate),size=3)+
  scale_colour_gradient(low="#cbc5f0",high="#130a4a")

# хотим посмотреть категории по цветам!
ggplot(tt, aes(x=Precipitations, y=Pollution)) + 
  geom_point(aes(col=Death_rate_cat),size=3)

# изменение цвета точек scale_color_manual(values...)
ggplot(tt, aes(x=Precipitations, y=Pollution)) + 
  geom_point(aes(col=Death_rate_cat),size=3) +
scale_color_manual(values=c("High"="green4","Low"="cyan"))

#играя с аргументами geom_point() можно разные фичерс подставлять для определения связи между данными
ggplot(tt, aes(x=Precipitations, y=Pollution)) +
  geom_point(aes(col=Death_rate_cat,size=Death_rate))+
  scale_color_manual(values=c("High"="green4","Low"="cyan"))

# название диаграммы, изменим подписи осей и легенд при помощи функции labs
ggplot(tt, aes(x=Precipitations, y=Pollution)) + 
  geom_point(aes(col=Death_rate_cat,size=Death_rate))+
  scale_color_manual(values=c("High"="green4","Low"="cyan"))+
  labs(title="Population Vs Precipitation", subtitle="From death_rate dataset", y="Relative 
pollution of Sulfur Dioxide", x="Average annual precipitation", caption="Data on death rate")+
  labs(color="Death level", size="Death rate")

# facet_wrap() - разбиение данных на подмассив согласно указанным категориям в столбце. работает как матрица (например у нас две категории и матрица 2х1 или 1х2)
ggplot(tt, aes(x=Precipitations, y=Pollution)) + 
  geom_point(aes(col=Death_rate_cat,size=Death_rate))+
  scale_color_manual(values=c("High"="green4","Low"="cyan"))+
  labs(title="Population Vs Precipitation", subtitle="From death_rate dataset", y="Relative 
pollution of Sulfur Dioxide", x="Average annual precipitation", caption="Data on death rate")+
  labs(color="Death level", size="Death rate")+
  facet_wrap( ~ Schooling_cat,nrow=2,ncol=1,scale="free")

ggplot(tt, aes(x=Precipitations, y=Pollution)) + 
  geom_point(aes(col=Death_rate_cat,size=Death_rate))+
  scale_color_manual(values=c("High"="green4","Low"="cyan"))+
  labs(title="Population Vs Precipitation", subtitle="From death_rate dataset", y="Relative pollution of Sulfur 
Dioxide", x="Average annual precipitation", caption="Data on death rate")+
  labs(color="Death level", size="Death rate")+
  theme(plot.title=element_text(size=20,
                                face="bold",
                                family="American Typewriter",
                                color="tomato"),
        plot.subtitle=element_text(size=15,
                                   family="American Typewriter",
                                   face="bold", color = 'blue'),
        plot.caption=element_text(size=15, color = 'orange'),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=10,
                                 angle = 90),
        axis.text.y=element_text(size=10))


# цвет фона, цвет, тип и толщину линий решетки, а также цвет и толщину осей panel.background = element_rect

ggplot(tt, aes(x=Precipitations, y=Pollution)) + 
  geom_point(aes(col=Death_rate_cat,size=Death_rate))+
  scale_color_manual(values=c("High"="green4","Low"="cyan"))+
  labs(title="Population Vs Precipitation", subtitle="From death_rate dataset", y="Relative pollution of Sulfur 
Dioxide", x="Average annual precipitation", caption="Data on death rate")+
  labs(color="Death level", size="Death rate")+
  theme(panel.background = element_rect(fill = 'khaki'),
        panel.grid.major = element_line(colour = "burlywood", size=1.5),
        panel.grid.minor = element_line(colour = "tomato", 
                                        size=0.25, 
                                        linetype = "dashed"),
        axis.line.x = element_line(colour = "darkorange", 
                                   size=1.5),
        axis.line.y = element_line(colour = "darkorange", 
                                   size=1.5))
# автоматические параметры theme_grey(), theme_gray(), 
# theme_bw(), theme_linedraw(), theme_light(), theme_dark(), theme_minimal(), 
# theme_classic(), theme_void(), theme_test()

ggplot(tt, aes(x=Precipitations, y=Pollution)) + 
  geom_point(aes(col=Death_rate_cat,size=Death_rate))+
  scale_color_manual(values=c("High"="green4","Low"="cyan"))+
  labs(title="Population Vs Precipitation", subtitle="From death_rate dataset", y="Relative pollution of Sulfur 
Dioxide", x="Average annual precipitation", caption="Data on death rate")+
  labs(color="Death level", size="Death rate") + theme_bw()

