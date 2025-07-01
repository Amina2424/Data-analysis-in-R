library(ggplot2)
library(ggExtra)
df = read.delim('mRNA-protein_correlation.txt')
colnames(df) = c('Protein', 'Gene', 'mRNA_CNV', 'Protein_CNV')

концентрации матричной РНК и белка для 4962 генов мыши. 
Концентрации измерены в мышиных фибробластах и представлены в виде числа молекул на клетку. 
Оцените наличие корреляции между ними, используя функции из пакета ggplot2.
# sum(is.na(df)) [1] 666
df = na.omit(df)
df$Protein_CNV = log(df$Protein_CNV)
df$mRNA_CNV = log(df$mRNA_CNV)
corr = cor.test(df$Protein_CNV,df$mRNA_CNV) #  cor 0.6368912 
ggplot(df, aes(x=mRNA_CNV, y=Protein_CNV)) + 
  geom_point(col="darkgreen", size=0.5) + 
  geom_smooth(method="lm", se=F,col="firebrick") + 
  labs(title="Correlation between Protein and Gene copy number", subtitle="Концентрации измерены в мышиных фибробластах и представлены в виде числа молекул на клетку", 
       y="Protein", x="mRNA") + theme_bw() +
  theme(plot.title=element_text(size=18,
                              face="bold",
                              color = 'firebrick',
                              family="American Typewriter"),
      plot.subtitle=element_text(size=10,
                                 family="American Typewriter",
                                 face="italic", color = 'darkgreen'),
      plot.caption=element_text(size=8, face = 'bold'),
      axis.title.x=element_text(size=12),
      axis.title.y=element_text(size=12),
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10)) + annotate(
        "text", label = "correlation = 0.64", 
        x = 1, y = 18, size = 4, colour = "red"
      )
ggMarginal(gg, type = "boxplot", fill="transparent")

