a=cor(ALL[3:23],method = "pearson")
View(a)
library(corrplot)
library(ggplot2)
?ggplot
corrplot(a,method ='square')
