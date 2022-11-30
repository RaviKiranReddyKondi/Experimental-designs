data(ALL)
x<-ALL[,3:22]
y<-ALL[,23]
library(agricolae)
cor.y<-correlation(y,x)$correlation
cor.x<-correlation(x)$correlation
a=path.analysis(cor.x,cor.y)
write.csv(a,file = 'path.csv',row.names = TRUE,col.names = TRUE)
library(corrplot)
corrplot(a,method='circle')
view(a)
