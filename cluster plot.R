library(rattle)
library(cluster)
df=scale(ALL[3:23])
kmf=kmeans(df,4)
attributes(kmf)
kmf$size
kmf$cl
cl=cbind(kmf$cluster)
clusplot(df,kmf$cluster,main='2d representation of cluster', shade = TRUE, label=2, lines = 0)
