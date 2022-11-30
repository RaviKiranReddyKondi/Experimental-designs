###import the mean value data set
ALL=ALL
head(ALL)
summary(ALL)
#assign pca with output by using prcomp function, here ALL is the data and 2:22 shows the columns of response variable
pca=prcomp(ALL[,2:22], scale. = TRUE)
summary(pca)
#ploting the pca results
plot(pca,type='l')
biplot(pca,scale = 0)
pca$rotation
pca$x
# x have pc score
# rotation have eginen vectors
#sdev have standard deviation
# to get egien values just do square for stdev
#assign pcaf with coloumn bind command and bind the normal data and pc scores for which pca egien value should be greater than 1
#here i selected 1 to 8 coulumns because 8 are having above 1 egien values
pcaf=cbind(ALL,pca$x[,1:6])
head(pcaf)
View(pcaf)
##ploting pcaf out put
library(ggplot2)
ggplot(pcaf,aes(PC1,PC2, col=Treatment,fill=Treatment))+
  stat_ellipse(geom = 'polygon',col='black',alpha=0.5)+
  geom_point(shape=21,col='black')
###creating a correlation plot between raw data and pcs by using corrplot package
a=cor(ALL[-1],pcaf[,23:28])
library(corrplot)
corrplot(a,method = 'circle')
#printing the values from pca  results
a=pca$sdev
a=as.data.frame(a)
write.csv(pca$sdev,file = 'sdev1.csv',col.names = TRUE,row.names = TRUE)
b=pca$rotation
write.csv(b,file = 'rotation.csv',col.names = TRUE,row.names = TRUE)
a=summary(pca)
View(a)
write.csv(a$importance,file = 'imp.csv',col.names = TRUE,row.names = TRUE)
write.csv(a$x,file = 'scores.csv',col.names = TRUE,row.names = TRUE)
###

aa=eigen(ALL[,2:22])
#####visulatisation############
library(plotly)
library(stats)
data("iris")

View(iris)
?prcomp
rm(list=ls())
a=average_data_for_pca
X <- subset(a, select = -c(Treatment))

prin_comp <- prcomp(X, rank. = 3)

components <- prin_comp[["x"]]
components <- data.frame(components)
components$PC2 <- -components$PC2
components$PC3 <- -components$PC3
components = cbind(components, a$Treatment)

tot_explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]
tot_explained_variance_ratio <- 100 * sum(tot_explained_variance_ratio)

tit = 'Total Explained Variance = 99.48'

fig <- plot_ly(components, x = ~PC1, y = ~PC2,  color = ~a$Treatment, colors = c('#636EFA','#EF553B','#00CC96') ) %>%
  add_markers(size = 12)


fig <- fig %>%
  layout(
    title = tit,
    scene = list(bgcolor = "#e5ecf6")
  )

fig
######################

#################
library(factoextra)
fviz_eig(pca)


#########
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
##############
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#############3
screeplot(pca, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)


cumpro <- cumsum(wdbc.pr$sdev^2 / sum(wdbc.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)
