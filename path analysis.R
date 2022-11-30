#first load the packages
library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)
library(ggplot2)
#draw a relationship between dependent variables
#with independent variables and name it as an object
a=GY~PH+PL+NETP+TNFGP+NTSM+NPSM+DTF+HI+
  TW+HR+GL+GB+GLB+KL+KB+KLB+ASV+GC+AC+KER
#indepent variables are towards left side of "~" this sign
#than find cfa() confirmatory factor analysis models and save in object
path=cfa(a,data = ALL)
#than draw summary for the cfa object
summary(path,fit.measures=T,standardized=T,rsquare=T)
#by using semplot we can draw circle and tree plot as follows
semPaths(path,'std',layout='circle')
semPaths(path,'std',layout = 'tree',edge.label.cex = 1,
         curvePivot= T)
semPaths(path,'std','est',curveAdjacent = TRUE,
         style = 'lisrel',layout='tree',edge.label.cex = 1,curvePivot=T)
#other way of representation
ggcorr(ALL[-c(1,2)],nbreaks =6,label = F,label_size = 2,
       low = 'red3',high = 'green3',label_round = 2,
       name ='positive and negative effects',
       label_alpha = T,hjust=0.75)+
  ggtitle(label = 'Path Analysis')+
  theme(plot.title = element_text(hjust = 0.6))
