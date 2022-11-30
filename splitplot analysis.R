library(doebioresearch)
rm(list=ls())
a=X2017_18_heat
aAA=splitplot(a[4:13],a$block,a$var,a$treatments,1)
aAA

sink('heat17-18.doc')
print(aAA)
sink()
aAA[["Yield"]][[1]][[1]]


#####
a=X2017_18_heat
head(a)
str(a)
a$var=as.factor(a$var)
a$treatments=as.factor(a$treatments)
str(a)
attach(a)
library(agricolae)
attach(a)
model=sp.plot(block = block,pplot = var,splot = treatments,Y=Yield)
# Get first error df
Edf_a <- model$gl.a
Edf_a
# Get second error df
Edf_b <- model$gl.b
Edf_b
# Get first error MS
EMS_a <- model$Ea
EMS_a
# Get second error MS
EMS_b <- model$Eb
EMS_b
####lsd
out1 <- LSD.test(y = Yield, 
                 trt = var,
                 DFerror = Edf_a, 
                 MSerror = EMS_a,
                 alpha = 0.05,
                 p.adj = "bonferroni",
                 group = TRUE,
                 console = TRUE)
out2 <- LSD.test(y =Yield, 
                 trt = treatments,
                 DFerror = Edf_b, 
                 MSerror = EMS_b,
                 alpha = 0.05,
                 p.adj = "bonferroni",
                 group = TRUE,
                 console = TRUE)
out3 <- LSD.test(y = Yield, 
                 trt = var:treatments,
                 DFerror = Edf_b, 
                 MSerror = EMS_b,
                 alpha = 0.05,
                 p.adj = "bonferroni",
                 group = TRUE,
                 console = TRUE)
write.csv(out3$groups,file = "2017-18interaction.csv")
write.csv(out2$groups,file = "2017-18treatments.csv")
write.csv(out1$groups,file = "2017-18var.csv")

######
plot(out1, 
     xlab = "varieties",
     ylab = "Yield",
     las = 1, 
     variation = "IQR")
plot(out2, 
     xlab = "treatmnts",
     ylab = "Yield",
     las = 1, 
     variation = "IQR")
plot(out3, 
     xlab = "var:treatments",
     ylab = "Yield",
     las = 1,cex.names=0.75,
     variation = "IQR")
#########
# Main plot factor (varieties)
bar.err(out1$means, 
        variation = "SE", 
        ylim = c(0, 10000),
        names.arg = c("V1","V2","V3"))

# Adding title, X and Y labels
title(main = "Barplot with standard error",
      cex.main = 0.8,
      xlab = "Varieties", 
      ylab = "Yields")

# Sub-plot factor (seedling age)
bar.err(out2$means, 
        variation = "SE", 
        ylim = c(0, 10000),
        names.arg = c("T1", "T2", "T3","T4","T5","T6","T7"))

# Adding title, X and Y labels
title(main = "Barplot with standard error",
      cex.main = 0.8,
      xlab = "Treatments", 
      ylab = "Yields")
####
# Main plot factor (varieties)
bar.err(out3$means, 
        variation = "SE", 
        ylim = c(0, 11000),
        cex.names=0.75)

# Adding title, X and Y labels
title(main = "Barplot with standard error",
      cex.main = 0.8,
      xlab = "Varities:Treatments", 
      ylab = "Yield")
