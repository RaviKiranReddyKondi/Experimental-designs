rm(list = ls())
ssp=X2017_18_heat_Copy
str(ssp)
ssp$year=as.factor(ssp$year)
ssp$var=as.factor(ssp$var)
ssp$treatment=as.factor(ssp$treatment)
str(ssp)
###
library(agricolae)
attach(ssp)
model=with(ssp,ssp.plot(block,year,var,treatment,TPH))
model
sink('heatyield.doc')
print(model)
sink()
#####
gla=model$gl.a
glb=model$gl.b
glc=model$gl.c
Ea=model$Ea
Eb=model$Eb
Ec=model$Ec

###
out1=with(ssp, LSD.test(TPH, year,gla,Ea,console = TRUE))
out2=with(ssp, LSD.test(TPH, var,glb,Eb,console = TRUE))
out3=with(ssp, LSD.test(TPH, treatment,glc,Ec,console = TRUE))
out4=with(ssp, LSD.test(TPH, year:var,glb,Eb,console = TRUE))
out5=with(ssp, LSD.test(TPH, var:treatment,glc,Ec,console = TRUE))
out6=with(ssp, LSD.test(TPH, treatment:year,glc,Ec,console = TRUE))
out7=with(ssp, LSD.test(TPH, treatment:year:var,glc,Ec,console = TRUE))
heatyield=out7$groups
heatyield=as.matrix(heatyield)
heatyield
write.csv(heatyield,file="yield.csv",row.names = TRUE)


########
######
par(mfrow=c(1,4))
plot(out1, 
     xlab = "Years",
     ylab = "Yield",
     las = 1, 
     variation = "IQR")
plot(out2, 
     xlab = "Varieties",
     ylab = "Yield",
     las = 1, 
     variation = "IQR")
plot(out3, 
     xlab = "Treatments",
     ylab = "Yield",
     las = 1,cex.names=0.75,
     variation = "IQR")
plot(out5, 
     xlab = "Treatments:varities",
     ylab = "Yield",
     las = 2,cex.names=0.75,
     variation = "IQR")
#########
# Main plot factor (varieties)
bar.err(out7$means, 
        variation = "SE", 
        ylim = c(0, 10000),las = 2,cex.names=0.70)

# Adding title, X and Y labels
title(main = "Barplot with standard error",
      cex.main = 0.8,
      xlab = "Treatments:Years:Varities", 
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

