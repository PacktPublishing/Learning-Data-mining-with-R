#@author: Romeo Kienzler
#This file is copyright protected - do not redistribute

library(ggplot2)
attach(mtcars)



qplot(wt,mpg, data = mtcars)

transmission = factor(mtcars$am,levels = c(0,1), labels = c("Automatic","Manual"))
mod=lm(wt~mpg,data=mtcars)
qplot(wt,mpg,
      data= mod,
      color=transmission,
      shape=transmission,
      #geom="smooth",
      geom=c("point","smooth"),
      #method="lm", formula=y~x,
      xlab = "Weight",
      ylab= "Miles per Gallon",
      main = "Regression Example")
