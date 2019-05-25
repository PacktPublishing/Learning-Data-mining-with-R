#@author: Romeo Kienzler
#This file is copyright protected - do not redistribute


library(dbscan)
library(scatterplot3d)
data(iris)
View(iris)
irisorg = iris
iris <- as.matrix(iris[,1:4])
unique(irisorg$Species)
scatterplot3d(iris[,1],iris[,4],iris[,3], highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", main="scatterplot3d - 1", pch=20)
res = dbscan(iris, eps = .8, minPts = 4)
length(unique(res$cluster))
map = list("setosa"=1, "versicolor"=2, "virginica"=3)
specieskey = unlist(lapply(irisorg$Species,  function(species) map[[species]]))


truthVectorValidate = res$cluster == specieskey
good = length(truthVectorValidate[truthVectorValidate==TRUE])
bad = length(truthVectorValidate[truthVectorValidate==FALSE])
good/(good+bad)
