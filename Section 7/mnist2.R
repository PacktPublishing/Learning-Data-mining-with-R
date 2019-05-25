#@author: Romeo Kienzler
#This file is copyright protected - do not redistribute


library(h2o)
localH2O =h2o.init(nthreads = -1)
#Data can be downloaded here: https://www.kaggle.com/c/digit-recognizer/data
MNIST_DIGITStrain = read.csv( '...train.csv' )
dim(MNIST_DIGITStrain)
par( mfrow = c(10,10), mai = c(0,0,0,0))
for(i in 1:100){
  y = as.matrix(MNIST_DIGITStrain[i, 2:785])
  dim(y) = c(28, 28)
  image( y[,nrow(y):1], axes = FALSE, col = gray(255:0 / 255))
  text( 0.2, 0, MNIST_DIGITStrain[i,1], cex = 3, col = 2, pos = c(3,4))
}


mfile = '....train.csv'

MDIG = h2o.importFile(path = mfile,sep=',')

# Show the data objects on the H2O platform
h2o.ls()

#---
NN_model = h2o.deeplearning(
  x = 2:785,
  training_frame = MDIG,
  hidden = c(400, 200, 2, 200, 400 ),
  epochs = 600,
  activation = 'Tanh',
  autoencoder = TRUE
)

train_supervised_features2 = h2o.deepfeatures(NN_model, MDIG, layer=3)

plotdata2 = as.data.frame(train_supervised_features2)
plotdata2$label = as.character(as.vector(MDIG[,1]))

library(ggplot2)
qplot(DF.L3.C1, DF.L3.C2, data = plotdata2, color = label, main = 'Neural network: 400 - 200 - 2 - 200 - 400')

#---


tsData<-h2o.importFile(path = "...train.csv")
res.dl <- h2o.deeplearning(x = 2:785, y = 1, training_frame = MDIG, activation = "Tanh",hidden=rep(160,5),epochs = 20)
pred.dl<-h2o.predict(object=res.dl,newdata=tsData[,-1])
pred.dl.df<-as.data.frame(pred.dl)
tsData.df<-as.data.frame(tsData)

truthVector = tsData.df$label == as.integer(round(pred.dl.df[,1]))
good = length(truthVector[truthVector==TRUE])
bad = length(truthVector[truthVector==FALSE])
good/(good+bad)



