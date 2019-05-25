#@author: Romeo Kienzler
#This file is copyright protected - do not redistribute


library(scatterplot3d)
library(ggplot2)
library(stats)
library(h2o)
localH2O = h2o.init(nthreads = -1)
simuURL="https://pmqsimulator-romeokienzler-1155.mybluemix.net/data"
df = read.csv(simuURL,sep = ";",header = FALSE)
dim(df)
colnames(df) = c("t","x","y","z")
scatterplot3d(df$x,df$y,df$z, type = "l")

ggplot() +
  geom_line(data = df, aes(x = t, y = x),colour = "red") +
  geom_line(data = df, aes(x = t, y = y), colour = "blue") +
  geom_line(data = df, aes(x = t, y = z), colour = "green")


xfft = Im(fft(df$x))
yfft = Im(fft(df$y))
zfft = Im(fft(df$z))
ggplot() +
  geom_line(data = df, aes(x = t, y = xfft),colour = "red")  +
  geom_line(data = df, aes(x = t, y = yfft), colour = "blue") +
  geom_line(data = df, aes(x = t, y = zfft), colour = "green")

df = read.csv(simuURL,sep = ";",header = FALSE)
dim(df)
colnames(df) = c("t","x","y","z")

xfft = Im(fft(df$x))
yfft = Im(fft(df$y))
zfft = Im(fft(df$z))
ggplot() +
  geom_line(data = df, aes(x = t, y = xfft),colour = "red")  +
  geom_line(data = df, aes(x = t, y = yfft), colour = "blue") +
  geom_line(data = df, aes(x = t, y = zfft), colour = "green")

tsDataPrev=NULL
while (TRUE) {
  print("reading data...")
  df = read.csv(simuURL,sep = ";",header = FALSE)
  print("done")
  colnames(df) = c("t","x","y","z")
  xfft = Im(fft(df$x))
  yfft = Im(fft(df$y))
  zfft = Im(fft(df$z))
  tsData = as.h2o(t(data.frame(xfft,yfft,zfft)))
  NN_model = h2o.deeplearning(
    x = 1:3,
    training_frame = tsData,
    hidden = c(3000, 2, 3000),
    epochs = 300,
    activation = 'Tanh',
    autoencoder = TRUE
  )
  
  anomalyrates = as.data.frame(h2o.anomaly(NN_model,tsData))
  print(sum(anomalyrates))
  flush.console()
  if (!is.null(tsDataPrev)) {
    anomalyrates = as.data.frame(h2o.anomaly(NN_model,tsDataPrev))
    print(sum(anomalyrates))
    flush.console()   
  }
  tsDataPrev=tsData;
}

