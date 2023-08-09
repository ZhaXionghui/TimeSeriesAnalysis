setwd("E:/onedrive/OneDrive - mail.sdu.edu.cn/大学/大二/下/时间序列分析/论文")
BOC <- read.csv("3988.HK.csv", stringsAsFactors = F)

# https://geek-docs.com/r-language/r-ask-answer/g_how-to-convert-string-to-datetime-in-r.html#:~:text=dataframe%E6%98%AF%E8%BE%93%E5%85%A5%E7%9A%84%E6%95%B0%E6%8D%AE%E6%A1%86%E6%9E%B6%20column_name%E6%98%AF%E5%AD%97%E7%AC%A6%E4%B8%B2%E6%97%A5%E6%9C%9F%E5%88%97%20%23%20consider%20a%20dataframe%20dataframe%20%3D,data%20column%20to%20datetime%20print%28as.POSIXct%28dataframe%24data%2C%20format%3D%22%25Y-%25m-%25d%20%25H%3A%25M%3A%25S%22%2C%20tz%3D%22UTC%22%29%29
# String to DateTime
BOC$Date = as.POSIXct(BOC$Date, format="%Y/%m/%d",tz="UTC")

plot(x = BOC$Date,
     y = BOC$Open,
     # type = 'p',
     # pch = 20,
     # cex = 0.5,
     type = 'l',
     xlab = "Date",
     ylab = "Open"
     )
plot(x = BOC$Date,
     y = BOC$High,
     # type = 'p',
     # pch = 20,
     # cex = 0.5,
     type = 'l',
     xlab = "Date",
     ylab = "High"
)
plot(x = BOC$Date,
     y = BOC$Low,
     # type = 'p',
     # pch = 20,
     # cex = 0.5,
     type = 'l',
     xlab = "Date",
     ylab = "Low"
)
plot(x = BOC$Date,
     y = BOC$Close,
     # type = 'p',
     # pch = 20,
     # cex = 0.5,
     type = 'l',
     xlab = "Date",
     ylab = "Close"
)
plot(x = BOC$Date,
     y = BOC$Adj.Close,
     # type = 'p',
     # pch = 20,
     # cex = 0.5,
     type = 'l',
     xlab = "Date",
     ylab = "Adj.Close"
)
plot(x = BOC$Date,
     y = BOC$Volume,
     # type = 'p',
     # pch = 20,
     # cex = 0.5,
     type = 'l',
     xlab = "Date",
     ylab = "Volume"
)
# ChangePoint
# install.packages('changepoint')
library(changepoint)

TS = BOC
rownames(TS) <- TS$Date
TS = TS[,-1]
TSmat = as.matrix(TS)
TSmat = t(TSmat)
# cptean = cpt.mean(TSmat)
# TSmat['Open',]
# # Example of using the CROPS penalty in data set above
# out=cpt.mean(TSmat['Open',], pen.value = c(4,1500),penalty = "CROPS",method = "PELT")
# cpts.full(out) # returns 7 segmentations for penalty values between 4 and 1500.
# # We find segmentations with 7, 5, 4, 3, 2, 1 and 0 changepoints.
# # Note that the empty final row indicates no changepoints.
# pen.value.full(out) # gives associated penalty transition points
# # CROPS does not give an optimal set of changepoints thus we may wish to explore further
# plot(out,diagnostic=TRUE)
# # looks like the segmentation with 3 changepoints, 50,100,150 is the most appropriate
# plot(out,ncpts=5,ylab = "Open")

CPT_mean <- function(ylab,penalty,method,ncpts){
  out=cpt.mean(TSmat[ylab,], pen.value = c(4,1500),penalty = penalty,method = method)
  cpts.full(out) 
  # Note that the empty final row indicates no changepoints.
  pen.value.full(out) # gives associated penalty transition points
  par(mfrow = c(2,1))
  # CROPS does not give an optimal set of changepoints thus we may wish to explore further
  # plot(out,diagnostic=TRUE,main=paste('cpt mean',ylab),cex.main=1)
  plot(out,diagnostic=TRUE,main=paste('cpt mean',ylab))
  # looks like the segmentation with 3 changepoints, 50,100,150 is the most appropriate
  plot(out,ncpts=ncpts,ylab = ylab,
       main = paste('number of mean changepoints:',ncpts))
}

CPT_mean("Open","CROPS","PELT",5)
CPT_mean("High","CROPS","PELT",5)
CPT_mean("Low","CROPS","PELT",5)
CPT_mean("Close","CROPS","PELT",5)
CPT_mean("Adj.Close","CROPS","PELT",4)
CPT_mean("Volume","CROPS","PELT",1230)


CPT_meanvar <- function(ylab,penalty,method,ncpts){
  out=cpt.meanvar(TSmat[ylab,], pen.value = c(4,1500),penalty = penalty,method = method)
  cpts.full(out) 
  # Note that the empty final row indicates no changepoints.
  pen.value.full(out) # gives associated penalty transition points
  par(mfrow = c(2,1))
  # CROPS does not give an optimal set of changepoints thus we may wish to explore further
  plot(out,diagnostic=TRUE,main=paste('cpt meanvar',ylab))
  # looks like the segmentation with 3 changepoints, 50,100,150 is the most appropriate
  plot(out,ncpts=ncpts,ylab = ylab,main = paste('number of meanvar changepoints:',ncpts))
}

CPT_meanvar("Open","CROPS","PELT",454)
CPT_meanvar("High","CROPS","PELT",526)
CPT_meanvar("Low","CROPS","PELT",477)
CPT_meanvar("Close","CROPS","PELT",464)
CPT_meanvar("Adj.Close","CROPS","PELT",465)
CPT_meanvar("Volume","CROPS","PELT",429)


CPT_var <- function(ylab,penalty,method,ncpts){
  out=cpt.var(TSmat[ylab,], pen.value = c(4,1500),penalty = penalty,method = method)
  cpts.full(out) 
  # Note that the empty final row indicates no changepoints.
  pen.value.full(out) # gives associated penalty transition points
  par(mfrow = c(2,1))
  # CROPS does not give an optimal set of changepoints thus we may wish to explore further
  plot(out,diagnostic=TRUE,main=paste('cpt var',ylab))
  # looks like the segmentation with 3 changepoints, 50,100,150 is the most appropriate
  plot(out,ncpts=ncpts,ylab = ylab,main = paste('number of var changepoints:',ncpts))
}

CPT_var("Open","CROPS","PELT",49)
CPT_var("High","CROPS","PELT",44)
CPT_var("Low","CROPS","PELT",47)
CPT_var("Close","CROPS","PELT",44)
CPT_var("Adj.Close","CROPS","PELT",48)
CPT_var("Volume","CROPS","PELT",81)

# CWT
# install.packages('Rwave')
library(Rwave)
# # par(mfrow = c(1,2))
# layout(mat = matrix(c(1,1,2,3), 2, 2,byrow = T),
#        widths = c(1,1),
#        heights = c(2,3))
# plot(x = BOC$Date,
#      y = BOC$Open,
#      # type = 'p',
#      # pch = 20,
#      # cex = 0.5,
#      type = 'l',
#      xlab = "Date",
#      ylab = "Open"
# )
# # cwt(BOC$Open,noctave = 2,nvoice = 1,w0 = 2*pi,plot = TRUE)
# chirp <- BOC$Open
# retChirp <- cwt(chirp, noctave=3, nvoice=12, twoD=FALSE, plot=FALSE)
# retPolar <- cwtpolar(retChirp)
# retImageMod <- cwtimage(retPolar$modulus)
# retImageArg <- cwtimage(retPolar$argument)

CWT_Rwave_plot <- function(X,Y,xlab,ylab){
  layout(mat = matrix(c(1,1,2,3), 2, 2,byrow = T),
         widths = c(1,1),
         heights = c(2,3))
  plot(x = X,
       y = Y,
       # type = 'p',
       # pch = 20,
       # cex = 0.5,
       type = 'l',
       xlab = xlab,
       ylab = ylab
  )
  # cwt(BOC$Open,noctave = 2,nvoice = 1,w0 = 2*pi,plot = TRUE)
  chirp <- Y
  retChirp <- cwt(chirp, noctave=9, nvoice=12, twoD=FALSE, plot=FALSE)
  retPolar <- cwtpolar(retChirp)
  retImageMod <- cwtimage(retPolar$modulus)
  retImageArg <- cwtimage(retPolar$argument)
}

CWT_Rwave_plot(BOC$Date,BOC$Open,'Date','Open')
CWT_Rwave_plot(BOC$Date,BOC$High,'Date','High')
CWT_Rwave_plot(BOC$Date,BOC$Low,'Date','Low')
CWT_Rwave_plot(BOC$Date,BOC$Close,'Date','Close')
CWT_Rwave_plot(BOC$Date,BOC$Adj.Close,'Date','Adj.Close')
CWT_Rwave_plot(BOC$Date,BOC$Volume,'Date','Volume')


# install.packages('dplR')
library(dplR)
# days <- time(BOC$Date)
# CAMstd <- BOC$Open
# out.wave <- morlet(y1 = CAMstd, x1 = days, p2 = 9, dj = 0.1,
#                    siglvl = 0.99)
# wavelet.plot(out.wave, useRaster = NA,
#              crn.lab = 'Open')
CWT_dplR_plot <- function(X,Y,ylabel){
  days <- time(X)
  CAMstd <- Y
  out.wave <- morlet(y1 = CAMstd, 
                     x1 = days, 
                     p2 = 9, 
                     dj = 0.1,
                     siglvl = 0.99)
  wavelet.plot(out.wave, useRaster = NA,
               crn.lab = ylabel)
}

CWT_dplR_plot(BOC$Date,BOC$Open,'Open')
CWT_dplR_plot(BOC$Date,BOC$High,'High')
CWT_dplR_plot(BOC$Date,BOC$Low,'Low')
CWT_dplR_plot(BOC$Date,BOC$Close,'Close')
CWT_dplR_plot(BOC$Date,BOC$Adj.Close,'Adj.Close')
CWT_dplR_plot(BOC$Date,BOC$Volume,'Volume')