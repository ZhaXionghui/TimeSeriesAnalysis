setwd("./")
# sales <- read.csv("sales_train_evaluation.csv", stringsAsFactors = F)
# sale <- t(as.matrix(apply(sales[,6:ncol(sales)],1,as.numeric)))
BOC <- read.csv("3988.HK.csv", stringsAsFactors = F)
BOCnew <- read.csv("3988.HK_2023_8_9.csv", stringsAsFactors = F)
source("ard_vs_arma+volat.R")
# steps=28

# ardpredict(BOC$Volume,7)
# py = ArmaEWMA(y0,7)

# st = 1
# et = length(BOC$Volume)-2
# y0 = BOC$Volume[1:et]
# # y0[length(y0)]
# st = length(BOCnew$Volume)-6
# et = length(BOCnew$Volume)
# y1 = BOCnew$Volume[st:et]
# 
# plot(py$mean,
#      type = 'l',
#      lwd = 2,
#      col = 'red',
#      xlab = 'Time',
#      ylab = '',
#      main = 'Prediction from 2023.8.1~2023.8.9',
#      ylim = c(1.5e+08,3.5e+08))
# lines(y1,col = 'black', lwd = 2)
# 计算平均绝对百分比误差（MAPE）
calculate_mape <- function(actual_values, forecast_values) {
  mape <- mean(abs((forecast_values - actual_values) / actual_values)) * 100
  return(mape)
}
# mape <- calculate_mape(y1, py$mean)
Prediction1 <- function(Y0,Y1,ylab){
  st = 1
  et = length(Y0)-2
  y0 = Y0[1:et]
  # y0[length(y0)]
  st = length(Y1)-6
  et = length(Y1)
  y1 = Y1[st:et]
  
  py = ArmaEWMA(y0,7)
  mape <- calculate_mape(y1, py$mean)
  plot(py$mean,
       type = 'l',
       lwd = 2,
       col = 'red',
       xlab = paste('Time\n','MAPE of',ylab,mape,"%"),
       ylab = ylab,
       main = 'Prediction from 2023.8.1~2023.8.9',
       ylim = c(2.7,3))
  lines(y1,col = 'black', lwd = 2)
  legend("top",                                    #图例位置为右上角
         legend=c("Prediction","Original"),        #图例内容
         ncol=2,
         cex = 0.7,
         col=c("red","black"),                 #图例颜色
         lty=1,lwd=2)  
  
  # legend("topright",                                    #图例位置为右上角
  #        legend=c("Prediction","Original"),        #图例内容
  #        col=c("red","black"),                 #图例颜色
  #        lty=1,lwd=2)                                          #图例大小 
}
Prediction2 <- function(Y0,Y1,ylab){
  st = 1
  et = length(Y0)-2
  y0 = Y0[1:et]
  # y0[length(y0)]
  st = length(Y1)-6
  et = length(Y1)
  y1 = Y1[st:et]
  
  py = ArmaEWMA(y0,7)
  mape <- calculate_mape(y1, py$mean)
  plot(py$mean,
       type = 'l',
       lwd = 2,
       col = 'red',
       xlab = paste('Time\n','MAPE of',ylab,mape,"%"),
       ylab = ylab,
       main = 'Prediction from 2023.8.1~2023.8.9',
       ylim = c(1.5e+08,3.5e+08))
  lines(y1,col = 'black', lwd = 2)
  legend("top",                                    #图例位置为右上角
         legend=c("Prediction","Original"),        #图例内容
         ncol=2,
         cex = 0.7,
         col=c("red","black"),                 #图例颜色
         lty=1,lwd=2) 
}
Prediction1(BOC$Open,BOCnew$Open,'Open')
Prediction1(BOC$High,BOCnew$High,'High')
Prediction1(BOC$Low,BOCnew$Low,'Low')
Prediction1(BOC$Close,BOCnew$Close,'Close')
Prediction1(BOC$Adj.Close,BOCnew$Adj.Close,'Adj.Close')
Prediction2(BOC$Volume,BOCnew$Volume,'Volume')


# 计算MAPE
mape_close <- calculate_mape(actual_close, forecast_close)
mape_adjclose <- calculate_mape(actual_adjclose, forecast_adjclose)
mape_volume <- calculate_mape(actual_volume, forecast_volume)
mape_open <- calculate_mape(actual_open, forecast_open)
mape_high <- calculate_mape(actual_high, forecast_high)
mape_low <- calculate_mape(actual_low, forecast_low)

# 打印计算结果
print(paste("MAPE for Close Price:", mape_close, "%"))
print(paste("MAPE for Adjusted Close Price:", mape_adjclose, "%"))
print(paste("MAPE for Volume:", mape_volume, "%"))
print(paste("MAPE for Open Price:", mape_open, "%"))
print(paste("MAPE for High Price:", mape_high, "%"))
print(paste("MAPE for Low Price:", mape_low, "%"))
