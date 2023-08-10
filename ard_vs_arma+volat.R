
ardpredict<-function(y,steps){prob1=.5;prob2=.67;prob3=.95;prob4=.99;

s=7;h=steps;
w<-rep(1/s,s);
cma<-matrix(NA,length(y),1);
for(g in 4:(length(y)-3)){cma[g]<-sum(w*y[(g-3):(g+3)])};
residuals<-y-cma
sfactors<-c();for(seas in 1:s){
  sfactors[seas]<-mean(na.omit(residuals[seq(seas,length(y)-s+seas,by=s)]))}
#sfactors<-sfactors-mean(sfactors)
sfactout<-rep(sfactors,length(y)+h)[(length(y)+1):(length(y)+h)]
y<-y-rep(sfactors,ceiling(length(y)/s))[1:length(y)]

m<-v<-rep(0,length(y));
### Non-persistent model
like0<-function(param){co=param[1];q=param[2];p=1-exp(-abs(param[3]))
m[1]=co/(1-p);
k=(-1 - q + p*p +sqrt(4*q+(-1+q+p*p)*(-1+q+p*p)))/(2*p);
for (t in 1:(length(y))) {
  v[t]<-y[t]-m[t]
  m[t+1]<-co+p*m[t]+k*v[t]
}
sum(v^2)
}
aa0<-optim(c(.03,1,4),like0);
co=aa0[[1]][1];q=aa0[[1]][2];p=1-exp(-abs(aa0[[1]][3]))
m[1]=co/(1-p);
k=(-1 - q + p*p +sqrt(4*q+(-1+q+p*p)*(-1+q+p*p)))/(2*p);
for (t in 1:(length(y))) {
  v[t]<-y[t]-m[t]
  m[t+1]<-co+p*m[t]+k*v[t]
}
lik0=sum(v^2)

Forecasts0<-c()
Forecasts0[1]=m[length(m)]
for(s in 2:steps){Forecasts0[s]=co+p*Forecasts0[s-1]}

Forecasts0=Forecasts0+sfactout
riccati=.5*(-1 + q + p*p + sqrt(4* q + (-1 + q + p*p)*(-1 + q + p*p)));
f=riccati+1;
LIST0=list(Forecasts0,riccati,sum(v^2)/(f*length(y)),p,q)

### Persistent model
like1<-function(param){co=param[1];q=param[2];p=1
m[1]=y[1]
k=(-1 - q + p*p +sqrt(4*q+(-1+q+p*p)*(-1+q+p*p)))/(2*p);
for (t in 1:(length(y))) {
  v[t]<-y[t]-m[t]
  m[t+1]<-co+p*m[t]+k*v[t]
}
sum(v^2)
}
aa1<-optim(c(.03,1),like1);
co=aa1[[1]][1];q=aa1[[1]][2];p=1
m[1]=y[1];
k=(-1 - q + p*p +sqrt(4*q+(-1+q+p*p)*(-1+q+p*p)))/(2*p);
for (t in 1:(length(y))) {
  v[t]<-y[t]-m[t]
  m[t+1]<-co+p*m[t]+k*v[t]
}
lik1=sum(v^2)

Forecasts1<-c()
Forecasts1[1]=m[length(m)]
for(s in 2:steps){Forecasts1[s]=co+Forecasts1[s-1]}

Forecasts1=Forecasts1+sfactout
riccati=.5*(-1 + q + p*p + sqrt(4* q + (-1 + q + p*p)*(-1 + q + p*p)));
f=riccati+1;
LIST1=list(Forecasts1,riccati,sum(v^2)/(f*length(y)),p,q)
if(length(y)*log(lik1/lik0)<3.84){output=LIST1}else{output=LIST0}

# Prediction intervals

H=output[[3]];p=output[[4]];q=output[[5]];Eta=H*q;P= output[[2]]*H
Pfor<-c();Pfor[1]=Eta;if(steps>1){for(f in 2:steps){Pfor[f]=Pfor[f-1]+(p^(f-1))*Eta*(p^(f-1))}}
Interv<-c();for(j in 1:steps){Interv[j]=(p^j)*P*(p^j)+Pfor[j]+H};
fo=output[[1]]
lower0<-upper0<-fo
lower50<-fo-qnorm((1+prob1)/2)*sqrt(Interv);
lower67<-fo-qnorm((1+prob2)/2)*sqrt(Interv);
lower95<-fo-qnorm((1+prob3)/2)*sqrt(Interv);
lower99<-fo-qnorm((1+prob4)/2)*sqrt(Interv);
upper50<-fo+qnorm((1+prob1)/2)*sqrt(Interv);
upper67<-fo+qnorm((1+prob2)/2)*sqrt(Interv);
upper95<-fo+qnorm((1+prob3)/2)*sqrt(Interv);
upper99<-fo+qnorm((1+prob4)/2)*sqrt(Interv);
list(mean=fo,lower=cbind(lower0,lower50,lower67,lower95,lower99),upper=cbind(upper0,upper50,upper67,upper95,upper99))

}


ArmaEWMA<-function(y,steps){prob1=.5;prob2=.67;prob3=.95;prob4=.99;yy=y
s=7;h=steps;w<-rep(1/s,s);cma<-matrix(NA,length(y),1);
for(g in 4:(length(y)-3)){cma[g]<-sum(w*y[(g-3):(g+3)])};
residuals<-y-cma;sfactors<-c();for(seas in 1:s){
  sfactors[seas]<-mean(na.omit(residuals[seq(seas,length(y)-s+seas,by=s)]))}
sfactout<-rep(sfactors,length(y)+h)[(length(y)+1):(length(y)+h)]
y<-y-rep(sfactors,ceiling(length(y)/s))[1:length(y)]

ARMAg=function(para){phi=para[1];theta=para[2];co=para[3];
v<-m<-f<-rep(0,length(y));alpha=.03*(1-exp(-abs(para[4])));
beta=1-alpha;
m[1]=0;v[1]=y[1];f[1]=var(y);like=0;K=phi+theta
for(t in 2:length(y)){
  m[t]=phi*m[t-1]+K*v[t-1]
  v[t]=y[t]-co-m[t]
  f[t]=f[t-1];if(yy[t]>0){f[t]<-alpha*v[t-1]^2+beta*f[t-1];}
  like<-like+.5*log(2*pi+1)+.5*log(f[t])+.5*(v[t]^2)/(f[t])
}
like
}

res=optim(c(.9,-.5,mean(y)/2,3),ARMAg);phi=res[[1]][1];if(phi>1){phi=1};if(phi< -1){phi=-1}
co=res[[1]][3];theta=res[[1]][2];if(theta< -1){theta=-1};if(theta>1){theta=1}
v<-m<-f<-rep(0,length(y));alpha=.03*(1-exp(-abs(res[[1]][4])));
beta=1-alpha;m[1]=0;v[1]=y[1];f[1]=var(y)
K=phi+theta
for(t in 2:length(y)){
  m[t]=phi*m[t-1]+K*v[t-1]
  v[t]=y[t]-co-m[t]
  f[t]=f[t-1];if(yy[t]>0){f[t]<-alpha*v[t-1]^2+beta*f[t-1];}
}
# 预测
mf<-rep(0,steps);mf[1]=phi*m[length(y)]+K*v[length(y)]
for(s in 2:steps){mf[s]=phi*mf[s-1]}

fo=(mf+co)+sfactout;fout=c();fout[1]=alpha*v[length(y)]^2+beta*f[length(y)]
for(s in 2:steps){fout[s]=beta*fout[s-1]}
# 置信区间
Interv<-c();Interv[1]=fout[1];for(j in 2:steps){Interv[j]=Interv[j-1]+phi^(2*(j-2))*K^(2)*fout[j]};
lower0<-upper0<-fo;lower50<-fo-qnorm((1+prob1)/2)*sqrt(Interv);
lower67<-fo-qnorm((1+prob2)/2)*sqrt(Interv);lower95<-fo-qnorm((1+prob3)/2)*sqrt(Interv);
lower99<-fo-qnorm((1+prob4)/2)*sqrt(Interv);upper50<-fo+qnorm((1+prob1)/2)*sqrt(Interv);
upper67<-fo+qnorm((1+prob2)/2)*sqrt(Interv);upper95<-fo+qnorm((1+prob3)/2)*sqrt(Interv);
upper99<-fo+qnorm((1+prob4)/2)*sqrt(Interv);list(mean=fo,lower=cbind(lower0,lower50,lower67,lower95,lower99),
                                                 upper=cbind(upper0,upper50,upper67,upper95,upper99))
}


# ArmaEWMA2<-function(y,steps){prob1=.5;prob2=.67;prob3=.95;prob4=.99;
# s=7;h=steps;w<-rep(1/s,s);cma<-matrix(NA,length(y),1);
# for(g in 4:(length(y)-3)){cma[g]<-sum(w*y[(g-3):(g+3)])};
# residuals<-y-cma;sfactors<-c();for(seas in 1:s){
#   sfactors[seas]<-mean(na.omit(residuals[seq(seas,length(y)-s+seas,by=s)]))}
# sfactout<-rep(sfactors,length(y)+h)[(length(y)+1):(length(y)+h)]
# y<-y-rep(sfactors,ceiling(length(y)/s))[1:length(y)]
# 
# ARMAg=function(para){phi=para[1];theta=-abs(para[2]);co=para[3];
# v<-m<-f<-rep(0,length(y));alpha=.03*(1-exp(-abs(para[4])));
# beta=1-alpha;
# m[1]=0;v[1]=y[1];f[1]=var(y);like=0;K=phi+theta
# for(t in 2:length(y)){
#   m[t]=phi*m[t-1]+K*v[t-1]
#   v[t]=y[t]-co-m[t]
#   f[t]<-alpha*v[t-1]^2+beta*f[t-1];
#   like<-like+.5*log(2*pi+1)+.5*log(f[t])+.5*(v[t]^2)/(f[t])
# }
# like
# }
# 
# res=optim(c(.9,-.5,mean(y)/2,3),ARMAg);phi=res[[1]][1];if(phi>1){phi=1};if(phi< -1){phi=-1}
# co=res[[1]][3];theta=-abs(res[[1]][2]);if(theta< -1){theta=-1}
# v<-m<-f<-rep(0,length(y));alpha=.03*(1-exp(-abs(res[[1]][4])));
# beta=1-alpha;m[1]=0;v[1]=y[1];f[1]=var(y)
# K=phi+theta
# for(t in 2:length(y)){
#   m[t]=phi*m[t-1]+K*v[t-1]
#   v[t]=y[t]-co-m[t]
#   f[t]<-alpha*v[t-1]^2+beta*f[t-1]
# }
# 
# mf<-rep(0,steps);mf[1]=phi*m[length(y)]+K*v[length(y)]
# for(s in 2:steps){mf[s]=phi*mf[s-1]}
# 
# fo=(mf+co)+sfactout;fout=c();fout[1]=alpha*v[length(y)]^2+beta*f[length(y)]
# for(s in 2:steps){fout[s]=beta*fout[s-1]}
# 
# Interv<-c();Interv[1]=fout[1];for(j in 2:steps){Interv[j]=Interv[j-1]+phi^(2*(j-2))*K^(2)*fout[j]};
# lower0<-upper0<-fo;lower50<-fo-qnorm((1+prob1)/2)*sqrt(Interv);
# lower67<-fo-qnorm((1+prob2)/2)*sqrt(Interv);lower95<-fo-qnorm((1+prob3)/2)*sqrt(Interv);
# lower99<-fo-qnorm((1+prob4)/2)*sqrt(Interv);upper50<-fo+qnorm((1+prob1)/2)*sqrt(Interv);
# upper67<-fo+qnorm((1+prob2)/2)*sqrt(Interv);upper95<-fo+qnorm((1+prob3)/2)*sqrt(Interv);
# upper99<-fo+qnorm((1+prob4)/2)*sqrt(Interv);list(mean=fo,lower=cbind(lower0,lower50,lower67,lower95,lower99),
#                                                  upper=cbind(upper0,upper50,upper67,upper95,upper99))
# }