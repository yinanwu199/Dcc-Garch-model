library(ccgarch)
library(fGarch)
stock1<- read.csv('/Users/YinanWu/Downloads/google.csv',header = TRUE,sep=',')
stock2<- read.csv('/Users/YinanWu/Downloads/apple.csv',header = TRUE,sep=',')
stock3<- read.csv('/Users/YinanWu/Downloads/nike.csv',header = TRUE,sep=',')
stock4<- read.csv('/Users/YinanWu/Downloads/mi.csv',header = TRUE,sep=',')

stock1 <- stock1[order(stock1$Date),]
stock2 <- stock2[order(stock2$Date),]
stock3 <- stock3[order(stock3$Date),]
stock4 <- stock4[order(stock4$Date),]

stock1$ve[1] <- 0
stock2$ve[1] <- 0
stock3$ve[1] <- 0
stock4$ve[1] <- 0 
stock1$return[1] <- 0
stock2$return[1] <- 0
stock3$return[1] <- 0
stock4$return[1] <- 0
for (i in 2:755){
  stock1$ve[i]<- stock1$Adj.Close[i]-stock1$Adj.Close[i-1]
  stock2$ve[i]<- stock2$Adj.Close[i]-stock2$Adj.Close[i-1]
  stock3$ve[i]<- stock3$Adj.Close[i]-stock3$Adj.Close[i-1]
  stock4$ve[i]<- stock4$Adj.Close[i]-stock4$Adj.Close[i-1]
  stock1$return[i]<- log(stock1$Adj.Close[i])-log(stock1$Adj.Close[i-1])
  stock2$return[i]<- log(stock2$Adj.Close[i])-log(stock2$Adj.Close[i-1])
  stock3$return[i]<- log(stock3$Adj.Close[i])-log(stock3$Adj.Close[i-1])
  stock4$return[i]<- log(stock4$Adj.Close[i])-log(stock4$Adj.Close[i-1])
}

window <- 20 
value <- cbind(stock1$Adj.Close[(754-window):754],stock2$Adj.Close[(754-window):754],
               stock3$Adj.Close[(754-window):754],stock4$Adj.Close[(754-window):754])
for (i in 1:window+1) {
  pf[i] <-value[i,1] +value[i,2] +value[i,3] +value[i,4] 
}
w <- c(value[1,1]/pf[1],value[1,2]/pf[1],
       value[1,3]/pf[1],value[1,4]/pf[1])

###########
stock1 <- stock1$return[-1]
stock2 <- stock2$return[-1]
stock3 <- stock3$return[-1]
stock4 <- stock4$return[-1]
VaR<-matrix(c(1,2),ncol=2,nrow = window)
v <- matrix(c(1,2),ncol=4)

j <-1

for(j in 1:window){
  s1 <- stock1[c(j:(753-window+j))]
  s2 <- stock2[c(j:(753-window+j))]
  s3 <- stock3[c(j:(753-window+j))]
  s4 <- stock4[c(j:(753-window+j))]
  T=length(s1)
  p<- cbind(s1,s2,s3,s4)
  f1 = garchFit(~ garch(1,1), data=s1,include.mean=FALSE)
  ft1 <- f1@residuals[T]
  f1 = f1@fit$coef
  
  f2 = garchFit(~ garch(1,1), data=s2,include.mean=FALSE)
  ft2 <- f2@residuals[T]
  f2 = f2@fit$coef
  
  f3 = garchFit(~ garch(1,1), data=s3,include.mean=FALSE)
  ft3 <- f3@residuals[T]
  f3 = f3@fit$coef
  
  f4 = garchFit(~ garch(1,1), data=s4,include.mean=FALSE)
  ft4 <- f4@residuals[T]
  f4 = f4@fit$coef
  
  a = c(f1[1], f2[1],f3[1],f4[1]) 
  A = diag(c(f1[2],f2[2],f3[2],f4[2]))
  B = diag(c(f1[3], f2[3],f3[3],f4[3]))
  dccpara = c(0.2,0.6) 
  dccresults = dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dccpara,dvar=p, 
                              model="diagonal")
  
  para<-  dccresults$out
  alpha <- para[1,13]
  beta <-  para[1,14]
  h=dccresults$h
  dcc=dccresults$DCC
  si1=f1[1]+ft1^2*f1[2]+f1[3]*h[T,1]
  si2=f2[1]+ft2^2*f2[2]+f2[3]*h[T,2]
  si3=f3[1]+ft3^2*f3[2]+f3[3]*h[T,3]  
  si4=f4[1]+ft4^2*f4[2]+f4[3]*h[T,4]
  v=sqrt(diag(c(si1,si2,si3,si4)))
  
  R=matrix(data = dcc[1,],nrow = 4, ncol = 4)
  
  R=R*(alpha+beta)
  H=v%*%R%*%v
  
  
  VaR[j,1]=-sqrt(t(w)%*%H%*%w)*qnorm(0.05)*pf[1]
  VaR[j,2]=-sqrt(t(w)%*%H%*%w)*qnorm(0.01)*pf[1]
}
plot(VaR[,1],type="l")
plot(VaR[,2],type="l")
VaRTest(alpha=0.05,actual = pf[c(2:21)],VaR[,1],conf.level = 0.95)
VaRTest(alpha=0.01,actual = pf[c(2:21)],VaR[,2],conf.level = 0.99)

