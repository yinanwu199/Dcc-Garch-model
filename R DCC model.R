x<- read.csv('/Users/YinanWu/Downloads/g1.csv',header = TRUE,sep=',')
y<- read.csv('/Users/YinanWu/Downloads/a1.csv',header = TRUE,sep=',')

x$return[1] <- 0
y$return[1] <- 0
### calculate the log return
for (i in 2:755){
  x$return[i]<- log(x$Adj.Close[i-1])-log(x$Adj.Close[i])
}
for (j in 2:755){
  y$return[j]<- log(y$Adj.Close[j-1])-log(y$Adj.Close[j])
}
### stock 1 is the google stock price log return
### stock 2 is the apple stock price log return
stock1 <- x$return[-1]
stock2 <- y$return[-1]
T=length(stock1)
#### they are both normally distributed  
shapiroTest(stock1)
jarqueberaTest(stock2)
### combine the two stocks into a list.
c1 <- cor(stock1,stock2)
p<- cbind(stock1,stock2)
### apply the garch model in each model get omega, alpha and beta. 
f1 = garchFit(~ garch(1,1), data=stock1,include.mean=FALSE)
f1 = f1@fit$coef
f2 = garchFit(~ garch(1,1), data=stock2,include.mean=FALSE)
f2 = f2@fit$coef
a = c(f1[1], f2[1]) 
A = diag(c(f1[2],f2[2]))
B = diag(c(f1[3], f2[3])) 
dccpara = c(0.2,0.6) 
### DCC garch model 
dccresults = dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dccpara,dvar=p, 
                            model="diagonal")
DCCrho = dccresults$DCC[,2]
DCCh= dccresults$h
### define the matrices
H <- as.matrix(1)
D<- as.matrix(1)
R<- as.matrix(1)
V<- matrix(c(1,2,3,4),nrow=754,ncol = 3)
### get the volatility, and value at risk
for (t in 1:754) {
  D <- diag(c(dccresults$h[t,1],dccresults$h[t,2]))
  R <- matrix(c(1,DCCrho[t],DCCrho[t],1),nrow=2)
  H <- D*R*D
  VaR= -sqrt(H)*qnorm(0.10)*c(p[t,1],p[t,2])
  V[t,1] <- VaR[1,1]
  V[t,2] <- VaR[2,2]
  V[t,3] <- paste (x$Date[t])
}
### plot the value at risk 
plot(V[,1],type='l')
plot(V[,2],type='l')
