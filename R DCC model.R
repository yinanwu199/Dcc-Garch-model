x<- read.csv('/Users/YinanWu/Downloads/g1.csv',header = TRUE,sep=',')
 y<- read.csv('/Users/YinanWu/Downloads/a1.csv',header = TRUE,sep=',')

 x$return[1] <- 0
 y$return[1] <- 0
for (i in 2:755){
  x$return[i]<- log(x$Adj.Close[i-1])-log(x$Adj.Close[i])
}
 for (j in 2:755){
   y$return[j]<- log(y$Adj.Close[j-1])-log(y$Adj.Close[j])
 }
stock1 <- x$return[-1]
stock2 <- y$return[-1]
T=length(stock1)
shapiroTest(stock1)
jarqueberaTest(stock2)
#they are both normally distributed  
c1 <- cor(stock1,stock2)
			p<- cbind(stock1,stock2)

f1 = garchFit(~ garch(1,1), data=stock1,include.mean=FALSE)
f1 = f1@fit$coef
omega = f1@fit$matcoef[1,1]
alpha = f1@fit$matcoef[2,1]
beta = f1@fit$matcoef[3,1]
f2 = garchFit(~ garch(1,1), data=stock2,include.mean=FALSE)
f2 = f2@fit$coef
a = c(f1[1], f2[1]) 
A = diag(c(f1[2],f2[2]))
B = diag(c(f1[3], f2[3])) 
dccpara = c(0.2,0.6) 
dccresults = dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dccpara,dvar=p, 
                            model="diagonal")
DCCrho = dccresults$DCC[,2]
DCCh= dccresults$h
h1 <- DCCh[,1]
h1
h2 <- DCCh[,2]

sqrt(h1[1+1])
Res[754]<-  9.766737e-06

for(t in 753:1){Res[t]=omega+alpha*sqrt(h1[t+1])*stock1[t]*sqrt(h1[t+1])*stock1[t]
+beta*Res[t+1]}
Res
R <- sqrt(Res)*Res*sqrt(Res)

sigma1 = sqrt(h1)*R*sqrt(h1)
VaR = -sqrt(sigma1) * qnorm(0.05) * stock1
VaR
