# 1.b Use Monte-Carlo Simulation to price a coupon-paying bond
price_CPB<-function(Time,r0,r_mu,K,sigma,dt,principal,coupon,term){
  m=Time*252
  rt=matrix(0,1000,(m+1))
  rt[,1]=r0
  
  for(i in 1:1000){
    dw=rnorm(m,0,1)
    for(k in 2:(m+1)){
      rt[i,k]=rt[i,(k-1)]+K*(r_mu-rt[i,(k-1)])*dt+sigma*sqrt(dt)*dw[(k-1)]
    }
  }
  
  price=c(0)
  for(i in 1:(term-1)){
    price[i]=mean(coupon*exp(-apply(dt*rt[,2:(i*(m/term)+1)],1,sum)))
  }
  price[term]=mean((principal+coupon)*exp(-apply(dt*rt[,2:(m+1)],1,sum)))
  
 return(sum(price))
}

CPB <- price_CPB(Time=4,r0=0.05,r_mu=0.05,K=0.82,sigma=0.18,dt=1/252,principal=1000,coupon=30,term=8)
