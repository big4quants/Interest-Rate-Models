price_ZCB<-function(Time,r0,r_mu,K,sigma,dt,principal){
  m=Time/dt
  rt=matrix(0,1000,(m+1))
  rt[,1]=r0

  for(i in 1:1000){
    dw=rnorm(m,0,1)
    for(k in 2:(m+1)){
      # Standard Vasicek model, NO treatment of negative value
      rt[i,k]=rt[i,(k-1)]+K*(r_mu-rt[i,(k-1)])*dt+sigma*sqrt(dt)*dw[(k-1)]
    }
  }
  price=mean(principal*exp(-apply(dt*rt[,2:(m+1)],1,sum))) # get the row mean of semi year spot rate
  
  return (price)
}
