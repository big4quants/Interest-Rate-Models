price_opt_Gplus <- function(Time,dt,r0,sigma,delta,rho,phi,strike,maturity){
  x0=0
  y0=0
  a=0.1
  b=0.3
  
  pri_ZCB=c(0)
  # construct rt matrix up to option maturity
  m1=maturity/dt
  m2=Time/dt-m1
  rt1=xt1=yt1=matrix(0,1000,(m1+1))
  xt1[,1]=yt1[,1]=x0
  rt1[,1]=r0
  for(i in 1:1000){
    dw1=rnorm(m1,0,1)
    dw2=rho*dw1+sqrt(1-rho^2)*rnorm(m1,0,1) # dw1 x dw2 = rho dt
    
    for(k in 2:(m1+1)){
      xt1[i,k]=xt1[i,(k-1)]-a*xt1[i,(k-1)]*dt+sigma*sqrt(dt)*dw1[(k-1)]
      yt1[i,k]=yt1[i,(k-1)]-b*yt1[i,(k-1)]*dt+delta*sqrt(dt)*dw2[(k-1)]
      rt1[i,k]=xt1[i,k]+yt1[i,k]+phi
    }
    
    # rt matrix after option till bond maturity
    rt2=xt2=yt2=matrix(0,1000,(m2+1))
    xt2[,1]=xt1[i,(m1+1)]
    yt2[,1]=yt1[i,(m1+1)]
    rt2[,1]=rt1[i,(m1+1)]
    for(j in 1:1000){
      dw3=rnorm(m2,0,1)
      dw4=rho*dw1+sqrt(1-rho^2)*rnorm(m2,0,1) # dw3 x dw4 = rho dt
      for(t in 2:(m2+1)){
        xt2[j,t]=xt2[j,(t-1)]-a*xt2[j,(t-1)]*dt+sigma*sqrt(dt)*dw3[(t-1)]
        yt2[j,t]=yt2[j,(t-1)]-b*yt2[j,(t-1)]*dt+delta*sqrt(dt)*dw4[(t-1)]
        rt2[j,t]=xt2[j,t]+yt2[j,t]+phi
      }
    }
    pri_ZCB[i]=mean(1*exp(-apply(dt*rt2,1,sum)))
  }
  opt_CIR=mean(pmax(strike-pri_ZCB,0)*exp(-apply(dt*rt1,1,sum)))
  
  return(opt_CIR)
}

opt_Gplus <- 1000*price_opt_Gplus(Time=1,dt=1/252,r0=0.03,sigma=0.03,delta=0.08,rho=0.7,phi=0.03,strike=0.985,maturity=0.5)
