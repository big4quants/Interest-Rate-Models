price_opt_CIR1 <- function(Time,r0,r_mu,K,sigma,dt,strike,maturity){
  pri_ZCB=c(0)
  # construct rt matrix up to option maturity
  m1=maturity/dt
  m2=Time/dt-m1
  rt1=matrix(0,500,(m1+1))
  rt1[,1]=r0
  for(i in 1:500){
    dw1=rnorm(m1,0,1)
    for(k in 2:(m1+1)){
      rt1[i,k]=abs(rt1[i,(k-1)]+K*(r_mu-rt1[i,(k-1)])*dt+sigma*sqrt(rt1[i,(k-1)])*sqrt(dt)*dw1[(k-1)])
    }
    
    # rt matrix after option till bond maturity
    rt2=matrix(0,500,(m2+1))
    rt2[,1]=rt1[i,(m1+1)]
    for(j in 1:500){
      dw2=rnorm(m2,0,1)
      for(t in 2:(m2+1)){
        rt2[j,t]=abs(rt2[j,(t-1)]+K*(r_mu-rt2[j,(t-1)])*dt+sigma*sqrt(rt2[j,(t-1)])*sqrt(dt)*dw2[(t-1)])
      }
    }
    pri_ZCB[i]=mean(1*exp(-apply(dt*rt2,1,sum)))
      }
    opt_CIR=mean(pmax(pri_ZCB-strike,0)*exp(-apply(dt*rt1,1,sum)))
    
    return(opt_CIR)
  }

opt_ZCB_CIR1 <- 1000*price_opt_CIR1(Time=1,r0=0.05,r_mu=0.055,K=0.92,sigma=0.18,dt=1/252,strike=0.98,maturity=0.5)
