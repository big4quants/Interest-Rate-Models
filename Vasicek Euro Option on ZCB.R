price_opt_mc <- function(Time,r0,r_mu,K,sigma,dt,strike,maturity,coupon,term){
  # construct rt matrix up to option maturity
  m1=maturity/dt
  m2=Time/dt-m1
  rt1=matrix(0,500,(m1+1))
  rt1[,1]=r0
  pri_CPB_i=pri_CPB_j=c(0)
  for(i in 1:500){
    dw1=rnorm(m1,0,1)
    for(k in 2:(m1+1)){
      rt1[i,k]=rt1[i,(k-1)]+K*(r_mu-rt1[i,(k-1)])*dt+sigma*sqrt(dt)*dw1[(k-1)]
    }
    # rt matrix after option till bond maturity
    rt2=matrix(0,500,(m2+1))
    rt2[,1]=rt1[i,(m1+1)]
    for(j in 1:500){
      dw2=rnorm(m2,0,1)
      for(t in 2:(m2+1)){
        rt2[j,t]=rt2[j,(t-1)]+K*(r_mu-rt2[j,(t-1)])*dt+sigma*sqrt(dt)*dw2[(t-1)]
      }
      coup=c(0)
      coup[1]=coupon*exp(-sum(dt*rt2[j,2:(21*3+1)]))
      for(k in 2:(term-1)){
        coup[k]=coupon*exp(-sum(dt*rt2[j,2:(21*3+21*(k-1)*6+1)]))
      }
      coup[term]=(1+coupon)*exp(-sum(dt*rt2[j,]))
      pri_CPB_j[j]=sum(coup)
    }
    pri_CPB_i[i]=mean(pri_CPB_j)
  }
  
  opt=mean(pmax(pri_CPB_i-strike,0)*exp(-apply(dt*rt1,1,sum)))
  
  return (opt)  
  }


opt_ZCB_mc <- 1000*price_opt_mc(Time=4,r0=0.05,r_mu=0.05,K=0.82,sigma=0.18,dt=1/252,strike=0.98,maturity=0.25,coupon=0.03,term=8)
