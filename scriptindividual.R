
f2 <- function(data, iterations, linfrange){
  # Now is time for the Individual models
  library(R2jags) 
  
  #-------------
  # Section 3: BAYESIAN ESTIMATION OF VON BERTALANFY PARAMETERS
  # INDIVIDUAL MODEL Several observations per fish
  #
  #-------------
  #STEP 1: INITIALS
  #-------------
 # getwd()
   # data<-read.csv(file = "www/D.annularis.csv",sep=";")
  
  #names(data)
  ID=NULL
  for (i in 1: length(unique(data$fishID))){
    temp2<-rep(i,length(data$age[which(data$fishID==unique(data$fishID)[i])]))
    
    ID=c(ID,temp2)
  }
  data$ID=ID
  
  
  #maximum age of each fish== later will be AGE
  Age=NULL
  for (i in 1: length(unique(data$fishID))){
    temp<-max(data$age[which(data$fishID==unique(data$fishID)[i])])
    Age=c(Age,temp)
  }
  maxage=max(Age)
  
  nfish=length(unique(data$fishID))# Sample size

#linfrange<-c(100,300)  

  inits <- function() list(
    Linf=runif(nfish,linfrange[1],linfrange[2]),
    k=runif(nfish,0,1),
    T0=runif(nfish,-1,1),
    tau=1,
    tauk=1,
    tauLinf=1,
    tauT0=1
  )
  
  #-------------
  #STEP 2:DATA
  #-------------
  
  
  # Prepare the data for JAGS
  jags.data <- list(
    nobs=length(data$age),
    nfish=nfish,
    age=data$age,
    size=data$size,
    ID=data$ID,
    maxAge=maxage+2,
    
    Linfmin=linfrange[1],
    Linfmax=linfrange[2]
    
    
  )
  
  #-------------
  #STEP 3: INDIVIDUAL BAYESIAN MODEL
  #-------------
  
  sink("individualmodel.txt")
  cat("
    model {
      for( i in 1 : nobs ) {
      mu[i]<-Linf[ID[i]]*(1-exp(-k[ID[i]]*(age[i]-T0[ID[i]])))
      #size[i] ~ dnorm(mu[i],tau)
      shape[i]<-(mu[i]/sd)^2
      rate[i]<-mu[i]/(sd)^2
      size[i] ~ dgamma(shape[i],rate[i])
      }
      
      #priors individual fish (ojo!the same within group variance)
      for( i in 1 : nfish ) {
      T0[i]~dnorm(mT0,tauT0)
      k[i]~dnorm(mk,tauk)
      Linf[i]~dnorm(mLinf,tauLinf)
      }
      
      #prior groups
      mT0~dunif(-2,2)
      mk~dunif(0,1)
      mLinf~dunif(Linfmin,Linfmax)
      
      #prior variances (flat prior)
      tau ~ dgamma(0.001,0.001)
      sd<-1/sqrt(tau)
      tauT0 ~ dgamma(0.001,0.001)
      tauk ~ dgamma(0.001,0.001)
      tauLinf ~ dgamma(0.001,0.001)
     
#estimating size at age
    for(i in 1 : maxAge) {
    estimated[i]<-mLinf*(1-exp(-mk*(i-(mT0))))
    }

}
      ",fill = TRUE)
sink()

  
  #-------------
  #STEP 4: RUNNING THE MODEL AND UPDATING
  #-------------
  
  # Parameters monitored
  params =c("T0","Linf","k","mT0","mLinf","mk","estimated")
  
  # MCMC settings
  nt <- 5
  ni <- 10000
  nb <- 100
  nc <- 3
  
  out <- jags(jags.data, inits, params, "individualmodel.txt", 
              n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
  
  # Since this is calculating one curve per fish it is really time consuming and 
  # only a few traceplots are usefull to see if the Markov Chains have converged
  
  #traceplot(out,varname=c("deviance"))
  #traceplot(out,varname=c("Linf.alpha"))
  #traceplot(out,varname=c("Linf.beta"))
  #traceplot(out,varname=c("k.alpha"))
  #traceplot(out,varname=c("k.beta"))
  
  # A rough estimation of how long the updating process will take 
  # is provided here. 
#iterations=1000
  pre.time <- Sys.time()
  out <- update(out, n.iter=iterations,n.thin=10)
  post.time <- Sys.time()
  post.time-pre.time
  
  ### WARNING VERY TIME CONSUMING (HOURS)
  # out <- update(out, n.iter=500000,n.thin=100)
  
  
  #-------------
  #STEP 5: RESULTS VISUALIZATION 
  #-------------
  
  # To show the results and the estimates of the von Bertalanffy parameters
  print(out)
  # Or
  out$BUGSoutput$summary
  
}
# Plot to see the results

f4 <- function(indparams,data){
  
  individual=indparams
 # individual= out$BUGSoutput$summary
  
  #data<-read.csv(file = "data.csv",sep=";")
  #names(data)
  ID=NULL
  for (i in 1: length(unique(data$fishID))){
    temp2<-rep(i,length(data$age[which(data$fishID==unique(data$fishID)[i])]))
    
    ID=c(ID,temp2)
  }
  data$ID=ID 
  
  #maximum age of each fish== later will be AGE
  AGE=NULL
  for (i in 1: length(unique(data$fishID))){
    temp<-max(data$age[which(data$fishID==unique(data$fishID)[i])])
    AGE=c(AGE,temp)
  }
  
  
  nfish=length(unique(data$fishID))
  
  # Prepare the data for JAGS
  Age=data$age
  age=data$age
  size=data$size
  Ind=data$ID
  max(Age)
  
  seq=seq(1,max(Age)+2)
  

plot(age,size,type="n",main="Von Bertalanffy Individual Model",
     ylab= "Size (units of size)", xlab="Age (years)", 
     xlim= c(1,max(seq)), ylim= c(0,max(size)), las=1)


for(i in 1:nfish){
    temp=which(Ind==i)
    lines(age[temp],size[temp], col="grey")
  }


pre=NULL
for (i in 1:(max(Age)+2)){
  temp2=parse(text=paste("individual['estimated[",i,"]',
                         c(3,5,7)]",sep = ""))
  pre=rbind(pre,eval(temp2))
}

# Median estimations of the parameters
lines(seq(1,max(Age)+2),pre[,2],type="l",col="blue",lwd=2) 
# Credibility intervals of the estimations
lines(seq(1,max(Age)+2),pre[,1],col="red",lty=3)
lines(seq(1,max(Age)+2),pre[,3],col="red",lty=3)

}


