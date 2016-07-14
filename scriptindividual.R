
f2 <- function(data, iterations, linfrange){
  
  #------------------------------------------------
  # "Life-history growth-mortality tradeoffs as revealed by 
  #  otolith geochemistry in a sedentary coastal fish"
    
  # I.A. Catalan, J. Alos, C. Diaz-Gil, S. Perez, 
  # G. Basterretxea, B. Morales-Nin and M. Palmer
    
  # Model designed and implemented by Miquel Palmer
  # Shiny app designed and constructed by Carlos Diaz-Gil and
  # Roc Itxart Alba
  # 
  #
  # Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)
  # Laboratory of Marine Research and Aquaculture (LIMIA)
    
  #------------------------------------------------
  # General objective: 
    
  # Objective: Comparing Von Bertalanffy growth models  (VBGM)
  # at population and individual levels (fish of diffent ages) 
  # Last update: Jun 22, 2016 
  # 
  #
  # Section 1 Load the data from the example or any fish data
  # (age, size between otoliths marks deposited in the otoliths 
  # annually, or size of the fish plus fishID)
  # Section 2 Bayesian estimation for the individual von Bertalanffy 
  # model (back calculated ages)
  #
  #
  #------------------------------------------------
  # Section 1: Loading libraries and data
  #------------------------------------------------
  # Required libraries
  # It is necessary to download JAGS (Just Another Gibbs Sampler)
  # http://mcmc-jags.sourceforge.net/
  # And the R library to interact with it for the Bayesian approach
    
    library(R2jags) 
    
  # As extra information data has been obtained from fish 
  # obtained from commercial landings, measured and otoliths 
  # extracted, afterwards the otoliths were photographed and 
  # the position of the core, and each annual deposit mark 
  # recorded using the TPSdig2 software (version 2.17. Available at 
  # http://life.bio.sunysb.edu/ee/rohlf/software.html (Rohlf 2007). 
  # Alternatively ImageJ software can be used to obtain x,y 
  # coordinates of the points marked. In this case we directly 
  # provide with the necessary data for running both models
  
    
  #-------------
  # Section 2: BAYESIAN ESTIMATION OF VON BERTALANFY PARAMETERS
  # INDIVIDUAL MODEL Several observations per fish
  #
  #-------------
  #STEP 1: INITIALS
  #-------------
  
  # Manual load of the data of interest
    
  # data<-read.csv(file = "www/DannularisPalma.csv",sep=";")
  
    
    
  # Getting a numeric ID for each fish that repeat itself
  
    ID=NULL
      for (i in 1: length(unique(data$fishID))){
        temp2<-rep(i,length(data$age[which(data$fishID==unique(data$fishID)[i])]))
        ID=c(ID,temp2)
        }
    
    data$ID=ID
    
    
  #Maximum age of each fish
    
    Age=NULL
      for (i in 1: length(unique(data$fishID))){
        temp<-max(data$age[which(data$fishID==unique(data$fishID)[i])])
        Age=c(Age,temp)
        }
    
    maxAge=max(Age)
    
    nfish=length(unique(data$fishID))# Sample size
  
  #-------------
  #STEP 1: INITIALS
  #-------------
  
  # linfrange is an input from the shiny app, it can be changed manually here
    
  #  linfrange<-c(100,400)
  
  ## These are the initial values of the bayesian model
  
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
      maxAge= maxAge+2,
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
          #size[i] ~ dnorm(mu[i],tau)#Here you can change from a gamma distribution to a normal distribution
          shape[i]<-(mu[i]/sd)^2
          rate[i]<-mu[i]/(sd)^2
          size[i] ~ dgamma(shape[i],rate[i])
          }
        
        #priors individual fish (the same within group variance)
        
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
  #iterations=10000 # this parameter is given by the shiny app, it can be manually changed here
    
    pre.time <- Sys.time()
    out <- update(out, n.iter=iterations,n.thin=10)
    post.time <- Sys.time()
    post.time-pre.time
    
  # Another indicator of convergence is the Rhat that will be close to 1 at convergence
  
  ## WARNING VERY TIME CONSUMING (HOURS)
    # out <- update(out, n.iter=500000,n.thin=100)
    
    
  #-------------
  #STEP 5: RESULTS VISUALIZATION 
  #-------------
    
  # To show the results and the estimates of the von Bertalanffy parameters
    print(out)
  # Or
    out$BUGSoutput$summary
  
  #this is to assure a clean save of the results.
  col.names<-c("Parameter", "mean" , "sd"  ,  "2.5%" , "25%"  , "50%" ,  "75%" ,  "97.5%" ,"Rhat" , "n.eff")
  row.names<-rownames(out$BUGSoutput$summary)
  a<-array(cbind(row.names,signif(out$BUGSoutput$summary,3)), dim=c(dim(out$BUGSoutput$summary)[1],dim(out$BUGSoutput$summary)[2]+1))
  dim(a)
  colnames(a)<-col.names
  results<-a
  results<-print(results, row.names=FALSE)
  
  
}

# Plot to see the results

f4 <- function(indparams,data){
  
  # The plot is made out of the shiny app output but in case
  # you are using this manually you will need to get "est" from the
  # bugsoutput (See line below)
  
  # individual= results
    individual=indparams
    
  # This part reshape the data as at the beginning but for the plotting
    
  ID=NULL
    for (i in 1: length(unique(data$fishID))){
      temp2<-rep(i,length(data$age[which(data$fishID==unique(data$fishID)[i])]))
      ID=c(ID,temp2)
    }
    data$ID=ID 
    
  # Maximum age of each fish
  
  Age=NULL
  for (i in 1: length(unique(data$fishID))){
    temp<-max(data$age[which(data$fishID==unique(data$fishID)[i])])
    Age=c(Age,temp)
  }  
  
  nfish=length(unique(data$fishID))  # number of fish
    
  # Prepare the data for plotting
  
    age=data$age
    size=data$size
    Ind=data$ID
    maxAge=max(Age)
    seq=seq(1,maxAge+2)
    
  # Plotting
  
  plot(age,size,type="n",main="Von Bertalanffy Individual Model",
       ylab= "Size (units of size)", xlab="Age (years)", 
       xlim= c(1,max(seq)), ylim= c(0,max(size)), las=1)
  
  
  for(i in 1:nfish){
      temp=which(Ind==i)
      lines(age[temp],size[temp], col="grey")
  }
  
  #Get the estimated values
  pre=NULL
  
  for (i in 1:(max(Age)+2)){
    temp=parse(text=paste("'estimated[",i,"]'",sep = ""))
    
    temp2<-individual[which(individual[,1]==eval(temp)),c(4,6,8)]
    pre=rbind(pre,temp2)
  }
  
  # Median estimations of the parameters
    
    lines(seq(1,maxAge+2),pre[,2],type="l",col="blue",lwd=2) 
  
  # Credibility intervals of the estimations
    
    lines(seq(1,maxAge+2),pre[,1],col="red",lty=3)
    lines(seq(1,maxAge+2),pre[,3],col="red",lty=3)

}


