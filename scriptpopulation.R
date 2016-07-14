f1 <- function(data, iterations, linfrange){
  
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
  # Section 2 Bayesian estimation for the population von Bertalanffy 
  # model (age-at-capture, one point per fish)
  # Section 3 Bayesian estimation for the individual von Bertalanffy 
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
  # POPULATION MODEL 1 observation per fish
  #-------------
  
  # Manual load of the data of interest
   
  #data<-read.csv(file = "www/DannularisPalma.csv",sep=";")
  
  
  # Getting one measurement per fish (the radius of the otolith
  # or in case of lengths the size of the fish at capture)
  Radius=NULL
  Age=NULL
  
  for (i in 1: length(unique(data$fishID))){
    temp<-max(data$size[which(data$fishID==unique(data$fishID)[i])])
    Radius=c(Radius,temp)
    temp2<-max(data$age[which(data$fishID==unique(data$fishID)[i])])
    Age=c(Age,temp2)
  }
  fishID<-unique(data$fishID)
  
  
  data<-cbind.data.frame(fishID,Radius,Age)
  
  
  
  #-------------
  #STEP 1: INITIALS
  #-------------
  # linfrange is an input from the shiny app, it can be changed manually here
  #linfrange<-c(100,300)
  
  ## These are the initial values of the bayesian model
  inits <- function() list(
    Linf=runif(1,linfrange[1],linfrange[2]),
    k=runif(1,0.15,0.25),
    T0=runif(1,-2,2),
    tau=1
  )
  
  #-------------
  #STEP 2:DATA
  #-------------
  
  # Prepare the data for JAGS
  
  jags.data <- list(
    nobs=length(data$Age),
    age=data$Age,
    size=data$Radius,
    maxAge=max(data$Age)+2,
    Linfmin=linfrange[1],
    Linfmax=linfrange[2]
  )
  
  #-------------
  #STEP 3: POBATIONAL BAYESIAN MODEL
  #-------------
  
  sink("populationmodel.txt")
  cat("
      model {
      for( i in 1 : nobs ) {
        mu[i]<-Linf*(1-exp(-k*(age[i]-T0))) #this is the von Bertalanffy model
        size[i] ~ dnorm(mu[i],tau)
      }
      
      #priors
        T0~dunif(-2,2)
        k~dunif(0,1)
        Linf~dunif(Linfmin,Linfmax)
  
      #prior variances
        
        tau ~ dgamma(0.001,0.001)
        sd<-1/sqrt(tau)
      
      #estimating size at age
      
      for(i in 1 : maxAge) {
        estimated[i]<-Linf*(1-exp(-k*(i-(T0))))
      }
      
   }
      ",fill = TRUE)
  sink()
  
  #-------------
  #STEP 4: RUNNING THE MODEL AND UPDATING
  #-------------
  
  # Parameters monitored
  
    params =c("T0","Linf","k","sd","estimated")
  
  # MCMC settings (Markov Chain Monte Carlo)
    nt <- 5 # Number of thining
    ni <- 10000 # Number of iterations
    nb <- 100 # Number of initial burning period
    nc <- 3 # Number of Markov Chains
  
  # Launching JAGS with the poblational model
    
    out <- jags(jags.data, inits, params, "populationmodel.txt", 
              n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
  
  # When running this in R you want to see the traceplots
  # that allow to see if the 3 Markov Chains converge or not.
  
  # traceplot(out) #Keep hitting enter or escape to stop showing the plots
  
  # If the The MCMC have not converged we update the model with more iterations
  #iterations=1000 # this parameter is given by the shiny app, it can be manually changed here
  
    out <- update(out, n.iter=iterations,n.thin=100)## WARNING TIME CONSUMING
  
  # Ideally for assuring convergence in the estimation of the parameters
  # we need bigger number of iterations (for example see below)
  
  #out <- update(out, n.iter=500000,n.thin=100)## WARNING TIME CONSUMING
  
  # Another indicator of convergence is the Rhat that will be close to 1 at convergence
  
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

f3 <- function(pobparams,data){
  
  Radius=NULL
  Age=NULL
  for (i in 1: length(unique(data$fishID))){
    temp<-max(data$size[which(data$fishID==unique(data$fishID)[i])])
    Radius=c(Radius,temp)
    temp2<-max(data$age[which(data$fishID==unique(data$fishID)[i])])
    Age=c(Age,temp2)
  }
  fishID<-unique(data$fishID)
  
  # This is just to make easier the plotting
  data<-cbind.data.frame(fishID,Radius,Age)
  attach(data)

  # The plot is made out of the shiny app output but in case
  # you are using this manually you will need to get "est" from the
  # bugsoutput (See line below)
    
    #est=results
    est=pobparams 


    plot(Age,Radius,type="n", pch= 1, cex=0.5, main="Von Bertalanffy Population Model",
      ylab= "Size (units of size)", xlab="Age (years)", 
      xlim=c(1,max(Age)+2), ylim=c(0, max(Radius)),las=1, col="grey")

  points(Age, Radius, col="grey", cex=0.5)
  pre=NULL
 
  for (i in 1:(max(Age)+2)){
    temp=parse(text=paste("'estimated[",i,"]'",sep = ""))
    
    temp2<-est[which(est[,1]==eval(temp)),c(4,6,8)]
    pre=rbind(pre,temp2)
  }
  
  
  # Median estimations of the parameters
    
    lines(seq(1,max(Age)+2),pre[,2],type="l",col="blue",lwd=2) 
  
  # Credibility intervals of the estimations
  
    lines(seq(1,max(Age)+2),pre[,1],col="red",lty=3)
    lines(seq(1,max(Age)+2),pre[,3],col="red",lty=3)

}



