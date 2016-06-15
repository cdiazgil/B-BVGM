f1 <- function(data){
#------------------------------------------------
# Fishery-dependent sampling may bias growth estimates

# Supplementary material
# Marine and Freshwater Research

# Bernat Morro, Miguel Palmer, Carlos D?az-Gil, Josep Al?s, 
# Rosario Rossell?, Amalia Grau, Inmaculada Riera-Batle, 
# Beatriz Morales-Nin 
#
# Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)
# Laboratory of Marine Research and Aquaculture (LIMIA)

#------------------------------------------------
# General objective: 

# Objective: Comparing growth models at population and individual 
# level (fish of diffent age) 
# Last update: Feb 12, 2015 
# 
#
# Section 1 Load the data from 296 individuals of Scorpaena porcus 
# (age, lenght of the otolith and lenght between marks deposited 
# in the otoliths annually)
# Section 2 Bayesian estimation for the population von Bertalanffy 
# model (age-at-capture, one point per fish)
# Section 3 Bayesian estimation for the individual von Bertalanffy 
# model (back calculated ages)
#
#
#------------------------------------------------
# Section 1: Loading libraries and data
#------------------------------------------------
#remove(list=ls())
# Required libraries
# It is necessary to download JAGS (Just Another Gibbs Sampler)
# http://mcmc-jags.sourceforge.net/
# And the R library to interact with it for the Bayesian approach
library(R2jags) 

#Load the file rDataScorpaenaporcus

# As extra information the data has been obtained from fish 
# obtained from commercial landings, measured and otoliths 
# extracted, afterwards the otoliths were photographed and 
# the position of the core, and each annual deposit mark 
# recorded using the TPSdig2 software (version 2.17. Available at 
# http://life.bio.sunysb.edu/ee/rohlf/software.html (Rohlf 2007). 
# Alternatively ImageJ software can be used to obtain x,y 
# coordinates of the points marked. In this case we directly 
# provide with the necessary data for running both models

# The code can be run line by line or all together at once. 

#-------------
# Section 2: BAYESIAN ESTIMATION OF VON BERTALANFY PARAMETERS 
# POPULATION MODEL 1 observation per fish
#-------------
  ###   EL LOAD VA AQUI SI NO FUNCIONA!!!!!!! metadata <- read.csv("data.csv",sep = ";")
#data<-read.csv(file = "data.csv",sep=";")
names(data)

Radius=NULL
Age=NULL
i=1
for (i in 1: length(unique(data$fishID))){
  temp<-max(data$size[which(data$fishID==unique(data$fishID)[i])])
  Radius=c(Radius,temp)
  temp2<-max(data$age[which(data$fishID==unique(data$fishID)[i])])
  Age=c(Age,temp2)
}
fishID<-unique(data$fishID)


data2<-cbind.data.frame(fishID,Radius,Age)



#-------------
#STEP 1: INITIALS
#-------------

inits <- function() list(
  Linf=runif(1,1,10),
  k=runif(1,0.15,0.25),
  T0=runif(1,-1,-0.5),
  tau=1
)
#-------------

#-------------
#STEP 2:DATA
#-------------

# Prepare the data for JAGS
win.data <- list(
  nobs=length(data2$Age),
  age=data2$Age,
  size=data2$Radius
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
    Linf~dunif(0,300)
    #prior variances
    tau ~ dgamma(0.001,0.001)
    sd<-1/sqrt(tau)
    
    #estimating size at age
    for(i in 1 : 15) {
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
nt <- 5 #Number of thining
ni <- 10000 #Number of iterations
nb <- 100 #Number of initial burning period
nc <- 3 #Number of Markov Chains

# Launching JAGS with the poblational model
out <- jags(win.data, inits, params, "populationmodel.txt", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# The traceplots allow to see if the 3 Markov chains converge or not.
# traceplot(out) #Keep hitting enter or escape to stop showing the plots

# The MC have not converged so we update the model with more iterations
out <- update(out, n.iter=5000,n.thin=100)## WARNING TIME CONSUMING

# Ideally for assuring convergence in the estimation of the parameters
# we need bigger number of iterations (for example see below)

#out <- update(out, n.iter=500000,n.thin=100)## WARNING TIME CONSUMING



#-------------
#STEP 5: RESULTS VISUALIZATION 
#-------------

# To show the results and the estimates of the von Bertalanffy parameters
print(out)
# Or
out$BUGSoutput$summary

#out$BUGSoutput$summary[c(1:2,19),]
}

f3 <- function(pobparams,data){
  
  Radius=NULL
  Age=NULL
  i=1
  for (i in 1: length(unique(data$fishID))){
    temp<-max(data$size[which(data$fishID==unique(data$fishID)[i])])
    Radius=c(Radius,temp)
    temp2<-max(data$age[which(data$fishID==unique(data$fishID)[i])])
    Age=c(Age,temp2)
  }
  fishID<-unique(data$fishID)
  
  
  data2<-cbind.data.frame(fishID,Radius,Age)
  attach(data2)
# A plot to see the results
 #X11(height=5.9,width=6.9, pointsize=12) #Just for the right size

#est=out$BUGSoutput$summary[4:11,]
est=pobparams[4:11,]
  plot(Age,Radius, pch= 1, cex=0.5, main="Von Bertalanffy Population Model",
     ylab= "Size (units of size)", xlab="Age (years)", xlim=c(1,max(Age)), 
     ylim=c(0, max(Radius)),las=1)
# Median estimations of the parameters
lines(seq(1,max(Age)),est[1:max(Age),5],type="l",col="blue",lwd=2) 
# Credibility intervals of the estimations
lines(seq(1,max(Age)),est[1:max(Age),3],col="red",lty=3)
lines(seq(1,max(Age)),est[1:max(Age),7],col="red",lty=3)
}
