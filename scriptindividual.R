
f2 <- function(data){
# Now is time for the Individual models
  library(R2jags) 
  
#-------------
# Section 3: BAYESIAN ESTIMATION OF VON BERTALANFY PARAMETERS
# INDIVIDUAL MODEL Several observations per fish
#
#-------------
#STEP 1: INITIALS
#-------------
#getwd()
#data<-read.csv(file = "example.csv",sep=";")

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
inits <- function() list(
  Linf=runif(nfish,2,10),
  k=runif(nfish,0,1),
  T0=runif(nfish,-1,1),
  tau=1,
  tauk=1,
  tauLinf=1,
  tauT0=1,
  k.beta=0,
  Linf.beta=0
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
  AGE=Age,
  MaxAge=max(Age)
)

#-------------
#STEP 3: INDIVIDUAL BAYESIAN MODEL
#-------------

sink("individualmodel.txt")
cat("
    model {
    for( i in 1 : nobs ) {
    mu[i]<-Linf[ID[i]]*(1-exp(-k[ID[i]]*(age[i]-T0[ID[i]])))
    size[i] ~ dnorm(mu[i],tau)
    }
    #priors individual fish 
    for( i in 1 : nfish ) {
    T0[i]~dnorm(T0.alpha,tauT0)
    Linf[i]~dnorm(Linf.exp[i],tauLinf)T(2,10)
    Linf.exp[i]<-Linf.alpha+Linf.beta*AGE[i]
    k[i]~dnorm(k.exp[i],tauk)T(0,1)
    k.exp[i]<-k.alpha+k.beta*AGE[i]
    }
    T0.alpha~dnorm(0,0.000001)
    Linf.alpha~dnorm(0,0.000001)
    k.alpha~dnorm(0,0.000001)
    Linf.beta~dnorm(0,0.000001)T(-1,1)
    k.beta~dnorm(0,0.000001)T(-1,1)
    
    #prior variances
    tau ~ dgamma(0.001,0.001)
    tauT0 ~ dgamma(0.001,0.001)
    tauk ~ dgamma(0.001,0.001)
    tauLinf ~ dgamma(0.001,0.001)
    
    #estimating size at age for each grup
    for(i in 1 : MaxAge) {
    estimated[i,1]<-0
    }
    for(j in 2 : MaxAge){
    Linf.hat[j]<-Linf.alpha+Linf.beta*j
    k.hat[j]<-k.alpha+k.beta*j
    T0.hat[j]<-T0.alpha#+T0.beta*j
    for(i in 1 : MaxAge) {
    estimated[i,j]<-(Linf.hat[j])*(1-exp(-(k.hat[j])*(i-(T0.hat[j]))))
    }
    }
    }
    ",fill = TRUE)
sink()

#-------------
#STEP 4: RUNNING THE MODEL AND UPDATING
#-------------

# Parameters monitored
params =c("T0","Linf","k","Linf.alpha","Linf.beta","k.alpha","k.beta",
          "T0.alpha","estimated")

# MCMC settings
nt <- 5
ni <- 1000
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
pre.time <- Sys.time()
out <- update(out, n.iter=5000,n.thin=10)
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
  
  pepe=indparams
 #pepe= out$BUGSoutput$summary
  
 #data<-read.csv(file = "data.csv",sep=";")
  
  #maximum age of each fish== later will be AGE
  Age=NULL
  for (i in 1: length(unique(data$fishID))){
    temp<-max(data$age[which(data$fishID==unique(data$fishID)[i])])
    Age=c(Age,temp)
  }
  
  
  nfish=length(unique(data$fishID))
  
  # Prepare the data for JAGS
    age=data$age
    size=data$size
    F=data$ID
    AGE=Age
  max(Age)
  
 # windows(height=5.9,width=6.9, pointsize=12) #Just for the right size
  x=10#grey scale
  ## Execute all below to see the plots
  par(mfrow=c(round(max(Age)/2),2), mar=c(4,4,2,3))
  seq=seq(1,max(Age)+2)
  
  for (j in 2:max(Age)){
    plot(age,size,type="n",xlab="", ylab="", 
         xlim= c(min(seq),max(seq)), ylim= c(0,5), las=1)
    pre=NULL
   
    for (i in 1:max(Age)){
      temp2=parse(text=paste("pepe['estimated[",i,",",j,"]',
                             c(3,5,7)]",sep = ""))
      pre=rbind(pre,eval(temp2))
    }
    polygon(c(seq(1,max(Age)),rev(seq(1,max(Age)))),c(pre[,1],rev(pre[,3])),col=gray(7/x),border=gray(7/x))
    lines(seq(1,max(Age)),pre[,2],lwd=2, col="blue")
    mtext("Von Bertalanffy Individual Model", side=3, line=0.5)
    mtext("Age (years)", side=1, line=2,cex=1)
    mtext("Size (units of size)", side=2, line=2,cex=1)
    text(7,1, paste(j,"Year class"), cex=1.2)
    for(i in 1:nfish){
      if(AGE[i]==j){
        temp=which(F==i)
        lines(age[temp],size[temp])
      }
    }  
  }
  
  }
