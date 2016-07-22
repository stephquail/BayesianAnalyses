# (Script adapted from Jags-Ydich-Xnom1subj-MbernBetaModelComp.R) 
# Accompanies the book:
#   Kruschke, J. K. (2014). Doing Bayesian Data Analysis: 
#   A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier.
graphics.off()
rm(list=ls(all=TRUE))
source("R/DBDA2E-utilities.R") # loads Krushcke's functions. 
require(rjags)
fileNameRoot="Jags-ModelComp-" # for output filenames

#------------------------------------------------------------------------------
# THE DATA.

## EXPERIMENT 1 DATA

exp1data <- read.csv("R/data/group_summ.csv")

# BIAS SCORES
# Calculate bias scores from the average same and different responses for each responses 
# For Excitors: proportion of SAME responses 
# For Inhibitors: proportion of DIFFERENT responses
exp1ePITbias <- exp1data$exc.same/(exp1data$exc.same + exp1data$exc.diff)
exp1iPITbias <- exp1data$in.diff/(exp1data$in.diff + exp1data$in.same)
exp1iPITbias <- as.numeric(na.omit(exp1iPITbias)) #remove participants who did not make any responses (and therefore have a NA bias score) from analysis


## EXPERIMENT 2 DATA

exp2data <- read.csv("R/data/learner_summ.csv")


#Split participants into Specific Learner and General Learner groups. 
exp2specific <- exp2data[exp2data$learner == "specific",]
exp2general <- exp2data[exp2data$learner == "general",]

#Group Data Analysis

# BIAS SCORES
# Calculate bias scores from the average same and different responses for each responses 
# For Excitors: proportion of SAME responses 
# For Inhibitors: proportion of DIFFERENT responses

exp2ePITbias <- exp2data$exc.same/(exp2data$exc.same + exp2data$exc.diff)
exp2iPITbias <- exp2data$in.diff/(exp2data$in.diff + exp2data$in.same)
exp2iPITbias <- as.numeric(na.omit(exp2iPITbias)) #remove participants who did not make any responses (and therefore have a NA bias score) from analysis

# Specific Learners Bias Scores

sp_exp2ePITbias <- exp2specific$exc.same/(exp2specific$exc.same + exp2specific$exc.diff)
sp_exp2iPITbias <- exp2specific$in.diff/(exp2specific$in.diff + exp2specific$in.same)
sp_exp2iPITbias <- as.numeric(na.omit(sp_exp2iPITbias)) #remove participants who did not make any responses (and therefore have a NA bias score) from analysis

# General Learners Bias Scores

g_exp2ePITbias <- exp2general$exc.same/(exp2general$exc.same + exp2general$exc.diff)
g_exp2iPITbias <- exp2general$in.diff/(exp2general$in.diff + exp2general$in.same)


## SETTING UP EACH DIFFERENT DATA POINT FOR SENDING INTO JAGGS
## y = whatever data we want to analyse
## N = number of data points in our vector of our data
## text = label of the data. Used in the graph title outputs. 

# y = exp1ePITbias
# N = length(exp1ePITbias)
# text = "exp1ePITbias"
# 
# y = exp1iPITbias
# N = length(exp1iPITbias)
# text = "exp1iPITbias"
# 
# y = exp2ePITbias
# N = length(exp2ePITbias)
# text = "exp2ePITbias"
# 
# y = exp2iPITbias
# N = length(exp2iPITbias)
# text = "exp2iPITbias"
# 
# y = sp_exp2ePITbias
# N = length(sp_exp2ePITbias)
# text = "sp_exp2ePITbias"
# 
# y = sp_exp2iPITbias
# N = length(sp_exp2iPITbias)
# text = "sp_exp2iPITbias"
# 
# y = g_exp2ePITbias
# N = length(g_exp2ePITbias)
# text = "g_exp2ePITbias"
# 
# y = g_exp2iPITbias
# N = length(g_exp2iPITbias)
# text = "g_exp2iPITbias"


# For saving the priors use the 
# text = "priors"



exp1ePIT <- list(
  y = exp1ePITbias,
  N = length(exp1ePITbias),
  text = "exp1ePITbias"  
  )

exp1iPIT <- list(
  y = exp1iPITbias,
  N = length(exp1iPITbias),
  text = "exp1iPITbias")

exp2ePIT <- list(
  y = exp2ePITbias,
  N = length(exp2ePITbias),
  text = "exp2ePITbias")

exp2iPIT <- list(
  y = exp2iPITbias,
  N = length(exp2iPITbias),
  text = "exp2iPITbias")

sp_exp2ePIT <- list(
  y = sp_exp2ePITbias,
  N = length(sp_exp2ePITbias),
  text = "sp_exp2ePITbias")

sp_exp2iPIT <- list(
  y = sp_exp2iPITbias,
  N = length(sp_exp2iPITbias),
  text = "sp_exp2iPITbias")

g_exp2ePIT <- list(
  y = g_exp2ePITbias,
  N = length(g_exp2ePITbias),
  text = "g_exp2ePITbias")

g_exp2iPIT <- list(
  y = g_exp2iPITbias,
  N = length(g_exp2iPITbias),
  text = "g_exp2iPITbias")

dataInputs <- list(
  exp1ePIT,
  exp1iPIT,
  exp2ePIT,
  exp2iPIT,
  sp_exp2ePIT,
  sp_exp2iPIT,
  g_exp2ePIT,
  g_exp2iPIT)

# Get the data into a list to send to JAGS
# To sample from the priors, comment out the y = y line (but leave the N= N line intact)


# Loops through the Bayesian Model comparison for each data group. 
for(i in 1:length(dataInputs)){
  dataList = list(
    y = dataInputs[[i]]$y ,
    N = dataInputs[[i]]$N
  )
  
  #------------------------------------------------------------------------------
  # THE MODEL.
  
  modelString = "
model {
for ( i in 1:N ) {
y[i] ~ dnorm( mu, lambda )
}

#Priors for the data
mu ~ dnorm(omega[m], 100) # data is distributed normally. mu of prior differs between models. 
sigma ~ dunif(0,1) # both models have uninformative uniform priors on the sd
lambda <- 1/pow(sigma,2)

#Model priors
omega[1] <- .5 #uses a prior mu of .5 (i.e. NO BIAS)
omega[2] <- .75 #uses a prior mu of .75 (i.e. BIAS)
m ~ dcat( mPriorProb[] )
mPriorProb[1] <- .5 # sets the prior probabilites for each model  at .5 (i.e. no differences in the prior weights on each model)
mPriorProb[2] <- .5
}
" # close quote for modelString
  writeLines( modelString , con="TEMPmodel.txt" )
  
  #------------------------------------------------------------------------------
  
  # INTIALIZE THE CHAINS.
  
  # Specific initialization is not necessary in this case, 
  # but here is a lazy version if wanted:
  initsList = list( mu=0.5 , m=1 ) 
  
  #------------------------------------------------------------------------------
  
  # RUN THE CHAINS.
  
  parameters = c("mu","m", "sigma") 
  adaptSteps = 1000             # Number of steps to "tune" the samplers.
  burnInSteps = 1000           # Number of steps to "burn-in" the samplers.
  nChains = 4                   # Number of chains to run.
  numSavedSteps=50000          # Total number of steps in chains to save.
  thinSteps=1                   # Number of steps to "thin" (1=keep every step).
  nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  # Create, initialize, and adapt the model:
  jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , inits=initsList , 
                          n.chains=nChains , n.adapt=adaptSteps )
  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )
  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                              n.iter=nPerChain , thin=thinSteps )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  
  save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )
  
  #------------------------------------------------------------------------------- 
  # Display diagnostics of chain:
  
  parameterNames = varnames(codaSamples) # get all parameter names
  for ( parName in parameterNames ) {
    diagMCMC( codaSamples , parName=parName ,
              saveName=paste0("R/output/figures/", fileNameRoot, dataInputs[[i]]$text), saveType="png" )
  }
  
  #------------------------------------------------------------------------------
  # EXAMINE THE RESULTS.
  
  # Convert coda-object codaSamples to matrix object for easier handling.
  mcmcMat = as.matrix( codaSamples , chains=TRUE )
  m = mcmcMat[,"m"]
  mu = mcmcMat[,"mu"]
  
  # Compute the proportion of m at each index value:
  pM1 = sum( m == 1 ) / length( m )
  pM2 = 1 - pM1
  
  # Extract theta values for each model index:
  muM1 = mu[ m == 1 ]
  muM2 = mu[ m == 2 ]
  
  # Plot histograms of sampled theta values for each model,
  # with pM displayed.
  openGraph(width=8,height=5)
  par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
  layout( matrix(c(1,1,2,3),nrow=2,byrow=FALSE) , widths=c(1,2) )
  plotPost( m , breaks=seq(0.9,2.1,0.2) , cenTend="mean" , xlab="m" , main="Model Index" )
  plotPost( muM1 , 
            main=bquote( mu*" when m=NO BIAS" * " ; p(m=NO BIAS|D)" == .(signif(pM1,3)) ) , 
            cex.main=1.75 , xlab=bquote(mu) , xlim=c(0,1) )
  plotPost( muM2 , 
            main=bquote( mu*" when m=BIAS" * " ; p(m=BIAS|D)" == .(signif(pM2,3)) ) , 
            cex.main=1.75 , xlab=bquote(mu) , xlim=c(0,1) )
  saveGraph( file=paste0("R/output/figures/", fileNameRoot, dataInputs[[i]]$text,"Post") , type="png" )
  
  #------------------------------------------------------------------------------
  
  graphics.off()
  
}


# Sample from the Priors to demonstrate the different models. 

priors <- list(
  N = 20)

priorsInputs <- list(
  priors)


for(i in 1:length(priorsInputs)){
  dataList = list(
#     y = dataInputs[[i]]$y ,
    N = priorsInputs[[i]]$N
  )
  
  #------------------------------------------------------------------------------
  # THE MODEL.
  
  modelString = "
  model {
  for ( i in 1:N ) {
  y[i] ~ dnorm( mu, lambda )
  }
  
  #Priors for the data
  mu ~ dnorm(omega[m], 100) # data is distributed normally. mu of prior differs between models. 
  sigma ~ dunif(0,1) # both models have uninformative uniform priors on the sd
  lambda <- 1/pow(sigma,2)
  
  #Model priors
  omega[1] <- .5 #uses a prior mu of .5 (i.e. NO BIAS)
  omega[2] <- .75 #uses a prior mu of .75 (i.e. BIAS)
  m ~ dcat( mPriorProb[] )
  mPriorProb[1] <- .5 # sets the prior probabilites for each model  at .5 (i.e. no differences in the prior weights on each model)
  mPriorProb[2] <- .5
  }
  " # close quote for modelString
  writeLines( modelString , con="TEMPmodel.txt" )
  
  #------------------------------------------------------------------------------
  
  # INTIALIZE THE CHAINS.
  
  # Specific initialization is not necessary in this case, 
  # but here is a lazy version if wanted:
  initsList = list( mu=0.5 , m=1 ) 
  
  #------------------------------------------------------------------------------
  
  # RUN THE CHAINS.
  
  parameters = c("mu","m", "sigma") 
  adaptSteps = 1000             # Number of steps to "tune" the samplers.
  burnInSteps = 1000           # Number of steps to "burn-in" the samplers.
  nChains = 4                   # Number of chains to run.
  numSavedSteps=50000          # Total number of steps in chains to save.
  thinSteps=1                   # Number of steps to "thin" (1=keep every step).
  nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  # Create, initialize, and adapt the model:
  jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , inits=initsList , 
                          n.chains=nChains , n.adapt=adaptSteps )
  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )
  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                              n.iter=nPerChain , thin=thinSteps )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  
  save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )
  
  #------------------------------------------------------------------------------- 
  # Display diagnostics of chain:
  
  parameterNames = varnames(codaSamples) # get all parameter names
  for ( parName in parameterNames ) {
    diagMCMC( codaSamples , parName=parName ,
              saveName=paste0("R/output/figures/", fileNameRoot, "priors") , saveType="png" )
  }
  
  #------------------------------------------------------------------------------
  # EXAMINE THE RESULTS.
  
  # Convert coda-object codaSamples to matrix object for easier handling.
  mcmcMat = as.matrix( codaSamples , chains=TRUE )
  m = mcmcMat[,"m"]
  mu = mcmcMat[,"mu"]
  
  # Compute the proportion of m at each index value:
  pM1 = sum( m == 1 ) / length( m )
  pM2 = 1 - pM1
  
  # Extract theta values for each model index:
  muM1 = mu[ m == 1 ]
  muM2 = mu[ m == 2 ]
  
  # Plot histograms of sampled theta values for each model,
  # with pM displayed.
  openGraph(width=8,height=5)
  par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
  layout( matrix(c(1,1,2,3),nrow=2,byrow=FALSE) , widths=c(1,2) )
  plotPost( m , breaks=seq(0.9,2.1,0.2) , cenTend="mean" , xlab="m" , main="Model Index" )
  plotPost( muM1 , 
            main=bquote( mu*" when m=NO BIAS" * " ; p(m=NO BIAS|D)" == .(signif(pM1,3)) ) , 
            cex.main=1.75 , xlab=bquote(mu) , xlim=c(0,1) )
  plotPost( muM2 , 
            main=bquote( mu*" when m=BIAS" * " ; p(m=BIAS|D)" == .(signif(pM2,3)) ) , 
            cex.main=1.75 , xlab=bquote(mu) , xlim=c(0,1) )
  saveGraph( file=paste0("R/output/figures/", fileNameRoot, "priors","Post") , type="png" )
  
  #------------------------------------------------------------------------------
  
  graphics.off()
  
}
