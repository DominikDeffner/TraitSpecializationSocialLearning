
seq<-expand.grid(Nsim=1000, N=100, Tmax=1000, e=c(0.001, 0.01),Mxi=0.1, Mm=0, m=0, SigmaModMu=0.1,SigmaModSigma=0.01,
                 Nlearn = c(1,10), NumberModels = 3, UpperBoundSigma=0.2, EnvChange= c("Random","Gradual0.05"),SigmaOn= "Yes", SD_Copy_error=0)

#Define Gaussian adaptation function of environmental state e at time t, e(t), with parameters mu, sigma and amax
Gaussian<-function(E,mu,sigma,amax){amax*exp(-((E-mu)^2)/(2*sigma^2))}

#Define and plot function relating amax to sigma
amax.funct<- function(sigma){exp(-b*sigma)}
b<-10

simfunct <- function(Nsim,N, Tmax, e, Mxi, Mm,m, SigmaModMu, SigmaModSigma,Nlearn, NumberModels, UpperBoundSigma,EnvChange, SigmaOn, SD_Copy_error){
  
  Adapt_matrix      <- matrix(nrow = Nsim, ncol = 1.5/e)
  Sigma_matrix      <- matrix(nrow = Nsim, ncol = 1.5/e)
  Xi_matrix         <- matrix(nrow = Nsim, ncol = 1.5/e)
  m_matrix          <- matrix(nrow = Nsim, ncol = 1.5/e)
  
  for (xsim in 1:Nsim) {
    
    #Randomly select starting environmental condition
    E<- runif(1,-1,1)
    
    #Create ID vector for newborns
    ID_vector <- N+1:1e6
    
    #Set up population matrix
    Pop<-data.frame(id=1:N,mu=NA,sigma=NA, xi=NA, m=NA, Fitness=NA, RelFitness=NA, Strategy=NA)
    
    #Initialize population
    Pop$mu<- runif(1,-1,1)
    
    if (SigmaOn =="Yes"){
      Pop$sigma<- runif(length(Pop$mu),0,0.2)
    }
    if (SigmaOn =="No0.01"){
      Pop$sigma<- 0.01    
    }
    if (SigmaOn =="No0.05"){
      Pop$sigma<- 0.05     #runif(length(Pop$mu),0,0.2)
    }
    
    Pop$xi<- runif(length(Pop$xi),0,1)
    Pop$m<- m
    
    AddTime <- rgeom(1,e)+1
    StableTime <- 1.5/e
    
    
    #Simulation loop
    for (i in 1:(Tmax + AddTime + StableTime)){
      
      #1. Learning
      #Select Nlearn individuals for learning
      LearnID <- sample(Pop$id, size = Nlearn, replace = FALSE)
      
      #Assign learning strategy based on xi
      Pop$Strategy <-NA
      
      for (x_ID in LearnID) {
        Pop$Strategy[which(Pop$id==x_ID)] <- sample(c("Individual", "Social"), size = 1, prob = c(Pop$xi[which(Pop$id==x_ID)],1-Pop$xi[which(Pop$id==x_ID)]))
      }
      
      
      #Individual learning
      for (x_ind in Pop$id[which(Pop$Strategy=="Individual")]){
        
        InnoStyle <- sample(c("Modification", "Innovation"), size = 1, prob = c(Pop$m[which(Pop$id==x_ind)], 1-Pop$m[which(Pop$id==x_ind)]))
        
        if(InnoStyle=="Innovation"){
          
          Pop$mu[which(Pop$id==x_ind)]<- runif(1,-1,1)
          
          if (SigmaOn == "Yes"){
            Pop$sigma[which(Pop$id==x_ind)] <- runif(1, 0, 0.2)
          }
        }
        
        if(InnoStyle=="Modification"){
          
          OldMu <- Pop$mu[which(Pop$id==x_ind)]
          Pop$mu[which(Pop$id==x_ind)]<- OldMu + rnorm(1, mean = 0, sd=SigmaModMu)
          
          while (Pop$mu[which(Pop$id==x_ind)] < -1 | Pop$mu[which(Pop$id==x_ind)] > 1){
            Pop$mu[which(Pop$id==x_ind)]<- OldMu + rnorm(1, mean = 0, sd=SigmaModMu)
          }
          
          if (SigmaOn == "Yes"){
            
            OldSigma <- Pop$sigma[which(Pop$id==x_ind)]
            Pop$sigma[which(Pop$id==x_ind)] <- OldSigma + rnorm(1, mean = 0, sd=SigmaModSigma)
            
            while (Pop$sigma[which(Pop$id==x_ind)] < 0 | Pop$sigma[which(Pop$id==x_ind)] > 0.2){
              Pop$sigma[which(Pop$id==x_ind)] <- OldSigma + rnorm(1, mean = 0, sd=SigmaModSigma)
            }
          }
        }
      }
      
      
      #Social learning  
      #Loop over all individuals with SL strategy in random order if there are more than one social learners
      
      if (length(Pop$id[which(Pop$Strategy=="Social")])>1){
        for (x_soc in sample(Pop$id[which(Pop$Strategy=="Social")])){
          
          #Determine adaptedness of ones own variant X
          AdaptX<- Gaussian(E=E, mu=Pop$mu[Pop$id==x_soc],sigma=Pop$sigma[Pop$id==x_soc], amax = amax.funct(Pop$sigma[Pop$id==x_soc]))
          
          #Sample N = Nmod random interaction partners
          Models <- subset(Pop, Pop$id %in% sample(Pop$id[-(which(Pop$id==x_soc))],size=NumberModels))
          
          #Determine adaptedness of variant of all interaction partners
          
          for (x_mod in Models$id){
            Models$Fitness[Models$id==x_mod]<-Gaussian(E=E, mu=Models$mu[Models$id==x_mod],sigma=Models$sigma[Models$id==x_mod], amax = amax.funct(Models$sigma[Models$id==x_mod]))
          }
          
          #Relativize fitness
          if(all(Models$Fitness==0)==FALSE){
            Models$RelFitness <- Models$Fitness / sum(Models$Fitness)
          } else {
            Models$RelFitness <- 1/N
          }
          
          #If learner has best variant, keep that, otherwise copy proportional to payoff
          if (AdaptX > max(Models$Fitness)) {
            Pop$mu[Pop$id==x_soc]<- Pop$mu[Pop$id==x_soc]
            Pop$sigma[Pop$id==x_soc]<- Pop$sigma[Pop$id==x_soc]
          } else {
            SelectedModel <- sample(Models$id, size = 1, prob = Models$RelFitness)
            Pop$mu[Pop$id==x_soc]<- Models$mu[Models$id==SelectedModel] + rnorm(1,0, SD_Copy_error)
            Pop$sigma[Pop$id==x_soc]<- Models$sigma[Models$id==SelectedModel]
          }
          
        }
      } else {
        for (x_soc in Pop$id[which(Pop$Strategy=="Social")]){
          
          #Determine adaptedness of ones own variant X
          AdaptX<- Gaussian(E=E, mu=Pop$mu[Pop$id==x_soc],sigma=Pop$sigma[Pop$id==x_soc], amax = amax.funct(Pop$sigma[Pop$id==x_soc]))
          
          #Sample N = Nmod random interaction partners
          Models <- subset(Pop, Pop$id %in% sample(Pop$id[-(which(Pop$id==x_soc))],size=NumberModels))
          
          #Determine adaptedness of variant of all interaction partners
          
          for (x_mod in Models$id){
            Models$Fitness[Models$id==x_mod]<-Gaussian(E=E, mu=Models$mu[Models$id==x_mod],sigma=Models$sigma[Models$id==x_mod], amax = amax.funct(Models$sigma[Models$id==x_mod]))
          }
          
          #Relativize fitness
          if(all(Models$Fitness==0)==FALSE){
            Models$RelFitness <- Models$Fitness / sum(Models$Fitness)
          } else {
            Models$RelFitness <- 1/N
          }
          
          #If learner has best variant, keep that, otherwise copy proportional to payoff
          if (AdaptX > max(Models$Fitness)) {
            Pop$mu[Pop$id==x_soc]<- Pop$mu[Pop$id==x_soc]
            Pop$sigma[Pop$id==x_soc]<- Pop$sigma[Pop$id==x_soc]
          } else {
            SelectedModel <- sample(Models$id, size = 1, prob = Models$RelFitness)
            Pop$mu[Pop$id==x_soc]<- Models$mu[Models$id==SelectedModel] + rnorm(1,0, SD_Copy_error)
            Pop$sigma[Pop$id==x_soc]<- Models$sigma[Models$id==SelectedModel]
          }
          
        } 
      }
      
      
      
      #Calculate fitness of each individual
      for (x in Pop$id){
        Pop$Fitness[Pop$id==x]<-Gaussian(E=E, mu=Pop$mu[Pop$id==x],sigma=Pop$sigma[Pop$id==x], amax = amax.funct(Pop$sigma[Pop$id==x]))
      }
      
      #Record stuff after i=Tmax time steps
      
      if(i>Tmax+AddTime){
        Adapt_matrix[xsim, i - (Tmax+AddTime)]   <- mean(Pop$Fitness, na.rm = TRUE)
        Sigma_matrix[xsim, i - (Tmax+AddTime)]   <- mean(Pop$sigma, na.rm = TRUE)
        Xi_matrix[xsim, i - (Tmax+AddTime)]      <- mean(Pop$xi, na.rm = TRUE)
        m_matrix[xsim, i - (Tmax+AddTime)]       <- mean(Pop$m, na.rm = TRUE)
      }
      
      
      #Relativize fitness
      if(all(Pop$Fitness==0)==FALSE){
        Pop$RelFitness <- Pop$Fitness / sum(Pop$Fitness)
      } else {
        Pop$RelFitness <- 1/N
      }
      
      
      #Moran birth-death process
      
      #Select one individual for reproduction
      ParentID <- sample(Pop$id, size = 1, prob = Pop$RelFitness)
      
      #Create offspring with new id and no fitness level
      
      Newborn <- Pop[which(Pop$id==ParentID),]
      
      #Sample new ID for newborn from ID vector and remove IDs from ID vector
      Newborn$id<-sample(ID_vector, size = 1)
      ID_vector <- ID_vector[! ID_vector %in% Newborn$id] 
      
      Newborn$Fitness<-NA
      Newborn$RelFitness<-NA
      
      #Mutation on xi/m with probability Mxi/Mm 
      Newborn$xi<- sample(c(Newborn$xi,runif(1,0,1)), size = 1, prob = c(1-Mxi, Mxi))
      Newborn$m<- sample(c(Newborn$m,runif(1,0,1)), size = 1, prob = c(1-Mm, Mm))
      
      
      # Vertical transmission or innovation
      
      #Determine if any innovation occurs for mu and sigma
      
      Innovation <- sample(c("Yes","No"), size = 1, prob = c(Newborn$xi, 1-Newborn$xi))
      
      
      #Innovation on mu and sigma simultaneously
      
      if (Innovation == "Yes"){
        
        Newborn$mu <- runif(1, -1, 1)
        if (SigmaOn == "Yes"){Newborn$sigma <- runif(1, 0, UpperBoundSigma)}
        
        
      } else {Newborn$mu <- Newborn$mu + rnorm(1,0, SD_Copy_error)
      Newborn$sigma <- Newborn$sigma}
      
      
      #Select 1 individual for death
      
      DeathId <- sample(Pop$id, size = 1)
      
      #Add new and remove dead individual
      Pop <- subset(Pop, Pop$id != DeathId)
      Pop<- rbind(Pop, Newborn)
      
      #Environmental change; if there is a change, environment switches to random state between -1 and 1 or 
      
      if (i <= Tmax){
        ProbEnvChange<-runif(1)
        
        if(ProbEnvChange<e){
          
          if(EnvChange=="Random"){
            E<- runif(1, -1, 1)
          }
          
          if(EnvChange=="Gradual0.05"){
            E0 <- E
            E <- E0 + rnorm(1, mean = 0, sd=0.05) 
            while (E < -1 | E > 1) {
              E <- E0 + rnorm(1, mean = 0, sd=0.05) 
            }
          }
          
          
          if(EnvChange=="Gradual0.2"){
            E0 <- E
            E <- E0 + rnorm(1, mean = 0, sd=0.2) 
            while (E < -1 | E > 1) {
              E <- E0 + rnorm(1, mean = 0, sd=0.2) 
            }
          }
        }
      }
      
      if (i == Tmax+AddTime){
        if(EnvChange=="Random"){
          E<- runif(1, -1, 1)
        }
        
        if(EnvChange=="Gradual0.05"){
          E0 <- E
          E <- E0 + rnorm(1, mean = 0, sd=0.05) 
          while (E < -1 | E > 1) {
            E <- E0 + rnorm(1, mean = 0, sd=0.05) 
          }
        }
        
        if(EnvChange=="Gradual0.2"){
          E0 <- E
          E <- E0 + rnorm(1, mean = 0, sd=0.2) 
          while (E < -1 | E > 1) {
            E <- E0 + rnorm(1, mean = 0, sd=0.2) 
          }
        }
      }
      
      if (i > Tmax+AddTime) E<- E
      
    }#end simulation loop
    
  }#end many simulation loop
  
  return(list(
    Adaptation = Adapt_matrix,
    Sigma      = Sigma_matrix,
    Xi         = Xi_matrix,
    m          = m_matrix))
}
library(parallel)

resultBaselineDynamics <- mclapply(
  1:nrow(seq) ,
  function(i) simfunct(seq$Nsim[i],seq$N[i], seq$Tmax[i], seq$e[i], seq$Mxi[i], 
                       seq$Mm[i],seq$m[i], seq$SigmaModMu[i], seq$SigmaModSigma[i],seq$Nlearn[i], 
                       seq$NumberModels[i], seq$UpperBoundSigma[i],seq$EnvChange[i],seq$SigmaOn[i], seq$SD_Copy_error[i]),
  mc.cores=12)

