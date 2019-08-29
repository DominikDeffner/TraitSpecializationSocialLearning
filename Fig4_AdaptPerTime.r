
# Code for Fig.4, Proportion of simulations that reach a given adaptation threshold
# conditional on the time since the last change in the environmen


seq<-expand.grid(Nsim=1000, N=100, Tmax=300, e=c(0.001),Mxi=0.1, Mm=0, m=c(0,0.8),SigmaModMu=0.1,SigmaModSigma=0.01,
                 Nlearn = 1, NumberModels = 3, Combination= c("A","B","C", "D","E"), EnvChange= c("Random"),SigmaOn=c("Yes"), SD_Copy_error = c(0, 0.01, 0.05))

library(fields)

result <- resultFixedComparison

#"A": Mild Specialists (0,0.05)

#"B": Hyperspecialists (0,0.02)

#"C": Mild Generalists (0.05, 0.2)

#"D": HyperGeneralists (0.1, 0.2)

#"E": Flexible (0, 0.2)


Timesteps <- 300
Error <- 0
mod <- 0

for (x in 1:nrow(seq)) {
  result[[x]][["ProportionMatrix"]] <- matrix(nrow = Timesteps, ncol = length(seq(from=0.005, to=0.50, by= 0.005)))
  for (i in 1:Timesteps) {
    for (z in seq(from=0.005, to=0.50, by= 0.005)) {
      result[[x]][["ProportionMatrix"]][i,which(seq(from=0.005, to=0.50, by= 0.005)==z)] <- sum(result[[x]][["Adaptation"]][,i]>=z)/1000
    }
  }
}
colors <- colorRampPalette(c("black","white"))


mod=0
Error=0
    par(mfrow= c(1,3),
        oma=c(1,1,0,0)+2,
        mar=c(2,2,2,1))
    image(1:Timesteps, seq(from=0.005, to=0.50, by= 0.005), result[[which(seq$m==mod & seq$SD_Copy_error==Error & seq$Combination=="D")]][["ProportionMatrix"]],xlab="Timestep",ylab="", col = colors(20), main= expression(paste("Generalists, ", sigma %in% "[0.1,0.2]")))
    
    image(1:Timesteps, seq(from=0.005, to=0.50, by= 0.005), result[[which(seq$m==mod & seq$SD_Copy_error==Error & seq$Combination=="E")]][["ProportionMatrix"]],xlab="Timestep",ylab="", col = colors(20), main= expression(paste("Flexible, ", sigma %in% "(0,0.2]")))
    
    image.plot(1:Timesteps, seq(from=0.005, to=0.50, by= 0.005), result[[which(seq$m==mod & seq$SD_Copy_error==Error & seq$Combination=="B")]][["ProportionMatrix"]],legend.width=3, 
               xlab="Timestep",ylab="",col = colors(20),main= expression(paste("Specialists, ", sigma %in% "(0,0.02]")))
    mtext(side = 1, line = 1.3, "Timestep", outer = TRUE)
    mtext(side = 2, line = 1.3, "Adaptation Threshold", outer = TRUE)
    
    dev.off()
