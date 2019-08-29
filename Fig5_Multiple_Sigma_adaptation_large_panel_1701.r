
#Code for Fig. 5, Comparison of mean adaptation levels (and 90% PIs) of populations with specialist variants, 
#flexible variants generalist variants,comparison mofification and copying error

seq<-expand.grid(Nsim=1000, N=100, Tmax=300, e=c(0.001),Mxi=0.1, Mm=0, m=c(0,0.8),SigmaModMu=0.1,SigmaModSigma=0.01,
                 Nlearn = 1, NumberModels = 3, Combination= c("A","B","C", "D","E"), EnvChange= c("Random"),SigmaOn=c("Yes"), SD_Copy_error = c(0, 0.01, 0.05))

result <- resultFixedComparison

library(fields)

#"A": Mild Specialists (0,0.05)

#"B": Hyperspecialists (0,0.02)

#"C": Mild Generalists (0.05, 0.2)

#"D": HyperGeneralists (0.1, 0.2)

#"E": Flexible (0, 0.2)


#color stuff
require(RColorBrewer)#load package
display.brewer.all(colorblindFriendly = TRUE) #shows only colorblind friendly schemes, pick ione


x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Dark2") #create a pallette which you loop over for corresponding values


library(scales)

resultsMedian <- result

for ( i in 1:nrow(seq)){
  resultsMedian[[i]] <- apply(resultsMedian[[i]][["Adaptation"]], 2, function(x){return(sort(x)[500])})
  resultsMedian[[i]] <- resultsMedian[[i]][1:5000]
}

resultsUpper <- result

for ( i in 1:nrow(seq)){
  resultsUpper[[i]] <- apply(resultsUpper[[i]][["Adaptation"]], 2, function(x){return(sort(x)[950])})
  resultsUpper[[i]] <- resultsUpper[[i]][1:5000]
  
}

resultsLower <- result

for ( i in 1:nrow(seq)){
  resultsLower[[i]] <- apply(resultsLower[[i]][["Adaptation"]], 2, function(x){return(sort(x)[50])})
  resultsLower[[i]] <- resultsLower[[i]][1:5000]
  
}

par(mfrow= c(2,3),
    oma=c(3,3.8,0,0),
    mar=c(2,2.5,2,1))

  m=0
  Error=0
  #Specialist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[4], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]])), col=alpha(col.pal[4],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],type="l",col=col.pal[4],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  #Generalist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha("black", alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]])), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],type="l",col=col.pal[1],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  
  #Variable Sigma
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[2], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]])), col=alpha(col.pal[2],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],type="l",col=col.pal[2],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  title("Faithful Learning")
  legend("topright","A",bty = "n",cex = 1.2)
  
  
  m=0
  Error=0.01
  #Specialist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[4], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]])), col=alpha(col.pal[4],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],type="l",col=col.pal[4],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  #Generalist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha("black", alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]])), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],type="l",col=col.pal[1],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  
  #Variable Sigma
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[2], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]])), col=alpha(col.pal[2],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],type="l",col=col.pal[2],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  title("Small Copying Error")
  legend("topright","B",bty = "n",cex = 1.2)
  
  
  m=0
  Error=0.05
  #Specialist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[4], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]])), col=alpha(col.pal[4],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],type="l",col=col.pal[4],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  #Generalist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha("black", alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]])), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],type="l",col=col.pal[1],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  
  #Variable Sigma
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[2], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]])), col=alpha(col.pal[2],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],type="l",col=col.pal[2],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  title("Large Copying Error")
  legend("topright","C",bty = "n",cex = 1.2)
  
  
  m=0.8
  Error=0
  #Specialist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[4], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]])), col=alpha(col.pal[4],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],type="l",col=col.pal[4],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  #Generalist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha("black", alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]])), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],type="l",col=col.pal[1],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  
  #Variable Sigma
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[2], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]])), col=alpha(col.pal[2],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],type="l",col=col.pal[2],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  legend("topright","D",bty = "n",cex = 1.2)
  
  
  m=0.8
  Error=0.01
  #Specialist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[4], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]])), col=alpha(col.pal[4],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],type="l",col=col.pal[4],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  #Generalist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha("black", alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]])), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],type="l",col=col.pal[1],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  
  #Variable Sigma
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[2], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]])), col=alpha(col.pal[2],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],type="l",col=col.pal[2],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  legend("topright","E",bty = "n",cex = 1.2)
  
  m=0.8
  Error=0.05
  #Specialist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[4], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]])), col=alpha(col.pal[4],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="B")]],type="l",col=col.pal[4],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  #Generalist
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha("black", alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]])), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="D")]],type="l",col=col.pal[1],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  
  par(new=TRUE)
  
  #Variable Sigma
  plot(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]], type = "n", xlab = "", ylab = "", ylim = c(0,0.9),col=alpha(col.pal[2], alpha=0.2))
  polygon(c(1:5000,5000:1), c(resultsUpper[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],
                              rev(resultsLower[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]])), col=alpha(col.pal[2],alpha = 0.2), border = NA, ylim=c(0,1))
  par(new=TRUE)
  plot(resultsMedian[[which(seq$m==m & seq$SD_Copy_error==Error & seq$Combination=="E")]],type="l",col=col.pal[2],xlab = "", ylab = "",  ylim = c(0,0.9), lwd=3)
  legend("topleft",title = expression(sigma), c("(0, 0.02], Specialist","[0.1,0.2], Generalist","(0, 0.2], Flexible"), col=c(col.pal[4], col.pal[1], col.pal[2]), lty = 1, lwd=3, bty = "n")
  mtext(side = 1, line = 1, "Time since Environmental Change", outer = TRUE)
  mtext(side = 2, line = 0.5, "Adaptation Level", outer = TRUE)
  mtext(side = 2, line = 2.5, '    80% Modifications       Independent Inventions Only',font=3, outer = TRUE,cex = 0.9)
  legend("topright","F",bty = "n",cex = 1.2)

