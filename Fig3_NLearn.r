
# Code for Fig.3, specialization, innovation and number of learning opportunities


seq<-expand.grid(Nsim=1000, N=100, Tmax=1000, e=c(0.001, 0.01, 0.1),Mxi=0.1, Mm=0, m=c(0),
                 SigmaModMu=0.1,SigmaModSigma=0.01, Nlearn = c(1,5,10,20,50), NumberModels = 3, UpperBoundSigma=0.2, EnvChange= c("Random","Gradual0.05","Gradual0.2"),SigmaOn=c("Yes"),SD_Copy_error=c(0))


N_labels <- c("k = 1","k = 5", "k = 10", "k = 20", "k = 50")

result <- resultsLearningOpportunities

#color stuff
require(RColorBrewer)#load package
display.brewer.all(colorblindFriendly = TRUE) #shows only colorblind friendly schemes, pick ione


x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Dark2") #create a pallette which you loop over for corresponding values


Plot_fct <- function(m, Error){
  
  par(mfrow=c(1,3),
      oma=c(3,2.5,0,0),
      mar=c(1.5,2.4,1.5,0.1))
  ###Independent Innovation Only####
  
  Xi001 <-c()
  for (x in which(seq$e==0.001 & seq$m==m & seq$EnvChange=="Random" & seq$SD_Copy_error== Error)){
    Xi001 <- c(Xi001, mean(result[[x]][["Xi"]]))
  }
  
  Sigma001 <-c()
  for (x in which(seq$e==0.001 & seq$m==m & seq$EnvChange=="Random"& seq$SD_Copy_error== Error)){
    Sigma001 <- c(Sigma001, mean(result[[x]][["Sigma"]]))
  }
  plot(Sigma001,Xi001, ylim= c(0.1,0.5), xlim = c(0.02,0.13), type = "n", ylab = "", xlab = "")
  
  points(Sigma001,Xi001, pch=c(4,15,16,17,18), col=col.pal[4], cex=1.5)

  
  par(new=TRUE)
  
  Xi01 <-c()
  for (x in which(seq$e==0.01 & seq$m==m & seq$EnvChange=="Random"& seq$SD_Copy_error== Error)){
    Xi01 <- c(Xi01, mean(result[[x]][["Xi"]]))
  }
  
  Sigma01 <-c()
  for (x in which(seq$e==0.01 & seq$m==m & seq$EnvChange=="Random"& seq$SD_Copy_error== Error)){
    Sigma01 <- c(Sigma01, mean(result[[x]][["Sigma"]]))
  }
  plot(Sigma01,Xi01, ylim= c(0.1,0.5), xlim = c(0.02,0.13), type = "n", ylab = "", xlab = "")
  points(Sigma01,Xi01, pch=c(4,15,16,17,18), col=col.pal[5], cex=1.5)

  par(new=TRUE)
  
  Xi1 <-c()
  for (x in which(seq$e==0.1 & seq$m==m & seq$EnvChange=="Random"& seq$SD_Copy_error== Error)){
    Xi1 <- c(Xi1, mean(result[[x]][["Xi"]]))
  }
  
  Sigma1 <-c()
  for (x in which(seq$e==0.1 & seq$m==m & seq$EnvChange=="Random"& seq$SD_Copy_error== Error)){
    Sigma1 <- c(Sigma1, mean(result[[x]][["Sigma"]]))
  }
  plot(Sigma1,Xi1, ylim= c(0.1,0.5), xlim = c(0.02,0.13), type = "n", ylab = "", xlab = "")
  points(Sigma1,Xi1, pch=c(4,15,16,17,18), col=col.pal[6], cex=1.5)
  title("Drastic Fluctuations")
  
  
  
  
  
  
  
  lines(c(Sigma001[1],Sigma01[1],Sigma1[1]),c(Xi001[1],Xi01[1],Xi1[1]), col="lightgrey", lwd=0.5)
  lines(c(Sigma001[2],Sigma01[2],Sigma1[2]),c(Xi001[2],Xi01[2],Xi1[2]), col="lightgrey", lwd=0.5)
  
  lines(c(Sigma001[3],Sigma01[3],Sigma1[3]),c(Xi001[3],Xi01[3],Xi1[3]), col="lightgrey", lwd=0.5)
  lines(c(Sigma001[4],Sigma01[4],Sigma1[4]),c(Xi001[4],Xi01[4],Xi1[4]), col="lightgrey", lwd=0.5)
  lines(c(Sigma001[5],Sigma01[5],Sigma1[5]),c(Xi001[5],Xi01[5],Xi1[5]),col="lightgrey", lwd=0.5)
  
  ###
  ###
  ###
  ### GRADUAL environment
  ###
  ###
  ###
  
  ###Independent Innovation Only####
  
  Xi001 <-c()
  for (x in which(seq$e==0.001 & seq$m==m & seq$EnvChange=="Gradual0.2" & seq$SD_Copy_error== Error)){
    Xi001 <- c(Xi001, mean(result[[x]][["Xi"]]))
  }
  
  Sigma001 <-c()
  for (x in which(seq$e==0.001 & seq$m==m & seq$EnvChange=="Gradual0.2"& seq$SD_Copy_error== Error)){
    Sigma001 <- c(Sigma001, mean(result[[x]][["Sigma"]]))
  }
  plot(Sigma001,Xi001, ylim= c(0.1,0.5), xlim = c(0.02,0.13), type = "n", ylab = "", xlab = "")
  
  points(Sigma001,Xi001, pch=c(4,15,16,17,18), col=col.pal[4], cex=1.5)
  
  
  par(new=TRUE)
  
  Xi01 <-c()
  for (x in which(seq$e==0.01 & seq$m==m & seq$EnvChange=="Gradual0.2"& seq$SD_Copy_error== Error)){
    Xi01 <- c(Xi01, mean(result[[x]][["Xi"]]))
  }
  
  Sigma01 <-c()
  for (x in which(seq$e==0.01 & seq$m==m & seq$EnvChange=="Gradual0.2"& seq$SD_Copy_error== Error)){
    Sigma01 <- c(Sigma01, mean(result[[x]][["Sigma"]]))
  }
  plot(Sigma01,Xi01, ylim= c(0.1,0.5), xlim = c(0.02,0.13), type = "n", ylab = "", xlab = "")
  points(Sigma01,Xi01, pch=c(4,15,16,17,18), col=col.pal[5], cex=1.5)
  
  par(new=TRUE)
  
  Xi1 <-c()
  for (x in which(seq$e==0.1 & seq$m==m & seq$EnvChange=="Gradual0.2"& seq$SD_Copy_error== Error)){
    Xi1 <- c(Xi1, mean(result[[x]][["Xi"]]))
  }
  
  Sigma1 <-c()
  for (x in which(seq$e==0.1 & seq$m==m & seq$EnvChange=="Gradual0.2"& seq$SD_Copy_error== Error)){
    Sigma1 <- c(Sigma1, mean(result[[x]][["Sigma"]]))
  }
  plot(Sigma1,Xi1, ylim= c(0.1,0.5), xlim = c(0.02,0.13), type = "n", ylab = "", xlab = "")
  points(Sigma1,Xi1, pch=c(4,15,16,17,18), col=col.pal[6], cex=1.5)
  title("Strong Gradual Fluctuations")
  
  
  
  
  
  
  
  lines(c(Sigma001[1],Sigma01[1],Sigma1[1]),c(Xi001[1],Xi01[1],Xi1[1]), col="lightgrey", lwd=0.5)
  lines(c(Sigma001[2],Sigma01[2],Sigma1[2]),c(Xi001[2],Xi01[2],Xi1[2]), col="lightgrey", lwd=0.5)
  
  lines(c(Sigma001[3],Sigma01[3],Sigma1[3]),c(Xi001[3],Xi01[3],Xi1[3]), col="lightgrey", lwd=0.5)
  lines(c(Sigma001[4],Sigma01[4],Sigma1[4]),c(Xi001[4],Xi01[4],Xi1[4]), col="lightgrey", lwd=0.5)
  lines(c(Sigma001[5],Sigma01[5],Sigma1[5]),c(Xi001[5],Xi01[5],Xi1[5]),col="lightgrey", lwd=0.5)
  
  ###
  ###
  ###
  ### GRADUAL environment
  ###
  ###
  ###
  
  ###Independent Innovation Only####
  
  Xi001 <-c()
  for (x in which(seq$e==0.001 & seq$m==m & seq$EnvChange=="Gradual0.05" & seq$SD_Copy_error== Error)){
    Xi001 <- c(Xi001, mean(result[[x]][["Xi"]]))
  }
  
  Sigma001 <-c()
  for (x in which(seq$e==0.001 & seq$m==m & seq$EnvChange=="Gradual0.05"& seq$SD_Copy_error== Error)){
    Sigma001 <- c(Sigma001, mean(result[[x]][["Sigma"]]))
  }
  plot(Sigma001,Xi001, ylim= c(0.1,0.5), xlim = c(0.02,0.13), type = "n", ylab = "", xlab = "")
  
  points(Sigma001,Xi001, pch=c(4,15,16,17,18), cex=1.5 ,col=col.pal[4])
  
  
  par(new=TRUE)
  
  Xi01 <-c()
  for (x in which(seq$e==0.01 & seq$m==m & seq$EnvChange=="Gradual0.05"& seq$SD_Copy_error== Error)){
    Xi01 <- c(Xi01, mean(result[[x]][["Xi"]]))
  }
  
  Sigma01 <-c()
  for (x in which(seq$e==0.01 & seq$m==m & seq$EnvChange=="Gradual0.05"& seq$SD_Copy_error== Error)){
    Sigma01 <- c(Sigma01, mean(result[[x]][["Sigma"]]))
  }
  plot(Sigma01,Xi01, ylim= c(0.1,0.5), xlim = c(0.02,0.13), type = "n", ylab = "", xlab = "")
  points(Sigma01,Xi01, pch=c(4,15,16,17,18), col=col.pal[5], cex=1.5)
  
  par(new=TRUE)
  
  Xi1 <-c()
  for (x in which(seq$e==0.1 & seq$m==m & seq$EnvChange=="Gradual0.05"& seq$SD_Copy_error== Error)){
    Xi1 <- c(Xi1, mean(result[[x]][["Xi"]]))
  }
  
  Sigma1 <-c()
  for (x in which(seq$e==0.1 & seq$m==m & seq$EnvChange=="Gradual0.05"& seq$SD_Copy_error== Error)){
    Sigma1 <- c(Sigma1, mean(result[[x]][["Sigma"]]))
  }
  plot(Sigma1,Xi1, ylim= c(0.1,0.5), xlim = c(0.02,0.13), type = "n", ylab = "", xlab = "")
  points(Sigma1,Xi1, pch=c(4,15,16,17,18), col=col.pal[6], cex=1.5)
  title("Mild Gradual Fluctuations")
  
  
  
  
  
  
  
  lines(c(Sigma001[1],Sigma01[1],Sigma1[1]),c(Xi001[1],Xi01[1],Xi1[1]), col="lightgrey", lwd=0.5)
  lines(c(Sigma001[2],Sigma01[2],Sigma1[2]),c(Xi001[2],Xi01[2],Xi1[2]), col="lightgrey", lwd=0.5)
  
  lines(c(Sigma001[3],Sigma01[3],Sigma1[3]),c(Xi001[3],Xi01[3],Xi1[3]), col="lightgrey", lwd=0.5)
  lines(c(Sigma001[4],Sigma01[4],Sigma1[4]),c(Xi001[4],Xi01[4],Xi1[4]), col="lightgrey", lwd=0.5)
  lines(c(Sigma001[5],Sigma01[5],Sigma1[5]),c(Xi001[5],Xi01[5],Xi1[5]),col="lightgrey", lwd=0.5)
  
  legend("topleft", c("e = 0.001","e = 0.01","e = 0.1"), col=c(col.pal[4], col.pal[5], col.pal[6]), lty=1, bty="n", lwd = 3)
  legend("topright", c("k = 1","k = 5","k = 10","k = 20","k = 50"), pch=c(4,15,16,17,18), lty=0, bty="n", cex = 1.1)
  mtext(side = 1, expression(bar(sigma)[pop]), line = 2, outer = TRUE, cex = 1.2)
  mtext(side = 2, expression(bar(xi)[pop]), line = 0, outer = TRUE, cex = 1.2)
  
  #######################################################
  #######################################################
  #######################################################
}

Plot_fct(0,0)

