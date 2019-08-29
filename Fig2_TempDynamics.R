
# Code for Fig.2, basic adaptation dynamics, xi and sigma for baseline model


library(scales)

seq<-expand.grid(Nsim=1000, N=100, Tmax=300, e=c(0.001, 0.01),Mxi=0.1, Mm=0, m=c(0),SigmaModMu=0.1,SigmaModSigma=0.01,
                 Nlearn = c(0,1,10), NumberModels = 3, UpperBoundSigma=0.2, EnvChange= c("Random","Gradual0.05"),SigmaOn=c("Yes"), SD_Copy_error=0)

CI_fct <- function(x){return(sort(x)[c(51,950)])}


result <- resultBaselineDynamics

#Calculate mean and 90% PIs over 1000 independent simulations
for (i in 1:nrow(seq)) {
  result[[i]][["MeanSigma"]] <- apply(result[[i]][["Sigma"]], 2, mean)
  result[[i]][["MeanAdaptation"]] <- apply(result[[i]][["Adaptation"]], 2, mean)
  result[[i]][["MeanXi"]] <- apply(result[[i]][["Xi"]], 2, mean)
  
  result[[i]][["CISigma"]] <- apply(result[[i]][["Sigma"]], 2, CI_fct)
  result[[i]][["CIXi"]] <- apply(result[[i]][["Xi"]], 2, CI_fct)
  result[[i]][["CIAdaptation"]] <- apply(result[[i]][["Adaptation"]], 2, CI_fct)
}

#color stuff
require(RColorBrewer)#load package
display.brewer.all(colorblindFriendly = TRUE) #shows only colorblind friendly schemes, pick ione


x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Dark2") #create a pallette which you loop over for corresponding values


# Define plotting function

Plot_fct <- function(e, Mod){
  par(mfrow=c(4,2),
      oma = c(3,3,1,0),
      mar = c(2,4,2,5))
  
 #k=10
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CISigma"]][1,], type = "n", xlab = "",yaxt='n', ann=FALSE, ylab = "", ylim = c(0,0.15), col=col.pal[1])
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CISigma"]][2,], type = "n", xlab = "",yaxt='n', ann=FALSE,ylab = "", ylim = c(0,0.15),col=col.pal[1])
  
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CISigma"]][1,],
                              rev(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CISigma"]][2,])), col=alpha(col.pal[1],alpha = 0.4),yaxt='n', ann=FALSE, border = NA, ylim=c(0,0.15))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIXi"]][1,], type = "n", xlab = "", ylab = "", ylim = c(0,0.6), axes = FALSE)
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIXi"]][2,], type = "n", xlab = "", ylab = "", ylim = c(0,0.6),axes = FALSE)
  par(new=TRUE)
  
  
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIXi"]][1,],
                              rev(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIXi"]][2,])),col=alpha(col.pal[4],alpha = 0.3), border = NA, ylim = c(0,0.6))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["MeanSigma"]], type = "l", xlab = "", lwd=2,ylab = "", ylim = c(0,0.15),yaxt='n', ann=FALSE,col=col.pal[1])
  axis(side=2, col=col.pal[1], at=seq(from=0,to=0.15,by=0.05), col.axis=col.pal[1])
  
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["MeanXi"]], type = "l", xlab = "", lwd=2,ylab = "", ylim = c(0,0.6), col=col.pal[4], axes = FALSE)

  axis(side=4, col=col.pal[4], at=seq(from=0,to=0.6,by=0.1) , col.axis=col.pal[4])
  

    
  mtext(side = 3, "Ten Learners (k = 10)", line = 0.8, cex = 0.8)
  mtext(side = 2, expression(bar(sigma)[pop]), line = 2.5,col=col.pal[1])
  mtext(side = 4, expression(bar(xi)[pop]), line = 3, col=col.pal[4])
  
  #k=1
  
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CISigma"]][1,], type = "n", xlab = "",yaxt='n', ann=FALSE, ylab = "", ylim = c(0,0.15), col=col.pal[1])
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CISigma"]][2,], type = "n", xlab = "",yaxt='n', ann=FALSE,ylab = "", ylim = c(0,0.15),col=col.pal[1])
  
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CISigma"]][1,],
                                    rev(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CISigma"]][2,])), col=alpha(col.pal[1],alpha = 0.4),yaxt='n', ann=FALSE, border = NA, ylim=c(0,0.15))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIXi"]][1,], type = "n", xlab = "", ylab = "", ylim = c(0,0.6), axes = FALSE)
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIXi"]][2,], type = "n", xlab = "", ylab = "", ylim = c(0,0.6),axes = FALSE)
  par(new=TRUE)
  
  
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIXi"]][1,],
                                    rev(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIXi"]][2,])),col=alpha(col.pal[4],alpha = 0.3), border = NA, ylim = c(0,0.6))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["MeanSigma"]], type = "l", xlab = "", lwd=2,ylab = "", ylim = c(0,0.15),yaxt='n', ann=FALSE,col=col.pal[1])
  axis(side=2, col=col.pal[1], at=seq(from=0,to=0.15,by=0.05), col.axis=col.pal[1])
  
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["MeanXi"]], type = "l", xlab = "", lwd=2,ylab = "", ylim = c(0,0.6), col=col.pal[4], axes = FALSE)
  
  axis(side=4, col=col.pal[4], at=seq(from=0,to=0.6,by=0.1) , col.axis=col.pal[4])
  
  
  
  mtext(side = 3, "One Learner (k = 1)", line = 0.8, cex = 0.8)
  mtext(side = 2, expression(bar(sigma)[pop]), line = 2.5,col=col.pal[1])
  mtext(side = 4, expression(bar(xi)[pop]), line = 3, col=col.pal[4])
  
  ###Adaptation plots###

  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIAdaptation"]][1,], type = "n", xlab = "", ylab = "", ylim = c(0,0.8))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIAdaptation"]][2,], type = "n", xlab = "", ylab = "", ylim = c(0,0.8))
  par(new=TRUE)
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIAdaptation"]][1,],
          rev(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIAdaptation"]][2,])), col=alpha("grey",alpha = 0.5), border = NA)
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["MeanAdaptation"]], type = "l", lwd=2,xlab = "", ylab = "", ylim = c(0,0.8))
  mtext(side = 2, expression(bar(a)[pop]), line = 2.5)
  
  
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIAdaptation"]][1,], type = "n", xlab = "", ylab = "", ylim = c(0,0.8))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIAdaptation"]][2,], type = "n", xlab = "", ylab = "", ylim = c(0,0.8))
  par(new=TRUE)
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIAdaptation"]][1,],
                              rev(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["CIAdaptation"]][2,])), col=alpha("grey",alpha = 0.5), border = NA)
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Random"&seq$m==Mod)]][["MeanAdaptation"]], type = "l",  lwd=2,xlab = "", ylab = "", ylim = c(0,0.8))
  mtext(side = 2, expression(bar(a)[pop]), line = 2.5)
  
  
  

  #k=10
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CISigma"]][1,], type = "n", xlab = "",yaxt='n', ann=FALSE, ylab = "", ylim = c(0,0.15), col=col.pal[1])
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CISigma"]][2,], type = "n", xlab = "",yaxt='n', ann=FALSE,ylab = "", ylim = c(0,0.15),col=col.pal[1])
  
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CISigma"]][1,],
                                    rev(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CISigma"]][2,])), col=alpha(col.pal[1],alpha = 0.4),yaxt='n', ann=FALSE, border = NA, ylim=c(0,0.15))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIXi"]][1,], type = "n", xlab = "", ylab = "", ylim = c(0,0.6), axes = FALSE)
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIXi"]][2,], type = "n", xlab = "", ylab = "", ylim = c(0,0.6),axes = FALSE)
  par(new=TRUE)
  
  
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIXi"]][1,],
                                    rev(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIXi"]][2,])),col=alpha(col.pal[4],alpha = 0.3), border = NA, ylim = c(0,0.6))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["MeanSigma"]], type = "l", xlab = "", lwd=2,ylab = "", ylim = c(0,0.15),yaxt='n', ann=FALSE,col=col.pal[1])
  axis(side=2, col=col.pal[1], at=seq(from=0,to=0.15,by=0.05), col.axis=col.pal[1])
  
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["MeanXi"]], type = "l", xlab = "", lwd=2,ylab = "", ylim = c(0,0.6), col=col.pal[4], axes = FALSE)
  
  axis(side=4, col=col.pal[4], at=seq(from=0,to=0.6,by=0.1) , col.axis=col.pal[4])
  
  
  mtext(side = 2, expression(bar(sigma)[pop]), line = 2.5,col=col.pal[1])
  mtext(side = 4, expression(bar(xi)[pop]), line = 3, col=col.pal[4])
  
  #k=1
  
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CISigma"]][1,], type = "n", xlab = "",yaxt='n', ann=FALSE, ylab = "", ylim = c(0,0.15), col=col.pal[1])
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CISigma"]][2,], type = "n", xlab = "",yaxt='n', ann=FALSE,ylab = "", ylim = c(0,0.15),col=col.pal[1])
  
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CISigma"]][1,],
                                    rev(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CISigma"]][2,])), col=alpha(col.pal[1],alpha = 0.4),yaxt='n', ann=FALSE, border = NA, ylim=c(0,0.15))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIXi"]][1,], type = "n", xlab = "", ylab = "", ylim = c(0,0.6), axes = FALSE)
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIXi"]][2,], type = "n", xlab = "", ylab = "", ylim = c(0,0.6),axes = FALSE)
  par(new=TRUE)
  
  
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIXi"]][1,],
                                    rev(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIXi"]][2,])),col=alpha(col.pal[4],alpha = 0.3), border = NA, ylim = c(0,0.6))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["MeanSigma"]], type = "l", xlab = "", lwd=2,ylab = "", ylim = c(0,0.15),yaxt='n', ann=FALSE,col=col.pal[1])
  axis(side=2, col=col.pal[1], at=seq(from=0,to=0.15,by=0.05), col.axis=col.pal[1])
  
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["MeanXi"]], type = "l", xlab = "", lwd=2,ylab = "", ylim = c(0,0.6), col=col.pal[4], axes = FALSE)
  
  axis(side=4, col=col.pal[4], at=seq(from=0,to=0.6,by=0.1) , col.axis=col.pal[4])
  
  
    mtext(side = 2, expression(bar(sigma)[pop]), line = 2.5,col=col.pal[1])
  mtext(side = 4, expression(bar(xi)[pop]), line = 3, col=col.pal[4])
  
  
  ###Adaptation plots###
  ###Stable Random###
  
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIAdaptation"]][1,], type = "n", xlab = "", ylab = "", ylim = c(0,0.8))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIAdaptation"]][2,], type = "n", xlab = "", ylab = "", ylim = c(0,0.8))
  par(new=TRUE)
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIAdaptation"]][1,],
                              rev(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIAdaptation"]][2,])), col=alpha("grey",alpha = 0.5), border = NA)
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==10 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["MeanAdaptation"]], type = "l", lwd=2,xlab = "", ylab = "", ylim = c(0,0.8))
  mtext(side = 2, expression(bar(a)[pop]), line = 2.5)
  
  
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIAdaptation"]][1,], type = "n", xlab = "", ylab = "", ylim = c(0,0.8))
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIAdaptation"]][2,], type = "n", xlab = "", ylab = "", ylim = c(0,0.8))
  par(new=TRUE)
  polygon(c(1:(1.5/e),(1.5/e):1), c(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIAdaptation"]][1,],
                            rev(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["CIAdaptation"]][2,])), col=alpha("grey",alpha = 0.5), border = NA)
  par(new=TRUE)
  plot(result[[which(seq$Nlearn==1 & seq$e==e & seq$SigmaOn=="Yes"&seq$EnvChange=="Gradual0.05"&seq$m==Mod)]][["MeanAdaptation"]], type = "l", lwd=2,xlab = "", ylab = "", ylim = c(0,0.8))
  mtext(side = 2, expression(bar(a)[pop]), line = 2.5)
  
  
  mtext(side = 1, "Time since Environmental Change", line = 1.5, outer = TRUE, cex = 1.1)
  mtext(side = 2, "Gradual Fluctuations                                     Drastic Fluctuations", outer = TRUE, line = 1.5, cex = 1.1)
}

Plot_fct(0.001,0)

