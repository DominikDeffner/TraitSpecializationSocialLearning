
#Script for Fig. 1, illustration of adaptation functions and modes of environmental change

library(scales)

#color stuff
require(RColorBrewer)#load package
display.brewer.all(colorblindFriendly = TRUE) #shows only colorblind friendly schemes, pick ione


x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Dark2") #create a pallette which you loop over for corresponding values


#make sure color is indexed with whatever item you want to differnetiate in a vecor i long/wide
plot(1~1, xlim=c(0,1), ylim=c(0,1))
for(i in 1:length(x)){
  points(x[i],0.5, col=col.pal[i], cex=3, pch=19) 
}

#Define Gaussain adaptation function of environmental state e at time t, e(t), with parameters mu, sigma and amax
Gaussian<-function(t,mu,sigma,amax){amax*exp(-((t-mu)^2)/(2*sigma^2))}

#Define function for geometric mean fitness
geometric.mean<- function(vec){exp(mean(log(vec)))}

#Define adaptation functions for 5 different variants
mu1<--0.6
sigma1<-0.2
amax1<-exp(-10*sigma1)

mu2<--0
sigma2<-0.1
amax2<-exp(-10*sigma2)


mu4<-0.5
sigma4<-0.05
amax4<-exp(-10*sigma4)


mu5<-0.8
sigma5<-0.01
amax5<-exp(-10*sigma5)


Variant1<-function(t){Gaussian(t=t,mu=mu1,amax=amax1,sigma=sigma1)}
Variant2<-function(t){Gaussian(t=t,mu=mu2,amax=amax2,sigma=sigma2)}
Variant4<-function(t){Gaussian(t=t,mu=mu4,amax=amax4,sigma=sigma4)}
Variant5<-function(t){Gaussian(t=t,mu=mu5,amax=amax5,sigma=sigma5)}

#Set up list with 5 adaptation functions
VariantList<- list(Var1=Variant1,Var2=Variant2,Var4=Variant4,Var5=Variant5)

par(mfrow=c(1,2), 
    oma=c(0,0,2,0),
    mar=c(5,4,0,1))

#Plot adaptation functions of our 5 variants
plot(seq(-1,1,by=0.0001),VariantList$Var1(seq(-1,1,by=0.0001)),type = "n", lwd=2, col=col.pal[1],xlab = "State of the Environment E(t)",ylab = "Adaptation Level", ylim = c(0,1))
polygon(c(seq(-1,1,by=0.0001),rev(seq(-1,1,by=0.0001))),c(VariantList$Var1(seq(-1,1,by=0.0001)), rep(0, length(VariantList$Var1(seq(-1,1,by=0.0001))))), col = alpha(col.pal[1], alpha = 0.8), border = NA)
par(new=TRUE)
plot(seq(-1,1,by=0.0001),VariantList$Var2(seq(-1,1,by=0.0001)),type = "n", lwd=2, col=col.pal[2],xlab = "",ylab = "Adaptation Level", ylim = c(0,1))
polygon(c(seq(-1,1,by=0.0001),rev(seq(-1,1,by=0.0001))),c(VariantList$Var2(seq(-1,1,by=0.0001)), rep(0, length(VariantList$Var2(seq(-1,1,by=0.0001))))), col = alpha(col.pal[2], alpha = 0.8), border = NA)

par(new=TRUE)
plot(seq(-1,1,by=0.0001),VariantList$Var4(seq(-1,1,by=0.0001)),type = "n", lwd=2, col=col.pal[3],xlab = "",ylab = "Adaptation Level", ylim = c(0,1))
polygon(c(seq(-1,1,by=0.0001),rev(seq(-1,1,by=0.0001))),c(VariantList$Var4(seq(-1,1,by=0.0001)), rep(0, length(VariantList$Var4(seq(-1,1,by=0.0001))))), col = alpha(col.pal[3], alpha = 0.8), border = NA)

par(new=TRUE)
plot(seq(-1,1,by=0.0001),VariantList$Var5(seq(-1,1,by=0.0001)),type = "n", lwd=2, col=col.pal[4],xlab = "",ylab = "Adaptation Level", ylim = c(0,1))
polygon(c(seq(-1,1,by=0.0001),rev(seq(-1,1,by=0.0001))),c(VariantList$Var5(seq(-1,1,by=0.0001)), rep(0, length(VariantList$Var5(seq(-1,1,by=0.0001))))), col = alpha(col.pal[4], alpha = 0.8), border = NA)

legend("top", c(expression(paste(sigma," = 0.2")), expression(paste(sigma," = 0.1")),expression(paste(sigma," = 0.05")),expression(paste(sigma," = 0.01"))), col = c(col.pal[1],col.pal[2],col.pal[3],col.pal[4]), cex=1.1,lty=1,lwd = 5, bty = "n")
legend("topleft", "A", bty = "n")

Change <- sample(c(1:5000), 50)    
E<-c() 

E[1]<-0
for (i  in 2:5000) {  
  if (i %in% Change){E[i] <- runif(1, -1,1)} 
  else {E[i] <- E[i-1]}
}
plot(E, type = "l", ylim = c(-1,1), ylab = "E(t)", xlab = "Timestep t", lwd=1)

par(new=TRUE)
E<-c()
E[1]<-0
for (i  in 2:5000) {
  if (i %in% Change){
    E0 <- E[i-1]
    E[i] <- E0 + rnorm(1, mean = 0, sd=0.05) 
    while (E[i] < -1 | E[i] > 1) {
      E[i] <- E0 + rnorm(1, mean = 0, sd=0.05) 
    }
  }  else {E[i] <- E[i-1]}
}
plot(E, type = "l", ylim = c(-1,1), ylab = "",lty=2, xlab = "", lwd=2)
legend("topleft", "B", bty = "n")
legend("topright", c("Drastic","Gradual"), lty=c(1,2), lwd=1.5,bty = "n")
