### R code from vignette source 'kedd.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: kedd.Rnw:130-131
###################################################

library(kedd)

###################################################
### code chunk number 2: kedd.Rnw:133-135
###################################################

kernel.fun(x = seq(-0.02,0.02,by=0.01), deriv.order = 1, kernel = "gaussian")$kx
kernel.conv(x = seq(-0.02,0.02,by=0.01), deriv.order = 1, kernel = "gaussian")$kx

###################################################
### code chunk number 3: kedd.Rnw:137-139
###################################################

plot(kernel.fun(deriv.order = 1, kernel = "gaussian"))
plot(kernel.conv(deriv.order = 1, kernel = "gaussian"))

###################################################
### code chunk number 6: kedd.Rnw:174-180
###################################################

fx <- function(x) 0.5 *(-4*x-6)* dnorm(x,-1.5,0.5) + 0.5 *(-4*x+6) * dnorm(x,1.5,0.5)
kernels <- c("gaussian","biweight","triweight","tricube")
plot(dkde(x = bimodal, deriv.order = 1, h = 0.6, kernel = kernels[1]),col = 1,ylim=c(-0.6,0.6) ,sub="", main="")
for (i in 2:length(kernels))lines(dkde(x = bimodal, deriv.order = 1, h = 0.6, kernel = kernels[i] ), col = i)
curve(fx,add=TRUE,lty=8)
legend("topright", legend = c(TRUE,kernels), col = c("black",seq(kernels)),lty = c(8,rep(1,length(kernels))), inset = .015)

###################################################
### code chunk number 7: kedd.Rnw:182-187
###################################################

h <- c(0.14,0.3,0.6,1.2)                                                                                                               
plot(dkde(x = bimodal, deriv.order = 1, h = h[1], kernel = kernels[1]),col = 1,ylim=c(-0.6,1) ,sub="", main="")                   
for (i in 2:length(h))lines(dkde(x = bimodal, deriv.order = 1, h = h[i], kernel = kernels[1] ), col = i)                            
curve(fx,add=TRUE,lty=8)                                                                                                            
legend("topright", legend = c("TRUE",paste("h =",bquote(.(h)))), col = c("black",seq(h)),lty = c(8,rep(1,length(h))), inset = .015) 

###################################################
### code chunk number 8: kedd.Rnw:227-231
###################################################

hatf  <- dkde(bimodal, deriv.order = 0)
hatf1 <- dkde(bimodal, deriv.order = 1)
hatf2 <- dkde(bimodal, deriv.order = 2)
hatf3 <- dkde(bimodal, deriv.order = 3)

###################################################
### code chunk number 9: kedd.Rnw:235-246
###################################################

fx  <- function(x) 0.5 * dnorm(x,-1.5,0.5) + 0.5 * dnorm(x,1.5,0.5)
fx1 <- function(x) 0.5 *(-4*x-6)* dnorm(x,-1.5,0.5) + 0.5 *(-4*x+6) * 
                   dnorm(x,1.5,0.5)
fx2 <- function(x) 0.5 * ((-4*x-6)^2 - 4) * dnorm(x,-1.5,0.5) + 0.5 *
                   ((-4*x+6)^2 - 4) * dnorm(x,1.5,0.5)
fx3 <- function(x) 0.5 * (-4*x-6) * ((-4*x-6)^2 - 12) * dnorm(x,-1.5,0.5) +
                     0.5 * (-4*x+6) * ((-4*x+6)^2 - 12) * dnorm(x,1.5,0.5)
plot(hatf ,fx = fx)
plot(hatf1,fx = fx1)
plot(hatf2,fx = fx2)
plot(hatf3,fx = fx3)

###################################################
### code chunk number 14: kedd.Rnw:310-314
###################################################

h.amise(bimodal, deriv.order = 0)
h.amise(bimodal, deriv.order = 1)
h.amise(bimodal, deriv.order = 2)
h.amise(bimodal, deriv.order = 3)

###################################################
### code chunk number 15: kedd.Rnw:350-357
###################################################

kernels <- eval(formals(h.mlcv.default)$kernel)
hmlcv <- numeric()
for(i in 1:length(kernels))  
             hmlcv[i] <- h.mlcv(bimodal, kernel =  kernels[i])$h
data.frame(kernels,hmlcv)

###################################################
### code chunk number 17: kedd.Rnw:360-362
###################################################

plot(h.mlcv(bimodal, kernel =  kernels[1]), seq.bws = seq(0.1,1,length=50))
plot(h.mlcv(bimodal, kernel =  kernels[2]), seq.bws = seq(0.1,1,length=50))

###################################################
### code chunk number 20: kedd.Rnw:410-414
###################################################

h.ucv(bimodal, deriv.order = 0)
h.ucv(bimodal, deriv.order = 1)
h.ucv(bimodal, deriv.order = 2)
h.ucv(bimodal, deriv.order = 3)

###################################################
### code chunk number 21: kedd.Rnw:417-418
###################################################

for (i in 0:3) plot(h.ucv(bimodal, deriv.order = i))

###################################################
### code chunk number 26: kedd.Rnw:479-483
###################################################

h.bcv(bimodal, whichbcv = 1, deriv.order = 0)
h.bcv(bimodal, whichbcv = 2, deriv.order = 0)
h.bcv(bimodal, whichbcv = 1, deriv.order = 1, lower=0.1, upper=0.8)
h.bcv(bimodal, whichbcv = 2, deriv.order = 1, lower=0.1, upper=0.8)

###################################################
### code chunk number 27: kedd.Rnw:486-497
###################################################
#
# deriv.order = 0
plot(h.bcv(bimodal, whichbcv = 2, deriv.order = 0))
lines(h.bcv(bimodal, whichbcv = 1, deriv.order = 0),col="red")
legend("topright", c("BCV1","BCV2"),lty=1,col=c("red","black"),
       inset = .015)
## deriv.order = 1
plot(h.bcv(bimodal, whichbcv = 2, deriv.order = 1),seq.bws = 
     seq(0.1,0.8,length=50))
lines(h.bcv(bimodal, whichbcv = 1, deriv.order = 1),col="red")
legend("topright", c("BCV1","BCV2"),lty=1,col=c("red","black"),
      inset = .015)

###################################################
### code chunk number 30: kedd.Rnw:551-555
###################################################

h.ccv(bimodal, deriv.order = 0, upper = 0.5)
h.ccv(bimodal, deriv.order = 1, upper = 0.5)
h.ccv(bimodal, deriv.order = 2, upper = 0.5)
h.ccv(bimodal, deriv.order = 3, upper = 0.5)

###################################################
### code chunk number 31: kedd.Rnw:558-560
###################################################

for (i in 0:3) 
 plot(h.ccv(bimodal, deriv.order = i), seq.bws=seq(0.1,0.5,length=50))

###################################################
### code chunk number 36: kedd.Rnw:612-616
###################################################

h.mcv(bimodal, deriv.order = 0, upper = 0.5)
h.mcv(bimodal, deriv.order = 1, upper = 0.5)
h.mcv(bimodal, deriv.order = 2, upper = 0.5)
h.mcv(bimodal, deriv.order = 3, upper = 0.5)

###################################################
### code chunk number 37: kedd.Rnw:619-621
###################################################

for (i in 0:3)
 plot(h.mcv(bimodal, deriv.order = i), seq.bws=seq(0.1,0.5,length=50))

###################################################
### code chunk number 42: kedd.Rnw:674-678
###################################################

h.tcv(bimodal, deriv.order = 0)
h.tcv(bimodal, deriv.order = 1)
h.tcv(bimodal, deriv.order = 2)
h.tcv(bimodal, deriv.order = 3)

###################################################
### code chunk number 43: kedd.Rnw:681-682
###################################################

for (i in 0:3) plot(h.tcv(bimodal, deriv.order = i))
