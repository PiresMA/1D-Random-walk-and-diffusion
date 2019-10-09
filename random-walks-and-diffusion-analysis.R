# Random walks in one dimension

# Import data
matriz.sd.rw <- read.table("random-walk-matriz-sd.txt")

N = dim(matriz.sd.rw)[1]
M = dim(matriz.sd.rw)[2]

# Compute the dispersion 
vec.sd = apply(matriz.sd.rw,1,mean)
scale = 1:N

# linear fit log scale
log.y <- log( vec.sd[2:N] , base = exp(1) )
log.x <- log( scale[2:N]  , base = exp(1) )
fit   <- lm( log.y ~ log.x )

plot(log.y~log.x,  xlab="log(N) ", ylab="log(Dispersion)",
     lty=0, pch=16, col="cyan", main="RW 1D")
abline(fit,lwd=2, col="red")

# also see print( summary(fit)[[4]] ) 

# parameter estimation
D_est = 0.5*exp( 2*coef(fit)[[1]] )
alpha = coef(fit)[[2]]
c(D_est, alpha)

#-------------Plots----------------------#
plot(scale, vec.sd[scale], ylab="Dispersion" ,xlab="N", col="red", pch=16 )

y = sqrt(2*D_est*scale)  # from Diffusion equation for the random walk 
lines(scale, y,col="black")

mtext( "1D Random Walks and Diffusion", side=3, cex=1.0, line=2)
mtext( expression( R[N]%~%N^{1/2}), side=3, cex=1.0, line=0)

legend( "topleft",  cex=0.7, 
        lty = c(1,0), 
        pch = c(NA, 16), 
        col = c( "black","red"), 
        legend=c( expression( R[N]==sqrt(2*D*N)),
                  expression( "Monte Carlo Simulation" ) ) )

