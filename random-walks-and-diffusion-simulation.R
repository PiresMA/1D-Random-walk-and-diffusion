# Random walks in one dimension

oneRW1D<-function(N){
  xdir    = 0
  
  xpos    = vector()
  sigma.n = vector()
  
  xpos[1] = xdir  
  sigma.n[1] = 0
  
  for (i in 1:N )
  {
    num <-sample(1:2,1,replace=TRUE)
    
    if(num==1) {xdir = xdir+1}
    if(num==2) {xdir = xdir-1}
    
    xpos[i+1] = xdir
    sigma.n[i+1]   = sd(xpos)
  }
  
  output = data.frame(xpos[1:N],sigma.n[1:N])

  return(output)
}


M     = 10^2 # number of random walks
N     = 10^3 # total number of steps
scale = 1:N

matriz.sd.rw = replicate(M,oneRW1D(N)[,2] )


write.table(matriz.sd.rw, "random-walk-matriz-sd-2.txt", 
            row.names = FALSE, col.names = FALSE)
