# Function to simulate one random walk in one dimension
oneRW1D<-function(N){
  xdir    = 0  # Initial position
  
  xpos    = vector()
  sigma.n = vector()
  
  xpos[1] = xdir  
  sigma.n[1] = 0
  
  for (i in 1:N )
  {
    xdir = xdir + sample( c(-1,1) ,1,replace=TRUE) # symmetric random walk, p=q=1/2    
    xpos[i+1] = xdir
    sigma.n[i+1]   = sd(xpos)
  }
  
  output = data.frame(xpos[1:N],sigma.n[1:N])

  return(output)
}


M     = 10^2 # number of random walks
N     = 10^3 # total number of steps

# The function replicate() allows us to do M random walks
matriz.sd.rw = replicate(M,oneRW1D(N)[,2] )

# Save data
write.table(matriz.sd.rw, "random-walk-matriz-sd-2.txt", 
            row.names = FALSE, col.names = FALSE)
