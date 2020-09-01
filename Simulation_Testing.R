options(scipen=999)

#Inverse Transform Model - Triangular calculation
inv_triangle_cdf <- function(P, vmin, vml, vmax){
  
  Pvml <- (vml-vmin)/(vmax-vmin)
  
  return(ifelse(P < Pvml,
                vmin + sqrt(P*(vml-vmin)*(vmax-vmin)),
                vmax - sqrt((1-P)*(vmax-vml)*(vmax-vmin))))
}

#Number of simulations
n <- 10000

#Price of gold in year one
year_1_price <- 2500

#Triangular Inputs
Inputs <- read.csv(file="inputs.txt", stringsAsFactors = F)

Output <- as.data.frame(matrix(nrow=n,ncol=nrow(Inputs)))

for (i in 1:nrow(Inputs)){
  #set task costs
  vmin <- Inputs$min[i]
  vml <- Inputs$likely[i]
  vmax <- Inputs$max[i]
  
  #generate n random numbers (one per trial)
  psim <- runif(n)
  #simulate n instances of task
  Output[,i] <- inv_triangle_cdf(psim,vmin,vml,vmax) 
}

output_year_one <- as.data.frame(matrix(nrow=n,ncol=nrow(Inputs[2,])))

for (i in 1:nrow(Inputs[2,])){
  #set task costs
  vmin <- Inputs[2,]$min[i]
  vml <- Inputs[2,]$likely[i]
  vmax <- Inputs[2,]$max[i]
  
  #simulate n instances of task
  output_year_one[,i] <- inv_triangle_cdf(runif(n),vmin,vml,vmax)
  
}

Inputs[3,]

output_changes <- as.data.frame(matrix(nrow=n, ncol=4))

for (i in 1:nrow(Inputs[3,])){
  #set task costs
  vmin <- Inputs[3,]$min[i]
  vml <- Inputs[3,]$likely[i]
  vmax <- Inputs[3,]$max[i]
  
  #simulate n instances of task
  output_changes[,1] <- inv_triangle_cdf(runif(n),vmin,vml,vmax)
  output_changes[,2] <- inv_triangle_cdf(runif(n),vmin,vml,vmax)
  output_changes[,3] <- inv_triangle_cdf(runif(n),vmin,vml,vmax)
  output_changes[,4] <- inv_triangle_cdf(runif(n),vmin,vml,vmax)
}
