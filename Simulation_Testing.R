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

output_year_one <- as.data.frame(matrix(nrow=n,ncol=nrow(Inputs[2,])))

for (i in 1:nrow(Inputs[2,])){
  #set task costs
  vmin <- Inputs[2,]$min[i]
  vml <- Inputs[2,]$likely[i]
  vmax <- Inputs[2,]$max[i]
  
  #simulate n instances of task
  output_year_one[,i] <- inv_triangle_cdf(runif(n),vmin,vml,vmax)
  
}

colnames(output_year_one)[1] <- 'Year_One'

Inputs[3,]

output_changes <- as.data.frame(matrix(nrow=n, ncol=4))

#Lets just imagine that I did this in a nice way right?
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

#As above
output_year_one['Year_Two'] <- output_year_one[1]*(1+output_changes[1])
output_year_one['Year_Three'] <- output_year_one[2]*(1+output_changes[2])
output_year_one['Year_Four'] <- output_year_one[3]*(1+output_changes[3])
output_year_one['Year_Five'] <- output_year_one[4]*(1+output_changes[4])

yearly_gold_price <- as.data.frame(matrix(nrow=n, ncol=1))

yearly_gold_price[1] <- 2500

yearly_gold_price_change <- as.data.frame(matrix(nrow=n, ncol=4))

Inputs[1,]

for (i in 1:nrow(Inputs[1,])){
  #set task costs
  vmin <- Inputs[1,]$min[i]
  vml <- Inputs[1,]$likely[i]
  vmax <- Inputs[1,]$max[i]
  
  #simulate n instances of task
  yearly_gold_price_change[,1] <- inv_triangle_cdf(runif(n),vmin,vml,vmax)
  yearly_gold_price_change[,2] <- inv_triangle_cdf(runif(n),vmin,vml,vmax) 
  yearly_gold_price_change[,3] <- inv_triangle_cdf(runif(n),vmin,vml,vmax)
  yearly_gold_price_change[,4] <- inv_triangle_cdf(runif(n),vmin,vml,vmax)
}


yearly_gold_price_change

yearly_gold_price

yearly_gold_price['Year_Two'] <- yearly_gold_price[1]+yearly_gold_price_change[1]
yearly_gold_price['Year_Three'] <- yearly_gold_price[2]+yearly_gold_price_change[2]
yearly_gold_price['Year_Four'] <- yearly_gold_price[3]+yearly_gold_price_change[3]
yearly_gold_price['Year_Five'] <- yearly_gold_price[4]+yearly_gold_price_change[4]

