install.packages("plyr")
install.packages("doMC")
library(plyr)
library(doMC)

doMC::registerDoMC(cores=32) # or however many cores you have access tog <- rnorm(10000000)
h <- rep(NA, 1000000000)

# Start the clock!
ptm <- proc.time()

# Loop through the vector, adding one
for (i in 1:1000000000){
  h[i] <- g[i] + 1
}

# Stop the clock
proc.time() - ptm
