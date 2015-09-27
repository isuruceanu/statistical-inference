set.seed(123)
lambda <- 0.2
nSim <- 1000
batchSize <- 40


### Function to run exponential distribution simulation
### Arguments
###   simulationCount - number of simulations to run
###   batchSize - number of random uniforms
###   rate - rate parameter or lambda
simulation <- function(simulationCount, batchSize = 40, rate = 0.2){
    sim <- matrix(
        rexp(simulationCount * batchSize, rate = rate)
        , simulationCount
        , batchSize)
    simMeans <- rowMeans(sim)
    simMeans
}


### Function to plot exponential distribution histogram
### Arguments
###  meanX - a vector of simulated averages
###  lambda - rate parameter
###  batchSize - number of random uniforms
plotHistogram <- function(meanX, lambda = 0.2, batchSize = 40){
    
    len = length(meanX)
    hist(meanX, breaks = 50, prob = T, xlab=""
         , ylim = c(0, 0.8)
         , main = paste("Distribution of averages of", len, "samples"))
    lines(density(meanX))
    #theoretical
    abline(v = 1/lambda, col="red")
    x <- seq(min(meanX), max(max(meanX)), length = len)
    y <- dnorm(x, mean = 1/lambda, sd = (1/lambda/sqrt(batchSize)))
    lines(x, y, col="red", lty=2)
    
    #only with recordGraphics was able to draw a nice legend 
    recordGraphics(
    legend('topright', c("simulation", "theoretical")
           , lty=c(1,2) 
           , col=c("black", "red")
           , cex=0.8)
    ,list(), getNamespace("graphics"))
    
}