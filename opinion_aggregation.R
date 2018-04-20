library(markovchain)  #'markovchain' package for making a stochastic matrix
library(Rfast)        #'Rfast' package contains handy 'nth()' function

#Generate random opinions (10 agents below)

agents <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10")
optimists <- runif(1/3*length(agents), 0.75, 0.99)
pessimists <- runif(1/3*length(agents), 0.01, 0.25)
fencers <- runif(length(agents)-sum(length(optimists), length(pessimists)), 0.26, 0.74)
random.opinions <- matrix(sort(c(optimists, fencers, pessimists), decreasing=TRUE), dimnames=list(agents))


#Create Weight Table (10 x 10 matrix) for Biased Optimists/Pessimists and Neutral Fencers

num.optimists <- length(random.opinions[random.opinions >= 0.75])

self.a1 <- sort(runif(num.optimists, 0.5, 1), decreasing=TRUE)
a2.data <- runif(num.optimists, 0.5, 1) 
self.a2 <- c(nth(a2.data,2), nth(a2.data,3), nth(a2.data, 1))
self.a3 <- sort(runif(num.optimists, 0.5, 1), decreasing=FALSE)

remaining.a1 <- runif(length(which(random.opinions < 0.75)), 0, 0.5)
remaining.a2 <- runif(length(which(random.opinions < 0.75)), 0, 0.5)
remaining.a3 <- runif(length(which(random.opinions < 0.75)), 0, 0.5)

profile.a1 <- c(self.a1, remaining.a1)
profile.a2 <- c(self.a2, remaining.a2)
profile.a3 <- c(self.a3, remaining.a3)

opt <- matrix(c(profile.a1, profile.a2, profile.a3), byrow=TRUE, nrow=num.optimists)
normalize <- function(x){ x / sum(x) }
norm.opt <- apply(opt, 1, normalize)
optimist.wts <- t(norm.opt)


num.fencers <- length(random.opinions[random.opinions > 0.25 & random.opinions < 0.75])

self.a4 <- runif(length(agents), 0, 1)
self.a5 <- runif(length(agents), 0, 1) 
self.a6 <- runif(length(agents), 0, 1) 
self.a7 <- runif(length(agents), 0, 1)

fence <- matrix(c(self.a4, self.a5, self.a6, self.a7), byrow=TRUE, nrow=num.fencers)
norm.fence <- apply(fence, 1, normalize)
fencers.wts <- t(norm.fence)


num.pessimists <- length(random.opinions[random.opinions <= 0.25])

self.a8 <- sort(runif(num.pessimists, 0.5, 1), decreasing=FALSE)
a9.data <- runif(num.pessimists, 0.5, 1) 
self.a9 <- c(nth(a9.data,1), nth(a9.data,3), nth(a9.data, 2))
self.a10 <- sort(runif(num.pessimists, 0.5, 1), decreasing=TRUE)

remaining.a8 <- runif(length(which(random.opinions > 0.25)), 0, 0.5)
remaining.a9 <- runif(length(which(random.opinions > 0.25)), 0, 0.5)
remaining.a10 <- runif(length(which(random.opinions > 0.25)), 0, 0.5)

profile.a8 <- c(remaining.a8, self.a1)
profile.a9 <- c(remaining.a9, self.a2)
profile.a10 <- c(remaining.a10, self.a3)

pest <- matrix(c(profile.a8, profile.a9, profile.a10), byrow=TRUE, nrow=num.pessimists)
norm.pest <- apply(pest, 1, normalize)
pessimist.wts <- t(norm.pest)


#Form weight table in the order optimists, fencers, and pessimists

weights <- rbind(optimist.wts, fencers.wts, pessimist.wts)
weight.table <- matrix(t(weights), byrow=TRUE, nrow=length(agents), dimnames=list(agents, agents))

#Convert 'weight.table' to stochastic matrix using 'markovchain'
stochastic.matrix <- new("markovchain", states=agents, byrow=TRUE, 
  transitionMatrix=weight.table, name="Weights")


#Opinion Aggregation
opinion.data <- list(random.opinions)
i <- 1
#DeGroot, Lehrer, Wagner model [ F^(n) = P^(n)F where P is a stochastic matrix and F is a column vector ] for 20 "deliberations"
aggregation <- for(i in 1:20) {
  iterations <- stochastic.matrix^i
  consensus <- iterations * random.opinions
  opinion.data[[i + 1]] <- consensus
}
structured.data <- do.call(cbind, opinion.data)


#Visualization
plot(structured.data[1, ], xlab="Time Series", ylab="Probability Estimates", 
  pch=20, ylim=c(.01,.99), xlim=c(1,21))
colors = rainbow(length(agents))
plot.lines <- for (i in 1:length(agents)){  
  lines(structured.data[i,], type="o", lty=1, lwd=1, col=colors[i], pch=1)
}
