#rm(list = ls())
require(rstan)
#options(scipen=6)

################################
# EARLY PERIOD
################################

# P(D|H1)
z<-(6.1-6.9)/(2/sqrt(10))
z
2*pnorm(abs(z),lower.tail = F) 

# P(D|H2)
z<-(6.1-11)/(2/sqrt(10))
z
2*pnorm(abs(z),lower.tail = F) 

# P(D|H2)
z<-(6.1-14)/(2/sqrt(10))
z
2*pnorm(abs(z),lower.tail = F) 

# create early period data
set.seed(222)
N <- 10
mu <- 6.1
sigma.sq <- 2^2
x <- rnorm(N, mu, sqrt(sigma.sq))

# Stan model
modelcode<-
  "data{
  int<lower=0> N; // number of observations
  real x[N]; //data vector
}

parameters {
  real mu; // mean parameter
  real<lower=0> sigma_sq; // variance parameter
}

model {
  sigma_sq ~ uniform(0, 100);
  mu ~ normal(0, 100);
  for (i in 1:N) {
    x[i] ~ normal(mu, sigma_sq);
  }

}"

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

mod1 <- stan(model_code = modelcode,
             data = list("x" = x,
                         "N" = N),
             cores=10, chains=10, iter = 100000, warmup = 500,
             control=list(adapt_delta=.95))
print(mod1)
mat <- as.array(mod1)
plot(mod1)
samps<-mat[,,1]      
head(samps)
dim(samps)
hist(samps[,1])
range(samps[,1])

# 90% probability
quantile(samps[,1],c(.05, .95))

# compute probabilities
sum((samps[,1]>=4.9 & samps[,1]<=8.9))/length(samps[,1])
sum((samps[,1]>=9 & samps[,1]<=13))/length(samps[,1])
sum((samps[,1]>=12 & samps[,1]<=16))/length(samps[,1])

###############################
# Late Period
###############################

# P(D|H1)
z<-(13-6.9)/(3.2/sqrt(9))
z
2*pnorm(abs(z),lower.tail = F) 

# P(D|H2)
z<-(13-11)/(3.2/sqrt(9))
z
2*pnorm(abs(z),lower.tail = F) 

# P(D|H2)
z<-(13-14)/(3.2/sqrt(9))
z
2*pnorm(abs(z),lower.tail = F) 

# create late period data
set.seed(222)
N <- 9
mu <- 13
sigma.sq <- 3.2^2
x <- rnorm(N, mu, sqrt(sigma.sq))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

mod2 <- stan(model_code = modelcode,
             data = list("x" = x,
                         "N" = N),
             cores=10, chains=10, iter = 100000, warmup = 500, 
             control=list(adapt_delta=.95))

print(mod2)
mat <- as.array(mod2)
plot(mod2)
samps<-mat[,,1]
range(samps[,1])

# 90% probability
quantile(samps[,1],c(.05, .95))

# compute probabilities
sum((samps[,1]>=4.9 & samps[,1]<=8.9))/length(samps[,1])
sum((samps[,1]>=9 & samps[,1]<=13))/length(samps[,1])
sum((samps[,1]>=12 & samps[,1]<=16))/length(samps[,1])