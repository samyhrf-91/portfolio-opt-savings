library(ggplot2)

# ---- constants ----
r = 0.01
r_min = 0.005 # guaranteed rate of return (for assignment c) and d))

set.seed(NULL)
set.seed(221122)

# ---- simulated log returns for assignement a) ----
n <- 1000 # 1000 works also reasonably well
z <- matrix(rnorm(30*n), nrow = 30, ncol = n)
mu <- 0.03
sigma <- 0.2

# ---- market data log returns for assignement e) ----
indexdata <- read.csv(file = "indexdata.csv", sep=";", header=TRUE)
#indexdata <- read.table(file = "indexdata.txt", sep = "\t", header=TRUE)

date_days <- substr(x = indexdata$date, start = 1, stop = 2)

monthly_indexdata <- indexdata[date_days == "01", "fund_movement"]

monthly_returns <- numeric(length(monthly_indexdata)-1)
for (i in 1:length(monthly_returns)) {
  monthly_returns[i] <- monthly_indexdata[i+1]/monthly_indexdata[i]
}

# get mean and variance of historical returns:
l_returns <- log(monthly_returns)
mean(lreturns)*12 #we multiply by 12 since we consider yearly investments
sd(l_returns)*sqrt(12)

# get n yearly log returns by historical sampling
log_returns <- numeric(30*n) 
for (i in 1:length(log_returns)) {
  returns <- sample(monthly_returns, 12, replace=TRUE)
  log_returns[i] = sum(log(returns))
}

mu <- mean(log_returns)
sigma <- sd(log_returns)

# standardize log_returns - Z_k 
log_returns_standardised <- (log_returns - mu)/sigma


z <- matrix(log_returns_standardised, nrow = 30, ncol = n)

qqnorm(z) ## QQ-plot to check that we obtain a normal distribution for the yearly returns

# ---- assignement a) ----

## function for assignment a) and b)
generate <- function(mu=0.03, sigma=0.2, p = 0.5, c = 0.5) {
  g <- function(k) {p * (1 - c * (k-1)/30)}
  
  v <- function(k) {
    if (k == 0) {
      return(0)
    }
    return((v(k-1) + 1000) * (g(k) * exp(mu + sigma * z[k, ]) + (1-g(k)) * exp(r)) )
    
  }
  
  values <- v(30)
  return(values)
}


#### assignment a)

## plot different values in same plot
mus <- c(0.01, 0.03, 0.06, 0.12)
sigmas <- c(0.1, 0.2, 0.3, 0.4)

ps <- c(0, 0.25, 0.5, 0.75, 1)
cs <- c(0, 0.25, 0.5, 0.75, 1)

df <- data.frame(matrix(nrow = 0, ncol = 2)) # empty dataframe
colnames(df) <- c('v30', 'mu')
for (mu in mus) {
  temp <- data.frame(v30 = generate(mu = mu), mu = rep(paste(mu), n))
  df <- rbind(df, temp)
}
ggplot(df, aes(x = v30, col = mu)) +
  stat_ecdf() 
# sigma
df <- data.frame(matrix(nrow = 0, ncol = 2)) # empty dataframe
colnames(df) <- c('v30', 'sigma')
for (sigma in sigmas) {
  temp <- data.frame(v30 = generate(sigma = sigma), sigma = rep(paste(sigma), n))
  df <- rbind(df, temp)
}
ggplot(df, aes(x = v30, col = sigma)) +
  stat_ecdf() 
# p
df <- data.frame(matrix(nrow = 0, ncol = 2)) # empty dataframe
colnames(df) <- c('v30', 'p')
for (p in ps) {
  temp <- data.frame(v30 = generate(p = p), p = rep(paste(p), n))
  df <- rbind(df, temp)
}
ggplot(df, aes(x = v30, col = p)) +
  stat_ecdf() 
# c
df <- data.frame(matrix(nrow = 0, ncol = 2)) # empty dataframe
colnames(df) <- c('v30', 'c')
for (c in cs) {
  temp <- data.frame(v30 = generate(c = c), c = rep(paste(c), n))
  df <- rbind(df, temp)
}
ggplot(df, aes(x = v30, col = c)) +
  stat_ecdf() 

## vary everything at once
# for (mu in mus) {
#   for (sigma in sigmas) {
#     for (p in ps) {
#       for (c in cs) {
#         values <- generate(p, c, mu, sigma)
#         
#         y <- ecdf(values)
#         plot(y)
#       }
#     }
#   }
# }


#### assigment b)
# IDEA:
# Find (p,c) such that q := F^-1(0.01) is as large as possible
# then P(v30 <= q) = 0.01 
# --> the probability that our portfolio is worth less than q is one percent
# --> we are very certain to have more than u at the end of the time (-> conservative portfolio)
# Alternative:
# Find (p,c) such that E[V_30] is as large as possible
# then (on average) we have the most money at the end of the 30 years (-> more risky portfolio)
# Alternative:
# compute the Loss, which is:
# X = V_1 - V_0 * R
# X = winnings = v_30 - discounted(1000 euro every year) * risk_free_rate_over_30_years (=:R0)
# L = -X/R0 = discounted(1000 euros every year) - v_30 / R0
# --> this leads to same conclusion as when choosing the mean as optimality
# TODO: use ES as optimality principle ? -> ask on Tuesday, what we should use
#       in case we have to use it: write function to compute ES, given the loss and level p. 

# max E[v30] w.r.t. F^-1(0.01) >= 30_000

# ---- assignement b) ----

# calculations for computing the loss:
# risk free rate over 30 years:
R0 <- exp(r*30)
# discount factor for having money in the beginning of year (i-1)
discount_factors = c()
for (i in 1:30) {
  discount_factors <- c(discount_factors, exp(-(i-1)*r))
}
# discounted value of the 1000 euros added to the portfolio each year
v0 <- sum(1000*discount_factors)


ExpShortfall <- function(l, p) {
  n <- length(l)
  l <- sort(l, decreasing = TRUE)
  out <- sum(l[1:floor(n*p)])/n 
  if (floor(n*p) != n*p) {
    out <- out + (p - floor(n*p)/n)*l[floor(n*p)+1]
  }
  return(out/p)
}


ps <- seq(0, 1, by=0.02) #list of p 
cs <- seq(0, 1, by=0.02) #list of c

ps <- c(seq(0.5, 1, by=0.01))
cs <- c(0, seq(0.5, 1, by=0.01))

df = data.frame(matrix(nrow = 0, ncol=5))
colnames(df) <- c('p', 'c', 'q', 'mean', 'sd')
for (p in ps) {
  for (c in cs) {
    v30 <- generate(mu=mu, sigma=sigma, p = p, c = c)
    loss = v0 - v30/R0 # -X/R0
    ESp = ExpShortfall(loss, 0.01)
    temp <- data.frame(p=p, c=c, q=quantile(v30, 0.01), mean=mean(v30), sd=sd(v30),
                       VaR = quantile(loss, 0.99), ES = ESp,
                       frac = mean(v30)/ESp)
    df <- rbind(df, temp)
  }
}


# crit = E[] / ESp as large as possible

## plot results as heatmap
ggplot(df, aes(x=p, y=c, fill=q)) +
  geom_tile()
ggplot(df, aes(x=p, y=c, fill=mean)) +
  geom_tile()
ggplot(df[df$p > 0,], aes(x=p, y=c, fill=frac)) +
  geom_tile()
ggplot(df, aes(x=p, y=c, fill=VaR)) +
  geom_tile()
ggplot(df, aes(x=p, y=c, fill=ES)) +
  geom_tile()
ggplot(df[df$p > 0 & df$q >= v0 & df$mean >= 0.6*max(df$mean),], aes(x=p, y=c, fill=frac)) +
  geom_tile() #focus on high values of v30 with 2 criteria
ggplot(df[df$p > 0 & df$mean >= 0.6*max(df$mean),], aes(x=p, y=c, fill=frac)) +
  geom_tile()

df_restr <- df[df$p > 0 & df$mean >= 0.6*max(df$mean),]
ggplot(df_restr, aes(x=p, y=c, fill=frac)) +
  geom_tile()
head(df_restr[order(df_restr$frac, decreasing = TRUE),]) #return the best optimiser

# Results for n=100_000 
# (p, c) = (0.62, 0.84)

# Results for n=10_000
# (p, c) = (0.63, 0.86) # 6.42 # 27431 # 42746
# (p, c) = (0.62, 0.86) # 6.48 # 27585 # 42677
# (p, c) = (0.64, 0.90) # 
# (p, c) = (0.65, 0.92) # 6.66 # 27947 # 42246
# (p, c) = (0.75, 0.96) # 6.55 # 27607 # 43071
# (p, c) = (0.71, 0.96) # 6.50 # 27763 # 42523
# (p, c) = (0.76, 0.95) # 6.52 # 27390 # 43423
# (p, c) = (0.73, 0.94) # 6.64 # 27749 # 43151
# (p, c) = (0.73, 0.94) # 6.58 # 27554 # 43300
# (p, c) = (0.64, 0.88) # 6.95 # 27899 # 42851
# (p, c) = (0.70, 0.94) # 6.74 # 27698 # 42626
scatter_df <- data.frame(p=c(0.63, 0.62, 0.64, 0.65, 0.75, 0.71, 0.76, 0.73, 0.64, 0.7), 
                         c=c(0.86, 0.86, 0.90, 0.92, 0.96, 0.96, 0.95, 0.94, 0.88, 0.94))
ggplot(scatter_df, aes(x=p, y=c)) +
  geom_point()

mean(scatter_df$p)
mean(scatter_df$c)
mean(c(27431, 27585, 27947, 27604, 27763, 27390, 27749, 27554, 27899, 27698)) # mean quantiles
mean(c(42746, 42677, 42246, 43971, 42523, 43423, 43151, 43300, 42851, 42626)) # mean of means

# Results for n=1_000 
# (p, c) = (0.60, 0.86) # 7.51 # 28187 # 42380 (with set.seed(221122))
# (p, c) = (0.56, 0.88)
# (p, c) = (0.68, 0.82)

# report mean and quantile for best result
v30_b <- generate(p=0.683, c=0.917)
# v30_b <- generate(p=0.60, c=0.86) # optimum for random data
v30_b <- generate(p=1, c=0.78) # optimum for market data

print(paste('mean : ', round(mean(v30_b), 4), '; sd : ', round(sd(v30_b), 4), 
            '; 1%-quantile : ', round(quantile(v30_b, 0.01), 4)) )
# histogram for b)
df <- data.frame(
  values=c(v30_b)
)
ggplot(df, aes(x=values)) + 
  geom_histogram(binwidth = 1000, alpha = .5, position="identity")

# ---- assignement c) ----

#### assignment c) 
# new calculation for v(k) with guaranteed value G(k)
generate <- function(mu, sigma) {
  G <- function(k) {
    if (k == 0){
      return(0)
    }
    
    R <- 0
    for (i in 1:k) {
      R <- R + exp(r_min*i)
    }
    return(1000 * R)
  }
  
  v <- function(k) {
    if (k == 0) {
      return(0)
    }
    guaranteed_value <- G(k)
    
    return((v(k-1) + 1000 - guaranteed_value * exp(-r)) * exp(mu + sigma * z[k, ]) + guaranteed_value)
    # I used: guaranteed_value * exp(-r) * exp(r) = G(k+1)
  }
  
  values <- v(30)
  return(values)
}
# report mean and quantile
v30_c <- generate(mu=mu, sigma=sigma)
print(paste('mean : ', round(mean(v30_c), 4), '; sd : ', round(sd(v30_c), 4), 
            '; 1%-quantile : ', round(quantile(v30_c, 0.01), 4)) )
# histogram for c)
df <- data.frame(
  values=c(v30_c)
)
ggplot(df, aes(x=values)) + 
  geom_histogram(binwidth = 1000, alpha = .5, color="darkcyan", fill="cyan", position="identity")

# ---- assignement d) ----

#### assignment d)
leverage <- 1/0.5
generate <- function(mu, sigma) {
  G <- function(k) { #computation of the guaranteed value
    if (k == 0){
      return(0)
    }
    
    R <- 0
    for (i in 1:k) {
      R <- R + exp(r_min*i)
    }
    return(1000 * R)
  }
  
  
  v <- function(k) {
    if (k == 0) {
      return(numeric(n))
    }
    
    current_value <- v(k-1)
    guaranteed_value_next_year <- G(k)
    guaranteed_value_current_year <- G(k-1)
    
    next_value <- numeric(length(n))
    for (i in 1:n) {
      if (current_value[i] < guaranteed_value_current_year) {
        print(paste("The portfolio fell behind the guaranteed value at (k, i) = (", k, ", ", i, ")"))
        print(paste("Portfolio: ", current_value[i], " guaranteed: ", guaranteed_value_current_year, " difference: ", guaranteed_value_current_year - current_value[i] )) 
        
        next_value[i] <- (current_value[i] + 1000)*exp(r) #only investment in the bond
         
      } else {
        next_value[i] <- leverage * (current_value[i] + 1000 - guaranteed_value_next_year * exp(-r)) * exp(mu + sigma * z[k, i]) + leverage * guaranteed_value_next_year - (current_value[i] + 1000) * exp(r)
      }
    }
    return(next_value)
  }
  
  values <- v(30)
  return(values)
}
# report mean and quantile
v30_d <- generate(mu=mu, sigma=sigma)
print(paste('mean : ', round(mean(v30_d), 4), '; sd : ', round(sd(v30_d), 4), 
            '; 1%-quantile : ', round(quantile(v30_d, 0.01), 4)) )

# ---- comparison ----

# histogram for comparison of c) and d)
df <- data.frame(
  kind=factor(rep(c("no leverage", "leverage = 2"), each=n)),
  values=c(v30_c, v30_d)
)
ggplot(df, aes(x=values, color=kind, fill=kind)) + 
  geom_histogram(binwidth = 1000, alpha = .5, position="identity")

# histogram for comparison of b), c) and d)
df <- data.frame(
  kind=factor(rep(c("no leverage", "leverage = 2", "assignment b)"), each=n)),
  values=c(v30_c, v30_d, v30_b)
)
ggplot(df, aes(x=values, color=kind, fill=kind)) + 
  geom_histogram(binwidth = 1000, alpha = .5, position="identity")