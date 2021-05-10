library(ggplot2)
library(dplyr)
library(magrittr)
library(reshape2)

# https://www.r-bloggers.com/pricing-optimization-how-to-find-the-price-that-maximizes-your-profit/
# https://insightr.wordpress.com/2018/06/03/different-demand-functions-and-optimal-price-estimation-in-r/?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com


# HISTORICAL DATA ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Tutti questi dati storici sono a disposizione del bussiness come tipo di consuntivato

# 01_PRICE ####
set.seed(10)
hist.prices <- rnorm(252, mean = 6, sd = .5) # random prices defined by the company
hist.prices <- seq(1, 100) # incremental prices

# 02_COST ####
unity.cost = 4 # production cost per unity

# 03_DEMAND ####
# LINEAR demand curve - example 
demand_linear = function(p, alpha, beta, sd) {
  error = rnorm(length(p), sd = sd)
  q = p*alpha + beta + error
  return(q)
}
# EXPONENTIAL demand curve - example
demand_espon = function(p, alpha, beta, sd){
  error = rnorm(length(p), sd = sd)
  q = exp(alpha*log(p)+beta)
  return(q)
}
# LOGISTIC demand curve - example
demand_logistic = function(p, c, alpha, p0, sd){
  error = rnorm(length(p), sd = sd)
  q = c/(1+exp(-alpha*(p-p0)))
  return(q)
}

dem1 = demand_linear(hist.prices, -1, 100, 10)
dem2 = demand_espon(hist.prices, -0.5, 4.5, 10)
dem3 = demand_logistic(hist.prices, 100, -0.2, 50, 10)

df = data.frame('Prices' = hist.prices, 'Linear' = dem1, 'Esponential' = dem2, 'Logistic' = dem3)
df.plot = melt(df, id = 'Prices') %>% set_colnames(c('Prices', 'Model', 'Demand'))

ggplot(df.plot) + aes(x = Prices, y = Demand) +
  geom_line(color = 'blue', alpha = .6, lwd = 1) +
  facet_grid(~Model)

# 04_REVENUE ####
rev1 = hist.prices*dem1 # From the revenue equation
rev2 = hist.prices*dem2 
rev3 = hist.prices*dem3 

# 05_PROFIT ####
pro1 = (hist.prices - unity.cost)*dem1 # From the price equation
pro2 = (hist.prices - unity.cost)*dem2 # From the price equation
pro3 = (hist.prices - unity.cost)*dem3 # From the price equation



# STATISTICAL INFERENCE -------------------------------------------------------------------------------------------------------------------------------------------------
# linear Demand - Fit of the demand model
dem_model1 = lm(dem1 ~ hist.prices)
pro.model1 = model1$fitted.values*(hist.prices - unity.cost)
beta1 = dem_model1$coefficients[1]
alpha1 = dem_model1$coefficients[2]  

df.model1 = data.frame('Prices' = hist.prices, 'Demand' = dem1,
                       'Profit.fitted' = pro.model1, 'Profit' = pro1)
ggplot(select(df.model1, Prices, Demand)) + aes(x = Prices, y = Demand) + geom_point() + geom_smooth(method = lm)


# Exponential Demand - Fit of the demand model
dem_model2 = lm(log(dem2)~log(hist.prices))
pro.model2 = exp(dem_model2$fitted.values)*(hist.prices - unity.cost)

# Logistic Demand - Fit of the demand model



# OPTIMIZATION PROCESS ---------------------------------------------------------------------------------------------------------------------------------------------------

# Linear Demand
pro_optimum = (alpha1*unity.cost - beta1)/(2*alpha1)

ggplot(select(df.linear, Prices, Profit)) + aes(x = Prices, y = Profit) +
  geom_point() + geom_vline(xintercept = p.max.profit, lty = 2) +
  geom_line(data = df.linear, aes(x = Prices, y = Profit.fitted), color = 'blue')

ggplot(select(df.linear, Prices, Profit)) + aes(x = Prices, y = Profit) +
  geom_point() + geom_vline(xintercept = hist.prices.max.profit, lty = 2) +
  geom_line(data = df.linear, aes(x = Prices, y = Profit.fitted), color = 'blue')




