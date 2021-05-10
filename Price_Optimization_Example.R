library(ggplot2)

###########################################################################################
# NON ABBIAMBO DATI STORICI SU DOMANDA E PREZZO --> MODELLO BY DESIGNED ####
###########################################################################################

# DEFINIZIONE DI FUNZIONE DI DOMANDA 
# example of linear demand curve (first equation) 
demand = function(p, alpha = -40, beta = 500, sd = 10) {
  error = rnorm(length(p), sd = sd)
  q = p*alpha + beta + error
  return(q)
}

# DEFINIZIONE DI COSTO
unity.cost = 4 # production cost per unity

# Identificazione del prezzo di ottimo per il quale vendere un prodotto 
p.revenue_1 = -500/(2*(-40)) # estimated price for revenue
p.profit_1 = ((-40)*unity.cost - 500)/(2*(-40)) # estimated price for profit

# Identificazione funzioni di Revenue
model.revenue = function(p) p*(-40*p + 500) # Revenue with parameters defined by designed (chunck demand)
model.profit = function(p) (p - unity.cost)*(-40*p + 500) # price with parameters defined by designed

opt.revenue_1 = model.revenue(p.revenue_1) # Revenue with estimated optimum price
opt.profit_1 = model.profit(p.profit_1) # Profit with estimated optimum price


# CREAZIONE DI QUANTITà DOMANDATE, DI REVENUE E DI PROFIT IN BASE ALLA FUNZIONE DI DOMANDA 
set.seed(10)
hist.prices = rnorm(252, mean = 6, sd = .5) # random prices defined by the company
hist.demand = demand(hist.prices) # demand curve defined in the chunck above
hist.revenue = hist.prices*hist.demand # From the revenue equation
hist.cost = unity.cost*hist.demand
hist.profit = (hist.prices - unity.cost)*hist.demand # From the price equation


data = data.frame('Period' = seq(1,252),'Daily.Prices' = hist.prices,
                  'Daily.Demand' = hist.demand, 'Daily.Revenue' = hist.revenue,
                  'Daily.Cost' = hist.cost, 'Daily.Profit' = hist.profit)

#VISUALISE DEMAND FUNCTION
ggplot(data, aes(hist.prices, hist.demand)) +
  geom_point(shape=1) +
  geom_smooth(method='lm') +
  ggtitle('Demand Curve')

# VISUALISE PRICES
ggplot(data, aes(Period, Daily.Prices)) +
  geom_line(color = 4) +
  ggtitle('Historical Prices used for explotation')

# VISUALIZE COST, PROFIT & REVENUE
ggplot(data, aes(Period, Daily.Revenue, colour = 'Revenue')) +
  geom_line() +
  geom_line(aes(Period, Daily.Profit, colour = 'Profit')) +
  geom_line(aes(Period, Daily.Cost, colour = 'Cost')) +
  labs(title = 'Historical Performance', colour = '')





#############################################################################
# ABBIAMBO DATI STORICI SU DOMANDA E PREZZO --> MODELLO STIMATO DAI DATI ####
#############################################################################

# Creazione di una funzione di domanda da dati di prezzo e di quantità venduta   
library(stargazer)
model.fit = lm(hist.demand ~ hist.prices) # linear model for demand
stargazer(model.fit, type = 'text', header = FALSE) # output
# estimated parameters
beta = model.fit$coefficients[1]
alpha = model.fit$coefficients[2]  

#Identificazione del prezzo di ottimo per il quale vendere un prodotto 
p.revenue2 = -beta/(2*alpha) # estimated price for revenue
p.profit2 = (alpha*unity.cost - beta)/(2*alpha) # estimated price for profit

# Confronto del modello by designed con modello 
estimated.revenue = function(p) p*(model.fit$coefficients[2]*p + model.fit$coefficients[1])
estimated.profit = function(p) (p - unity.cost)*(model.fit$coefficients[2]*p + model.fit$coefficients[1])

opt.revenue_2 = estimated.revenue(p.revenue2) # Revenue with estimated optimum price
opt.profit_2 = estimated.profit(p.profit2) # Profit with estimated optimum price



####################################################
# COMPARAZIONE MODELLI ####
####################################################

# plot
df = data.frame(x1 = p.revenue2, x2 = p.profit2,
                y1 = opt.revenue_1, y2 = opt.profit_1, y3 = 0)

ggplot(data = data.frame(Price = 0)) +
  stat_function(fun = model.revenue, mapping = aes(x = Price, color = 'Model Revenue')) +
  stat_function(fun = model.profit, mapping = aes(x = Price, color = 'Model Profit')) +
  stat_function(fun = estimated.revenue, mapping = aes(x = Price, color = 'Estimated Revenue')) +
  stat_function(fun = estimated.profit, mapping = aes(x = Price, color = 'Estimated Profit')) +
  scale_x_continuous(limits = c(4, 11)) +
  labs(title = 'Model curves without noise') +
  ylab('Results') +
  scale_color_manual(name = "", values = c("Model Revenue" = 2, "Model Profit" = 3, "Estimated Revenue" = 4, "Estimated Profit" = 6)) +
  geom_segment(aes(x = x1, y = y1, xend = x1, yend = y3), data = df) +
  geom_segment(aes(x = x2, y = y2, xend = x2, yend = y3), data = df)


