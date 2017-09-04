#Set working directory
setwd("E:/Acads/UT Dallas/Econometrics/Moran Blueshtein/Homeworks/HW2")

#Load libraries
library(foreign)
library(ggplot2)

#Load data
data = read.dta('capm4.dta')

#Describe data
head(data)
summary(data)

# CAPM econometric model is (rj - rf) = aj + Bj(rm - rf) +e
# Let's calculate (rj - rf) and (rm - rf) for every date and firm. And save it in a dfferent dataframe named df
df = data
df['dis'] = df['dis'] - df['riskfree']
df['ge'] = df['ge'] - df['riskfree']
df['gm'] = df['gm'] - df['riskfree']
df['ibm'] = df['ibm'] - df['riskfree']
df['msft'] = df['msft'] - df['riskfree']
df['xom'] = df['xom'] - df['riskfree']
df['mkt'] = df['mkt'] - df['riskfree']

# Run regression for every firm
lm_dis = lm(dis~mkt, data = df)
lm_ge = lm(ge~mkt, data = df)
lm_gm = lm(gm~mkt, data = df)
lm_ibm = lm(ibm~mkt, data = df)
lm_msft = lm(msft~mkt, data = df)
lm_xom = lm(xom~mkt, data = df)

coeff <- c(lm_dis$coefficients,lm_ge$coefficients,lm_gm$coefficients,lm_ibm$coefficients,lm_msft$coefficients,lm_xom$coefficients)

#Plotting Microsoft
plot(df$msft~df$mkt, data = df, xlab = "Rate of Return (Microsoft) - Risk Free Rate", ylab = "Rate of Return (Market) - Risk Free Rate")  
abline(lm_msft, col = 'blue')


# Run regression for every firm elimnatig aj
lm_dis1 = lm(dis~mkt - 1, data = df)
lm_ge1 = lm(ge~mkt - 1, data = df)
lm_gm1 = lm(gm~mkt - 1, data = df)
lm_ibm1 = lm(ibm~mkt - 1, data = df)
lm_msft1 = lm(msft~mkt - 1, data = df)
lm_xom1 = lm(xom~mkt - 1, data = df)