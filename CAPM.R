#This code is working example of problem 2.10 from the book.
#The capital asset pricing model (CAPM) is an important model in the field of finance.
#It explains variations in the rate of return on a security as a function of the rate of
#return on a portfolio consisting of all publicly traded stocks, which is called the
#market portfolio. Generally the rate of return on any investment is measured relative
#to its opportunity cost, which is the return on a risk free asset. The resulting difference
#is called the risk premium, since it is the reward or punishment for making a risky
#investment. TheCAPM says that the risk premium on security j is proportional to the
#risk premium on the market portfolio. That is,
#rj  rf ¼ bjðrm  rf Þ,
#where rj and rf are the returns to security j and the risk-free rate, respectively, rm is
#the return on the market portfolio, and bj is the jth security’s ‘‘beta’’ value. A stock’s
#beta is important to investors since it reveals the stock’s volatility. It measures the
#sensitivity of security j’s return to variation in the whole stock market. As such,
#values of beta less than 1 indicate that the stock is ‘‘defensive’’ since its variation is less than the market’s. A beta greater than 1 indicates an ‘‘aggressive stock.’’
#Investors usually want an estimate of a stock’s beta before purchasing it. The CAPM
#model shown above is the ‘‘economic model’’ in this case. The ‘‘econometric
#model’’ is obtained by including an intercept in the model (even though theory says it
#should be zero) and an error term,
#rj  rf ¼ aj þ bjðrm  rfÞ þ e
#(a) Explain why the econometric model above is a simple regression model like
#those discussed in this chapter.
#(b) In the data file capm4.dat are data on the monthly returns of six firms (Microsoft,
#GE, GM, IBM, Disney, and Mobil-Exxon), the rate of return on the market
#portfolio (MKT ), and the rate of return on the risk free asset (RISKFREE). The
#132 observations cover January 1998 to December 2008. Estimate the CAPM
#model for each firm, and comment on their estimated beta values. Which firm
#appears most aggressive? Which firm appears most defensive?
#(c) Finance theory says that the intercept parameter aj should be zero. Does this
#seem correct given your estimates? For the Microsoft stock, plot the fitted
#regression line along with the data scatter.
#(d) Estimate the model for each firm under the assumption that aj ¼ 0. Do the
#estimates of the beta values change much?

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
