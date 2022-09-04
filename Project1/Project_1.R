#Data downloaded from: 
#https://finance.yahoo.com/ for index NASDAQ100, and Apple and Tesla companies.
#Time period: 10.6.2021. - 10.6.2022. (253 trading days)
Apple <- read.csv("AAPL.csv")
Tesla <- read.csv("TSLA.csv")
Nasdaq <- read.csv("^NDX.csv")

#Closing prices:
AC <- Apple$Close
TC <- Tesla$Close
NC <- Nasdaq$Close

#Returns
n = length(AC)   #number of trading days
R_AC = (AC[2:n]-AC[1:(n-1)])/AC[1:(n-1)]   #Apple 
R_TC = (TC[2:n]-TC[1:(n-1)])/TC[1:(n-1)]   #Tesla
R_NC = (NC[2:n]-NC[1:(n-1)])/NC[1:(n-1)]   #Nasdaq

#Graphs
#Tesla:
plot(R_TC, type="l", col="green", xlab = "Days", ylab="Return", lwd = 2)
legend("topleft", inset = 0.02, col=c("green", "CornflowerBlue"), legend=c('Tesla', 'Nasdaq100'), cex = 0.5, lwd = c(2,2))
lines(R_NC, col="CornflowerBlue", lwd = 2)
#Observations from graph:
# - Tesla has higher returns compared to Nasdaq100
# - We can see periods when the prices of index fell, but Tesla's rose (indicating somewhat negative correlation).

#Apple:
plot(R_AC, type="l", col="red", xlab = "Days", ylab="Return", lwd = 2)
legend("topleft", inset = 0.02, col=c("red", "CornflowerBlue"), legend=c('Apple', 'Nasdaq100'), cex = 0.5, lwd = c(2,2))
lines(R_NC, col="CornflowerBlue", lwd = 2)
#Observations from graph:
# - Apple also realizes higher returns compared to Nasdaq100
# - Apple returns seem to be better (positively) correlated to the returns of whole index

#beta 
beta_TC <- cov(R_NC, R_TC)/var(R_NC)
beta_AC <- cov(R_NC, R_AC)/var(R_NC)
beta_TC   #1.594561
beta_AC   #0.9560856
#When choosing which company to invest in:
#Lower beta value suggests that it is better to invest in Apple (not necessarily only Apple, but to put higher weights on Apple compared to Tesla), since it is more "resistant" to market changes.
#This further confirms assumptions we made from graphs.

#Realized return
mean(R_TC)   #0.001352584
mean(R_AC)   #0.0006513606
#Average (realized) return is higher for Tesla - from this it seems better to invest in Tesla.
#Keeping in mind Tesla's beta value, this conclusion makes sense - since Tesla is a riskier stock, it can long-term realize higher returns.

#CAPM model
r_f = 0.02/12 #arbitrarily chosen risk-free rate
r_f+beta_TC*(mean(R_NC)-r_f)   #Tesla: -0.001602129
r_f+beta_AC*(mean(R_NC)-r_f)   #Apple: -0.0002932759
#If we interpret CAPM returns as returns required for a profitable investment, we can see that both 
#Apple and Tesla are profitable options (since their realized return is higher than CAPM).

#Sharpe ratio
S_TC = mean(R_TC-r_f)/sd(R_TC-r_f)   
S_AC = mean(R_AC-r_f)/sd(R_AC-r_f)   
S_TC   #-0.008395956
S_AC   #-0.05624192
#Sharpe ratio mesures expected return of an investment compared to its riskiness (if for measure of risk we look at volatility).
#From these results, it seems better to invest in Tesla (higher Sharpe ratio).

#Treynor ratio
T_TC=mean(R_TC-r_f)/beta_TC   
T_AC=mean(R_AC-r_f)/beta_AC   
T_TC   #-0.0001969715
T_AC   #-0.001061941
#The Treynor ratio is a performance metric for determining how much excess return was generated for each unit of risk taken on by a portfolio.
#Risk in the Treynor ratio refers to systematic risk as measured by a portfolio's beta.
#Looking at this metric, it seems more profitable to invest in Tesla.

#Jensen's alpha:
J_TC = mean(R_TC)-r_f-beta_TC*(mean(R_NC)-r_f)
J_AC = mean(R_AC)-r_f-beta_AC*(mean(R_NC)-r_f)
J_TC   #0.002954712
J_AC   #0.0009446365
#Jensen's alpha is a risk-adjusted performance measure that represents the average return on an investment, above or below that predicted by the CAPM, given the investment's beta and the average market return. 
#Similar to previous performance measures, higher alpha is more desireable, therefore we conclude it is better to invest in Tesla.

#Putting it all in a table
table = matrix(c(beta_AC, beta_TC, S_AC, S_TC, T_AC, T_TC, J_AC, J_TC),ncol=2,byrow=TRUE)
colnames(table) <- c("Apple", "Tesla")
rownames(table) <- c("beta", "Sharpe", "Treynor", "Jensen")
table
#                Apple         Tesla
#beta     0.9560856197  1.5945611790
#Sharpe  -0.0562419173 -0.0083959563
#Treynor -0.0010619405 -0.0001969715
#Jensen   0.0009446365  0.0029547123

#log-returns
log_R_TC <- log(TC[2 : n]) - log(TC[1 : n-1])
log_R_AC <- log(AC[2 : n]) - log(AC[1 : n-1])

#Graphs
plot(log_R_TC, type="l", col="green", xlab = "Days", ylab="log-return", lwd = 2)
legend("topleft", inset = 0.02, col=c("red", "green"), legend=c('Apple', 'Tesla'), cex = 0.5, lwd = c(2,2))
lines(log_R_AC, col = "red", lwd = 2)

#Checking normality of data distribution for Apple:
hist(log_R_AC, main = "Apple Histogram", xlab = "log-return", prob = TRUE)
curve(dnorm(x, mean=mean(log_R_AC), sd=sd(log_R_AC)), col = "red", add=TRUE)
qqnorm(log_R_AC, main = "Apple Normal Q-Q Plot")
qqline(log_R_AC, col = "red")

library(nortest)
lillie.test(log_R_AC)      #p-value = 0.003698
shapiro.test(log_R_AC)     #p-value = 0.02882
install.pACkages('tseries')
library(tseries)
jarque.bera.test(log_R_AC) #p-value = 0.006622
#With confidence level 5% we reject the nul-hypothesis in favour of alternative,
#i.e. the data does not come from a normal distribution (which was expected because of "fat" tails). 

#Checking normality of data distribution for Tesla:
hist(log_R_TC, main = "Tesla Histogram", ylim = c(0,12), xlim = c(-0.2, 0.2), xlab = "log-return", prob = TRUE)
curve(dnorm(x, mean=mean(log_R_TC), sd=sd(log_R_TC)), col = "green", add=TRUE)
qqnorm(log_R_TC, main = "Tesla Normal Q-Q Plot")
qqline(log_R_TC, col = "green")

library(nortest)
lillie.test(log_R_TC)      #p-value = 6.801e-06
shapiro.test(log_R_TC)     #p-value = 5.072e-05
install.pACkages('tseries')
library(tseries)
jarque.bera.test(log_R_TC) #p-value = 1.174e-07
#With all standard confidence levels (1, 5, 10%) we reject the nul-hypotesis in favour of alternative; 
#i.e. the data does not come from a normal distribution (which was expected because of "fat" tails).

#Since data for Apple and Tesla are not from a normal distribution, we shall use Wilcoxon test for equivalence of expectations:
wilcox.test(log_R_AC, log_R_TC)   #p-value = 0.4331
#Because of this p-value, we can not reject the nul-hypothesis.
#Therefore, expected log-returns for the two companies are roughly the same (difference is at 4th point decimal)

mean(log_R_AC)   #0.0004887675
mean(log_R_TC)   #0.0006522722

#Testing equivalence of variances for data that doesn't follow normal distribution:
bartlett.test(x = c(log_R_TC, log_R_AC), g  = c(rep(1, length(log_R_TC)), rep(2, length(log_R_AC))))
#p-value < 2.2e-16
#From graphs of log-returns we suspect that Apple and Tesla do not have same variances, formally:
#given this p-value, on all standard confidence levels (1, 5, 10%) we can conclude that Apple and Tesla do not have same variances for the observed time period


#Conclusion: 
#From this analysis, we established that both Apple and Tesla (individually) outperform Nasdaq100 with returns. 
#If we want to construct portfolio similar to Nasdaq100 but with higher returns we should put more weights on Apple and Tesla.
#Between the two, Tesla is more volatile (thus riskier) but earns more long-term (our observed period was 1 year).

