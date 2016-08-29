#Loading required libraries
library(FinTS)
library(rstan)

#Read data
data(q.jnj)
q.jnjts=as.ts(q.jnj)

#create variables
jtime=time(q.jnj)
jtime = jtime-1960
jseas = cycle(q.jnj)
jseas = as.factor(jseas)
jdata = coredata(q.jnj)

#create separate dataframe
jnjData = data.frame(jdata, jtime, jseas)

#create dummy variables
modelData = model.matrix(jdata ~ 0 + jtime + jseas, data=jnjData)
str(modelData)

#Fit normal Reression model
regQuad = lm(jdata ~ 0 + jtime + I(jtime^2) + jseas)
summary(regQuad)

##################### Quadratic Equation ###########################################

#create dataset which we want to pass to stan program
dat<-list(n=84,time=jnjData$jtime, time_sq=(jnjData$jtime)^2, sea1=modelData[,2], sea2=modelData[,3], sea3=modelData[,4], sea4=modelData[,5], y=jnjData$jdata)

#Call stan program
fit_quad<-stan(file='F:\\Nilam\\MSBA_Degree\\Spring2016\\MA611\\Project\\Bayesian-Stan\\New Code\\stan_quad.stan',data=dat,iter=1000,chains=4)

#View Output
fit_quad

#Stucture of output
str(fit_quad)

#Extract beta parameter
beta_est1 = extract(fit_quad, 'beta1')
beta1_est = mean(beta_est1$beta1)

beta_est2 = extract(fit_quad, 'beta2')
beta2_est = mean(beta_est2$beta2)

beta_est3 = extract(fit_quad, 'beta3')
beta3_est = mean(beta_est3$beta3)

beta_est4 = extract(fit_quad, 'beta4')
beta4_est = mean(beta_est4$beta4)

beta_est5 = extract(fit_quad, 'beta5')
beta5_est = mean(beta_est5$beta5)

beta_est6 = extract(fit_quad, 'beta6')
beta6_est = mean(beta_est6$beta6)

###### Checking Convergence #############################################
#Trace Plots
stan_trace(fit_quad, pars=c("beta1"), inc_warmup = TRUE)
stan_trace(fit_quad, pars=c("beta2"), inc_warmup = TRUE)
stan_trace(fit_quad, pars=c("beta3"), inc_warmup = TRUE)
stan_trace(fit_quad, pars=c("beta4"), inc_warmup = TRUE)
stan_trace(fit_quad, pars=c("beta5"), inc_warmup = TRUE)
stan_trace(fit_quad, pars=c("beta6"), inc_warmup = TRUE)
stan_trace(fit_quad, pars=c("sigma"), inc_warmup = TRUE)

#Autocorrelation Plots
stan_ac(fit_quad, pars=c("beta1"), inc_warmup = TRUE)
stan_ac(fit_quad, pars=c("beta2"), inc_warmup = TRUE)
stan_ac(fit_quad, pars=c("beta3"), inc_warmup = TRUE)
stan_ac(fit_quad, pars=c("beta4"), inc_warmup = TRUE)
stan_ac(fit_quad, pars=c("beta5"), inc_warmup = TRUE)
stan_ac(fit_quad, pars=c("beta6"), inc_warmup = TRUE)
stan_ac(fit_quad, pars=c("sigma"), inc_warmup = TRUE)

stan_ac(fit_quad, pars=c("beta1","beta2","beta3","beta4","beta5","beta6"), inc_warmup = TRUE,nrow=3, ncol=3)

#Density Plots
par(3:3)
stan_dens(fit_quad, pars=c("beta1","beta2","beta3","beta4","beta5","beta6"), inc_warmup = TRUE,nrow=3, ncol=3)
stan_dens(fit_quad, pars=c("beta1"), inc_warmup = TRUE)
stan_dens(fit_quad, pars=c("beta2"), inc_warmup = TRUE)
stan_dens(fit_quad, pars=c("beta3"), inc_warmup = TRUE)
stan_dens(fit_quad, pars=c("beta4"), inc_warmup = TRUE)
stan_dens(fit_quad, pars=c("beta5"), inc_warmup = TRUE)
stan_dens(fit_quad, pars=c("beta6"), inc_warmup = TRUE)
stan_dens(fit_quad, pars=c("sigma"), inc_warmup = TRUE)

######################### Extracting posterior #############################
# Extract Entire posterior (for all parameters) - 3 chains
posterior <- extract(fit_quad, 'mu',inc_warmup = FALSE, permute = FALSE)
str(posterior)

#declare vector
mu_est=rep(NA, 84)

#calculate average mu esimate for all iterations and all chains
for( i in 1:84){
  mu_est[i] = mean(posterior[1:500,1:4,i])
}

#calculate model residuls
res = jdata - (mu_est)

#calculate RMSE
rmse  = sqrt(sum(res^2)/length(res))

# Plot residuals
plot(mu_est,res, main="Residuals Vs Fitted")

#Normal Q-Q plot
qqnorm(res)
qqline(res)

#Actual vs. Predicted plot
matplot(cbind(jdata,mu_est),type='l',lty=1,lwd=c(2,3),col=c(1,2))

######### Forecasting ###################################
predTime = c(21.00, 21.25, 21.50, 21.75)
predSeas1 = c(1, 0, 0, 0)
predSeas2 = c(0, 1, 0, 0)
predSeas3 = c(0, 0, 1, 0)
predSeas4 = c(0, 0, 0, 1)

#Defining Vector
predData = rep(NA,4)

#Model Equation
for(i in 1:length(predTime)){
  predData[i] = beta1_est*predTime[i] + beta2_est*((predTime[i])^2) + beta3_est*predSeas1[i] + 
    beta4_est*predSeas2[i] + beta5_est*predSeas3[i] + beta6_est*predSeas4[i];
}

