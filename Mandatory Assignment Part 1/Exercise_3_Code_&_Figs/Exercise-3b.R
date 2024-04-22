P_ci_1 = function(yi, p, tau_sqrd) {
  num = p*dnorm(yi, 0, 1)
  denomi = num + (1 - p) * dnorm(yi, 0, tau_sqrd + 1)
  P_ci_0 = num/denomi
  return(1-P_ci_0)
}
Expect = function(yi, tau_sqrd) {
  E = yi*tau_sqrd/(tau_sqrd+1)
  return(E)
}
beta_hat = function(yi, p, tau_sqrd) {
  PC1 = P_ci_1(yi, p, tau_sqrd)
  E = Expect(yi, tau_sqrd)
  beta = E*PC1
  return(beta)
}
#####Plotting######
y = seq(-5,5,length =501)
p=0.90
tau_sqrd = 80
beta_hat_yi=beta_hat(y,p,tau_sqrd)
plot(y,beta_hat_yi)