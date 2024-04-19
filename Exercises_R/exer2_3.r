#Exercise 2.3
tobs = c(6,6,6,6,7,9,10,10,11,13,16,17,19,20,22,23,25,32,32,34,35,
         1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
w = c(0,1,1,1,1,0,0,1,0,1,1,0,0,0,1,1,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
x = c(rep(1,21),rep(0,21))
X = cbind(1,x)
print(x)
loglik = function(param,tobs=tobs,w=w,x=x)
{
	alpha = exp(param[1])
	beta = param[2:3]
	Lam = tobs^alpha
	lam = alpha*tobs^(alpha-1)
	mu = Lam*exp(beta[1]+beta[2]*x)
	sum(w*log(mu)-mu+w*log(lam/Lam))
}

minusloglik = function(param,tobs=tobs,w=w,x=x)
{
	-loglik(param,tobs,w,x)
}

l1func = function(param,tobs=tobs,w=w,x=x)
{
	alpha = exp(param[1])
	beta = param[2:3]
	Lam = tobs^alpha
	lam = alpha*tobs^(alpha-1)
	mu = Lam*exp(beta[1]+beta[2]*x)
	matrix(c(sum((w-mu)*log(tobs)+w/alpha),sum(w-mu),sum((w-mu)*x)),ncol=1)
}

l2func = function(param,tobs=tobs,w=w,x=x)
{
	alpha = exp(param[1])
	beta = param[2:3]
	Lam = tobs^alpha
	lam = alpha*tobs^(alpha-1)
	mu = Lam*exp(beta[1]+beta[2]*x)
	matrix(c(-sum(mu*log(tobs)^2+w/alpha^2),-sum(mu*log(tobs)),-sum(mu*x*log(tobs)),
             -sum(mu*log(tobs)), -sum(mu),-sum(mu*x),
             -sum(mu*x*log(tobs)),-sum(mu*x),-sum(mu*x^2)),ncol=3)
}

#Newton-Raphson
theta = c(1,0,0)
l = loglik(theta,tobs,w,x)
g = l1func(theta,tobs,w,x)
h = l2func(theta,tobs,w,x)
eps = 0.00001
while(sum(abs(g))>eps)
{
  theta = theta - solve(h,g)
  l = loglik(theta,tobs,w,x)
  g = l1func(theta,tobs,w,x)
  h = l2func(theta,tobs,w,x)
  show(c(theta,l))
}

show(c(exp(theta[1]),theta[2:3]))
Sigma = -solve(h)
show(Sigma)
show(Sigma[2,3]/(sqrt(Sigma[2,2]*Sigma[3,3])))


res.opt = optim(c(1,0,0),minusloglik,tobs=tobs,w=w,x=x,method="BFGS",control=list(maxit=1000),hessian=T)
show(c(exp(res.opt$par[1]),res.opt$par[2:3]))
show(solve(-res.opt$hessian))

#Gauss-Seidel
minusl.alpha = function(lalpha,beta,tobs=tobs,w=w,x=x)
{
  par = c(lalpha,beta)
  l = minusloglik(par,tobs,w,x)
  show(c(par,l))
  l
}
minusl.beta0 = function(beta0,lalpha,beta1,tobs=tobs,w=w,x=x)
{
  par = c(lalpha,beta0,beta1)
  minusloglik(par,tobs,w,x)
}
minusl.beta1 = function(beta1,lalpha,beta0,tobs=tobs,w=w,x=x)
{
  par = c(lalpha,beta0,beta1)
  minusloglik(par,tobs,w,x)
}

theta = c(1,0,0)
for(i in 1:100)
{
  res = optimize(minusl.alpha,interval=c(-10,10),beta=theta[2:3],tobs=tobs,w=w,x=x)
  theta[1] = res$min
  res = optimize(minusl.beta0,interval=c(-10,10),lalpha=theta[1],beta1=theta[3],tobs=tobs,w=w,x=x)
  theta[2] = res$min
  res = optimize(minusl.beta1,interval=c(-10,10),lalpha=theta[1],beta0=theta[2],tobs=tobs,w=w,x=x)
  theta[3] = res$min
  l = loglik(theta,tobs,w,x)
  show(c(theta,l))
}

#Discrete Netwon, using M=gamma*I
theta = c(1,0,0)
l = loglik(theta,tobs,w,x)
g = l1func(theta,tobs,w,x)
eps = 0.0001
gamma = 0.00001
count=1
while(sum(abs(g))>eps)
{ 
  theta = theta + gamma*g
  l = loglik(theta,tobs,w,x)
  g = l1func(theta,tobs,w,x)
  show(c(theta,l))
}

