#Baseball
rm(list=ls())
baseball <- read.table("data/baseball.dat", header=T)
baseball$freeagent = factor(baseball$freeagent)
baseball$arbitration = factor(baseball$arbitration)
p = ncol(baseball)-1
#Local search, 1-neigh
mod = sample(c(0,1),p,replace=T)
ind2 = c(1:p)[mod==1]

2
base2 = baseball[,c(1,1+ind2)]
fit = lm(log(salary)~.,data=base2)
numcal = 1
AICopt = AIC(fit)
more = TRUE
AICseq = AICopt
print(AICseq)
while(more)
{
  more = FALSE
  for(j in 1:p)
  {
    mod2 = mod
    mod2[j] = 1-mod2[j]
    ind2 = c(1:p)[mod2==1]
    base2 = baseball[,c(1,1+ind2)]
    fit2 = lm(log(salary)~.,data=base2)
    numcal = numcal+1
    if(AIC(fit2)<AICopt)
    {
      more = TRUE
      mod[j] = 1-mod[j]
      AICopt = AIC(fit2)
    }
  }
  AICseq = c(AICseq,AICopt)
}
plot.ts(AICseq)
show(AICopt)
show(numcal)
#a)
#Local search, randomly until better
mod = sample(c(0,1),p,replace=T)
ind2 = c(1:p)[mod==1]
base2 = baseball[,c(1,1+ind2)]
fit = lm(log(salary)~.,data=base2)
numcal=1
AICopt = AIC(fit)
more = TRUE
AICseq = AICopt
while(more)
{
  ind = sample(1:p,p)   #Random order of changes
  i=0
  more2 = TRUE
  while(more2)
  {
    i = i+1
    j = ind[i]
    mod2 = mod
    mod2[j] = 1-mod2[j]
    ind2 = c(1:p)[mod2==1]
    base2 = baseball[,c(1,1+ind2)]
    fit2 = lm(log(salary)~.,data=base2)
    numcal=numcal+1
    if((AIC(fit2) < AICopt) | (i==p))
      more2 = FALSE
  }
  more = FALSE
  if(AIC(fit2)<AICopt)
  {
    more = TRUE
    mod[j] = 1-mod[j]
    AICopt = AIC(fit2)
    AICseq = c(AICseq,AICopt)
  }
}
plot.ts(AICseq)
show(AICopt)
show(numcal)

#b)
#Local search, 2-neigh
mod = sample(c(0,1),p,replace=T)
ind2 = c(1:p)[mod==1]
base2 = baseball[,c(1,1+ind2)]
fit = lm(log(salary)~.,data=base2)
numcal=1
AICopt = AIC(fit)
more = TRUE
AICseq = AICopt
while(more)
{
  #First 1-neigh
  more = FALSE
  for(j in 1:p)
  {
    mod2 = mod
    mod2[j] = 1-mod2[j]
    ind2 = c(1:p)[mod2==1]
    base2 = baseball[,c(1,1+ind2)]
    fit2 = lm(log(salary)~.,data=base2)
    numcal=numcal+1
    if(AIC(fit2)<AICopt)
    {
      more = TRUE
      mod[j] = 1-mod[j]
      AICopt = AIC(fit2)
    }
  }
  AICseq = c(AICseq,AICopt)
  #Then 2-neigh
  for(j in 1:(p-1))
  for(k in (j+1):p)
  {
    i=i+1
    mod2 = mod
    mod2[j] = 1-mod2[j]
    mod2[k] = 1-mod2[k]
    ind2 = c(1:p)[mod2==1]
    base2 = baseball[,c(1,1+ind2)]
    fit2 = lm(log(salary)~.,data=base2)
    numcal=numcal+1
    if(AIC(fit2)<AICopt)
    {
      more = TRUE
      mod[j] = 1-mod[j]
      mod[k] = 1-mod[k]
      AICopt = AIC(fit2)
    }
  }
 AICseq = c(AICseq,AICopt)
}
plot.ts(AICseq)
show(AICopt)
show(numcal)
