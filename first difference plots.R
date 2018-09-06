rm(list=ls())
library('tidyverse')
set.seed(1337)

###########
# Poisson #
###########

### create some data from a poisson model
x1 <- rnorm(1000, 0, 1)
x2 <- rbinom(1000, 1, .35)

y <- exp(.3*x1 + .4*x2 + rnorm(1000,0,.4)) %>% round(., 0)

glmp <- glm(y ~ x1 + x2, family="poisson") #b0 = .11996; b1 = .27568; b2 = .35875

### fit a glm model with correct specification
predict(glmp, newdata=data.frame(x1=3,x2=1), type="response")
exp(.11996 + .27568*3 + .35875*1)

dat <- data.frame(x1=x1, x2=x2, y=y)

### model predictions for x2=0
x1hyp <- seq(-3, 3, by=.2)
x2hyp0 <- rep(0, 31)
fits0 <- predict(glmp, newdata=data.frame(x1=x1hyp, x2=x2hyp0), type="response")
hyp0 <- data.frame(x1=x1hyp, x2=x2hyp0, y=fits0)

### model predictions for x2=1
x2hyp1 <- rep(1, 31)
fits1 <- predict(glmp, newdata=data.frame(x1=x1hyp, x2=x2hyp1), type="response")
hyp1 <- data.frame(x1=x1hyp, x2=x2hyp1, y=fits1)

### graph model predictions for x2=1, x2=0
hyps <- full_join(hyp0, hyp1)

ggplot(data=hyps, aes(x=x1, y=y, color=factor(x2))) + 
  geom_point() +
  geom_linerange(x=-2, ymin=filter(hyps, x1==-2, x2==1)$y, ymax=filter(hyps, x1==-2, x2==0)$y, color="black") + 
  geom_linerange(x=-1, ymin=filter(hyps, x1==-1, x2==1)$y, ymax=filter(hyps, x1==-1, x2==0)$y, color="black") + 
  geom_linerange(x=0, ymin=filter(hyps, x1==0, x2==1)$y, ymax=filter(hyps, x1==0, x2==0)$y, color="black") + 
  geom_linerange(x=1, ymin=filter(hyps, x1==1, x2==1)$y, ymax=filter(hyps, x1==1, x2==0)$y, color="black") + 
  geom_linerange(x=2, ymin=filter(hyps, x1==2, x2==1)$y, ymax=filter(hyps, x1==2, x2==0)$y, color="black") + 
  ylab("Predicted Count")

############
# Logistic #
############

### create some data from a logistic model
yl <- 1*(1/(1 + exp(-(.3*x1 + .2*x2 + rnorm(1000,0,.3)))) > .5)

glml <- glm(yl ~ x1 + x2, family=binomial(link="logit")) #b0 = -.04762; b1 = 1.26040; b2 = .73

### fit a glm model with correct specification
predict(glml, newdata=data.frame(x1=0,x2=1), type="response")
1/(1 + exp(-(glml$coef[1] + glml$coef[2]*0 + glml$coef[3]*1)))


### model predictions for x2=0
fits0l <- predict(glml, newdata=data.frame(x1=x1hyp, x2=x2hyp0), type="response")
hyp0l <- data.frame(x1=x1hyp, x2=x2hyp0, y=fits0l)

### model predictions for x2=1
fits1l <- predict(glml, newdata=data.frame(x1=x1hyp, x2=x2hyp1), type="response")
hyp1l <- data.frame(x1=x1hyp, x2=x2hyp1, y=fits1l)

### graph model predictions for x2=1, x2=0
hypsl <- full_join(hyp0l, hyp1l)

ggplot(data=hypsl, aes(x=x1, y=y, color=factor(x2))) + 
  geom_point() +
  geom_linerange(x=-2, ymin=filter(hypsl, x1==-2, x2==1)$y, ymax=filter(hypsl, x1==-2, x2==0)$y, color="black") + 
  geom_linerange(x=-1, ymin=filter(hypsl, x1==-1, x2==1)$y, ymax=filter(hypsl, x1==-1, x2==0)$y, color="black") + 
  geom_linerange(x=0, ymin=filter(hypsl, x1==0, x2==1)$y, ymax=filter(hypsl, x1==0, x2==0)$y, color="black") + 
  geom_linerange(x=1, ymin=filter(hypsl, x1==1, x2==1)$y, ymax=filter(hypsl, x1==1, x2==0)$y, color="black") + 
  geom_linerange(x=2, ymin=filter(hypsl, x1==2, x2==1)$y, ymax=filter(hypsl, x1==2, x2==0)$y, color="black") + 
  ylab("Predicted Probability")

