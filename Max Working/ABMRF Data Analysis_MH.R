############################################
# Real-data (ABMRF) example for GzLM Paper #
############################################
# Max Halvorson #
#   11/6/18     #
#################

rm(list=ls())
require(tidyverse)
require(pscl)
require(boot)
require(leaps)
require(car)
require(foreign)
require(psych)
require(oddsratio)

###################
# Data processing #
###################
dg <- as.tibble(read.spss("https://raw.githubusercontent.com/mhalvo/glm_main/master/ABMRF%20Data/ABMRF_clean.sav", 
                          use.value.labels = F, 
                          use.missings=F))
names(dg)[names(dg)=="DEMO19"] <- "colyr" 
dg$colyr[which(dg$colyr==99)] <- NA #replace 99's with NA
dgc <- dg[complete.cases(dg),] #subset to complete cases
dgc$gen <- dgc$gen - 1 #recode gender so 0=male, 1=female
dgc$gen_f0 <- 1 - dgc$gen

# This isn't a missing data paper; so it's OK that we drop 70 cases. Final n = 404
write.csv(dgc, file="abmrf_for_interactive.csv", row.names=F)

############################################################
# Run ZIP model of past-year negative alcohol consequences #
############################################################
zip1 <- zeroinfl(negpysum ~ colyr + as.factor(gen) + pre + purg + ss, 
                 data = dgc,
                 dist="poisson",
                 link="logit")
summary(zip1)

## Structural zero parameters
zip1z <- summary(zip1)$coef$zero
RR <- exp(zip1z[,1])
CI2.5 <- exp(zip1z[,1]-1.96*zip1z[,2])
CI97.5 <- exp(zip1z[,1]+1.96*zip1z[,2])
(zip1z <- round(cbind(zip1z[,1], RR, CI2.5, CI97.5, zip1z[,4]),3))

###
# So a structural zero is MORE likely when college year is lower, and MORE likely when SS is lower.
# Flipping this, having ANY negative alcohol consequences is MORE likely when college year is higher, and MORE likely when SS is higher.
###

## Count parameters
zip1c <- summary(zip1)$coef$`count`
OR <- exp(zip1c[,1])
CI2.5 <- exp(zip1c[,1]-1.96*zip1c[,2])
CI97.5 <- exp(zip1c[,1]+1.96*zip1c[,2])
(zip1c <- round(cbind(zip1c[,1], OR, CI2.5, CI97.5, zip1c[,4]),3))

###
# So a higher NUMBER of negative alcohol consequences is predicted by MALE gender, LESS premeditation, and MORE positive urgency.
###


##############
# TO PLOT... #
##############
### 1. Plot Count of Alc Consequences by Pos Urg, divided by gender 
# Create hypothetical values at which to plot
range(dgc$purg)
purghyp <- seq(from=1,to=4, by=.25)
dhypm <- as.tibble(data.frame(pre=rep(mean(dgc$pre, na.rm=T),13),
                             purg=purghyp,
                             ss=rep(mean(dgc$pre, na.rm=T),13),
                             gen=rep(0,13),
                             colyr=rep(2,13)))
dhypf <- as.tibble(data.frame(pre=rep(mean(dgc$pre, na.rm=T),13),
                              purg=purghyp,
                              ss=rep(mean(dgc$pre, na.rm=T),13),
                              gen=rep(1,13),
                              colyr=rep(2,13)))
# Predict consequences at these hypothetical values
dhypm$negpysum <- predict(zip1, newdata=dhypm, type="response")
dhypf$negpysum <- predict(zip1, newdata=dhypf, type="response")
# Calculate confidence ribbon values
zip1lo <- zip1hi <- zip1 
sez <- summary(zip1)$coef$zero[,2] # pull out standard errors
sec <- summary(zip1)$coef$`count`[,2]
zip1lo$coefficients$zero <- zip1$coefficients$zero - 1.96*sez # calculate parameters for zero
zip1hi$coefficients$zero <- zip1$coefficients$zero + 1.96*sez
zip1lo$coefficients$`count` <- zip1$coefficients$`count` - 1.96*sec # calculate parameters for count
zip1hi$coefficients$`count` <- zip1$coefficients$`count` + 1.96*sec

# Plot
ggplot(mapping=aes(x=purg, y=negpysum), data=dhypm) +
   geom_smooth(method="loess", color="blue") + 
   geom_smooth(mapping=aes(x=purg, y=negpysum), data=dhypf, method="loess", color="pink") + 
   geom_linerange(x=1, 
                  ymin=filter(dhypf, purg==1)$negpysum, 
                  ymax=filter(dhypm, purg==1)$negpysum, 
                  color="black") + 
   geom_linerange(x=2, 
                  ymin=filter(dhypf, purg==2)$negpysum, 
                  ymax=filter(dhypm, purg==2)$negpysum, 
                  color="black") +
   geom_linerange(x=3, 
                  ymin=filter(dhypf, purg==3)$negpysum, 
                  ymax=filter(dhypm, purg==3)$negpysum, 
                  color="black") +
   geom_linerange(x=4, 
                  ymin=filter(dhypf, purg==4)$negpysum, 
                  ymax=filter(dhypm, purg==4)$negpysum, 
                  color="black") + 
   geom_text(aes(y=(dhypm$negpysum+dhypf$negpysum)/2,
                 label=c((filter(dhypm, purg==1)$negpysum-filter(dhypf, purg==1)$negpysum) %>% round(.,2),"","","",
                         (filter(dhypm, purg==2)$negpysum-filter(dhypf, purg==2)$negpysum) %>% round(.,2),"","","",
                         (filter(dhypm, purg==3)$negpysum-filter(dhypf, purg==3)$negpysum) %>% round(.,2),"","","",
                         (filter(dhypm, purg==4)$negpysum-filter(dhypf, purg==4)$negpysum) %>% round(.,2))),
             nudge_x=-.1, nudge_y=0) + 
  labs(x = "Positive Urgency", y="Count of Negative Alcohol Consequences", )

  #replicate David Atkins' logistic graph