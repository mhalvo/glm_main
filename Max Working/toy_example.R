####################
# Toy Example      #
#   for GzLM Paper #
####################
# Max Halvorson #
#  9/20/18      #
#########################
# Data Analysis Example #
#########################


###
################################
# Multiple Imputation Examples #
################################
###

rm(list=ls())
library('tidyverse')
library('MASS')
library('pscl')

### 0) data load and cleaning ###
dn <- as.tibble(read.csv('https://raw.githubusercontent.com/mhalvo/teaching/master/NBAcombine.csv'))
dn$PID <- 1:1000
dn <-  select(dn, 
                Name, PID, Height, Weight, Vertical, T.Q..Spring,
                L.A.T., NSeasons)
dng <- select(dn, -Name) %>% replace(., . == 0, NA)
dng$NSeasons <- replace(dng$NSeasons, is.na(dng$NSeasons), 0)
dng <- dng[complete.cases(dng),]
dng$AnySeasons <- 1*(dng$NSeasons > 0)
dngc <- as.tibble(scale(dplyr::select(dng, Height, Weight, Vertical, T.Q..Spring, L.A.T.)))
dngc$NSeasons <- dng$NSeasons
dngc$AnySeasons <- dng$AnySeasons
names(dngc)[1:5] <- c("cHt", "cWt", "cVt", "cTQ", "cLA")
hist(dng$NSeasons)


############
# LOGISTIC #
############

Mclogi <- glm(AnySeasons ~ cHt + cWt + cVt + cTQ + cLA, data=dngc, family=binomial(link="logit"))
summary(Mclogi)
Mlogi <- glm(AnySeasons ~ Height + Weight + Vertical + T.Q..Spring + L.A.T., data=dng, family=binomial(link="logit"))
summary(Mlogi)

### Output results for table
# Showing the effect of vertical leap at mean levels of size
# Showing the effect of vertical leap for tall
# Showing the effect of vertical leap for short
psych::describe(dng)

xhypm <- as.tibble(data.frame(Height=mean(dng$Height),
                   Weight=mean(dng$Weight),
                   Vertical=mean(dng$Vertical),
                   T.Q..Spring=mean(dng$T.Q..Spring),
                   L.A.T.=mean(dng$L.A.T.)))

xhypl <- list()
i <- 1
for(hw in 0:4) {
  for(v in 0:4) {
    xhypl[[i]] <- as.tibble(data.frame(Height=72 + hw*3,
                                     Weight=165 + hw*25,
                                     Vertical=26 + v*4,
                                     T.Q..Spring=mean(dng$T.Q..Spring),
                                     L.A.T.=mean(dng$L.A.T.)))
    i <- i + 1
  }
}

lapply(xhypl, function(x) predict(Mlogi, newdata=x, type="response"))



# height/weight increasing

xhypl <- as.tibble(data.frame(Height=rep(75, 38), # height/weight low
                  Weight=rep(190, 38),
                  Vertical=seq(25, 44, length.out=38),
                  T.Q..Spring=rep(mean(dng$T.Q..Spring), 38),
                  L.A.T.=rep(mean(dng$L.A.T.), 38)))

xhypm <- as.tibble(data.frame(Height=rep(78, 38), # height/weight med
                              Weight=rep(215, 38),
                              Vertical=seq(25, 44, length.out=38),
                              T.Q..Spring=rep(mean(dng$T.Q..Spring), 38),
                              L.A.T.=rep(mean(dng$L.A.T.), 38)))

xhyph <- as.tibble(data.frame(Height=rep(81, 38), # height/weight hi
                              Weight=rep(240, 38),
                              Vertical=seq(25, 44, length.out=38),
                              T.Q..Spring=rep(mean(dng$T.Q..Spring), 38),
                              L.A.T.=rep(mean(dng$L.A.T.), 38)))

fitsl <- predict(Mlogi, newdata=xhypl, type="response")
fitsm <- predict(Mlogi, newdata=xhypm, type="response")
fitsh <- predict(Mlogi, newdata=xhyph, type="response")

hypsl <- as.tibble(data.frame(xhypl, P=fitsl))
hypsm <- as.tibble(data.frame(xhypm, P=fitsm)) 
hypsh <- as.tibble(data.frame(xhyph, P=fitsh))

hyps <- bind_rows(hypsl, hypsm, hypsh)
hyps$HW <- rep(1:3, each=38)

ggplot(data=hyps, aes(x=Vertical, y=P, color=HW)) + 
  geom_point() +
  ylab("Predicted P(NBA)")

# Odds of playing for a season increase by factor of 1.17 for each add'l inch of vertical leap




### Graphic in INTERACTIVE
write.table(dng, file="nbagraph.csv", sep=",")


###########
# POISSON #
###########

Mpois <- glm(NSeasons ~ Height + Weight + Vertical + T.Q..Spring + L.A.T., data=dng, family=poisson(link="log"))
summary(Mpois)

########
# ZINB #
########

