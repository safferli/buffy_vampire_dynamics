options(bitmapType='cairo')
options(scipen = 999)
library(ggplot2)

## http://spiff.rit.edu/classes/phys317/buffy/vampecology.htm
## Brian Thomas

# dH/dt = rH(K-H)/(K-aHV)
# dV/dt = baHV + mV - sV
# 
# where   r           is the intrinsic growth rate of the human population, incorporating
# natural rates of both birth and death as well as immigration
# K         is the human carrying capacity of the habitat in question
# a          is a coefficient that relates the number of human-vampire
# encounters to the number of actual feedings
# b          is the proportion of feedings in which the vampire sires the victim
# (i.e.- this is the vampire birth rate)
# m         is the net rate of vampire migration into Sunnydale
# s           is the rate at which the Scoobies stake vampires (assumed to be the
# only important source of vampire deaths).  
# 
# equilibrium rates: 
# H^hat = (s-m)/ba
# V^hat = (r/a)*(1+(m-s)/baK)

# Let's assume the following:
#   
# Sunnydale's human population growth rate is 10% annually, which is at the high
# end for a budding California community.
# 
# A vampire feeds every three days, and encounters about one hundred potential
# victims in the course of a day, meaning that 1 out of every 300 encounters
# involves a little refreshment.
# 
# An individual vampire sires a victim every other year, or once per 240 feedings.
# 
# Buffy and her Slayerettes, busy little beavers that they are,  annually stake about
# 1/3 of the vampires plaguing Sunnydale.
# 
# Vampires are flocking to Sunnydale, since the Hellmouth is the underwordly
# equivalent of Silicon Valley, and the demon labor market is just too good
# to be true.  Thus, we'll assume a yearly migration rate of about 10%, or
# the same as for the humans.
# 
# In terms of our model, we have:

r = 0.0953 # ln(1+0.1)
a = 0.00333
b = 0.00417 # 1/240
s = 0.600 # ln(1-0.33) = -0.4
K = 100000
m = 0.0953 # ln(1+01)


# equilibrium rates: 
H.hat <- (s-m)/(b*a)
V.hat <- (r/a)*(1+(m-s)/(b*a*K))


# dH/dt = rH(K-H)/(K-aHV)
dH.dt <- function(H, V) {
  (r*H*(K-H))/(K-a*H*V)
}

# dV/dt = baHV + mV - sV
dV.dt <- function(H, V) {
  b*a*H*V + m*V - s*V
}

## simulation loop
simulate.vampires <- function(periods = 50, H, V) {
  # initialise vectors at correct size for speed
  humans <- rep(NA, periods+1)
  dH <- rep(NA, periods+1)
  vampires <- rep(NA, periods+1)
  dV <- rep(NA, periods+1)
  
  # initial populations
  humans[1] <- H
  vampires[1] <- V
  
  # loop through periods, start at 2
  for (i in (seq_len(periods)+1)) {
    dH[i] <- dH.dt(humans[i-1], vampires[i-1])
    humans[i] <- humans[i-1] + dH[i]
    dV[i] <- dV.dt(humans[i-1], vampires[i-1])
    vampires[i] <- vampires[i-1] + dV[i]
  }
  
  # return all as data.frame
  return(as.data.frame(cbind(period = seq_len(periods+1), humans, dH, vampires, dV)))
}


# initial values for humans and vampires
H <- 30000
V <- 18

# run loop and output data
dta <- simulate.vampires(50, H, V)


dta %>% ggplot()+
  geom_line()+
  aes(x=humans, y=log(vampires))


