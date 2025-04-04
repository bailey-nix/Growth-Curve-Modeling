##Growth Curve Modeling Example

#reading in libraries
library(lavaan)
library(tidyverse)
library(haven)
library(ggplot2)
library(Rmisc)


#load data and cleanup
dat<-read_sav("sem_final_q1.sav")
dat$id<-1:nrow(dat)
long_dat <- reshape(dat, varying=c("Math1","Math2","Math3","Math4","Math5"),
                    v.names="grade", idvar = c("id", "cgender"), direction = "long")

long_dat <- long_dat[order(long_dat$id), ]

long_dat$cgender <- as.factor(long_dat$cgender)
long_dat$cgender <- relevel(long_dat$cgender, "1")

long_dat$time <- long_dat$time-1


dat.mean <- summarySE(long_dat, measurevar="grade", 
                      groupvars=c("time", "cgender"), na.rm=TRUE)
dat.mean$cgender <- as.factor(dat.mean$cgender)
dat.mean$cgender <- relevel(dat.mean$cgender, "1")



#mean plot
mean_plot<- ggplot(data=dat.mean, aes(x=time, y=grade, group=cgender, color=cgender)) + 
  geom_errorbar(aes(ymin=grade-ci, ymax=grade+ci), width=.1) +
  geom_line(type=1, width=1) +
  geom_point(width=2)  +
  xlab("Time") +
  ylab("Grade") + 
  scale_color_manual(values=c('red', 'dodgerblue')) +
  theme_classic()
#print mean plot
mean_plot

#spaghetti plot
spag_plot <- ggplot(data=long_dat, aes(x=time, y=grade, group=id, colour=cgender)) + 
  geom_point(shape=4, width=1.5) +
  geom_line(width=0.5) +
  xlab("Time") +
  ylab("Grade") + 
  scale_color_manual(values=c('red', 'dodgerblue')) +
  theme_classic()
#print spaghetti plot
spag_plot



#estimating a linear model with grade 1 as the intercept
#defining model
linear.model <- '
i =~ 1*Math1 +  1*Math2 +  1*Math3 +  1*Math4 +  1*Math5 
s =~ 0*Math1 +  1*Math2 +  2*Math3 +  3*Math4 +  4*Math5 
i ~~ s

Math1 ~~ theta11*Math1
Math2 ~~ theta11*Math2
Math3 ~~ theta11*Math3
Math4 ~~ theta11*Math4
Math5 ~~ theta11*Math5
'
#fitting model
linear_fit <- growth(linear.model, data=dat)
#summary of fit indices and parameters
summary(linear_fit, fit.measures=TRUE, standardized=TRUE)



#estimating a quadratic model with grade 1 as intercept
quad.model <- '
i =~ 1*Math1 +  1*Math2 +  1*Math3 +  1*Math4 +  1*Math5 
s =~ 0*Math1 +  1*Math2 +  2*Math3 +  3*Math4 +  4*Math5 
q =~ 0*Math1 +  1*Math2 +  4*Math3 +  9*Math4 +  16*Math5

i ~~ s
i ~~ q
s ~~ q

Math1 ~~ theta11*Math1
Math2 ~~ theta11*Math2
Math3 ~~ theta11*Math3
Math4 ~~ theta11*Math4
Math5 ~~ theta11*Math5
'
#fitting model
quad_fit <- growth(quad.model, data=dat)
#summary of fit indices and paramaters
summary(quad_fit, fit.measures=TRUE, standardized=TRUE)



#chi squared difference test
lavTestLRT(quad_fit, linear_fit)
#suggests quad



#testing difference in gender
gender.model <- '
i =~ 1*Math1 +  1*Math2 +  1*Math3 +  1*Math4 +  1*Math5 
s =~ 0*Math1 +  1*Math2 +  2*Math3 +  3*Math4 +  4*Math5 
q =~ 0*Math1 +  1*Math2 +  4*Math3 +  9*Math4 +  16*Math5

i ~~ s
i ~~ q
s ~~ q

Math1 ~~ theta11*Math1
Math2 ~~ theta11*Math2
Math3 ~~ theta11*Math3
Math4 ~~ theta11*Math4
Math5 ~~ theta11*Math5

i~cgender
s~cgender
q~cgender
'

#fitting model
gender_fit<- growth(model = gender.model, data = dat)
#summary of indices and parameters
summary(gender_fit, fit.measures=TRUE, standardized=TRUE)
parameterestimates(gender_fit)
