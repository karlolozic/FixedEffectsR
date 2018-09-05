##################
### Approach 1 ###
##################

##################################################################
#                                                                #  
# I am using data on Conservative-Catholic countries             #
# (Germany, Italy, Spain, Portugal, Austria, Belgium)            #
# in order to estimate the consumtion smoothing.                 #
# I want to control for time and industry fixed effects          #
# Dependent variable is Expenditure.Change                       #
# Independent variables are Revenue.Change, GDP.Change           #
# Could include the lagged Revenue, Expenditure and GDP changes. #
# In some models simply use dummy variables for time or industry #
#                                                                #
##################################################################


######################################################################################
#                                                                                    #
# The fixed effect model is as follows:                                              #
#                                                                                    #
# y = ?? + X?? + u                                                                     #
# where:                                                                             #
# u = Z?? + ??                                                                         #
#                                                                                    #
# so ultimately we have                                                              #
# y = ?? + X?? + Z?? + ??                                                                #
# where:                                                                             #
# X is the vector of dependent variable inputs                                       #
# Z is a vector of industry and time unvarying factors picked up by the model        #
#                                                                                    #
# In particular for this example we have:                                            #
# Y(i,t) = ??0 + ??1GDPGrowth(i,t) + ??2RevenueChange(i,t) + ??(i,t) + ??(i,t) + ??(i,t)   #
# where:                                                                             #
# ??(i,t) is the industry fixed effects                                               #
# ??(i,t) is the time fixed effects                                                   #
# ??(i,t) is the residual                                                             #
######################################################################################



###import the dataset###
data <- read.csv("conservative.csv")
View(data)
attach(data)

###explore inter-variable graphs###
plot(data$Expenditure.Change,data$Revenue.Change)
plot(data$Expenditure.Change,data$GDP.growth)
plot(data$Revenue.Change,data$GDP.growth)

###explore variable correlations### 
data.new <-data[4:6] #get rid of string variables/only include columns(variables) you want to explore
pairs(data.new)
cor(data.new)
#install.packages("corrplot")
library(corrplot)
forcorrplot <-cor(data.new) ###plot correlations
corrplot(forcorrplot)       ###plot correlations
corrplot(forcorrplot, method = "color")

###using a least square dummy variable model############
###using time and industry dummies to fix the effects###
###careful to avoid the dummy variable trap#############
Y <- cbind(Expenditure.Change)
X <- cbind(Revenue.Change, GDP.growth, Y2006, Y2007, Y2008, Y2009, Y2010, Y2011, Y2012, Y2013, Y2014, Y2015, Utilities, Transport, Telecommunications, Energy, Post)

model <-lm(Y ~ X) ###regress Y on X
summary(model)



##################
### Approach 2 ###
##################
#install.packages("zoo") #if you get the error and need to reinstall the 'zoo' package
library(plm) ### panel data linear models
data1<-read.csv("conservative.csv")### import the dataset
attach(data1)

Y1 <-cbind(Expenditure.Change)
X1 <-cbind(Revenue.Change, GDP.growth, Utilities,Post, Transport, Energy, Telecommunications)
# NOTICE: only using industry dummies since the plm model can control for time

###plm cannot have multiple id-time pairs (e.g. different companies from same country (id) in same period (time))
###so only one variable fixed for this purpose and include other dummies in X
pdata <-plm.data(data1, index=c("Year")) ### fixing the years 
summary(Y1) ###explore Y1=Y
summary(X1) ###explore X1=X

###create fixed effects estimator
fixed <- plm(Y1~X1, data=pdata, model = "within")
summary(fixed)

###create a random effects estimator
random <- plm(Y1~X1, data=pdata, model = "random")
summary(random)


###Hausman test for fixed effects vs random effects
###to decide what model to use
phtest(fixed,random)