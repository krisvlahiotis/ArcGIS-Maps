##Apr 22nd 2023#
##KV stats####
###running glmms on urbanizations category and year wrt BCC distrubtion#### 


library("ggplot2") #PLOTS
library("knitr") #making htmls
library("lme4") #for running GLMMs
library("plyr") #subsetting the data
library("Hmisc")
library("lsmeans")
library("multcomp") #for bptest
library("lmerTest")
library("glmmTMB")#for running nb's##
library("MASS")#for running nb's##
library("DHARMa")#for running nb's##
library("lme4")#for running nb's##
install.packages("glmmTMB")

##Using only counts from 2003 to 2008##
DF<-read.csv("RelCount 2003.csv")
str(DF)
DF$county<- as.factor(DF$county)
DF$urban_category<- as.factor(DF$urban_category)
DF$observation_date<- as.factor(DF$observation_date)
DF$protocol_type<- as.factor(DF$protocol_type)
DF$number_observers<- as.factor(DF$number_observers)
DF$year<- as.factor(DF$year)
DF$latitude <-as.factor(DF$latitude)
DF$longitude<-as.factor(DF$longitude)
DF$RelCount<- as.numeric(DF$RelCount)
str(DF)


####These are models two ways, the glm which has a binomial family (distribution) and then the glmTMB is for the negative binomial and nb is the operations.#####
###I've put the removal of random effects down just for the glms becuause you can easily change the operation to match the nb if you choose to do that####

#RelCount = relative count 

#Section 1 GLM Code with random effect tests######################################################################################################################################################################################################################
#This was with latitude, longitude and number_observers as random effects
glm_interactions<-glmer(RelCount~ urban_category*year+ (1|county)+ (1|latitude) + (1|longitude) + (1|protocol_type) + (1|number_observers)+ (1|observation_date), family="poisson", data = DF)

#This was run with latitude, longitude and number_observers as fixed effects
glm_interactions<-glmer(RelCount~ urban_category*year+ latitude + longitude + number_observers + (1|county) + (1|protocol_type) + (1|observation_date), family="poisson", data = DF)

#I got this warning:fixed-effect model matrix is rank deficient so dropping 1641 columns / coefficients
#THis seems to not effect the model

#And also this warning once I forced it to stop:
#Warning messages:
  #1: In (function (fr, X, reTrms, family, nAGQ = 1L, verbose = 0L,  ... :
                    # non-integer x = 0.001428

####I didn't make it this far###
#only use this if there is no significant interactions##
glm_fixed<-glmer(RelCount~ Urban.Category+Year+ (1|County:Locality)+ (1|Latitude) + (1|Longitude) + (1|Protocol.Type) + (1|Observer.ID)+ (1|Observation.Date), family="binomial", data = DF)

##Removal of random effects to test them for significance if the model works##
#removing county and locality#
glm_county<-glmer(RelCount~ Urban.Category*Year+ (1|Latitude) + (1|Longitude) + (1|Protocol.Type) + (1|Observer.ID)+ (1|Observation.Date), family="binomial", data = DF)
anova_county<- anova(glm_interactions,glm_county)

#removing lat##
glm_lat<-glmer(RelCount~ Urban.Category*Year+ (1|County:Locality)+ (1|Longitude) + (1|Protocol.Type) + (1|Observer.ID)+ (1|Observation.Date), family="binomial", data = DF)
anova_lat<- anova(glm_interactions,glm_lat)

##Removing long###
glm_long<-glmer(RelCount~ Urban.Category*Year+ (1|County:Locality)+ (1|Latitude) + (1|Protocol.Type) + (1|Observer.ID)+ (1|Observation.Date), family="binomial", data = DF)
anova_long<- anova(glm_interactions,glm_long)

#removing protocol type##
glm_protocol<-glmer(RelCount~ Urban.Category*Year+ (1|County:Locality)+ (1|Latitude) + (1|Longitude) + (1|Observer.ID)+ (1|Observation.Date), family="binomial", data = DF)
anova_protocol<- anova(glm_interactions,glm_protocol)

#removing observer ID##
glm_ID<-glmer(RelCount~ Urban.Category*Year+ (1|County:Locality)+ (1|Latitude) + (1|Longitude) + (1|Protocol.Type) + (1|Observation.Date), family="binomial", data = DF)
anova_ID<- anova(glm_interactions,glm_ID)

#Removing observation date##
glm_date<-glmer(RelCount~ Urban.Category*Year+ (1|County:Locality)+ (1|Latitude) + (1|Longitude) + (1|Protocol.Type) + (1|Observer.ID), family="binomial", data = DF)
anova_date<- anova(glm_interactions,glm_date)

##########################################################################################################################################################################################
##########################################################################################################################################################################################
##########################################################################################################################################################################################
##Section 2, negative binomials###########################################################################################################################################################

#I ran this and it gave me a weird graph#
nb_interactions<-glmmTMB(RelCount~ urban_category*year+ (1|county)+ (1|latitude) + (1|longitude) + (1|protocol_type) + (1|number_observers)+ (1|observation_date), family="nbinom2", data = DF)

nb_interactions<-glmmTMB(RelCount~ urban_category*year+ latitude + longitude + number_observers + (1|county)+ (1|protocol_type) + (1|observation_date), family="nbinom2", data = DF)

#checking the model residuals to see if it fits#
res=simulateResiduals(nb_interactions)
plot(res,rank=T)

#Check for over dispersion# 

testDispersion(nb_interactions)
overdisp_fun1 <- function(nb_interactions) {
  rdf <- df.residual(nb_interactions)
  rp <- residuals(nb_interactions, type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <-pchiqu(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq, ratio=prat,rdf=rdf,p=pval)
}

##############################################################