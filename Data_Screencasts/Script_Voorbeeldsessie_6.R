#####
#  STATISTIEK B
#  Voorbeeldsessie 6: Assumpties bij lineaire regressie
#
#  Focus op volgende variabelen:
#  - AGE_R  = Leeftijd 
#  - PVLIT1 = Geletterdheid van de respondent 
#             Definitie:“understanding, evaluating, using and engaging with written texts 
#             to participate in society, to achieve one’s goals, and to develop one’s 
#             knowledge and potential"
#  - ICTHOME (Index die aangeeft in welke mate de respondent thuis ICT dient te hanteren)

source(file.choose())
library(car)
options(scipen=999)


Model_AGE<-lm(PVLIT1~AGE_R,data=PiaacBel)
summary(Model_AGE)

par(mfrow=c(2,2))
plot(Model_AGE)

par(mfrow=c(1,1))
residuals_plot(Model_AGE)
cooks_plot(Model_AGE)

PiaacBel$Idnummer<-seq(1:nrow(PiaacBel))
PiaacBel2<-PiaacBel[PiaacBel$Idnummer!=2611,]

Model_AGE2<-lm(PVLIT1~AGE_R,data=PiaacBel2)
summary(Model_AGE2)


Model_ICTHOME<-lm(PVLIT1~ICTHOME,data=PiaacBel)
summary(Model_ICTHOME)

par(mfrow=c(2,2))
plot(Model_ICTHOME)

par(mfrow=c(1,1))
residuals_plot(Model_ICTHOME)
cooks_plot(Model_ICTHOME)


# Uitsmijters: 
# Hoe zit het met de assumpties van dezelfde modellen indien je ze schat op de Nedl data?