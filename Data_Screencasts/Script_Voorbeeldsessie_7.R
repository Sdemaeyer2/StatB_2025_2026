#####
#  STATISTIEK B
#  Voorbeeldsessie 7: Meervoudige regressie-analyse
#
#  1 Grafisch verbanden verkennen
#  2 Meervourdige regressie
#  3 Modellen vergelijken
#  Focus op volgende variabelen:
#  - AGE_R  = Leeftijd 
#  - PVLIT1 = Geletterdheid van de respondent 
#             Definitie:“understanding, evaluating, using and engaging with written texts 
#             to participate in society, to achieve one’s goals, and to develop one’s 
#             knowledge and potential"
#  - ICTWORK (Index die aangeeft in welke mate de respondent voor z'n job ICT dient te hanteren)
#  - ICTHOME (Index die aangeeft in welke mate de respondent thuis ICT dient te hanteren)
#  - LEAVEDU = Leeftijd waarop de respondent is afgestudeerd
#  - NFEHRS = Number of hours for non-formal education

source(file.choose())
library(car)
options(scipen=999)

#  1 Grafisch verbanden verkennen

par(mfrow=c(3,1))
plot(PiaacBel$AGE_R,PiaacBel$PVLIT1)
abline(reg=lm(PVLIT1~AGE_R,data=PiaacBel))
plot(PiaacBel$ICTWORK,PiaacBel$PVLIT1)
abline(reg=lm(PVLIT1~ICTWORK,data=PiaacBel))
plot(PiaacBel$ICTHOME,PiaacBel$PVLIT1)
abline(reg=lm(PVLIT1~ICTHOME,data=PiaacBel))
par(mfrow=c(1,1))

#  2 Meervoudige regressieanalyse

Model1<-lm(PVLIT1~AGE_R+ICTWORK+ICTHOME,data=PiaacBel)
summary(Model1)

PiaacBel$AGE_Z<-zscore(PiaacBel$AGE_R)
PiaacBel$ICTWORK_Z<-zscore(PiaacBel$ICTWORK)
PiaacBel$ICTHOME_Z<-zscore(PiaacBel$ICTHOME)

Model2<-lm(PVLIT1~AGE_Z+ICTWORK_Z+ICTHOME_Z,data=PiaacBel)
summary(Model2)
vif(Model2)

# 3 Modellen vergelijken
PiaacBel$LEAVEDU_Z<-zscore(PiaacBel$LEAVEDU)
PiaacBel$NFEHRS_Z<-zscore(PiaacBel$NFEHRS)
Model3<-lm(PVLIT1~AGE_Z+ICTWORK_Z+ICTHOME_Z+LEAVEDU_Z+NFEHRS_Z,data=PiaacBel)
summary(Model3)
anova(Model2,Model3)

PiaacBel2<-na.omit(PiaacBel[c("PVLIT1","AGE_Z","ICTWORK_Z","ICTHOME_Z","LEAVEDU_Z","NFEHRS_Z")])
Model2b<-lm(PVLIT1~AGE_Z+ICTWORK_Z+ICTHOME_Z,data=PiaacBel2)
Model3b<-lm(PVLIT1~AGE_Z+ICTWORK_Z+ICTHOME_Z+LEAVEDU_Z+NFEHRS_Z,data=PiaacBel2)
anova(Model2b,Model3b)
vif(Model3b)

# Uitsmijters: 
# Hoe zit het in Nederland? Repliceer de verschillende analyses voor Nederland.