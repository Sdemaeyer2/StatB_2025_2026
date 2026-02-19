#####
#  STATISTIEK B
#  Voorbeeldsessie 5: Bivariate regressie-analyse
#
#  1 Grafisch verbanden verkennen
#  2 Regressie-analyse
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

#  1 Grafisch verbanden verkennen

plot(PiaacBel$AGE_R,PiaacBel$PVLIT1,xlim=c(0,80))
plot(PiaacBel$ICTHOME,PiaacBel$PVLIT1)

#  2 De regressie-analyse
Model_AGE<-lm(PVLIT1~AGE_R,data=PiaacBel)
summary(Model_AGE)
anova(Model_AGE)
309.88+20*(-0.83075) #voorspelde scores bereken (bv:voor iemand die 20jr oud is)
309.88+60*(-0.83075) #voorspelde scores bereken (bv:voor iemand die 60jr oud is)
plot(PiaacBel$AGE_R,PiaacBel$PVLIT1,main="Geletterdheid in functie van leeftijd",
     xlab="Leeftijd", ylab="Geletterdheid")
abline(reg=Model_AGE,col="red",lwd=2)

Model_ICTHOME<-lm(PVLIT1~ICTHOME,data=PiaacBel)
summary(Model_ICTHOME)
plot(PiaacBel$ICTHOME,PiaacBel$PVLIT1,main="Geletterdheid in functie van ICT-gebruik thuis",
     xlab="ICT gebruik thuis", ylab="Geletterdheid")
abline(reg=Model_ICTHOME,col="red",lwd=2)

# Uitsmijters: 
# In hoeverre is de Leeftijd voorspellend voor het het gebruik van ICT Thuis (ICTHOME)?
# Geef eventueel ook grafisch weer.
# Welke score verwacht je voor een 30-jarige op de variabele ICTHOME, gegeven je model?
# Welke score verwacht je voor iemand van je eigen leeftijd op de variabele ICTHOME, gegeven je model?
# Hoe zit het in Nederland? Repliceer de verschillende analyses voor Nederland.
# Welke score voor ICTHOME verwacht je voor een 30-jarige Nederlander, gegeven het model voor Nedl?
# Welke score voor ICTHOME verwacht je van een leeftijdsgenoot van jezelf in Nederland, gegeven het model voor Nedl?