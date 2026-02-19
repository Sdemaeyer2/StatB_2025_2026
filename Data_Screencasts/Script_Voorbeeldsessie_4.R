#####
#  STATISTIEK B
#  Voorbeeldsessie 4: Correlatie-analyse
#
#  1 Grafisch verbanden verkennen
#  2 Covarianties/correlaties
#  Focus op volgende variabelen:
#  - AGE_R  = Leeftijd 
#  - PVLIT1 = Geletterdheid van de respondent 
#             Definitie:“understanding, evaluating, using and engaging with written texts 
#             to participate in society, to achieve one’s goals, and to develop one’s 
#             knowledge and potential"
#  - ICTWORK (Index die aangeeft in welke mate de respondent voor z'n job ICT dient te hanteren)
#  - ICTHOME (Index die aangeeft in welke mate de respondent thuis ICT dient te hanteren)

source(file.choose())
library(car)

#  1 Grafisch verbanden verkennen

plot(PiaacBel$AGE_R,PiaacBel$PVLIT1)
plot(PiaacBel$AGE_R,PiaacBel$ICTWORK)
plot(PiaacBel$AGE_R,PiaacBel$ICTHOME)
plot(PiaacBel$ICTWORK,PiaacBel$ICTHOME)
plot(PiaacBel$ICTWORK,PiaacBel$PVLIT1)

#  2 Covarianties / correlaties
#  2.1 De parameters op zich berekenen
cov(PiaacBel$AGE_R,PiaacBel$PVLIT1,use="pairwise.complete")
cov(PiaacBel$AGE_R/10,PiaacBel$PVLIT1,use="pairwise.complete")
cor(PiaacBel$AGE_R,PiaacBel$PVLIT1,use="pairwise.complete")

#  2.2 De significantietoets 
cor.test(PiaacBel$AGE_R,PiaacBel$PVLIT1)
cor.test(PiaacBel$AGE_R,PiaacBel$ICTWORK)
cor.test(PiaacBel$AGE_R,PiaacBel$ICTHOME)
cor.test(PiaacBel$ICTWORK,PiaacBel$ICTHOME)
cor.test(PiaacBel$ICTWORK,PiaacBel$PVLIT1)

#  2.3 Alles gecombineerd
Variabelen<-data.frame(PiaacBel$AGE_R,PiaacBel$PVLIT1,
                       PiaacBel$ICTWORK,PiaacBel$ICTHOME)
cor.prob(Variabelen)
options(scipen=999)
cor.prob(Variabelen)
plot(Variabelen)

# Uitsmijters: 
# Hoe zit het in Nederland? Repliceer de verschillende analyses voor Nederland.