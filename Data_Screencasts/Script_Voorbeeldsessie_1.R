#####
#  STATISTIEK B
#  Voorbeeldsessie 1: Verschil in gemiddelden (t-test)
#
#  1 Gemiddelden per categorie/groep respondenten berekenen
#  2 Betrouwbaarheidsintervallen
#  3 t-test uitvoeren
#  4 Effectgrootte (Cohen's d)
#  Focus op volgende variabelen:
#  - PVLIT1 = Geletterdheid van de respondent 
#             Definitie:“understanding, evaluating, using and engaging with written texts 
#             to participate in society, to achieve one’s goals, and to develop one’s 
#             knowledge and potential"
#  - ICTWORK (Index die aangeeft in welke mate de respondent voor z'n job ICT dient te hanteren)
#  - ICTHOME (Index die aangeeft in welke mate de respondent thuis ICT dient te hanteren)
#####
source(file.choose())
library(car)
#  0 De 2 "designs":
#  - Independent Samples Test (= hoe verschillend scoren 2 groepen gemiddeld bekeken?)
#  - Repeated measures (= als ik twee kenmerken meet van dezelfde personen, 
#    hoe verschillend scoren die personen dan voor beide kenmerken)

#  1 Gemiddelden per categorie/groep respondenten berekenen (enkel bij Indep.Sampl.Test relevant)

tapply(PiaacBel$PVLIT1,PiaacBel$GENDER,mean,na.rm=T)
tapply(PiaacBel$ICTHOME,PiaacBel$GENDER,mean,na.rm=T)


#  2 Betrouwbaarheidsintervallen 

#  2.1 BI per categorie/groep respondenten berekenen + visualiseren 
#      (enkel bij Indep.Sampl.Test relevant)

tapply(PiaacBel$PVLIT1,PiaacBel$GENDER,betr.interval)
tapply(PiaacBel$ICTHOME,PiaacBel$GENDER,betr.interval)

errorbar(PiaacBel$PVLIT1~PiaacBel$GENDER)
errorbar(PiaacBel$PVLIT1~PiaacBel$GENDER,
         main="95%-BI voor Geletterdheid naar Geslacht",
         xlab="Geslacht",ylab="Geletterdheid")

errorbar(PiaacBel$PVLIT1~PiaacBel$GENDER,
         p=0.8,
         barcol="red",
         main="80%-BI voor Geletterdheid naar Geslacht",
         xlab="Geslacht",ylab="Geletterdheid")

errorbar(PiaacBel$ICTHOME~PiaacBel$GENDER,
         p=0.8,
         barcol="black",
         main="80%-BI voor ICT thuis toepassen naar Geslacht",
         xlab="Geslacht",ylab="ICT thuis toepassen")


#  2.2 BI per categorie/groep respondenten berekenen + visualiseren 
#      (enkel bij Repeated Measures relevant)

betr.interval(PiaacBel$ICTHOME)
betr.interval(PiaacBel$ICTWORK)
errorbar2(PiaacBel$ICTHOME,PiaacBel$ICTWORK)

#  3 t-test uitvoeren
#  3.1 Independent Samples T-test

leveneTest(PiaacBel$PVLIT1,PiaacBel$GENDER)
t.test(PiaacBel$PVLIT1~PiaacBel$GENDER,var.equal=T)

leveneTest(PiaacBel$ICTHOME,PiaacBel$GENDER)
t.test(PiaacBel$ICTHOME~PiaacBel$GENDER,var.equal=F)

#  3.2 Repeated Measures (paired T-test)

t.test(PiaacBel$ICTHOME,PiaacBel$ICTWORK,var.equal=T,paired=T)

#  4 Effectgroottes!!!
d(PiaacBel$PVLIT1,PiaacBel$GENDER)
d(PiaacBel$ICTHOME,PiaacBel$GENDER)
dpaired(5.5279,2384)

# Uitsmijter: Hoe zit het in Nederland? Er is tevens data PiaacNedl.RData op
# Blackboard te vinden. Daar kan je dezelfde effecten nagaan. 
# Is daar het verschil tussen mannen en vrouwen eveneens statistisch significant?
# En waar is het effect van GENDER het grootst? In Nederland of in Vlaanderen?