#####
#  STATISTIEK B
#  Voorbeeldsessie 2: Verschil in gemiddelden (ANOVA)
#
#  1 Gemiddelden per categorie + BI
#  2 LeveneTest
#  3 Anova
#  4 Effectgrootte (Eta Kwadraat)
#  5 Post-hoc testen
#  Focus op volgende variabelen:
#  - PVLIT1 = Geletterdheid van de respondent 
#             Definitie:“understanding, evaluating, using and engaging with written texts 
#             to participate in society, to achieve one’s goals, and to develop one’s 
#             knowledge and potential"
#  - I_Q04b = How often do you relate new ideas to real life
#  - LEARNATWORK_WLE_CA = Mate waarin respondent leeropportuniteiten heeft op het werk (6 categorieën)
#  - ICTWORK (Index die aangeeft in welke mate de respondent voor z'n job ICT dient te hanteren)
#  - ICTHOME (Index die aangeeft in welke mate de respondent thuis ICT dient te hanteren)
#####
source(file.choose())
library(car)

#  0 De 2 categorische variabelen die we als onafhankelijke variabelen hanteren:

table(PiaacBel$LEARNATWORK_WLE_CA)
PiaacBel$LW<-PiaacBel$LEARNATWORK_WLE_CA
levels(PiaacBel$LW)<-c("1","2","3","4","5","6")
table(PiaacBel$LEARNATWORK_WLE_CA,PiaacBel$LW)
table(PiaacBel$I_Q04b)

#  1 Gemiddelden per categorie/groep respondenten berekenen + BI's

tapply(PiaacBel$PVLIT1,PiaacBel$LW,betr.interval)
tapply(PiaacBel$PVLIT1,PiaacBel$I_Q04b,betr.interval)

errorbar(PiaacBel$PVLIT1~PiaacBel$LW,
         main="95%-BI voor Geletterdheid naar Learning at work",
         xlab="Learning at work",ylab="Geletterdheid")

errorbar(PiaacBel$PVLIT1~PiaacBel$I_Q04b,
         main="95%-BI voor Geletterdheid naar Relate new ideas",
         xlab="Relate new ideas",ylab="Geletterdheid")

# 2  Levene Test

leveneTest(PiaacBel$PVLIT1,PiaacBel$LW)
leveneTest(PiaacBel$PVLIT1,PiaacBel$I_Q04b)

# 3  ANOVA
# Varianties gelijk (p-value > 0.05 bij Levene Test)
M1_LW<-aov(PiaacBel$PVLIT1~PiaacBel$LW)
M1_LW<-aov(PVLIT1~LW,data=PiaacBel)
summary(M1_LW)

# Varianties niet gelijk (p-value < 0.05 bij Levene Test)
M1_RI<-oneway.test(PVLIT1~I_Q04b,data=PiaacBel)
M1_RI

# 4 Effectgrootte
etasq(M1_LW)
etasq(aov(PVLIT1~I_Q04b,data=PiaacBel))

# 5 Post-hoc tests
# Varianties gelijk (p-value > 0.05 bij Levene Test)
M1_LW_Tukey<-TukeyHSD(M1_LW)
M1_LW_Tukey
plot(M1_LW_Tukey)

# Varianties niet gelijk (p-value < 0.05 bij Levene Test)
bonferroni(PiaacBel$PVLIT1,PiaacBel$I_Q04b)

# Uitsmijters: 
# Wat is het effect van "Learning opportunities at work" (LW) op ICT gebruik op het werk(ICTWORK)?
# Hoe zit het in Nederland? Repliceer de verschillende analyses voor Nederland.