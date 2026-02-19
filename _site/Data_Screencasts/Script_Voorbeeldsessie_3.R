#####
#  STATISTIEK B
#  Voorbeeldsessie 3: Kruistabel
#
#  1 Tabel aanmaken
#  2 Chi-kwadraat toets uitvoeren
#  3 Grafische weergave
#  Focus op volgende variabelen:
#  - I_Q04b = How often do you relate new ideas to real life
#  - LEARNATWORK_WLE_CA = Mate waarin respondent leeropportuniteiten heeft op het werk (6 categorieën)
#  - EDLEVEL3 = Hoogste opleidingsniveau

source(file.choose())
library(car)

#  0 De variabelen bekijken

table(PiaacBel$LEARNATWORK_WLE_CA)
table(PiaacBel$I_Q04b)
table(PiaacBel$EDLEVEL3)

#  1 Kruistabel aanmaken

table(PiaacBel$I_Q04b,PiaacBel$EDLEVEL3)
kruistabel.kolom(PiaacBel$I_Q04b,PiaacBel$EDLEVEL3)
kruistabel.kolom(PiaacBel$LEARNATWORK_WLE_CA,PiaacBel$EDLEVEL3)

# 2  Chi-kwadraat toets (verband ook statistisch significant?)

Tab1<-table(PiaacBel$I_Q04b,PiaacBel$EDLEVEL3)
Tab2<-table(PiaacBel$LEARNATWORK_WLE_CA,PiaacBel$EDLEVEL3)
chi.kwadraat.test(Tab1)
chi.kwadraat.test(Tab2)

# 3 Grafische weergave
Tab1b<-table(PiaacBel$EDLEVEL3,PiaacBel$I_Q04b)
Tab2b<-table(PiaacBel$EDLEVEL3,PiaacBel$LEARNATWORK_WLE_CA)
assocplot(Tab1b)
assocplot(Tab2b)

# Uitsmijters: 
# Hoe zit het in Nederland? Repliceer de verschillende analyses voor Nederland.