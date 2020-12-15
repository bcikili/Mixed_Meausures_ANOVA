library("rcompanion")
library("car")

suicide <- read.csv('suicide.csv')

str(suicide$suicides.100k.pop)

plotNormalHistogram(suicide$suicides.100k.pop)

suicide$suicides.100k.popSQRT <- sqrt(suicide$suicides.100k.pop)
plotNormalHistogram(suicide$suicides.100k.popSQRT)

suicide$suicides.100k.popLOG <- log(suicide$suicides.100k.pop)

suicide2 <- NaRV.omit(suicide)
plotNormalHistogram(suicide2$suicides.100k.popLOG)


leveneTest(suicides.100k.popLOG ~ generation, data=suicide2)
# it is significant, fails the assumption.

RManova1 <- aov(suicides.100k.popLOG~(generation*year)+Error(ï..country/(year)), suicide2)
summary(RManova1)
# it is significant so there is generation effect