fill = c("A" = "darkorange1", "B" = "mediumseagreen", "C" = "hotpink2", "D"="mediumpurple3", "E"="yellow", "F"="deepskyblue3", "G"="chocolate4")
g=ggplot(bhac, mapping=aes(bhac$V1)) + geom_density(aes(color="A", fill="A"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Humans in Each Environment", color="legend") + theme_bw() +scale_color_manual(values = fill)
g + theme(legend.position="bottom", legend.title=element_blank())
g=ggplot(bhac, mapping=aes(bhac$V1)) + geom_density(aes(fill="A"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Humans in Each Environment", color="legend") + theme_bw() +scale_color_manual(values = fill)
g + theme(legend.position="bottom", legend.title=element_blank())
g1=ggplot_add(bhbc$v1, g, bhbc) + geom_density(aes(bhbc$V1, fill="B"), alpha=0.5) + labs(color="legend")
g1 + theme(legend.position="bottom", legend.title=element_blank())
g=ggplot(bhac, mapping=aes(bhac$V1)) + geom_density(aes(fill="A"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Humans in Each Environment", fill="legend") + theme_bw() +scale_color_manual(values = fill)
g + theme(legend.position="bottom", legend.title=element_blank())
g1=ggplot_add(bhbc$v1, g, bhbc) + geom_density(aes(bhbc$V1, fill="B"), alpha=0.5) + labs(fill="legend")
g1 + theme(legend.position="bottom", legend.title=element_blank())
g=ggplot(bhac, mapping=aes(bhac$V1)) + geom_density(aes(fill="A"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Humans in Each Environment", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
g + theme(legend.position="bottom", legend.title=element_blank())
g1=ggplot_add(bhbc$v1, g, bhbc) + geom_density(aes(bhbc$V1, fill="B"), alpha=0.5) + labs(fill="legend")
g1 + theme(legend.position="bottom", legend.title=element_blank())
g2=ggplot_add(bhcc$v1, g1, bhcc) + geom_density(aes(bhcc$V1, fill="C"), alpha=0.5)
g2 + theme(legend.position="bottom", legend.title=element_blank())
g3=ggplot_add(bhdc$v1, g2, bhdc) + geom_density(aes(bhdc$V1, fill="D"), alpha=0.5)
g3 + theme(legend.position="bottom", legend.title=element_blank())
g4=ggplot_add(bhec$v1, g3, bhec) + geom_density(aes(bhec$V1, fill="E"), alpha=0.5)
g4 + theme(legend.position="bottom", legend.title=element_blank())
g5=ggplot_add(bhfc$v1, g4, bhfc) + geom_density(aes(bhfc$V1, fill="F"), alpha=0.5)
g5 + theme(legend.position="bottom", legend.title=element_blank())
g6=ggplot_add(bhgc$v1, g5, bhgc) + geom_density(aes(bhgc$V1, fill="G"), alpha=0.5)
g6 + theme(legend.position="bottom", legend.title=element_blank())
g6 + theme(legend.position="bottom", legend.box.just="bottom", legend.title=element_blank())
g6 + theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank(), )
bhan=bayesboot(humanA$nitrogen, weighted.mean, use.weights = TRUE)
bhbn=bayesboot(humanB$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
bhcn=bayesboot(humanC$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
bhdn=bayesboot(humanD$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
bhen=bayesboot(humanE$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
bhgn=bayesboot(humanG$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
bhfn=bayesboot(humanF$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
hn=ggplot(bhan, mapping=aes(bhan$V1)) + geom_density(aes(fill="A"), alpha=0.5) + xlab(expression(paste(delta^{15}, "N (\u2030)"))) + labs(title="Posterior Distribution of Humans in Each Environment", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
hn + theme(legend.position="bottom", legend.title=element_blank())
hn1=ggplot_add(bhbn$v1, hn, bhbn) + geom_density(aes(bhbn$V1, fill="B"), alpha=0.5) + labs(fill="legend")
hn1 + theme(legend.position="bottom", legend.title=element_blank())
hn2=ggplot_add(bhcn$v1, hn1, bhcn) + geom_density(aes(bhcn$V1, fill="C"), alpha=0.5) + labs(fill="legend")
hn2 + theme(legend.position="bottom", legend.title=element_blank())
hn3=ggplot_add(bhdn$v1, hn2, bhdn) + geom_density(aes(bhdn$V1, fill="D"), alpha=0.5) + labs(fill="legend")
hn3 + theme(legend.position="bottom", legend.title=element_blank())
hn4=ggplot_add(bhen$v1, hn3, bhen) + geom_density(aes(bhen$V1, fill="E"), alpha=0.5) + labs(fill="legend")
hn4 + theme(legend.position="bottom", legend.title=element_blank())
hn5=ggplot_add(bhfn$v1, hn4, bhfn) + geom_density(aes(bhfn$V1, fill="F"), alpha=0.5) + labs(fill="legend")
hn5 + theme(legend.position="bottom", legend.title=element_blank())
hn6=ggplot_add(bhgn$v1, hn5, bhgn) + geom_density(aes(bhgn$V1, fill="G"), alpha=0.5) + labs(fill="legend")
hn6 + theme(legend.position="bottom", legend.title=element_blank())
baac=bayesboot(herbivoresA$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
babc=bayesboot(herbivoresB$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
bacc=bayesboot(herbivoresC$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
badc=bayesboot(herbivoresD$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
baec=bayesboot(herbivoresE$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
bafc=bayesboot(herbivoresF$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
bagc=bayesboot(herbivoresG$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
ac=ggplot(baac, mapping=aes(baac$V1)) + geom_density(aes(fill="A"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in Each Environment", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
ac + theme(legend.position="bottom", legend.title=element_blank())
ac1=ggplot_add(babc$v1, ac, babc) + geom_density(aes(babc$V1, fill="B"), alpha=0.5) + labs(fill="legend")
ac1 + theme(legend.position="bottom", legend.title=element_blank())
ac2=ggplot_add(bacc$v1, ac1, bacc) + geom_density(aes(bacc$V1, fill="C"), alpha=0.5)
ac2 + theme(legend.position="bottom", legend.title=element_blank())
ac3=ggplot_add(badc$v1, ac2, badc) + geom_density(aes(badc$V1, fill="D"), alpha=0.5)
ac3 + theme(legend.position="bottom", legend.title=element_blank())
ac4=ggplot_add(baec$v1, ac3, baec) + geom_density(aes(baec$V1, fill="E"), alpha=0.5)
ac4 + theme(legend.position="bottom", legend.title=element_blank())
ac5=ggplot_add(bafc$v1, ac4, bafc) + geom_density(aes(bafc$V1, fill="F"), alpha=0.5)
ac5 + theme(legend.position="bottom", legend.title=element_blank())
ac6=ggplot_add(bagc$v1, ac5, bagc) + geom_density(aes(bagc$V1, fill="G"), alpha=0.5)
ac6 + theme(legend.position="bottom", legend.title=element_blank())
baan=bayesboot(herbivoresA$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
babn=bayesboot(herbivoresB$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
bacn=bayesboot(herbivoresC$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
badn=bayesboot(herbivoresD$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
baen=bayesboot(herbivoresE$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
bafn=bayesboot(herbivoresF$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
bagn=bayesboot(herbivoresG$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
an=ggplot(bhan, mapping=aes(baan$V1)) + geom_density(aes(fill="A"), alpha=0.5) + xlab(expression(paste(delta^{15}, "N (\u2030)"))) + labs(title="Posterior Distribution of Animals in Each Environment", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
hn + theme(legend.position="bottom", legend.title=element_blank())
an + theme(legend.position="bottom", legend.title=element_blank())
hn=ggplot(bhan, mapping=aes(bhan$V1)) + geom_density(aes(fill="A"), alpha=0.5) + xlab(expression(paste(delta^{15}, "N (\u2030)"))) + labs(title="Posterior Distribution of Humans in Each Environment", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
ac=ggplot(baac, mapping=aes(baac$V1)) + geom_density(aes(fill="A"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Domestic Herbivores in Each Environment", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
ac + theme(legend.position="bottom", legend.title=element_blank())
ac1=ggplot_add(babc$v1, ac, babc) + geom_density(aes(babc$V1, fill="B"), alpha=0.5) + labs(fill="legend")
ac1 + theme(legend.position="bottom", legend.title=element_blank())
ac2=ggplot_add(bacc$v1, ac1, bacc) + geom_density(aes(bacc$V1, fill="C"), alpha=0.5)
ac2 + theme(legend.position="bottom", legend.title=element_blank())
ac3=ggplot_add(badc$v1, ac2, badc) + geom_density(aes(badc$V1, fill="D"), alpha=0.5)
ac3 + theme(legend.position="bottom", legend.title=element_blank())
ac4=ggplot_add(baec$v1, ac3, baec) + geom_density(aes(baec$V1, fill="E"), alpha=0.5)
ac4 + theme(legend.position="bottom", legend.title=element_blank())
ac5=ggplot_add(bafc$v1, ac4, bafc) + geom_density(aes(bafc$V1, fill="F"), alpha=0.5)
ac5 + theme(legend.position="bottom", legend.title=element_blank())
ac6=ggplot_add(bagc$v1, ac5, bagc) + geom_density(aes(bagc$V1, fill="G"), alpha=0.5)
ac6 + theme(legend.position="bottom", legend.title=element_blank())
an=ggplot(baan, mapping=aes(baan$V1)) + geom_density(aes(fill="A"), alpha=0.5) + xlab(expression(paste(delta^{15}, "N (\u2030)"))) + labs(title="Posterior Distribution of Domestic Herbivores in Each Environment", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
an + theme(legend.position="bottom", legend.title=element_blank())
an1=ggplot_add(babn$v1, an, babn) + geom_density(aes(babn$V1, fill="B"), alpha=0.5) + labs(fill="legend")
an1 + theme(legend.position="bottom", legend.title=element_blank())
an2=ggplot_add(bacn$v1, an1, bacn) + geom_density(aes(bacn$V1, fill="C"), alpha=0.5) + labs(fill="legend")
an2 + theme(legend.position="bottom", legend.title=element_blank())
an3=ggplot_add(badn$v1, an2, badn) + geom_density(aes(badn$V1, fill="D"), alpha=0.5) + labs(fill="legend")
an3 + theme(legend.position="bottom", legend.title=element_blank())
an4=ggplot_add(baen$v1, an3, baen) + geom_density(aes(baen$V1, fill="E"), alpha=0.5) + labs(fill="legend")
an4 + theme(legend.position="bottom", legend.title=element_blank())
an5=ggplot_add(bafn$v1, an4, bafn) + geom_density(aes(bafn$V1, fill="F"), alpha=0.5) + labs(fill="legend")
an5 + theme(legend.position="bottom", legend.title=element_blank())
an6=ggplot_add(bagn$v1, an5, bagn) + geom_density(aes(bagn$V1, fill="G"), alpha=0.5) + labs(fill="legend")
an6 + theme(legend.position="bottom", legend.title=element_blank())
herbivoresbronze=subset(herbivores, herbivores$CHRONOLOGY=="BRONZE AGE")
herbivoresiron=subset(herbivores, herbivores$CHRONOLOGY=="IRON AGE")
herbivoreslant=subset(herbivores, herbivores$CHRONOLOGY=="LATE ANTIQUITY")
herbivoresmed=subset(herbivores, herbivores$CHRONOLOGY=="MEDIEVAL")
hUMANmed=subset(human, human$CHRONOLOGY=="MEDIEVAL")
humanbronze=subset(human, human$CHRONOLOGY=="BRONZE AGE")
humanlant=subset(human, human$CHRONOLOGY=="LATE ANTIQUITY")
humaniron=subset(human, human$CHRONOLOGY=="IRON AGE")
abc=bayesboot(herbivoresbronze$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(abc)
#Bayesian bootstrap
#Number of posterior draws: 4000 
#Summary of the posterior (with 95% Highest Density Intervals):
#  statistic    mean        sd      hdi.low   hdi.high
#     V1    -18.02104   0.1414367  -18.27662  -17.72462
#Quantiles:
#  statistic   q2.5%      q25%   median      q75%     q97.5%
#     V1     -18.27638 -18.12037 -18.0293 -17.92769  -17.72458
#Call:
#  bayesboot(data = herbivoresbronze$carbon, statistic = weighted.mean, use.weights = TRUE, na.rm = TRUE)

abc=bayesboot(herbivoresiron$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
abc=bayesboot(herbivoresbronze$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
aic=bayesboot(herbivoresiron$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(aic)
  # Bayesian bootstrap
  # Number of posterior draws: 4000 
  # Summary of the posterior (with 95% Highest Density Intervals):
  #   statistic      mean        sd   hdi.low hdi.high
  # V1 -18.09031 0.2467396 -18.56377 -17.6024
  # 
  # Quantiles:
  #   statistic    q2.5%      q25%    median      q75%   q97.5%
  #   V1 -18.5387 -18.26803 -18.09969 -17.93277 -17.5666
  # 
  # Call:
  # bayesboot(data = herbivoresiron$carbon, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)
alc=bayesboot(herbivoreslant$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(alc)
# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic      mean        sd   hdi.low  hdi.high
# V1 -18.70643 0.1408365 -18.99835 -18.44577
# 
# Quantiles:
#   statistic     q2.5%      q25%    median      q75%    q97.5%
#   V1 -18.98302 -18.79944 -18.70648 -18.61287 -18.43146
# Call:
  # bayesboot(data = herbivoreslant$carbon, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

amc=bayesboot(herbivoresmed$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(amc)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic      mean        sd   hdi.low  hdi.high
# V1 -16.56131 0.6399128 -17.75524 -15.27311
# 
# Quantiles:
#   statistic     q2.5%     q25%    median      q75%    q97.5%
#   V1 -17.74102 -17.0115 -16.59035 -16.14096 -15.24817
# 
# Call:
  # bayesboot(data = herbivoresmed$carbon, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

fill = c("Bronze Age" = "darkslategray", "Iron Age" = "slategrey", "Late Antiquity" = "cornsilk3", "Medieval"="moccasin")
acc=ggplot(abc, mapping=aes(abc$V1)) + geom_density(aes(fill="Bronze Age"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Domestic Herbivores in Each Period", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
acc + theme(legend.position="bottom", legend.title=element_blank())
acc1=ggplot_add(aic$v1, acc, aic) + geom_density(aes(aic$V1, fill="Iron Age"), alpha=0.5) + labs(fill="legend")
acc1 + theme(legend.position="bottom", legend.title=element_blank())
acc2=ggplot_add(alc$v1, acc1, alc) + geom_density(aes(alc$V1, fill="Late Antiquity"), alpha=0.5) + labs(fill="legend")
acc2 + theme(legend.position="bottom", legend.title=element_blank())
acc3=ggplot_add(amc$v1, acc2, amc) + geom_density(aes(amc$V1, fill="Medieval"), alpha=0.5) + labs(fill="legend")
acc3 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Bronze Age" = "darkslategray", "Iron Age" = "gold4", "Late Antiquity" = "cornsilk3", "Medieval"="moccasin")
acc=ggplot(abc, mapping=aes(abc$V1)) + geom_density(aes(fill="Bronze Age"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Domestic Herbivores in Each Period", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
acc + theme(legend.position="bottom", legend.title=element_blank())
acc1=ggplot_add(aic$v1, acc, aic) + geom_density(aes(aic$V1, fill="Iron Age"), alpha=0.5) + labs(fill="legend")
acc1 + theme(legend.position="bottom", legend.title=element_blank())
acc2=ggplot_add(alc$v1, acc1, alc) + geom_density(aes(alc$V1, fill="Late Antiquity"), alpha=0.5) + labs(fill="legend")
acc2 + theme(legend.position="bottom", legend.title=element_blank())
acc3=ggplot_add(amc$v1, acc2, amc) + geom_density(aes(amc$V1, fill="Medieval"), alpha=0.5) + labs(fill="legend")
acc3 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Bronze Age" = "darkslategray", "Iron Age" = "gold4", "Late Antiquity" = "cornsilk4", "Medieval"="moccasin")
acc=ggplot(abc, mapping=aes(abc$V1)) + geom_density(aes(fill="Bronze Age"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Domestic Herbivores in Each Period", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
acc + theme(legend.position="bottom", legend.title=element_blank())
acc1=ggplot_add(aic$v1, acc, aic) + geom_density(aes(aic$V1, fill="Iron Age"), alpha=0.5) + labs(fill="legend")
acc1 + theme(legend.position="bottom", legend.title=element_blank())
acc2=ggplot_add(alc$v1, acc1, alc) + geom_density(aes(alc$V1, fill="Late Antiquity"), alpha=0.5) + labs(fill="legend")
acc2 + theme(legend.position="bottom", legend.title=element_blank())
acc3=ggplot_add(amc$v1, acc2, amc) + geom_density(aes(amc$V1, fill="Medieval"), alpha=0.5) + labs(fill="legend")
acc3 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Bronze Age" = "darkslategray", "Iron Age" = "gold4", "Late Antiquity" = "cornsilk3", "Medieval"="navajowhite")
acc=ggplot(abc, mapping=aes(abc$V1)) + geom_density(aes(fill="Bronze Age"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Domestic Herbivores in Each Period", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
acc + theme(legend.position="bottom", legend.title=element_blank())
acc1=ggplot_add(aic$v1, acc, aic) + geom_density(aes(aic$V1, fill="Iron Age"), alpha=0.5) + labs(fill="legend")
acc1 + theme(legend.position="bottom", legend.title=element_blank())
acc2=ggplot_add(alc$v1, acc1, alc) + geom_density(aes(alc$V1, fill="Late Antiquity"), alpha=0.5) + labs(fill="legend")
acc2 + theme(legend.position="bottom", legend.title=element_blank())
acc3=ggplot_add(amc$v1, acc2, amc) + geom_density(aes(amc$V1, fill="Medieval"), alpha=0.5) + labs(fill="legend")
acc3 + theme(legend.position="bottom", legend.title=element_blank())
abn=bayesboot(herbivoresbronze$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(abn)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic     mean         sd  hdi.low hdi.high
# V1 7.252561 0.08717962 7.088109 7.424508
# 
# Quantiles:
#   statistic    q2.5%     q25%   median     q75%   q97.5%
#   V1 7.082111 7.193442 7.253937 7.310005 7.421153
# 
# Call: bayesboot(data = herbivoresbronze$nitrogen, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

ain=bayesboot(herbivoresiron$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(ain)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic    mean        sd  hdi.low hdi.high
# V1 7.05822 0.1428521 6.779894 7.333613
# 
# Quantiles:
#   statistic    q2.5%     q25%   median     q75%   q97.5%
#   V1 6.787762 6.959528 7.055576 7.153352 7.342938
# 
# Call:
# bayesboot(data = herbivoresiron$nitrogen, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

aln=bayesboot(herbivoreslant$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(aln)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic     mean        sd  hdi.low hdi.high
# V1 7.975131 0.2618145 7.473653  8.50869
# 
# Quantiles:
#   statistic    q2.5%     q25%   median     q75%   q97.5%
#   V1 7.468113 7.799826 7.973129 8.141777 8.505984
# 
# Call: bayesboot(data = herbivoreslant$nitrogen, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

amn=bayesboot(herbivoresmed$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(amn)
# 
# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic     mean       sd  hdi.low hdi.high
# V1 10.55091 0.707241 9.081942 11.86698
# 
# Quantiles:
#   statistic    q2.5%    q25%   median     q75%   q97.5%
#   V1 9.097518 10.0836 10.57082 11.03405 11.89498
# 
# Call: bayesboot(data = herbivoresmed$nitrogen, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

acn=ggplot(abn, mapping=aes(abn$V1)) + geom_density(aes(fill="Bronze Age"), alpha=0.5) + xlab(expression(paste(delta^{15}, "N (\u2030)"))) + labs(title="Posterior Distribution of Domestic Herbivores in Each Period", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
acn + theme(legend.position="bottom", legend.title=element_blank())
acn1=ggplot_add(ain$v1, acn, ain) + geom_density(aes(ain$V1, fill="Iron Age"), alpha=0.5) + labs(fill="legend")
acn1 + theme(legend.position="bottom", legend.title=element_blank())
acn2=ggplot_add(aln$v1, acn1, aln) + geom_density(aes(aln$V1, fill="Late Antiquity"), alpha=0.5) + labs(fill="legend")
acn2 + theme(legend.position="bottom", legend.title=element_blank())
acn3=ggplot_add(amn$v1, acn2, amn) + geom_density(aes(amn$V1, fill="Medieval"), alpha=0.5) + labs(fill="legend")
acn3 + theme(legend.position="bottom", legend.title=element_blank())
hbc=bayesboot(humanbronze$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(hbc)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic   mean         sd   hdi.low  hdi.high
# V1 -18.14 0.05158319 -18.23895 -18.03604
# 
# Quantiles:
#   statistic    q2.5%      q25%    median      q75%    q97.5%
#   V1 -18.2371 -18.17438 -18.14141 -18.10631 -18.03218
# 
# Call:bayesboot(data = humanbronze$carbon, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

hic=bayesboot(humaniron$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(hic)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic     mean        sd   hdi.low  hdi.high
# V1 -16.6671 0.1183525 -16.90112 -16.42561
# 
# Quantiles:
#   statistic     q2.5%      q25%    median      q75%   q97.5%
#   V1 -16.90278 -16.74369 -16.66705 -16.59206 -16.4264
# 
# Call: bayesboot(data = humaniron$carbon, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

hlc=bayesboot(humanlant$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(hlc)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic      mean       sd   hdi.low  hdi.high
# V1 -16.93732 0.400056 -17.75758 -16.18989
# 
# Quantiles:
#   statistic     q2.5%      q25%    median      q75%   q97.5%
#   V1 -17.72078 -17.20438 -16.93833 -16.66916 -16.1453
# 
# Call: bayesboot(data = humanlant$carbon, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

hmc=bayesboot(hUMANmed$carbon, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(hmc)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic      mean       sd   hdi.low  hdi.high
# V1 -16.47005 0.253677 -16.95089 -15.95347
# 
# Quantiles:
#   statistic     q2.5%      q25%    median      q75%    q97.5%
#   V1 -16.94738 -16.64113 -16.47661 -16.30553 -15.94474
# 
# Call: bayesboot(data = hUMANmed$carbon, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

hcc=ggplot(hbc, mapping=aes(hbc$V1)) + geom_density(aes(fill="Bronze Age"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Humans in Each Period", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
hcc + theme(legend.position="bottom", legend.title=element_blank())
hcc1=ggplot_add(hic$v1, hcc, hic) + geom_density(aes(hic$V1, fill="Iron Age"), alpha=0.5) + labs(fill="legend")
hcc1 + theme(legend.position="bottom", legend.title=element_blank())
hcc2=ggplot_add(hlc$v1, hcc1, hlc) + geom_density(aes(hlc$V1, fill="Late Antiquity"), alpha=0.5) + labs(fill="legend")
hcc2 + theme(legend.position="bottom", legend.title=element_blank())
hcc3=ggplot_add(hmc$v1, hcc2, hmc) + geom_density(aes(hmc$V1, fill="Medieval"), alpha=0.5) + labs(fill="legend")
hcc3 + theme(legend.position="bottom", legend.title=element_blank())
hbn=bayesboot(humanbronze$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(hbn)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic     mean         sd  hdi.low hdi.high
# V1 12.85107 0.06781548 12.72006 12.98259
# 
# Quantiles:
#   statistic    q2.5%     q25%   median     q75%   q97.5%
#   V1 12.72036 12.80353 12.85094 12.89776 12.98286
# 
# Call: bayesboot(data = humanbronze$nitrogen, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

hin=bayesboot(humaniron$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(hin)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic     mean         sd  hdi.low hdi.high
# V1 12.92441 0.09290196 12.75161 13.11346
# 
# Quantiles:
#   statistic    q2.5%     q25%   median     q75%   q97.5%
#   V1 12.74096 12.86067 12.92656 12.98693 13.10525
# 
# Call: bayesboot(data = humaniron$nitrogen, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

hin=bayesboot(humaniron$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
hln=bayesboot(humanlant$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(hln)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic     mean      sd  hdi.low hdi.high
# V1 12.46521 0.16671 12.14746 12.79701
# 
# Quantiles:
#   statistic    q2.5%     q25%   median     q75%   q97.5%
#   V1 12.14741 12.35182 12.46399 12.57544 12.79658
# 
# Call: bayesboot(data = humanlant$nitrogen, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

hmn=bayesboot(hUMANmed$nitrogen, weighted.mean, use.weights = TRUE, na.rm=TRUE)
summary(hmn)

# Bayesian bootstrap
# Number of posterior draws: 4000 
# Summary of the posterior (with 95% Highest Density Intervals):
#   statistic     mean        sd hdi.low hdi.high
# V1 11.98995 0.1758627  11.639 12.33384
# 
# Quantiles:
#   statistic    q2.5%     q25%   median     q75%   q97.5%
#   V1 11.64453 11.87006 11.99109 12.10838 12.34433
# 
# Call: bayesboot(data = hUMANmed$nitrogen, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)

hcn=ggplot(hbn, mapping=aes(hbn$V1)) + geom_density(aes(fill="Bronze Age"), alpha=0.5) + xlab(expression(paste(delta^{15}, "N (\u2030)"))) + labs(title="Posterior Distribution of Humans in Each Period", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
hcn + theme(legend.position="bottom", legend.title=element_blank())
hcn1=ggplot_add(hin$v1, hcn, hin) + geom_density(aes(hin$V1, fill="Iron Age"), alpha=0.5) + labs(fill="legend")
hcn1 + theme(legend.position="bottom", legend.title=element_blank())
hcn2=ggplot_add(hln$v1, hcn1, hln) + geom_density(aes(hln$V1, fill="Late Antiquity"), alpha=0.5) + labs(fill="legend")
hcn2 + theme(legend.position="bottom", legend.title=element_blank())
hcn3=ggplot_add(hmn$v1, hcn2, hmn) + geom_density(aes(hmn$V1, fill="Medieval"), alpha=0.5) + labs(fill="legend")
hcn3 + theme(legend.position="bottom", legend.title=element_blank())
head(banc)
# The first 10 draws (out of 6) from the Bayesian bootstrap:
#   
#   V1
# 1 -17.11215
# 2 -17.46968
# 3 -16.88543
# 4 -17.14864
# 5 -17.20516
# 6 -17.18312
# .. ...
# 
# Use summary() to produce a summary of the posterior distribution.

fill = c("Nomadic" = "darkturquoise", "Urban" = "plum4")
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc + theme(legend.position="bottom", legend.title=element_blank())
anc1=ggplot_add(bauc$v1, anc, bauc) + geom_density(aes(bauc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
anc1 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Nomadic" = "darkslateblue", "Urban" = "plum4")
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc1 + theme(legend.position="bottom", legend.title=element_blank())
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc + theme(legend.position="bottom", legend.title=element_blank())
anc1=ggplot_add(bauc$v1, anc, bauc) + geom_density(aes(bauc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
anc1 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Nomadic" = "turquoise", "Urban" = "darkred")
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc + theme(legend.position="bottom", legend.title=element_blank())
anc1=ggplot_add(bauc$v1, anc, bauc) + geom_density(aes(bauc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
anc1 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Nomadic" = "cyan4", "Urban" = "orchid4")
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc + theme(legend.position="bottom", legend.title=element_blank())
anc1=ggplot_add(bauc$v1, anc, bauc) + geom_density(aes(bauc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
anc1 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Nomadic" = "mediumaquamarine", "Urban" = "indianred4")
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc + theme(legend.position="bottom", legend.title=element_blank())
anc1=ggplot_add(bauc$v1, anc, bauc) + geom_density(aes(bauc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
anc1 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Nomadic" = "mediumaquamarine", "Urban" = "mediumorchid4")
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc + theme(legend.position="bottom", legend.title=element_blank())
anc1=ggplot_add(bauc$v1, anc, bauc) + geom_density(aes(bauc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
anc1 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Nomadic" = "darkgreen", "Urban" = "firebrick")
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc + theme(legend.position="bottom", legend.title=element_blank())
anc1=ggplot_add(bauc$v1, anc, bauc) + geom_density(aes(bauc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
anc1 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Nomadic" = "turquoise", "Urban" = "firebrick")
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc + theme(legend.position="bottom", legend.title=element_blank())
anc1=ggplot_add(bauc$v1, anc, bauc) + geom_density(aes(bauc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
anc1 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Nomadic" = "darkseagreen1", "Urban" = "firebrick")
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc + theme(legend.position="bottom", legend.title=element_blank())
anc1=ggplot_add(bauc$v1, anc, bauc) + geom_density(aes(bauc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
anc1 + theme(legend.position="bottom", legend.title=element_blank())
fill = c("Nomadic" = "midnightblue", "Urban" = "firebrick")
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc + theme(legend.position="bottom", legend.title=element_blank())
anc1=ggplot_add(bauc$v1, anc, bauc) + geom_density(aes(bauc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
anc1 + theme(legend.position="bottom", legend.title=element_blank())
anc=ggplot(banc, mapping=aes(banc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Animals in Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
anc + theme(legend.position="bottom", legend.title=element_blank())
anc1=ggplot_add(bauc$v1, anc, bauc) + geom_density(aes(bauc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
anc1 + theme(legend.position="bottom", legend.title=element_blank())
aun=ggplot(bann, mapping=aes(bann$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{15}, "N (\u2030)"))) + labs(title="Posterior Distribution of Animals in  Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
aun + theme(legend.position="bottom", legend.title=element_blank())
aun1=ggplot_add(baun$v1, aun, baun) + geom_density(aes(baun$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
aun1 + theme(legend.position="bottom", legend.title=element_blank())
aun=ggplot(bann, mapping=aes(bann$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{15}, "N (\u2030)"))) + labs(title="Posterior Distribution of Animals in Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
aun + theme(legend.position="bottom", legend.title=element_blank())
aun1=ggplot_add(baun$v1, aun, baun) + geom_density(aes(baun$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
aun1 + theme(legend.position="bottom", legend.title=element_blank())
bhnc=bayesboot(data = humannomad$carbon, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)
bhuc=bayesboot(data = humanurban$carbon, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE)
hnc=ggplot(bhnc, mapping=aes(bhnc$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + labs(title="Posterior Distribution of Humans in Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
hnc + theme(legend.position="bottom", legend.title=element_blank())
hnc1=ggplot_add(bhuc$v1, hnc, bhuc) + geom_density(aes(bhuc$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
hnc1 + theme(legend.position="bottom", legend.title=element_blank())
bhnn=bayesboot(data = humannomad$nitrogen, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE) 
bhun=bayesboot(data = humanurban$nitrogen, statistic = weighted.mean,      use.weights = TRUE, na.rm = TRUE) 
hnn=ggplot(bhnn, mapping=aes(bhnn$V1)) + geom_density(aes(fill="Nomadic"), alpha=0.5) + xlab(expression(paste(delta^{15}, "N (\u2030)"))) + labs(title="Posterior Distribution of Humans in Urban v Nomadic Contexts", fill="legend") + theme_bw() +scale_fill_manual(values = fill)
hnn + theme(legend.position="bottom", legend.title=element_blank())
hnn1=ggplot_add(bhun$v1, hnn, bhun) + geom_density(aes(bhun$V1, fill="Urban"), alpha=0.5) + labs(fill="legend")
hnn1 + theme(legend.position="bottom", legend.title=element_blank())