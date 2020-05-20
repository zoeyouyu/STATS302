
library(vegan)
library(mdatools)
EPL = read.csv("EPLDataset.csv")
# remove the outliner
EPL = EPL[-1112, ]
home = EPL[ ,c(3,7,9,11,13,15,17,19,21,27)]
away = EPL[ ,c(seq(4,22,2),28)]

# Run Canonical Analysis using CCorA, keep track of x and y
EPL.cor = CCorA(Y = home, X = away, permutations = 5000)
names(EPL.cor)

# Is there actually relationship? 
# Check the permutation based p-value
# The p-value based in Pillai's trace requires multivariate normality
EPL.cor$p.perm
EPL.cor$p.Pillai

EPL.cor$RDA.Rsquares
EPL.cor$RDA.adj.Rsq

# Standarlised data 
EPL.st = CCorA(Y = home, X = away, stand.Y = TRUE, stand.X = TRUE, permutations = 5000)
EPL.st$RDA.Rsquares
EPL.st$RDA.adj.Rsq

# How many canonical variable pairs shoud we look at?
EPL.cor$CanCorr
# Maybe 2 or 3


# Consider interpretation - Look at the correlation between our original variables and the canonical variates scores
EPL.cor$corr.Y.Cy[4, 1]
# Gives the correlation of the 4th original variable in the 'home' dataset, 
# home_touch, and the 1st canonical variate derived from the *home* variables



EPL.cor$corr.Y.Cx[4, 1]
# This one shang1 gives correlation of the 4th original variable in the 'home' dataset, 
# home_touch, and the 1st canonical variate derived from the *away* variables
# Home_touch is highly correlated with both the home and away canonical variates from the first pair

# Biplot
# look at the associations of the first 2 canonical variate pairs 
# with the variables used to construct them

biplot(EPL.cor)


# Analyse with PLS
# Predict home (response) variables with away (predictor) variables
# Choose the number of componets (canonical variables) using CV
plsout = pls(away, home, scale = TRUE, cv = 10)
par(mfrow = c(3, 4))
for (i in 1:10) 
  plotRMSE(plsout, i)

# The second variable looks like the most important
names(away)[2]

plsout6 = selectCompNum(plsout, 6)
par(mfrow = c(3, 4))
for (i in 1:10) 
  plotVIPScores(plsout6,i)


# Now lets predict the outcomes (exclude the `goal`)
home.wins = EPL$winner
home.wins = factor(home.wins, labels = c("tie", "home", "away"))

wins.out = plsda(EPL[, 5:28], home.wins, cv = 10)

par(mfrow = c(1,3))
for (i in 1:3) 
  plotRMSE(wins.out, i)

wins.out8 = selectCompNum(wins.out, 8)

par(mfrow=c(1,1))
plotPredictions(wins.out8, pch=1)