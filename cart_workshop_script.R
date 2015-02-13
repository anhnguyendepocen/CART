# Clay Ford 
# UVa StatLab
# Classification and Regression Trees
# Spring 2015


# Building Trees ----------------------------------------------------------

# Load rpart package. rpart comes with base R installation:
library(rpart)


# Classification Tree

glaucoma <- read.csv("Glaucoma.csv")
# About Glaucoma.csv

# Contains 98 normal and 98 glaucomatous subjects matched by age and sex. The 
# dichotomous Class variable classifies eyes as "normal" or "glaucoma." The
# other 62 explanatory variables are derived from laser scanning images of the
# optic nerve head.

# Source: 
# Torsten Hothorn and Berthold Lausen (2003), Double-Bagging: Combining
# classifiers by bootstrap aggregation. Pattern Recognition, 36(6), 1303–1309.

# Torsten Hothorn and Berthold Lausen (2003), Bagging tree classifiers for laser
# scanning images: a data- and simulation-based strategy. Artificial 
# Intelligence in Medicine, 27(1), 65–79.

# Our goal is to construct a classification tree which is able to decide if an
# eye is normal or glaucoma based on laser image data.


# fit classification tree; "~ ." means use all predictors in data frame
gfit <- rpart(Class ~ ., data=glaucoma)

# plot tree
# create the tree branches
plot(gfit)
# add text to the tree
text(gfit)

# By default the vertical spacing between nodes is proportional to the error in
# the fit.

# Usually need to play with plot arguments to get the tree to look right.

plot(gfit, margin=0.05)
text(gfit)

plot(gfit, branch=0, uniform=TRUE, margin=0.05)
text(gfit)

plot(gfit, branch=0.5, margin=0.05, compress=TRUE)
text(gfit)

plot(gfit, margin=0.05, compress=TRUE)
text(gfit)


# playing with text arguments
# effect of use.n
plot(gfit, margin=0.1)
text(gfit, use.n=TRUE)

# effect of all
plot(gfit, margin=0.1)
text(gfit, all=TRUE)

# effect of use.n and all
plot(gfit, margin=0.1)
text(gfit, use.n=TRUE, all=TRUE)

# effect of use.n, all and cex
plot(gfit, margin=0.1)
text(gfit, use.n=TRUE, all=TRUE, cex=0.75)



# another option is to change R graphics parameters
op <- par(mar = rep(0.1, 4)) # make margins smaller in plot region
plot(gfit, margin=.05)
text(gfit, use.n = TRUE)
# restore default graphics parameters
par(op)



# About the tree: 

# Notice that of 62 predictors, only 5 were used in the construction of the 
# tree. So we have a type of variable selection happening. We might think of
# these 5 predictors as being "most important" to detecting glaucoma.

# Since the first split is on varg, we might think of varg being the most
# important factor in detecting glaucoma.

# Notice that even though all predictors are continuous numeric measurements, a 
# simple cutpoint, or splitting rule, has been selected for predicting 
# classification. There is indication of the relationship between predictors and
# Class.

# While the tree itself provides a decision making process for predicting 
# glaucoma, there is no summary displayed of how well it performs overall.


# Regression Tree

forbes <- read.csv("Forbes2000.csv")
# About Forbes.csv

# The Forbes 2000 list is a ranking of the world's biggest companies in 2004, 
# measured by sales, profits, assets and market value. It is not a random
# sample.

# Our goal is to construct a regression tree that shows simple relationships
# between profit and sales, assets and market value.

ftree <- rpart(profits ~ assets + marketvalue + sales, data=forbes)
plot(ftree, margin=0.05)
text(ftree)

# with uniform branches and n
plot(ftree, uniform=TRUE, margin=0.05)
text(ftree, use.n=TRUE)

# The leaves (terminal nodes) display the predicted mean profit. 


# We can also view trees in the console:
gfit
ftree
# The child nodes of node x are always 2x and 2x + 1



# tree construction and purity measures -----------------------------------

# The summary() function displays the tree constructions process. Beware, it can
# be quite long! Can use the file= argument to write the output to a given file
# name.

summary(gfit)

# Let's look at node 1, the root node

# Node number 1: 196 observations,    complexity param=0.6530612
# predicted class=glaucoma  expected loss=0.5  P(node) =1
# class counts:    98    98
# probabilities: 0.500 0.500 
# left son=2 (76 obs) right son=3 (120 obs)
# Primary splits:
#   varg < 0.209   to the left,  improve=44.01404, (0 missing)
#   vari < 0.0615  to the left,  improve=41.01677, (0 missing)


# expected loss=0.5
98 / (98 + 98)

# P(node) =1 because all observations are in the root node.
(98 + 98) / (98 + 98)

# probabilities: 0.500 0.500 
98 / (98 + 98)
98 / (98 + 98)

# left son=2 (76 obs) right son=3 (120 obs)
# 76 obs sent to node 2 and 
# 120 obs sent to node 3
# based on split varg < 0.209 
with(glaucoma, sum(varg < 0.209))
with(glaucoma, sum(varg >= 0.209))

# improve=44.01404
# This is the reduction in impurity
# It is tedious to calculate "by hand":

196 * (
  (1 * (2 * 98/196 * 98/196)) -      # (2 * 98/196 * 98/196) is the gini index for root node
  (76/196 * (2 * 6/76 * 70/76)) -    # (2 * 6/76 * 70/76) is the gini index for node 2
  (120/196 * (2 * 28/120 * 92/120))  # (2 * 28/120 * 92/120)is the gini index for node 3
    )


# compare selected split to 1st runner-up:
# varg < 0.209,   improve=44.01404,
# vari < 0.0615,  improve=41.01677

# It can sometimes help to compare splits with stripcharts:
op <- par(mfrow=c(1,2))
stripchart(varg ~ Class, data=glaucoma, main="varg < 0.209", vertical = T, pch=1, method = "jitter")
abline(h=0.209, col="red")
stripchart(vari ~ Class, data=glaucoma, main="vari < 0.0615", vertical = T, pch=1, method = "jitter")
abline(h=0.0615, col="red")
par(op)

# Let's look at node 2 for gfit; it's a terminal node

# Node number 2: 76 observations
# predicted class=glaucoma  expected loss=0.07894737  P(node) =0.3877551
# class counts:    70     6
# probabilities: 0.921 0.079 

# expected loss=0.07894737
6/(70 + 6)

# P(node) =0.3877551
(70+6)/196

# probabilities: 0.921 0.079 
70/(70+6)
6/(70+6)


# Let's look at node 3 for gfit; it's the parent node for nodes 6 and 7

# Node number 3: 120 observations,    complexity param=0.07142857
# predicted class=normal    expected loss=0.2333333  P(node) =0.6122449
# class counts:    28    92
# probabilities: 0.233 0.767 
# left son=6 (7 obs) right son=7 (113 obs)
# Primary splits:
#   mhcg < 0.1695  to the right, improve=8.738643, (0 missing)
#   mhci < 0.099   to the right, improve=7.836770, (0 missing)

# expected loss=0.2333333
28 / (28 + 92)

# P(node) =0.6122449
(28 + 92) / 196

# probabilities: 0.233 0.767
28 / (28 + 92)
92 / (28 + 92)

# compare selected split to 1st runner-up:
# mhcg < 0.1695  improve=8.738643,
# mhci < 0.099   improve=7.836770

# recall observations in node 3 have varg >= 0.209!
op <- par(mfrow=c(1,2))
stripchart(mhcg ~ Class, data=glaucoma, main="mhcg < 0.1695", 
           vertical = T, pch=1, method = "jitter",
           subset= varg >= 0.209)
abline(h=0.1695, col="red")
stripchart(mhci ~ Class, data=glaucoma, main="mhci < 0.099", 
           vertical = T, pch=1, method = "jitter",
           subset= varg >= 0.209)
abline(h=0.099, col="red")
par(op)



summary(ftree)


printcp(gfit)

# Tree too big; fit is too good
gfitB <- rpart(Class ~ ., data=glaucoma, control = rpart.control(minsplit = 2))
plot(gfitB, margin=0.05, uniform=TRUE, main="A Tree too big")
text(gfitB, cex=0.7, use.n=TRUE)

# Tree too small; ignores important predictors
gfitS <- rpart(Class ~ ., data=glaucoma, control = rpart.control(minsplit = 50))
plot(gfitS, margin=0.05, uniform=TRUE, main="A Tree too small")
text(gfitS, use.n=TRUE)


boxplot(varg ~ Class, data=glaucoma, main="varg")
abline(h=0.209, col="red")

boxplot(mhcg ~ Class, data=glaucoma, subset= varg > .209, main="mhcg")
abline(h=0.1695, col="red")


# view details of tree
gfit
# view summary of tree building process
summary(gfit)
# Displays CP (complexity parameter) table
printcp(gfit)
# Plot a Complexity Parameter Table
plotcp(gfit)


# using tree package
# grow a large tree using gini split criteria
g.tr <- tree(Class ~ ., data=glaucoma, split="gini")
plot(g.tr)
text(g.tr, cex=0.8) # cex means character expansion; set to 80% normal size
summary(g.tr)

# 446) mhci < -0.005 5   5.004 normal ( 0.20000 0.80000 ) *
# how to calculate 5.004
# deviance = -2*(1*log(0.2) + 4*log(0.8))
g.tr

# more on deviance, see p. 21, 24 of CDA (Agresti)

# how to calculate deviance for tree
sum(g.tr$frame$dev[g.tr$frame$var=="<leaf>"])





# create a training set
train <- sample(nrow(glaucoma), round(0.5*nrow(glaucoma)))
# fit and plot tree from training set
gfit <- rpart(Class ~ ., data=glaucoma, subset= train)
op <- par(mar = rep(0.1, 4))
plot(gfit)
text(gfit)
par(op)

# predict test set
test.p <- predict(gfit, newdata=glaucoma[-train,], type = "class")
# misclassification table
table(test.p, glaucoma[-train,"Class"])



fit <- rpart(skips ~ Opening + Solder + Mask + PadType + Panel,
             data = solder, method = "anova")

plot(fit)
text(fit)

summary(residuals(fit))


plot(predict(fit),residuals(fit))


fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
plot(fit)
text(fit)


summary(residuals(fit))

residuals(fit) # 0 = successful prediction; 1 = misclassified
table(residuals(fit)) # number of successful, misclassified
# confusion matrix
table(kyphosis$Kyphosis, predict(fit, type="class"))






z.auto <- rpart(Mileage ~ Weight, car.test.frame)
post(z.auto, file = "")   # display tree on active device
# now construct postscript version on file "pretty.ps"
# with no title
post(z.auto, file = "pretty.ps", title = " ")
z.hp <- rpart(Mileage ~ Weight + HP, car.test.frame)
post(z.hp)
