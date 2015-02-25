# Clay Ford 
# UVa StatLab
# Classification and Regression Trees
# Spring 2015


# Building Trees ----------------------------------------------------------

# Load rpart package. rpart comes with base R installation:
library(rpart)


# Classification Tree

glaucoma <- read.csv("http://people.virginia.edu/~jcf2d/workshops/cart/Glaucoma.csv")
# About Glaucoma.csv

# Contains 98 normal and 98 glaucomatous subjects matched by age and sex. The 
# dichotomous Class variable classifies eyes as "normal" or "glaucoma." The
# other 62 explanatory variables are derived from laser scanning images of the
# optic nerve head.

# Our goal is to construct a classification tree which is able to decide if an
# eye is normal or glaucoma based on laser image data.

# Source: Torsten Hothorn and Brian Everitt (2006), A Handbook of Statistical
# Analyses using R.


# fit classification tree; "~ ." means use all predictors in data frame
gfit <- rpart(Class ~ ., data=glaucoma)

# plot tree
# create the tree branches
plot(gfit)
# add text to the tree
text(gfit)

# Usually need to play with plot arguments to get the tree to look right!

# add space around tree
plot(gfit, margin=0.05)
text(gfit)

# By default the vertical spacing between nodes is proportional to the
# "importance" of the predictor. uniform=TRUE changes that. branch=0 will make V
# shaped branches.

plot(gfit, branch=0, uniform=TRUE, margin=0.05)
text(gfit)

# playing around...
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
# classification. There is no indication of the relationship between predictors 
# and Class. For example, we don't estimate the changing likelihood of glaucoma
# as varg increases.


# Regression Tree

forbes <- read.csv("http://people.virginia.edu/~jcf2d/workshops/cart/Forbes2000.csv")
# About Forbes.csv

# The Forbes 2000 list is a ranking of the world's biggest companies in 2004, 
# measured by sales, profits, assets and market value. It is not a random
# sample.

# Our goal is to construct a regression tree that shows simple relationships
# between profit and sales, assets and market value.

# Source: Torsten Hothorn and Brian Everitt (2006), A Handbook of Statistical
# Analyses using R.

ftree <- rpart(profits ~ assets + marketvalue + sales, data=forbes)
plot(ftree, margin=0.05)
text(ftree)

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
# summary(gfit, file="summary.txt")

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

# P(node) = 1 because all observations are in the root node.
196 / 196

# probabilities: 0.500 0.500 
98 / 196
98 / 196

# left son=2 (76 obs) right son=3 (120 obs)
# 76 obs sent to node 2 and 120 obs sent to node 3 based on split varg < 0.209
sum(glaucoma$varg < 0.209)
sum(glaucoma$varg >= 0.209)

# improve=44.01404
# This is the reduction in impurity.
# It is tedious to calculate "by hand":

#  n[p(A)I(A) - p(A_L)I(A_L) -  p(A_R)I(A_R)]

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
stripchart(varg ~ Class, data=glaucoma, main="varg < 0.209", 
           vertical = T, pch=1, method = "jitter")
abline(h=0.209, col="red")
stripchart(vari ~ Class, data=glaucoma, main="vari < 0.0615", 
           vertical = T, pch=1, method = "jitter")
abline(h=0.0615, col="red")
par(op)

# Let's look at node 2 for gfit; it's a terminal node

# Node number 2: 76 observations
# predicted class=glaucoma  expected loss=0.07894737  P(node) =0.3877551
# class counts:    70     6
# probabilities: 0.921 0.079 

# expected loss = 0.07894737
6/(70 + 6)

# P(node) = 0.3877551
76/196

# probabilities: 0.921 0.079 
70/76
6/76


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
120 / 196

# probabilities: 0.233 0.767
28 / 120
92 / 120

# compare selected split to 1st runner-up:
# mhcg < 0.1695  improve=8.738643,
# mhci < 0.099   improve=7.836770


# calculate improvement
196 * (
  (120/196 * (2 * 28/120 * 92/120)) - # P(node 3) * node 3 purity
    (7/196 * (2 * 0/7 * 7/7)) -       # P(node 6) * node 6 purity
    (113/196 * (2 * 21/113 * 92/113)) # P(node 7) * node 7 purity 
)


# recall observations in node 3 have varg >= 0.209
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


# Surrogate splits

# These are splits that substitute for the primary split in the event an
# observation is missing a value for the primary split.


# Node number 1: 196 observations
# left son=2 (76 obs) right son=3 (120 obs)
# Primary splits:
#   varg < 0.209   to the left,  improve=44.01404, (0 missing)
# Surrogate splits:
#   varn < 0.0895  to the left,  agree=0.934, adj=0.829, (0 split)

# agree=0.934
# This is proportion of observations that split in the same direction as the
# primary.
vargC <- with(glaucoma, cut(varg, c(-0.1,0.209,2)))
varnC <- with(glaucoma, cut(varn, c(-0.1,0.0895,2)))
(tab <- table(vargC,varnC))
# The surrogate split sends 183 of the 196 obs in the same direction as the
# primary split. Hence the agree value is 183/196 = 0.934
(66 + 117)/196
# or
round(sum(diag(tab))/sum(tab),3)

# adj=0.829

# This is the adjusted agreement that takes into account the "majority rule". In
# other words, if there were no surrogates we could send observations in the 
# same direction as the majority. In this case the majority of 120 went to the 
# right. If we substract 120 from the numerator and denominator of our original
# "agree" calculation we get the adjusted agreement:
round((183 - 120)/(196 - 120),3)

# Variable importance
# From the Introduction to Rpart:

# "A variable may appear in the tree many times, either as a primary or a
# surrogate variable. An overall measure of variable importance is the sum of
# the goodness of split measures for each split for which it was the primary
# variable, plus goodness * (adjusted agreement) for all splits in which it was
# a surrogate. In the printout these are scaled to sum to 100 and the rounded
# values are shown, omitting any variable whose proportion is less than 1%."


# Variable importance
# varg vars varn vari  rnf vbri mhcg mhcn  eas mhcs phcn abrs mhct phcg  hic vbss  tms   as  eag 
#   18   14   14   13   12   10    3    2    1    1    1    1    1    1    1    1    1    1    1 

# we see that varg, vars, varn, vari, rnf and vbri were the 6 most important
# variables.


# summary of regression tree
summary(ftree)


# Node number 1: 1995 observations,    complexity param=0.2374845
# mean=0.3811328, MSE=3.115265 
# left son=2 (1962 obs) right son=3 (33 obs)
# Primary splits:
#   marketvalue < 89.335  to the left,  improve=0.23748450, (0 missing)
#   sales       < 112.85  to the left,  improve=0.13006880, (0 missing)
# 

# drop records with missing profit to simplify R code a little
forbes <- subset(forbes, !is.na(profits))

# mean=0.3811328
with(forbes, mean(profits))

# MSE=3.115265 
with(forbes, sum((profits - mean(profits))^2)/nrow(forbes))


# improve=0.23748450
# This is the reduction in impurity
# It is tedious to calculate "by hand":
SS1 <- with(forbes, sum((profits - mean(profits))^2)) # parent
SS2 <- with(forbes[forbes$marketvalue < 89.335,], 
            sum((profits - mean(profits))^2))         # left child 
SS3 <- with(forbes[forbes$marketvalue >= 89.335,], 
            sum((profits - mean(profits))^2))         # right child
1 - (SS2 + SS3)/SS1

# Note this is also the R-squared if we regress profits on the grouping
# indicator (marketvalue >= 89.335):
summary(lm(profits ~ (marketvalue < 89.335), data=forbes))


# Compare primary splits:
#   marketvalue < 89.335  to the left,  improve=0.23748450, (0 missing)
#   sales       < 112.85  to the left,  improve=0.13006880, (0 missing)

# compare primary splits with stripcharts:
op <- par(mfrow=c(1,2))
stripchart(profits ~ (marketvalue < 89.335), data=forbes, main="marketvalue < 89.335", 
           vertical = T, pch=1, method = "jitter", col="grey80")
m <- aggregate(profits ~ (marketvalue < 89.335), data=forbes, mean)[,2]
points(x = c(1,2),y = m, col="red", pch=19)

stripchart(profits ~ (sales < 112.85), data=forbes, main="sales < 112.85",
           vertical = T, pch=1, method = "jitter", col="grey80")
m <- aggregate(profits ~ (sales < 112.85), data=forbes, mean)[,2]
points(x = c(1,2),y = m, col="red", pch=19)
par(op)


# Node number 2: 1962 observations,    complexity param=0.04258786
# mean=0.2695821, MSE=1.898379 
# left son=4 (1845 obs) right son=5 (117 obs)
# Primary splits:
#   marketvalue < 32.715  to the left,  improve=0.07106272, (0 missing)

# Again notice improve=0.07106272 is simply the R-squared when regressing 
# profits on the grouping indicator (marketvalue < 32.715) for those data with 
# marketvalue < 89.335.
summary(lm(profits ~ (marketvalue < 32.715), data=forbes, subset= marketvalue < 89.335))


# Tree pruning ------------------------------------------------------------

# The size of trees can be controled with the "control" paramter. Pass to 
# control the function rpart.control() with arguments. For example, below we 
# specify minsplit to be 2 and 90, respectively. This tells rpart the minimum
# number of observations that must exist in a node in order for a split to be
# attempted. The default minsplit setting is 20.

# Tree too big; fit is too good
gfitB <- rpart(Class ~ ., data=glaucoma, control = rpart.control(minsplit = 2))
plot(gfitB, margin=0.05, uniform=TRUE, main="A Tree too big")
text(gfitB, cex=0.7, use.n=TRUE)

# Tree too small; ignores important predictors
gfitS <- rpart(Class ~ ., data=glaucoma, control = rpart.control(minsplit = 90))
plot(gfitS, margin=0.10, uniform=TRUE, main="A Tree too small")
text(gfitS, use.n=TRUE)

# Pruning - classification tree

# printcp() prints a table of optimal prunings based on a complexity parameter.
printcp(gfit)

# recall this information is also available in the summary:
summary(gfit)

# It's also printed with each node:
# Node number 3: 120 observations,    complexity param=0.07142857

# This basically says node 3 becomes "afforadable" when alpha is 0.07. When the 
# cost-complexity is 0.07, node 3 is not pruned away but rather retained. This
# is the highest alpha we can afford to pay to keep node 3.

# plotcp() plots xerror vs cp. The dotted line is drawn 1SE above the minimum of
# the curve.
plotcp(gfit)

# Rule of thumb: select the smallest tree within one standard error of the
# smallest CV error.

# can also access cptable directly from rpart object
gfit$cptable

# Recall that the cptable is scaled by the number of misclassifications in a
# model with no splits.
gfit$cptable[,c(1,3:5)]*98

# to select and plot a pruned tree:
gfit2 <- prune(gfit, cp=0.22)
plot(gfit2, margin=0.2)
text(gfit2, use.n=TRUE)


# Pruning - regression tree
printcp(ftree)
plotcp(ftree)

# all trees appear to be within one SE of tree with smallest CV error. In this
# case let's just prune to the smallest xerror.

ftree2 <- prune(ftree, cp=0.029)
plot(ftree2, margin=0.1)
text(ftree2, use.n=TRUE)



# Making predictions with trees -------------------------------------------

# Classification trees
# Predictions for same data used to build tree

# Return probability predictions
predict(gfit2)
# vector of predicted classifications as a level number
predict(gfit2, type="vector")
# vector of predicted classifications as a level name
predict(gfit2, type="class")
# matrix of predicted class, class counts in terminal node, class probabilities
# in the node, and the proportion of observations in the terminal node.
predict(gfit2, type="matrix")

# create a confusion matrix
(cm <- table(predict(gfit2, type="class"), glaucoma$Class))
addmargins(cm)

# predicting with new data; must use a data frame; the predictors referred to in
# the right side of the formula must be present by name in newdata

# the following does not work
newd <- data.frame(varg=c(0.2,0.3), mhcg=c(0.1,0.2))
predict(gfit2, newdata = newd, type="class")

# a workaround in this case
newd <- glaucoma[sample(nrow(glaucoma),size=2),]
newd$varg <- c(0.2, 0.3)
newd$mhcg <- c(0.1, 0.2)
predict(gfit2, newdata = newd, type="class")

# Regression trees
# Predictions for same data used to build tree

# return predicted mean
predict(ftree2)




# Bagging and Random Forests ----------------------------------------------


# The decision on how to prune a tree can vary between runs of cross-validation.

# For example, let's build 50 trees and check the minimum CV standard error for
# the suggested amount of pruning

gfit <- rpart(Class ~ ., data=glaucoma)

# view the cp table
gfit$cptable 
# view "xerror" column of the cp table
gfit$cptable[,"xerror"] 
# get the location of the minimum xerror value
m <- which.min(gfit$cptable[,"xerror"]) 
# use that location to identify the number of splits
gfit$cptable[m,"nsplit"]

# now build a loop to do this 50 times
ns <- numeric(50)
for(i in 1:50){
  gfit <- rpart(Class ~ ., data=glaucoma)
  m <- which.min(gfit$cptable[,"xerror"]) 
  ns[i] <- gfit$cptable[m,"nsplit"]
}
table(ns)

# Notice the variability in the suggested number of tree prunings. Two methods 
# that help reduce the variance of decision trees and improve performace is
# bagging and random forests.

# bagging - bootstrap aggregation

# resample data (with replacement) and grow one tree (B=1)
bsamp <- sample(nrow(glaucoma), replace = T)
gl <- glaucoma[bsamp,]
gl.fitB <- rpart(Class ~ ., data=gl)
plot(gl.fitB, margin=0.1)
text(gl.fitB, use.n=TRUE, cex=0.9)
# make predictions on out-of-bag observations:
table(predict(gl.fitB, newdata = glaucoma[-bsamp,], type="class"), 
      glaucoma[-bsamp,]$Class)

# If we repeat the above we almost always get a different tree

# We do this B times (usually 100, or 500) and evaluate the prediction for each 
# observation. For example, if an observation was predicted glaucoma 10 times
# and normal 23 times, we would classify as "normal" (majority vote)


# We can use the randomForest package for both bagging and random forests.

# install.packages("randomForest")
library(randomForest)

# set mtry equal to the number of all predictors to perform bagging
set.seed(123)
bag.gfit <- randomForest(Class ~ ., data=glaucoma, mtry=62)
bag.gfit

# notice the confusion matrix is provided. To calculate it by hand:
table(glaucoma$Class, predict(bag.gfit))

# OOB estimate of error rate: 14.8%
# This is the total error rate
round((13+16)/nrow(glaucoma)*100,1)

# OOB = out-of-bag. This refers to those observations not used in building the 
# tree. To estimate the test error rate of a bagged tree, predictions are made
# on those observations NOT used to build the tree.

# Calling plot on the bag.gfit shows the classification error rates and OOB
# rates as the number of trees increases
plot(bag.gfit)
legend("topright", legend = c("OOB","glaucoma","normal"), col = 1:3, lty = 1:3)

# We can use the bagging object, bag.gfit, to make predictions. But instead of 
# going down one tree, we go down 500. The predicted classification that occurs
# most often (ie, the majority vote) is the classification.

# Variance importance plot
varImpPlot(bag.gfit)
varImpPlot(bag.gfit, n.var = 15)

# see which variables used in trees and how often
varUsed(bag.gfit)
data.frame(var=names(glaucoma)[-63], count=varUsed(bag.gfit))


# view one of the trees
getTree(bag.gfit, k=4, labelVar=TRUE)

# to see the "votes" for the observations
bag.gfit$votes

# to see how many times cases are "out-of-bag" and used for predictions
bag.gfit$oob.times
bag.gfit$oob.times/500
1-mean(bag.gfit$oob.times/500)

# see the Bootstrap Rule of 0.632: on average a bootstrap sample uses 0.632 of 
# the original observations, thus about 0.368 are used in checking the fit of a
# bagged tree.

# random forest
# mtry by default set to sqrt(p) for classification trees
set.seed(123)
rf.gfit <- randomForest(Class ~ ., data=glaucoma)
rf.gfit

# notice the confusion matrix is provided. To calculate it by hand:
table(glaucoma$Class, predict(rf.gfit))

# OOB estimate of error rate: 14.8%
# This is the total error rate
round((13+16)/nrow(glaucoma)*100,1)

# Also notice the number of variables tried at each split: 7

# Calling plot on the bag.gfit shows the classification error rates and OOB
# rates as the number of trees increases
plot(rf.gfit)
legend("topright", legend = c("OOB","glaucoma","normal"), col = 1:3, lty = 1:3)

# We can use the random forest object, rf.gfit, to make predictions. But instead
# of going down one tree, we go down 500. The predicted classification that
# occurs most often (ie, the majority vote) is the classification.

# variance importance plot
varImpPlot(rf.gfit)
varImpPlot(rf.gfit, n.var = 15)

# see which variables used in trees and how often
varUsed(rf.gfit)
data.frame(var=names(glaucoma)[-63], count=varUsed(rf.gfit))


# Extract a single tree from a forest.
getTree(rf.gfit, k=1, labelVar=TRUE)

# to see the "votes" for the observations
rf.gfit$votes

# to see how many times cases are "out-of-bag" and used for predictions
rf.gfit$oob.times
rf.gfit$oob.times/500
1-mean(rf.gfit$oob.times/500)


# regression

# bagging
# set mtry equal to the number of all predictors
set.seed(123)
bag.ftree <- randomForest(profits ~ assets + marketvalue + sales, data=forbes, 
                          mtry=3, ntree = 200)
bag.ftree
varImpPlot(bag.ftree)

# random forest
set.seed(123)
rf.ftree <- randomForest(profits ~ assets + marketvalue + sales, data=forbes, 
                         ntree = 200)
rf.ftree
varImpPlot(rf.ftree)



# trees vs. linear models -------------------------------------------------

# generate data
set.seed(123)
x1 <- runif(400,1,10)
x2 <- runif(400,1,10)
df <- data.frame(x1,x2)
df <- df[order(df$x1),]
# create grouping indicator based on boundary
df$b <- 0.5 + 1.2*seq(1,10,length=400)
fg <- function(x) if(x) rbinom(1,1,0.95) else rbinom(1,1,0.05)
df$g <- sapply(df$x2 > df$b,fg)
plot(x2 ~ x1, data=df, col=(g+3),pch=19)
abline(a=0.5,b=1.2)

# let's create a training set to build model;
# we'll evaluate the model with the data not used to build it;
train <- sample(400,200)

# linear model with confusion matrix;
glm1 <- glm(g ~ x1 + x2, data=df, subset=train, family="binomial")
# make predictions on held out data
p <- ifelse(predict(glm1, newdata = df[-train,], type="response")>0.5,1,0)
(tab <- table(df$g[-train],p))
sum(diag(tab))/sum(tab)

# tree model with confusion matrix
tree1 <- rpart(g ~ x1 + x2, data=df, subset=train, method = "class")
p <- predict(tree1, newdata = df[-train,], type="class")
(tab <- table(df$g[-train],p))
sum(diag(tab))/sum(tab)

# add fitted boundary
cs <- coef(glm1)
abline(a = -cs[1]/cs[3], b= -cs[2]/cs[3], lty=3, lwd=3)


# true decision boundary is non-linear
set.seed(123)
x1 <- runif(400,1,10)
x2 <- runif(400,1,10)
df <- data.frame(x1,x2)
# boundary
fg <- function(x) if(x) rbinom(1,1,0.95) else rbinom(1,1,0.05)
df$g <- sapply(df$x1 < 3 | df$x2 > 7,fg)
plot(x2 ~ x1, data=df, col=(g+3),pch=19)
abline(v=3,h=7)

# linear model with confusion matrix
glm2 <- glm(g ~ x1 + x2, data=df, subset=train, family="binomial")
p <- ifelse(predict(glm2, newdata = df[-train,], type="response")>0.5,1,0)
(tab <- table(df$g[-train],p))
sum(diag(tab))/sum(tab)

# tree model with confusion matrix
tree2 <- rpart(g ~ x1 + x2, data=df, subset=train, method = "class")
p <- predict(tree2, newdata = df[-train,],type="class")
(tab <- table(df$g[-train],p))
sum(diag(tab))/sum(tab)
tree2

# add fitted boundaries
abline(h=6.998292, v=3.029319, lty=3, lwd=3)

# END OF WORKSHOP SCRIPT
