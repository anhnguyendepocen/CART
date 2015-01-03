# ch 10: Tree based methods
library(MASS)
library(rpart)
library(tree)


# regression tree example -------------------------------------------------


# using cpus data with MASS. cpus: Performance of Computer CPUs
# A relative performance measure and characteristics of 209 CPUs.
set.seed(123)
cpus.rp <- rpart(log10(perf) ~ ., cpus[,2:8], cp=1e-3)
cpus.rp # large tree
print(cpus.rp, cp=0.01)
# plot tree
plot(cpus.rp, uniform = T)
text(cpus.rp, digits=3)
# tree not yet pruned.

printcp(cpus.rp)
plotcp(cpus.rp)

# more on cp
# cp = alpha/R(T_null) = alpha/error of root tree

# R(T_null)
sum((log10(cpus$perf) - mean(log10(cpus$perf)))^2)/length(cpus$perf)

# prune the tree
cpus.rp1 <- prune(cpus.rp, cp=0.006)
print(cpus.rp1, digits=3)
plot(cpus.rp1, branch=0.4, uniform = TRUE)
text(cpus.rp1, digits=3)



# classification tree example ---------------------------------------------

# using forensic glass data with MASS: Measurements of Forensic Glass Fragments.
# The fgl data frame has 214 rows and 10 columns. It was collected by B. German 
# on fragments of glass collected in forensic work. Response is type, which has
# 6 classes.

set.seed(123)
fgl.rp <- rpart(type ~ ., fgl, cp = 0.001)
fgl.rp
plotcp(fgl.rp)
printcp(fgl.rp)

fgl.rp2 <- prune(fgl.rp, cp=0.02)
plot(fgl.rp2, uniform = T)
text(fgl.rp2, use.n=T)
fgl.rp2

# this produces a lot of output
summary(fgl.rp2)

# try entropy index (gini is the default)
set.seed(123)
fgl.rp3 <- rpart(type ~ ., fgl, cp = 0.001,
                 parms = list(split="information"))
plotcp(fgl.rp3)
printcp(fgl.rp3)
fgl.rp4 <- prune(fgl.rp3, cp=0.03)
op <- par(xpd=TRUE) # to prevent clipping of text labels
plot(fgl.rp4, uniform = T, compress = T, main="Plot of fgl.rp4")
text(fgl.rp4, use.n=T)
par(op)

# Plots section
plot(cpus.rp, branch=0.6, compress=T, uniform = T)
text(cpus.rp, digits=3, all=T, use.n=T)


# possible stuff for spring workshop
# tree regression
set.seed(12)
Y <- c(rnorm(10,10,0.5), rnorm(10,13,0.5), rnorm(10,18,0.5), rnorm(10,23,0.5))
X1 <-c(rnorm(20,3,0.5), rnorm(20,5,0.5))
X2 <- c(rnorm(20,10,0.5), rnorm(20,8,0.5))
DF <- data.frame(Y,X1,X2)
plot(DF)
library(rpart)
tout <- rpart(Y ~ X1 + X2, data=DF, method="anova")
plot(tout, xpd = NA)
text(tout)
tout

# example of changing rpart control
tout2 <- rpart(Y ~ X1 + X2, data=DF, method="anova", control = rpart.control(cp = 0))
plot(tout2, xpd = NA)
text(tout2)
tout2

# node 1 calculations (root; the top of the tree)
# split
length(DF$Y)
# deviance (ie, Residual sum of squares)
with(DF, sum((Y - mean(Y))^2))
# yval
mean(DF$Y)

# node 2 calculations
# split 
with(DF, sum(X2>=9.163165))
# deviance (ie, Residual sum of squares)
with(DF[X2 >= 9.163165,], sum((Y - mean(Y))^2))
# yval
with(DF, mean(Y[X2 >= 9.163165]))

# node 3 calculations
# split 
with(DF, sum(X2 < 9.163165))
# deviance (ie, Residual sum of squares)
with(DF[X2 < 9.163165,], sum((Y - mean(Y))^2))
# yval
with(DF, mean(Y[X2 < 9.163165]))

# node 6 calculations
# split 
with(DF, sum(X2 < 9.163165 & X1 >= 5.33777))
# deviance (ie, Residual sum of squares)
with(DF[X2 < 9.163165 & X1 >= 5.33777,], sum((Y - mean(Y))^2))
# yval
with(DF, mean(Y[X2 < 9.163165 & X1 >= 5.33777]))


# duplicate leaf calculations
mean(DF$Y[DF$X2 >= 9.163])
mean(DF$Y[DF$X2 < 9.163 & DF$X1 >= 5.338])
mean(DF$Y[DF$X2 < 9.163 & DF$X1 < 5.338])

# make a prediction for value with X1=4 and X2=10
predict(tout, newdata = data.frame(X1=4, X2=10))
predict(tout, newdata = data.frame(X1=6, X2=9))

# compare to least-squares regression
mout <- lm(Y ~ X1 + X2, data=DF)
predict(mout, newdata = data.frame(X1=4, X2=10))
predict(mout, newdata = data.frame(X1=6, X2=9))

# Which is better? Depends on your data and application.

# partition of the predictor space
# we can visualize this with two predictors
with(DF, plot(X1, X2, pch=20))
abline(h = 9.163) # X2 >= 9.163
segments(x0 = 5.338, y0 = 0, x1 = 5.338, y1 = 9.163)
points(X2 ~ X1, data=DF, subset= X2 >= 9.163, pch=20, col="red")
points(X2 ~ X1, data=DF, subset= X2 < 9.163 & X1 < 5.338, pch=20, col="darkgreen")
points(X2 ~ X1, data=DF, subset= X2 < 9.163 & X1 >= 5.338, pch=20, col="blue")
# label the regions:
text(x = 5, y = 10, labels = "Node 2")
text(x = 5.7, y = 8.5, labels = "Node 6")
text(x = 3, y = 8.5, labels = "Node 7")

# see a summary of tree building process
summary(tout)

# Where is 9.163165 coming from
# 9.163 is the midpoint between 9.362970 and 8.963359
sort(DF$X2)
9.362970 - ((9.362970 - 8.963359)/2)

# Let's walk through choosing the variable and split point.

# First we write a function to find midpoints in a vector of numbers

X2 <- sort(DF$X2)
tmp <- numeric(length(X2)-1)
for(i in (seq_along(tmp))){
  tmp[i] <- X2[i+1] - ((X2[i+1] - X2[i])/2)
}
tmp
# function to find midpoints between numbers
midp <- function(x){
  tmp <- numeric(length(x)-1)
  y <- sort(x)
  for(i in (seq_along(tmp))){
    tmp[i] <- y[i+1] - ((y[i+1] - y[i])/2)
  }
  tmp
}
# find and save midpoints in X1 and X2
X1m <- midp(DF$X1)
X2m <- midp(DF$X2)

# cycle through midpoints using them as splits and find minimum sum of squares

RSSX1 <- numeric(length(X1m))
for(i in seq_along(RSSX1)){
  R1 <- subset(DF, subset = X1 < X1m[i])
  R2 <- subset(DF, subset = X1 >= X1m[i])
  RSSX1[i] <- sum((R1$Y - mean(R1$Y))^2) + sum((R2$Y - mean(R2$Y))^2)
}

RSSX2 <- numeric(length(X2m))
for(i in seq_along(RSSX2)){
  R1 <- subset(DF, subset = X2 < X2m[i])
  R2 <- subset(DF, subset = X2 >= X2m[i])
  RSSX2[i] <- sum((R1$Y - mean(R1$Y))^2) + sum((R2$Y - mean(R2$Y))^2)
}

min(RSSX1)
min(RSSX2)
# minimum RSS in X2

# The split point in X2
X1m[which.min(RSSX1)]
X2m[which.min(RSSX2)]


# improve=0.8284586
# what is improve?
# The improvement listed is the percent change in SS for this split, i.e., 1 -
# (SSright + SSleft)/SSparent, which is the gain in R2 for the fit.


# Let's calculate "improvement" manually
SSright  <- with(DF[X2 < 9.163165,], sum((Y - mean(Y))^2))
SSleft <- with(DF[X2 >= 9.163165,], sum((Y - mean(Y))^2))
SSparent <- with(DF, sum((Y - mean(Y))^2))
(SSright + SSleft)/SSparent
1 - (SSright + SSleft)/SSparent

min(RSSX1)/20
min(RSSX2)/20




# tree classification
set.seed(12)
X1 <-c(rnorm(20,3,0.5), rnorm(20,5,0.5))
X2 <- c(rnorm(20,10,0.5), rnorm(20,8,0.5))

# generate classification
Y <- numeric(length = 40)
for(i in seq_along(X1)){
  Y[i] <- if(X1[i] > 4 & X2[i] < 9) rbinom(n = 1, size = 1, prob = 0.75) else 
    rbinom(n = 1, size = 1, prob = 0.25)
}
DF <- data.frame(Y=as.factor(Y),X1,X2)
summary(DF)

plot(X2 ~ X1, data=DF, col = Y, pch=19)

library(rpart)
tout2 <- rpart(Y ~ X1 + X2, data=DF, method = "class")
plot(tout2, xpd = NA, margin = 0.05)
text(tout2, use.n = TRUE)
tout2

# understanding printed output:
# 21  5 0 (0.7619048 0.2380952)
# > 5/21
# [1] 0.2380952
# > 16/21
# [1] 0.7619048

summary(tout2)
# split defaults to gini

# The improvement is n times the change in impurity index. The actual values of
# the improvement are not so important, but their relative size gives an
# indication of the comparative utility of the variables.

# calculate Gini index
K1 <- prop.table(table(subset(DF, X1 < 4.18732)$Y))
K2 <- prop.table(table(subset(DF, X1 >= 4.18732)$Y))
prod(K1) + prod(K2)

40*(0.5*0.5) - 40*(prod(K1) + prod(K2))


# rpart manual ------------------------------------------------------------



# work on rpart manual
# Node 2 example (p. 3)
# P(A = 2)
c1 <- (24/168)*22/24
c2 <- (144/168)*13/144
c1 + c2
35/168
# P(i = 1 | A = 2)
(24/168)*(22/24)/(c1 + c2)
22/35
0.6285714

# gini calculation for 2 groups
gini <- function(x) 2*x*(1-x)

# comparison of misclassification rate and gini
# two-class problem: 400 obs in each class
# NOTE: we take weighted averages below

# misclassification rate
# split 1: (300,100) and (100,300);  p = 0.25
0.5*(100/400) + 0.5*(100/400)
# split 2: (200,400) and (200,0); NOTE: 600/800 = 0.75
0.75*(200/600) + 0.25*(0/200) # same as split 1

# gini
# split 1: (300,100) and (100,300);  p = 0.25
0.5*gini(300/400) + 0.5*gini(100/400)
# split 2: (200,400) and (200,0); NOTE: 600/800 = 0.75
0.75*gini(200/600) + 0.25*gini(200/200) # lower (ie, lower impurity, hence split considered more "pure")


## impurity reduction (Delta I formula, p. 6 of rpart manual)
# misclassification rate
# delta I - split 1
0.5 - 0.5*(100/400) - 0.5*(100/400)
# delta I - split 2
0.5 - 0.75*(200/600) - 0.25*(0/200) # same as split 1 reduction

# gini
# delta I - split 1
gini(0.5) - 0.5*gini(100/400) - 0.5*gini(100/400)
# delta I - split 2
gini(0.5) - 0.75*gini(200/600) + 0.25*gini(0/200) # larger reduction in impurity


# gini and missclassification compared graphically for a given node
pm1 <- seq(0,1,0.01)
pm2 <- 1 - pm1
class <- 1 - pmax(pm1,pm2)
gini <- pm1*(1-pm1) + pm2*(1-pm2)
plot(pm1, class, type="l", xlab="p", ylab="Impurity", 
     main="Comparison of gini and misclassification for 2 groups")
lines(pm1, gini, type="l", col="red")
legend("topright", legend = c("Misclassification Error", "Gini Index"), 
       col = c("black", "red"),lty = 1)

# p=0.25, 1 - p = 0.75
# missclassification
abline(v=0.25, h=0.25, lty=3)
# gini
gini(0.25) + gini(0.75)
abline(h=0.375, lty=3, col="red")


# another comparison (from rpart manual, p. 5)
# comparison of misclassification rate and gini
# splits: 85% - 50% versus 70% - 70% purity

# misclassification rate
0.5*(0.15) + 0.5*(.50)
0.5*(0.30) + 0.5*(.30) # lower but we prefer the former

# gini
0.5*(gini(0.15)+gini(0.85)) + 0.5*(gini(0.50)+gini(0.50)) # lower, the one we prefer
0.5*(gini(0.30)+gini(0.70)) + 0.5*(gini(0.30)+gini(0.70))


# 3.3: example stage C prostate cancer
data(stagec)
head(stagec)
str(stagec)

stagec$pgstat <- factor(stagec$pgstat, levels=0:1, labels=c("No","Prog"))
cfit <- rpart(pgstat ~ . - pgtime, data=stagec, method="class")
print(cfit)
summary(cfit)
printcp(cfit)
plotcp(cfit)
# can also do this:
cfit$cptable
cptab <- printcp(cfit)
cptab[,3:5]
cptab[,3:5]*54

cfit2 <- prune.rpart(cfit, cp = 0.039)

op <- par(mar=rep(0.1, 4))
plot(cfit)
text(cfit, use.n=TRUE, cex=0.8)
par(op)
# pruned tree - example for presentation
op <- par(mar=rep(0.1, 4))
plot(cfit2, branch = 0.7, uniform=T)
text(cfit2, cex=0.8)
par(op)



# with tree package
library(tree)
tree.cfit <- tree(factor(pgstat) ~ . - pgtime, data=stagec, split="gini",
                  control = tree.control(nobs=146, mincut = 7, minsize = 20))
summary(tree.cfit)
cv.tree(tree.cfit, , prune.misclass)

# calculate "improve" as seen in summary(cfit) for node 1:
146*(gini(54/146) - ((52+9)/146)*gini(9/61) - ((40+45)/146)*gini(40/85))


# Plot a Complexity Parameter Table for an Rpart Fit
plotcp(cfit)
# for x-axis numbers, see bottom of page 13 in rpart manual
# 0.076 comes from...
sqrt(0.104938 * 0.055556)
# 0.104938 and 0.055556 come from the cp table
printcp(cfit)
summary(cfit, cp = 0.104)

# 4.3: example digit recognition
set.seed(1953)
n <- 200
y <- rep(0:9, length = 200)
temp <- c(1,1,1,0,1,1,1,    # 0
            0,0,1,0,0,1,0,  # 1
            1,0,1,1,1,0,1,  # 2
            1,0,1,1,0,1,1,  # 3
            0,1,1,1,0,1,0,  # 4
            1,1,0,1,0,1,1,  # 5
            0,1,0,1,1,1,1,  # 6
            1,0,1,0,0,1,0,  # 7
            1,1,1,1,1,1,1,  # 8
            1,1,1,1,0,1,0)  # 9
lights <- matrix(temp, 10, 7, byrow = TRUE) # The true light pattern 0-9
temp1 <- matrix(rbinom(n*7, 1, 0.9), nrow = n, ncol = 7) # 200 Noisy lights
temp2 <- ifelse(lights[y+1, ] == 1, temp1, 1-temp1) # adds noise to true light pattern;
# when true, replaces with temp1 value, which will be 1 90% of the time. When not true,
# replace with 1-temp1, which will be 0 about 90% of the time
temp3 <- matrix(rbinom(n*17, 1, 0.5), n, 17) # Random lights (17 columns of variables)
# bind the noisy lights (ones correct 90% of time with 17 add'l columns)
x <- cbind(temp2, temp3)


ifelse(lights == 1, temp1, 1-temp1)
ifelse(lights[y+1, ] == 1, temp1, 1-temp1)

dfit <- rpart(y ~ x, method='class',
              control = rpart.control(xval = 10, minbucket = 2, cp = 0))
printcp(dfit)

dfit$cptable[,3:5]*180

plotcp(dfit)


fit9 <- prune(dfit, cp = 0.02) # 9 splits, 10 terminal nodes
op <- par(mar = rep(0.1, 4))
plot(fit9, branch = 0.3, compress = TRUE)
text(fit9)
par(op)

op <- par(mar = rep(0.1, 4))
plot(dfit)
text(dfit)
par(op)



# trying to understand cp -------------------------------------------------


# 3.3: example stage C prostate cancer
data(stagec)
head(stagec)
str(stagec)
T_0 <- rpart(pgstat ~ . - pgtime, data=stagec, method="class", 
              control = rpart.control(minsplit = 20, xval=0, cp=0))
print(T_0)
summary(T_0)
printcp(T_0)
op <- par(mar=rep(0.1, 4))
plot(T_0)
text(T_0, use.n = TRUE, cex=0.8)
par(op)



# rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, 
#               maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
#               surrogatestyle = 0, maxdepth = 30, ...)
# 




# ESL - figure 9.3 --------------------------------------------------------


# gini, entropy and missclassification
pm1 <- seq(0,1,0.01)
pm2 <- 1 - pm1
class <- 1 - pmax(pm1,pm2)
gini <- pm1*(1-pm1) + pm2*(1-pm2)
# need to scale entropy so it passed through (0.5, 0.5)
scale <- 0.5/(-0.5*log(0.5) - 0.5*log(0.5))
entr <- (-1*pm1*log(pm1) - pm2*log(pm2))*scale
# figure 9.3 in ESL
plot(pm1, class, type="l", xlab="p", ylab="", ylim=c(0,0.6))
lines(pm1, gini, type="l", col="red")
lines(pm1, entr, type="l", col="blue")
legend("topright", legend = c("Misclassification Error", "Gini Index","Entropy"), 
       col = c("black", "red", "blue"),lty = 1)
