x = rnorm(100)
y = x * 3 + rnorm(100)

# scale the data
x = (x - mean(x)) / sd(x)
y = (y - mean(y)) / sd(y)
dat = cbind(x, y)
smallex = prcomp(dat)

par(mfrow = c(1,2), pty = 's')
plot(x, y, main = 'Original Data', ylim = c(-4, 4), xlim = c(-4, 4))
plot(smallex$x[ ,1:2], main = 'First 2 PC scores', ylim = c(-4, 4), xlim = c(-4, 4))

# Our x, y axis are the same scale
sum(smallex$rotation[, 1]^2)
sum(smallex$rotation[, 2]^2)
# Our constraint: sum of the square of coefficient = 1



# Now, We will make the sd of the scores = 1 
# Yhe rotation will have different length
# The important axis will have longer length
# still score * axis

# STANDARLISED THE SCORES
# So each has sd = 1
scaleex = list(rotation = smallex$rotation, 
               x = smallex$x, 
               sd = smallex$sd)

# Divide each column by sd
for (i in 1:2) {
  scaleex$x[ ,i] = scaleex$x[,i] / scaleex$sd[i];
  scaleex$rotation[ ,i] = scaleex$rotation[ ,i] * scaleex$sd[i]
}

# score * rotation vector still has the same value
# the original data values if we use all components

plot(scaleex$x[,1:2], xlim = c(-4, 4), ylim = c(-4, 4),
     main = 'Scaled PC scores')

# It contains the correlation 
scaleex$rotation
# check
cor(x, scaleex$x[, 1])

# Easy to interpret with correlation

# Rotating
varimax(smallex$rotation)

### Real example
loans = read.csv("loans-subset.csv")
loans.pr = prcomp(loans[, 2:11], scale = TRUE)

plot(loans.pr,
     main = "PCA scree plot for scaled loan data",
     xlab = "Component")

round(loans.pr$sd^2 / sum(loans.pr$sd^2), 2)
loans.pr$sd
pairs(loans.pr$x[, 1:3], col = loans$Loans.loan_status,
      main = "First three PC", pch = ".")

# Two groups overlapping each other, nothing differentiate them

loans.prscaled = list(sd = loans.pr$sd[1:3], 
                      scores = loans.pr$x[ ,1:3], 
                      loadings = loans.pr$rotation[ ,1:3])

for (i in 1:3){
  loans.prscaled$loadings[ ,i] = loans.prscaled$loadings[ ,i] * loans.prscaled$sd[i]
  loans.prscaled$x[ ,i] = loans.prscaled$x[ ,i] / loans.prscaled$sd[i]}

# Make it a loading matrix so we can cut off

class(loans.prscaled$loadings) = "loadings"

# Dont show me anything less than 0.2 (blank)
print(loans.prscaled$loadings, cutoff = .2)

# Challenging interpretation

# Now rotate
varrot = varimax(loans.prscaled$loadings)
print(varrot$loadings, cutoff = .2)

# Fewer variables are left
# What does PC1 means? The size of loan
# Fewer variables are associated with each PC
# The variables with big coefficients are highly correlated


# PC2 - how much debt do you have

# PC3 - how long you been around, track record is?

# Rotate allows us to intepret it

# FACTOR ANALYSIS
loan.fac = factanal(loans[, 2:11], 3, scores = "regression")
loan.fac = factanal(loans[, 2:11], 3, scores = "regression", nstart = 10)


loan.fac$converged
loan.fac

# Uniqueness is not ok

# Drop one
loan.fac2 = factanal(loans[ ,3:11], 2, scores = "regression")
loan.fac2

# p value, not good, we dont have adequent factors

# Check factor scores
plot(loan.fac2$scores, pch = ".", col = loans[ ,12])


# We have some banding going on
