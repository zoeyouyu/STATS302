library(scatterplot3d)
#read.csv(file.choose()) -> LCMS
LCMS = read.csv(list.files(pattern = "LCMS"))
x = rnorm(100)
y = rnorm(100)
z = x + y + rnorm(100, sd = 0.1)
x = (x - mean(x)) / sd(x)
y = (y - mean(y)) / sd(y)
z = (z - mean(z)) / sd(z)
dat = cbind(x, y, z)

# PCA, scale or not?
smallex = prcomp(dat, scale = TRUE)

# What does it contain?
names(smallex)

# Scale, if 1, already scaled
smallex$scale

# Eigenvectors, coefficients used to create the PCs in rotation
smallex$rotation

# Scores 
smallex$x

# sd for 
smallex$sdev
apply(smallex$x, 2, sd)

# Screeplot, showing sd^2 = variance
screeplot(smallex)


## Showing 3D
scat = scatterplot3d(dat)

# Reconstruction using first 2 PCs
scat$points3d(t((smallex$rotation[, 1:2]) %*% t(smallex$x[,1:2])), col = 2)

# *10 just to make it longer, showing the direction of PC1
scat$points3d(rbind(c(0, 0, 0), smallex$rotation[, 1] * 10), type = 'l', col = 3)

# Showing direction for PC2
scat$points3d(rbind(c(0, 0, 0), smallex$rotation[, 2] * 10), type = 'l', col = 3)


LCMS = read.csv('LCMS.reduced.csv')
names(LCMS)

# Divide data into 2 subsets
samp.info = LCMS[, 1:2]
LCMS = LCMS[, -c(1:2)]

# We have 10 batches
table(samp.info$Batch)

# Check data
hist(LCMS[, 1])
# Comment, highly skewed, so we need to log the data
LCMS = log(LCMS)
hist(LCMS[, 1])

# Do we have missing data?
image(is.na(LCMS))

# Data cleaning
# Count how many missing in each column
missing = apply(is.na(LCMS), 2, sum)
hist(missing)

LCMS = LCMS[, missing == 0]
sds = apply(LCMS, 2, sd)
LCMS = LCMS[, sds > 0.001]


# PCA
LCcomp = prcomp(LCMS, center = T, scale = T)
screeplot(LCcomp)

# Or
plot(LCcomp$sd^2, xlab = 'component', 
     ylab = 'variance', main = 'LCMS screeplot')
abline(h = 1)

# how much variance first 3 PCs represented?
LCcomp$sd[1:3]^2/sum(LCcomp$sd^2)

scatterplot3d(LCcomp$x[, 1:3])
pairs(LCcomp$x[, 1:5])

# Is batch important?
scatterplot3d(LCcomp$x[, 1:3], color = samp.info$Batch)
# comment, we can see clustering by color, so yes, batch is important






