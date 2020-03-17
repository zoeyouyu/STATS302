# How does matrix works?
getwd()
possum = read.csv("possum.csv")
head(possum)
# A column - vector
c(1:12)
x = matrix(c(1:12), ncol = 3)

# * does element-wise multiplication
x * y

# Matrix multiplication, remember must be m x n * n x z
y = matrix(c(1:12), ncol = 3, byrow = TRUE)

t(x) %*% y


# Diag
# create an identity matrix, nrow = 10
diag(1, 10)

# Create a matrix with diagonal entry as 1, 2, 3, ..., 10
diag(1:10)

# Pull out the diganal entry of a matrix
diag(x)



possum[1:10, ]
# 7 sites
table(possum$site)

# 2 types
table(possum$Pop)

# only interested in these cols
possum[, 7:15]
?cor
# Getting correlations between column 7 to 15
cor(possum[, 7:15])

round(cor(possum[, 7:15]), 2)

## footlength NA, missing values!!

?image
image(is.na(possum[, 7:15]))

# inverse of the matrix
possum.cor = cor(possum[, 7:15], use = "complete.obs")
ipossum = solve(possum.cor)

# Multiply them together, should get identity
possum.cor %*% ipossum

# Rounding will help
round(possum.cor %*% ipossum, 10)

# For loop to help plot pairs plots
morph = possum[,7:15]
par(mfrow = c(3, 3))
for(i in 1:9){
  hist(morph[,i], main = names(morph)[i], xlab='')
}

# Summary Stats
apply(morph, 2, sd, na.rm = TRUE)

# Visualizing the correlation matrix
# Quick one
par(mfrow = c(1, 1))
image(cor(morph, use = "pairwise.complete.obs"))


library(ggplot2)
library(reshape2)
PcorMelt = melt(possum.cor) # have a look---what has this done?
names(PcorMelt) = c("Measurement1", "Measurement2", "Correlation")
ggplot(data = PcorMelt, 
       aes(x = Measurement1, y = Measurement2, fill = Correlation)) + 
  geom_tile()