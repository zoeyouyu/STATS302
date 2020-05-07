library(MASS)
diabetes = read.csv("diabetes-bivar.csv")

# Linear Discriminant Analysis
diabetes.lda = lda(group ~ log(glutest) + insulin, data = diabetes)

# Input equal prior probabilities, we have 3 groups here
diabetes.eqprior = lda(group ~ log(glutest) + insulin, prior = c(1/3, 1/3, 1/3), 
                       data = diabetes)
# Check in the output
diabetes.eqprior$prior

# if not specified
diabetes.lda$prior
# Some useful summaries for original df
diabetes.lda$counts
diabetes.lda$means
diabetes.lda$N

# Coefficients for the canonical variates are stored in scaling
diabetes.lda$scaling

# Corresponding eigenvalues
diabetes.lda$svd

# Get the scores
diabetes.predict = predict(diabetes.lda)
plot(diabetes.predict$x, col = diabetes$group)

# Works because we have 2 columns, 2 LD
# If we have more than 2 we can use pairs
pairs(diabetes.predict$x, col = diabetes$group) # same as above 

# Or, for selected columns of interest
plot(diabetes.predict$x[, 1], diabetes.predict$x[, 2], 
     col = diabetes$group) # same as above 

# To get an idea of classification accuracy
table(diabetes.predict$class, diabetes$group)


# (Same observation as lecture but slightly different probabilities)
round(diabetes.predict$posterior[32, ], 2)
#notice how changing the prior changes the posterior probabilities
eqprob.predict = predict(diabetes.lda, prior = c(1/3, 1/3, 1/3))
round(eqprob.predict$posterior[32, ], 2)


# posterior probabilities for observation 32. 
# misclassification, it is group 2 but predicted to be group 3

# find 32
diabetes[32, ]
log(429)
diabetes.predict$x[32, ]
abline(v = 0.562222)
abline(h = -0.5185387)


# To interpret our canonical variates, check the *loadings*
# the correlations of the new variables with the original variables
for (i in 1:2){print(paste("variate", i));
  for (j in 1:2){print(paste(names(diabetes)[j], 
                             round(cor(diabetes.predict$x[, i], diabetes[, j]), 2)))
  }
  }

# Here we see both variables are reflected in the first variate, 
# whereas the second variate reflects primarily insulin

## Use CV
diabetes.cv = lda(group ~ log(glutest) + insulin, data = diabetes, CV = TRUE)
table(diabetes.cv$class, diabetes$group)

# Write our own leave-one-out loop
predictions = matrix(NA, ncol = 2, nrow = nrow(diabetes))
for (i in 1:nrow(diabetes)){
  model = lda(group ~ insulin + log(glutest), data = diabetes[-1, ])
  for (j in 1:2){
    model.predict = predict(model, newdata = diabetes[i, ], dimen = j)
    predictions[i, j] = model.predict$class
  }
}

# Check accurate prediction
sum(predictions[, 1] == diabetes$group)
sum(predictions[, 2] == diabetes$group)

# To predict a new data point, supply `newdata`
diabetes.newobs = predict(diabetes.lda, newdata = list(insulin = 300, glutest = 430))
diabetes.newobs




