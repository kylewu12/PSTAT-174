######################################################
# Author: Prof. Gareth W. Peters
# Date: 19/01/2023
# Purpose: Demonstrate the Box-Cox Transform
# The boxcox function from the MASS package in R
######################################################

# set up releant packages and libraries

# install.packages("MASS")
# install.packages("ggplot2")
library(MASS)
library(ggplot2)

# Example of synthetic data

x <- c(0.2, 0.528, 0.11, 0.260, 0.091,
       1.314, 1.52, 0.244, 1.981, 0.273,
       0.461, 0.366, 1.407, 0.79, 2.266)

######################
# Visualise the data
######################

# Basic Histogram
hist(x)

# kernel density estimate
p1 <- density(x)

plot(p1)

# Basic violin plot
p2 <- ggplot(as.data.frame(x), aes(x=rep(1,length(x)), y=x)) + 
  geom_violin()

p2

# Rotate the violin plot
p2 + coord_flip()


# Set trim argument to FALSE
ggplot(as.data.frame(x), aes(x=rep(1,length(x)), y=x)) + 
  geom_violin(trim=FALSE)


#########################################
# Lets do the Box-Cox transformation
#########################################

#REMARK: You must compute a linear model with the lm function and pass it to 
#the boxcox function as shown below in order to determine the appropriate 
#“lambda”:

boxcox(lm(x ~ 1))

# You get back the 95% confidence interval of the estimation, 
# and the dashed vertical line in the middle represents the estimated 
# parameter lambda hat.

# The best choice is to apply the logarithmic transformation of the 
# data because the preceding plot indicates that the 0 is inside the 
# confidence interval of the optimal “lambda” and because the estimation of 
# the parameter in this example is quite near to 0. 
# (see the table in the accompanying notes).

# Transformed data
x_Transformed <- log(x)

# Histogram
hist(x_Transformed)

# kernel density estimate
p1 <- density(x_Transformed)

plot(p1)

# The data now appears to be more closely following a normal distribution, 
# but you can also run a statistical test like the Shapiro-Wilk test to \
# make sure:
  
shapiro.test(x_Transformed)

# It shows we lack evidence to reject the null hypothesis of normalcy 
# because the p-value is higher than the typical levels of significance 
# (1%, 5%, and 10%).


# We can also do a little better by extracting the EXACT optimal value of lambda

# You can determine the actual lambda using the following code if the 
# confidence interval of the estimated parameter doesn’t fit with any of 
# the table’s values:
  
x_bc <- boxcox(lm(x ~ 1))

# Exact lambda
lambda <- x_bc$x[which.max(b$y)]

lambda

# Using the expression from the notes, you can now transform the variable:
  
x_Transformed_exact_BC <- (x ^ lambda - 1) / lambda

# Histogram
p1 <- density(x_Transformed_exact_BC )

plot(p1)


