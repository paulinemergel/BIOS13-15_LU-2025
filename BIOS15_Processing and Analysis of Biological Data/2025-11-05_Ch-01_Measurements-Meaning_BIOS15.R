### EXERCISES BIOS15 ###

################################################################################
## MEASUREMENT & MEANING

## Chapter 1 (of lecture notes)
## 2025-11-05

# Before starting to work on exercises in R, read the Appendix on 
# reproducibility, and think about whether you would like to create 
# a GitHub account for all your materials from this course.
################################################################################

## Placing data in a proportional scale

# Via natural logarithm
# log(a)-log(b) = log (a/b) = log(b/a)

log(1.1/1)
-log(1/1.1)

# Note also that when a (1.1) is 10% larger than b (1.0), the log ratio is ~0.1
log(1/1)
log(1.1/1)  # log ratio ~ 0.10
log(1.3/1)
log(1.7/1)  # log ratio lower: ~0.53

# Differences on log scale *100 => can be roughly interpreted as difference
# in percent

# Log ratios are often good measures of EFFECT SIZE *if* a and b are not too
# different



## Simulating data from statistical distributions

# Normal distribution
distribution <- function(x,m,n) {
  dist_values <- 0
  for (i in 1:x) {
  result <- rnorm(n=i, mean=m, sd=1)
  dist_values <- result + dist_values
  }
  print(dist_values)
}
distribution(100,5,20)
    ## does not work

x = rnorm(n=100,mean=5,sd=1)
mean(x)
sd(x)
hist(x,las=1, main="")



## BOOTSTRAPPING
# To ensure reproducibility of the results (i.e. that we will get the same 
# result every time, even when we work on different computers), we set 
# the “seed” of the random number generator using the set.seed() function
set.seed(1)
x=rnorm(50,10,2)
x
# standard error = square-root of (variance/sample size)
se_x=sqrt(var(x)/length(x))
se_x


# Non-parametric bootstrap
# resample data many times
# maintain original sample size BUT
# draw from data with replacement, so that by chance some values will be 
# sampled several times and others not at all

# define empty variable "out" that we will subsequently "fill"
out = NULL

# run for-loop
?sample
for(i in 1:1000){
  sample=sample(x,replace=T) # refers to the x=rnorm() above!
  out[i]=mean(sample)
}

out # contains the "sampling distribution" of the mean of x


# The standard deviation of the sampling distribution gives 
# an approximation of the standard error (and this is very different
# from the standard deviation of the original data!)
hist(out,las=1,main="")
sd(out)
se_x
  # => sd is close to the theoretical standard error 


# we have the full sampling distribution
# we can derive quantiles, such as the 95% confidence interval
quantile(out,c(0.025,0.975))

#Recall that we could also have obtained an approximation of the 
# 95% confidence interval as ±1.96SE. This follows from the properties 
# of the standard normal distribution (with a mean of zero and a 
# variance of one), for which the 2.5 and 97.5 percentiles falls 
# ~1.96 standard deviations from the mean. 
qnorm(c(0.025,0.975))
mean(x)-1.96*se_x
mean(x)+1.96*se_x



###################
## EXERCISE Ch.1-1
###################
# Use non-parametric bootstrapping to derive a 95% confidence 
# interval for the coefficient of variation (CV) of a variable. 
# Start by writing a function that computes the CV for a variable 
# (see the Appendix for a brief introduction writing functions in R). 
# Then, simulate a random variable and write a loop that samples 
# many times from this variable and computes the CV.

# 1) Data
# Sample data
library(sampledatasets)
data(arbuthnot_tbl)       # dataset: Male and female births in London
data <- arbuthnot_tbl
data
data$boys # each number represents the amount of boys born in a given year in London
data <- data$boys

# Alternatively: simulate data
?rnorm
datasim <- rnorm(150,mean=12,sd=2)
datasim


# 2) Coefficient of variation = CV
# CV = variance/standard deviation
# So let's calculate variance and standard deviation first, 
# then infer the CV from there

cv.data <- function(x){
  x <- as.numeric(x)        # make sure x == data is numeric and has no NAs
  x <- na.omit(x)
  result <- 0               # empty vector we want to fill
  #for(i in 1:length(x)){
  m <- mean(x)
  sum_sq_diff <- sum((x - m)^2)
  var <- sum_sq_diff / (length(x) - 1)
  sd <- sqrt(var)
  cv <- sd/m
  result <- result+cv
  #}
  return(result)
}
cv.data(data)
cv.data(datasim)

# 3) Sample many times from dataset "datasim"
datasim

resampling <- function(x,y){    # y = how many time to resample from data set
                                # can be </>/= original sample size
  data.resample <-rep(0,y)
  for (i in 1:y){
  sample=sample(x,replace=TRUE)
  data.resample[i]=mean(sample)
  #data.resample=sample
  }
  return(data.resample)
}

data.resampled <- resampling(datasim,150)
data.resampled
    ## why do I get more than 120 (or y) results??
    ## seems to work fine otherwise though


# 4) Compute CV from resample data "data.resampled
cv.data(data.resampled)


# 5) Confidence intervals of non-linear bootstrapped
hist(data.resampled,las=1,main="")
sd(data.resampled)
# Does the standard deviation of the sampling distribution give 
# an approximation of the standard error??
se_x <- sqrt(var(datasim)/length(datasim))
se_x
# ==> YES, they are about the same

q <- quantile(data.resampled,c(0.025,0.975))
q

mean(datasim)-1.96*se_x
mean(datasim+1.96+se_x) # quite a bit higher than for quantile(data.resampled ...) above
                        # correct?


# 6) Plot the results
plot(density(data.resampled))
## ?? line(q[[1]]) xxx
      ## Why does it NOT work? 
      ## HOW to add vertical lines for conf interval to density plot??


# 7) Optional Exercise --- The proportional properties of the natural log

# Use simulated data to show the close relationship between the SD of log-transformed data and the CV on 
# arithmetic scale. You may need e.g. the rnorm function and a for-loop to achieve this. One strategy would 
# be to start with comparing the two values for a single case, then build a matrix to hold the paired values, 
# and finally use a for-loop to populate the matrix. See Appendix 1 for help to get started with programming. 
# The following figure illustrates the kind of pattern we expect.

# x = cv(X)
# y = SD(log(X))
# rnorm()
# for loop
# matrix
# populate matrix via for loop


      #### NOT DONE YET ####





