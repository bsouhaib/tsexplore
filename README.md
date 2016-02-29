# tsexplore
[![Pending Pull-Requests](http://githubbadges.herokuapp.com/bsouhaib/tsexplore/pulls.svg?style=flat)](https://github.com/bsouhaib/tsfeatures/pulls)

The R package *tsexplore* provides methods for extracting various features from time series data.

# Installation
The **stable** version on R CRAN is coming soon.

You can install the **development** version from [Github](https://github.com/bsouhaib/tsfeatures)

```s
# install.packages("devtools")
library(devtools)
install_github("bsouhaib/tsexplore") 
```

# Usage (still in development)

```s
#library(tsexplore)

######## MAIN YAHOO ########
yahoo_DT <- c("dat0", "dat1", "dat2", "dat3", "dat4", "dat5")
data(list = yahoo_DT, package = "anomalous")

r <- lapply(yahoo_DT, function(name_DT){
  DT <- get(name_DT)
  return(lapply(seq(ncol(DT)), function(j){
    DT[, j]
  }))
})
list_real <- c(r[[1]], r[[2]], r[[3]], r[[4]])
list_simulated <- c(r[[5]], r[[6]])

# real world yahoo series
myfeatures <- c("entropy", "FoAcf", "trend", "seasonality")
mat <- tabfeatures(list_real, myfeatures)
idkeep <- which(apply(t(apply(mat, 1, is.na)), 1, sum) == 0)
matnona <- mat[idkeep, ]
obj <- reducedim(matnona, method = "robPCA", retpca = TRUE)
p1   <- getplot(obj, colouring = "feature", feature = "trend", pca.plotarrow = TRUE)
p2   <- getplot(obj, colouring = "clustering", k = 3)

obj <- reducedim(matnona, method = "custom", variables = c("entropy", "seasonality"))
p1   <- getplot(obj, colouring = "feature", feature = "trend")

print(p1)
print(p2)

# simulated yahoo series
myfeatures <- c("entropy", "FoAcf", "trend")
mat <- tabfeatures(list_simulated, myfeatures)
idkeep <- which(apply(t(apply(mat, 1, is.na)), 1, sum) == 0)
matnona <- mat[idkeep, ]
obj <- reducedim(matnona, method = "robPCA", retpca = TRUE)
p1   <- getplot(obj, colouring = "feature", feature = "trend", pca.plotarrow = TRUE)
p2   <- getplot(obj, colouring = "clustering", k = 3)
print(p1)
print(p2)


######## MAIN M3 ########
library(Mcomp)
alln <- unlist(lapply(M3, "[[", "n"))
allfreq <- unlist(lapply(lapply(M3, "[[", "x"), frequency))
alltypes <- unlist(lapply(M3, "[[", "type"))
allperiod <- unlist(lapply(M3, "[[", "period"))

nseries <- length(M3)
interval <- seq(nseries)

tslist <- lapply(M3[interval], "[[", "x")
types <- unlist(lapply(M3[interval], "[[", "type"))

myfeatures <- c("entropy", "FoAcf", "trend", "seasonality")
mat <- tabfeatures(tslist, myfeatures)

# We do not want missing values in mat
idkeep <- which(apply(t(apply(mat, 1, is.na)), 1, sum) == 0)
matnona <- mat[idkeep, ]
types <- types[idkeep]

mytypes <- substr(types, 1, 3)

####
obj <- reducedim(matnona, method = "robPCA", retpca = TRUE)
p1   <- getplot(obj, colouring = "feature", feature = "trend", pca.plotarrow = TRUE)
p2   <- getplot(obj, colouring = "clustering", k = 3)
p3   <- getplot(obj, colouring = "custom", colours = mytypes)
print(p1)
print(p2)
print(p3)

######## MAIN M4 ########
library(M4comp)
alln <- unlist(lapply(M4, "[[", "n"))
allfreq <- unlist(lapply(lapply(M4, "[[", "past"), frequency))
alltypes <- unlist(lapply(M4, "[[", "type"))
allperiod <- unlist(lapply(M4, "[[", "period"))

idremove <- unique(c(which(alln < 10), c(3168, 3224, 3249, 4473, 4673, which(allfreq > alln))))
nseries <- length(M4)
interval <- seq(nseries)
interval <- setdiff(interval, idremove)

tslist <- lapply(M4[interval], "[[", "past")
types <- unlist(lapply(M4[interval], "[[", "type"))

myfeatures <- c("entropy", "FoAcf", "trend", "seasonality")
mat <- tabfeatures(tslist, myfeatures)
idkeep <- which(apply(t(apply(mat, 1, is.na)), 1, sum) == 0)
matnona <- mat[idkeep, ]
types <- types[idkeep]

mytypes <- substr(types, 1, 3)

####
obj <- reducedim(matnona, method = "robPCA", retpca = TRUE)
p1   <- getplot(obj, colouring = "feature", feature = "trend", pca.plotarrow = TRUE)
p2   <- getplot(obj, colouring = "clustering", k = 3)
p3   <- getplot(obj, colouring = "custom", colours = mytypes)
print(p1)
print(p2)
print(p3)



```

# License

This package is free and open source software, licensed under GPL (>= 2).
