
# 
Shapes <- function(x, width, scale = T, FUN = mean, ...){
	nr <- length(x)
	if (nr %% width != 0) {
    	stop("width must be a divisor of the length of the series.")
  	}
  	shapes <- matrix(x, ncol = width, byrow= T)
  	if(scale){
  		dtotal <- apply(shapes, 1, sum)
  		idremove <- which(dtotal == 0)
  		if(length(idremove) > 0){
  	  	shapes <- shapes[-idremove, ]
  		  dtotal <- dtotal[-idremove]
  		}
  		shapes <- t(t(shapes) / dtotal)
  	}
  	xprofile <- apply(shapes, 2, FUN, ...)
  	return(xprofile)
}


# Trimmed time series elimating outliers's influence
Trim <- function(x, trim = 0.1) {
	qtl <- quantile(x, c(trim, 1 - trim), na.rm = TRUE)
	lo <- qtl[1L]
	hi <- qtl[2L]
	x[x < lo | x > hi] <- NA
	return(x)
}

# Lumpiness and stationarity using sliding windows
# cannot be used for yearly data --> Why? --> can be used for yearly data
Lump <- function(x, width) {
	nr <- length(x)
	lo <- seq(1, nr, by = width)
	up <- seq(width, nr + width, by = width)
	nsegs <- nr/width
  # Lumpiness
	varx <- sapply(1:nsegs, function(idx)
                 var(x[lo[idx]:up[idx]], na.rm = TRUE))
	lumpiness <- var(varx, na.rm = TRUE)
  # Degree of stationary
  meanx <- sapply(1:nsegs, function(idx) mean(x[lo[idx]:up[idx]], na.rm = TRUE))
  stationary <- var(meanx, na.rm = TRUE)
	return(list(lumpiness = lumpiness, stationary = stationary))
}

# Spectral entroy (using spectral_entropy from ForeCA package)
Entropy <- function(x) {
  entropy <- try(ForeCA::spectral_entropy(na.contiguous(x))[1L], silent = TRUE)
  if (class(entropy) == "try-error") {
    entropy <- NA
  }
  return(entropy)
}

# Autocorrelation function at lag 1
FoAcf <- function(x) {
  return(acf(x, plot = FALSE, na.action = na.exclude)$acf[2L])
}

# Level shift using rolling window
RLshift <- function(x, width) {
  rollmean <- RcppRoll::roll_mean(x, width, na.rm = TRUE)
  lshifts <- tryCatch(max(abs(diff(rollmean, width)), na.rm = TRUE),
                      warning = function(w) w)
  if (any(class(lshifts) == "warning")) {
    lshifts <- NA
  }
  return(lshifts)
}

RVarChange <- function(x, width) {
  rollvar <- RcppRoll::roll_var(x, width, na.rm = TRUE)
  vchange <- tryCatch(max(abs(diff(rollvar, width)), na.rm = TRUE),
                      warning = function(w) w)
  if (any(class(vchange) == "warning")) {
    vchange <- NA
  }
  return(vchange)
}

# Number of crossing points
Cpoints <- function(x) {
  midline <- diff(range(x, na.rm = TRUE))/2
  ab <- x <= midline
  lenx <- length(x)
  p1 <- ab[1:(lenx - 1)]
  p2 <- ab[2:lenx]
  cross <- (p1 & !p2) | (p2 & !p1)
  return(sum(cross))
}

# Flat spots using discretization
Fspots <- function(x) {
  cutx <- try(cut(x, breaks = 10, include.lowest = TRUE, labels = FALSE),
              silent = TRUE)
  if (class(cutx) == "try-error") {
    fspots <- NA
  } else {
    rlex <- rle(cutx)
    # Any flat spot
    return(max(rlex$lengths))
    # Low flat spots
    # ones <- (rlex$values == 1)
    # return(max(rlex$lengths[ones]))
  }
}

# Strength of trend and seasonality and spike
VarTS <- function(x, tspx) {
  x <- as.ts(x)
  tsp(x) <- tspx
  freq <- tspx[3]
  contx <- try(na.contiguous(x), silent = TRUE)
  len.contx <- length(contx)
  if (length(contx) <= 2 * freq || class(contx) == "try-error") {
    trend <- linearity <- curvature <- season <- spike <- peak <- trough <- NA
  } else {
    if (freq > 1L) {
      all.stl <- stl(contx, s.window = "periodic", robust = TRUE)
      starty <- start(contx)[2L]
      pk <- (starty + which.max(all.stl$time.series[, "seasonal"]) - 1L) %% freq
      th <- (starty + which.min(all.stl$time.series[, "seasonal"]) - 1L) %% freq
      pk <- ifelse(pk == 0, freq, pk)
      th <- ifelse(th == 0, freq, th)
      trend0 <- all.stl$time.series[, "trend"]
      fits <- trend0 + all.stl$time.series[, "seasonal"]
      adj.x <- contx - fits
      v.adj <- var(adj.x, na.rm = TRUE)
      detrend <- contx - trend0
      deseason <- contx - all.stl$time.series[, "seasonal"]
      peak <- pk * max(all.stl$time.series[, "seasonal"], na.rm = TRUE)
      trough <- th * min(all.stl$time.series[, "seasonal"], na.rm = TRUE)
      remainder <- all.stl$time.series[, "remainder"]
      season <- ifelse(var(detrend, na.rm = TRUE) < 1e-10, 0,
                       max(0, min(1, 1 - v.adj/var(detrend, na.rm = TRUE))))
    } else { # No seasonal component
      tt <- 1:len.contx
      trend0 <- fitted(mgcv::gam(contx ~ s(tt)))
      remainder <- contx - trend0
      deseason <- contx - trend0
      v.adj <- var(trend0, na.rm = TRUE)
    }
    trend <- ifelse(var(deseason, na.rm = TRUE) < 1e-10, 0,
                    max(0, min(1, 1 - v.adj/var(deseason, na.rm = TRUE))))
    n <- length(remainder)
    v <- var(remainder, na.rm = TRUE)
    d <- (remainder - mean(remainder, na.rm = TRUE))^2
    varloo <- (v * (n - 1) - d)/(n - 2)
    spike <- var(varloo, na.rm = TRUE)
    pl <- poly(1:len.contx, degree = 2)
    tren.coef <- coef(lm(trend0 ~ pl))[2:3]
    linearity <- tren.coef[1]
    curvature <- tren.coef[2]
  }
  if (freq > 1) { 
    return(list(trend = trend, season = season, spike = spike,
                peak = peak, trough = trough, linearity = linearity,
                curvature = curvature))
  } else { # No seasonal component
    return(list(trend = trend, spike = spike, linearity = linearity,
                curvature = curvature))
  }
}

# Kullback-Leibler score
KLscore <- function(x, window, threshold = dnorm(38)) {
  gw <- 100 # grid width
  xgrid <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = gw)
  grid <- xgrid[2L] - xgrid[1L]
  tmpx <- x[!is.na(x)] # Remove NA to calculate bw
  bw <- bw.nrd0(tmpx) 
  lenx <- length(x)
  if (lenx <= (2 * window)) {
    stop("I cannot compute KLscore when the length is too small.")
  }
  # Using binning algorithm to achieve efficiency but obsecure exact positions.
  # lastrep <- ceiling(lenx/5) 
  # group <- rep(1:lastrep, each = 5)[1:lenx]
  # midpoints <- aggregate(x, by = list(group), function(y) y[3L])[, 2]
  # dens.mat <- matrix(, nrow = lastrep, ncol = gw)
  # for (i in 1L:lastrep) {
  #   dens.mat[i, ] <- dnorm(xgrid, mean = midpoints[i], sd = bw)
  # }
  dens.mat <- matrix(, nrow = lenx, ncol = gw)
  for (i in 1L:lenx) {
    dens.mat[i, ] <- dnorm(xgrid, mean = x[i], sd = bw)
  }
  dens.mat <- pmax(dens.mat, threshold)
  rmean <- RcppRoll::roll_mean(dens.mat, n = window, na.rm = TRUE, fill = NA,
                               align = "right") # by column
  # lo <- seq(1, lastrep - window + 1)
  # hi <- seq(window + 1, lastrep)
  lo <- seq(1, lenx - window + 1)
  hi <- seq(window + 1, lenx)
  seqidx <- min(length(lo), length(hi))
  kl <- sapply(1:seqidx, function(i) sum(rmean[lo[i], ] *
               (log(rmean[lo[i], ]) - log(rmean[hi[i], ])) *
               grid, na.rm = TRUE))
  diffkl <- diff(kl, na.rm = TRUE)
  maxidx <- which.max(diffkl) + 1
  return(list(score = max(diffkl), change.idx = maxidx))
}

# Count missing values
CountNAs <- function(x) {
  return(sum(is.na(x)))
}
