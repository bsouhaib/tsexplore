
#' Feature matrix from a list of time series
#'
#' \code{tabfeatures} computes the feature matrix from a list of time series
#' @param tslist a list of time series
#' @param features a vector of features to compute
#' @return A feature matrix.
#' @examples
#' mylist <- list(sunspot.year)
#' tabfeatures(mylist)
#' @export
tabfeatures <- function(tslist, features = c("entropy", "FoAcf", "trend", "seasonality")){
	nseries <- length(tslist)
	scaled_tslist  <- lapply(lapply(lapply(tslist, scale, center = TRUE, scale = TRUE), as.numeric), as.ts)
	trimmed_tslist <- lapply(tslist, Trim)
	
	tsp_tslist <- lapply(tslist, tsp)
	trimmed_tslist <- lapply(seq(length(trimmed_tslist)), 
		function(iseries){ series <- trimmed_tslist[[iseries]]; tsp(series) <- tsp_tslist[[iseries]]; return(series)}
	)
	scaled_tslist <- lapply(seq(length(scaled_tslist)), 
		function(iseries){ series <- scaled_tslist[[iseries]]; tsp(series) <- tsp_tslist[[iseries]]; return(series)})

	if(any(c("trend", "seasonality") %in% features)){
	   			varts_list <- lapply(seq(length(scaled_tslist)),
	   				function(iseries){ VarTS(scaled_tslist[[iseries]], tsp_tslist[[iseries]]) })
	   			# Why do we need to pass tsp to Varts if all series already contains the information?	
	   			#varts <- VarTS(scaled_series, tspx = tspy)
	 }
		
	mat <- sapply(features, function(f){
	   		
	   		switch(f,
        		entropy = sapply(tslist, Entropy),
         		FoAcf  = sapply(tslist, FoAcf),
         		trend = sapply(varts_list, function(x) ifelse("trend" %in% names(x), x$trend, 0)),
         		seasonality = sapply(varts_list, function(x)  ifelse("season" %in% names(x), x$season, 0) ),
         		#stop("Enter something that switches me!")
        		sapply(tslist, f)
         		)
	})
	colnames(mat) <- myfeatures
	#mat <- structure(mat, class = c("features", "matrix"))
	return(mat)	
}

#' Reduce the dimensionality of the feature matrix. Multiple methods are available.
#' @export
reducedim <- function(DT, method = c("PCA", "robPCA", "custom"), variables = NULL, retpca = FALSE){
	match.arg(method)
	if(is.null(rownames(DT))){
		rownames(DT) <- 1:nrow(DT)
	}
	if(method == "custom" && (is.null(variables))){
		stop("variables must be specified for the custom method")
	}
	if(retpca && !grepl("PCA", method)){
		stop("retpca = TRUE is only possible with PCA methods")
	}
	
	if(grepl("PCA", method)){
		if(method == "robPCA"){
			pca <- pcaPP::PCAproj(DT, k = 2, scale = sd, center = mean) # loadings and scores
			pca$loadings <- unclass(pca$loadings)
			
		}else if(method == "PCA"){
			pca <- prcomp(DT, center = TRUE, scale = TRUE) # x and rotation
			pca$loadings <- pca$rotation
			pca$scores <- pca$x
		}
		colnames(pca$scores) <- paste("PC", seq(ncol(pca$scores)), sep = "")
		colnames(pca$loadings) <- paste("PC", seq(ncol(pca$loadings)), sep = "")
		reducedDT <- pca$scores[, 1:2]
							                
	}else if(method == "custom"){
		reducedDT <- DT[, variables]
	}
	if(retpca){
		results <- list(DT = DT, reducedDT = reducedDT, method = method, pca = pca)
	}else{
		results <- list(DT = DT, reducedDT = reducedDT, method = method)
	}
	return(results)
}

#' Build a ggplot from the reduced feature matrix
#' @export
getplot <- function(obj, colouring = c("feature", "clustering", "custom"), feature = NULL, k = NULL, colours = NULL, pca.plotarrow = TRUE){
	match.arg(colouring)
	is.pca <- grepl("PCA", obj$method)

	if(is.pca){
		pca <- obj$pca
		
		varexp <- 100 * pca$sdev^2/sum(pca$sdev^2)
		xlabel <- paste("PC1 (", format(varexp[1], digits = 3),"% explained var.)", sep = "")
		ylabel <- paste("PC2 (", format(varexp[2], digits = 3),"% explained var.)", sep = "") 
		
		if(pca.plotarrow){
			# Information to plot arrows (loadings) LOADINGS ARROWS
			if(is.null(rownames(pca$loadings))){
				rownames(pca$loadings) <- 1:nrow(pca$loadings)
			}
			#browser()
			dataphi <- data.frame(obsnames = row.names(pca$scores), pca$scores)
			datapc <- data.frame(varnames = rownames(pca$loadings), pca$loadings)
			  mult <- min(
			    (max(dataphi[, "PC2"]) - min(dataphi[, "PC2"]) / (max(datapc[, "PC2"]) - min(datapc[, "PC2"]))),
			    (max(dataphi[, "PC1"]) - min(dataphi[, "PC1"]) / (max(datapc[, "PC1"]) - min(datapc[, "PC1"])))
			  )
			datapc <- transform(datapc, v1 = .5 * mult * (get("PC1")), v2 = .5 * mult * (get("PC2")))
		}
	}else{
		xlabel <- colnames(obj$reducedDT)[1]
		ylabel <- colnames(obj$reducedDT)[2]
	}
	
	if(colouring == "feature"){
			mycols <- c("#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C")
			myvalue <- obj$DT[, feature]
			maintitle <- feature
			myshapes <- factor(rep(19, length(myvalue)))
	}else if(colouring == "clustering"){
			res <- kmeans(obj$DT, centers = k)
			colours <- res$cluster
			myvalue <- factor(colours)
			maintitle <- "Colours based on clustering (in feature space)"
			myshapes <- factor(c(rep(19, length(myvalue)), rep(0, k)))
	}else if(colouring == "custom"){
			myvalue <- colours
			maintitle <- "Custom"
			myshapes <- factor(rep(19, length(myvalue)))
	}
	

	d <- data.frame(x = obj$reducedDT[, 1], y = obj$reducedDT[, 2], value = myvalue)
	
#	if(colouring == "clustering"){
#		infocenters <- cbind(res$centers %*%  pca$loadings[, 1:2], value = as.numeric(names(table(colours))))
#		colnames(infocenters) <- c("x", "y", "value")
#		d <- rbind(d, infocenters)
#	}
#	d <- data.frame(d, myshapes = myshapes)
	
	myplot <- ggplot(data = d, mapping = aes(x = x, y = y)) +
    			geom_point(aes(colour = value), cex = 1) +
    			ggtitle(maintitle) +
    			xlab(xlabel) +
				ylab(ylabel) 
				#+ theme(text = element_text(size = 15))
					
	if(colouring == "feature"){
		myplot <- myplot + scale_colour_gradientn(colours = mycols)
	}
	
	if(is.pca && pca.plotarrow){
		myplot <- myplot + 
			geom_segment(data = datapc, aes(x = 0, y = 0, xend = v1, yend = v2), arrow = arrow(length = unit(1/2, 'picas')), color = "red") +
			geom_text(data = datapc, aes(label = varnames, x = 1.4 * v1, y = 1.4 * v2), color = 'darkred', size = 3)
	}
		
	return(myplot)
}

