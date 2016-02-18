
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
         		trend = sapply(varts_list, function(x) x$trend),
         		seasonality = sapply(varts_list, function(x) ifelse(is.null(x$season), NA, x$season) ),
         		stop("Enter something that switches me!")
         		)
	})
	#mat <- structure(mat, class = c("features", "matrix"))
	return(mat)	
}

visualize <- function(x, dimred = c("PCA", "robPCA", "custom"), colouring = c("features", "clustering", "custom"), varx = NULL, vary = NULL, colours = NULL, k = NULL, maintitle = NULL, plot.arrow = TRUE){
	match.arg(dimred)
	match.arg(colouring)
	
	if(is.null(rownames(x))){
		rownames(x) <- seq(nrow(x))
	}
	
	xlabel <- varx
	ylabel <- vary

	if(dimred %in%c("PCA", "robPCA")){
		if(dimred == "PCA"){
			pca <- pcaPP::PCAproj(x, k = 2, scale = sd, center = mean)
			pca$x <- pca$scores
		}else if(dimred == "robPCA"){
			pca <- prcomp(x, center = TRUE, scale = TRUE)
		}
		X <- pca$x[, 1]
		Y <- pca$x[, 2]
		
		varexp <- 100 * pca$sdev^2/sum(pca$sdev^2)
		
		# LOADINGS ARROWS
		#x <- "PC1"; y <- "PC2";
		#browser()
		rownames(pca$rotation) <- seq(nrow(pca$rotation))
		
		data <- data.frame(obsnames=row.names(pca$x), pca$x)
		datapc <- data.frame(varnames=rownames(pca$rotation), pca$rotation)
		  mult <- min(
		    (max(data[, "PC2"]) - min(data[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"]))),
		    (max(data[, "PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
		  )
		  datapc <- transform(datapc,
		                      v1 = .5 * mult * (get("PC1")),
		                      v2 = .5 * mult * (get("PC2")))

		  xlabel <- paste("PC1 (", format(varexp[1], digits = 3),"% explained var.)", sep = "")
		  ylabel <- paste("PC2 (", format(varexp[1], digits = 3),"% explained var.)", sep = "")   
							                
	}else if(dimred == "custom"){
		X <- x[, varx]
		Y <- x[, vary]
		
	}

	if(colouring == "features"){
		mycols <- c("#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C")
		nfeatures <- ncol(x)
		listplots <- vector("list", nfeatures)
		for(j in seq(nfeatures)){
			print(j)
			d <- data.frame(X, Y, value = x[, j])
			listplots[[j]] <- ggplot(data = d, mapping = aes(x = X, y = Y)) +
	     		geom_point(aes(colour = value), shape = 19) +
	     		ggtitle(colnames(x)[j]) +
	     		theme(text = element_text(size = 15)) +
	     		scale_colour_gradientn(colours = mycols) +
	     		xlab(xlabel) +
				ylab(ylabel) 
			
			if(dimred %in% c("PCA", "robPCA") && plot.arrow){
				listplots[[j]] <- listplots[[j]] + geom_segment(data = datapc, aes(x = 0, y = 0, xend = v1, yend = v2), arrow = arrow(length = unit(1/2, 'picas')), color = "red") +
					geom_text(data = datapc, aes(label = varnames, x = 1.4 * v1, y = 1.4 * v2), color = 'darkred', size = 3)
			}

		}
	}else{
		
		if(colouring == "clustering"){
			
			res <- kmeans(x, centers = k)
			colours <- res$cluster
			maintitle <- "Clustering-based (in feature space) colouring"
		}
		
		d <- data.frame(X, Y, value = factor(colours))
		listplots <- ggplot(data = d, mapping = aes(x = X, y = Y)) +
			geom_point(aes(colour = value), shape = 19) +
			ggtitle(maintitle) +
		 	xlab(xlabel) +
			ylab(ylabel) 
			#+theme(legend.text=element_text(size=2))
			
		if(dimred %in% c("PCA", "robPCA") && plot.arrow){
				listplots <- listplots + geom_segment(data = datapc, aes(x = 0, y = 0, xend = v1, yend = v2), arrow = arrow(length = unit(1/2, 'picas')), color = "red") +
					geom_text(data = datapc, aes(label = varnames, x = 1.4 * v1, y = 1.4 * v2), color = 'darkred', size = 3)
		}
		listplots <- list(listplots)
		
	}

	return(listplots)
}