# http://deanattali.com/2015/04/21/r-package-shiny-app/

#mydata <- "M4"
mydata <- "YAHOO"
mydata <- "M3"

if(mydata == "M4"){
  alln <- unlist(lapply(M4, "[[", "n"))
  allfreq <- unlist(lapply(lapply(M4, "[[", "past"), frequency))
  alltypes <- unlist(lapply(M4, "[[", "type"))
  allperiod <- unlist(lapply(M4, "[[", "period"))
  
  idremove <- unique(c(which(alln < 10), c(3168, 3224, 3249, 4473, 4673, which(allfreq > alln))))
  nseries <- length(M4)
  interval <- seq(nseries)
  interval <- setdiff(interval, idremove)
  
  #n <- 10000; interval <- seq(n)
  interval <- sample(1000)
  
  tslist <- lapply(M4[interval], "[[", "past")
  types <- unlist(lapply(M4[interval], "[[", "type"))
  
  myfeatures <- c("entropy", "FoAcf", "trend", "seasonality")
  mat <- tabfeatures(tslist, myfeatures)
  idkeep <- which(apply(t(apply(mat, 1, is.na)), 1, sum) == 0)
  matnona <- mat[idkeep, ]
  types <- types[idkeep]
  
  mytypes <- substr(types, 1, 3)
  tslist <- tslist[idkeep]
  ####
  obj <- reducedim(matnona, method = "robPCA", retpca = TRUE)
  
  DT <- cbind(seq(nrow(matnona)), obj$pca$scores)
  #p1   <- getplot(obj, colouring = "feature", feature = "trend", pca.plotarrow = TRUE)
  #p2   <- getplot(obj, colouring = "clustering", k = 3)
  #p3   <- getplot(obj, colouring = "custom", colours = mytypes)
  
  fmat <- data.frame(matnona)
  
}else if(mydata == "YAHOO"){
  
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
  
  tslist <- list_real
  myfeatures <- c("entropy", "FoAcf", "trend")
  mat <- tabfeatures(tslist, myfeatures)
  
  idkeep <- which(apply(t(apply(mat, 1, is.na)), 1, sum) == 0)
  matnona <- mat[idkeep, ]
  
  obj <- reducedim(matnona, method = "robPCA", retpca = TRUE)
  DT <- cbind(seq(nrow(matnona)), obj$pca$scores)
  
  fmat <- data.frame(matnona)
}else if(mydata == "M3"){
  
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

  DT <- matnona

}


use.ggplotly <- F
#############

ui <- fluidPage(
  fluidRow(column(6, plotlyOutput("fplot")),
           column(6, plotOutput("ggfplot"))),
  fluidRow(column(6, plotlyOutput("onetsplot")),
           column(6, plotlyOutput("multipletsplot"))
  )
  ,
  #fluidRow(column(6, plotlyOutput("threedplot1")), column(6, plotlyOutput("threedplot2") ))
    mainPanel()
)

ui <- shinyUI(pageWithSidebar(
  
  headerPanel("Time Series Exploration"),
  
  mainPanel(
    fluidPage(fluidRow(column(6, plotlyOutput("fplot")),
                       column(6, plotOutput("ggfplot"))), 
              fluidRow(column(6, plotlyOutput("onetsplot")), 
                       column(6, plotlyOutput("multipletsplot")))
    ) 
  )
  
  
))

server <- function(input, output, session) {
  output$fplot <- renderPlotly({    
    s <- event_data("plotly_click", source = "A")
    s2 <- event_data("plotly_selected", source = "A")
    
    vec <- rep(1, nrow(DT))
    mycol <- rep("blue", nrow(DT))
    if (length(s) != 0) {
      #print(s)
      id <- s[["pointNumber"]] + 1
      vec <- rep(0.2, nrow(DT))
      vec[id] <- 1
      
      mycol <- rep("blue", nrow(DT))
      mycol[id] <- "red"
    }else if(length(s2) != 0){
      id <- s2[["pointNumber"]] + 1
      mycol[id] <- rainbow(length(id))
    }
    
    
    if(use.ggplotly){
      #p1   <- getplot(obj, colouring = "feature", feature = "entropy", pca.plotarrow = TRUE) 
      #ggplotly(p1) %>% layout(dragmode = "select")
      
      #p1   <- getplot(obj, colouring = "feature", feature = "entropy", pca.plotarrow = TRUE)
      p1   <- getplot(obj, colouring = "clustering", k = 3, pca.plotarrow = TRUE)
      l <- plotly_build(p1)
      l$data[[1]]$key <- seq(nrow(DT))
      print(l)
    }else{
      #key = as.numeric(DT[, 1])
      p <- plot_ly(x = DT[, 2], y = DT[, 3], mode = "markers", 
                   marker = list(opacity = vec, color = mycol, size = 6), source = "A") %>% layout(dragmode = "select", xaxis = list(title = "PC1"), yaxis = list(title = "PC2"))
      p
    }
  })
  
  output$ggfplot <- renderPlot({ 
    #p1   <- getplot(obj, colouring = "feature", feature = "entropy", pca.plotarrow = TRUE)
    p1   <- getplot(obj, colouring = "clustering", k = 3, pca.plotarrow = TRUE)
    p1
  }) 
  output$click <- renderPrint({
    s <- event_data("plotly_click", source = "A")
    
    if (length(s) == 0) {
      "Click on a point to display the associated time series"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover", source = "A")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else as.list(d)
  })
  
  output$brush <- renderPrint({
    d <- event_data("plotly_selected", source = "A")
    if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
  })
  
  output$threedplot1 <- renderPlotly({
    if(mydata == "M4"){
      plot_ly(fmat, x = FoAcf, y = trend, z = seasonality, type = "scatter3d", mode = "markers", color  = mytypes, source= "W")
    }else if(mydata == "YAHOO"){
      plot_ly(fmat, x = FoAcf, y = trend, z = entropy, type = "scatter3d", mode = "markers", color  = entropy, source= "W")
    }
    
  })
  
  output$threedplot2 <- renderPlotly({
    if(mydata == "M4"){
      plot_ly(fmat, x = FoAcf, y = trend, z = seasonality, type = "scatter3d", mode = "markers", color  = entropy, source = "Z")
    }else if(mydata == "YAHOO"){
      plot_ly(fmat, x = FoAcf, y = trend, z = entropy, type = "scatter3d", mode = "markers", color  = entropy, source= "W")
    }
  })
  
  output$onetsplot <- renderPlotly({
    clicka <- event_data("plotly_click", source = "A")
    hovera <- event_data("plotly_hover", source = "A")
    selecta <- event_data("plotly_selected", source = "A")
    
    print("--------")
    print(clicka)
    print(hovera)
    print(selecta)
    
    #clickz <- event_data("plotly_click", source = "Z")
    #print(clickz)
    #if(length(clickz) != 0){
    #  plot_ly(y = tslist[[clickz[["pointNumber"]] + 1]], source = "B")
    #}
    
    if(length(hovera) != 0){
      if(use.ggplotly){
        ids <- as.numeric(hovera[["key"]])
      }else{
        ids <- as.numeric(hovera[["pointNumber"]]) + 1
      }
      plot_ly(y = tslist[[ids]], source = "B") %>%
        layout(yaxis = list(title = "Value"))
      
    }else if(length(clicka) != 0) {
      if(use.ggplotly){
        ids <- as.numeric(clicka[["key"]])
      }else{
        ids <- as.numeric(clicka[["pointNumber"]]) + 1
      }
      plot_ly(y = tslist[[ids]], source = "B") %>%
        layout(yaxis = list(title = "Value"))
      
    }else if(length(selecta) != 0){
      if(use.ggplotly){
        ids <- as.numeric(selecta[["key"]])
      }else{
        ids <- as.numeric(selecta[["pointNumber"]]) + 1
      }
      
      mytslist <- tslist[ids]
      obsts <- unlist(lapply(mytslist, length))
      
      tsmat <- matrix(, nrow = max(obsts), ncol = length(ids))
      for(j in seq_along(ids)){
        tsmat[seq(obsts[j]), j] <- mytslist[[j]]
      }
      tsmat <- apply(tsmat, 2, scale)
      
      
      df1 = stack(as.data.frame(tsmat))
      #print(df1)
      plot_ly(df1, y=values, group=ind, source = "B", showlegend = F) %>%
        layout(yaxis = list(title = "Value"))
      
    }else{
      plot_ly()
    }
    
  })
  
  output$multipletsplot <- renderPlotly({
    
    selecta <- event_data("plotly_selected", source = "A")
    #clickb <- event_data("plotly_click", source = "B")
    
    if(length(selecta) != 0){
      
      id <- selecta[["pointNumber"]] + 1
      #print(selecta[["pointNumber"]])
      #print(id)
      
      mytslist <- tslist[id]
      nseries <- length(mytslist)
      obsts <- unlist(lapply(mytslist, length))
      #lapply(lapply(M4[c(1, 2)], "[[", "id"), function(i){rep(i, )}
      
      x <- unlist(lapply(obsts, seq))
      y <- unlist(mytslist)
      colvec <- rainbow(nseries)
      variable <- unlist(lapply(seq(nseries), function(i){rep(i, obsts[i])}))
      mycols   <- unlist(lapply(seq(nseries), function(i){rep(colvec[i], obsts[i])}))
      
      D <- data.frame(x = x, y = y, variable = variable)
      #print(D)
      
      p <- ggplot(D, aes(x = x, y = y, col = mycols)) + 
        #	 geom_line(aes(x = x, y = y)) +
        geom_line() +
        facet_wrap(~ variable, scales = "free") + theme(legend.position="none")
      p
      #ggplotly(p, source = "C")  
      
    }else{
      plot_ly()
    }
    
  })
  
}

shinyApp(ui, server, options = list(display.mode = "showcase"))
