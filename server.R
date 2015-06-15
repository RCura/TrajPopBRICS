###########################
# File: server.R
# Description: Server File for Shiny Application TrajPop
# Date: 26/06/2013
# Author: Robin Cura (robin.cura@gmail.com)
###########################

library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
  
  load("data/countriesPop.RData")
  
  dataValues <- reactiveValues(dataSource= NULL, rawDF = NULL, filtredDF = NULL, calcDF = NULL, resultDF = NULL)
  
  analysisValues <- reactiveValues(AFC = NULL, CAH = NULL, Clusters = NULL)
  
  plotValues <- reactiveValues(cPal= NULL, MeanPop = NULL, RelativeWeight = NULL, MeanRelativeWeight = NULL)

  #### Update Color Palette ####
  
  observe({
    if (input$brewerPalette == 'Rainbow'){
      plotValues$cPal <- substr(rainbow(n=input$nbClusters), start = 1, stop = 7)
    } else if (brewer.pal.info[input$brewerPalette,]$maxcolors < input$nbClusters) {
      plotValues$cPal <- substr(rainbow(n=input$nbClusters), start = 1, stop = 7)
      updateSelectInput(session=session, inputId='brewerPalette',
                        choices=colorPaletteList, selected="Rainbow")
    } else {
      plotValues$cPal <- brewer.pal(n=input$nbClusters, name=input$brewerPalette)
    }
  })
  
  
  #### Data upload ####
  

   
  observe({
    countryName <- input$dataset
    if (countryName ==  "Brazil") {
      dataValues$rawDF <- Brazil
    } else if (countryName ==  "Russia") {
      dataValues$rawDF <- Russia
    } else  if (countryName ==  "India"){
      dataValues$rawDF <- India
    }  else if (countryName ==  "China"){
      dataValues$rawDF <- China
    } else  if (countryName ==  "South Africa"){
      dataValues$rawDF <- SouthAfrica
    } else if (countryName ==  "USA"){
      dataValues$rawDF <- USA
    } else  {
      dataValues$rawDF <- France 
    }
    
    
    timeColumns <- as.numeric(na.omit(as.numeric(unlist(colnames(dataValues$rawDF)))))
    allColumns <- c("None", unlist(colnames(dataValues$rawDF)))
    updateInputs(session, allColumns, timeColumns)
    
    dataValues$calcDF <- NULL
    dataValues$resultDF <- NULL
    
    analysisValues$AFC  <- NULL
    analysisValues$CAH  <- NULL
    analysisValues$Clusters  <- NULL
    
    plotValues$MeanPop <-  NULL
    plotValues$RelativeWeight <-  NULL
    plotValues$MeanRelativeWeight <-  NULL
  
  })
  
  #### Data preparation ####
  
  observe({
    timeColumns <- input$timeColumnSelected
    rawDF <- dataValues$rawDF
    
    if (!is.null(timeColumns) && timeColumns %in% names(rawDF)){
    dataValues$filtredDF <- rawDF[complete.cases(rawDF[timeColumns]),]
    calcDF <- dataValues$filtredDF[timeColumns]
    if (nrow(unique(dataValues$filtredDF)) == nrow(dataValues$filtredDF)) {
      row.names(calcDF) <- dataValues$filtredDF$ID
    }
    dataValues$calcDF <- calcDF
    }
  })
  
  output$nbCities <- renderText({
    totalCities <- nrow(dataValues$rawDF)
    selectedCities <- nrow(dataValues$filtredDF)
    sprintf("<strong>%s</strong> cities selected / <strong>%s</strong> total (<em>%s &#37</em>)",
            selectedCities, totalCities, round(selectedCities/totalCities * 100))
  })
  
  #### Analysis ####
  
  ##### AFC #####
  
  observe({
    if (!is.null(dataValues$calcDF)){
      if (ncol(dataValues$calcDF) > 2) {
        analysisValues$AFC <- runAFC(calcDF = dataValues$calcDF)
      } else {
        analysisValues$AFC <- NULL
      }
    } else {
      analysisValues$AFC <- NULL
    }
  })
  
  ##### CAH #####
  
  observe({
    if (!is.null(analysisValues$AFC)){
      analysisValues$CAH <- runCAH(AFC=analysisValues$AFC, calcDF=dataValues$calcDF)
    } else {
      analysisValues$CAH <- NULL
    }
  })
  
  observe({
    if(!is.null(analysisValues$CAH)){
      analysisValues$Clusters <- cutCAH(analysisValues$CAH, input$nbClusters)
    } else {
      analysisValues$Clusters <- NULL
    }
  })
  
  #### Results and plot data ####
  
  ##### resultDF #####
  
  observe({
    if (!is.null(analysisValues$Clusters)){
      resultDF <- dataValues$filtredDF
      resultDF$TrajPopCluster <- analysisValues$Clusters
      dataValues$resultDF <- resultDF
    } else {
      dataValues$resultDF <- NULL
    }
  })

  ##### Mean Pop #####

  observe({
    if (!is.null(dataValues$resultDF)){
      plotData <- dataValues$calcDF
      plotData$TrajPopCluster <- analysisValues$Clusters
      plotValues$MeanPop <- computeMeans(plotData)
    } else {
      plotValues$MeanPop <- NULL
    }
  })
  
  ##### Relative Weight #####
  
  observe({
    if (!is.null(dataValues$resultDF)){
      plotData <- dataValues$calcDF
      plotData$TrajPopCluster <- analysisValues$Clusters
      plotValues$RelativeWeight <- computeRelativeWeights(plotData)
    } else {
      plotValues$RelativeWeight <- NULL
    }
  }) 
  
  
  ##### Mean Relative Weight #####
  
  observe({
    if (!is.null(dataValues$resultDF)){
      plotData <- dataValues$calcDF
      plotData$TrajPopCluster <- analysisValues$Clusters
      plotValues$MeanRelativeWeight <- computeMeanRelativeWeights(plotData)
    } else {
      plotValues$MeanRelativeWeight <- NULL
    }
  }) 
  
  #### Plot Hclust Tree ####
  
  output$tree <- renderPlot({
    if(!is.null(dataValues$resultDF)) {
      plotCAH(analysisValues$CAH,
              input$nbClusters,
              plotValues$cPal,
              analysisValues$Clusters)
      
      legend(x="topleft",
             paste("Cluster",
                   1:input$nbClusters,
                   sep=" "),
             cex=1,
             seg.len=4,
             col=plotValues$cPal,
             pch=NA,
             lty=1,
             lwd=4)
    }
  })
  

  #### Plot AFC ####
  observe({
    if(!is.null(dataValues$resultDF)) {
      afcPlot <- ggvisAFC(afc = analysisValues$AFC,
              clusters = analysisValues$Clusters,
              cPal = plotValues$cPal)
      bind_shiny(vis = afcPlot, plot_id = "afc")
    }
  })
  
  
  #### Plot Clusters' Mean ####
  output$clustersMean <- renderPlot({
    if(!is.null(plotValues$MeanPop)) {
      clustersMeanPlot <- plotClustersMeans(plotValues$MeanPop, plotValues$cPal)
      print(clustersMeanPlot)
    }
  })
  
  #### Plot Clusters' Weight ####
  output$clustersWeights <- renderPlot({
    if(!is.null(plotValues$RelativeWeight)) {
      clustersWeightPlot <- plotClustersWeights(plotValues$RelativeWeight, plotValues$cPal)
      print(clustersWeightPlot)
    }
  })
  
  #### Plot Clusters' Means Weight ####
  output$clustersMeanWeights <- renderPlot({
    if(!is.null(plotValues$MeanRelativeWeight)) {
      clustersMeanWeightPlot <- plotClustersMeanWeights(plotValues$MeanRelativeWeight, plotValues$cPal)
      print(clustersMeanWeightPlot)
    }
  })
  
  #### Plot Ranksize ####
    observe({
    if(!is.null(dataValues$resultDF)) {
      ranksizePlot <- ggvisRankSize(dataValues$calcDF)
      bind_shiny(vis = ranksizePlot, plot_id = "ranksize2")
    }
  })
  
  #### Plot static map ####
  output$ggmap <- renderPlot({
    if (!is.null(dataValues$resultDF) && isTRUE(input$makemap)){
      baseDF <- dataValues$resultDF
      Lat <- baseDF$Lat
      Long <- baseDF$Long
      Size <- baseDF[,input$sizeAttribute]
      cPal <- plotValues$cPal
      Colour <- baseDF$TrajPopCluster
      
      mapDF <- data.frame(Lat=Lat, Long=Long, Size=Size, Colour=Colour)
      staticMap <- ggplotMap(DF=mapDF,latColumn=1, longColumn=2, sizeColumn=3, colourColumn=4,
                             colourPalette=plotValues$cPal, maxRadius=input$maxSize)
      print(staticMap)
    }
  })
  
  #### Plot dynamic map ####
  output$webmap <- renderText({
    if (!is.null(dataValues$resultDF)){
      mapData <- dataValues$resultDF
      mapData$Name <- mapData$ID
      mapData$color <- as.character(factor(x=mapData$TrajPopCluster,
                                           levels=unique(mapData$TrajPopCluster),
                                           labels=plotValues$cPal))
      lastPopColumn <- input$timeColumnSelected[length(input$timeColumnSelected)]
      mapData$pointRadius <- pointScale(data=mapData[, input$sizeAttribute], max=input$maxSize)
      mapData$long <- mapData$Long
      mapData$lat <- mapData$Lat
      popup <- sprintf("Name: %s <br> Cluster : %s <br> Pop (%s) : %s",
                       mapData$Name,
                       mapData$TrajPopCluster,
                       input$sizeAttribute,
                       mapData[, input$sizeAttribute])
      mapData$popup <- popup
      
      multiMap <- OSMMap(mapData[mapData$TrajPopCluster == 1,], color='color', size='pointRadius',
                         layer = paste('1', " (", nrow(mapData[mapData$TrajPopCluster == 1,]), " cities)", sep=""),
                         colorByFactor = FALSE,popup = 'popup')
      for (nbCluster in unique(mapData$TrajPopCluster)){
        if (nbCluster != 1){
          multiMap <- addLayers(multiMap,
                                OSMMap(mapData[mapData$TrajPopCluster == nbCluster,],
                                       color='color', size='pointRadius',
                                       layer = paste(nbCluster, " (", nrow(mapData[mapData$TrajPopCluster == nbCluster,]), " cities)", sep=""),
                                       colorByFactor = FALSE,
                                       popup = 'popup'))
        }
      }
      #pointPlot = OSMMap(mapData, color='color', size='pointRadius', layer = 'Points', colorByFactor = F,popup = 'popup')
      print(multiMap,returnText=T, title="", subtitle="")
    }
   
  })

  #### Plot table ####
  output$mytable <- renderDataTable({
    dataValues$resultDF
  })
  
  #### Plot contingency table ####
  output$contingencyTable <- renderTable({
    if (!is.null(dataValues$resultDF)) {
      baseDF <- dataValues$resultDF
      
      contingencyTable <- table('test' = baseDF[,input$correspondanceColumnSelected],
                                'Cluster'= baseDF$TrajPopCluster)
      summed <- addmargins(contingencyTable)
      summed
    }
  })
  
  #### Plot Chi2 results ####
  output$Chi2results <- renderPrint({
    if (!is.null(dataValues$resultDF)) {
      baseDF <- dataValues$resultDF
      
      contingencyTable <- table('test' = baseDF[,input$correspondanceColumnSelected],
                                'Cluster'= baseDF$TrajPopCluster)
      chi2 <- chisq.test(contingencyTable)
      myDT <- data.frame('Chi2'=chi2$statistic, 'Delta' = chi2$parameter, 'P-value' = chi2$p.value)
      print(xtable(myDT), type='html', include.rownames=FALSE)
    }
  })
  
  #### Plot Chi2 residuals ####
  output$Chi2residuals <- renderTable({
    if (!is.null(dataValues$resultDF)) {
      baseDF <- dataValues$resultDF
      
      contingencyTable <- table('test' = baseDF[,input$correspondanceColumnSelected],
                                'Cluster'= baseDF$TrajPopCluster)
      chi2 <- chisq.test(contingencyTable)
      chi2$residuals
    }
  })
  #### Plot Boxplot ####
  
  output$AnovaBoxPlot <- renderPlot({
    if (!is.null(dataValues$resultDF)) {
      baseDF <- dataValues$resultDF
      baseDF$Cluster <- factor(baseDF$TrajPopCluster)
      baseDF$boxplotName <- baseDF[,input$correspondanceColumnSelected]
      myBoxPlot <- ggplot(baseDF, aes_string(x='Cluster', y='boxplotName'), environment=environment()) +
        geom_boxplot(aes(fill=Cluster)) +
        stat_summary(fun.y=mean, geom="point", shape=4, size=2) +
        scale_fill_manual(values=plotValues$cPal, name="Cluster") +
        guides(fill=FALSE) +
        labs(x = "Cluster",
             y = input$correspondanceColumnSelected) +
        theme_bw()
      
      if (isTRUE(input$logBoxPlot)){
        myBoxPlot  <- myBoxPlot + scale_y_log10()
      }
      print(myBoxPlot)
    }
  })
  
  output$AnovaBoxPlot2 <- renderPlot({
    if (!is.null(dataValues$resultDF)) {
      baseDF <- dataValues$resultDF
      baseDF$Cluster <- factor(baseDF$TrajPopCluster)
      abc <- input$timeColumnSelected
      meltDF <- melt(baseDF, value.name="Pop", variable.name="Time", measure.vars=abc)
      
      meltDF$Cluster <- factor(meltDF$TrajPopCluster)
      
      myBoxPlot2 <- ggplot(meltDF, aes_string(x='Cluster', y='value'), environment=environment()) +
        geom_boxplot(aes(fill=Cluster)) +
        stat_summary(fun.y=mean, geom="point", shape=4, size=2) +
        scale_fill_manual(values=plotValues$cPal, name="Cluster") +
        guides(fill=FALSE) +
        labs(x = "Cluster",
             y = "Pop") +
        theme_bw() + 
        facet_wrap( ~ variable)
      
      if (isTRUE(input$logBoxPlot)){
        myBoxPlot2  <- myBoxPlot2 + scale_y_log10()
      }
      
      print(myBoxPlot2)
    }
  })
  
  output$AnovaResults <- renderTable({
    if (!is.null(dataValues$resultDF)) {
      baseDF <- dataValues$resultDF
      baseDF$Cluster <- baseDF$TrajPopCluster
      baseDF$Variable <- baseDF[, input$correspondanceColumnSelected]
      ANOVA <- aov(Variable ~ Cluster, baseDF)
      summary(ANOVA)
    }
  })
  
  #### Compute report ####
  
  output$downloadPDF <- downloadHandler(filename = "TrajPop_report.pdf",
                                        content = function(file){
                                          knit2pdf("TrajPop_report.Rnw")
                                          file.copy("TrajPop_report.pdf", file)
                                          file.remove("TrajPop_report.pdf", "TrajPop_report.tex",
                                                      "TrajPop_report.aux", "TrajPop_report.log")
                                          unlink("figure", recursive = TRUE)
                                        },
                                        contentType = "application/pdf"
  )
  
  #### Export table ####
  
  output$tableExport <- downloadHandler(filename = "TrajPop_table.csv",
                                        content = function(file) {
                                          write.csv(dataValues$resultDF, file) },
                                        contentType = "text/csv"                        
  )
  
  #### Compute report ####
  
  output$clusterMeans <- renderTable({
    if (!is.null(dataValues$resultDF)) {
      df <- dataValues$calcDF
      globalMean <- as.data.frame(t(colMeans(df)))
      globalMean$TrajPopCluster <- "Global"
      
      clusterDF <- plotValues$MeanPop
      clusterDF$TrajPopCluster <- as.integer(clusterDF$TrajPopCluster)
      clusterDF <- dcast(data=clusterDF, formula= TrajPopCluster ~ variable)
      meansDF <- as.data.frame(rbind(globalMean, clusterDF))
      
      df <- subset(meansDF, select=c("TrajPopCluster",names(meansDF)))
      df <- df[-ncol(df)]
      df <- prettyNum(x=df, big.mark=" ")
      return(df)
    }
  })
  
  output$clustersCounts <- renderTable({
    if (!is.null(dataValues$resultDF)) {
      df <- dataValues$resultDF
      countDF <- dcast(df, TrajPopCluster ~ .)
      colnames(countDF)[2] <- "Count"
      df <- rbind(c("Global", nrow(df)), countDF)
      df <- prettyNum(x=df, big.mark=" ")
      
      return(df)
    }
  })
  
  output$clustersVariances <- renderTable({
    if (!is.null(dataValues$resultDF)) {
      df <- dataValues$calcDF
      df$TrajPopCluster <- analysisValues$Clusters
      myDF <- do.call(rbind, by(data=df, INDICES=df$TrajPopCluster, FUN = function(x) {apply(x, MARGIN=2, FUN=var)}))
      myDF <- as.data.frame(myDF)
      myDF <- as.data.frame(cbind(myDF$TrajPopCluster, myDF))
      myDF <- myDF[-ncol(myDF)]
      colnames(myDF)[1] <- "TrajPopCluster"
      myDF$TrajPopCluster <- row.names(myDF)
      
      
      df <- rbind(c("Global", unlist(apply(df[-ncol(df)], MARGIN=2, FUN=var))), myDF)
      df <- prettyNum(x=df, big.mark=" ")
      return(df)
    }
  })
  
  output$clustersDistances <- renderTable({
    if (!is.null(dataValues$resultDF)) {
      calcDF <- dataValues$calcDF
      globalMean <- as.data.frame(t(colMeans(calcDF)))
      globalMean$TrajPopCluster <- "Global"
      
      calcDF$TrajPopCluster <- analysisValues$Clusters
      l2 <- as.data.frame(do.call(rbind, (by(data=calcDF, INDICES=calcDF$TrajPopCluster, FUN=colMeans))))
      
      df2 <- as.data.frame(rbind(globalMean, l2))

      df3 <- df2[,-ncol(df2)]
      
      myChi2dist <- function(df, n) {
        return(dist.dudi(dudi.coa(df=df[c(1,n),], scannf=FALSE, nf=ncol(df)), amongrow=TRUE)[1])
      }
      
      test <- lapply(X=2:(input$nbClusters + 1), FUN= function(x, ...) {myChi2dist(df3, x)})
      myDF <- data.frame("TrajPopCluster"=c("Global", 1:input$nbClusters), "Distance"=c(0, unlist(test)))
      myDF$TrajPopCluster <- as.vector(myDF$TrajPopCluster)
      myDF[nrow(myDF) + 1,] <- c("Sum", sum(unlist(test)))
      return(myDF)
    }
    
  })
  
  ggvisAFC <- function(afc, clusters, cPal){
    inertia <- inertia.dudi(afc)$TOT
    axis1name <- paste("Axis 1 ( ", format((inertia$inertia[1]) / sum(inertia$inertia) * 100, digits = 3),"% of inertia)")
    axis2name <- paste("Axis 2 ( ", format((inertia$inertia[2]) / sum(inertia$inertia) * 100, digits = 3),"% of inertia)")
    
    plotDF <- data.frame(x = afc$li$Axis1, y = afc$li$Axis2,
                         Cluster = as.factor(clusters), row.names = row.names(afc$li))
    
    
    ellipseDF <- computeEllipse(plotDF)
    
    clustersMean <- aggregate(x = plotDF[,c("x", "y")], by = list(plotDF$Cluster), mean)
    colnames(clustersMean)[1] <- "Cluster"
    
    plotDF$name <- row.names(plotDF)
    
    plotDF$color <- cPal[as.numeric(as.character(plotDF$Cluster))]
    clustersMean$color <- cPal[as.numeric(as.character(clustersMean$Cluster))]
    ellipseDF$color <- cPal[as.numeric(as.character(ellipseDF$Cluster))]
    
    maxXvalue <- max(abs(plotDF[,"x"]))
    maxYvalue <- max(abs(plotDF[,"y"]))
    
    myFullPlot <- ggvis(~x, ~y, data = ellipseDF) %>%
      group_by(Cluster) %>%
      layer_paths(~x, ~y, stroke := ~color,
                  strokeWidth := 2) %>%
      layer_points(~x, ~y, fill := ~color, key := ~name,
                   size := 25, strokeWidth := 0.5, stroke := "black", opacity := 2/3, 
                   data = plotDF) %>%
      layer_points(~x, ~y, stroke := ~color,
                   size := 250, fill := "white", shape := "square",
                   data=clustersMean) %>%
      layer_text(~x, ~y, fill := ~color, text := ~as.character(Cluster),
                 align := "center", baseline := "middle", fontSize := 18,
                 data=clustersMean) %>%
      scale_numeric("x", domain = c(-maxXvalue, maxXvalue), nice = FALSE, label = axis1name) %>%
      scale_numeric("y", domain = c(-maxXvalue, maxXvalue), nice = FALSE, label =  axis2name) %>%
      #set_options(width = 700, height = 400, keep_aspect = TRUE) %>%
      add_tooltip(afcPopup, "hover")
    
    return(myFullPlot)
    
  }
  
  afcPopup <- function(x) {
    if ("name" %in% names(x)){
      plotData <- dataValues$resultDF
      plotData$Name <- plotData$ID
      myCluster <- plotData[plotData$Name == x$name,"TrajPopCluster"][[1]]
      #myCluster <- 3
      sprintf("Name : %s <br />Cluster : %s", x$name, myCluster)
    } else {return(NULL)}
  }
  
})


updateInputs <- function(session, columns, timeColumns){
  updateSelectInput(session=session, inputId="timeColumnSelected",
                    choices=timeColumns, selected=timeColumns)
  updateSelectInput(session=session, inputId="capitalColumnSelected",
                    choices=columns, selected="")
  updateSelectInput(session=session, inputId='sizeAttribute',
                    choices=columns, selected=timeColumns[length(timeColumns)])
  updateSelectInput(session=session, inputId='correspondanceColumnSelected',
                    choices=columns, selected="")
}
