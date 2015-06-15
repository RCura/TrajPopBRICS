#### Packages load ####

# Computation
library(ade4) # Correspondance Analysis
library(FactoClass) # Hierarchical Clustering
library(ellipse) # Cluster ellipses on factors
library(reshape2)


# Plots
library(ggplot2)
library(scales)
library(RColorBrewer)
library(ggvis)
source(file="A2R.R") # Nice dendrogram

# Spatial
library(sp) # static map
library(rgdal) # static map
library(ggmap) # static map
library(OpenStreetMapR) # dynamic map

# Reporting
library(knitr)


#### ANALYSES ####

runAFC <- function(calcDF){
    print("execution de l'AFC")
    AFCresults <- dudi.coa(df=calcDF,
                           scannf=FALSE,
                           nf=ncol(calcDF))
    return(AFCresults)
}

runCAH <- function(AFC, calcDF){
    print("execution de la CAH")
    
    dist.dudi2 <- function (dudi, amongrow = TRUE) 
    {
      if (!inherits(dudi, "dudi")) 
        stop("Object of class 'dudi' expected")
      if (amongrow) {
        x <- t(t(dudi$tab) * sqrt(dudi$cw))
        x <- x %*% t(x)
        y <- diag(x)
        x <- (-2) * x + y
        x <- t(t(x) + y)
        x <- (x + t(x))/2
        diag(x) <- 0
        correctRounding <- function(x){
          if (x <= 0) {
            return(0)
          } else {
            return(x)
          }
        }
        x <- apply(X = x, MARGIN = 1:2, FUN = correctRounding)
        x <- as.dist(sqrt(x))
        attr(x, "Labels") <- row.names(dudi$tab)
        attr(x, "method") <- "DUDI"
        return(x)
      }
      else {
        x <- as.matrix(dudi$tab) * sqrt(dudi$lw)
        x <- t(x) %*% x
        y <- diag(x)
        x <- (-2) * x + y
        x <- t(t(x) + y)
        x <- (x + t(x))/2
        diag(x) <- 0
        x <- as.dist(sqrt(x))
        attr(x, "Labels") <- names(dudi$tab)
        attr(x, "method") <- "DUDI"
        return(x)
      }
    }
    
        CAHresults <- ward.cluster(dista=dist.dudi2(AFC),
                                   peso=apply(calcDF, 1, sum),
                                   plots=FALSE,
                                   h.clust=1)
    return(CAHresults)
}

cutCAH <- function(CAH, nbClusters){
    clusters <- cutree(tree=CAH, k=nbClusters)
    return(clusters)
}

#### PLOTS ####

plotCAH <- function (cah, nbClusters, cPal, clusterCol) {
    classOrder <- unique(clusterCol[cah$order])
    treePlot <- A2Rplot(x = cah,
            k = nbClusters,
            boxes = FALSE,
            col.up = "gray50",
            col.down = cPal[classOrder],
            show.labels = FALSE,
            main = "Dendrogram")
    return(treePlot)
}

plotAFC <- function(afc, cah, nbClusters, clusters, cPal) {
  inertia <- list('axe1' = round(afc$eig[1]/sum(afc$eig)*100,2),
                  'axe2' = round(afc$eig[2]/sum(afc$eig)*100,2))
  
  return({
    s.class(cstar=1,
            addaxes=TRUE,
            grid=FALSE,
            axesell=FALSE,
            dfxy=afc$li,
            fac=as.factor(clusters),
            col=cPal,
            label=c(1:nbClusters),
            sub=paste(names(inertia), ":", inertia, "%",sep=" ", collapse=" - "),
            csub=1.2,
            possub="bottomright")
  })
}


computeEllipse <- function(afcData){
  df_ellipse <- data.frame()
  for(g in levels(afcData$Cluster)){
    df_ellipse <- rbind(df_ellipse,
                        cbind(as.data.frame(
                          with(afcData[afcData$Cluster==g,],
                               ellipse(cor(x, y),
                                       level=(2/3),
                                       scale=c(sd(x),sd(y)),
                                       centre=c(mean(x),mean(y))))),
                          Cluster=g))
  }
  return(df_ellipse)
}

computeMeans <- function(plotDF) {
    clustersMeanPop <- plotDF[-c(1:(nrow(plotDF)- length(unique(plotDF$TrajPopCluster)))),]
    clustersMeanPop[] <- as.data.frame(t(apply(X=data.frame(rbind(by(data=plotDF,
                                                                     INDICES=plotDF$TrajPopCluster,
                                                                     FUN=colMeans,
                                                                     simplify=FALSE))[]),
                                               MARGIN=2,
                                               FUN=unlist)))
    meltedMeanPop <- melt(data=clustersMeanPop,id.vars="TrajPopCluster", )
    meltedMeanPop$variable <- as.integer(as.character(meltedMeanPop$variable))
  return(meltedMeanPop)  
}

computeRelativeWeights <- function(plotDF) {
    clustersSumPop<- plotDF[-c(1:(nrow(plotDF)-length(unique(plotDF$TrajPopCluster)))),]
    clustersSumPop[] <- as.data.frame(
        t(
            apply(
                X=data.frame(rbind(
                    by(data=plotDF,
                        INDICES=plotDF$TrajPopCluster,
                        FUN=colSums,
                        simplify=FALSE))[]),
                 MARGIN=2,
                 FUN=unlist)))

    clustersSumPop <- subset(x=clustersSumPop, select=-TrajPopCluster)
    totalWeights <- colSums(x=subset(x=plotDF, select=-TrajPopCluster))
    
    relativeWeights <- data.frame(t(
        apply(X=clustersSumPop,
              MARGIN=1,
              FUN=function(x,y){x / totalWeights[y]})),
        check.names=FALSE)
    relativeWeights$TrajPopCluster <- 1:length(unique(plotDF$TrajPopCluster))
    
    meltedRelativeWeights <- melt(data=relativeWeights,id.vars="TrajPopCluster")
    meltedRelativeWeights$variable <- as.integer(as.character(meltedRelativeWeights$variable))
    return(meltedRelativeWeights)
}

computeMeanRelativeWeights <- function(plotDF){
    clustersMeanPop<- plotDF[-c(1:(nrow(plotDF)-length(unique(plotDF$TrajPopCluster)))),]
    clustersMeanPop[] <- as.data.frame(
        t(
            apply(
                X=data.frame(rbind(
                    by(data=plotDF,
                       INDICES=plotDF$TrajPopCluster,
                       FUN=colMeans,
                       simplify=FALSE))[]),
                MARGIN=2,
                FUN=unlist)))
    
    clustersMeanPop <- subset(x=clustersMeanPop, select=-TrajPopCluster)
    totalWeights <- colSums(x=subset(x=plotDF, select=-TrajPopCluster))
    
    meanRelativeWeights <- data.frame(t(
        apply(X=clustersMeanPop,
              MARGIN=1,
              FUN=function(x,y){x / totalWeights[y]})),
                                  check.names=FALSE)
    meanRelativeWeights$TrajPopCluster <- 1:length(unique(plotDF$TrajPopCluster))
    
    meltedMeanRelativeWeights <- melt(data=meanRelativeWeights,id.vars="TrajPopCluster")
    meltedMeanRelativeWeights$variable <- as.integer(as.character(meltedMeanRelativeWeights$variable))
    return(meltedMeanRelativeWeights)
}


plotClustersMeans <- function(meltedDF, cPal)  {
    myBreaks <- c(min(meltedDF$value), median(meltedDF$value), max(meltedDF$value))
    meanPlot <- ggplot(data=meltedDF, environment = environment()) + 
                geom_line(aes(x=variable, y=value, group=TrajPopCluster, colour=factor(TrajPopCluster)), size=1) +
                scale_color_manual(values=cPal, name="Cluster") +
                scale_y_log10(breaks=myBreaks) +
                labs(title = "Clusters mean populations",
                     x = "Date",
                     y = "Population") +
                theme_bw()
    return(meanPlot)
}

plotClustersWeights <- function(meltedDF, cPal)  {
    myBreaks <- c(min(meltedDF$value), median(meltedDF$value), max(meltedDF$value))
    
    relativePlot <- ggplot(data=meltedDF, environment = environment()) + 
        geom_line(aes(x=variable, y=value, group=TrajPopCluster, colour=factor(TrajPopCluster)), size=1) +
        scale_color_manual(values=cPal, name="Cluster") +
        scale_y_log10(breaks=myBreaks, labels = percent) +
        labs(title = "Clusters relative weight",
             x = "Date",
             y = "Share of total population") +
        theme_bw()
    return(relativePlot)
}

plotClustersMeanWeights <- function(meltedDF, cPal)  {
    myBreaks <- c(min(meltedDF$value), median(meltedDF$value), max(meltedDF$value))
    
    meanRelativePlot <- ggplot(data=meltedDF, environment = environment()) + 
        geom_line(aes(x=variable, y=value, group=TrajPopCluster, colour=factor(TrajPopCluster)), size=1) +
        scale_color_manual(values=cPal, name="Cluster") +
        scale_y_continuous(labels = percent) +
        labs(title = "Clusters' means relative weight",
             x = "Date",
             y = "Share of total population of clusters' means") +
        theme_bw()
    return(meanRelativePlot)
}

plotRankSize <- function(baseDF){
    baseDF$UID <-row.names(baseDF) 
    meltedDF <- melt(data=baseDF,id.vars="UID")

    timeValues <- unique(meltedDF$variable)
    for (currentTime in timeValues){
        myValues <- meltedDF[meltedDF$variable == currentTime, 'value']
        meltedDF[meltedDF$variable == currentTime,'rank'] <- rank(x=-myValues)
    }
    ranksizePlot <- ggplot(data=meltedDF, environment = environment()) + 
        geom_line(aes(x=rank, y=value, group=variable, colour=as.numeric(levels(variable)[variable])), size=1) +
        scale_colour_gradient(low="skyblue3", high="firebrick4", "Date") +
        scale_alpha(range=c(0.1,1)) +
        scale_y_log10() +
        scale_x_log10() +
        labs(title = "Rank-Size evolution",
             x = "Rank",
             y = "Population") +
        theme_bw()
    
    return(ranksizePlot)
}

ggvisRankSize <- function(baseDF){
  baseDF$UID <-row.names(baseDF) 
  meltedDF <- melt(data=baseDF,id.vars="UID")
  
  timeValues <- unique(meltedDF$variable)
  for (currentTime in timeValues){
    myValues <- meltedDF[meltedDF$variable == currentTime, 'value']
    meltedDF[meltedDF$variable == currentTime,'rank'] <- rank(x=-myValues)
  }
  dateValue <- function(x) {
    return(x$variable)
  }
  meltedDF$variable <- as.numeric(as.character(meltedDF$variable))
  ranksizePlot <- meltedDF %>%
    ggvis(x = ~rank, y = ~value, stroke = ~variable) %>%
    group_by(variable) %>%
    layer_lines(strokeWidth := 2) %>%
    scale_numeric("x",  label = "Rank", trans="log", expand=0, nice = TRUE) %>%
    scale_numeric("y", label = "Population", trans="log", expand = 0, nice = TRUE) %>%
    scale_numeric("stroke", range = c("lightgrey", "red")) %>%
    add_legend("stroke", title = "Years", format = "g") %>%
    add_tooltip(dateValue, "hover")
  return(ranksizePlot)
}

ggplotMap <- function(DF, latColumn, longColumn, sizeColumn, colourColumn, colourPalette, maxRadius){
    geoDF <- DF
    coordinates(geoDF) <- c(longColumn, latColumn)
    proj4string(geoDF) <- CRS("+proj=longlat +datum=WGS84")
    geoBbox <- as.vector(geoDF@bbox)
    geoDF <- spTransform(x=geoDF, CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
    CoordsCol <- as.data.frame(geoDF@coords)
    DF$projLat <- CoordsCol$Lat
    DF$projLong <- CoordsCol$Long
    DF$Cluster <- DF[,colourColumn]
    
    
    
    mapPlot <- ggplot(DF, aes_string(x='projLong', y='projLat', size=names(DF)[sizeColumn]), environment=environment()) +
      geom_point(aes(colour=factor(Cluster))) +
      scale_color_manual(values=colourPalette, name="Cluster") +
      scale_size_area( max_size=maxRadius, name="Population") +
      coord_fixed() +
      labs(title = "Cluster's map",
           x = "Long (UTM meters)",
           y = "Lat (UTM meters)")
    
#     mapPlot <- ggmap(get_map(location = geoBbox, maptype = 'toner-lite', source = 'stamen'), extent = 'device', base_layer = {
#       ggplot(DF, aes_string(x='projLong', y='projLat', size=names(DF)[sizeColumn]), environment=environment()) +
#                         geom_point(aes(colour=factor(Cluster))) +
#                         scale_color_manual(values=colourPalette, name="Cluster") +
#                         scale_size_area( max_size=maxRadius, name="Population") +
#                         coord_fixed() +
#                         labs(title = "Cluster's map",
#                              x = "Long (UTM meters)",
#                              y = "Lat (UTM meters)")
      
    
    return(mapPlot)
}

pointScale <- function(data, maxRadius=20){
    maxData <- max(data)
    pointsRadiuses <- unlist(lapply(X=data,
                                    FUN=function(x){(maxRadius * sqrt(x)) / sqrt(maxData)}))
    return(pointsRadiuses)
}


colorPaletteList <- c('Set3',
                      'Set2',
                      'Set1',
                      'Pastel2',
                      'Pastel1',
                      'Paired',
                      'Dark2',
                      'Accent',
		      'Rainbow')
