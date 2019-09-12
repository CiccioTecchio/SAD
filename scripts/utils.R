showBar <- function(vector, lbl, main, rows, posizioneLegenda){
    pal<- distinctColorPalette(rows)
    graph <- barplot(vector, main = main, col = pal)
    legend(posizioneLegenda, legend = lbl, col = pal, lwd=4, ncol=3, cex=0.3)
}

showPareto <- function(totale, main, rows){
    ordinato <- dsTassi[order(dsTassi$Totale, decreasing = TRUE),]
    lbls <- rownames(ordinato)
    ordinato <- ordinato$Totale
    freqRel <- prop.table(ordinato)
    app <- data.frame(ordinato)
    pal<- distinctColorPalette(rows)
    x<- barplot(freqRel, ylim= c(0, 1.5), main = main, col=pal, axisname = FALSE)
    axis(1, las=2, hadj= 0.6, at=x, labels=lbls, line=1, col="transparent")
    lines(x, cumsum(freqRel), type = "b", pch=16)
    text(x-0.2, cumsum(freqRel) + 0.03, paste(format(cumsum(freqRel)*100, digits = 3), "%"))
}

showBoxPlot <- function(label, tasso, name, title, color, lim){
    boxplot.with.outlier.label(label_name = label, tasso,
    names=c(name), main=title, horizontal = TRUE,
    col= color, pars=list(ylim=c(0,lim)))
}

mykm <- function(ds, k, iter){
    #scelta dei centroidi
    d <- dist(ds, method="euclidean", diag=TRUE, upper = TRUE)
    tree <- hclust(d^2, method = "centroid")
    
    cut <- cutree(tree, k = k, h = NULL)
    cutList <- list(cut)
    
    initialCentroid <- aggregate(ds, cutList, mean)[, -1]
    print(initialCentroid)
    
    km <- kmeans(ds, centers = initialCentroid, iter.max = iter)
    print(km)
    plot(dsTassi[1:2], main=paste("K-means con k =",k))
    points(km$center, col= 1:10, pch=8, cex=2)
}

metodiGerarchici <- function(ds, labels, numCluster, mtd, title){
    d = dist(ds, method = "euclidean", diag = TRUE, upper = TRUE)
    hls = hclust(d, method = mtd)
    
    if(mtd %in% c("centroid", "median")){
        d = d ^ 2
    }
    #str(hls)
    
    #dengrogramma
    plot(hls, hang=-1, xlab= title)
    axis(side = 4, at = round(c(0, hls$height),2))
    
    rect.hclust(hls, k=numCluster, border="red")
    cut = cutree(hls, k=numCluster, h=NULL)
    cutList = list(cut)
    numberCut = table(cut)
    #print(cutList)
    
    agmean = aggregate(ds, cutList, mean)[, -1]
    
    plot(ds)
    points(agmean, col=1:2, pch=8, cex=1)

    n = nrow(ds)
    #print(n)
    # total omogenity
    totHomogenity = (n-1)*sum(apply(ds, 2, var))
    #print(totHomogenity)
    #between
    within = 0
    clusterHomogeneity = vector(length = numCluster)
    for(i in 1:numCluster){
        value = (numberCut[[i]] - 1) * sum(agmean[i, ])
        clusterHomogeneity[i] = value
        #print(value)
        if(!is.na(value)){
            within = within + value
        }
    }
    between = totHomogenity - within
    betweenTotal = (between / totHomogenity)
    #print(c("Between Total", betweenTotal))
    #print(paste("Between Total", betweenTotal))
    betweenTotal = paste(round(betweenTotal, digits = 2), "%", sep = "")
    return(betweenTotal)
}

buildClusterTable <- function(clusterNum){
    tableK <- data.frame("Tipo" = c("k-means","gerarchico", "", "", "", ""), 
                          "Cluster" = rep(clusterNum,6),
                          "Distanza"= rep("Euclidea", 6),
                          "Aggregazione" = c("Centroide", "Singolo", "Completo", "Medio", "Centroide", "Mediana"), 
                          "B/T"= rep(0, 6))
    #stampare la tabella
    #kable(buildClusterTable(2, c(valori di bt)))
    return(tableK)
}