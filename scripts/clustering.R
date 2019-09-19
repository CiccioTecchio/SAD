applyClustering <- function(data, distanceMethod, clusteringMethod, clusters) {
  totalHomogeneity = (nrow(data) - 1) * sum(apply(data, 2, var))
  distanceMatrix = dist(data, method = distanceMethod, diag = FALSE, upper = FALSE)
  if(clusteringMethod %in% c("centroid", "median")){
    distanceMatrix = distanceMatrix ^ 2
  }
  hierarchialClustering = hclust(distanceMatrix, method = clusteringMethod)
  clusteringCut = cutree(hierarchialClustering, k = clusters, h = NULL)
  clusteringCuts = list(clusteringCut)
  numberCut = table(clusteringCut)
  singleGroupHomogeneity = aggregate(data, clusteringCuts, var)[, -1]
  within = 0
  clusterHomogeneity = vector(length = clusters)
  for(i in 1:clusters){
    value = (numberCut[[i]] - 1) * sum(singleGroupHomogeneity[i, ])
    clusterHomogeneity[i] = value
    if(!is.na(value)){
      within = within + value
    }
  }
  between = totalHomogeneity - within
  betweenTotal = between / totalHomogeneity
  return(
    list("th"=totalHomogeneity, "dm"=distanceMethod, "cm"=clusteringMethod, "cls"=clusters, 
         "hc"=hierarchialClustering, "cc"=clusteringCut, "sh"=singleGroupHomogeneity, 
         "wtn"=within, "btw"=between, "bt"=betweenTotal, "ch" = clusterHomogeneity)
  )
}

clusterSummary <- function(cluster) {
  library(xtable)
  cluster_size = length(cluster$ch)
  rows = cluster_size + 4 # Spaces for th, wtn, btw, b
  clusterTable <- data.frame(matrix(nrow = rows, ncol = 1))
  row = c(cluster$ch, cluster$th, cluster$wtn, cluster$btw, cluster$bt)
  clusterTable[, 1] <- row
  rn <- c(paste0("Non omogeneità cluster", seq(1:cluster_size)), "Non omogeneità totale", 
          "Within", "Between", "Between Total")
  rownames(clusterTable) <- rn
  colnames(clusterTable) <- c(paste("Misure Metodo:", cluster$cm))
  xclusterTable <- xtable(clusterTable, caption = paste("Misure metodo:", cluster$cm))
  print.xtable(xclusterTable, file = paste("../tables/", cluster$cm, ".tex", sep = ""))
}