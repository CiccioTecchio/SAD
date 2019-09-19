misureNonOmogeneita <- function (df, mk, dist="euclidean", aggr) {
  
  dfPopolazione<-data.frame(df$`Tasso laureati triennali`,df$`Tasso laureati magistrali`)

  n<-nrow(df)
  trHI<-(n-1)*sum(apply(dfPopolazione,2, var))
  print("trHI:")
  print(trHI)
  d<-dist(dfPopolazione,method = dist,diag = TRUE,upper = TRUE) #individua la struttura di Missimilità
  if(aggr=="centroid" || aggr == "median") {
    d = d * d
  }
  
  hls<-hclust(d,method = aggr)
  taglio<-cutree(hls,k=mk,h=NULL)
  num<-table(taglio)
  tl<-list(taglio)
  agvar<-aggregate(dfPopolazione,tl,var)[-1]
  if(mk==3) {
    t1<-(num[[1]]-1)*sum(agvar[1,])
    t2<-(num[[2]]-1)*sum(agvar[2,])
    t3<-(num[[3]]-1)*sum(agvar[3,])
    print("T")
    print(t1)
    print(t2)
    w<-t1 + t2 +t3
  }
  else {
    t1<-(num[[1]]-1)*sum(agvar[1,])
    t2<-(num[[2]]-1)*sum(agvar[2,])
    print("T")
    print(t1)
    print(t2)
    w<-t1 + t2
  }
  print("Misure associate ai cluster:")
  print(w)
  b<-trHI - w
  print("b/trHI")
  print((b/trHI)*100)
  print("s/trHI")
  print((w/trHI))
  print((w/trHI) + (b/trHI))
}