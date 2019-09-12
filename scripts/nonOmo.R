misureNonOmogeneita <- function (df, mk,aggr) {
    n<-nrow(df)
    trHI<-(n-1)*sum(apply(df,2, var))
    print("trHI:")
    print(trHI)
    d<-dist(df, method = "euclidean", diag = TRUE,upper = TRUE) #individua la struttura di Missimilità
    if(aggr=="centroid" || aggr == "median") {
        d = d * d
    }
    
    hls<-hclust(d, method = aggr)
    taglio<-cutree(hls,k=mk,h=NULL)
    num<-table(taglio)
    tl<-list(taglio)
    agvar<-aggregate(df,tl,var)[-1]
    if(mk==3) {
        t1<-(num[[1]]-1)*sum(agvar[1,])
        t2<-(num[[2]]-1)*sum(agvar[2,])
        t3<-(num[[3]]-1)*sum(agvar[3,])
        print("T")
        print(t1)
        print(t2)
        print(t3)
        w<-t1 + t2 +t3
    }
    else {
        t1<-(num[[1]]-1)*sum(agvar[1,])
        t2<-(num[[2]]-1)*sum(agvar[2,])
        print("Prova")
        print((num[[2]]-1))
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