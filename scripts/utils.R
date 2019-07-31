showBar <- function(vector, lbl, main, rows, posizioneLegenda){
    pal<- distinctColorPalette(rows)
    graph <- barplot(vector, main = main, col = pal)
    legend(posizioneLegenda, legend = lbl, col = pal, lwd=4, ncol=3, cex=0.3)
}

showPareto <- function(totale, main, rows){
    ordinato <- dsTassi[order(dsTassi$Totale, decreasing = TRUE),]
    lbls <- ordinato$Regioni
    ordinato <- ordinato$Totale
    freqRel <- prop.table(ordinato)
    app <- data.frame(ordinato)
    pal<- distinctColorPalette(rows)
    x<- barplot(freqRel, ylim= c(0, 1.5), main = main, col=pal, axisname = FALSE)
    axis(1, las=2, hadj= 0.6, at=x, labels=lbls, line=1, col="transparent")
    lines(x, cumsum(freqRel), type = "b", pch=16)
    text(x-0.2, cumsum(freqRel) + 0.03, paste(format(cumsum(freqRel)*100, digits = 3), "%"))
}