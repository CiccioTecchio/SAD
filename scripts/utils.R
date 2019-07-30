showBar <- function(vector, lbl, main, rows){
    pal<- distinctColorPalette(rows)
    graph <- barplot(vector, main = main, col = pal)
    legend("topleft", legend = lbl, col = pal, lwd=4, ncol=3, cex=0.4)
}