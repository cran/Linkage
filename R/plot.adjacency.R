plot.adjacency <-
function(res,...){
  STBM.colors.vertex = rep(RColorBrewer::brewer.pal(12,"Paired"), 20)
  STBM.colors.edges = rep(RColorBrewer::brewer.pal(8,"Dark2"), 20)
  old_palette = palette()
  palette(STBM.colors.edges)
  X = res$edges+1
  g = network::as.network(X)
  edgeCol = as.numeric(res$result$topics_with_edges[,3]) +1 #+ res$param_clusters
  vertexCol = as.numeric(res$result$clusters_mat) +1
  Q = max(vertexCol)
  
  A = as.matrix.network.adjacency(g)
  A[A>0] = edgeCol 
  ord = order(as.numeric(vertexCol))
  A.ord = A[ord,ord]
  image(A.ord)
  M = nrow(A)
  
  arr.ind = which(A.ord>0, arr.ind = TRUE)
  edgeCol.ord = A.ord[A.ord>0]
  xlab = "Q (recipient)";   ylab = "Q (sender)"
  plot(1, 1, xlim = c(0, M + 1), ylim = c(0, M + 1), type = "n", axes= FALSE,
       xlab=xlab, ylab=ylab, main="Reorganized Adjacency matrix")
  rect(xleft = arr.ind[,2] - 0.5, ybottom = arr.ind[,1] - 0.5, xright = arr.ind[,2] + 0.5,
       ytop = arr.ind[,1] + 0.5, col=edgeCol.ord, border=NA)
  
  limits_row = table(vertexCol)
  limits_row = cumsum(limits_row)[1:(length(limits_row)-1)] + 0.5
  abline(h=c(0.5, limits_row, M + 0.5), col="grey")
  # now the axis
  middle = (c(0.5, limits_row) + c(limits_row, M + 0.5)) / 2
  axis(2, at = middle, lwd=0, labels = 1:Q, las=2)
  
  # COLUMN
  limits_col = table(vertexCol)
  limits_col = cumsum(limits_col)[1:(length(limits_col)-1)] + 0.5
  abline(v=c(0.5, limits_col, M + 0.5), col="grey")
  middle = (c(0.5, limits_col) + c(limits_col, M + 0.5)) /2
  axis(1, at = middle, lwd=0, labels = 1:Q, line = -1)
  palette(old_palette)
}
