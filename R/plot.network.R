plot.network <-
function(res,...){
  STBM.colors.vertex = rep(RColorBrewer::brewer.pal(12,"Paired"), 20)
  STBM.colors.edges = rep(RColorBrewer::brewer.pal(8,"Dark2"), 20)
  old_palette = palette()
  palette(STBM.colors.edges)
  X = res$edges+1
  g = network::as.network(X)
  edgeCol = as.numeric(res$result$topics_with_edges[,3]) +1 #+ res$param_clusters
  vertexCol = as.numeric(res$result$clusters_mat) +1
  pos = sna::gplot.layout.fruchtermanreingold(g, NULL)
  sna::gplot(g,usecurve=TRUE,edge.curve = 0.05,coord=pos,
             vertex.col = STBM.colors.vertex[vertexCol], edge.col = edgeCol)
  palette(old_palette)
}
