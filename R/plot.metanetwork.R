plot.metanetwork <-
function(res, ...){
  # Display the flows of communication between groups
  edge.width = 10
  edge.minValue = 0.001
  nodeSize.scale = 1
  nodeSize.min = 0.5
  edge.curve = 0.1
  loop.cex = 1
  
  # Data
  Q = res$result$param_clusters; K = res$result$param_topics
  Pi =  matrix(unlist(res$result$pi_mat),nrow=Q)
  Y = as.numeric(res$result$clusters_mat) # the clusters
  M = length(Y)
  topicMat = apply(res$result$theta_qr_mat,1,which.max)
  
  # Parameters
  LWD = (Pi / max(Pi)) * edge.width
  CEX = (table(Y) / M) * Q + nodeSize.min
  
  # Colors
  vertex.col = rep(RColorBrewer::brewer.pal(12,"Paired"), 20)
  edge.col = rep(RColorBrewer::brewer.pal(8,"Dark2"), 20)
  old_palette = palette()
  palette(edge.col)
  
  # The graph
  Pi_bin = (Pi>edge.minValue)*1
  pos = sna::gplot.layout.fruchtermanreingold(Pi_bin, NULL)
  sna::gplot( Pi_bin, label = 1:nrow(Pi),  usecurve=TRUE, edge.curve=edge.curve,
              edge.lwd=LWD, vertex.col = vertex.col[1:Q], edge.col = topicMat, 
              diag = TRUE, loop.cex=loop.cex, vertex.cex = CEX, coord=pos,
              label.col = "black", vertex.border = "black",label.pos=5,...)
  palette(old_palette)
}
