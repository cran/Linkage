plot.rho <-
function(res,...){
  STBM.colors.vertex = rep(RColorBrewer::brewer.pal(12,"Paired"), 20)
  old_palette = palette()
  palette(STBM.colors.vertex)
  Q = res$result$param_clusters
  barplot(res$result$rho_mat,names.arg = paste('Cluster',1:Q),
          main='Group proportions (rho)',col=1:Q)
  palette(old_palette)
}
