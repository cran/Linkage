plot.topwords <-
function(res,...){
  STBM.colors.edges = rep(RColorBrewer::brewer.pal(8,"Dark2"), 20)
  old_palette = palette()
  palette(STBM.colors.edges)
  Q = res$result$param_clusters
  K = res$result$param_topics
  image(x=1:K,y=1:5,z=matrix(0,K,5),col=0,xlab='',ylab='',xaxt='n',yaxt='n',main='Topics')
  axis(1,at=1:K,labels=paste('Topic',1:K))
  text(expand.grid(x=1:K, y=1:5), labels=res$result$top_words[,5:1], col=rep(1:K,5))
  palette(old_palette)
}

