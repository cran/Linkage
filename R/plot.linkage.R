plot.linkage <-
function(x,type="all",...){
  # Recovering information
  res = x
  type = tolower(as.character(type))
  allTypes = c("all","network","metanetwork","topics","proportions","adjacency")
  type = match.arg(type, allTypes)
  
  # Plot
  if (type=="all"){ # All results
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    par(mfrow=c(2,2))
    plot.network(res,...)
    plot.metanetwork(res,...)
    plot.topwords(res,...)
    plot.rho(res,...)
  }
  else if (type=="network"){ # Network
    plot.network(res,...)
  }
  else if (type=="metanetwork"){ # Metanetwork
    plot.metanetwork(res,...)
  }
  else if (type=="topics"){ # Topics
    plot.topwords(res,...)
  }
  else if (type=="proportions"){ # Group proportions
    plot.rho(res,...)
  }
  else if (type=="adjacency"){ # Group proportions
    plot.adjacency(res,...)
  }
}
