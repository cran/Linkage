linkage.getresults <-
function(job_id,token){
  ans = GET(url=paste("https://linkage.fr/api/jobs/",token,"/?",sep=''))
  ans = fromJSON(rawToChar(ans$content))
  ind = which(ans$id == job_id)
  clusters_optim = ans$clusters_optim[ind]
  topics_optim = ans$topics_optim[ind]
  
  # Retrieve best result
  out = GET(url=paste("https://linkage.fr/api/retrieve/",token,"/",job_id,
                      "/?clusters=",clusters_optim,
                      "&topics=",topics_optim,sep=''))
  results = fromJSON(rawToChar(out$content))
  cat(paste('Results of job #',job_id,' has been retrived.\n',sep=''))
  class(results) = "linkage"
  results
}
