linkage.check <-
function(token){
  ans = GET(url=paste("https://linkage.fr/api/jobs/",token,"/?last",sep=''))
  progress = fromJSON(rawToChar(ans$content))$progress
  job_id = fromJSON(rawToChar(ans$content))$id
  cat(paste('Achievment of last job (#',job_id,'):\t',sep=''),round(progress*100,1),'%\n')
  fromJSON(rawToChar(ans$content))
}
