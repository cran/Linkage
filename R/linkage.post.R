linkage.post <-
function(file,token,job_title="",clusters_min = 2,
                         clusters_max = 10,topics_min = 2,topics_max = 10,
                           filter_largest_subgraph = TRUE){
  url = paste("https://linkage.fr/api/launch/",token,"/",sep='')
  body = list(file = upload_file(file),
              job_title = job_title,
              clusters_min = clusters_min,
              clusters_max = clusters_max,
              topics_min = topics_min, 
              topics_max = topics_max,
              filter_largest_subgraph = filter_largest_subgraph)
  
  res = POST(url=url,body=body)
  job_id = fromJSON(rawToChar(res$content))$job_id
  cat(paste('Job #',job_id,' is currently processed on Linkage.fr.\n',sep=''))
  job_id
}
