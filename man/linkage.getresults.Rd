\name{linkage.getresults}
\alias{linkage.getresults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve results for a specific job.
}
\description{
Retrieve results for a specific job posted on the Linkage.fr server.
}
\usage{
linkage.getresults(job_id, token)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{job_id}{
The id of the job to retrieve (as returned by the linkage.post or the linkage.check functions).
}
  \item{token}{
The token of the user. This personal token can be found on https://linkage.fr/developers/ after registration. Registration is free of charge for individual and academic users.
}
}
\value{
It returns a list containing in particular:
\item{job_id}{the job id}
\item{nb_nodes}{the number of nodes}
\item{nb_edges}{the number of edges}
\item{clusters_optim}{the optimal number of clusters}
\item{topics_optim}{the optimal number of topics}
\item{dictionary}{the list of words used in the texts}
\item{result}{a list containing the clustering results for the optimal numbers of clusters and topics. This list contains in particular:

  - clusters_mat: clustering of the nodes
  
  - rho_mat: node cluster proportions
  
  - pi_mat: estimated connexion probabilities between clusters
  
  - theta_qr_mat: estimated proportions of topics in interactions between groups
  
  - top_words: most representative words for each topic
}
}
\references{
C. Bouveyron, P. Latouche and R. Zreik, The Stochastic Topic Block Model for the Clustering of Networks with Textual Edges, Statistics and Computing, vol. 28(1), pp. 11-31, 2017 <doi:10.1007/s11222-016-9713-7>
}
\author{
Charles Bouveyron <charles.bouveyron@gmail.com>
}
\examples{
\dontrun{
data(Enron)
write.table(Enron, file="Enron.csv",row.names=FALSE,col.names=FALSE, sep=",")
file = "Enron.csv"

# Provide the user token, which is provided on "developers" page
# of http://linkage.fr (after registration)
token = "xxxxxxxxxxxxxxxxxxxx"

# Post the job
job_id = linkage.post(file, token, job_title="My job: Enron",
                      clusters_min = 8, clusters_max = 8,
                      topics_min = 6,topics_max = 6,
                      filter_largest_subgraph = TRUE)

# Monitor achievment of the current job
ans = linkage.check(token)

# Retrieve results (once achievment is 100%)
res = linkage.getresults(job_id,token)

# Plot the results
plot(res,type='all')
}
}
