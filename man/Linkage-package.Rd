\name{Linkage-package}
\alias{Linkage-package}
\alias{Linkage}
\docType{package}
\title{
\packageTitle{Linkage}
}
\description{
\packageDescription{Linkage}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{Linkage}
\packageIndices{Linkage}
It allows to cluster communication networks using the Stochastic Topic Block Model (Bouveyron et al., 2018, <doi:10.1007/s11222-016-9713-7>) by posting jobs through the API of the linkage.fr server, which implements the clustering method. The package also allows to visualize the clustering results returned by the server.
}
\author{
\packageAuthor{Linkage}

Maintainer: \packageMaintainer{Linkage}
}
\references{
C. Bouveyron, P. Latouche and R. Zreik, The Stochastic Topic Block Model for the Clustering of Networks with Textual Edges, Statistics and Computing, vol. 28(1), pp. 11-31, 2017 <doi:10.1007/s11222-016-9713-7>
}
\keyword{ package }
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
