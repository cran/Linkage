\name{linkage.post}
\alias{linkage.post}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Post a job on Linkage.fr to cluster a network with STBM
}
\description{
Post a clustering job on the server Linkage.fr though the API. The Linkage.fr server implements the Stochastic Topic Block Model (STBM, Bouveyron et al., 2018, doi:10.1007/s11222-016-9713-7).

The users should have registered on the web server https://linkage.fr. Registration is free of charge for individual and academic users.}
\usage{
linkage.post(file, token, job_title = "", clusters_min = 2, clusters_max = 10,
              topics_min = 2, topics_max = 10, filter_largest_subgraph = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
the location on the disk of the CSV file containing the communication network. Each line of tje CSV file should be of the form: sender_id, receiver_id, text of the message.
}
  \item{token}{
The token of the user. This personal token can be found on https://linkage.fr/developers/ after registration. Registration is free of charge for individual and academic users.
}
  \item{job_title}{
Title of the job
}
  \item{clusters_min}{
Minimum number of node clusters to test
}
  \item{clusters_max}{
Maximum number of node clusters to test
}
  \item{topics_min}{
Minimum number of topics to test
}
  \item{topics_max}{
Maximum number of topics to test
}
  \item{filter_largest_subgraph}{
a boolean indicating if the clustering should be done only on the largest subgraph or not}
}
\value{
The id of the job is returned.
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