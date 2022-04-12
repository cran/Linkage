\name{plot.linkage}
\alias{plot.linkage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The plot function for 'linkage' objects.
}
\description{
This function plots different information about 'linkage' objects.}
\usage{
\method{plot}{linkage}(x, type="all", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of type 'linkage' to plot
}
  \item{type}{
the type of information to plot:
  
  - "all": all information,
  
  - "network": the clustered network,
  
  - "metanetwork": the metanetwork which summarizes all model parameters,
  
  - "topics": the most representative words of each topic,
  
  - "prop": the node cluster proportions.
}
  \item{\dots}{
Additional options to pass to the plot function.
}
}
\value{No value is returned by this function.}
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