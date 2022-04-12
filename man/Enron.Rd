\name{Enron}
\alias{Enron}
\docType{data}
\title{
The Enron email network
}
\description{
This data set contains an extract of the email network of the Enron company. This extract focuses on the emails exchanged between Enron employees in October 2001. The reported texts of the emails are only the email subjects. The full email data set is available at https://www.cs.cmu.edu/~enron/.
}
\usage{data(Enron)}
\format{
The data frame is organized as follows:

- the first column contains the id of the sender,

- the second column contains the id of the receiver,

- the third column contains the text of the email
}
\source{
The full email data set is available at https://www.cs.cmu.edu/~enron/.
}
\references{
C. Bouveyron, P. Latouche and R. Zreik, The Stochastic Topic Block Model for the Clustering of Networks with Textual Edges, Statistics and Computing, vol. 28(1), pp. 11-31, 2017 <doi:10.1007/s11222-016-9713-7>
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

# Retrieve results (once achievment is 100\%)
res = linkage.getresults(job_id,token)

# Plot the results
plot(res,type='all')
}
}
\keyword{datasets}
