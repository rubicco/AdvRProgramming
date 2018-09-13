\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, init_node) 
{
    if (!is.data.frame(graph)) 
        stop("'graph' argument is NOT a data.frame!")
    if (NCOL(graph) != 3) 
        stop("'graph' argument has more (or less) than 3 columns!")
    if (!is.numeric(init_node) | length(init_node) != 1) 
        stop("'init_node' argument is not a scalar!")
    colnames(graph) <- c("v1", "v2", "w")
    values <- unique(c(graph$v1, graph$v2))
    if (!(init_node \%in\% values)) 
        stop("'init_node' is not in the present values")
    d_matrix <- matrix(Inf, length(values), 2)
    d_matrix[init_node, 1] <- 0
    unvisited <- values
    curr_node <- init_node
    while (length(unvisited) != 0) {
        indexes <- which(graph$v2 == curr_node | graph$v1 == 
            curr_node)
        neighbours <- rep(NA, length(indexes))
        w_i <- graph$w[indexes] + d_matrix[curr_node, 1]
        for (i in 1:length(indexes)) {
            if (graph$v1[indexes[i]] != curr_node) 
                neighbours[i] <- graph$v1[indexes[i]]
            else neighbours[i] <- graph$v2[indexes[i]]
            if (d_matrix[neighbours[i], 1] > w_i[i]) {
                d_matrix[neighbours[i], 1] <- w_i[i]
                d_matrix[neighbours[i], 2] <- neighbours[i]
            }
        }
        curr_node <- which(d_matrix[, 1] == min(d_matrix[unvisited, 
            1]))
        unvisited <- unvisited[-which(unvisited == curr_node)]
    }
    return(d_matrix)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
