#' Title
#'
#' @param graph 
#' @param init_node 
#'
#' @return asd
#' @export
#'
#' @examples asdasd
dijkstra <-
function(graph, init_node) 
{
  if(!is.data.frame(graph)) 
    stop("'graph' argument is NOT a data.frame!")
  if(NCOL(graph)!=3) 
    stop("'graph' argument has more (or less) than 3 columns!")
  if(!is.numeric(init_node) | length(init_node)!=1) 
    stop("'init_node' argument is not a scalar!")
  if( !all.equal(colnames(graph), c("v1", "v2", "w")) )
    stop("graph names are wrong")
  
  values <- unique(c(graph$v1, graph$v2))
  
  if(!(init_node %in% values)) stop("'init_node' is not in the present values")
  
  d_matrix <- matrix(Inf, length(values), 2)
  #d_matrix[,1] <- values
  d_matrix[init_node,1] <- 0
  unvisited <- values
  curr_node <- init_node
  while(length(unvisited)!=0) 
  {
    indexes <- which(graph$v2==curr_node | graph$v1==curr_node)
    #temp <- graph[indexes,]
    neighbours <- rep(NA, length(indexes))
    w_i <- graph$w[indexes] + d_matrix[curr_node,1]
    for(i in 1:length(indexes))
    {
      if(graph$v1[indexes[i]]!=curr_node) neighbours[i] <- graph$v1[indexes[i]]
      else neighbours[i] <- graph$v2[indexes[i]]
      if(d_matrix[neighbours[i],1]>w_i[i]) 
      {
        d_matrix[neighbours[i],1] <- w_i[i]
        d_matrix[neighbours[i],2] <- neighbours[i]
      }
    }
    curr_node <- which(d_matrix[, 1] == min(d_matrix[unvisited,1]))
    unvisited <- unvisited[-which(unvisited==curr_node)]
  }
  return(d_matrix[,1])
}
