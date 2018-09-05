euclidean_recursive <- function(x, y) 
{
  if(x==y) return(x)
  else if(x>y) return( euclidean(x-y, y) )
  else return( euclidean(x, y-x) )
}

euclidean_recursive(252, 105)
euclidean_recursive(123612, 13892347912)
euclidean_recursive(14335, 74678)

euclidean_subtraction <- function(x, y)
{
  while(x!=1 & y!=1 & x!=y) 
  {
    if(x>y) x <- x-y
    else y <- y-x
  }
  return(min(x, y))
}

euclidean <- function(x, y)
{
  if(!is.numeric(x) | !is.numeric(y) | length(x)!=1 | length(y)!=1) 
    stop("Arguments are not numeric!")
  
  r = 1
  
  while(r!=0) 
  {
    
    if(x>y) 
      {
        r <- x%%y
        x <- r
      }
    
    else
    {
      r <- y%%x
      y <- r
    }
    
  }
  
  return( max(x, y) )
}

euclidean(123612, 13892347912)
euclidean(252, 105)
euclidean(105, 252)
euclidean(100, 1000)
euclidean(14335, 74678)
euclidean(22334, 64531)
euclidean(64531, 22334)
euclidean(5, 5)

dijkstra <- function(graph, init_node) 
{
  if(!is.data.frame(graph)) 
    stop("'graph' argument is NOT a data.frame!")
  if(NCOL(graph)!=3) 
    stop("'graph' argument has more (or less) than 3 columns!")
  if(!is.numeric(init_code) | length(init_node)!=1) 
    stop("'init_node' argument is not a scalar!")
  
  colnames(graph) <- c("v1", "v2", "w")
  values <- unique(c(graph$v1, graph$v2))
  
  if(!(init_node %in% values)) stop("'init_node' is not in the present values")
  
  d_matrix <- matrix(Inf, length(values), 2)
  #d_matrix[,1] <- values
  d_matrix[init_node,1] <- 0
  unvisited <- values
  curr_node = init_node
  while(length(unvisited)!=0) 
  {
    indexes = c(which(graph$v2==curr_node | graph$v1==curr_node))
    #temp <- graph[indexes,]
    c(graph$v1[which(graph$v2==curr_node)], graph$v2[which(graph$v1==curr_node)])
    curr_node
  }
  unvisited <- unvisited[-which(unvisited==4)]
}