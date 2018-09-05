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
euclidean(100, 1000)
euclidean(14335, 74678)
euclidean(22334, 64531)
euclidean(64531, 22334)
