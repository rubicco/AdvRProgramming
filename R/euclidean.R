#' Title
#'
#' @param x 
#' @param y 
#'
#' @return greater common divisor
#' @export
#'
#' @examples qweqwe
euclidean <-
function(x, y)
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
