is_weekend = function(vec)
{
  col = vector(mode= "numeric", length = length(vec))
  
  if (wday(vec) ==1 | wday(vec)==7)
  {
    TRUE
  }
  
  else
  {
    FALSE
  }
  
}

is_weekend = Vectorize(is_weekend)