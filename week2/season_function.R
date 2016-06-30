#
season = function(ymd)
{
  month = month(as.Date(ymd))
  day = day(as.Date(ymd))
  
  if(month<3)
  {
    "Winter"
  }
  else if(month==3 && day<20)
  {
    "Winter"
  }
  else if(month<6)
  {
    "Spring"
  }
  else if(month==6 && day<20)
  {
   "Spring" 
  }
  else if(month<9)
  {
    "Summer"
  }
  else if(month==9 && day<20)
  {
    "Summer"
  }
  else if(month<12)
  {
    "Fall"
  }
  else if(month==12 && day<20)
  {
    "Fall"
  }
  else
  {
    "Winter"
  }
  
  
  
}

season = Vectorize(season)