# util functions
f2si2<-function (number,rounding=F) 
{
  lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
           0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
           1e+24)
  pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "k", 
           "M", "G", "T", "P", "E", "Z", "Y")
  ix <- findInterval(number, lut)
  if (lut[ix]!=1) {
    if (rounding==T) {
      sistring <- paste(round(number/lut[ix]), pre[ix])
    }
    else {
      sistring <- paste(number/lut[ix], pre[ix])
    } 
  }
  else {
    sistring <- as.character(number)
  }
  return(sistring)
}
