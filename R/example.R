#'Example
#'
#'deqn ASCII example
#'
#'\deqn{ \sigma = \sqrt{ \frac{Z}{n} \sum
#'  \left[ \textstyle\frac{1}{2}\displaystyle
#'    \left( \log \frac{H_i}{L_i} \right)^2  - (2\log 2-1)
#'    \left( \log \frac{C_i}{O_i} \right)^2 \right] }
#'}{sqrt(N/n * runSum(0.5 * log(OHLC[,2]/OHLC[,3])^2 -
#'           (2*log(2)-1) * log(OHLC[,4]/OHLC[,1])^2, n))}
#'
#'@param x An example parameter
#'@return A example result
#'@author Joshua Ulrich
#'@keywords ts
#'@export
"example" <-
  function(x) {
  }