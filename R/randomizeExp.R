randomizeExp <-
function(data, sd =0.2){
  data=data*exp(rnorm(prod(dim(data)), 0, sd))
  return(data)
}
