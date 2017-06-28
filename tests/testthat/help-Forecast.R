################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####Forecast Test Functions
################################################################################

is_same_Forecast_as = function(rhs){
  return(function(lhs){
    equals(lhs$data)(rhs$data)
    equals(lhs$model)(rhs$model)
    equals(lhs$forecastMadeTime)(rhs$forecastMadeTime)
    equals(lhs$forecastTimes)(rhs$forecastTimes)
    equals(lhs$nrow)(rhs$nrow)
    equals(lhs$rnames)(rhs$rnames)
  })
}

test_Forecast = function(object,name,equivalence){
  if(!('Forecast' %in% class(object))){
    warning("Object is not of the correct class")
  }
  
  test_that(paste(name,": mean works"),{
    tempObject = object$clone(TRUE)
    expect_error({
      tempObject$mean()
    },NA)
    
    tempObject = object$clone(TRUE)
    expect_that({
      tempObject$mean()
      tempObject
    },equivalence(object))
    
    #Should the mean be an AbstractIncidenceMatrix object?
    tempObject = object$clone(TRUE)
    expect_equal({
      'MatrixData' %in% class(tempObject$mean())
    },TRUE)
  })
  
  test_that(paste(name,": median works"),{
    tempObject = object$clone(TRUE)
    expect_error({
      tempObject$median()
    },NA)
    
    tempObject = object$clone(TRUE)
    expect_that({
      tempObject$median()
      tempObject
    },equivalence(object))
    
    #Should the median be an AbstractIncidenceMatrix object?
    tempObject = object$clone(TRUE)
    expect_equal({
      'MatrixData' %in% class(tempObject$median())
    },TRUE)
  })
}
