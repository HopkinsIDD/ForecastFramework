################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
#####Simulated Forecast Test Functions
################################################################################

#######################################TODO#####################################


source('help-Forecast.R')

is_same_SimulatedForecast_as = function(rhs){
  return(function(lhs){
    is_same_Forecast_as(rhs)(lhs)
    equals(rhs$nsim)(lhs$nsim)
    equals(rhs$simulations)(lhs$simulations)
  })
}

test_SimulatedForecast = function(object,name,equivalence){
  if(!('SimulatedForecast' %in% class(object))){
    warning("Object is not of the correct class")
  }
  first_object = object$clone(TRUE)
  test_Forecast(object,name,equivalence)
  test_that(paste(name,": Inherited Methods don't change object"),{
    equivalence(first_object)(object)
  })

  test_that(paste(name,": sample works"),{
    first_object=object$clone(TRUE)

    tempObject = object$clone(TRUE)
    expect_error({
      tempObject$sample
    },NA)

    tempObject = object$clone(TRUE)

    ##Should the sample be an AbstractIncidenceMatrix object?
    tempObject = object$clone(TRUE)
    expect_equal({
      'MatrixData' %in% class(tempObject$sample)
    },TRUE)

    expect_that(first_object,equals(object))
  })


}
