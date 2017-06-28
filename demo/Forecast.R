##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##-----------------------Infectious Disease Forecasting-----------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
#' @include DataContainers.R
#' @include Forecasts.R

##----------------------------------------------------------------------------##
##------------------------------Simple Forecast-------------------------------##
##----------------------------------------------------------------------------##

####################################Forecast####################################
#' @title SimpleForecast
#' @description A basic concrete SimulatedForecast class.
#' @docType class
#' @importFrom R6 R6Class
#' @export SimpleForecast
#' @keywords forecast
#' @family Forecast
#' @example SimpleForecast.R
SimpleForecast <- R6Class(
  classname = "SimpleForecast",
  inherit = Forecast,
  private = list(
    .data = MatrixData$new()
  ),
  public = list(
    #' @method binDist This throws an error.  This method is not meaningful for this data.
    #' @param cutoffs A numeric vector with elements to use as the dividing values for the bins.
    binDist = function(cutoffs){
      stop("This doesn't really make sense.")
    },
    #' @method mean This method returns the data.  It is included for compliance.
    #' @return a MatrixData.
    mean = function(){
      return(self$data)
    },
    #' @method median This method returns the data.  It is included for compliance.
    #' @return a MatrixData.
    median = function(){
      return(self$data)
    },
    #' @method quantile This throws an error.  This method is not meaningful for this data.
    #' @param alphas A numeric vector with elements between \code{0} and \code{1} of percentiles to find cutoffs for.
    #' @return an ArrayData.
    quantile = function(alphas){
      stop("This doesn't really make sense.")
    },
    #' @method initialize Create a new SimpleForecast.
    #' @param data The data to initialize with
    #' @param forecastTimes Boolean representing which times are forecasted, and which times are not.
    initialize = function(data,forecastTimes){
    	if(missing(data) && missing(forecastTimes)){
    		return()
    	}
      if(data$ncol != length(forecastTimes)){
        stop("The number of columns should be the number of times forecasted.")
      }
      #' @importFrom lubridate now
      private$.forecastMadeTime = now()
      private$.forecastTimes = forecastTimes
      private$.data = data
    }
  ),
  active = list(
  )
)
