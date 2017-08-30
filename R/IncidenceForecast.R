##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##-----------------------Infectious Disease Forecasting-----------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
#' @include DataContainers.R
#' @include Forecasts.R
#' @include SimulatedIncidenceMatrix.R

##----------------------------------------------------------------------------##
##-----------------------------Incidence Forecast-----------------------------##
##----------------------------------------------------------------------------##

####################################Forecast####################################
#' @title IncidenceForecast
#' @description A basic concrete SimulatedForecast class.
#' @docType class
#' @importFrom R6 R6Class
#' @export IncidenceForecast
#' @keywords forecast
#' @family Forecast
#' @family SimulatedForecast
#' @example IncidenceForecast.R
IncidenceForecast <- R6Class(
  classname = "IncidenceForecast",
  inherit = SimulatedForecast,
  private = list(
    .data = AbstractSimulatedIncidenceMatrix$new()
  ),
  public = list(
    #' @method initialize Create a new IncidenceForecast.
    #' @param data The data to initialize with
    #' @param forecastTimes Boolean representing which times are forecasted, and which times are not.
    initialize = function(data=SimulatedIncidenceMatrix$new(),forecastTimes=c()){
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
    #' @field data The prediction this model is responsible for.  The data should be of class SimulatedIncidenceMatrix
    data = function(value){
      private$defaultActive(".data","private",value)
    }
  )
)
