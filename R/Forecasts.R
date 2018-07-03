##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##-----------------------Infectious Disease Forecasting-----------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
#' @include DataContainers.R

##----------------------------------------------------------------------------##
##-----------------------------------Forecasts--------------------------------##
##----------------------------------------------------------------------------##

####################################Forecast####################################
#' @title Forecast
#' @description An abstract class for storing the results of forecasting.  These
#'   classes do not contain any data directly, but instead contain a data object.
#'   Extend this class when you want to store the results of a model, and none
#'   of the convenience classes are applicable.
#' @docType class
#' @importFrom R6 R6Class
#' @export Forecast
#' @keywords forecast core
#' @family Forecast
#' @example Forecast.R

Forecast <- R6Class(
  classname = "Forecast",
  inherit = Generic,
  private = list(
    .forecastMadeTime = NA,
    .forecastTimes = NA,
    .model = Generic$new(),
    .data = MatrixData$new()
  ),
  public = list(
    #' @method mean This \bold{must} be extended.  This method extracts the elementwise mean of the forecast.  This function will not change the number of rows or columns in the data, but will convert probabilistic estimates into deterministic ones.
    #' @return a MatrixData.
    mean = function(){
      private$defaultAbstract()
    },
    #' @method median This \bold{must} be extended.  This method extracts the elementwise median of the forecast.  This function will not change the number of rows or columns in the data, but will convert probabilistic estimates into deterministic ones.
    #' @return a MatrixData.
    median = function(){
      private$defaultAbstract()
    },
    #' @method quantile This \bold{must} be extended.  Get the cutoffs for each percentile in alphas.
    #' @param alphas A numeric vector with elements between \code{0} and \code{1} of percentiles to find cutoffs for.
    #' @param na.rm A boolean regarding whether to remove NA values before computing the quantiles.
    #' @return an ArrayData.
    quantile = function(alphas,na.rm=FALSE){
      private$defaultAbstract()
    },
    #' @method binDist This \bold{must} be extended.  Get the distribution of simulations of the data within fixed bins.
    #' @param cutoffs A numeric vector with elements to use as the dividing values for the bins.
    #' @param include.lowest logical, indicating if an x[i] equal to the lowest (or highest, for right = FALSE) breaks value should be included.
    #' @param right logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
    #' @return an ArrayData.
    binDist = function(cutoffs,include.lowest = FALSE,right = TRUE){
      private$defaultAbstract()
    }
  ),
  active = list(
    #' @field forecastMadeTime When the forecast was created.
    forecastMadeTime = function(value){
      if(missing(value)){
        return(private$.forecastMadeTime)
      }
      stop("Do not write directly to the creation time.")
    },
    #' @field forecastTimes The times the forecast is about.
    forecastTimes = function(value){
      if(missing(value)){
        return(private$.forecastTimes)
      }
      stop("If you want different times, do another forecast instead.")
    },
    #' @field model The model used to create the forecast.
    model = function(value){
      if(missing(value)){
        return(private$.model)
      }
      stop("If you want a different model, do another forecast instead.")
    },
    #' @field data  The data used to create the forecast.
    data = function(value){
      if(missing(value)){
        return(private$.data)
      }
      stop("If you want different data, do another forecast instead.")
    }
  )
)

###############################Simulated Forecast###############################
#' @title SimulatedForecast
#' @description This class is a forecast where the data is many simulated trials.
#' @docType class
#' @importFrom R6 R6Class
#' @export SimulatedForecast
#' @keywords forecast
#' @family Forecast
#' @example SimulatedForecast.R
SimulatedForecast <- R6Class(
  classname = "SimulatedForecast",
  inherit = Forecast,
  private = list(
    .nsim = 0,
    .data = AbstractSimulatedIncidenceMatrix$new()
  ),
  public = list(
    #' @method mean This method extracts the elementwise mean of the forecast.  This function will not change the number of rows or columns in the data, but will convert probabilistic estimates into deterministic ones.
    mean = function(){
      self$data$summarize(mean)
    },
    #' @method median This method extracts the elementwise median of the forecast.  This function will not change the number of rows or columns in the data, but will convert probabilistic estimates into deterministic ones.
    #' @return a MatrixData.
    median = function(){
      self$data$summarize(median)
    },
    #' @method quantile Get the cutoffs for each percentile in alphas.
    #' @param alphas A numeric vector with elements between \code{0} and \code{1} of percentiles to find cutoffs for.
    #' @param na.rm A boolean regarding whether to remove NA values before computing the quantiles.
    #' @return an ArrayData.
    quantile = function(alphas,na.rm=FALSE){
      SimulatedIncidenceMatrix$new(
        lapply(
          alphas,
          function(alpha){
            self$data$summarize(function(x){quantile(x , alpha,na.rm=na.rm)})
          }
        )
      )
    },
    #' @method binDist Get the distribution of simulations of the data within fixed bins.
    #' @param cutoffs A numeric vector with elements to use as the dividing values for the bins.  -Inf, and Inf will be added automatically.
    #' @param include.lowest logical, indicating if an x[i] equal to the lowest (or highest, for right = FALSE) breaks value should be included.
    #' @param right logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
    #' @return an ArrayData.
    binDist = function(cutoffs,include.lowest = FALSE,right = TRUE){
      if('binDist' %in% private$.debug){browser()}
      if(include.lowest | (right)){
        warning("The interval names will not be correctly formatted")
      }
      cutoffs = sort(unique(c(cutoffs,-Inf,Inf)))
      data <- aperm(
        apply(
          self$data$arr,
          c(1,2),
          function(x){
            c(
              table(cut(
                x=x,
                breaks=cutoffs,
                include.lowest=include.lowest,
                right=right
              ))/length(x),
              "NA"=sum(is.na(x))/length(x)
            )
          }
        ),
        c(2,3,1)
      )
      if(!is.null(self$data$rnames)){
        dimnames(data)[[1]] <- self$data$rnames
      }
      if(!is.null(self$data$cnames)){
        dimnames(data)[[2]] <- self$data$cnames
      }
      dimnames(data)[[3]] <- c(paste("[",cutoffs[-length(cutoffs)],",",cutoffs[-1],")",sep=''),"NA")
      ## Special name for the final box if include.lowest is false
      dimnames(data)[[3]][length(cutoffs)-1] <- paste('[',cutoffs[length(cutoffs)-1], ",", cutoffs[length(cutoffs)-0],"]",sep='')
      ## Even in the include.lowest=FALSE case, we want Inf/-Inf to get reported normally
      if(!include.lowest){
        tmp.data <- aperm(
          apply(
            self$data$arr,
            c(1,2),
            function(x){
              table(cut(
                x=x,
                breaks=cutoffs,
                include.lowest=TRUE,
                right=right
              ))/length(x)
            }
          ),
          c(2,3,1)
        )
        if(!right){
          data[,,length(cutoffs)-1] = tmp.data[,,length(cutoffs)-1]
        } else {
          data[,,1] = tmp.data[,,1]
        }
      }
      rc <- SimulatedIncidenceMatrix$new( data)
      rc$rowData <- self$data$rowData
      rc$colData <- self$data$colData
      rc$dimData[[3]] <- list(start=c(cutoffs[-length(cutoffs)],NA),end=c(cutoffs[-1],NA))
      return(rc)
    }
  ),
  active = list(
    #' @field nsim The number of simulations.
    nsim = function(value){
      if('nsim' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.nsim)
      }
      stop("Do not modify the number of simulations directly")
    },
    #' @field sample Draw a random sample from the possible model predictions.  Please see implementation of the data for the properties of the sampling.
    sample = function (value){
      if(!missing(value)){
        stop("Do not try to write to a random sample")
      }
      return(self$data$sample)
    }
  )
)
