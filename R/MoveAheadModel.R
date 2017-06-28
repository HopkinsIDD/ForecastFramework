##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##             Spatial Prediction Model Object Oriented Framework             ##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
#' @include Models.R
#' @include IncidenceMatrix.R SimulatedIncidenceMatrix.R
#' @include SimpleForecast.R
#################################MoveAheadModel#################################
#' @title MoveAheadModel
#' @description An autoregressive model which assumes everything stays the same.
#' @docType class
#' @importFrom R6 R6Class
#' @export MoveAheadModel
#' @keywords model forecast
#' @family Models ForecastModels
#' @example MoveAheadModel.R
MoveAheadModel <- R6Class(
  classname = 'MoveAheadModel',
  inherit = RecursiveForecastModel,
  private = list(
    .data = IncidenceMatrix$new(),
    newdata = IncidenceMatrix$new(),
    output = ArrayData$new(),
  	.nsim = 3,
    .predCols = c(as.integer(1)),
    .maxPredCol = as.integer(1)
  ),
  public = list(
    #' @method initialize Create a new MoveAheadModel
    #' @param nsim The number of stochastic simulations to use.
    initialize = function(nsim = 3){
      private$.nsim = nsim
    },
    #' @method fit_ This method is included for compliance with the standard but does nothing.  This model does not need to be fit.
    fit_ = function(){
      if(self$data$ncol <= self$predCols){
        stop("We cannot go further back than the start of the matrix.")
      }
    },
    #' @method predictRow_ This method predicts a particular row.
    #' @param row The row to predict
    #' @param col This is for internal use.
    predictRow_ = function(row,col=0){
      predict_(col)
      #return(private$output$subset(mutate=FALSE,rows=row))
    },
    #' @method predict_ This method predicts as much as possible about the next time step.
    #' @param col This is for internal use.
    predict_ = function(col=0){
      if('predict_' %in% private$.debug){
        browser()
      }
      if(col == 0){
        col=1:private$output$ncol
      }
      private$output$mutate(
        cols=col,
        data = SimulatedIncidenceMatrix$new(
          private$newdata,
          private$.nsim
        )$simulations
      )
      #return(SimulatedIncidenceMatrix$new(private$newdata,private$.nsim))
    },
    #' @method prepareFitData This method gets data ready for fitting and stores it in the model for later use.
    #' @param data The data used to fit the model.
    prepareFitData = function(data){
      private$.data = data$clone(TRUE)
    },
    #' @method preparePredictData This method gets data ready to use to predict and stores it in the model for later use.
    #' @param newdata The data to use when fitting the model.
    preparePredictData = function(newdata){
      if('preparePredictData' %in% private$.debug){
        browser()
      }
      private$newdata = SimulatedIncidenceMatrix$new(data=newdata,nsim=self$nsim)
      private$.nrow = private$newdata$nrow
      private$newdata$addColumns(min(self$predCols))
      private$newdata$lag(min(self$predCols),na.rm=FALSE)
      private$newdata$subset(cols=2:private$newdata$ncol)
      ##Deal with time in year etc here.
    },
    #' @method prepareForecastData This method gets data ready to use to forecast and stores it in the model for later use.
    #' @param data The data to use when forecasting.
    prepareForecastData = function(data){
      if(self$data$ncol <= self$predCols){
        stop("We cannot go further back than the start of the matrix.")
      }
      private$newdata = data$clone(TRUE)
      private$.nrow = private$newdata$nrow
      private$newdata$addColumns(min(self$predCols))
      private$newdata$lag(min(self$predCols),na.rm=TRUE)
      ##Deal with time in year etc here.
    },
    #' @method prepareOutputData This method prepares the output for predict or forecast.
    #' @param inputData The data being used to predict/forecast.
    #' @param steps The number of steps in the forecast. (0 for predict).
    prepareOutputData = function(inputData,steps=0){
      private$output = SimulatedIncidenceMatrix$new(inputData,private$.nsim)
      if(steps > 0){
        private$output$addColumns(steps)
      }
    }
  ),
  active = list(
    #' @field nsim The number of stochastic simulations to perform.
    nsim = function(value){
      private$defaultActive(type='private',name='.nsim',val=value)
    }
  )
)
