##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##----------------------Infectious Disease Forecasting------------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
#' @include DataContainers.R Forecasts.R

##----------------------------------------------------------------------------##
##----------------------------------Models------------------------------------##
##----------------------------------------------------------------------------##

#######################################Model####################################
#' @title Model
#' @description An abstract class for making predictions.  This class contains
#'   only what's necessary for data based modeling.  For the most part, the
#'   other model classes in this package are there to make extending this class
#'   easier.  You should extend this class directly if you are making a model
#'   which does not forecast, and none of the convenience classes seem
#'   appropriate.
#' @docType class
#' @importFrom R6 R6Class
#' @export Model
#' @keywords Model core
#' @family model
#' @example Model.R
Model <- R6Class(
  classname="Model",
  inherit = Generic,
  private = list(
    output = Forecast$new()
  ),
  public = list(
    #' @method predict This method \bold{must} be extended.  Predict using the model
    #' @param newdata the data to predict.
    predict = function(newdata){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method fit This method \bold{must} be extended.  Get the model ready to predict.
    #' @param data The data to fit the model to.
    fit = function(data){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    }
  )
)

####################################ForecastModel################################

#' @title ForecastModel
#' @description A model for predicting multiple time steps into the future.  You
#'   should extend this class if you are predicting over some future time period
#'   and none of the convenience classes which extend forecast are applicable.
#' @docType class
#' @importFrom R6 R6Class
#' @export ForecastModel
#' @keywords model forecast core
#' @family Models ForecastModels
#' @example ForecastModel.R
ForecastModel <- R6Class(
  classname="ForecastModel",
  inherit=Model,
  public = list(
    #' @method forecast This method \bold{must} be extended.  This function is similar to predict, in that it predicts the rows of the input data, however it can predict multiple timesteps into the future, instead of a single timestep.
    #' @param newdata The data to forecast from.
    #' @param steps The number of timesteps into the future to predict.
    #' @return A forecast
    #' @mutate private$output This function should save the return here.
    forecast = function(newdata,steps){
      ##AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    }
  )
)

##############################RecursiveForecastModel############################
#' @title RecursiveForecastModel
#' @description A model for recursively predicting multiple timesteps into the
#'   future.  This class implements a particular type of forecasting, where a
#'   predict method is called to predict each successive time step in order.
#'   Extend this class if you have a predict method, but not a special forecast
#'   method.
#' @docType class
#' @importFrom R6 R6Class
#' @export RecursiveForecastModel
#' @keywords model forecast
#' @family Models ForecastModels
#' @example MoveAheadModel.R
RecursiveForecastModel <- R6Class(
  classname="RecursiveForecastModel",
  inherit=ForecastModel,
  private = list(
    ##Where we put the data used for fitting/predicting.
    .data = MatrixData$new(),
    ##Where we put temporary data during either fitting or predicting.
    newdata = MatrixData$new(),
    ##Since the model is matrix-like, we keep track of which columns (relative to the current column)
    ##We keep track of whether the model has been fit to some data or not.
    modelFitted = FALSE,
    .maxPredCol = as.integer(NA),
    .predCols = as.integer(c(NA)),
    output=SimulatedForecast$new(),
    .nrow = NA,
    rownames = c(''),
    .nsim = NA
  ),
  public = list(
    #' @method fit Fit the model for predicting.  This method breaks down the fitting  process into two parts, data preparation, and the model fitting.
    #' @param data The data to use to fit the model.  This object should be a  MatrixData object.
    fit = function(data){
      ## for debugging: see AbstractClasses.R::Generic::debug for details.
      if('fit' %in% private$.debug){
        browser()
      }
      ##We set the model back to unfitted.  We only want it to seem fitted when
      ##  fit_ runs successfully
      private$modelFitted=FALSE
      ##Note that data only gets passed to prepareFitData, and not to fit_.  This
      ##  is because prepareFitData stores the data within the model, so we don't
      ##  need to hold on to it anymore.
      self$prepareFitData(data)
      ##After preparing, we do the actual data analysis parts.
      self$fit_()
      ##The model is now fitted.
      private$modelFitted=TRUE
    },
    #' @method predict Use a model previously fit with fit to predict. This function does not assume any data preprocessing.  This function should not, in general need  to be overwritten by the user.  The main prediction function is predict_, so please modify that function instead if possible.
    #' @param newdata Optional.  The data used to predict.  This data should be a MatrixData object.
    predict = function(newdata){
      ## for debugging: see AbstractClasses.R::Generic::debug for details.
      if('predict' %in% private$.debug){
        browser()
      }
      if(!private$modelFitted){
        stop("Please fit the model before predicting")
      }
      ##This function just calls auxilliary functions.  These auxilliary functions
      ##  will be written by the user specifically tailored to their model.
      ##One for preparing the input
      self$preparePredictData(newdata)
      ##One for preparing the output
      self$prepareOutputData(newdata)
      ##One for transforming between the two
      self$predict_()
      return(
        IncidenceForecast$new(
          private$output,
          forecastTimes = (1:private$output$ncol == private$output$ncol)
        )
      )
    },
    #' @method fit_ This method \bold{must} be extended.  Fit the model for predicting all of the rows. Assumes the data has been  put into place.
    #' @reference private$.data The data to use in fitting the model is here. private$newdata is an AbstractIncidenceMatrix object, which is mainly a matrix.  The dimensions correspond to Variable - Named row with the variable name as rowname. Instance - Each instance represents a slice of data to be predicted.
    fit_ = function(){
      private$defaultAbstract()
    },
    #' @method preparePredictData This method \bold{must} be extended.  Take \code{newdata} and use it to prepare the model, so that predicting doesn't need to directly reference it.  This allows the model to make multiple predict_, predictRow_, or forecast calls without re-allocating the data every time.
    #' @param newdata  The data to prepare.
    preparePredictData = function(newdata){
      private$defaultAbstract()
    },
    #' @method prepareFitData This method \bold{must} be extended.  Take \code{data} and store it in the object.  This allows the data to be referenced later in predict() where newdata is NULL.  It may also be helpful to put other data preparation steps in this method, so that the fit function runs more smoothly.
    #' @param data  The data to prepare.
    prepareFitData = function(data){
      private$defaultAbstract()
    },
    #' @method predictRow Using a model previously fit with \code{fitRow} to predict the  \code{row}th row of the next column. This function does not assume any data preprocessing.  Since the predict method predicts every row at the  the same time, we include this method for predicting only a single row.
    #' @param row The row to predict the value of.
    #' @param newdata Optional.  The data use to predict.  This can either be a matrix, appropriately formatted lag vector, or NULL to predict based on the data used to fit.
    #' @param col This is for internal use only.
    predictRow = function(newdata,row,col){
      ##This function just calls auxilliary functions.  These auxilliary functions
      ##  will be written by the user specifically tailored to their model.
      ##One for preparing the input
      self$preparePredictData(newdata)
      ##One for preparing the output
      self$prepareOutputData(newdata)
      ##One for transforming between the two.
      self$predictRow_(row,col)
      return(
        IncidenceForecast$new(
          private$output$subset(rows=row,mutate=FALSE),
          forecastTimes = (1:private$output$ncol == private$output$ncol)
        )
      )
    },
    #' @method predictRow_ This method \bold{must} be extended.  Using a model previously fit with \code{fitRow} to predict the  \code{row}th row of the next column. This function assumes that all of the data preprocessing has already been taken care of.
    #' @param row The row to predict the value of.
    #' @param private$newdata The data to use in fitting the model is here. private$newdata is an MatrixData object, the three dimensions  correspond to Variable - Named row with the variable name as rowname. Instance - Each instance represents a slice of data to be predicted.
    #' @return private$output The return value should be both stored in  private$output and returned using return.  This should contain the  results of the prediction in a Forecast object with dimensions similar to private$newdata.
    predictRow_ = function(row){
      private$defaultAbstract()
    },
    #' @method predict_ This method \bold{must} be extended.  Using a model previously fit with \code{fit} to predict each row of the  next column. This function assumes that all of the data preprocessing has already been taken care of.
    #' @param private$newdata The data to use in fitting the model is here. private$newdata is an MatrixData object, the three dimensions  correspond to Variable - Named row with the variable name as rowname. Instance - Each instance represents a slice of data to be predicted.
    #' @return private$output The return value should be both stored in  private$output and returned using return.  This should contain the  results of the prediction in a Forecast object with dimensions similar to private$newdata.
    #' @param col Which columns of private$output should be modified.  This  parameter is mainly used in forecast, but could be used to store repeated predict_ calls in a single matrix.
    predict_ = function(col=0){
      private$defaultAbstract()
    },
    #' @method forecast Using a model previously fit with \code{fit} to predict the next \code{steps} columns.  This function assumes that all of the data preprocessing has already been taken care of.  This function is similar to predict, except that it can predict multiple time steps into the future instead of a single timestep.
    #' @param newdata The data to forecast from.
    #' @param steps The number of timesteps into the future to predict.
    #' @param stochastic Treat the data as though drawn from a random  distribution, and recover the true value.  Can be used to calculate  confidence intervals, and investigate model sensitivity.
    #' @param nsims The number of simulations to do if stochastic.
    #' @param addToData Whether or not to mutate the data to incorporate the new predictions.
    #' @return private$output This function should both modify and return private$output.
    forecast = function(newdata,steps=1,stochastic='Poisson',nsims=self$nsim,addToData = FALSE){

      ## for debugging: see AbstractClasses.R::Generic::debug for details.
      if('forecast' %in% private$.debug){
        browser()
      }

      ##Check to make sure the fit function has been run (or possibly that the
      ##  model needs no fit)
      if(!private$modelFitted){
        stop("Please fit the model before predicting")
      }

      ##Start by storing the parameter values where appropriate.
      private$.nsim = nsims
      ncolOld = newdata$ncol
      ##Prepare the output data, so we're ready to forecast.
      self$prepareOutputData(newdata,steps)
      ##This is a recursive forecast, so we start by forecasting 1, and then using
      ##the first step to do the second step until we get all of the steps.
      for(i in 1:steps){
        ##In each step, we prepare the data for forecasting.  In this case, we are
        ##  extracting the data from the output, and using only some of it to
        ##  forecast.
        ##We could potentially speed up this step by adding having predict_ only
        ##  predict certain columns when the col_ flag is on.
        ##if we are predicting the i'th step, then
        ##  ncolOld + i - self$maxPredCol is the first index we need to predict
        ##  ncolOld + i-1 is the last index we need to predict
        ##Models.R::ForecastModel::prepareForecastData
        self$prepareForecastData(
          ##DataContainers.R::MatrixData::subset
          private$output$subset(
            mutate=FALSE,
            cols=(ncolOld + (i -self$maxPredCol)):(ncolOld + (i-1))
          )
        )
        ##With everything in place, we make a call to predict.
        ##ncolOld + i is where the ith step's prediction should be stored.
        self$predict_(col=ncolOld+i)
        ##We now add noise based on stochastic.
        if(stochastic == 'Deterministic'){
          ##Do nothing, because we're deterministic.
          if(nsims > 1){
            warning("If the model is deterministic, then doing multiple simulations is a waste of time.")
          }
        }
        else if(stochastic == 'Poisson'){
          ##Note that the SimulatedForecastClass allows us to add the error
          ##  automatically
          ##Simulations::AbstractSimulatedIncidenceMatrix::addError
          private$output$addError(
            rows = 1:private$.nrow,
            cols = private$output$ncol-steps+i,
            type = 'Poisson'
          )
        }
        else{
          ##General catch all for other noise types.
          stop(paste(
            "Forecasting with option: stochastic =",
            stochastic,
            "is not yet implemented."
          ))
        }
      }
      ##At this point we are finished our forecast.  All that remains is to return
      if(!addToData){
        ##Remove all of the old data
        ##DataContainers.R::MatrixData::tail
        private$output$tail(k=steps,direction=2)
      }
      return(
        IncidenceForecast$new(
          private$output,
          forecastTimes = c(
            rep(FALSE,private$output$ncol-steps),
            rep(TRUE,steps)
          )
        )
      )
    },
    #' @method prepareForecastData This method \bold{must} be extended.  This function takes input data and prepares it to forecast.  It should in  principle, be similar to preparePredictData, but sometimes forecasting  requires different preparation from prediction.
    #' @param data The data to prepare for forecasting from.
    #' @return private$newdata Store the processed value here, so that forecast can access it.
    prepareForecastData = function(data){
      ##AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method prepareOutputData This method \bold{must} be extended.  This function takes the input data and constructs an appropriate container for the output of the model.
    #' @param inputData The input to the model.  Used to determine properties of the container.
    #' @param steps If the input data is used as part of a forecast, the number of steps is passed in case the output size depends on the number of time steps.
    prepareOutputData = function(inputData,steps=0){
      private$defaultAbstract()
    }
  ),
  active = list(
    #' @field data A MatrixData containing the data used in fitting of the model.
    data = function(value){
      private$defaultActive('.data','private',value)
    },
    #' @field maxPredCol The farthest back column from the output value used in prediction.
    maxPredCol = function(value){
      if(missing(value)){
        return(private$defaultActive('.maxPredCol','private',value))
      }
      stop("Do not write directly to the maxPredCol")
    },
    #' @field predCols An array of which columns are used in prediction.  1 means the column before the prediction, 2 the one before that etc.
    predCols = function(value){
      ## for debugging: see AbstractClasses.R::Generic::debug for details.
      if('predCols' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.predCols)
      }
      ##AbstractClasses.R::Generic::defaultActive
      private$defaultActive('.predCols','private',value)
      private$.maxPredCol = max(private$.predCols)
    }
  )
)
