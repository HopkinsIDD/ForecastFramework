##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##----------------------Infectious Disease Forecasting------------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
#' @include AbstractClasses.R
#######################################TODO#####################################

##----------------------------------------------------------------------------##
##-------------------------------Data Containers------------------------------##
##----------------------------------------------------------------------------##

################################Data Container#############################
#' @title DataContainer
#' @description A class for storing a matrix and relevant metaData.  This class
#'  is the starting point for all of the Data oriented classes.  However, its
#'  not really useful on its own, and serves mostly as a placeholder generic
#'  class in case we want to implement a drastically different type of model.
#'  This class should only need to be extended in the case where we implement
#'  something outside the scope of MatrixDataContainer
#' @docType class
#' @importFrom R6 R6Class
#' @export DataContainer
#' @keywords data
#' @family base
#' @example DataContainer.R
DataContainer <- R6Class(
  classname='DataContainer',
  inherit=Generic,
  private = list(
    ##Variables starting with a '.' represent hidden values corresponding to
    ##  active bindings of the same name without the .
    .metaData = list()
  ),
  active = list(
    #' @field metaData Any data not part of the main data structure.
    metaData = function(value){
      ## for debugging: see AbstractClasses::Generic::debug for details.
      if('metaData' %in% private$.debug){
        browser()
      }
      ##AbstractClasses::Generic::defaultActive
      private$defaultActive('.metaData','private',value)
    }
  )
)

#################################Matrix Data###############################
#' @title MatrixData
#' @description An abstract class for storing metaData relevant to a matrix.
#'   This class forms the backbone of the data structures.  It enforces the core
#'   functionality we want the data we model to have.  This class covers any
#'   data which has two major axes, and can be thought of as a matrix.  It also
#'   can cover the case where the data is matrix-like, but our representation of
#'   the data is not matrix-like.
##       TODO: make an example for this (think cholera data).
#' @docType class
#' @importFrom R6 R6Class
#' @export MatrixData
#' @keywords data core
#' @family MatrixData
#' @example MatrixData.R

MatrixData <- R6Class(
  classname = "MatrixData",
  inherit = DataContainer,
  private = list(
    ##Variables starting with a '.' represent hidden values corresponding
    ##  to active bindings of the same name without the .
    ##Typing is not as strong as it could be here...
    ##These are lists of lists.  Each inner list should be of length determined
    ##  by the matrix.  ie: if there are 3 rows, rowData might be
    ##  list(list(1,2,3),list(3,4,5))
    .colData = list(),
    .rowData = list(),
    ##This will either be a list of lists of lists (the inner lists would have
    ##  the same size as the matrix), or a list of matrices.  Not sure yet.
    .cellData = list(),
    ##These track the number of rows and columns of our data.
    .nrow = 0,
    .ncol = 0,
    ##These track the names of those rows and columns if applicable.
    .rnames = NULL,
    .cnames = NULL,
    .mat = 0+matrix(NA,0,0)
  ),
  ##Active bindings are overloads to the <- operator.  If a has a binding b,
  ##  then a$b is a::b() and a$b <- c is a::b(c).  In general, if value is
  ##  missing, return the object, if value is not missing, perform an assignment
  active = list(
    #' @field colData A list of metadata associated with the columns of the data.
    colData = function(value){
      ## AbstractClasses.R::Generic::defaultActive
      private$defaultActive('.colData','private',value)
    },
    #' @field rowData A list of metadata associated with the columns of the data.
    rowData = function(value){
      ## AbstractClasses.R::Generic::defaultActive
      private$defaultActive('.rowData','private',value)
    },
    #' @field cellData A list of metadata associated with the cells of the data.
    cellData = function(value){
      ## AbstractClasses.R::Generic::defaultActive
      if(missing(value)){
        return(private$.cellData)
      }
      private$defaultAbstract()
    },
    ##Change this so nrow and ncol cannot be written to directly
    #' @field nrow The number of rows in the data
    nrow = function(value){
      ## for debugging: see AbstractClasses::Generic::debug for details.
      if('nrow' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.nrow)
      }
      ##The number of rows should be automatically calculated so that it doesn't
      ##differ from the true number
      stop("Do not modify the number of rows directly")
    },
    #' @field ncol The number of columns in the data.
    ncol = function(value){
      ## for debugging: see AbstractClasses::Generic::debug for details.
      if('ncol' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.ncol)
      }
      ##The number of columns should be automatically calculated so that it
      ##doesn't differ from the true number
      stop("Do not modify the number of columns directly")
    },
    #' @field rnames The names of rows in the data.
    rnames = function(value){
      ## for debugging: see AbstractClasses::Generic::debug for details.
      if('rnames' %in% private$.debug){
        browser()
      }
      if(!missing(value)){stop("Do not write directly to the row names")}
      return(private$.rnames)
    },
    #' @field cnames The names of columns in the data.
    cnames = function(value){
      ## for debugging: see AbstractClasses::Generic::debug for details.
      if('cnames' %in% private$.debug){
        browser()
      }
      if(!missing(value)){stop("Do not write directly to the column names")}
      return(private$.cnames)
    },
    #' @field mat This is the matrix.  For extensibility, it cannot be written to directly and must be modified through methods.
    mat = function(value){
      if(!missing(value)){stop("Do not write directly to the matrix")}
      return(private$.mat)
    }
  )
)

############################Abstract Incidence Matrix###########################
#' @title AbstractIncidenceMatrix
#' @description An abstract class for storing an actual matrix.  It has an
#'   actual matrix of data mat, which it is responsible for storing.  For
#'   creating matrices with particular metadata, consider extending
#'   IncidenceMatrix instead of this class.  Extend this class if you have data
#'   which can be thought of as a matrix, but that is not its true form.
##      TODO: Include an example of this.
#' @docType class
#' @importFrom R6 R6Class
#' @export AbstractIncidenceMatrix
#' @keywords data
#' @family MatrixData
#' @example AbstractIncidenceMatrix.R

AbstractIncidenceMatrix <- R6Class(
  classname = "AbstractIncidenceMatrix",
  inherit = MatrixData,
  public = list(
    #' @method subset This method \bold{must} be extended.  Select the data corresponding to the rows \code{rows} and the columns \code{columns}.  \code{rows} and \code{columns} can be either numeric or named indices.
    #' @param rows An row index or list of row indices which can be either numeric or named.
    #' @param cols An column index or list of column indices which can be either numeric or named.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    subset = function(rows,cols,mutate=TRUE...){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method tail This method \bold{must} be extended.  Select the last \code{k} slices of the data in dimension \code{direction}.
    #' @param k The number of slices to keep.
    #' @param direction The dimension to take a subset of. 1 for row, 2 for column.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    tail = function(k,direction,mutate=TRUE...){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method head This method \bold{must} be extended.  Select the first k slices of the data in dimension direction.
    #' @param k The number of slices to keep.
    #' @param direction The dimension to take a subset of. 1 for row, 2 for column.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    head = function(k,direction,mutate=TRUE...){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method lag This method \bold{must} be extended.  This function replaces the current matrix with a new matrix with one  column for every column, and a row for every row/index combination.  The column corresponding to the row and index will have the value of the  original matrix in the same row, but index columns previous.  This  shift will introduce NAs where it passes off the end of the matrix.
    #' @param indices A sequence of lags to use as part of the data.  Note that unless this list contains 0, the data will all be shifted back by at least one year.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    #' @param na.rm Whether to remove NA values generated by walking off the edge of the matrix.
    lag = function(indices,mutate=TRUE,na.rm=FALSE){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method scale This method \bold{must} be extended.  This function rescales each element of our object according to f
    #' @param f a function which takes in a number and outputs a rescaled version of that number
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    scale = function(f,mutate=TRUE){
      private$defaultAbstract()
    },
    #' @method diff This method \bold{must} be extended.  This function replaces the matrix value at column i with the difference. between the values at columns i and (i-lag).
    #' @param lag  How far back to diff.  Defaults to 1.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    diff = function(lag=1,mutate=TRUE){
      private$defaultAbstract()
    },
    #' @method addColumns This method \bold{must} be extended.  This function adds empty columns to the right side of the data.
    #' @param columns The number of columns to add.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    addColumns = function(columns,mutate=TRUE){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method addRows This method \bold{must} be extended.  This function adds empty rows to the data.
    #' @param rows The number of rows to add.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    addRows = function(rows,mutate=TRUE){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method mutate This method \bold{must} be extended.  This function is a way to modify the data as though it were a matrix.
    #' @param rows Which rows to modify.  These can be numeric or names.
    #' @param cols Which cols to modify.  These can be numeric or names.
    #' @param data The data to change the chosen values to.  It needs to be the right shape.
    mutate = function(rows,cols,data){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    }
  ),
  active = list(
    #' @field rnames The names of rows in the data.
    rnames = function(value){
      ## for debugging: see AbstractClasses::Generic::debug for details.
      if('rnames' %in% private$.debug){
        browser()
      }
      if(missing(value)){return(private$.rnames)}
      if(length(value) != private$.nrow){
        stop("Row names must be the same length as number of rows")
      }
      rownames(private$.mat) = value
      private$.rnames = value
    },
    #' @field cnames The names of columns in the data.
    cnames = function(value){
      ## for debugging: see AbstractClasses::Generic::debug for details.
      if('cnames' %in% private$.debug){
        browser()
      }
      if(missing(value)){return(private$.cnames)}
      if(length(value) != private$.ncol){
        stop("Column names must be the same length as number of columns")
      }
      colnames(private$.mat) = value
      private$.cnames = value
    }
  )
)

#################################Array Data################################
#' @title ArrayData
#' @description An abstract class for storing data in the form of an \code{n}-
#'   dimensional array.  It enforces core functionality that we want the data we
#'   model to have.  This class covers any data which has two major axis, but
#'   has more minor axis.
#' @docType class
#' @importFrom R6 R6Class
#' @export ArrayData
#' @keywords data core
#' @family MatrixData
#' @family ArrayData
#' @example ArrayData.R

ArrayData <- R6Class(
  classname = "ArrayData",
  inherit = MatrixData,
  private = list(
    ##Variables starting with a '.' represent hidden values corresponding
    ##  to active bindings of the same name without the .
    ##dimData is a list with one element per dimension.  Each element is also a
    ##  list, whose elements have length equal to its length in that dimension.
    .dimData = list(list(),list()),
    ##These track the number of rows and columns of our data.
    .ndim = 0,
    .dims = integer(),
    ##These track the names of those rows and columns if applicable.
    .dnames = NULL,
    .arr = array(as.numeric(NA),c(0,0,1))
  ),
  ##Active bindings are overloads to the <- operator.  If a has a binding b,
  ##  then a$b is a::b() and a$b <- c is a::b(c).  In general, if value is
  ##  missing, return the object, if value is not missing, perform an assignment
  active = list(
    #' @field colData A list of metadata associated with the columns of the data.
    colData = function(value){
      if('colData' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.dimData[[2]])
      }
      if(bindingIsActive('dimData',self)){
    		self$dimData[[2]] <- value
    	} else{
    		warning("The binding has come undone.")
    	}
    },
    #' @field rowData A list of metadata associated with the rows of the data.
    rowData = function(value){
      if('rowData' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.dimData[[1]])
      }
      if(bindingIsActive('dimData',self)){
    		self$dimData[[1]] <- value
    	} else{
    		warning("The binding has come undone.")
    	}
    },
    ##Change this so nrow and ncol cannot be written to directly
    #' @field nrow The number of rows in the data
    nrow = function(value){
      ## for debugging: see AbstractClasses::Generic::debug for details.
      if('nrow' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.dims[1])
      }
      ##The number of rows should be automatically calculated so that it doesn't
      ##differ from the true number
      stop("Do not modify the number of rows directly")
    },
    #' @field ncol The number of columns in the data.
    ncol = function(value){
      ## for debugging: see AbstractClasses::Generic::debug for details.
      if('ncol' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.dims[2])
      }
      ##The number of columns should be automatically calculated so that it
      ##doesn't differ from the true number
      stop("Do not modify the number of columns directly")
    },
    #' @field rnames The names of rows in the data.
    rnames = function(value){
      ## for debugging: see AbstractClasses::Generic::debug for details.
      if('rnames' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        if(is.null(self$dnames) || (length(self$dnames) < 1)){
          return(NULL)
        }
        return(self$dnames[[1]])
      }
      if(bindingIsActive('dnames',self)){
    		self$dnames[[1]] <- value
    	} else{
    		warning("The binding has come undone.")
    	}
    },
    #' @field cnames The names of columns in the data.
    cnames = function(value){
      ## for debugging: see AbstractClasses::Generic::debug for details.
      if(missing(value)){
        if(is.null(self$dnames) || (length(self$dnames) < 2)){
          return(NULL)
        }
        return(self$dnames[[2]])
      }
    	if(bindingIsActive('dnames',self)){
    		self$dnames[[2]] <- value
    	} else{
    		warning("The binding has come undone.")
    	}
    },
    #' @field mat This is the matrix.  For extensibility, it cannot be written to directly and must be modified through methods.
    mat = function(value){
      if(!missing(value)){stop("Do not write directly to the matrix")}
      if('cnames' %in% private$.debug){
        browser()
      }
      return(private$.mat)
    },
    #' @field arr This is the full array.  For extensibility, it cannot be written to directly and must be modified through methods.
    arr = function(value){
      if(!missing(value)){stop("Do not write directly to the array")}
      return(private$.arr)
    },
    #' @field dims The size of the array.
    dims = function(value){
      if(!missing(value)){stop("Do not write directly to the dimensions")}
      return(private$.dims)
    },
    #' @field ndim The number of dimensions of the array.
    ndim = function(value){
      if(!missing(value)){stop("Do not write directly to the number of dimensions.")}
      return(private$.ndim)
    },
    #' @field dnames The size of the array.
    dnames = function(value){
      if('dnames' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.dnames)
      } else if(is.null(value)){
        private$.dnames=NULL
      } else if((class(value) == 'list')){
        nval = length(value)
        if(nval <= self$ndim){
          if(all(mapply(function(self,other){
            ## Check that this works
            (is.null(other)) || (self==length(other))
          },
          self=self$dims[1:nval],
          other=value
          ))){
            private$.dnames[!sapply(value,is.null)] = value[!sapply(value,is.null)]
          } else{
            stop("The dimensions don't match up")
          }
        } else{
          stop("Invalid number of dimensions.")
        }
      } else if((class(value) == 'character') & (length(value) == self$ndim)){
        if(any(self$dims[which(!is.na(value))] != 1)){
          stop("The dimensions don't match up")
        }
        private$.dnames[which(!is.na(value))] = value[which(!is.na(value))]
      } else{
        stop(paste("Not sure how to make dimension metaData from object of class",class(value)))
      }
      if((!is.null(private$.dnames)) && (length(private$.dnames) < self$ndim)){
        private$.dnames[[self$ndim]] = NULL
      }
    },
    #' @field dimData The data associated with each dimension of the array.
    dimData = function(value){
      if('dimData' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.dimData)
      } else if(is.null(value)){
        private$.dimData=NULL
      } else if(class(value) == 'list'){
        nval = length(value)
        if(nval == 0){
          private$.dimData = value
        }else if(nval <= self$ndim){
          if(all(mapply(function(self,other){
            ## Check that this works
            is.null(other) ||
            all(sapply(other, function(x){self==length(x)}))
          },
          self=self$dims[1:nval],
          other=value
        ))){
            private$.dimData[!sapply(value,is.null)] = value[!sapply(value,is.null)]
          } else{
            stop("The dimensions don't match up")
          }
        } else{
          stop("Invalid number of dimensions.")
        }
      } else {
        stop(paste("Not sure how to make dimension metaData from object of class",class(value)))
      }
    }
  )
)


#######################Abstract Simulated Incidence Matrix######################
#' @title AbstractSimulatedIncidenceMatrix
#' @description This class stores a number of simulations each of which contains
#'   the same data as an IncidenceMatrix.
#' @docType class
#' @importFrom R6 R6Class
#' @export AbstractSimulatedIncidenceMatrix
#' @keywords data
#' @family MatrixData
#' @family ArrayData
#' @example AbstractSimulatedIncidenceMatrix.R
AbstractSimulatedIncidenceMatrix <- R6Class(
  classname = "AbstractSimulatedIncidenceMatrix",
  inherit = ArrayData,
  private = list(
    .arr = array(as.numeric(NA),c(0,0,0)),
    .dims = c(0,0,0)
  ),
  public = list(
    #' @method addColumns This method \bold{must} be extended.  This function adds empty columns to the right side of the data.
    #' @param columns The number of columns to add.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    addColumns = function(columns,mutate=TRUE){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method addRows This method \bold{must} be extended.  This function adds empty rows to the right side of the data.
    #' @param rows The number of rows to add.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    addRows = function(rows,mutate=TRUE){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method summarize This method \bold{must} be extended.  Apply a function FUNC to every simulation elementwise.
    #' @param FUNC a function which should be applied to every simulation.  It should reduce each simulation to a single number.
    #' @return A MatrixData where \code{return$mat} is \code{FUNC} applied to every simulation.
    summarize = function(FUNC){
      private$defaultAbstract()
    },
    #' @method addError This method \bold{must} be extended.  Add error of a particular type to the data.
    #' @param type What sort of error to add.
    addError = function(type){
      private$defaultAbstract()
    },
    #' @method diff This method \bold{must} be extended.  This function replaces the matrix value at column i with the difference. between the values at columns i and (i-lag).
    #' @param lag  How far back to diff.  Defaults to 1.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    diff = function(lag=1,mutate=TRUE){
      private$defaultAbstract()
    },
    #' @method lag This method \bold{must} be extended.  This function replaces the current matrix with a new matrix with one  column for every column, and a row for every row/index combination.  The column corresponding to the row and index will have the value of the  original matrix in the same row, but index columns previous.  This  shift will introduce NAs where it passes off the end of the matrix.
    #' @param indices A sequence of lags to use as part of the data.  Note that unless this list contains \\code{0}, the data will all be shifted back by at least one year.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    lag = function(indices,mutate=TRUE){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method mutate This method \bold{must} be extended.  This function is a way to modify the data as though it were a matrix.  \code{self$mutate(row,col,data)} is equivalent to \code{self$mat[row,col] <- data}.
    #' @param rows Which rows to modify.  These can be numeric or names.
    #' @param cols Which cols to modify.  These can be numeric or names.
    #' @param data The data to change the chosen values to.  It needs to be the right shape.
    mutate = function(rows,cols,data){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method scale This method \bold{must} be extended.  This function rescales each element of our object according to f
    #' @param f a function which takes in a number and outputs a rescaled version of that number
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    scale = function(f,mutate=TRUE){
      private$defaultAbstract()
    },
    #' @method subsample This method \bold{must} be extended.  Select only some of the simulations.
    #' @param simulations An index or list of column indices which simulations to keep.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    subsample = function(simulations,mutate=TRUE...){
      private$defaultAbstract()
    },
    #' @method subset This method \bold{must} be extended.  Select the data corresponding to the rows \code{rows} and the columns \code{columns}.  \code{rows} and \code{columns} can be either numeric or named indices.
    #' @param rows An row index or list of row indices which can be either numeric or named.
    #' @param cols An column index or list of column indices which can be either numeric or named.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    subset = function(rows,cols,mutate=TRUE...){
      private$defaultAbstract()
    },
    #' @method tail This method \bold{must} be extended.  Select the last \code{k} slices of the data in dimension \code{direction}.
    #' @param k The number of slices to keep.
    #' @param direction The dimension to take a subset of. 1 for row, 2 for column.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    tail = function(k,direction,mutate=TRUE...){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    },
    #' @method head This method \bold{must} be extended.  Select the first k slices of the data in dimension direction.
    #' @param k The number of slices to keep.
    #' @param direction The dimension to take a subset of. 1 for row, 2 for column.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    head = function(k,direction,mutate=TRUE...){
      ## AbstractClasses.R::Generic::defaultAbstract
      private$defaultAbstract()
    }
  ),
  active = list(
    #' @field sample An IncidenceMatrix sampled from the simulations.
    sample = function(value){
      self$arr[,,ceiling(runif(1,0,self$nsim))]
    },
    #' @field nsim The number of simulations in \code{self$simulaions}
    nsim = function(value){
      if(missing(value)){
        return(private$.dims[[3]])
      }
      stop("Please do not modify the number of simulations directly.  You can use subsample.")
    },
    #' @field simulations  The array of simulations.  This is another name for 'arr'.
    simulations = function(value){
      private$defaultActive('.arr','private',value)
    },
    #' @field dnames The names of dimensions of the data.
    dnames = function(value){
      if('dnames' %in% private$.debug){
        browser()
      }
      #Replace with a super$dnames call later
      if(missing(value)){
        return(private$.dnames)
      } else if(is.null(value)){
        private$.dnames=NULL
      } else if((class(value) == 'list')){
        nval = length(value)
        if(nval <= self$ndim){
          if(all(mapply(function(self,other){
            ## Check that this works
            (is.null(other)) || (self==length(other))
          },
          self=self$dims[1:nval],
          other=value
          ))){
            private$.dnames[!sapply(value,is.null)] = value[!sapply(value,is.null)]
          } else{
            stop("The dimensions don't match up")
          }
        } else{
          stop("Invalid number of dimensions.")
        }
      } else if((class(value) == 'character') & (length(value) == self$ndim)){
        if(any(self$dims[which(!is.na(value))] != 1)){
          stop("The dimensions don't match up")
        }
        private$.dnames[which(!is.na(value))] = value[which(!is.na(value))]
      } else{
        stop(paste("Not sure how to make dimension metaData from object of class",class(value)))
      }
      if((!is.null(private$.dnames)) && (length(private$.dnames) < self$ndim)){
        private$.dnames[[self$ndim]] = NULL
      }
      dimnames(private$.arr) <- private$.dnames
    }
  )
)

############################Abstract Incidence Array############################
#' @title AbstractIncidenceArray
#' @description An abstract class for storing an array.  It has an array of data
#'   arr, which it is responsible for storing.  For arrays with particular
#'   metadata, consider extending this class.  However, if the class is not
#'   truly an array, consider extending FrameData.
#' @docType class
#' @importFrom R6 R6Class
#' @export AbstractIncidenceArray
#' @keywords data
#' @family MatrixData
#' @family ArrayData
#' @family FrameData
#' @example AbstractIncidenceArray.R
AbstractIncidenceArray<- R6Class(
  classname = "AbstractIncidenceArray",
  inherit = ArrayData,
  public = list(
    #' @method addSlices This method \bold{must} be extended.  Extend a dimension by adding extra indices to the end.
    #' @param number How many slices to add.
    #' @param dimension Which dimension should slices be added to.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    addSlices= function(number,dimension=2,mutate=TRUE){
      private$defaultAbstract()
    },
    #' @method apply This method \bold{must} be extended.  Apply a function to each slice.
    #' @param FUNC The function to apply
    #' @param dimension The dimension(s) to hold fixed.
    #' @return An IncidenceArray with dimension equal to \code{self$dims[dimension]}
    apply = function(FUNC,dimension=c(1,2)){
      private$defaultAbstract()
    },
    #' @method subset This method \bold{must} be extended.  Take a subset of the matrix.
    #' @param \dots dimensional indices in order.
    #' @param mutate Whether to modify the existing object, or return a modified copy.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    subset = function(...,mutate=TRUE){
      private$defaultAbstract()
    }
  )
)

#################################Frame Data################################
#' @title FrameData
#' @description An abstract class for storing data in the form of a data frame.
#'   It enforces core functionality that we want the data we  model to have.
#'   This class covers any data which is stored as list.
#' @docType class
#' @importFrom R6 R6Class
#' @export FrameData
#' @keywords data core
#' @family MatrixData
#' @family ArrayData
#' @family FrameData
#' @example FrameData.R
FrameData <- R6Class(
  classname = "FrameData",
  inherit = ArrayData,
  private = list(
    #' @importFrom tibble tibble
    .frame = tibble()
  ),
  active = list(
    #' @field frame The data frame this class is responsible for.
    frame = function(value){
      if(!missing(value)){stop("Do not write directly to the frame.")}
      return(private$.frame)
    }
  )
)

############################Abstract Observation List###########################
#' @title AbstractObservationList
#' @description A class for storing data in multiple interrelated tables.  Each
#'   table relates to the other tables through keys.
#' @docType class
#' @importFrom R6 R6Class
#' @export AbstractObservationList
#' @keywords data
#' @family MatrixData
#' @family ArrayData
#' @family FrameData
#' @example AbstractObservationList.R
AbstractObservationList <- R6Class(
  classname = "AbstractObservationList",
  inherit = FrameData,
  private = list(
    ##Variables starting with a '.' represent hidden values corresponding to
    ##active bindings of the same name without the .
    ##Typing is not as strong as it could be here...
    #' @importFrom tibble tibble
    .frame = tibble(),
    updateArray = function(){
      private$defaultAbstract()
    }
  ),
  public = list(
    #' @method formArray This method \bold{must} be extended.  In order to use an ObservationList as an ArrayData, you need to select which columns to use to form the dimensions of the array.  Optionally, you can also assign some of the columns to be associated with each dimension (or cell).  Note that \code{aggregate} is used to determine how to deal with multiple observations associated with a particular grouping.
    #' @param val The attribute of \code{frame} to use for the values of the array (must \code{aggregate}_ to a numeric type)
    #' @param dimData A list containing for each dimension of the array, the attribute(s) of \code{obs} which are associated with that dimension.
    #' @param metaData The attribute(s) of \code{frame} to store in metaData so they can be accessed by methods expecting a MatrixData object.
    #' @param \dots Column names of columns which, in order should form the dimensions of the array
    formArray = function(val,...,metaData=list(),dimData=list()){
      private$defaultAbstract()
    }
  ),
  active = list(
    #' @field frame Long form data.
    frame = function(value){
      if(!missing(value)){stop("Do not write directly to the observation list")}
      return(private$.frame)
    },
    #' @field aggregate A function used to combine covariates of the same key/val pair.
    aggregate = function(value){
      if(missing(value)){return(private$defaultAbstract)}
    }
  )
)

#################################Relational Data################################
#' @title RelationalData
#' @description An abstract class for storing data in the form of a data frame.
#'   It enforces core functionality that we want the data we  model to have.
#'   This class covers any data which is stored as list.
#' @docType class
#' @importFrom R6 R6Class
#' @export RelationalData
#' @keywords data core
#' @family MatrixData
#' @family ArrayData
#' @family RelationalData
#' @example RelationalData.R
RelationalData <- R6Class(
  classname = "RelationalData",
  inherit = FrameData,
  private = list(
    #' @importFrom tibble tibble 
    .tables = list(tibble())
  ),
  active = list(
    #' @field tables The tables which make up the relational database.
    tables = function(value){
      private$defaultActive('.tables','private',value)
    }
  )
)

############################Abstract Relational Table###########################
#' @title AbstractRelationalTables
#' @description A class for storing data in multiple interrelated tables.  Each
#'   table relates to the other tables through keys.
#' @docType class
#' @importFrom R6 R6Class
#' @export AbstractRelationalTables
#' @keywords data
#' @family MatrixData
#' @family ArrayData
#' @family FrameData
#' @family RelationalData
#' @example RelationalData.R
AbstractRelationalTables <- R6Class(
  classname = "AbstractRelationalTables",
  inherit = RelationalData,
  private = list(
    ##Variables starting with a '.' represent hidden values corresponding to active bindings of the same name without the .
    #' @importFrom tibble tibble 
    .tables = list(tibble()),
    ##.keys will store the column names for the identifying columns for the
    ##  tables.
    .keys = list(character()),
    updateFrame = function(){
      private$defaultAbstract()
    }
  ),
  active = list(
    #' @field tables A list of tables of data
    tables = function(value){
      if(!missing(value)){stop("Do not write directly to the list of tables")}
      return(private$.tables)
    },
    #' @field keys A list containing the primary keys for each table
    keys = function(value){
      if(!missing(value)){stop("Do not write directly to the list of keys")}
      return(private$.key)
    }
  )
)
