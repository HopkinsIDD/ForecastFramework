SimulatedIncidenceMatrix <- R6Class(
  classname = "SimulatedIncidenceMatrix",
  inherit = AbstractSimulatedIncidenceMatrix,
  private = list(
    #.arr = array(NA,c(0,0,0)),
    .ndim = 3,
    ncore = 1,
    .sample = 1,
    parallelEnvironment = NULL
  ),
  public = list(
    initialize = function(data=MatrixData$new(),nsim=1){
      ##if(nsim > 1){
      ##  private$ncore = detectCores()
      ##  private$parallelEnvironment = makeCluster(private$ncore)
      ##}
      if('AbstractSimulatedIncidenceMatrix' %in% class(data)){
        private$.arr= data$simulations
        #private$.arr= array(data$arr,c(data$nrow,data$ncol,nsim))
        private$.metaData = data$metaData
        private$.dimData = data$dimData
        private$.dnames = data$dnames
        private$.dims = data$dims
        private$.ndim = length(self$dims)
        return()
      }
      else if('list' %in% class(data)){
        ##consider checking for every element
        if('MatrixData' %in% class(data[[1]])){
          if((!missing(nsim)) && (nsim != length(data))){
            ##stop or warn here?
            stop("nsim is not used for list data.")
          }
          private$.dims = c(data[[1]]$nrow,data[[1]]$ncol,length(data))
          private$.arr = array(NA,c(self$nrow,self$ncol,self$nsim))
          if(self$nsim > 0){
            for(i in 1: self$nsim){
              private$.arr[,,i] = data[[i]]$mat
            }
          }
          ##For extracting the metadata
          data = data[[1]]
          private$.metaData = data$metaData
          private$.dimData = list(data$rowData,data$colData)
          if(!is.null(data$rnames)){
            if(!is.null(data$cnames)){
              private$.dnames = list(data$rnames,data$cnames,NULL)
            } else{
              private$.dnames = list(data$rnames,NULL,NULL)
            }
          } else if(!is.null(data$cnames)){
            private$.dnames = list(NULL,data$cnames,NULL)
          } else {
            private$.dnames = NULL
          }
          dimnames(private$.arr) = private$.dnames
          private$.dims = c(data$nrow,data$ncol,nsim)
          private$.ndim = 3
          return()
        }
        else{
          ##Maybe not ever implemented
          stop("Not yet implemented")
        }
      } else if('MatrixData' %in% class(data)){
        private$.arr= array(data$mat,c(data$nrow,data$ncol,nsim))
        private$.metaData = data$metaData
        private$.dimData = list(data$rowData,data$colData)
        if(!is.null(data$rnames)){
          if(!is.null(data$cnames)){
            private$.dnames = list(data$rnames,data$cnames,NULL)
          } else{
            private$.dnames = list(data$rnames,NULL,NULL)
          }
        } else if(!is.null(data$cnames)){
          private$.dnames = list(NULL,data$cnames,NULL)
        } else {
          private$.dnames = NULL
        }
        dimnames(private$.arr) = private$.dnames
        private$.dims = c(data$nrow,data$ncol,nsim)
        private$.ndim = 3
        return()
      }
      else{
        stop("Input data is not a valid type to make a SimulatedIncidenceMatrix")
      }
      stop("This is currently broken.")
      rownames(private$.arr) <- rownames(data[[1]]$mat)
      colnames(private$.arr) <- colnames(data[[1]]$mat)
      private$.dimData = list(data$rowData,data$colData,NULL)
      private$.metaData = data$metaData
      if(!is.null(data$rnames)){
        if(!is.null(data$cnames)){
          private$.dnames = list(data$rnames,data$cnames,NULL)
        } else{
          private$.dnames = list(data$rnames,NULL,NULL)
        }
      } else if(!is.null(data$cnames)){
        private$.dnames = list(NULL,data$cnames,NULL)
      } else {
        private$.dnames = NULL
      }
      private$.dims = c(data$nrow,data$ncol,nsim)
    },
    #finalize = function(){
    #  #' This function should clean up the parallel environment.
    #  ## #' @importFrom parallel makeCluster
    #  ## #' @importFrom parallel detectCores
    #  ## #' @importFrom parallel stopCluster
    #  ##stopCluster(private$parallelEnvironment)
    #},
    #' @method mean Compute the mean over all simulations.
    mean = function(){
      "Compute the mean across simulations"
      if('mean' %in% private$.debug){
        browser()
      }
      return(IncidenceMatrix$new(apply(self$simulations,c(1,2),mean)))
    },
    #' @method median Compute the median over all simulations.
    median = function(){
      if('median' %in% private$.debug){
        browser()
      }
      return(IncidenceMatrix$new(apply(self$arr,c(1,2),median)))
    },
    addError = function(type,rows,cols,mutate = TRUE){
      if('addError' %in% private$.debug){
        browser()
      }
      if(missing(rows)){
        rows = 1:self$nrow
      }
      if(missing(cols)){
        cols = 1:self$ncol
      }
      if(type=='Poisson'){
        private$.arr[rows,cols,] =
          rpois(length(rows)*length(cols)*self$nsim,self$arr[rows,cols,])
      } else{
        stop("Not yet implemented")
      }
    },
    subsample = function(simulations,mutate=TRUE){
      ##for debugging: see AbstractClasses::Generic::debug for details.
      if('subsample' %in% private$.debug){
        browser()
      }
      if(!mutate){
        rc = self$clone(TRUE)
        rc$subsample(simulations,mutate=TRUE)
        return(rc)
      }
      ##From now on, we're assuming that we will mutate self$.arr.
      if(
      (min(simulations) < 0) ||
        (max(simulations) > self$nsim) ||
        any(round(simulations) != simulations)
      ){
        stop("simulations out of bounds.")
      }
      private$.dims[3] = length(simulations)
      private$.arr = self$arr[,,simulations]
    },
    subset = function(rows,cols,mutate=TRUE){
      ##for debugging: see AbstractClasses::Generic::debug for details.
      if('subset' %in% private$.debug){
        browser()
      }
      if(!mutate){
        rc = self$clone(TRUE)
        rc$subset(rows,cols,mutate=TRUE)
        return(rc)
      }
      ##From now on, we're assuming that we will mutate self$.arr.

      ##Dealing with missing row/col specifications
      if(missing(rows) && missing(cols)){
        return()
      }
      else if(missing(rows)){
        ##Keep all rows and simulations, select a subset of columns.
        rows = 1:self$nrow
      }
      else if(missing(cols)){
        ##Keep all columns and simulations, select a subset of rows.
        cols = 1:self$ncol
      }
      ##We keep all the simulations, and select a subset of rows and columns
      ##drop=FALSE keeps the dimension from automatically reducing
      private$.arr = self$arr[rows,cols,,drop=FALSE]
      ##Clean up dimensions which have changed
      private$.dims = c(nrow(self$arr),ncol(self$arr),self$nsim)
      private$.dnames = dimnames(self$arr)
      ##We need to clean up row and column metadata.
      ##Row metadata is a list of lists so iterate over it
      if(length(self$rowData)>0){
        if(length(self$colData) > 0){
          self$dimData = list(
            lapply(self$rowData,function(x){x[rows,drop=FALSE]}),
            lapply(self$colData,function(x){x[cols,drop=FALSE]})
          )
        } else {
          self$rowData <- lapply(self$rowData,function(x){x[rows,drop=FALSE]})
        }
      } else if(length(self$colData)>0){
        self$colData <- lapply(self$colData,function(x){x[cols,drop=FALSE]})
      }
      ##Note, we don't return anything, because this modifies the object.
    },
    head = function(k,direction=2,mutate=FALSE){
      ##TODO: Consider only taking the time head...

      ##for debugging: see AbstractClasses::Generic::debug for deheads.
      if('head' %in% private$.debug){
        browser()
      }
      ##Again, there are problems with a:b when b<a
      if(k>dim(self$arr)[[direction]]){
        stop("The size of the head is too large.")
      }
      ##These are the indices of the appropriate slices
      indices = 1:k

      ##The indexing code changes based on direction
      if(direction==1){
        ##rows
        ##This is the part where we actually do the head
        private$.arr = self$arr[indices,,,drop=FALSE]
        ##We also deal with the metadata
        if(length(self$rowData)>0){
          private$.dimData[[1]] =
            lapply(self$rowData,function(x){x[indices,drop=FALSE]})
          #for(i in 1:length(self$rowData)){
          #  self$rowData[[i]] = self$rowData[[i]][indices,drop=FALSE]
          #}
        }
      }
      else if(direction==2){
        ##columns
        ##This is the part where we actually do the head
        private$.arr = self$arr[,indices,,drop=FALSE]
        ##We also deal with the metadata
        if(length(self$colData)>0){
          private$.dimData[[2]] =
            lapply(self$colData,function(x){x[indices,drop=FALSE]})
          #for(i in 1:length(self$colData)){
          #  self$colData[[i]] = self$colData[[i]][indices,drop=FALSE]
          #}
        }
      }
      else{
        stop("This direction is not allowed.")
      }
      private$.dims = dim(self$arr)
      private$.dnames = dimnames(self$arr)
    },
    tail = function(k,direction=2){
      ##TODO: Consider only taking the time tail...

      ##for debugging: see AbstractClasses::Generic::debug for details.
      if('tail' %in% private$.debug){
        browser()
      }
      ##Again, there are problems with a:b when b<a
      if(k>dim(self$arr)[[direction]]){
        stop("The size of the tail is too large.")
      }
      ##These are the indices of the appropriate slices
      indices = (dim(self$arr)[[direction]]-k+1):dim(self$arr)[[direction]]

      ##The indexing code changes based on direction
      if(direction==1){
        ##rows
        ##This is the part where we actually do the tail
        private$.arr = self$arr[indices,,,drop=FALSE]
        private$.dims = dim(self$arr)
        ##We also deal with the metadata
        if(length(self$rowData)>0){
          self$rowData = lapply(self$rowData,function(x){x[indices,drop=FALSE]})
          #for(i in 1:length(self$rowData)){
          #  self$rowData[[i]] = self$rowData[[i]][indices,drop=FALSE]
          #}
        }
      }
      else if(direction==2){
        ##columns
        ##This is the part where we actually do the tail
        private$.arr = self$arr[,indices,,drop=FALSE]
        private$.dims = dim(self$arr)
        ##We also deal with the metadata
        if(length(self$colData)>0){
          self$colData = lapply(self$colData,function(x){x[indices,drop=FALSE]})
          #for(i in 1:length(self$colData)){
          #  self$colData[[i]] = self$colData[[i]][indices,drop=FALSE]
          #}
        }
      }
      else{
        stop("This direction is not allowed.")
      }
      private$.dims = dim(self$arr)
      private$.ndim = length(self$dims)
      private$.dnames = dimnames(self$arr)
    },
    lag = function(indices,mutate = TRUE,na.rm=FALSE){
      ##for debugging: see AbstractClasses::Generic::debug for details.
      if('lag' %in% private$.debug){
        browser()
      }
      if(mutate==FALSE){
        tmp = self$clone(TRUE)
        tmp$lag(indices=indices,mutate=TRUE)
        return(tmp)
      }
      ##Change this into a by
      if((1+max(indices)) > self$ncol){
        stop("We cannot go further back than the start of the matrix")
      }
      ##Store some information that will get modified later.
      numLags = length(indices)
      ##This populates the rownames with something sensible if they are empty.
      ##Consider changing this to using rownames(force=TRUE)
      if(is.null(rownames(self$arr))){
        rownames(private$.arr) = 1:(dim(self$arr)[[1]])
      }
      ##replicate has some automatic simplification behaviour which is hard to deal with
      rownames = replicate(numLags,rownames(self$arr))
      colnames = colnames(self$arr)
      ##Replicate the matrix once for each lag.
      private$.arr <- array(self$arr,c(dim(self$arr),numLags))
      if(numLags <= 0){
        stop("indices must be nonempty for the calculation of lags to make sense.")
      }
      ##For each lag, we need to actually shift the values back in time, and add NA where appropriate
      for(lag in 1:numLags){
        private$.arr[,(1+indices[[lag]]):self$ncol,,lag] <-
          self$arr[,1:(self$ncol-indices[[lag]]),,lag]
        if(indices[[lag]] > 0){
          ##Check and make sure this works
          private$.arr[,1:(indices[[lag]]),,lag] = NA
        }
      }

      ##We reshuffle the dimensions and collapse the 1st and 4th dimension
      private$.arr = aperm(self$arr,c(1,4,2,3))
      ##Check to make sure this works
      private$.arr = array(self$arr,c(self$nrow*numLags,self$ncol,self$nsim))

      lagnames = t(replicate(self$nrow,paste('L',indices,sep='')))

      ##We populate the column names (with renaming to account for lagged columns)
      rownames(private$.arr) <-
        as.character(
          array(paste(lagnames,"R",rownames,sep=''),numLags*self$nrow)
        )
      colnames(private$.arr) <- colnames

      private$.dims[[1]] = self$nrow * numLags
      if(!is.null(dimnames(self$arr))){
        private$.dnames = dimnames(self$arr)
      }
      if(length(self$rowData) > 0){
        self$rowData <- lapply(
          self$rowData,
          function(x){
            c(unlist(recursive=FALSE,lapply(1:numLags,function(y){x})))
          }
        )
      }
      if(na.rm==TRUE){
        self$subset(cols=!apply(self$arr,2,function(x){any(is.na(x))}))
      }
    },
    #' @method addRows This function adds rows to \code{self$simulations}
    #' @param rows The number of rows to add.
    addRows = function(rows){
      ##For debuggning
      if('addRows' %in% private$.debug){
        browser()
      }
      if(rows == 0){
        return()
      }
      #' @importFrom abind abind
      abind(self$arr,array(NA,c(rows,self$ncol,self$nsim)),along=1) ->
        private$.arr
      private$.dims[[1]] = nrow(self$arr)
      private$.dnames = dimnames(self$arr)
      if(length(self$rowData) > 0){
        self$rowData = lapply(self$rowData,function(x){c(x,replicate(rows,NA))})
      }
    },
    #' @method addColumns This function adds columns to \code{self$simulations}
    #' @method cols The number of columns to add.
    addColumns = function(columns){
      "This function adds columns to the data."
      "@param columns The number of columns to add."
      ##For debuggning
      if('addColumns' %in% private$.debug){
        browser()
      }

      if(columns == 0){
        return()
      }
      #' @importFrom abind abind
      abind(private$.arr,array(NA,c(self$nrow,columns,self$nsim)),along=2) ->
        private$.arr
      private$.dims[2]= ncol(self$arr)
      private$.dnames = dimnames(private$.arr)
      if(length(self$colData) > 0){
        self$colData = lapply(self$colData,function(x){c(x,replicate(columns,NA))})
      }
    },
    scale = function(f,mutate=TRUE){
      if('scale' %in% private$.debug){
        browser()
      }
      if(!mutate){
        tmp = self$clone(TRUE)
        tmp$scale(f=f,mutate=TRUE)
        return(tmp)
      }
      private$.arr[] = f(private$.arr[])
    },
    diff = function(lag = 1,mutate=TRUE){
      if('diff' %in% private$.debug){
        browser()
      }
      if(!mutate){
        tmp = self$clone(TRUE)
        tmp$diff(lag=lag,mutate=TRUE)
        return(tmp)
      }
      if(lag == 0){
        if(!is.null(rownames(private$.arr))){
          rownames(private$.arr) = paste("D",lag,"R",rownames(private$.arr),sep='')
        }
        private$.dnames = dimnames(private$.arr)
        return()
      }
      if(lag < 0){
        stop("lag must be non-negative.")
      }
      rn = rownames(private$.arr)
      private$.arr <-
        self$simulations- self$lag(indices=lag,mutate=FALSE)$simulations
      if(!is.null(rn)){
        rownames(private$.arr) = paste("D",lag,"R",rownames(private$.arr),sep='')
      }
      private$.dnames = dimnames(private$.arr)
    },
    mutate = function(rows,cols,data){
      ##For debugging
      if('mutate' %in% private$.debug){
        browser()
      }
      ##Testing this:
      tmpdata = data
      tmpdata = array(data,self$dims)
      data = as.array(data)

      if(missing(rows)){
        rows = 1:self$nrow
        if(!(is.null(self$cnames) || is.null(colnames(data)))){
          private$.dnames[[2]][cols] = colnames(data)
          #self$cnames[cols] = colnames(data)
          colnames(private$.arr) = self$cnames
        }
      }
      if(missing(cols)){
        cols = 1:self$ncol
        if(!(is.null(self$rnames) || is.null(rownames(data)))){
          private$.dnames[[1]][rows] = rownames(data)
          #self$rnames[rows] = rownames(data)
          rownames(private$.arr) = self$rnames
        }
      }
      ##Check the size of the data
      ##Make this work better...
      if(is.null(dim(data))){
        stop("Not yet implemented for non-matrixlike objects")
      }
      ##if(nrow(data) != length(rows)){
      ##  stop("The number of rows do not match")
      ##}
      ##if(ncol(data) != length(cols)){
      ##  stop("The number of cols do not match")
      ##}
      if(length(dim(data)) > 3){
        stop("There are too many dimensions in data.")
      }
      if(length(dim(data)) == 3){
        ##The data is an array
        if(dim(data)[[3]] == self$nsim){
          private$.arr[rows,cols,] = data
        } else if(dim(data)[[3]] == 1){
          private$.arr[rows,cols,] = replicate(self$nsim,data)
        }
      }
      else{
        private$.arr[rows,cols,] = replicate(self$nsim,data)
      }
    },
    #' @method summarize Apply a function to every simulation.
    #' @param FUNC The function to apply.
    #' @param \dots Any arguments to \code{FUNC} other than the matrix.
    summarize = function(FUNC,...){
      if('apply' %in% private$.debug){
        browser()
      }
      return(IncidenceMatrix$new(
        data=apply(private$.arr,c(1,2),FUNC),
        rowData=self$rowData,
        colData=self$colData,
        metaData=self$metaData)
      )
    }
  ),
  active = list(
    sample = function(value){
      "Randomly extract a simulation"
      if("sample" %in% private$.debug){
        browser()
      }
      return = FALSE
      if(missing(value)){
        if(self$nsim < 1){
          return(private$.arr)
        }
        value = sample(self$nsim,1)
        return = TRUE
      }
      if(floor(value) != value){
        stop("sample must be an integer.")
      }

      private$.sample = value

      ##This might not be the best idea...
      if(return){
        return(IncidenceMatrix$new(self))
      }
    },
    mat = function(value){
      #' @importFrom abind adrop
      private$.mat = adrop(private$.arr[,,private$.sample,drop=FALSE],3)
      rownames(private$.mat) = rownames(private$.arr)
      colnames(private$.mat) = colnames(private$.arr)
      return(private$.mat)
    },
    simulations = function(value){
      if(missing(value)){
        return(private$.arr)
      }
      stop("Do not write directly to the simulations")
    }
  )
)
