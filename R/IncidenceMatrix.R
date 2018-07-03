################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################
#' @include DataContainers.R
#######################################TODO#####################################

## Write test functions for initialize

################################################################################
####Incidence Matrix
################################################################################
#' @title IncidenceMatrix
#' @description A class for storing a matrix and relevant metaData.
#' @docType class
#' @importFrom R6 R6Class
#' @export IncidenceMatrix
#' @keywords data
#' @family Matrices
#' @example IncidenceMatrix.R
IncidenceMatrix <- R6Class(
  classname = "IncidenceMatrix",
  inherit = AbstractIncidenceMatrix,
  public = list(
    ##Unsure how to document initialize:
    #' @method initialize Create a new \code{IncidenceMatrix} object from its components.
    #' @param data Either a matrix, or an object of class MatrixData.  If data is a matrix, it will form the mat field.  If data is a MatrixData, then the new object will inherit all of its fields.
    #' @param metaData A list of metadata information, which the object will store
    #' @param rowData A list of metadata information associated with the rows of the matrix.
    #' @param colData A list of metadata information associated with the columns of the matrix.
    ## Note when its decided exactly what the cellData are, they should be updated in the documentation.
    #' @param cellData A list of metadata information associated with the elements of the matrix.
    initialize = function(data=matrix(),metaData=list(),rowData=list(),colData=list(),cellData = list()){
      ##We take in two types of input data:
      ##matrix Data
      ##MatrixData object Data
      if(Reduce('&&',c('MatrixData','DataContainer','Generic','R6') %in% class(data))){
        ##self$mat <- data$clone(TRUE)$mat
        private$.mat <- data$mat
        private$.metaData <- data$metaData
        private$.nrow <- data$nrow
        private$.ncol <- data$ncol
        private$.rnames <- data$rnames
        private$.cnames <- data$cnames
        private$.rowData <- data$rowData
        private$.colData <- data$colData
        private$.metaData <- data$metaData
        private$.cellData <- data$cellData
      }
      else{
        ##We could encapsulate this into a function, however I think we don't want to enforce subclasses of this class to have a setMatrix method...
        ##We perform a few checks to make sure anything which makes it in
        ##First we attempt to extract row and column names
        rtoggle = FALSE
        ctoggle = FALSE
        try({
          rnames <- dimnames(data)[[1]]
          rtoggle = TRUE
          ##Note: We should set these to NULL if this try block fails...
          ##TODO: Catch error
        })
        try({
          cnames <- dimnames(data)[[2]]
          ctoggle = TRUE
        })
        ##First, we check the type to see if it is compatible with our current data
        if(!private$checkType(name='.mat',val=data,type='private')){
          ##If its not, we try a typecast
          data <- as.matrix(data)
          if(rtoggle){
            rownames(data) = rnames
          }
          if(ctoggle){
            colnames(data) = cnames
          }
        }
        if(!private$checkType(name='.mat',val=data,type='private')){
          ##If its still no good after the typecast, then we abort
          stop(paste(
            "invalid data of type",
            paste(class(data),collapse=','),
            "expected",
            paste(class(private$.mat),collapse = ',')
          ))
        }
        ##If its too big, there's nothing we can really do, so we abort
        ##This is probably not necessary
        if(length(dim(data)) > 2){
          stop("The matrix is not intended to hold things with more than 3 dimensions.")
        }
        ##Assuming everything works out, we set the size of the matrix and the matrix itself
        ndim = dim(data)
        private$.nrow = ndim[[1]]
        private$.ncol = ndim[[2]]
        ##We also maintain the row and column names
        ##rownames(data) <- rnames
        ##colnames(data) <- cnames
        private$.rnames = rownames(data)
        private$.cnames = colnames(data)
        private$.mat <- 0+data
        ##With the base matrix done, we set the metadata.
        ##For these, we can use the active bindings.
        ##The active bindings will do most of the work here.
        self$rowData <- rowData
        self$cellData <- cellData
        self$colData <- colData
        self$metaData <- metaData
      }
    },
    #' @method subset Select the data corresponding to the rows \code{rows} and the columns \code{columns}.  \code{rows} and \code{columns} can be either numeric or named indices.
    #' @param rows An row index or list of row indices which can be either numeric or named.
    #' @param cols An column index or list of column indices which can be either numeric or named.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    subset = function(rows,cols,mutate=TRUE){
      ##for debugging: see AbstractClasses::Generic::debug for details.
      if('subset' %in% private$.debug){
        browser()
      }
      ##If we are not to modify the current object, clone it and then run the mutating one on the clone.
      if(!mutate){
        temp = self$clone(TRUE)
        temp$subset(rows,cols,mutate=TRUE)
        return(temp)
      }
      ##From now on, we're assuming that we will mutate private$.mat.
      ##Dealing with missing row/col specifications
      if(missing(rows) && missing(cols)){
      }
      else if(missing(rows)){
        ##Keep all rows, select a subset of columns.
        ##drop=FALSE keeps the dimension from automatically reducing.
        private$.mat = self$mat[,cols,drop=FALSE]
        ##We need to clean up the column metadata since we changed the number of columns
        ##Column metadata is a list of lists, so we iterate over it.
        ##This if statement is necessary for dealing with the fact that 1:0 in R is non-empty.
        if(length(private$.colData) > 0){
          ##Without the previous if statement, this loop would be nonempty when length(private$.colData) == 0, which is bad
          lapply(private$.colData,function(x){x[cols,drop=FALSE]}) -> private$.colData
        }
        if(length(private$.cellData) > 0){
          lapply(private$.cellData,function(x){x[,cols,drop=FALSE]}) -> private$.cellData
        }
      }
      else if(missing(cols)){
        ##Keep all columns, select a subset of rows.
        ##drop=FALSE keeps the dimension from automatically reducing.
        private$.mat = self$mat[rows,,drop=FALSE]
        ##We need to clean up the row metadata since we changed the number of rows
        ##Row metadata is a list of lists, so we iterate over it.
        ##This if statement is necessary for dealing with the fact that 1:0 in R is non-empty.
        if(length(private$.rowData) > 0){
          lapply(private$.rowData,function(x){x[rows,drop=FALSE]}) -> private$.rowData
        }
        if(length(private$.cellData) > 0){
          lapply(private$.cellData,function(x){x[rows,,drop=FALSE]}) -> private$.cellData
        }
      }
      else{
        ##We select a subset of rows and columns
        ##drop=FALSE keeps the dimension from automatically reducing
        private$.mat = self$mat[rows,cols,drop=FALSE]
        ##We need to clean up row and column metadata.
        ##Row metadata is a list of lists so iterate over it
        if(length(private$.rowData)>0){
          lapply(private$.rowData,function(x){x[rows,drop=FALSE]}) -> private$.rowData
        }
        if(length(private$.colData)>0){
          lapply(private$.colData,function(x){x[cols,drop=FALSE]}) -> private$.colData
          ##for(i in 1:length(private$.colData)){
          ##	#Does this work? The drop seems like it might cause problems.
          ##	#TODO: Write a test case for it
          ##	#Subset ith element of the column metadata.
          ##	private$.colData[[i]] = private$.colData[[i]][cols,drop=FALSE]
          ##}
        }
        if(length(private$.cellData) > 0){
          lapply(private$.cellData,function(x){x[rows,cols,drop=FALSE]}) -> private$.cellData
        }
      }
      private$.nrow = nrow(private$.mat)
      private$.rnames = rownames(private$.mat)
      private$.ncol = ncol(private$.mat)
      private$.cnames = colnames(private$.mat)
      ##Note, we don't return anything, because this modifies the object.
    },
    #' @method head Select the last \code{k} slices of the data in dimension \code{direction}.
    #' @param k The number of slices to keep.
    #' @param direction The dimension to take a subset of. 1 for row, 2 for column.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    head = function(k,direction=2,mutate=TRUE){
      ##TODO: Consider only taking the time head...
      ##for debugging: see AbstractClasses::Generic::debug for deheads.
      if('head' %in% private$.debug){
        browser()
      }
      ##Again, there are problems with a:b when b<a
      if(k>dim(private$.mat)[[direction]]){
        stop("The size of the head is too large.")
      }
      ##These are the indices of the appropriate slices
      indices = 1:k
      ##The indexing code changes based on direction
      if(direction==1){
        return(self$subset(rows=indices,mutate=mutate))
      } else if(direction==2){
        return(self$subset(cols=indices,mutate=mutate))
      }
      else{
        stop("This direction is not allowed.")
      }
    },
    #' @method tail Select the last \code{k} slices of the data in dimension \code{direction}.
    #' @param k The number of slices to keep.
    #' @param direction The dimension to take a subset of. 1 for row, 2 for column.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    tail = function(k,direction=2,mutate=TRUE){
      ##TODO: Consider only taking the time tail...
      ##for debugging: see AbstractClasses::Generic::debug for details.
      if('tail' %in% private$.debug){
        browser()
      }
      ##Again, there are problems with a:b when b<a
      if(k>dim(private$.mat)[[direction]]){
        stop("The size of the tail is too large.")
      }
      ##These are the indices of the appropriate slices
      indices = (dim(self$mat)[[direction]]-k+1):dim(self$mat)[[direction]]
      ##The indexing code changes based on direction
      if(direction==1){
        return(self$subset(rows = indices,mutate = mutate))
      } else if(direction==2){
        return(self$subset(cols = indices,mutate = mutate))
      } else{
        stop("This direction is not allowed.")
      }
    },
    #' @method lag This function replaces the current matrix with a new matrix with one  column for every column, and a row for every row/index combination.  The column corresponding to the row and index will have the value of the  original matrix in the same row, but index columns previous.  This  shift will introduce NAs where it passes off the end of the matrix.
    #' @param indices A sequence of lags to use as part of the data.  Note that unless this list contains 0, the data will all be shifted back by at least one year.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    #' @param na.rm Whether to remove NA values generated by walking off the edge of the matrix.
    lag = function(indices,mutate=TRUE,na.rm=FALSE){
      ##Consider adding na.rm to this method.
      ##for debugging: see AbstractClasses::Generic::debug for details.
      if('lag' %in% private$.debug){
        browser()
      }
      if(mutate==FALSE){
        tmp = self$clone(TRUE)
        tmp$lag(indices=indices,mutate=TRUE,na.rm=na.rm)
        return(tmp)
      }
      ##Change this into a by
      if((1+max(indices)) > self$ncol){
        stop("We cannot go further back than the start of the matrix")
      }
      ##Store some information that will get modified later.
      numLags = length(indices)
      oldNrow = self$nrow
      ##This populates the rownames with something sensible if they are empty.
      ##Consider changing this to using rownames(force=TRUE)
      if(is.null(rownames(private$.mat))){
        rownames(private$.mat) = 1:(dim(private$.mat)[[1]])
      }
      ##replicate has some automatic simplification behaviour which is hard to deal with
      rownames = replicate(numLags,rownames(private$.mat))
      colnames = colnames(private$.mat)
      ##Replicate the matrix once for each lag.
      private$.mat <- 0+array(self$mat,c(dim(self$mat),numLags))
      if(numLags <= 0){
        stop("indices must be nonempty for the calculation of lags to make sense.")
      }
      ##For each lag, we need to actually shift the values back in time, and add NA where appropriate
      for(lag in 1:numLags){
        private$.mat[,(1+indices[[lag]]):self$ncol,lag] = private$.mat[,1:(self$ncol-indices[[lag]]),lag]
        if(indices[[lag]] > 0){
          ##Check and make sure this works
          private$.mat[,1:(indices[[lag]]),lag] = NA
        }
      }
      ##We reshuffle the dimensions and collapse the 1st and 4th dimension
      private$.mat = aperm(private$.mat,c(1,3,2))
      ##Check to make sure this works
      private$.mat = matrix(private$.mat,self$nrow*numLags,self$ncol)
      lagnames = t(replicate(self$nrow,paste('L',indices,sep='')))
      ##We populate the column names (with renaming to account for lagged columns)
      rownames(private$.mat) <- as.character(paste(lagnames,"R",rownames,sep=''),numLags*self$nrow)
      colnames(private$.mat) <- colnames
      private$.nrow = self$nrow * numLags
      ##private$.rnames = array(paste(lagnames,"R",rownames,sep=''),numLags*self$nrow)
      private$.rnames = rownames(private$.mat)
      if(length(private$.rowData) > 0){
        private$.rowData <- lapply(
          private$.rowData,
          function(x){
            c(unlist(recursive=FALSE,lapply(1:numLags,function(y){x})))
          }
        )
      }
      if(length(private$.cellData) > 0){
        private$.cellData <- lapply(
          private$.cellData,
          function(x){
            x <- 0+array(x,c(dim(self$mat),numLags))
            ##For each lag, we need to actually shift the values back in time, and add NA where appropriate
            for(lag in 1:numLags){
              x[,(1+indices[[lag]]):self$ncol,lag] = x[,1:(self$ncol-indices[[lag]]),lag]
              if(indices[[lag]] > 0){
                x[,1:(indices[[lag]]),lag] = NA
              }
            }
            ##We reshuffle the dimensions and collapse the 1st and 4th dimension
            x = aperm(x,c(1,3,2))
            ##Check to make sure this works
            x = matrix(x,self$nrow*numLags,self$ncol)
            return(x)
          }
        )
      }
      if(na.rm==T){
        self$subset(cols=!apply(private$.mat,2,function(x){any(is.na(x))}))
      }
    },
    #' @method lead This function replaces the current matrix with a new matrix with one column for every column, and a row for every row/index combination.  The column corresponding to the row and index will have the value of the original matrix in the same row, but index columns ahead.
    #' @param indices A sequence of leads to use as part of the data.  Note that unless this list contains \\code{0}, the data will all be shifted foreward by one year.
    #' @param mutate Whether to modify this object, or create and return a modified object.
    #' @param na.rm Whether to remove the NA columns that result where the lead goes off the edge of self$simulations.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    lead = function(indices,mutate=TRUE,na.rm=FALSE){
      ##Consider adding na.rm to this method.
      ##for debugging: see AbstractClasses::Generic::debug for details.
      if('lead' %in% private$.debug){
        browser()
      }
      if(mutate==FALSE){
        tmp = self$clone(TRUE)
        tmp$lead(indices=indices,mutate=TRUE,na.rm=na.rm)
        return(tmp)
      }
      ##Change this into a by
      if((1+max(indices)) > self$ncol){
        stop("We cannot go further back than the start of the matrix")
      }
      ##Store some information that will get modified later.
      numLeads = length(indices)
      oldNrow = self$nrow
      ##This populates the rownames with something sensible if they are empty.
      ##Consider changing this to using rownames(force=TRUE)
      if(is.null(rownames(private$.mat))){
        rownames(private$.mat) = 1:(dim(private$.mat)[[1]])
      }
      ##replicate has some automatic simplification behaviour which is hard to deal with
      rownames = replicate(numLeads,rownames(private$.mat))
      colnames = colnames(private$.mat)
      ##Replicate the matrix once for each lead.
      private$.mat <- 0+array(self$mat,c(dim(self$mat),numLeads))
      if(numLeads <= 0){
        stop("indices must be nonempty for the calculation of leads to make sense.")
      }
      ##For each lead, we need to actually shift the values back in time, and add NA where appropriate
      for(lead in 1:numLeads){
        #private$.mat[,(1+indices[[lead]]):self$ncol,lead] = private$.mat[,1:(self$ncol-indices[[lead]]),lead]
        private$.mat[,1:(self$ncol-indices[[lead]]),lead] = private$.mat[,(1+indices[[lead]]):self$ncol,lead]
        if(indices[[lead]] > 0){
          ##Check and make sure this works
          private$.mat[,(self$ncol+1-indices[[lead]]):self$ncol,lead] = NA
        }
      }
      ##We reshuffle the dimensions and collapse the 1st and 4th dimension
      private$.mat = aperm(private$.mat,c(1,3,2))
      ##Check to make sure this works
      private$.mat = matrix(private$.mat,self$nrow*numLeads,self$ncol)
      leadnames = t(replicate(self$nrow,paste('L',indices,sep='')))
      ##We populate the column names (with renaming to account for leadged columns)
      rownames(private$.mat) <- as.character(paste(leadnames,"R",rownames,sep=''),numLeads*self$nrow)
      colnames(private$.mat) <- colnames
      private$.nrow = self$nrow * numLeads
      ##private$.rnames = array(paste(leadnames,"R",rownames,sep=''),numLeads*self$nrow)
      private$.rnames = rownames(private$.mat)
      if(length(private$.rowData) > 0){
        private$.rowData <- lapply(
          private$.rowData,
          function(x){
            c(unlist(recursive=FALSE,lapply(1:numLeads,function(y){x})))
          }
        )
      }
      if(na.rm==T){
        self$subset(cols=!apply(private$.mat,2,function(x){any(is.na(x))}))
      }
    },
    ##This scale function is not ideal.  Hopefully, we will replace it soon.
    #' @method scale This function rescales each element of our object according to f
    #' @param f a function which takes in a number and outputs a rescaled version of that number
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    scale = function(f,mutate=TRUE){
      if('scale' %in% private$.debug){
        browser()
      }
      if(!mutate){
        tmp = self$clone(TRUE)
        tmp$scale(f=f,mutate=TRUE)
        return(tmp)
      }
      ##This next part should be improved to deal with the case where f is
      ##  not vectorized
      private$.mat[] = f(private$.mat[])
    },
    #' @method diff This function replaces the matrix value at column i with the difference. between the values at columns i and (i-lag).
    #' @param lag  How far back to diff.  Defaults to 1.
    #' @param mutate Whether to change the original instance, or create a new one.  If FALSE, the instance performing the method will be left unchanged, and a modified copy will be returned.  If true, then the instance will modify itself and return nothing.
    #' @return If \code{mutate=FALSE}, a clone of this object will run the method and be returned.  Otherwise, there is no return.
    diff = function(lag = 1,mutate=TRUE){
      if('diff' %in% private$.debug){
        browser()
      }
      if(lag == 0){
        if(!is.null(private$.rnames)){
          rownames(private$.mat) =
            paste("D",lag,"R",private$.rnames,sep='')
          private$.rnames = rownames(private$.mat)
        } else {
          rownames(private$.mat) =
            paste("D",lag,"R",1:private$.nrow,sep='')
          private$.rnames = rownames(private$.mat)
        }
        return()
      }
      if(lag < 0){
        stop("Lag should be non-negative.")
      }
      if(!mutate){
        tmp = self$clone(TRUE)
        tmp$diff(lag=lag,mutate=TRUE)
        return(tmp)
      }
      private$.mat <- self$mat - self$lag(indices=lag,mutate=FALSE,na.rm=FALSE)$mat
      if(!is.null(private$.rnames)){
        rownames(private$.mat) =
          paste("D",lag,"R",private$.rnames,sep='')
        private$.rnames = rownames(private$.mat)
      } else {
        rownames(private$.mat) =
          paste("D",lag,"R",1:private$.nrow,sep='')
        private$.rnames = rownames(private$.mat)
      }
    },
      #' @method addColumns This function adds empty columns to the right side of the data.
      #' @param columns The number of columns to add.
    addColumns = function(columns){
      if('addColumns' %in% private$.debug){
        browser()
      }
      if(columns == 0){
        return()
      }
      cbind(private$.mat , matrix(NA,private$.nrow,columns)) -> private$.mat
      private$.ncol = ncol(private$.mat)
      if(!is.null(private$.cnames)){
        colnames(private$.mat) = c(private$.cnames,replicate(columns,"NA"))
        private$.cnames = colnames(private$.mat)
      }
      if(length(private$.colData) > 0){
        private$.colData <- lapply(
          private$.colData,
          function(x){
            c(x,replicate(columns,NA))
          }
        )
      }
    },
    #' @method addRows This function adds empty rows to the data.
    #' @param rows The number of rows to add.
    addRows = function(rows){
      if('addRows' %in% private$.debug){
        browser()
      }
      if(rows == 0){
        return()
      }
      rbind(private$.mat , matrix(NA,rows,private$.ncol)) -> private$.mat
      private$.nrow = nrow(private$.mat)
      if(!is.null(private$.rnames)){
        rownames(private$.mat) = c(private$.rnames,replicate(rows,"NA"))
        private$.rnames = rownames(private$.mat)
      }
      if(length(private$.rowData) > 0){
        private$.rowData <- lapply(
          private$.rowData,
          function(x){
            c(x,replicate(rows,NA))
          }
        )
      }
    },
    #' @method mutate This function is a way to modify the data as though it were a matrix.
    #' @param rows Which rows to modify.  These can be numeric or names.
    #' @param cols Which cols to modify.  These can be numeric or names.
    #' @param data The data to change the chosen values to.  It needs to be the right shape.
    mutate = function(rows,cols,data){
      ##For debugging
      if('mutate' %in% private$.debug){
        browser()
      }
      data = as.matrix(data)
      if(missing(rows)){
        rows = 1:private$.nrow
        if(!(is.null(private$.cnames) || is.null(colnames(data)))){
          private$.cnames[cols] = colnames(data)
          colnames(private$.mat) = private$.cnames
        }
      }
      if(missing(cols)){
        cols = 1:private$.ncol
        if(!(is.null(private$.rnames) || is.null(rownames(data)))){
          private$.rnames[rows] = rownames(data)
          rownames(private$.mat) = private$.rnames
        }
      }
      ##Check the size of the data
      ##Make this work better...
      if(is.null(dim(data))){
        stop("Not yet implemented for non-matrixlike objects")
      }
      ##if(nrow(data) != length(rows)){
      ##	stop("The number of rows do not match")
      ##}
      ##if(ncol(data) != length(cols)){
      ##	stop("The number of cols do not match")
      ##}
      if(length(dim(data)) > 2){
        stop("There are too many dimensions in data.")
      }
      if(length(dim(data)) == 2){
        ##The data is a matrix
        private$.mat[rows,cols] = data
      }
    }
  ),
  active = list(
    #' @field mat This is the matrix.  For extensibility, it cannot be written to directly and must be modified through methods.
    mat = function(value){
      if('mat' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.mat)
      }
      stop("Do not write directly to the mat.  Either use methods to modify the mat, or create a new instance.")
    },
    #' @field colData A list of metadata associated with the columns of the data.
    colData = function(value){
      if('colData' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        if(length(private$.colData) > 0){
          for(i in 1:length(private$.colData)){
            if(private$.ncol != length(private$.colData[[i]])){
              stop("If you alter the matrix, please also edit the column metaData.")
            }
          }
        }
        return(private$.colData)
      }
      if(class(value) != 'list'){
        stop("Column metaData should be a list of lists.")
      }
      if(length(value)>0){
        for(i in 1:length(value)){
          ##make this work for matrices and dataframe like things
          if(Reduce('&&',class(value[[i]]) != c('list','character','numeric','integer','logical','raw','complex'))){
            if(dim(as.matrix(value[[i]]))[[1]] != private$.ncol){
              stop(paste('The ',i,'th element of column metaData does not have one element for each column.',sep=''))
            }
          }
          else{
            if(length(value[[i]])!=private$.ncol){
              stop(paste('The ',i,'th element of column metaData does not have one element for each column.',sep=''))
            }
          }
        }
      }
      private$.colData <- value
      if(length(private$.colData) > 0){
        for(i in 1:length(private$.colData)){
          names(private$.colData[[i]]) <- colnames(self$mat)
        }
      }
    },
    #' @field rowData A list of metadata associated with the columns of the data.
    rowData = function(value){
      if('rowData' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        if(length(private$.rowData) > 0){
          for(i in 1:length(private$.rowData)){
            if(private$.nrow != length(private$.rowData[[i]])){
              stop("If you alter the matrix, please also edit the row metaData.")
            }
          }
        }
        return(private$.rowData)
      }
      if(class(value) != 'list'){
        stop("row metaData should be a list of lists.")
      }
      if(length(value) > 0){
        for(i in 1:length(value)){
          ##make this work for matrices and dataframe like things
          if(Reduce('&&',class(value[[i]]) != c('list','character','numeric','integer','logical','raw','complex'))){
            if(dim(as.matrix(value[[i]]))[[1]] != private$.nrow){
              stop(paste('The ',i,'th element of row metaData does not have one element for each row.',sep=''))
            }
          }
          else{
            if(length(value[[i]])!=private$.nrow){
              stop(paste('The ',i,'th element of row metaData does not have one element for each row.',sep=''))
            }
          }
        }
      }
      private$.rowData <- value
      if(length(private$.rowData)>0){
        for(i in 1:length(value)){
          names(private$.rowData[[i]]) <- rownames(self$mat)
        }
      }
    },
    #' @field cellData A list of metadata associated with the cells of the data.
    cellData = function(value){
      ## AbstractClasses.R::Generic::defaultActive
      if('cellData' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        if(length(private$.cellData) > 0){
          if(!all(sapply(
            private$.cellData,
            function(x){
              all(dim(x) == c(self$nrow,self$ncol))
            }
          ))){
            stop("If you edit the matrix, also edit the cell data")
          }
        }
        return(private$.cellData)
      }
      if(class(value) != 'list'){
        stop("cell metaData should be a list of matrices.")
      }
      if(length(value) > 0){
        for(i in 1:length(value)){
          ##make this work for matrices and dataframe like things
          if(class(value[[i]]) != 'matrix'){
            stop(paste('The ',i,'th element of cell metaData should be a matrix'))
          }
          if(nrow(value[[i]]) != private$.nrow){
            stop(paste('The ',i,'th element of cell metaData does not have the right number of rows',sep=''))
          }
          if(ncol(value[[i]]) != private$.ncol){
            stop(paste('The ',i,'th element of cell metaData does not have the right number of columns',sep=''))
          }
          if(length(dim(value[[i]])) != 2){
          }
        }
      }
      private$.cellData <- value
      if(length(private$.cellData)>0){
        for(i in 1:length(value)){
          names(private$.cellData[[i]]) <- names(self$mat)
        }
      }
    },
    #' @field frame A data.frame representation of the IncidenceMatrix.  The frame contains all rows/columns as rows of the data frame, and the rows, columns, values and all metadata types as columns.  This is similar to the melt function.
    frame = function(value){
      if('frame' %in% private$.debug){browser()}
      if(!missing(value)){
        stop("Do not write directly to the frame.  Adjust the matrix instead.")
      }
      #' @importFrom reshape2 melt
      frame = melt(self$mat)
      if(length(self$rowData) > 0){
        for(idx in 1:length(self$rowData)){
          #' @importFrom reshape2 melt
          if(is.null(self$rnames)){
            tmp_frame <- data.frame(Var1 = 1:self$nrow,stringsAsFactors=FALSE)
          } else {
            tmp_frame <- data.frame(Var1 = self$rnames,stringsAsFactors=FALSE)
          }
          tmp_frame$value = self$rowData[[idx]]
          tmp_name = names(self$rowData)[idx]
          if(is.na(tmp_name)){
            tmp_name = idx
          }
          tmp_name = paste('RowData',tmp_name,sep='_')
          names(tmp_frame)[names(tmp_frame)=='value'] = tmp_name
          tmp_frame$Var1 = as(tmp_frame$Var1,class(frame$Var1))
          #' @importFrom dplyr left_join
          frame <- left_join(frame,tmp_frame,by=c('Var1'))
          if(any(endsWith(names(frame),".x"))){
            stop("Not yet written")
          }
        }
      }
      if(length(self$colData) > 0){
        for(idx in 1:length(self$colData)){
          #' @importFrom reshape2 melt
          if(is.null(self$cnames)){
            tmp_frame <- data.frame(Var2 = 1:self$ncol,stringsAsFactors=FALSE)
          } else {
            tmp_frame <- data.frame(Var2 = self$cnames,stringsAsFactors=FALSE)
          }
          tmp_frame$value = self$colData[[idx]]
          tmp_name = names(self$colData)[idx]
          if(is.na(tmp_name)){
            tmp_name = idx
          }
          tmp_name = paste('ColData',tmp_name,sep='_')
          names(tmp_frame)[names(tmp_frame)=='value'] = tmp_name
          tmp_frame$Var2 = as(tmp_frame$Var2,class(frame$Var2))
          #' @importFrom dplyr left_join
          frame <- left_join(frame,tmp_frame,by=c('Var2'))
          if(any(endsWith(names(frame),".x"))){
            stop("Not yet written")
          }
        }
      }
      if(length(self$cellData) > 0){
        for(idx in 1:length(self$cellData)){
          #' @importFrom reshape2 melt
          tmp_frame <- melt(self$cellData[[idx]])
          tmp_name = names(self$cellData)[idx]
          if(is.na(tmp_name)){
            tmp_name = idx
          }
          tmp_name = paste('CellData',tmp_name,sep='_')
          names(tmp_frame)[names(tmp_frame)=='value'] = tmp_name
          #' @importFrom dplyr left_join
          frame <- left_join(frame,tmp_frame,by=c('Var1','Var2'))
          if(any(endsWith(names(frame),".x"))){
            stop("Not yet written")
          }
        }
      }
      if(length(self$metaData) > 0){
        for(idx in 1:length(self$metaData) ){
          tmp_name = names(self$metaData)[idx]
          if(is.na(tmp_name)){
            tmp_name = idx
          }
          tmp_name = paste('MetaData',tmp_name,sep='_')
          frame[[tmp_name]] = list(self$metaData[[idx]])
        }
      }
      return(frame)
    }
  )
)
