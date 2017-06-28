IncidenceMatrix <- R6Class(
  classname = "IncidenceMatrix",
  inherit = AbstractIncidenceMatrix,
  public = list(
    initialize = function(
      data=matrix(),
      metaData=list(),
      rowData=list(),
      colData=list()
    ){
      ##We take in two types of input data:
      ##matrix Data
      ##MatrixData object Data
      if(Reduce(
        '&&',
        c('MatrixData','DataContainer','Generic','R6') %in% class(data))
      ){
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
        ##We could encapsulate this into a function, however I think we don't
        ##  want to enforce subclasses of this class to have a setMatrix method.
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
        ##First, we check the type to see if it is compatible with our current
        ##  data
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
        ##Assuming everything works out, we set the size of the matrix and the
        ##  matrix itself
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
        self$colData <- colData
        self$metaData <- metaData
      }
    },
    subset = function(rows,cols,mutate=TRUE){
      ##for debugging: see AbstractClasses::Generic::debug for details.
      if('subset' %in% private$.debug){
        browser()
      }
      ##If we are not to modify the current object, clone it and then run the
      ##  mutating one on the clone.
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
        ##We need to clean up the column metadata since we changed the number of
        ##  columns
        ##Column metadata is a list of lists, so we iterate over it.
        ##This if statement is necessary for dealing with the fact that 1:0 in R
        ##  is non-empty.
        if(length(private$.colData) > 0){
          ##Without the previous if statement, this loop would be nonempty when
          ##  length(private$.colData) == 0, which is bad
          private$.colData <- lapply(
            private$.colData,function(x){x[cols,drop=FALSE]}
          )
        }
      }
      else if(missing(cols)){
        ##Keep all columns, select a subset of rows.
        ##drop=FALSE keeps the dimension from automatically reducing.
        private$.mat = self$mat[rows,,drop=FALSE]
        ##We need to clean up the row metadata since we changed the number of
        ##  rows
        ##Row metadata is a list of lists, so we iterate over it.
        ##This if statement is necessary for dealing with the fact that 1:0 in R
        ##  is non-empty.
        if(length(private$.rowData) > 0){
          private$.rowData <- lapply(
            private$.rowData,function(x){x[rows,drop=FALSE]}
          )
        }
      }
      else{
        ##We select a subset of rows and columns
        ##drop=FALSE keeps the dimension from automatically reducing
        private$.mat = self$mat[rows,cols,drop=FALSE]
        ##We need to clean up row and column metadata.
        ##Row metadata is a list of lists so iterate over it
        if(length(private$.rowData)>0){
          private$.rowData <- lapply(
            private$.rowData,function(x){x[rows,drop=FALSE]}
          )
        }
        if(length(private$.colData)>0){
          private$.colData <- lapply(
            private$.colData,function(x){x[cols,drop=FALSE]}
          )
          ##for(i in 1:length(private$.colData)){
          ##	#Does this work? The drop seems like it might cause problems.
          ##	#TODO: Write a test case for it
          ##	#Subset ith element of the column metadata.
          ##	private$.colData[[i]] = private$.colData[[i]][cols,drop=FALSE]
          ##}
        }
      }
      private$.nrow = nrow(private$.mat)
      private$.rnames = rownames(private$.mat)
      private$.ncol = ncol(private$.mat)
      private$.cnames = colnames(private$.mat)
      ##Note, we don't return anything, because this modifies the object.
    },
    head = function(k,direction=2){
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
        ##rows
        ##This is the part where we actually do the head
        private$.mat = self$mat[indices,,drop=FALSE]
        ##We also deal with the metadata
        if(length(private$.rowData)>0){
          for(i in 1:length(private$.rowData)){
            private$.rowData[[i]] = private$.rowData[[i]][indices,drop=FALSE]
          }
        }
      }
      else if(direction==2){
        ##columns
        ##This is the part where we actually do the head
        private$.mat = self$mat[,indices,drop=FALSE]
        ##We also deal with the metadata
        if(length(private$.colData)>0){
          for(i in 1:length(private$.colData)){
            private$.colData[[i]] = private$.colData[[i]][indices,drop=FALSE]
          }
        }
      }
      else{
        stop("This direction is not allowed.")
      }
      private$.nrow = nrow(private$.mat)
      private$.ncol = ncol(private$.mat)
      private$.cnames = colnames(private$.mat)
      private$.rnames = rownames(private$.mat)
    },
    tail = function(k,direction=2){
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
        ##rows
        ##This is the part where we actually do the tail
        private$.mat = self$mat[indices,,drop=FALSE]
        ##We also deal with the metadata
        if(length(private$.rowData)>0){
          for(i in 1:length(private$.rowData)){
            private$.rowData[[i]] = private$.rowData[[i]][indices,drop=FALSE]
          }
        }
      }
      else if(direction==2){
        ##columns
        ##This is the part where we actually do the tail
        private$.mat = self$mat[,indices,drop=FALSE]
        ##We also deal with the metadata
        if(length(private$.colData)>0){
          for(i in 1:length(private$.colData)){
            private$.colData[[i]] = private$.colData[[i]][indices,drop=FALSE]
          }
        }
      }
      else{
        stop("This direction is not allowed.")
      }
      private$.nrow = nrow(private$.mat)
      private$.ncol = ncol(private$.mat)
      private$.cnames = colnames(private$.mat)
      private$.rnames = rownames(private$.mat)
    },
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
      ##replicate has some automatic simplification behaviour which is hard to
      ##  deal with
      rownames = replicate(numLags,rownames(private$.mat))
      colnames = colnames(private$.mat)
      ##Replicate the matrix once for each lag.
      private$.mat <- 0+array(self$mat,c(dim(self$mat),numLags))
      if(numLags <= 0){
        stop("indices must be nonempty for the calculation of lags to make sense.")
      }
      ##For each lag, we need to actually shift the values back in time, and add
      ##  NA where appropriate
      for(lag in 1:numLags){
        private$.mat[,(1+indices[[lag]]):self$ncol,lag] <-
          private$.mat[,1:(self$ncol-indices[[lag]]),lag]
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
      ##We populate the column names with renaming to account for lagged columns
      rownames(private$.mat) <-
        as.character(paste(lagnames,"R",rownames,sep=''),numLags*self$nrow)
      colnames(private$.mat) <- colnames
      private$.nrow = self$nrow * numLags
      private$.rnames = rownames(private$.mat)
      if(length(private$.rowData) > 0){
        private$.rowData <- lapply(
          private$.rowData,
          function(x){
            c(unlist(recursive=FALSE,lapply(1:numLags,function(y){x})))
          }
        )
      }
      if(na.rm==T){
        self$subset(cols=!apply(private$.mat,2,function(x){any(is.na(x))}))
      }
    },
    ##This scale function is not ideal.  Hopefully, we will replace it soon.
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
      private$.mat <-
        self$mat - self$lag(indices=lag,mutate=FALSE,na.rm=FALSE)$mat
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
    mat = function(value){
      "The matrix of data."
      if('mat' %in% private$.debug){
        browser()
      }
      if(missing(value)){
        return(private$.mat)
      }
      stop(
        "Do not write directly to the mat. Either use methods to modify the mat,
         or create a new instance."
      )
    },
    #' @field colData A list of metadata associated with the columns of the data.
    colData = function(value){
      "The metaData associated with column in the matrix"
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
          if(
            Reduce(
              '&&',
              class(value[[i]]) !=
                c(
                  'list',
                  'character',
                  'numeric',
                  'integer',
                  'logical',
                  'raw',
                  'complex'
                )
            )
          ){
            if(dim(as.matrix(value[[i]]))[[1]] != private$.ncol){
              stop(paste(
                'The ',
                i,
                'th element of column metaData does not have one element for',
                'each column.',
                sep=''
              ))
            }
          }
          else{
            if(length(value[[i]])!=private$.ncol){
              stop(paste(
                'The ',
                i,
                'th element of column metaData does not have one element for',
                'each column.',
                sep=''
              ))
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
    rowData = function(value){
      "The metaData associated with rows in the matrix"
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
          if(
            Reduce('&&',
              class(value[[i]]) !=
                c(
                  'list',
                  'character',
                  'numeric',
                  'integer',
                  'logical',
                  'raw',
                  'complex'
                )
            )
          ){
            if(dim(as.matrix(value[[i]]))[[1]] != private$.nrow){
              stop(paste(
                'The ',
                i,
                'th element of row metaData does not have one element for each',
                'row.',
                sep=''
              ))
            }
          }
          else{
            if(length(value[[i]])!=private$.nrow){
              stop(paste(
                'The ',
                i,
                'th element of row metaData does not have one element for each',
                'row.',
                sep=''
              ))
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
    }
  )
)
