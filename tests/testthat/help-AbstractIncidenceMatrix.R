################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####Incidence Matrix Test Functions
################################################################################

source('help-MatrixData.R')
#######################################TODO#####################################


is_same_AbstractIncidenceMatrix_as = function(rhs){
  return(function(lhs){
    is_same_MatrixData_as(rhs)(lhs)
  })
}


test_AbstractIncidenceMatrix = function(object,name,equivalence){
  if(!('AbstractIncidenceMatrix' %in% class(object))){
    warning("Object is not an AbstractIncidenceMatrix.")
  }

  subset_runs = FALSE
  scale_runs = FALSE
  diff_runs = FALSE
  tail_runs = FALSE
  head_runs = FALSE
  lag_runs = FALSE
  lead_runs = FALSE
  addColumns_runs = FALSE
  addRows_runs = FALSE
  mutate_runs = FALSE

  first_object = object$clone(TRUE)
  test_MatrixData(object,name,equivalence)

  test_that(paste(name,": Inherited Methods don't change object"),{
    equivalence(first_object)(object)
  })

  test_that(paste(name,": subset works"),{
    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(1,1),NA)
    tempObject = object$clone(TRUE)
    expect_error({
      a = tempObject$nrow
      tempObject$subset(rows=a+1,cols=1)
    },'subscript out of bounds'
    )
    tempObject = object$clone(TRUE)
    expect_error({
      a = tempObject$ncol
      tempObject$subset(rows=1,cols=a+1)
    },'subscript out of bounds')

    tempObject = object$clone(TRUE)
    rn = object$rnames
    cn = object$cnames
    if(is.null(rn) && is.null(cn)){
      skip("No row names or column names")
    }

    if(!is.null(rn)){
      expect_error(tempObject$subset(rn),NA)
      expect_that(tempObject$mat,equals(object$mat[rn,,drop=FALSE]))
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(rn[[1]]),NA)
    }
    tempObject = object$clone(TRUE)
    if(!is.null(cn)){
      expect_error(tempObject$subset(cols=cn),NA)
      expect_that(tempObject$mat,equals(object$mat[,cn,drop=FALSE]))
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(cols= cn[[1]]),NA)
    }

  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$subset()
    subset_runs=TRUE
  },silent = TRUE)


  test_that(paste(name,": subset respects nrow"),{
    if(!subset_runs){
      skip("Skipping additional subset tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(rows=1),NA)
    expect_equal(tempObject$nrow,1)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(rows = tempObject$nrow),NA)
    expect_equal(tempObject$nrow,1)

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(rows = c(1,tempObject$nrow)),NA)
      expect_equal(tempObject$nrow,2)
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": subset respects ncol"),{
    if(!subset_runs){
      skip("Skipping additional subset tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(cols=1),NA)
    expect_equal(tempObject$ncol,1)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(cols = tempObject$ncol),NA)
    expect_equal(tempObject$ncol,1)

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(cols = c(1,tempObject$ncol)),NA)
      expect_equal(tempObject$ncol,2)
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": subset respects rnames"),{
    if(!subset_runs){
      skip("Skipping additional subset tests.")
    }
    if(is.null(object$rnames)){
      skip("No row names to test on.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(rows=tempObject$rnames[1]),NA)
    expect_equal(tempObject$nrow,1)
    expect_equal(tempObject$rnames,object$rnames[1])

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(rows = tempObject$rnames[tempObject$nrow]),NA)
    expect_equal(tempObject$nrow,1)
    expect_equal(tempObject$rnames,object$rnames[object$nrow])

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(rows = tempObject$rnames[1,tempObject$nrow]),NA)
      expect_equal(tempObject$nrow,2)
      expect_equal(tempObject$rnames,object$rnames[1,object$nrow])
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": subset respects cnames"),{
    if(!subset_runs){
      skip("Skipping additional subset tests.")
    }
    if(is.null(object$cnames)){
      skip("No column names to test on.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(cols=tempObject$cnames[1]),NA)
    expect_equal(tempObject$ncol,1)
    expect_equal(tempObject$cnames,object$cnames[1])

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(cols = tempObject$cnames[tempObject$ncol]),NA)
    expect_equal(tempObject$ncol,1)
    expect_equal(tempObject$cnames,object$cnames[object$ncol])

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(cols = tempObject$cnames[1,tempObject$ncol]),NA)
      expect_equal(tempObject$ncol,2)
      expect_equal(tempObject$cnames,object$cnames[1,object$ncol])
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": subset respects rowData"),{
    if(!subset_runs){
      skip("Skipping additional subset tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(rows=1),NA)
    expect_equal(tempObject$rowData,lapply(object$rowData,function(x){x[c(1)]}))

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(rows = tempObject$nrow),NA)
    expect_equal(tempObject$rowData,lapply(object$rowData,function(x){x[c(object$nrow)]}))

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(rows = c(1,tempObject$nrow)),NA)
      expect_equal(tempObject$rowData,lapply(object$rowData,function(x){x[c(1,object$nrow)]}))
    }
  })

  test_that(paste(name,": subset respects colData"),{
    if(!subset_runs){
      skip("Skipping additional subset tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(cols=1),NA)
    expect_equal(tempObject$colData,lapply(object$colData,function(x){x[c(1)]}))

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(cols = tempObject$ncol),NA)
    expect_equal(tempObject$colData,lapply(object$colData,function(x){x[object$ncol]}))

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(cols = c(1,tempObject$cols)),NA)
      expect_equal(tempObject$colData,lapply(object$colData,function(x){x[c(1,object$ncol)]}))
    }
  })

  test_that(paste(name,": tail works"),{

    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1),NA)
    #Dimensions
    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,1),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,2),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,3))
    #span
    tempObject = object$clone(TRUE)
    rows = tempObject$nrow
    cols = tempObject$ncol
    expect_that({
      tempObject$tail(rows,1)
      tempObject
    },equivalence(object))
    expect_that({
      tempObject$tail(cols,2)
      tempObject
    },equivalence(object))
    expect_error(tempObject$tail(rows+1,1))
    expect_error(tempObject$tail(cols+1,2))
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$tail(1,1)
    tail_runs = TRUE
  },silent=TRUE)

  test_that(paste(name,": tail respects nrow"),{
    if(!tail_runs){
      skip("Skipping additional tail tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,1),NA)
    expect_equal(tempObject$nrow,1)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,2),NA)
    expect_equal(tempObject$nrow,object$nrow)

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$tail(2,1),NA)
      expect_equal(tempObject$nrow,2)
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": tail respects metaData"),{
    if(!tail_runs){
      skip("Skipping additional tail tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,1),NA)
    expect_equal(tempObject$metaData,object$metaData)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,2),NA)
    expect_equal(tempObject$metaData,object$metaData)

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$tail(2,1),NA)
      expect_equal(tempObject$metaData,object$metaData)
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": tail respects ncol"),{
    if(!tail_runs){
      skip("Skipping additional tail tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(k=1,direction=1),NA)
    expect_equal(tempObject$ncol,object$ncol)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(k=1,direction=2),NA)
    expect_equal(tempObject$ncol,1)

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$tail(k=2,direction=2),NA)
      expect_equal(tempObject$ncol,2)
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": tail respects rnames"),{
    if(!tail_runs){
      skip("Skipping additional tail tests.")
    }
    if(is.null(object$rnames)){
      skip("No row names to test on.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(k=1,direction=1),NA)
    expect_equal(tempObject$rnames,object$rnames[object$nrow])

    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(k=1,direction=2),NA)
    expect_equal(tempObject$rnames,object$rnames)

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$tail(k=2,direction=1),NA)
      expect_equal(tempObject$rnames,object$rnames[(object$nrow-1):object$nrow])
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": tail respects cnames"),{
    if(!tail_runs){
      skip("Skipping additional tail tests.")
    }
    if(is.null(object$cnames)){
      skip("No column names to test on.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,1),NA)
    expect_equal(tempObject$cnames,object$cnames)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,2),NA)
    expect_equal(tempObject$cnames,object$cnames[object$ncol])

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$tail(2,2),NA)
      expect_equal(tempObject$cnames,object$cnames[(object$ncol-1):object$ncol])
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": tail respects rowData"),{
    if(!tail_runs){
      skip("Skipping additional tail tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,1),NA)
    expect_equal(tempObject$rowData,lapply(object$rowData,function(x){x[object$nrow]}))

    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,2),NA)
    expect_equal(tempObject$rowData,object$rowData)

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(2,1),NA)
      expect_equal(tempObject$rowData,lapply(object$rowData,function(x){x[(object$nrow-1):object$nrow]}))
    }
  })

  test_that(paste(name,": tail respects colData"),{
    if(!tail_runs){
      skip("Skipping additional tail tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,1),NA)
    expect_equal(tempObject$colData,object$colData)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,2),NA)
    expect_equal(tempObject$colData,lapply(object$colData,function(x){x[object$ncol]}))

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$tail(2,2),NA)
      expect_equal(tempObject$colData,lapply(object$colData,function(x){x[(object$ncol-1):object$ncol]}))
    }
  })

  test_that(paste(name,": tail respects mat"),{
    if(!tail_runs){
      skip("Skipping additional tail tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_error(tempObject$tail(1,1),NA)
    expect_equal(tempObject$mat,object$mat[object$nrow,,drop=FALSE ])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$tail(1,2),NA)
    expect_equal(tempObject$mat,object$mat[,object$ncol,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$tail(2,1),NA)
      expect_equal(tempObject$mat,object$mat[object$nrow-1:0,,drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
    }

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$tail(2,2),NA)
      expect_equal(tempObject$mat,object$mat[,object$ncol-1:0,drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
    }

    expect_equal(object,firstObject)
  })

  test_that(paste(name,": lag works"),{

    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$lag(0),NA)
    tempObject = object$clone(TRUE)
    #expect_that({
    #  tempObject$lag(0)
    #  tempObject
    #},equivalence(object))
    tempObject = object$clone(TRUE)
    if(tempObject$ncol > 1){
      expect_error(tempObject$lag(c(1)),NA)
    }
    expect_error(tempObject$lag(tempObject$ncol),"We cannot go further back than the start of the matrix")
    expect_that(object,equivalence(firstObject))
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$lag(0)
    lag_runs=TRUE
  },silent=TRUE)

  test_that(paste(name,": lag respects nrow"),{
    if(!lag_runs){
      skip("Skipping additional lag tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    expect_that({
      tempObject$lag(c(1))
      tempObject$nrow
    },equals(object$nrow))
    expect_that({
      tempObject$lag(c(0,1))
      tempObject$nrow/2
    },equals(object$nrow))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lag respects ncol"),{
    if(!lag_runs){
      skip("Skipping additional lag tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    expect_that({
      tempObject$lag(c(1))
      tempObject$ncol
    },equals(object$ncol))
    expect_that({
      tempObject$lag(c(0,1))
      tempObject$ncol
    },equals(object$ncol))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lag respects rnames"),{
    if(!lag_runs){
      skip("Skipping additional lag tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    if(is.null(object$rnames)){
      skip("No row names to test on")
    }
    expect_that({
      tempObject$lag(c(1))
      tempObject$rnames
    },equals(paste("L1R",object$rnames,sep='')))
    #expect_that({
    #  tempObject$lag(c(0,1))
    #  tempObject$nrow/2
    #},equals(object$nrow))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lag respects cnames"),{
    if(!lag_runs){
      skip("Skipping additional lag tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    if(is.null(object$cnames)){
      skip("No column names to test on")
    }
    expect_that({
      tempObject$lag(c(1))
      tempObject$cnames
    },equals(object$cnames))
    expect_that({
      tempObject$lag(c(0,1))
      tempObject$cnames
    },equals(object$cnames))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lag respects rowData"),{
    if(!lag_runs){
      skip("Skipping additional lag tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    if(length(tempObject$rowData) == 0){
      skip("No row data to test on.")
    }
    expect_that({
      tempObject$lag(c(1))
      tempObject$rowData
    },equals(object$rowData))
    tempObject=object$clone(TRUE)
    expect_that({
      tempObject$lag(c(0,1))
      tempObject$rowData
    },equals(
      lapply(object$rowData,function(x){c(unlist(recursive=FALSE,lapply(1:2,function(y){x})))})
      #lapply(tempObject$rowData,function(x){abind(lapply(1:2,function(y){x}))}))
    ))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lag respects colData"),{
    if(!lag_runs){
      skip("Skipping additional lag tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    if(length(tempObject$colData) == 0){
      skip("No col data to test on.")
    }
    expect_that({
      tempObject$lag(c(1))
      tempObject$colData
    },equals(object$colData))
    expect_that({
      tempObject$lag(c(0,1))
      tempObject$colData
    },equals(object$colData))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lag respects metaData"),{
    if(!lag_runs){
      skip("Skipping additional lag tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_that({
      tempObject$lag(0)
      tempObject$metaData
    },equals(object$metaData))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns works"),{
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$addColumns(0),NA)
    expect_that({
      tempObject$addColumns(0)
      tempObject
    },equivalence(object))
    expect_error(tempObject$addColumns(2),NA)
    expect_that(object,equivalence(firstObject))
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$addColumns(0)
    addColumns_runs=TRUE
  },silent=TRUE)

  test_that(paste(name,": addColumns respects nrow"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(0)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$nrow
    },object$nrow)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects ncol"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(0)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$ncol
    },object$ncol+2)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects rnames"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$rnames)){
      skip("No row names to test on.")
    }
    expect_equal({
      tempObject$addColumns(0)
      tempObject$rnames
    },object$rnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$rnames
    },object$rnames)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects cnames"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$cnames)){
      skip("No column names to test on.")
    }
    expect_equal({
      tempObject$addColumns(0)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    #expect_equal({
    #  tempObject$addColumns(2)
    #  tempObject$cnames
    #},c(object$cnames,"NA","NA"))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects metaData"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$metaData) == 0){
      skip("No metadata names to test on.")
    }
    expect_equal({
      tempObject$addColumns(0)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$metaData
    },object$metaData)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects rowData"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$rowData) == 0){
      skip("No row metadata to test on.")
    }
    expect_equal({
      tempObject$addColumns(0)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$rowData
    },object$rowData)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects colData"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$colData) == 0){
      skip("No column names to test on.")
    }
    expect_equal({
      tempObject$addColumns(0)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$colData
    },lapply(object$colData,function(x){c(x,NA,NA)}))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows works"),{
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$addRows(0),NA)
    expect_that({
      tempObject$addRows(0)
      tempObject
    },equivalence(object))
    expect_error(tempObject$addRows(2),NA)
    expect_that(object,equivalence(firstObject))
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$addRows(0)
    addRows_runs=TRUE
  },silent=TRUE)

  test_that(paste(name,": addRows respects nrow"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(0)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$nrow
    },object$nrow+2)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects ncol"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(0)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$ncol
    },object$ncol)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects rnames"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$rnames)){
      skip("No row names to test on.")
    }
    expect_equal({
      tempObject$addRows(0)
      tempObject$rnames
    },object$rnames)
    tempObject = object$clone(TRUE)
    #expect_equal({
    #  tempObject$addRows(2)
    #  tempObject$rnames
    #},c(object$rnames,"NA","NA"))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects cnames"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$cnames)){
      skip("No column names to test on.")
    }
    expect_equal({
      tempObject$addRows(0)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$cnames
    },object$cnames)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects metaData"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$metaData) == 0){
      skip("No metadata names to test on.")
    }
    expect_equal({
      tempObject$addRows(0)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$metaData
    },object$metaData)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects rowData"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$rowData) == 0){
      skip("No row metadata to test on.")
    }
    expect_equal({
      tempObject$addRows(0)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$rowData
    },lapply(object$rowData,function(x){c(x,NA,NA)}))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects colData"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$colData) == 0){
      skip("No column names to test on.")
    }
    expect_equal({
      tempObject$addRows(0)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$colData
    },object$colData)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": mutate works"),{
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$mutate(data=NA),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$mutate(rows=1,data=NA),NA)
    expect_that({
      tempObject$subset(rows=-1)
      tempObject
    },equivalence(object$subset(mutate=FALSE,rows=-1)))
    tempObject = object$clone(TRUE)
    expect_error({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
    },NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$mutate(cols=1,data=NA),NA)
    expect_that({
      tempObject$subset(cols=-1)
      tempObject
    },equivalence(object$subset(mutate=FALSE,cols=-1)))
    tempObject = object$clone(TRUE)
    expect_error({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
    },NA)
    tempObject=object$clone(TRUE)
    expect_error(tempObject$mutate(rows=1,cols=1,data=NA),NA)
    expect_equal(object,firstObject)
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$mutate(1,1,NA)
    mutate_runs=TRUE
  },silent=TRUE)

  test_that(paste(name,": mutate respects nrow"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$nrow
    },object$nrow)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$nrow
    },object$nrow)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects ncol"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$ncol
    },object$ncol)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$nrow
    },object$nrow)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects rnames"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$rnames)){
      skip("No row names to test on.")
    }
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$rnames
    },object$rnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$rnames
    },object$rnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$rnames
    },c(as.character(1),object$rnames[-1]))
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$rnames
    },object$rnames)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$rnames
    },object$rnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$rnames
    },object$rnames)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects cnames"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$cnames)){
      skip("No column names to test on.")
    }
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$cnames
    },object$cnames)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$cnames
    },c('1',object$cnames[-1]))
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects metaData"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$metaData)){
    }
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$metaData
    },object$metaData)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$metaData
    },object$metaData)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects rowData"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$rowData)){
      skip("No row names to test on.")
    }
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$rowData
    },object$rowData)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    #Fix this...
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$rowData
    },object$rowData)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects colData"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$colData)){
      skip("No row names to test on.")
    }
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$colData
    },object$colData)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    #Fix this...
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$colData
    },object$colData)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": subset respects mat"),{
    if(!subset_runs){
      skip("Skipping additional subset tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_error(tempObject$subset(rows=1),NA)
    expect_equal(tempObject$mat,object$mat[1,,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(rows = tempObject$nrow),NA)
    expect_equal(tempObject$mat,object$mat[object$nrow,,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(rows = c(1,tempObject$nrow)),NA)
      expect_equal(tempObject$mat,object$mat[c(1,object$nrow),drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
    }

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(cols=1),NA)
    expect_equal(tempObject$mat,object$mat[,1,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(cols = tempObject$ncol),NA)
    expect_equal(tempObject$mat,object$mat[,object$ncol,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(cols = c(1,tempObject$ncol)),NA)
      expect_equal(tempObject$mat,object$mat[,c(1,object$ncol),drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
    }

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(rows=1,cols = 1),NA)
    expect_equal(tempObject$mat,object$mat[1,1,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(rows= 1,cols = c(1,tempObject$ncol)),NA)
      expect_equal(tempObject$mat,object$mat[1,c(1,object$ncol),drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
    }

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(rows= c(1,tempObject$nrow,cols = 1)),NA)
      expect_equal(tempObject$mat,object$mat[c(1,object$nrow),1,drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
      if(tempObject$ncol > 1){
        tempObject = object$clone(TRUE)
        expect_error(tempObject$subset(rows= c(1,tempObject$nrow),cols = c(1,tempObject$ncol)),NA)
        expect_equal(tempObject$mat,object$mat[c(1,object$nrow),c(1,object$ncol),drop=FALSE])
        expect_equal(rownames(tempObject$mat),tempObject$rnames)
        expect_equal(nrow(tempObject$mat),tempObject$nrow)
        expect_equal(colnames(tempObject$mat),tempObject$cnames)
        expect_equal(ncol(tempObject$mat),tempObject$ncol)
      }
    }

    expect_equal(object,firstObject)
  })


  test_that(paste(name,": lag respects mat"),{
    if(!lag_runs){
      skip("Skipping additional lag tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$lag(c(0))
      tempObject$mat
    },object$mat,check.names=FALSE,check.attributes=FALSE)

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_equal({
        tempObject$lag(c(1))
        tempObject$mat[,-1]
      },object$mat[,-object$ncol],check.attributes=FALSE,check.names=FALSE)

      tempObject = object$clone(TRUE)
      expect_equal({
        tempObject$lag(c(0,1))
        tempObject$mat[,-1,drop=FALSE]
      },rbind(object$mat[,-1,drop=FALSE],object$mat[,-object$ncol,drop=FALSE]),check.attributes=FALSE,check.names=FALSE)
    }

    expect_that(object,equivalence(firstObject))
  })


  test_that(paste(name,": lead works"),{

    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$lead(0),NA)
    tempObject = object$clone(TRUE)
    #expect_that({
    #  tempObject$lead(0)
    #  tempObject
    #},equivalence(object))
    tempObject = object$clone(TRUE)
    if(tempObject$ncol > 1){
      expect_error(tempObject$lead(c(1)),NA)
    }
    expect_error(tempObject$lead(tempObject$ncol),"We cannot go further back than the start of the matrix")
    expect_that(object,equivalence(firstObject))
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$lead(0)
    lead_runs=TRUE
  },silent=TRUE)

  test_that(paste(name,": lead respects nrow"),{
    if(!lead_runs){
      skip("Skipping additional lead tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    expect_that({
      tempObject$lead(c(1))
      tempObject$nrow
    },equals(object$nrow))
    expect_that({
      tempObject$lead(c(0,1))
      tempObject$nrow/2
    },equals(object$nrow))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lead respects ncol"),{
    if(!lead_runs){
      skip("Skipping additional lead tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    expect_that({
      tempObject$lead(c(1))
      tempObject$ncol
    },equals(object$ncol))
    expect_that({
      tempObject$lead(c(0,1))
      tempObject$ncol
    },equals(object$ncol))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lead respects rnames"),{
    if(!lead_runs){
      skip("Skipping additional lead tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    if(is.null(object$rnames)){
      skip("No row names to test on")
    }
    expect_that({
      tempObject$lead(c(1))
      tempObject$rnames
    },equals(paste("L1R",object$rnames,sep='')))
    #expect_that({
    #  tempObject$lead(c(0,1))
    #  tempObject$nrow/2
    #},equals(object$nrow))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lead respects cnames"),{
    if(!lead_runs){
      skip("Skipping additional lead tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    if(is.null(object$cnames)){
      skip("No column names to test on")
    }
    expect_that({
      tempObject$lead(c(1))
      tempObject$cnames
    },equals(object$cnames))
    expect_that({
      tempObject$lead(c(0,1))
      tempObject$cnames
    },equals(object$cnames))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lead respects rowData"),{
    if(!lead_runs){
      skip("Skipping additional lead tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    if(length(tempObject$rowData) == 0){
      skip("No row data to test on.")
    }
    expect_that({
      tempObject$lead(c(1))
      tempObject$rowData
    },equals(object$rowData))
    tempObject=object$clone(TRUE)
    expect_that({
      tempObject$lead(c(0,1))
      tempObject$rowData
    },equals(
      lapply(object$rowData,function(x){c(unlist(recursive=FALSE,lapply(1:2,function(y){x})))})
      #lapply(tempObject$rowData,function(x){abind(lapply(1:2,function(y){x}))}))
    ))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lead respects colData"),{
    if(!lead_runs){
      skip("Skipping additional lead tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test properly.")
    }
    if(length(tempObject$colData) == 0){
      skip("No col data to test on.")
    }
    expect_that({
      tempObject$lead(c(1))
      tempObject$colData
    },equals(object$colData))
    expect_that({
      tempObject$lead(c(0,1))
      tempObject$colData
    },equals(object$colData))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": lead respects metaData"),{
    if(!lead_runs){
      skip("Skipping additional lead tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_that({
      tempObject$lead(0)
      tempObject$metaData
    },equals(object$metaData))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns works"),{
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$addColumns(0),NA)
    expect_that({
      tempObject$addColumns(0)
      tempObject
    },equivalence(object))
    expect_error(tempObject$addColumns(2),NA)
    expect_that(object,equivalence(firstObject))
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$addColumns(0)
    addColumns_runs=TRUE
  },silent=TRUE)

  test_that(paste(name,": addColumns respects nrow"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(0)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$nrow
    },object$nrow)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects ncol"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(0)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$ncol
    },object$ncol+2)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects rnames"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$rnames)){
      skip("No row names to test on.")
    }
    expect_equal({
      tempObject$addColumns(0)
      tempObject$rnames
    },object$rnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$rnames
    },object$rnames)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects cnames"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$cnames)){
      skip("No column names to test on.")
    }
    expect_equal({
      tempObject$addColumns(0)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    #expect_equal({
    #  tempObject$addColumns(2)
    #  tempObject$cnames
    #},c(object$cnames,"NA","NA"))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects metaData"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$metaData) == 0){
      skip("No metadata names to test on.")
    }
    expect_equal({
      tempObject$addColumns(0)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$metaData
    },object$metaData)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects rowData"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$rowData) == 0){
      skip("No row metadata to test on.")
    }
    expect_equal({
      tempObject$addColumns(0)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$rowData
    },object$rowData)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects colData"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$colData) == 0){
      skip("No column names to test on.")
    }
    expect_equal({
      tempObject$addColumns(0)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addColumns(2)
      tempObject$colData
    },lapply(object$colData,function(x){c(x,NA,NA)}))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows works"),{
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$addRows(0),NA)
    expect_that({
      tempObject$addRows(0)
      tempObject
    },equivalence(object))
    expect_error(tempObject$addRows(2),NA)
    expect_that(object,equivalence(firstObject))
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$addRows(0)
    addRows_runs=TRUE
  },silent=TRUE)

  test_that(paste(name,": addRows respects nrow"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(0)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$nrow
    },object$nrow+2)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects ncol"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(0)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$ncol
    },object$ncol)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects rnames"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$rnames)){
      skip("No row names to test on.")
    }
    expect_equal({
      tempObject$addRows(0)
      tempObject$rnames
    },object$rnames)
    tempObject = object$clone(TRUE)
    #expect_equal({
    #  tempObject$addRows(2)
    #  tempObject$rnames
    #},c(object$rnames,"NA","NA"))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects cnames"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$cnames)){
      skip("No column names to test on.")
    }
    expect_equal({
      tempObject$addRows(0)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$cnames
    },object$cnames)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects metaData"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$metaData) == 0){
      skip("No metadata names to test on.")
    }
    expect_equal({
      tempObject$addRows(0)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$metaData
    },object$metaData)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects rowData"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$rowData) == 0){
      skip("No row metadata to test on.")
    }
    expect_equal({
      tempObject$addRows(0)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$rowData
    },lapply(object$rowData,function(x){c(x,NA,NA)}))
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addRows respects colData"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(length(tempObject$colData) == 0){
      skip("No column names to test on.")
    }
    expect_equal({
      tempObject$addRows(0)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$addRows(2)
      tempObject$colData
    },object$colData)
    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": mutate works"),{
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$mutate(data=NA),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$mutate(rows=1,data=NA),NA)
    expect_that({
      tempObject$subset(rows=-1)
      tempObject
    },equivalence(object$subset(mutate=FALSE,rows=-1)))
    tempObject = object$clone(TRUE)
    expect_error({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
    },NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$mutate(cols=1,data=NA),NA)
    expect_that({
      tempObject$subset(cols=-1)
      tempObject
    },equivalence(object$subset(mutate=FALSE,cols=-1)))
    tempObject = object$clone(TRUE)
    expect_error({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
    },NA)
    tempObject=object$clone(TRUE)
    expect_error(tempObject$mutate(rows=1,cols=1,data=NA),NA)
    expect_equal(object,firstObject)
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$mutate(1,1,NA)
    mutate_runs=TRUE
  },silent=TRUE)

  test_that(paste(name,": mutate respects nrow"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$nrow
    },object$nrow)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$nrow
    },object$nrow)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$nrow
    },object$nrow)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects ncol"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$ncol
    },object$ncol)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$ncol
    },object$ncol)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$nrow
    },object$nrow)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects rnames"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$rnames)){
      skip("No row names to test on.")
    }
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$rnames
    },object$rnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$rnames
    },object$rnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$rnames
    },c(as.character(1),object$rnames[-1]))
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$rnames
    },object$rnames)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$rnames
    },object$rnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$rnames
    },object$rnames)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects cnames"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$cnames)){
      skip("No column names to test on.")
    }
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$cnames
    },object$cnames)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$cnames
    },object$cnames)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$cnames
    },c('1',object$cnames[-1]))
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects metaData"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$metaData)){
    }
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$metaData
    },object$metaData)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$metaData
    },object$metaData)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$metaData
    },object$metaData)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects rowData"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$rowData)){
      skip("No row names to test on.")
    }
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$rowData
    },object$rowData)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$rowData
    },object$rowData)
    tempObject = object$clone(TRUE)
    #Fix this...
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$rowData
    },object$rowData)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": mutate respects colData"),{
    if(!mutate_runs){
      skip("Skipping additional mutate tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(tempObject$colData)){
      skip("No row names to test on.")
    }
    expect_equal({
      tempObject$mutate(data=NA)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,data=NA)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    expect_equal({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$mutate(cols=1,data=NA)
      tempObject$colData
    },object$colData)
    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$colData
    },object$colData)
    tempObject = object$clone(TRUE)
    #Fix this...
    expect_equal({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$colData
    },object$colData)
    expect_equal(object,firstObject)
  })

  test_that(paste(name,": subset respects mat"),{
    if(!subset_runs){
      skip("Skipping additional subset tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_error(tempObject$subset(rows=1),NA)
    expect_equal(tempObject$mat,object$mat[1,,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(rows = tempObject$nrow),NA)
    expect_equal(tempObject$mat,object$mat[object$nrow,,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(rows = c(1,tempObject$nrow)),NA)
      expect_equal(tempObject$mat,object$mat[c(1,object$nrow),drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
    }

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(cols=1),NA)
    expect_equal(tempObject$mat,object$mat[,1,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(cols = tempObject$ncol),NA)
    expect_equal(tempObject$mat,object$mat[,object$ncol,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(cols = c(1,tempObject$ncol)),NA)
      expect_equal(tempObject$mat,object$mat[,c(1,object$ncol),drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
    }

    tempObject = object$clone(TRUE)
    expect_error(tempObject$subset(rows=1,cols = 1),NA)
    expect_equal(tempObject$mat,object$mat[1,1,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(rows= 1,cols = c(1,tempObject$ncol)),NA)
      expect_equal(tempObject$mat,object$mat[1,c(1,object$ncol),drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
    }

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(rows= c(1,tempObject$nrow,cols = 1)),NA)
      expect_equal(tempObject$mat,object$mat[c(1,object$nrow),1,drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
      if(tempObject$ncol > 1){
        tempObject = object$clone(TRUE)
        expect_error(tempObject$subset(rows= c(1,tempObject$nrow),cols = c(1,tempObject$ncol)),NA)
        expect_equal(tempObject$mat,object$mat[c(1,object$nrow),c(1,object$ncol),drop=FALSE])
        expect_equal(rownames(tempObject$mat),tempObject$rnames)
        expect_equal(nrow(tempObject$mat),tempObject$nrow)
        expect_equal(colnames(tempObject$mat),tempObject$cnames)
        expect_equal(ncol(tempObject$mat),tempObject$ncol)
      }
    }

    expect_equal(object,firstObject)
  })


  test_that(paste(name,": lead respects mat"),{
    if(!lead_runs){
      skip("Skipping additional lead tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_equal({
      tempObject$lead(c(0))
      tempObject$mat
    },object$mat,check.names=FALSE,check.attributes=FALSE)

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_equal({
        tempObject$lead(c(1))
        tempObject$mat[,-ncol(tempObject$mat)]
      },object$mat[,-1],check.attributes=FALSE,check.names=FALSE)

      tempObject = object$clone(TRUE)
      expect_equal({
        tempObject$lead(c(0,1))
        tempObject$mat[,-ncol(tempObject$mat),drop=FALSE]
      },rbind(object$mat[,-ncol(tempObject$mat),drop=FALSE],object$mat[,-1,drop=FALSE]),check.attributes=FALSE,check.names=FALSE)
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": addColumns respects mat"),{
    if(!addColumns_runs){
      skip("Skipping additional addColumns tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_equal({
      tempObject$addColumns(0)
      tempObject$mat
    },object$mat)

    tempObject = object$clone(TRUE)
    expect_equivalent({
      tempObject$addColumns(2)
      tempObject$mat
    },cbind(object$mat,matrix(NA,object$nrow,2)))

    expect_equal(object,firstObject)
  })


  test_that(paste(name,": addRows respects mat"),{
    if(!addRows_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_equal({
      tempObject$addRows(0)
      tempObject$mat
    },object$mat)

    tempObject = object$clone(TRUE)
    expect_equivalent({
      tempObject$addRows(2)
      tempObject$mat
    },rbind(object$mat,matrix(NA,2,object$ncol)))

    expect_equal(object,firstObject)
  })


  test_that(paste(name,": mutate respects mat"),{
    if(!mutate_runs){
      skip("Skipping additional addRows tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_equivalent({
      tempObject$mutate(data=NA)
      tempObject$mat
    },matrix(NA,nrow(object$mat),ncol(object$mat)))

    tempObject = object$clone(TRUE)
    expect_equivalent({
      tempObject$mutate(rows=1,data=NA)
      tempObject$mat
    },rbind(matrix(NA,1,object$ncol),object$mat[-1,]))

    tempObject = object$clone(TRUE)
    expect_equivalent({
      dataMat = matrix(NA,1,tempObject$ncol)
      rownames(dataMat) = as.character(1)
      colnames(dataMat) = as.character(1:tempObject$ncol)
      tempObject$mutate(rows=1,data=dataMat)
      tempObject$mat
    },rbind(dataMat,object$mat[-1,]))

    tempObject = object$clone(TRUE)
    expect_equivalent({
      tempObject$mutate(cols=1,data=NA)
      tempObject$mat
    },cbind(matrix(NA,object$nrow,1),object$mat[,-1,drop=FALSE]))

    #Fix this
    tempObject=object$clone(TRUE)
    expect_equivalent({
      tempObject$mutate(rows=1,cols=1,data=NA)
      tempObject$mat[-1]
    },object$mat[-1])

    tempObject=object$clone(TRUE)
    expect_equal({
      tempObject$mutate(rows=1,cols=1,data=NA)
      is.na(tempObject$mat[1])
    },TRUE)

    tempObject = object$clone(TRUE)
    expect_equivalent({
      dataMat = matrix(NA,tempObject$nrow,1)
      rownames(dataMat) = as.character(1:tempObject$nrow)
      colnames(dataMat) = as.character(1)
      tempObject$mutate(cols=1,data=dataMat)
      tempObject$mat
    },cbind(matrix(NA,object$nrow,1),object$mat[,-1,drop=FALSE]))

    expect_equal(object,firstObject)
  })

  test_that(paste(name,": diff works"),{
    first_object = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$diff(0),NA)

    tempObject = object$clone(TRUE)
    if(tempObject$ncol <= 1){
      skip("Not enough columns to test diff.")
    }
    expect_error(tempObject$diff(1),NA)
    expect_equal(object,first_object)
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$diff(0)
    diff_runs=TRUE
  },silent = TRUE)

  test_that(paste(name,": diff respects ncol"),{
    if(!diff_runs){
      skip("Skipping additional diff tests.")
    }
    first_object = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_error(tempObject$diff(0),NA)
    expect_equivalent(tempObject$ncol,object$ncol)
    if(tempObject$ncol > 1){
      expect_error(tempObject$diff(1),NA)
      expect_equivalent(tempObject$ncol,object$ncol)
    }

    expect_equal(object,first_object)
  })

  test_that(paste(name,": diff respects nrow"),{
    if(!diff_runs){
      skip("Skipping additional diff tests.")
    }
    first_object = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_error(tempObject$diff(0),NA)
    expect_equivalent(tempObject$nrow,object$nrow)
    if(tempObject$ncol > 1){
      expect_error(tempObject$diff(1),NA)
      expect_equivalent(tempObject$nrow,object$nrow)
    }

    expect_equal(object,first_object)
  })

  test_that(paste(name,": diff respects rnames"),{
    if(!diff_runs){
      skip("Skipping additional diff tests.")
    }
    first_object = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    if(is.null(object$rnames)){
      skip("No row names to test on.")
    }

    expect_error(tempObject$diff(0),NA)
    expect_equal(tempObject$rnames,paste("D0R",object$rnames,sep=''))
    tempObject = object$clone(TRUE)
    if(tempObject$ncol > 1){
      expect_error(tempObject$diff(1),NA)
      expect_equal(tempObject$rnames,paste("D1R",object$rnames,sep=''))
    }

    expect_equal(object,first_object)
  })

  ##Note this assumes that diff does not remove NAed out columns
  test_that(paste(name,": diff respects cnames"),{
    if(!diff_runs){
      skip("Skipping additional diff tests.")
    }
    first_object = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    if(is.null(object$rnames)){
      skip("No column names to test on.")
    }

    expect_error(tempObject$diff(0),NA)
    expect_equal(tempObject$cnames,object$cnames)
  ##expect_equivalent(tempObject$cnames,object$cnames)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol > 1){
      expect_error(tempObject$diff(1),NA)
      expect_equal(tempObject$cnames,object$cnames)
  ##  expect_equivalent(tempObject$cnames,object$cnames)
    }

    expect_equal(object,first_object)
  })

  test_that(paste(name,": diff respects colData"),{
    if(!diff_runs){
      skip("Skipping additional diff tests.")
    }
    first_object = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_error(tempObject$diff(0),NA)
    expect_equal(tempObject$colData,object$colData)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol > 1){
      expect_error(tempObject$diff(1),NA)
      expect_equal(tempObject$colData,object$colData)
    }

    expect_equal(object,first_object)
  })

  test_that(paste(name,": diff respects mat"),{
    if(!diff_runs){
      skip("Skipping additional diff tests.")
    }
    first_object = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_error(tempObject$diff(0),NA)
    expect_equal(tempObject$mat,object$mat,check.names=FALSE,check.attributes=FALSE)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol > 1){
      expect_error(tempObject$diff(1),NA)
      expect_equal(
        tempObject$mat[,1,drop=FALSE],
        matrix(as.numeric(NA),object$nrow,1),
        check.names=FALSE,
        check.attributes=FALSE
      )
      expect_equal(tempObject$nrow,nrow(object$mat))
      expect_equal(tempObject$ncol,ncol(object$mat))
      expect_equal(
        tempObject$mat[,2:tempObject$ncol,drop=FALSE],
        object$mat[,2:object$ncol,drop=FALSE]- object$mat[,1:(object$ncol-1),drop=FALSE],
        check.names=FALSE,
        check.attributes=FALSE
      )
      expect_equal(tempObject$metaData,object$metaData)
    }

    expect_equal(object,first_object)
  })

  test_that(paste(name,": diff respects metaData"),{
    if(!diff_runs){
      skip("Skipping additional diff tests.")
    }
    first_object = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_error(tempObject$diff(0),NA)
    expect_equal(tempObject$metaData,object$metaData)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol > 1){
      expect_error(tempObject$diff(1),NA)
      expect_equal(tempObject$metaData,object$metaData)
    }

    expect_equal(object,first_object)
  })

  test_that(paste(name,": diff respects rowData"),{
    if(!diff_runs){
      skip("Skipping additional diff tests.")
    }
    first_object = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_error(tempObject$diff(0),NA)
    expect_equal(tempObject$rowData,object$rowData)
    tempObject = object$clone(TRUE)
    if(tempObject$ncol > 1){
      expect_error(tempObject$diff(1),NA)
      expect_equal(tempObject$rowData,object$rowData)
    }

    expect_equal(object,first_object)
  })

  test_that(paste(name,": head works"),{

    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1),NA)
    #Dimensions
    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,1),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,2),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,3))
    #span
    tempObject = object$clone(TRUE)
    rows = tempObject$nrow
    cols = tempObject$ncol
    expect_that({
      tempObject$head(rows,1)
      tempObject
    },equivalence(object))
    expect_that({
      tempObject$head(cols,2)
      tempObject
    },equivalence(object))
    expect_error(tempObject$head(rows+1,1))
    expect_error(tempObject$head(cols+1,2))
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$head(1,1)
    head_runs = TRUE
  },silent=TRUE)

  test_that(paste(name,": head respects nrow"),{
    if(!head_runs){
      skip("Skipping additional head tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,1),NA)
    expect_equal(tempObject$nrow,1)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,2),NA)
    expect_equal(tempObject$nrow,object$nrow)

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$head(2,1),NA)
      expect_equal(tempObject$nrow,2)
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": head respects metaData"),{
    if(!head_runs){
      skip("Skipping additional head tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,1),NA)
    expect_equal(tempObject$metaData,object$metaData)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,2),NA)
    expect_equal(tempObject$metaData,object$metaData)

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$head(2,1),NA)
      expect_equal(tempObject$metaData,object$metaData)
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": head respects ncol"),{
    if(!head_runs){
      skip("Skipping additional head tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(k=1,direction=1),NA)
    expect_equal(tempObject$ncol,object$ncol)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(k=1,direction=2),NA)
    expect_equal(tempObject$ncol,1)

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$head(k=2,direction=2),NA)
      expect_equal(tempObject$ncol,2)
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": head respects rnames"),{
    if(!head_runs){
      skip("Skipping additional head tests.")
    }
    if(is.null(object$rnames)){
      skip("No row names to test on.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(k=1,direction=1),NA)
    expect_equal(tempObject$rnames,object$rnames[1])

    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(k=1,direction=2),NA)
    expect_equal(tempObject$rnames,object$rnames)

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$head(k=2,direction=1),NA)
      expect_equal(tempObject$rnames,object$rnames[1:2])
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": head respects cnames"),{
    if(!head_runs){
      skip("Skipping additional head tests.")
    }
    if(is.null(object$cnames)){
      skip("No column names to test on.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,1),NA)
    expect_equal(tempObject$cnames,object$cnames)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,2),NA)
    expect_equal(tempObject$cnames,object$cnames[1])

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$head(2,2),NA)
      expect_equal(tempObject$cnames,object$cnames[1:2])
    }

    expect_that(object,equivalence(firstObject))
  })

  test_that(paste(name,": head respects rowData"),{
    if(!head_runs){
      skip("Skipping additional head tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,1),NA)
    expect_equal(tempObject$rowData,lapply(object$rowData,function(x){x[1]}))

    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,2),NA)
    expect_equal(tempObject$rowData,object$rowData)

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$subset(2,1),NA)
      expect_equal(tempObject$rowData,lapply(object$rowData,function(x){x[1:2]}))
    }
  })

  test_that(paste(name,": head respects colData"),{
    if(!head_runs){
      skip("Skipping additional head tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,1),NA)
    expect_equal(tempObject$colData,object$colData)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,2),NA)
    expect_equal(tempObject$colData,lapply(object$colData,function(x){x[1]}))

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$head(2,2),NA)
      expect_equal(tempObject$colData,lapply(object$colData,function(x){x[1:2]}))
    }
  })

  test_that(paste(name,": head respects mat"),{
    if(!head_runs){
      skip("Skipping additional head tests.")
    }
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)

    expect_error(tempObject$head(1,1),NA)
    expect_equal(tempObject$mat,object$mat[1,,drop=FALSE ])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$head(1,2),NA)
    expect_equal(tempObject$mat,object$mat[,1,drop=FALSE])
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)

    if(tempObject$nrow > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$head(2,1),NA)
      expect_equal(tempObject$mat,object$mat[1:2,,drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
    }

    if(tempObject$ncol > 1){
      tempObject = object$clone(TRUE)
      expect_error(tempObject$head(2,2),NA)
      expect_equal(tempObject$mat,object$mat[,1:2,drop=FALSE])
      expect_equal(rownames(tempObject$mat),tempObject$rnames)
      expect_equal(nrow(tempObject$mat),tempObject$nrow)
      expect_equal(colnames(tempObject$mat),tempObject$cnames)
      expect_equal(ncol(tempObject$mat),tempObject$ncol)
    }

    expect_equal(object,firstObject)
  })

  test_that(paste(name,": scale works"),{
    first_object = object$clone(TRUE)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){x}),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){log(1+x)}),NA)
    expect_error(tempObject$scale(function(x){exp(x)-1}),NA)
    expect_that(tempObject,equivalence(object))
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){sqrt(x)}),NA)
    expect_error(tempObject$scale(function(x){x^2}),NA)
    expect_that(tempObject,equivalence(object))

    expect_equal(first_object,object)
  })

  try({
    tempObject = object$clone(TRUE)
    tempObject$scale(function(x){x})
    scale_runs=TRUE
  },silent = TRUE)

  test_that(paste(name,": scale respects nrow"),{
    first_object = object$clone(TRUE)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){x}),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){log(1+x)}),NA)
    expect_equal(tempObject$nrow,object$nrow)
    expect_error(tempObject$scale(function(x){exp(x)-1}),NA)
    expect_equal(tempObject$nrow,object$nrow)
    expect_that(tempObject,equivalence(object))
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){sqrt(x)}),NA)
    expect_equal(tempObject$nrow,object$nrow)
    expect_error(tempObject$scale(function(x){x^2}),NA)
    expect_equal(tempObject$nrow,object$nrow)
    expect_that(tempObject,equivalence(object))

    expect_equal(first_object,object)
  })

  test_that(paste(name,": scale respects ncol"),{
    first_object = object$clone(TRUE)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){x}),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){log(1+x)}),NA)
    expect_equal(tempObject$ncol,object$ncol)
    expect_error(tempObject$scale(function(x){exp(x)-1}),NA)
    expect_equal(tempObject$ncol,object$ncol)
    expect_that(tempObject,equivalence(object))
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){sqrt(x)}),NA)
    expect_equal(tempObject$ncol,object$ncol)
    expect_error(tempObject$scale(function(x){x^2}),NA)
    expect_equal(tempObject$ncol,object$ncol)
    expect_that(tempObject,equivalence(object))

    expect_equal(first_object,object)
  })

  test_that(paste(name,": scale respects cnames"),{
    first_object = object$clone(TRUE)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){x}),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){log(1+x)}),NA)
    expect_equal(tempObject$cnames,object$cnames)
    expect_error(tempObject$scale(function(x){exp(x)-1}),NA)
    expect_equal(tempObject$cnames,object$cnames)
    expect_that(tempObject,equivalence(object))
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){sqrt(x)}),NA)
    expect_equal(tempObject$cnames,object$cnames)
    expect_error(tempObject$scale(function(x){x^2}),NA)
    expect_equal(tempObject$cnames,object$cnames)
    expect_that(tempObject,equivalence(object))

    expect_equal(first_object,object)
  })

  test_that(paste(name,": scale respects rnames"),{
    first_object = object$clone(TRUE)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){x}),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){log(1+x)}),NA)
    expect_equal(tempObject$rnames,object$rnames)
    expect_error(tempObject$scale(function(x){exp(x)-1}),NA)
    expect_equal(tempObject$rnames,object$rnames)
    expect_that(tempObject,equivalence(object))
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){sqrt(x)}),NA)
    expect_equal(tempObject$rnames,object$rnames)
    expect_error(tempObject$scale(function(x){x^2}),NA)
    expect_equal(tempObject$rnames,object$rnames)
    expect_that(tempObject,equivalence(object))

    expect_equal(first_object,object)
  })

  test_that(paste(name,": scale respects colData"),{
    first_object = object$clone(TRUE)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){x}),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){log(1+x)}),NA)
    expect_equal(tempObject$colData,object$colData)
    expect_error(tempObject$scale(function(x){exp(x)-1}),NA)
    expect_equal(tempObject$colData,object$colData)
    expect_that(tempObject,equivalence(object))
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){sqrt(x)}),NA)
    expect_equal(tempObject$colData,object$colData)
    expect_error(tempObject$scale(function(x){x^2}),NA)
    expect_equal(tempObject$colData,object$colData)
    expect_that(tempObject,equivalence(object))

    expect_equal(first_object,object)
  })

  test_that(paste(name,": scale respects rowData"),{
    first_object = object$clone(TRUE)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){x}),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){log(1+x)}),NA)
    expect_equal(tempObject$rowData,object$rowData)
    expect_error(tempObject$scale(function(x){exp(x)-1}),NA)
    expect_equal(tempObject$rowData,object$rowData)
    expect_that(tempObject,equivalence(object))
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){sqrt(x)}),NA)
    expect_equal(tempObject$rowData,object$rowData)
    expect_error(tempObject$scale(function(x){x^2}),NA)
    expect_equal(tempObject$rowData,object$rowData)
    expect_that(tempObject,equivalence(object))

    expect_equal(first_object,object)
  })

  test_that(paste(name,": scale respects metaData"),{
    first_object = object$clone(TRUE)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){x}),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){log(1+x)}),NA)
    expect_equal(tempObject$metaData,object$metaData)
    expect_error(tempObject$scale(function(x){exp(x)-1}),NA)
    expect_equal(tempObject$metaData,object$metaData)
    expect_that(tempObject,equivalence(object))
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){sqrt(x)}),NA)
    expect_equal(tempObject$metaData,object$metaData)
    expect_error(tempObject$scale(function(x){x^2}),NA)
    expect_equal(tempObject$metaData,object$metaData)
    expect_that(tempObject,equivalence(object))

    expect_equal(first_object,object)
  })

  test_that(paste(name,": scale respects mat"),{
    first_object = object$clone(TRUE)

    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){x}),NA)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){log(1+x)}),NA)
    expect_equal(tempObject$mat,log(1+object$mat))
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){exp(x)-1}),NA)
    expect_equal(tempObject$mat,exp(object$mat)-1)
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){sqrt(x)}),NA)
    expect_equal(tempObject$mat,sqrt(object$mat))
    tempObject = object$clone(TRUE)
    expect_error(tempObject$scale(function(x){x^2}),NA)
    expect_equal(tempObject$mat,object$mat * object$mat)

    expect_equal(first_object,object)
  })

  return(c(subset_runs,tail_runs,lag_runs,addRows_runs,addColumns_runs,mutate_runs))
}
