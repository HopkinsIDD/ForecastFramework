################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
#####Array Meta Data Container Test Functions
################################################################################

#######################################TODO#####################################
#' Test for mutate=FALSE

is.prime <- function(pn){

  if(sum(pn/1:sqrt(pn)==pn%/%1:sqrt(pn))==1)
    return(TRUE)
  return(FALSE)
}

greatest.small.factor <- function(pn){
  return(max(which(pn/1:sqrt(pn)==pn%/%1:sqrt(pn))))
}

source('help-MatrixData.R')

is_same_ArrayData_as = function(rhs){
  return(function(lhs){
    is_same_MatrixData_as(rhs)(lhs)
    equals(rhs$arr)(lhs$arr)
    equals(rhs$dimData)(lhs$dimData)
    equals(rhs$dnames)(lhs$dnames)
    equals(rhs$ndim)(lhs$ndim)
    equals(rhs$dims)(lhs$dims)
  })
}

test_ArrayData = function(object,name,equivalence){
  if(!('ArrayData' %in% class(object))){
    warning("Object is not an ArrayData.")
  }
  first_object = object$clone(TRUE)

  test_MatrixData(object,name,equivalence)
  test_that(paste(name,": Inherited Methods don't change object"),{
    equivalence(first_object)(object)
  })

  test_that(paste(name,": Active bindings agree with functions of array"),{
    tempObject=object$clone(TRUE)
    expect_equal(length(dim(tempObject$arr)),tempObject$ndim)
    expect_equal(dim(tempObject$arr),tempObject$dims)
    expect_equal(rownames(tempObject$arr),tempObject$rnames)
    expect_equal(nrow(tempObject$arr),tempObject$nrow)
    expect_equal(colnames(tempObject$arr),tempObject$cnames)
    expect_equal(ncol(tempObject$arr),tempObject$ncol)
    expect_equal(dimnames(tempObject$arr),tempObject$dnames)
    expect_that(object,equivalence(tempObject))
    expect_that(object,equivalence(first_object))
  })

  test_that(paste(name,": matrix and array bindings agree."),{
    tempObject=object$clone(TRUE)
    expect_gte(tempObject$ndim,2)
    expect_equal(tempObject$dims[[1]],tempObject$nrow)
    expect_equal(tempObject$dims[[2]],tempObject$ncol)
    expect_equal(tempObject$dnames[[1]],tempObject$rnames)
    expect_equal(tempObject$dnames[[2]],tempObject$cnames)
    expect_equal(tempObject$dimData[[1]],tempObject$rowData)
    expect_equal(tempObject$dimData[[2]],tempObject$colData)
    expect_that(object,equivalence(tempObject))
    expect_that(object,equivalence(first_object))
  })

  test_that(paste(name,": dim metaData is settable"),{
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error({tempObject$dimData <- lapply(dim(tempObject$arr),function(x){list(rep(NA,x))})},NA)
    tempObject = object$clone(TRUE)
    expect_error({tempObject$dimData[[1]] <- list(rep(list(1),tempObject$nrow))},NA)
    expect_equal(tempObject$dimData[[1]],list(rep(list(1),tempObject$nrow)))
    expect_equal(tempObject$rowData,list(rep(list(1),tempObject$nrow)))
    tempObject = object$clone(TRUE)
    expect_error({tempObject$dimData[[2]] <- list(rep(list(1),tempObject$ncol))},NA)
    expect_equal(tempObject$dimData[[2]],list(rep(list(1),tempObject$ncol)))
    tempObject = object$clone(TRUE)
    expect_error({tempObject$dimData <- lapply(dim(tempObject$arr),function(x){list(rep(NA,x+1))})})
    expect_that(tempObject,equivalence(object))
    tempObject = object$clone(TRUE)
    expect_error({tempObject$dimData <- lapply(dim(tempObject$arr),function(x){list(rep(NA,x-1))})})
    expect_that(tempObject,equivalence(object))
    for(i in 1:length(dim(tempObject$arr))){
      tempObject = object$clone(TRUE)
      expect_error({tempObject$dimData[[i]] = list(rep(NA,tempObject$dims[[i]]+1))})
      expect_that(tempObject,equivalence(object))
      tempObject = object$clone(TRUE)
      expect_error({tempObject$dimData[[i]] = list(rep(NA,tempObject$dims[[i]]-1))})
      expect_that(tempObject,equivalence(object))
    }
    tempObject = object$clone(TRUE)
    expect_error({tempObject$dimData = list()},NA)
    expect_equivalent(tempObject$dimData,list())
    tempObject = object$clone(TRUE)
    expect_error({tempObject$dimData = lapply(tempObject$dims,function(x){list()})},NA)
    for(i in 1:length(dim(tempObject$arr))){
      expect_equivalent(tempObject$dimData[[i]],list())
    }
    for(i in 1:length(dim(tempObject$arr))){
      tempObject = object$clone(TRUE)
      expect_error({tempObject$dimData[[i]] = list()},NA)
      expect_equivalent(tempObject$dimData[[i]],list())
    }
    expect_that(object,equivalence(firstObject))
  })
}
