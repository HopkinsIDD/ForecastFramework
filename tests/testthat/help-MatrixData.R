################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####Matrix Meta Data Container Test Functions
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

source('help-DataContainer.R')

is_same_MatrixData_as = function(rhs){
  return(function(lhs){
    is_same_DataContainer_as(rhs)(lhs)
    equals(rhs$nrow)(lhs$nrow)
    equals(rhs$ncol)(lhs$ncol)
    equals(rhs$rowData)(lhs$rowData)
    equals(rhs$colData)(lhs$colData)
    equals(rhs$mat)(lhs$mat)
  })
}

test_MatrixData = function(object,name,equivalence){
  if(!('MatrixData' %in% class(object))){
    warning("Object is not of the correct")
  }
  first_object = object$clone(TRUE)

  test_DataContainer(object,name,equivalence)
  test_that(paste(name,": Inherited Methods don't change object"),{
    equivalence(first_object)(object)
  })

  test_that(paste(name,": Active bindings have correct mode."),{
    first_object=object$clone(TRUE)

    tempOjbect=object$clone(TRUE)
    expect_equal(mode(tempOjbect$mat),'numeric')
    tempOjbect=object$clone(TRUE)
    expect_equal(mode(tempOjbect$nrow),'numeric')
    tempOjbect=object$clone(TRUE)
    expect_equal(mode(tempOjbect$ncol),'numeric')
    tempOjbect=object$clone(TRUE)
    expect_that(mode(tempOjbect$rnames),function(x){x %in% c('NULL','character')})
    tempOjbect=object$clone(TRUE)
    expect_that(mode(tempOjbect$cnames),function(x){x %in% c('NULL','character')})

    expect_equal(first_object,object)
  })

  test_that(paste(name,": Active bindings agree with functions of matrix"),{
    tempObject=object$clone(TRUE)
    expect_equal(rownames(tempObject$mat),tempObject$rnames)
    expect_equal(nrow(tempObject$mat),tempObject$nrow)
    expect_equal(colnames(tempObject$mat),tempObject$cnames)
    expect_equal(ncol(tempObject$mat),tempObject$ncol)
    expect_that(object,equivalence(tempObject))
    expect_that(object,equivalence(first_object))
  })

  test_that(paste(name,": row metaData is settable"),{
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error({tempObject$rowData = list(rep(list(1),tempObject$nrow))},NA)
    expect_equivalent({tempObject$rowData[[1]]},rep(list(1),tempObject$nrow))
    expect_error({tempObject$rowData = list()},NA)
    expect_equivalent({tempObject$rowData},list())
    expect_that(object,equivalence(firstObject))

    expect_error({tempObject$rowData = list(rep(list(1),tempObject$nrow+1))})
    expect_error({tempObject$rowData = list(rep(list(1),tempObject$nrow-1))})
  })

  test_that(paste(name,": col metaData is settable"),{
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error({tempObject$colData = list(rep(list(1),tempObject$ncol))},NA)
    expect_equivalent({tempObject$colData[[1]]},rep(list(1),tempObject$ncol))
    expect_error({tempObject$colData = list()},NA)
    expect_equivalent({tempObject$colData},list())
    expect_that(object,equivalence(firstObject))

    expect_error({tempObject$colData = list(rep(list(1),tempObject$ncol+1))})
    expect_error({tempObject$colData = list(rep(list(1),tempObject$ncol-1))})
  })

  ## On hold for the initial release
  # test_that(paste(name,": cell metaData is settable"),{
  #   firstObject = object$clone(TRUE)
  #   tempObject = object$clone(TRUE)
  #   expect_error({tempObject$cellData = list(matrix(NA,tempObject$nrow,tempObject$ncol))},NA)
  #   expect_equivalent({tempObject$cellData[[1]]},matrix(NA,tempObject$nrow,tempObject$ncol))
  #   expect_error({tempObject$cellData = list()},NA)
  #   expect_equivalent({tempObject$cellData},list())
  #   expect_that(object,equivalence(firstObject))
  #
  #   #Fill this in
  #   expect_error({tempObject$cellData = list(matrix(1,tempObject$nrow,tempObject$ncol+1))})
  #   expect_error({tempObject$cellData = list(matrix(1,tempObject$nrow,tempObject$ncol-1))})
  #   expect_error({tempObject$cellData = list(matrix(1,tempObject$nrow+1,tempObject$ncol))})
  #   expect_error({tempObject$cellData = list(matrix(1,tempObject$nrow-1,tempObject$ncol))})
  #   if(!is.prime(tempObject$nrow)){
  #     i = greatest.small.factor(tempObject$nrow)
  #     expect_error({tempObject$cellData = list(matrix(1,tempObject$nrow/i,tempObject$ncol*i))})
  #   }
  #   if(!is.prime(tempObject$ncol)){
  #     i = greatest.small.factor(tempObject$nrow)
  #     expect_error({tempObject$cellData = list(matrix(1,tempObject$nrow*i,tempObject$ncol/i))})
  #   }
  # })

}
