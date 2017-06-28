################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####Meta Data Container Test Functions
################################################################################

#######################################TODO#####################################
                                        #TODO: Debug lag respects rowData test


is_same_DataContainer_as = function(rhs){
  return(function(lhs){
    equals(lhs$metaData)(rhs$metaData)
  })
}

test_DataContainer = function(object,name,equivalence){
  if(!('DataContainer' %in% class(object))){
    warning("Object is not of the correct")
  }
  test_that(paste(name,"global metaData is settable"),{
    firstObject = object$clone(TRUE)
    tempObject = object$clone(TRUE)
    expect_error({tempObject$metaData = list('test string')},NA)
    expect_equal({tempObject$metaData},list('test string'))
    expect_error({tempObject$metaData = list()},NA)
    expect_equal({tempObject$metaData},list())
    expect_that(tempObject,equivalence(object))
    expect_that(object,equivalence(firstObject))
  })
}
