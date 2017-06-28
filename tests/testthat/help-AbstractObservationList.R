################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####Forecast Test Functions
################################################################################

#######################################TODO#####################################

source('help-FrameData.R')

is_same_AbsractObservationList_as = function(rhs){
  return(function(lhs){
    is_same_FrameData_as(rhs)(lhs)
  })
}

test_AbstractObservationList = function(object,name,equivalence){
  if(!('MatrixData' %in% class(object))){
    warning("Object is not of the correct")
  }

  first_object = object$clone(TRUE)
  test_FrameData(object,name,equivalence)

  test_that(paste(name,": Inherited Methods don't change object"),{
    equivalence(first_object)(object)
  })

}
