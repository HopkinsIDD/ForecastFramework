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
  if(!('FrameData' %in% class(object))){
    warning("Object is not of the correct class")
  }

  first_object = object$clone(TRUE)
  test_FrameData(object,name,equivalence)

  test_that(paste(name,": Inherited Methods don't change object"),{
    equivalence(first_object)(object)
  })
  
  test_that(paste(name,"formArray works"),{
    if(ncol(object$frame) < 3){
      skip("frame needs at least 3 columns to form array")
    }
    tempObject = object$clone(TRUE)
    expect_error({
      ## Forming array from first two columns
      rname = names(tempObject$frame)[1]
      cname = names(tempObject$frame)[2]
      vname = names(tempObject$frame)[2]
      tempObject$formArray(rname,cname,val=vname)
      IncidenceMatrix$new(tempObject)
    },NA)
  })
}
