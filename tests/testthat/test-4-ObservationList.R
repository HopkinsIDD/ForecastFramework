#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#-----------------------Infectious Disease Forecasting-------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
####IncidenceMatrix Test Functions
#------------------------------------------------------------------------------#

#Write test function for initialize

context("Observation List")

rm(list =ls())
test_that('test functions load',{
  source('help-AbstractObservationList.R')
})

is_same_ObservationList_as = function(rhs){
  return(function(lhs){
    equals(rhs$mat)(lhs$mat)
    is_same_AbstractIncidenceMatrix_as(rhs)(lhs)
  })
}


#test_IncidenceMatrix_initialization()

testObservationList = ObservationList$new(tibble(row=1,col=1,val=NA))
testObservationList$formArray('row','col',val='val')
testObservationList$mat
test_AbstractObservationList(testObservationList,"Empty ObservationList",is_same_ObservationList_as)
testObservationList = ObservationList$new(
  tibble(x=letters[((1:1000) %% 26)+1],y=((1:1000)*51) %% 7,val=1:1000*0+1,w=1:1000 %% 26),'x','y',val='val',dimData = list(list('w'),NULL)
)
testObservationList$formArray('x','y',val='val')
testObservationList$mat
test_that("rownames successfully assigned",{
  rn = testObservationList$rnames
  cn = testObservationList$cnames
  expect_that(is.null(rn) && is.null(cn),equals(FALSE))
})
test_AbstractObservationList(testObservationList,"Full ObservationList",is_same_ObservationList_as)
