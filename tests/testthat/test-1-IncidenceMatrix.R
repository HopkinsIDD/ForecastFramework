################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####IncidenceMatrix Test Functions
################################################################################

#######################################TODO#####################################

context('Incidence Matrix')

rm(list =ls())
test_that('test functions load',{
  source('help-AbstractIncidenceMatrix.R')
})

##test of initialization method.

test_that('IncidenceMatrix initialize works',{
  expect_error(IncidenceMatrix$new(),NA)
  expect_error(IncidenceMatrix$new(data = matrix()),NA)
  expect_equivalent(IncidenceMatrix$new(data=matrix())$mat,matrix())
  ##expect_error(IncidenceMatrix$new(data.frame(),NA)
  ##expect_equal(IncidenceMatrix$new(data=data.frame())$mat,matrix())
  ##expect_error(IncidenceMatrix$new(tibble(),NA)
  ##expect_equal(IncidenceMatrix$new(data=tibble())$mat,matrix())
  expect_error(IncidenceMatrix$new(data=matrix(),rowData=list(),colData=list(),metaData=list()),NA)
  expect_equivalent(IncidenceMatrix$new(data=matrix(),rowData=list(),colData=list(),metaData=list())$mat,matrix())
  expect_equal(IncidenceMatrix$new(data=matrix(),rowData=list(),colData=list(),metaData=list())$rowData,list())
  expect_equal(IncidenceMatrix$new(data=matrix(),rowData=list(),colData=list(),metaData=list())$colData,list())
  expect_equal(IncidenceMatrix$new(data=matrix(),rowData=list(),colData=list(),metaData=list())$metaData,list())
  expect_error(
    IncidenceMatrix$new(
      data=matrix(1:35,7,5),
      rowData=list(a=1:7,b=list(1,2,3,4,5,6,7),c=letters[1:7]),
      colData=list(a=1:5,b=list(1,2,3,4,5),c=letters[1:5]),
      metaData=list('fish',a=5*3,b=1:10,d=matrix())
    ),
    NA
  )
  expect_equal(
    IncidenceMatrix$new(
      data=matrix(1:35,7,5),
      rowData=list(a=1:7,b=list(1,2,3,4,5,6,7),c=letters[1:7]),
      colData=list(a=1:5,b=list(1,2,3,4,5),c=letters[1:5]),
      metaData=list('fish',a=5*3,b=1:10,d=matrix())
    )$mat,
    matrix(1:35,7,5)
  )
  expect_equal(
    IncidenceMatrix$new(
      data=matrix(1:35,7,5),
      rowData=list(a=1:7,b=list(1,2,3,4,5,6,7),c=letters[1:7]),
      colData=list(a=1:5,b=list(1,2,3,4,5),c=letters[1:5]),
      metaData=list('fish',a=5*3,b=1:10,d=matrix())
    )$rowData,
    list(a=1:7,b=list(1,2,3,4,5,6,7),c=letters[1:7])
  )
  expect_equal(
    IncidenceMatrix$new(
      data=matrix(1:35,7,5),
      rowData=list(a=1:7,b=list(1,2,3,4,5,6,7),c=letters[1:7]),
      colData=list(a=1:5,b=list(1,2,3,4,5),c=letters[1:5]),
      metaData=list('fish',a=5*3,b=1:10,d=matrix())
    )$colData,
    list(a=1:5,b=list(1,2,3,4,5),c=letters[1:5])
  )
  expect_equal(
    IncidenceMatrix$new(
      data=matrix(1:35,7,5),
      rowData=list(a=1:7,b=list(1,2,3,4,5,6,7),c=letters[1:7]),
      colData=list(a=1:5,b=list(1,2,3,4,5),c=letters[1:5]),
      metaData=list('fish',a=5*3,b=1:10,d=matrix())
    )$metaData,
    list('fish',a=5*3,b=1:10,d=matrix())
  )

})


## Actual tests
testIncidenceMatrix = IncidenceMatrix$new()
test_AbstractIncidenceMatrix(testIncidenceMatrix,"Empty IncidenceMatrix",is_same_AbstractIncidenceMatrix_as)
testIncidenceMatrix = IncidenceMatrix$new(
  tibble(x=1:10,y=1:10),
  rowData=list(
    letters[1:10],
    letters[5:14],
    list(1,2,3,4,5,6,7,8,9,10),
    list(list(1,2),list(2,3),list(3,4),list(4,5),list(5,6),list(6,7),list(7,8),list(8,9),list(9,10),list(10,11))
  ),
  colData=list(w=1:2,z=list(3,4))
)
test_that("rownames successfully assigned",{
  rn = testIncidenceMatrix$rnames
  cn = testIncidenceMatrix$cnames
  expect_that(is.null(rn) && is.null(cn),equals(FALSE))
})
test_AbstractIncidenceMatrix(testIncidenceMatrix,"Full IncidenceMatrix",is_same_AbstractIncidenceMatrix_as)
