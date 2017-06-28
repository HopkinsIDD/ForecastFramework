################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####SimulatedIncidenceMatrix Test Functions
################################################################################

context("Simulated Incidence Matrix")

rm(list =ls())
test_that('test functions load',{
  source('help-AbstractSimulatedIncidenceMatrix.R')
})

##test_SimulatedIncidenceMatrix_initialization()
test_that('IncidenceMatrix initialize works',{
  expect_error(SimulatedIncidenceMatrix$new(),NA)
  dataMat = matrix(1:200,20,10)
  rowData=list(a = letters[1:20])
  colData=list(a = 1:10)
  metaData=list(10,4,matrix(1:10))
  nsim = 3
  dataArr = array(dataMat,c(nrow(dataMat),ncol(dataMat),nsim))
  expect_error(SimulatedIncidenceMatrix$new(
  ),NA)

  expect_error(SimulatedIncidenceMatrix$new(
    data = IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
  ),NA)
  expect_equal(SimulatedIncidenceMatrix$new(
    data = IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
  )$mat,dataMat)
  expect_equal(SimulatedIncidenceMatrix$new(
    data = IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
  )$arr,dataArr)
  expect_equal(SimulatedIncidenceMatrix$new(
    data = IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
  )$rowData,rowData)
  expect_equal(SimulatedIncidenceMatrix$new(
    data = IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
  )$colData,colData)
  expect_equal(SimulatedIncidenceMatrix$new(
    data = IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
  )$metaData,metaData)

  expect_error(SimulatedIncidenceMatrix$new(
    data = SimulatedIncidenceMatrix$new(IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
    )),NA)
  expect_equal(SimulatedIncidenceMatrix$new(
    data = SimulatedIncidenceMatrix$new(IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
    ))$mat,dataMat)
  expect_equal(SimulatedIncidenceMatrix$new(
    data = SimulatedIncidenceMatrix$new(IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
    ))$arr,dataArr)
  expect_equal(SimulatedIncidenceMatrix$new(
    data = SimulatedIncidenceMatrix$new(IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
    ))$rowData,rowData)
  expect_equal(SimulatedIncidenceMatrix$new(
    data = SimulatedIncidenceMatrix$new(IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
    ))$colData,colData)
  expect_equal(SimulatedIncidenceMatrix$new(
    data = SimulatedIncidenceMatrix$new(IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    ),
    nsim=nsim
    ))$metaData,metaData)

  expect_error(SimulatedIncidenceMatrix$new(
    data = lapply(1:nsim,function(x){IncidenceMatrix$new(
      data=dataMat,
      rowData=rowData,
      colData=colData,
      metaData=metaData
    )})),NA)
    expect_equal(SimulatedIncidenceMatrix$new(
      data = lapply(1:nsim,function(x){IncidenceMatrix$new(
        data=dataMat,
        rowData=rowData,
        colData=colData,
        metaData=metaData
      )}))$mat,dataMat)
      expect_equal(SimulatedIncidenceMatrix$new(
        data = lapply(1:nsim,function(x){IncidenceMatrix$new(
          data=dataMat,
          rowData=rowData,
          colData=colData,
          metaData=metaData
        )}))$arr,dataArr)
        expect_equal(SimulatedIncidenceMatrix$new(
          data = lapply(1:nsim,function(x){IncidenceMatrix$new(
            data=dataMat,
            rowData=rowData,
            colData=colData,
            metaData=metaData
          )}))$rowData,rowData)
          expect_equal(SimulatedIncidenceMatrix$new(
            data = lapply(1:nsim,function(x){IncidenceMatrix$new(
              data=dataMat,
              rowData=rowData,
              colData=colData,
              metaData=metaData
            )}))$colData,colData)
            expect_equal(SimulatedIncidenceMatrix$new(
              data = lapply(1:nsim,function(x){IncidenceMatrix$new(
                data=dataMat,
                rowData=rowData,
                colData=colData,
                metaData=metaData
              )}))$metaData,metaData)

            })

          testSimulatedIncidenceMatrix = SimulatedIncidenceMatrix$new(IncidenceMatrix$new(matrix()))
          testSimulatedIncidenceMatrix$sample
          test_AbstractSimulatedIncidenceMatrix(testSimulatedIncidenceMatrix,"Empty SimulatedIncidenceMatrix",is_same_ArrayData_as)
          testIncidenceMatrix = IncidenceMatrix$new(
            t(data_frame(x=1:10,y=1:10)),
            colData=list(letters[1:10],letters[5:14]),
            rowData=list(w=1:2,z=list(3,4))
          )
          testSimulatedIncidenceMatrix = SimulatedIncidenceMatrix$new(testIncidenceMatrix,5)
          test_that("rownames successfully assigned",{
            rn = testSimulatedIncidenceMatrix$rnames
            cn = testSimulatedIncidenceMatrix$cnames
            expect_that(is.null(rn) && is.null(cn),equals(FALSE))
          })
          testSimulatedIncidenceMatrix$sample
          test_AbstractSimulatedIncidenceMatrix(testSimulatedIncidenceMatrix,"Full SimulatedIncidenceMatrix",is_same_ArrayData_as)

          testIncidenceMatrix = IncidenceMatrix$new(
            t(data_frame(x=1:10,y=1:10)),
            colData=list(letters[1:10],letters[5:14]),
            rowData=list(w=1:2,z=list(3,4))
          )
          testSimulatedIncidenceMatrix = SimulatedIncidenceMatrix$new(
            list(
              testIncidenceMatrix,
              testIncidenceMatrix,
              testIncidenceMatrix,
              testIncidenceMatrix,
              testIncidenceMatrix
            ),
            5
          )
          testSimulatedIncidenceMatrix$sample
          test_AbstractSimulatedIncidenceMatrix(testSimulatedIncidenceMatrix,"List SimulatedIncidenceMatrix",is_same_ArrayData_as)
