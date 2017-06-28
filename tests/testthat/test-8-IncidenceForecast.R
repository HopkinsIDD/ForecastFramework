################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####Model Test Functions
################################################################################

#######################################TODO#####################################


context("Incidence Forecast")

rm(list =ls())
test_that('test functions load',{
  source('help-SimulatedForecast.R')
})

is_same_IncidenceForecast_as = is_same_SimulatedForecast_as

testSimulatedIncidenceMatrix = SimulatedIncidenceMatrix$new(IncidenceMatrix$new(matrix()))
testSimulatedIncidenceMatrix$sample
test_SimulatedForecast(IncidenceForecast$new(testSimulatedIncidenceMatrix,forecastTimes=T),"Empty IncidenceForecast",is_same_IncidenceForecast_as)
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
test_SimulatedForecast(IncidenceForecast$new(testSimulatedIncidenceMatrix,forecastTimes=c(F,F,F,F,F,F,T,T,T,T)),"Full IncidenceForecast",is_same_IncidenceForecast_as)
