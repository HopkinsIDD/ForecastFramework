################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####CompartmentalForecastModel Test Functions
################################################################################

#######################################TODO#####################################
test_that('test functions load',{
	source('help-RecursiveForecastModel.R')
	source('help-MatrixData.R')
})

is_same_CompartmentalForecastModel_as = function(rhs){
	return(function(lhs){
		TRUE
	})
}

test_CompartmentalForecastModel = function(model,modelName,modelEquivalence,data,dataName,dataEquivalence){
	firstModel = model$clone(TRUE)
	firstData = data$clone(TRUE)
	test_RecursiveForecastModel(model,modelName,modelEquivalence,data,dataName,dataEquivalence)
	expect_that(firstModel,modelEquivalence(model))
	expect_that(firstData,dataEquivalence(data))

	test_that(paste(modelName,'predictRow','works on data',dataName),{
	  firstModel = model$clone(TRUE)
	  firstData = data$clone(TRUE)
	  tempModel = model$clone(TRUE)
	  tempData = data$clone(TRUE)
	  if(!boolean_fit_works){
	    skip("Could not fit the data")
	  }
	  expect_error({
	    tempModel$fit(tempData)
	    tempModel$predictRow(tempData,1)
	  },NA)
	  expect_that(firstModel,modelEquivalence(model))
	  expect_that(firstData,dataEquivalence(data))
	})

	test_that(paste(modelName,'predictRow','respects','data','for data',dataName),{
	  firstModel = model$clone(TRUE)
	  firstData = data$clone(TRUE)
	  tempModel = model$clone(TRUE)
	  tempData = data$clone(TRUE)
	  if(!boolean_fit_works){
	    skip("Could not fit the data")
	  }
	  expect_that({
	    tempModel$fit(tempData)
	    tempModel$predictRow(tempData,1)
	    tempModel$data
	  },dataEquivalence(data))
	  expect_that(firstModel,modelEquivalence(model))
	  expect_that(firstData,dataEquivalence(data))
	})

	test_that(paste(modelName,'predictRow','respects','predCols','for data',dataName),{
	  firstModel = model$clone(TRUE)
	  firstData = data$clone(TRUE)
	  tempModel = model$clone(TRUE)
	  tempData = data$clone(TRUE)
	  if(!boolean_fit_works){
	    skip("Could not fit the data")
	  }
	  expect_that({
	    tempModel$fit(tempData)
	    tempModel$predictRow(tempData,1)
	    tempModel$predCols
	  },equals(model$predCols))
	  expect_that(firstModel,modelEquivalence(model))
	  expect_that(firstData,dataEquivalence(data))
	})
}
