################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####RecursiveForecastModel Test Functions
################################################################################

#######################################TODO#####################################
test_that('test functions load',{
	source('help-ForecastModel.R')
	source('help-MatrixData.R')
})

is_same_RecursiveForecastModel_as = function(rhs){
	return(function(lhs){
		TRUE
	})
}

test_RecursiveForecastModel = function(model,modelName,modelEquivalence,data,dataName,dataEquivalence){
	boolean_fit_works = FALSE
	boolean_predict_works = FALSE
	boolean_forecast_works=FALSE
	expect_true("RecursiveForecastModel" %in% class(model))
	firstModel = model$clone(TRUE)
	firstData = data$clone(TRUE)
	results = test_ForecastModel(model,modelName,modelEquivalence,data,dataName,dataEquivalence)

	expect_that(firstModel,modelEquivalence(model))
	expect_that(firstData,dataEquivalence(data))
	boolean_fit_works = results[1]
	boolean_predict_works = results[2]
	boolean_forecast_works = results[3]

	test_that(paste(modelName,'fit','respects','predCols','for data',dataName),{
	  firstModel = model$clone(TRUE)
	  firstData = data$clone(TRUE)
	  tempModel = model$clone(TRUE)
	  tempData = data$clone(TRUE)
	  if(!boolean_fit_works){
	    skip("Could not fit the data")
	  }
	  expect_that({
	    tempModel$fit(tempData)
	    tempModel$predCols
	  },equals(model$predCols))
	  expect_that(firstModel,modelEquivalence(model))
	  expect_that(firstData,dataEquivalence(data))
	})

	test_that(paste(modelName,'fit','respects','data','for data',dataName),{
	  firstModel = model$clone(TRUE)
	  firstData = data$clone(TRUE)
	  tempModel = model$clone(TRUE)
	  tempData = data$clone(TRUE)
	  if(!boolean_fit_works){
	    skip("Could not fit the data")
	  }
	  expect_that({
	    tempModel$fit(tempData)
	    tempModel$data
	  },dataEquivalence(data))
	  expect_that(firstModel,modelEquivalence(model))
	  expect_that(firstData,dataEquivalence(data))
	})

	test_that(paste(modelName,'predict','respects','predCols','for data',dataName),{
	  firstModel = model$clone(TRUE)
	  firstData = data$clone(TRUE)
	  tempModel = model$clone(TRUE)
	  tempData = data$clone(TRUE)
	  if(!boolean_predict_works){
	    skip("Could not fit the data")
	  }
	  expect_that({
	    tempModel$fit(tempData)
	    #Take a subset here
	    tempModel$predict(tempData)
	    tempModel$predCols
	  },equals(model$predCols))
	  expect_that(firstModel,modelEquivalence(model))
	  expect_that(firstData,dataEquivalence(data))
	})

	test_that(paste(modelName,'predict','respects','data','for data',dataName),{
	  firstModel = model$clone(TRUE)
	  firstData = data$clone(TRUE)
	  tempModel = model$clone(TRUE)
	  tempData = data$clone(TRUE)
	  if(!boolean_predict_works){
	    skip("Could not fit the data")
	  }
	  expect_that({
	    tempModel$fit(tempData)
	    tempModel$predict(tempData)
	    tempModel$data
	  },dataEquivalence(data))
	  expect_that(firstModel,modelEquivalence(model))
	  expect_that(firstData,dataEquivalence(data))
	})

	return(c(boolean_fit_works,boolean_predict_works))
}
