################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####ForecastModel Test Functions
################################################################################

#######################################TODO#####################################
test_that('test functions load',{
	source('help-Model.R')
	source('help-MatrixData.R')
})

is_same_ForecastModel_as = function(rhs){
	return(function(lhs){
		TRUE
	})
}

test_ForecastModel = function(model,modelName,modelEquivalence,data,dataName,dataEquivalence){
	firstModel = model$clone(TRUE)
	firstData = data$clone(TRUE)
	expect_true("ForecastModel" %in% class(model))
	results = test_Model(model,modelName,modelEquivalence,data,dataName,dataEquivalence)
	boolean_fit_works = results[[1]]
	boolean_predict_works = results[[2]]
	boolean_forecast_works = FALSE
	expect_that(firstModel,modelEquivalence(model))
	expect_that(firstData,dataEquivalence(data))

	test_that(paste(modelName,'forecast','works','for data',dataName),{
		firstModel = model$clone(TRUE)
		firstData = data$clone(TRUE)
		tempModel = model$clone(TRUE)
		tempData = data$clone(TRUE)
		if(!boolean_fit_works){
			skip("Could not fit the data")
		}
		expect_error({
			tempModel$fit(tempData)
			tempModel$forecast(tempData,5)
		},NA)
		expect_that(firstModel,modelEquivalence(model))
		expect_that(firstData,dataEquivalence(data))
	})

	tempModel = model$clone(TRUE)
	tempData = data$clone(TRUE)
	if(boolean_fit_works){
		try({
			tempModel$fit(tempData)
			tempModel$forecast(tempData,1)
			boolean_forecast_works = TRUE
		},silent=TRUE)
	}

	test_that(paste(modelName,'forecast','respects','predCols','for data',dataName),{
		firstModel = model$clone(TRUE)
		firstData = data$clone(TRUE)
		tempModel = model$clone(TRUE)
		tempData = data$clone(TRUE)
		if(!boolean_forecast_works){
			skip("Could not fit the data")
		}
		expect_that({
			tempModel$fit(tempData)
			tempModel$forecast(tempData,5)
			tempModel$predCols
		},equals(model$predCols))
		expect_that(firstModel,modelEquivalence(model))
		expect_that(firstData,dataEquivalence(data))
	})

	test_that(paste(modelName,'forecast','respects','data','for data',dataName),{
		firstModel = model$clone(TRUE)
		firstData = data$clone(TRUE)
		tempModel = model$clone(TRUE)
		tempData = data$clone(TRUE)
		if(!boolean_forecast_works){
			skip("Could not fit the data")
		}
		expect_that({
			tempModel$fit(tempData)
			#take a subset here
			tempModel$forecast(tempData,5)
			tempModel$data
		},dataEquivalence(data))
		expect_that(firstModel,modelEquivalence(model))
		expect_that(firstData,dataEquivalence(data))
	})

	return(c(boolean_fit_works,boolean_predict_works,boolean_forecast_works))
}
