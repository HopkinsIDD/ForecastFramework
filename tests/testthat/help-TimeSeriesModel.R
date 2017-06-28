################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####TimeSeriesModel Test Functions
################################################################################

#######################################TODO#####################################
test_that('test functions load',{
	source('help-Model.R')
	source('help-MatrixData.R')
})

is_same_TimeSeriesModel_as = function(rhs){
	return(function(lhs){
		TRUE
	})
}

test_TimeSeriesModel = function(model,modelName,modelEquivalence,data,dataName,dataEquivalence){
	firstModel = model$clone(TRUE)
	firstData = data$clone(TRUE)
	results = test_Model(model,modelName,modelEquivalence,data,dataName,dataEquivalence)
	boolean_fit_works = results[[1]]
	boolean_predict_works = results[[2]]
	expect_that(firstModel,modelEquivalence(model))
	expect_that(firstData,dataEquivalence(data))

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

	return(c(boolean_fit_works,boolean_predict_works))
}
