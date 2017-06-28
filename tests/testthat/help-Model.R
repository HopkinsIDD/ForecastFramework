################################################################################
################################################################################
###            Spatial Prediction Model Object Oriented Framework            ###
################################################################################
################################################################################

################################################################################
####ForecastModel Test Functions
################################################################################

is_same_Model_as = function(rhs){
	return(function(lhs){
		TRUE
	})
}

test_Model = function(model,modelName,modelEquivalence,data,dataName,dataEquivalence){
	boolean_fit_works = FALSE
	boolean_predict_works = FALSE
	expect_true("Model" %in% class(model))
	test_that(paste(modelName,": fit works","on data :",dataName),{
		firstModel = model$clone(TRUE)
		firstData = data$clone(TRUE)
		tempModel = model$clone(TRUE)
		tempData = data$clone(TRUE)
		if(any(tempData$ncol <= tempModel$predCols)){
			expect_error(tempModel$fit(tempData),"We cannot go further back than the start of the matrix")
		} else{
			expect_error({
				tempModel$fit(tempData)
				boolean_fit_works = TRUE
			},NA)
		}
		expect_equal(firstModel,model)
		expect_equal(firstData,data)
	})

	try({
		tempModel = model$clone(TRUE)
		tempData = data$clone(TRUE)
		tempModel$fit(tempData)
		boolean_fit_works = TRUE
		tempModel$predict(tempData)
		boolean_predict_works = TRUE
	},silent=TRUE)

	test_that(paste(modelName,": predict works","on data :",dataName),{
		firstModel = model$clone(TRUE)
		firstData = data$clone(TRUE)
		tempModel = model$clone(TRUE)
		tempData = data$clone(TRUE)
		if(!boolean_fit_works){
			skip("Could not fit on this data")
		}
		expect_error({
			tempModel$fit(tempData)
			tempModel$predict(tempData)
		},NA)
		tempModel = model$clone(TRUE)
		expect_error({
			tempModel$predict(tempData)
		},"Please fit the model before predicting")
		expect_equal(firstModel,model)
		expect_equal(firstData,data)
	})

	return(c(boolean_fit_works,boolean_predict_works))
}
