context("Move Ahead Model")

test_that('test functions load',{
  source('help-RecursiveForecastModel.R')
  source('help-MatrixData.R')
})

testMoveAheadModel1 = MoveAheadModel$new(nsim=1)
testMoveAheadModel2 = MoveAheadModel$new(nsim=10)

testData1 = IncidenceMatrix$new()


tempData = matrix((1:30)%%10,10,3)
col = matrix(0,10,1)
for(j in 1:997){
for(i in 1:8){
	col[i,] = sum(tempData[i:(i+2),j:(j+2)]) %% 20
}
col[9,] = sum(tempData[-c(2:8),j:(j+2)]) %% 20
col[10,] = sum(tempData[-c(3:9),j:(j+2)]) %% 20
cbind(tempData,col) -> tempData
}

testData2 = IncidenceMatrix$new(tempData)
testData2$colData = list(
time.in.yr = (1:1000) %% 20,
t = (1:1000) /20,
yr = (1:1000) %/% 20
)
testData2$metaData$t.step = 1/20
testData2$metaData$max.yr.time = 20

rownames(tempData) = letters[1:10]
testData3 = IncidenceMatrix$new(tempData)


testData3$colData = list(
time.in.yr = (1:1000) %% 20,
t = (1:1000) /20,
yr = (1:1000) %/% 20
)
testData3$metaData$t.step = 1/20
testData3$metaData$max.yr.time = 20

testData4 = ObservationList$new(
 dplyr::as_data_frame(testData3$colData) %>%
   dplyr::mutate(column=1:testData3$ncol) %>%
   dplyr::inner_join(reshape2::melt(testData3$mat),by=c('column'='Var2')),
 metaData=testData3$metaData
 )
testData4$formArray(
 row = 'Var1',
 col = 'column',
 val = 'value',
 dimData = list(NULL,c('t','time.in.yr','yr')),
 metaData = NULL
)
testData4$mat

test_RecursiveForecastModel(testMoveAheadModel1,"MoveAheadModel",is_same_RecursiveForecastModel_as,testData1,"Empty IncidenceMatrix",is_same_MatrixData_as)
test_RecursiveForecastModel(testMoveAheadModel1,"MoveAheadModel",is_same_RecursiveForecastModel_as,testData2,"Unlabeled IncidenceMatrix",is_same_MatrixData_as)
test_RecursiveForecastModel(testMoveAheadModel1,"MoveAheadModel",is_same_RecursiveForecastModel_as,testData3,"Labeled IncidenceMatrix",is_same_MatrixData_as)
test_RecursiveForecastModel(testMoveAheadModel1,"MoveAheadModel",is_same_RecursiveForecastModel_as,testData4,"Labeled ObservationList",is_same_MatrixData_as)

test_RecursiveForecastModel(testMoveAheadModel2,"Simulated MoveAheadModel",is_same_RecursiveForecastModel_as,testData1,"Empty IncidenceMatrix",is_same_MatrixData_as)
test_RecursiveForecastModel(testMoveAheadModel2,"Simulated MoveAheadModel",is_same_RecursiveForecastModel_as,testData2,"Unlabeled IncidenceMatrix",is_same_MatrixData_as)
test_RecursiveForecastModel(testMoveAheadModel2,"Simulated MoveAheadModel",is_same_RecursiveForecastModel_as,testData3,"Labeled IncidenceMatrix",is_same_MatrixData_as)
test_RecursiveForecastModel(testMoveAheadModel2,"Simulated MoveAheadModel",is_same_RecursiveForecastModel_as,testData4,"Labeled ObservationList",is_same_MatrixData_as)
