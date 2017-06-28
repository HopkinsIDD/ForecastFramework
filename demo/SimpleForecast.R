data = IncidenceMatrix$new(matrix(1:9,3,3))
forecast = SimpleForecast$new(data,forecastTimes=c(FALSE,FALSE,TRUE))
forecast
forecast$forecastTimes
forecast$forecastMadeTime
forecast$data$mat
forecast$nsim
forecast$mean()$mat
forecast$median()$mat

#While every forecast has these methods, they do not make sense here
#forecast$binDist(1:4*4)$arr
#forecast$quantile(c(.05,.5,.95))$arr
