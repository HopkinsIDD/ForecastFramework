% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ForecastModel

%Two new commands to help with formatting
\newcommand{\methodtable}{\bold{\cr #1 \cr} \tabular{lcl}}
\newcommand{\methoditem}{\emph{#1} \tab-\tab #2\cr}
\newcommand{\methodname}{\subsection{#1}}
\docType{class}
\name{ForecastModel}
\alias{ForecastModel}
\title{ForecastModel}
\description{A model for predicting multiple time steps into the future.  You should extend this class if you are predicting over some future time period and none of the convenience classes which extend forecast are applicable.}
\seealso{
}
\section{Methods}{
\methodname{forecast(newdata,steps)}{
This method \bold{must} be extended.  This function is similar to predict, in that it predicts the rows of the input data, however it can predict multiple timesteps into the future, instead of a single timestep.
\describe{\item{\emph{Arguments}}{\tabular{lcl}{
\methoditem{newdata}{The data to forecast from.}
\methoditem{steps}{The number of timesteps into the future to predict.}
}}
\item{Value}{A forecast}
}}
}
