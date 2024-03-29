% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{make_raster}
\alias{make_raster}
\title{make a raster}
\usage{
make_raster(cl4.object, dimensions, shape.file)
}
\arguments{
\item{cl4.object}{A C4R list with the Data slot in two dimension}

\item{dimensions}{vector specifying which dimensions corresponds to lat and lon. The rest will be averaged}

\item{shape.file}{sf object for which to crop and mask the spatRaster}
}
\value{
spatRaster
}
\description{
Make a spatRaster from a C4R list with dim(Data)=2
}
