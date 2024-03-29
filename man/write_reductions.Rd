% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h5_reductions.R
\name{write_reductions}
\alias{write_reductions}
\alias{guess_reductions}
\title{Write reduction information to a h5 file}
\usage{
write_reductions(h5_file, reductions, ...)

guess_reductions(seurat)
}
\arguments{
\item{h5_file}{Path to \code{h5} file}

\item{reductions}{A \code{list} of cell coordinates}

\item{...}{Arguments passed to \code{guess_reductions}}

\item{seurat}{Seurat object}
}
\description{
Write reduction information to a h5 file

Get a reductions list
}
\details{
Creates a list of 2D and 3D coordinates for reductions in \code{seurat}.
}
\section{Functions}{
\itemize{
\item \code{write_reductions}: Write coordinates for reductions to \code{h5} file

\item \code{guess_reductions}: Create a list of coordinates for reductions
}}

