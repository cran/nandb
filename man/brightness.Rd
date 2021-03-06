% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brightness.R
\name{brightness}
\alias{brightness}
\title{Calculate brightness from image series.}
\usage{
brightness(
  img,
  def,
  thresh = NULL,
  detrend = FALSE,
  quick = FALSE,
  filt = NULL,
  s = 1,
  offset = 0,
  readout_noise = 0,
  parallel = FALSE
)
}
\arguments{
\item{img}{A 4-dimensional array in the style of an
\link[ijtiff:ijtiff_img]{ijtiff_img} (indexed by \code{img[y, x, channel, frame]})
or a 3-dimensional array which is a single channel of an
\link[ijtiff:ijtiff_img]{ijtiff_img} (indexed by \code{img[y, x, frame]}).}

\item{def}{A character. Which definition of brightness do you want to use,
\code{"B"} or \code{"epsilon"}?}

\item{thresh}{The threshold or thresholding method (see
\code{\link[autothresholdr:mean_stack_thresh]{autothresholdr::mean_stack_thresh()}}) to use on the image prior to
detrending and brightness calculations.}

\item{detrend}{Detrend your data with \code{\link[detrendr:detrending]{detrendr::img_detrend_rh()}}. This is
the best known detrending method for brightness analysis. For more
fine-grained control over your detrending, use the \code{detrendr} package. If
there are many channels, this may be specified as a vector, one element for
each channel.}

\item{quick}{If \code{FALSE} (the default), the swap finding routine is run
several times to get a consensus for the best parameter. If \code{TRUE}, the
swap finding routine is run only once.}

\item{filt}{Do you want to smooth (\code{filt = 'mean'}) or median (\code{filt = 'median'}) filter the number image using \code{\link[=smooth_filter]{smooth_filter()}} or
\code{\link[=median_filter]{median_filter()}} respectively? If selected, these are invoked here with a
filter radius of 1 (with corners included, so each median is the median of
9 elements) and with the option \code{na_count = TRUE}. If you want to
smooth/median filter the number image in a different way, first calculate
the numbers without filtering (\code{filt = NULL}) using this function and then
perform your desired filtering routine on the result. If there are many
channels, this may be specified as a vector, one element for each channel.}

\item{s}{A positive number. The \eqn{S}-factor of microscope acquisition.}

\item{offset}{Microscope acquisition parameters. See reference
Dalal et al.}

\item{readout_noise}{Microscope acquisition parameters. See reference
Dalal et al.}

\item{parallel}{Would you like to use multiple cores to speed up this
function? If so, set the number of cores here, or to use all available
cores, use \code{parallel = TRUE}.}
}
\value{
A matrix, the brightness image.
}
\description{
Given a time stack of images, \code{brightness()} performs a calculation of the
brightness for each pixel.
}
\examples{
\donttest{
img <- ijtiff::read_tif(system.file("extdata", "50.tif", package = "nandb"))
ijtiff::display(img[, , 1, 1])
b <- brightness(img, "e", thresh = "Huang")
b <- brightness(img, "B", thresh = "tri")
}
}
\references{
Digman MA, Dalal R, Horwitz AF, Gratton E. Mapping the Number of
Molecules and Brightness in the Laser Scanning Microscope. Biophysical
Journal. 2008;94(6):2320-2332. \doi{10.1529/biophysj.107.114645}.

Dalal, RB, Digman, MA, Horwitz, AF, Vetri, V, Gratton, E (2008).
Determination of particle number and brightness using a laser scanning
confocal microscope operating in the analog mode. Microsc. Res. Tech., 71,
1:69-81. \doi{10.1002/jemt.20526}.
}
