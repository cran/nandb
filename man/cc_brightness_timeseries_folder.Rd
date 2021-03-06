% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cc_brightness.R
\name{cc_brightness_timeseries_folder}
\alias{cc_brightness_timeseries_folder}
\title{Cross-correlated brightness time-series calculations for every image in a
folder.}
\usage{
cc_brightness_timeseries_folder(
  folder_path = ".",
  frames_per_set,
  overlap = FALSE,
  ch1 = 1,
  ch2 = 2,
  thresh = NULL,
  detrend = detrend,
  quick = quick,
  filt = NULL,
  parallel = FALSE
)
}
\arguments{
\item{folder_path}{The path (relative or absolute) to the folder you wish to
process.}

\item{frames_per_set}{The number of frames with which to calculate the
successive cross-correlated brightnesses.

This may discard some images, for example if 175 frames are in the input and
\code{frames_per_set = 50}, then the last 25 are discarded. If bleaching or/and
thresholding are selected, they are performed on the whole image stack before
the sectioning is done for calculation of cross-correlated brightnesses.}

\item{overlap}{A boolean. If \code{TRUE}, the windows used to calculate brightness
are overlapped, if \code{FALSE}, they are not. For example, for a 20-frame image
series with 5 frames per set, if the windows are not overlapped, then the
frame sets used are 1-5, 6-10, 11-15 and 16-20; whereas if they are
overlapped, the frame sets are 1-5, 2-6, 3-7, 4-8 and so on up to 16-20.}

\item{ch1}{A natural number. The index of the first channel to use.}

\item{ch2}{A natural number. The index of the second channel to use.}

\item{thresh}{Do you want to apply an intensity threshold prior to
calculating cross-correlated brightness (via
\code{\link[autothresholdr:mean_stack_thresh]{autothresholdr::mean_stack_thresh()}})? If so, set your thresholding method
here. If this is a single value, that same threshold will be applied to
both channels. If this is a length-2 vector or list, then these two
thresholds will be applied to channels 1 and 2 respectively. A value of
\code{NA} for either channel gives no thresholding for that channel.}

\item{detrend}{Detrend your data with \code{\link[detrendr:detrending]{detrendr::img_detrend_rh()}}. This is
the best known detrending method for brightness analysis. For more
fine-grained control over your detrending, use the \code{detrendr} package. To
detrend one channel and not the other, specify this as a length 2 vector.}

\item{quick}{\code{FALSE} repeats the detrending procedure (which has some inherent
randomness) a few times to hone in on the best detrend. \code{TRUE} is quicker,
performing the routine only once. \code{FALSE} is better.}

\item{filt}{Do you want to smooth (\code{filt = 'smooth'}) or median (\code{filt = 'median'}) filter the cross-correlated brightness image using
\code{\link[=smooth_filter]{smooth_filter()}} or \code{\link[=median_filter]{median_filter()}} respectively? If selected, these are
invoked here with a filter radius of 1 and with the option \code{na_count = TRUE}. A value of \code{NA} for either channel gives no thresholding for that
channel. If you want to smooth/median filter the cross-correlated
brightness image in a different way, first calculate the cross-correlated
brightnesses without filtering (\code{filt = NULL}) using this function and then
perform your desired filtering routine on the result.}

\item{parallel}{Would you like to use multiple cores to speed up this
function? If so, set the number of cores here, or to use all available
cores, use \code{parallel = TRUE}.}
}
\description{
Perform \code{\link[=cc_brightness_timeseries]{cc_brightness_timeseries()}} calculations on all tif images in a
folder and save the resulting images to disk.
}
\examples{
\dontrun{
setwd(tempdir())
ijtiff::write_tif(img, "a.tif")
ijtiff::write_tif(img, "ab.tif")
cc_brightness_timeseries_folder(frames_per_set = 25)
list.files()
}

}
\seealso{
\code{\link[=cc_brightness_timeseries]{cc_brightness_timeseries()}}
}
