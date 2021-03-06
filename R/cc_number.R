#' Cross-correlated number.
#'
#' Given a time stack of images  and two channels, calculate the
#' cross-correlated number of those two channels for each pixel.
#'
#' @inheritParams number
#' @param ch1 A natural number. The index of the first channel to use.
#' @param ch2 A natural number. The index of the second channel to use.
#' @param thresh Do you want to apply an intensity threshold prior to
#'   calculating cross-correlated number (via
#'   [autothresholdr::mean_stack_thresh()])? If so, set your thresholding method
#'   here. If this is a single value, that same threshold will be applied to
#'   both channels. If this is a length-2 vector or list, then these two
#'   thresholds will be applied to channels 1 and 2 respectively. A value of
#'   `NA` for either channel gives no thresholding for that channel.
#' @param detrend Detrend your data with [detrendr::img_detrend_rh()]. This is
#'   the best known detrending method for brightness analysis. For more
#'   fine-grained control over your detrending, use the `detrendr` package. To
#'   detrend one channel and not the other, specify this as a length 2 vector.
#' @param filt Do you want to smooth (`filt = 'smooth'`) or median (`filt =
#'   'median'`) filter the cross-correlated number image using [smooth_filter()]
#'   or [median_filter()] respectively? If selected, these are invoked here with
#'   a filter radius of 1 and with the option `na_count = TRUE`. A value of `NA`
#'   for either channel gives no thresholding for that channel. If you want to
#'   smooth/median filter the cross-correlated number image in a different way,
#'   first calculate the cross-correlated numbers without filtering (`filt =
#'   NULL`) using this function and then perform your desired filtering routine
#'   on the result.
#'
#' @return A numeric matrix, the cross-correlated number image.
#'
#' @examples
#' \donttest{
#' img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
#'   package = "nandb"
#' ))
#' ijtiff::display(detrendr::mean_pillars(img[, , 1, ]))
#' ijtiff::display(detrendr::mean_pillars(img[, , 2, ]))
#' n <- number(img, def = "n", thresh = "Huang", filt = "median")
#' ijtiff::display(n[, , 1, 1])
#' ijtiff::display(n[, , 2, 1])
#' cc_n <- cc_number(img, thresh = "Huang")
#' ijtiff::display(cc_n[, , 1, 1])
#' }
#' @export
cc_number <- function(img, ch1 = 1, ch2 = 2,
                      thresh = NULL, detrend = FALSE, quick = FALSE,
                      filt = NULL, parallel = FALSE) {
  checkmate::assert_int(ch1, lower = 1)
  checkmate::assert_int(ch2, lower = 1)
  checkmate::assert_logical(detrend, min.len = 1, max.len = 2)
  if (length(detrend) == 1) detrend %<>% rep(2)
  thresh %<>% prepare_thresh()
  filt %<>% prepare_filt()
  img %<>% nb_get_img()
  checkmate::assert_array(img, d = 4)
  for (i in c(ch1, ch2)) {
    if (i > dim(img)[3]) {
      s <- dplyr::if_else(dim(img)[3] == 1, "", "s")
      custom_stop("
                  You have requested to use channel {i}, but your image has
                  only {dim(img)[3]} channel{s} in total.
                  ", "
                  This is not possible. Please retry with valid channel numbers.
                  ")
    }
  }
  ch1 <- img[, , ch1, ]
  ch2 <- img[, , ch2, ]
  thresh_atts <- as.list(rep(NA, 2))
  swaps_atts <- extend_for_all_chs(
    structure(NA, auto = FALSE),
    2
  )
  if (all(is.na(ch1))) {
    custom_stop(
      "The first channel is all NAs.",
      "Can't compute on an array of all NAs."
    )
  }
  if (all(is.na(ch2))) {
    custom_stop(
      "The second channel is all NAs.",
      "Can't compute on an array of all NAs."
    )
  }
  if (!is.na(thresh[[1]])) {
    ch1 %<>% autothresholdr::mean_stack_thresh(thresh[[1]])
    thresh_atts[[1]] <- attr(ch1, "thresh")
  }
  if (!is.na(thresh[[2]])) {
    ch2 %<>% autothresholdr::mean_stack_thresh(thresh[[2]])
    thresh_atts[[2]] <- attr(ch2, "thresh")
  }
  if (all(is.na(ch1))) {
    custom_stop("
                After thresholding, the first channel is all NAs.
                ", "
                Can't compute on an array of all NAs.
                ", "
                You need to choose a less severe threshold for this channel.
                ")
  }
  if (all(is.na(ch2))) {
    custom_stop("
                After thresholding, the second channel is all NAs.
                ", "
                Can't compute on an array of all NAs.
                ", "
                You need to choose a less severe threshold for this channel.
                ")
  }
  if (detrend[[1]]) {
    ch1 %<>% detrendr::img_detrend_rh(quick = quick)
    swaps_atts[[1]] <- attr(ch1, "parameter")
    attr(swaps_atts[[1]], "auto") <- attr(ch1, "auto")
  }
  if (detrend[[2]]) {
    ch2 %<>% detrendr::img_detrend_rh(quick = quick)
    swaps_atts[[2]] <- attr(ch2, "parameter")
    attr(swaps_atts[[2]], "auto") <- attr(ch2, "auto")
  }
  if (length(dim(ch1)) == 4) ch1 <- ch1[, , 1, ]
  if (length(dim(ch2)) == 4) ch2 <- ch2[, , 1, ]
  cc_n <- ((detrendr::mean_pillars(ch1, parallel = parallel) *
    detrendr::mean_pillars(ch2, parallel = parallel))[, , 1, 1]) /
    cross_var_pillars(ch1, ch2)
  if (!is.na(filt)) {
    if (filt == "median") {
      cc_n %<>% median_filter(na_count = TRUE)
    } else {
      cc_n %<>% smooth_filter(na_count = TRUE)
    }
  }
  cc_number_img(cc_n, thresh = thresh_atts, swaps = swaps_atts, filt = filt)
}

#' Create a cross-correlated number time-series.
#'
#' Given a stack of images `img`, use the first `frames_per_set` of them to
#' create one cross-correlated number image, the next `frames_per_set` of
#' them to create the next and so on to get a time-series of cross-correlated
#' number images.
#'
#' @param frames_per_set The number of frames with which to calculate the
#'   successive cross-correlated numbers.
#'
#' This may discard some images, for example if 175 frames are in the input and
#' `frames_per_set = 50`, then the last 25 are discarded. If bleaching or/and
#' thresholding are selected, they are performed on the whole image stack before
#' the sectioning is done for calculation of cross-correlated numbers.
#'
#' @inheritParams cc_number
#' @inheritParams number
#' @inheritParams number_timeseries
#'
#' @return An array where the \eqn{i}th slice is the \eqn{i}th cross-correlated
#'   number image.
#' @seealso [number()].
#'
#' @examples
#' \donttest{
#' img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
#'   package = "nandb"
#' ))
#' cc_nts <- cc_number_timeseries(img, 10,
#'   thresh = "Huang",
#'   filt = "median", parallel = 2
#' )
#' ijtiff::display(cc_nts[, , 1, 1])
#' }
#' @export
cc_number_timeseries <- function(img, frames_per_set, overlap = FALSE,
                                 ch1 = 1, ch2 = 2,
                                 thresh = NULL,
                                 detrend = FALSE, quick = FALSE,
                                 filt = NULL,
                                 parallel = FALSE) {
  checkmate::assert_int(ch1, lower = 1)
  checkmate::assert_int(ch2, lower = 1)
  checkmate::assert_logical(detrend, min.len = 1, max.len = 2)
  if (length(detrend) == 1) detrend %<>% rep(2)
  thresh %<>% prepare_thresh()
  filt %<>% prepare_filt()
  if (is.character(img)) img %<>% ijtiff::read_tif(msg = FALSE)
  checkmate::assert_array(img, d = 4)
  if (dim(img)[4] < frames_per_set) {
    custom_stop("
                You have selected {frames_per_set} frames per set,
                but there are only {dim(img)[4]}, frames in total.
                ", "
                Please select less than {dim(img)[4]} frames per set.
                ")
  }
  ch1 <- img[, , ch1, ]
  ch2 <- img[, , ch2, ]
  thresh_atts <- as.list(rep(NA, 2))
  swaps_atts <- extend_for_all_chs(
    structure(NA, auto = FALSE),
    2
  )
  if (all(is.na(ch1))) {
    custom_stop(
      "The first channel is all NAs.",
      "Can't compute on an array of all NAs."
    )
  }
  if (all(is.na(ch2))) {
    custom_stop(
      "The second channel is all NAs.",
      "Can't compute on an array of all NAs."
    )
  }
  if (!is.na(thresh[[1]])) {
    ch1 %<>% autothresholdr::mean_stack_thresh(thresh[[1]])
    thresh_atts[[1]] <- attr(ch1, "thresh")
  }
  if (!is.na(thresh[[2]])) {
    ch2 %<>% autothresholdr::mean_stack_thresh(thresh[[2]])
    thresh_atts[[2]] <- attr(ch2, "thresh")
  }
  if (all(is.na(ch1))) {
    custom_stop("
                After thresholding, the first channel is all NAs.
                ", "
                Can't compute on an array of all NAs.
                ", "
                You need to choose a less severe threshold for this channel.
                ")
  }
  if (all(is.na(ch2))) {
    custom_stop("
                After thresholding, the second channel is all NAs.
                ", "
                Can't compute on an array of all NAs.
                ", "
                You need to choose a less severe threshold for this channel.
                ")
  }
  if (detrend[[1]]) {
    ch1 %<>% detrendr::img_detrend_rh(quick = quick)
    swaps_atts[[1]] <- attr(ch1, "parameter")
    attr(swaps_atts[[1]], "auto") <- attr(ch1, "auto")
  }
  if (detrend[[2]]) {
    ch2 %<>% detrendr::img_detrend_rh(quick = quick)
    swaps_atts[[2]] <- attr(ch2, "parameter")
    attr(swaps_atts[[2]], "auto") <- attr(ch2, "auto")
  }
  if (length(dim(ch1)) == 4) ch1 <- ch1[, , 1, ]
  if (length(dim(ch2)) == 4) ch2 <- ch2[, , 1, ]
  if (overlap) {
    n_sets <- dim(ch1)[3] - frames_per_set + 1
    cc_n_ts <- array(0, dim = c(dim(ch1)[1:2], n_sets))
    for (i in seq_len(n_sets)) {
      indices_i <- seq(i, i + frames_per_set - 1)
      ch1_i <- ch1[, , indices_i]
      ch2_i <- ch2[, , indices_i]
      cc_n_ts[, , i] <-
        ((detrendr::mean_pillars(ch1_i, parallel = parallel) *
          detrendr::mean_pillars(ch2_i, parallel = parallel))[, , 1, 1]) /
          cross_var_pillars(ch1_i, ch2_i)
    }
  } else {
    n_sets <- dim(ch1)[3] %/% frames_per_set
    cc_n_ts <- array(0, dim = c(dim(ch1)[1:2], n_sets))
    for (i in seq_len(n_sets)) {
      indices_i <- seq((i - 1) * frames_per_set + 1, i * frames_per_set)
      ch1_i <- ch1[, , indices_i]
      ch2_i <- ch2[, , indices_i]
      cc_n_ts[, , i] <-
        ((detrendr::mean_pillars(ch1_i, parallel = parallel) *
          detrendr::mean_pillars(ch2_i, parallel = parallel))[, , 1, 1]) /
          cross_var_pillars(ch1_i, ch2_i)
    }
  }
  if (!is.na(filt)) {
    if (filt == "median") {
      for (i in seq_len(n_sets)) {
        cc_n_ts[, , i] %<>% median_filter(na_count = TRUE)
      }
    } else {
      for (i in seq_len(n_sets)) {
        cc_n_ts[, , i] %<>% smooth_filter(na_count = TRUE)
      }
    }
  }
  cc_number_ts_img(cc_n_ts,
    frames_per_set = frames_per_set, overlapped = overlap,
    thresh = thresh_atts, swaps = swaps_atts, filt = filt
  )
}

cc_number_file <- function(path, ch1 = 1, ch2 = 2, thresh = NULL,
                           detrend = FALSE, quick = FALSE,
                           filt = NULL, parallel = FALSE) {
  checkmate::assert_file_exists(path)
  if (endsWith(path, "/")) path %<>% filesstrings::before_last("/+$")
  need_to_change_dir <- stringr::str_detect(path, "/")
  if (need_to_change_dir) {
    dir <- filesstrings::str_before_last(path, "/")
    path %<>% filesstrings::str_after_last("/")
    checkmate::assert_directory_exists(dir)
    withr::local_dir(dir)
  }
  cc_n <- cc_number(path,
    ch1 = ch1, ch2 = ch2,
    thresh = thresh, detrend = detrend, quick = quick, filt = filt,
    parallel = parallel
  )
  suppressMessages(filesstrings::create_dir("cc_number"))
  path %<>% filesstrings::before_last_dot() %>%
    paste0("cc_number", "/", ., make_cc_nb_filename_ending(cc_n)) %>%
    deduplicate_cc_nb_filename()
  ijtiff::write_tif(cc_n, path, msg = FALSE)
}

cc_number_timeseries_file <- function(path, frames_per_set,
                                      overlap = FALSE,
                                      ch1 = 1, ch2 = 2,
                                      thresh = NULL,
                                      detrend = FALSE, quick = FALSE,
                                      filt = NULL, parallel = FALSE) {
  checkmate::assert_file_exists(path)
  if (endsWith(path, "/")) path %<>% filesstrings::before_last("/+$")
  need_to_change_dir <- stringr::str_detect(path, "/")
  if (need_to_change_dir) {
    dir <- filesstrings::str_before_last(path, "/")
    path %<>% filesstrings::str_after_last("/")
    checkmate::assert_directory_exists(dir)
    withr::local_dir(dir)
  }
  cc_n_ts <- cc_number_timeseries(path,
    ch1 = ch1, ch2 = ch2,
    frames_per_set = frames_per_set,
    thresh = thresh, detrend = detrend, quick = quick, filt = filt,
    parallel = parallel
  )
  suppressMessages(filesstrings::create_dir("cc_number_timeseries"))
  path %<>% filesstrings::before_last_dot() %>%
    paste0(
      "cc_number_timeseries", "/", .,
      make_cc_nb_filename_ending(cc_n_ts)
    ) %>%
    deduplicate_cc_nb_filename()
  ijtiff::write_tif(cc_n_ts, path, msg = FALSE)
}

#' Cross-correlated number calculations for every image in a folder.
#'
#' Perform [cc_number()] calculations on all TIFF images in a folder and
#' save the resulting images to disk.
#'
#'
#' @inheritParams cc_number
#' @inheritParams number_folder
#' @inheritParams number
#'
#' @examples
#' \dontrun{
#' setwd(tempdir())
#' ijtiff::write_tif(img, "a.tif")
#' ijtiff::write_tif(img, "ab.tif")
#' cc_number_folder()
#' list.files()
#' }
#' @export
cc_number_folder <- function(folder_path = ".", ch1 = 1, ch2 = 2,
                             thresh = NULL,
                             detrend = FALSE, quick = FALSE,
                             filt = NULL, parallel = FALSE) {
  checkmate::assert_directory_exists(folder_path)
  withr::local_dir(folder_path)
  file_names <- dir(pattern = "\\.tiff*$")
  purrr::map(file_names, cc_number_file,
    ch1 = ch1, ch2 = ch2,
    thresh = thresh, detrend = detrend, quick = quick,
    filt = filt, parallel = parallel
  ) %>%
    magrittr::set_names(filesstrings::before_last_dot(file_names)) %>%
    invisible()
}

#' Cross-correlated number time-series calculations for every image in a
#' folder.
#'
#' Perform [cc_number_timeseries()] calculations on all tif images in a
#' folder and save the resulting images to disk.
#'
#' @inheritParams cc_number
#' @inheritParams cc_number_timeseries
#' @inheritParams number_folder
#' @inheritParams number_timeseries
#'
#' @seealso [cc_number_timeseries()]
#'
#' @examples
#' \dontrun{
#' setwd(tempdir())
#' ijtiff::write_tif(img, "a.tif")
#' ijtiff::write_tif(img, "ab.tif")
#' cc_number_timeseries_folder(frames_per_set = 25)
#' list.files()
#' }
#'
#' @export
cc_number_timeseries_folder <- function(folder_path = ".", frames_per_set,
                                        overlap = FALSE,
                                        ch1 = 1, ch2 = 2,
                                        thresh = NULL,
                                        detrend = FALSE, quick = FALSE,
                                        filt = NULL, parallel = FALSE) {
  checkmate::assert_directory_exists(folder_path)
  withr::local_dir(folder_path)
  file_names <- dir(pattern = "\\.tiff*$")
  purrr::map(file_names, cc_number_timeseries_file,
    ch1 = ch1, ch2 = ch2,
    frames_per_set = frames_per_set, overlap = overlap, thresh = thresh,
    detrend = detrend, quick = quick,
    filt = filt, parallel = parallel
  ) %>%
    magrittr::set_names(filesstrings::before_last_dot(file_names)) %>%
    invisible()
}
