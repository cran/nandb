## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, comment = "#>")
set.seed(1)

## ----load library, include=FALSE-----------------------------------------
required <- c("nandb", "tidyverse")
lapply(required, library, character.only = TRUE)

## ----read image----------------------------------------------------------
img <- ijtiff::read_tif(system.file("extdata", "50_big.tif", package = "nandb"))
dim(img)  # get img's dimensions

## ----display image, fig.width=7, fig.height=7----------------------------
ijtiff::display(detrendr::mean_pillars(img)[, , 1, 1])  # first channel, first frame

## ----Brightness calculation with bleaching correction, fig.width=7, fig.height=7----
img_brightness <- brightness(img, "e", tau = "auto", thresh = "Huang")
ijtiff::display(img_brightness[, , 1, 1])

## ----Brightness Plot, fig.width=7, fig.height=7--------------------------
img_brightness_med <- brightness(img, "e", tau = "auto", 
                                 thresh = "Huang", filt = "median")
ijtiff::display(img_brightness_med[, , 1, 1])

## ----Brightness write to txt---------------------------------------------
ijtiff::write_tif(img_brightness, "BrightnessImage")
list.files(pattern = "\\.txt$")  # check that the .txt file was written to the folder

## ---- echo=FALSE, results="hide"-----------------------------------------
file.remove("BrightnessImage.tif")  # remove that file (clean up)

## ----brightness density, message=FALSE, fig.width=7, fig.height=7--------
db <- density(img_brightness, na.rm = TRUE) %>% 
  {data.frame(x = .$x, y = .$y)}
ggplot(db, aes(x, y)) + geom_line() + 
  labs(x = "brightness", y = "frequency") +
  xlim(-2, 2)

## ----immobile plot, fig.width=7, fig.height=7----------------------------
immobile_brightnesses <- matrix(rpois(50 * 10^6, 50), nrow = 10^6) %>% 
  {apply(., 1, var) / rowMeans(.) - 1}
di <- density(immobile_brightnesses) %>% {data.frame(x = .$x, y = .$y)}
rbind(mutate(db, mobility = "mobile"), mutate(di, mobility = "immobile")) %>%      mutate(mobility = factor(mobility)) %>% 
  ggplot(aes(x, y)) + geom_line(aes(colour = mobility)) + 
  labs(x = "brightness", y = "frequency") +
  xlim(-2, 2)

