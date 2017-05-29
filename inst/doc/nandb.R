## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, comment = "#>")
set.seed(1)

## ----load library, results='hide', message=FALSE-------------------------
required <- c("nandb", "EBImage", "dplyr", "ggplot2", "matrixStats")
sapply(required, library, character.only = TRUE)

## ----read image----------------------------------------------------------
img <- system.file("extdata", "50.tif", package = "nandb") %>% ReadImageData
dim(img)  # get img's dimensions

## ----display image, fig.width=7, fig.height=7----------------------------
display(normalize(img[, , 1]), method = "raster")

## ----Brightness calculation with bleaching correction, fig.width=7, fig.height=7----
img_brightness <- Brightness(img, tau = "auto", mst = "Huang")
MatrixRasterPlot(img_brightness, log.trans = TRUE)

## ----Brightness Plot, fig.width=7, fig.height=7--------------------------
Brightness(img, tau = "auto", mst = "Huang", filt = "median") %>% 
  MatrixRasterPlot(scale.name = "brightness", log.trans = TRUE)

## ----Brightness write to txt---------------------------------------------
WriteImageTxt(img_brightness, "BrightnessImage")
list.files(pattern = "\\.csv$")  # check that the csv file was written to the folder

## ---- echo=FALSE, results="hide"-----------------------------------------
file.remove("BrightnessImage.csv")  # remove that file (clean up)

## ----brightness density, message=FALSE, fig.width=7, fig.height=7--------
db <- density(img_brightness, na.rm = TRUE) %>% 
  {data.frame(x = .$x, y = .$y)}
ggplot(db, aes(x, y)) + geom_line() + 
  labs(x = "brightness", y = "frequency") +
  xlim(0.5, 2)

## ----immobile plot, fig.width=7, fig.height=7----------------------------
immobile_brightnesses <- matrix(rpois(50 * 10^6, 50), nrow = 10^6) %>% 
  {rowVars(.) / rowMeans(.)}
di <- density(immobile_brightnesses) %>% {data.frame(x = .$x, y = .$y)}
rbind(mutate(db, mobility = "mobile"), mutate(di, mobility = "immobile")) %>%      mutate(mobility = factor(mobility)) %>% 
  ggplot(aes(x, y)) + geom_line(aes(colour = mobility)) + 
  labs(x = "brightness", y = "frequency") +
  xlim(0.5, 2)

## ----kmer plot, fig.width=7, fig.height=7--------------------------------
KmerPlot(img_brightness, 1.04)

