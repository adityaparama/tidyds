library(shiny)
library(here)
library(vroom)
library(dplyr)
library(ggplot2)

udara_bandung <- vroom(here("data-raw", "udara_bandung.csv"))
option_station <- unique(udara_bandung[["station"]])
option_parameter <- colnames(udara_bandung)[-c(1, 2)]

