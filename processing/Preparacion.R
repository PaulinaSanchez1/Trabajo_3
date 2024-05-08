install.packages("pacman")

pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven, sjPlot, ggplot2, psych)

pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

injuv2022 <- read_dta("input/data_orig/BBDD Respuesta - Encuesta Jóvenes.dta")
View(injuv2022)


