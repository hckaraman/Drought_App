library(shiny)
library(RSQLite)
library(stringr)
library('SPEI')
library(ggplot2)
library('zoo')
library(Kendall)
library(DT)
library(trend)
library("ggpubr")
library(openxlsx)
library(vroom)


names <- c("Istasyon_No","Istasyon_Adi","YIL","AY","value","Enlem","Boylam","Rakim","var" )
varlist <- c("MINIMUM_SICAKLIK_C","MAKSIMUM_SICAKLIK_C","ORTALAMA_SICAKLIK_°C","AYLIK_TOPLAM_YAGIS_mm_Manuel")

template <- vroom('./Data/template.csv')

conn <- dbConnect(RSQLite::SQLite(), './Data/data.db')
query <-  "SELECT DISTINCT Istasyon_No from data ;"
stations <- dbGetQuery(conn, query)