# ---  --- --- --- --- --- --- --- --- --- --- --- --- #
# MAIN-arl-votes-2020.R
# runs all files necessary 
# ---  --- --- --- --- --- --- --- --- --- --- --- --- #




# packages ---- 


library(tidyverse)
library(sf)
library(readtext)
library(pdftools)
library(hablar)
library(assertthat)
library(heatmaply)



# repo path settings 
root.code     <- "/Volumes/LA-REPUBLIC/github/arl-votes-2020"
root.data     <- "/Volumes/la-republic"
  app         <- file.path(root.code, "app")
  shp         <- file.path(root.data, "arl-shp/Voter_Precinct_Polygons-shp/Voter_Precinct_Polygons.shp")
  votes       <- file.path(root.data, "arl-daily-reports")
    latest    <- file.path(votes, "10-18-2020.pdf") # this is the path to the most recent tally pdf file.
    register  <- file.path(votes, "Registrant_Counts_By_Locality_10-01.pdf")



# script settings 

s.import <- 1

export   <- 1 # 1 exports the file.

# min/max page row number 
min1 <- 11
max1 <- 56
min2 <- 4
max2 <- 11

# totals
totalsrow <- 12 # the row on page 2 that contains the total field
e.totmail     = 1 # error term for total mail. My counts are correct as far as I know
e.outstanding = 1 # error term for mail outstanding.
# final assertion values 
# note these values are copied 'by hand' from 
# the imported pdf each day to ensure total numbers are correct 
nrow        = 54
s.mailed    = 22860
s.received  = 22517
s.counted   = 2471
s.totalmail = 53848
s.earlyvoted= 20885
s.totalvoted= 45873
  



if (s.import == 1) {
  source(file.path(root.code, "code/import.R"))
}



# with immense credit to: https://github.com/szimmer/CongressionalApportionment/blob/master/01_ReadCensusPDF.R
# https://stackoverflow.com/questions/3838774/comma-separator-for-numbers-in-r
# https://stackoverflow.com/questions/32890762/how-to-manipulate-tmap-legend
# https://stackoverflow.com/questions/54356383/how-to-fix-label-when-i-hover-mouse-over-map-made-with-tmap
# https://stackoverflow.com/questions/56036518/how-to-create-a-map-with-transparent-background
# https://stackoverflow.com/questions/1523126/how-to-read-data-when-some-numbers-contain-commas-as-thousand-separator
# https://www.davidsolito.com/post/conditional-drop-down-in-shiny/
# https://statisticsnz.github.io/simplevis/index.html
# https://stackoverflow.com/questions/59517901/how-to-insert-valuebox-inside-navbarpage-layout
# https://shiny.rstudio.com/gallery/absolutely-positioned-panels.html
# https://stackoverflow.com/questions/22959635/remove-duplicated-rows-using-dplyr
# https://stackoverflow.com/questions/5824173/replace-a-value-in-a-data-frame-based-on-a-conditional-if-statement#5824371
# https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html#correlation-heatmaps
# https://stackoverflow.com/questions/33565949/add-row-to-data-frame-with-dplyr
# https://stackoverflow.com/questions/42798377/shiny-leaflet-ploygon-click-event
# https://stackoverflow.com/questions/33499651/rmarkdown-in-shiny-application
# https://stackoverflow.com/questions/7340472/how-do-i-generate-a-list-with-a-specified-increment-step
# https://stackoverflow.com/questions/18535823/how-to-union-two-date-vectors-in-r
# https://stackoverflow.com/questions/29968152/python-setting-background-color-to-transparent-in-plotly-plots




