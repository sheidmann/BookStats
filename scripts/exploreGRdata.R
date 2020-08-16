# exploreGRdata.R

# Sarah Heidmann
# Created 16 Aug, 2020
# Last modified 16 Aug, 2020

# Load libraries
library(tidyverse)

##### Import the data #####
books <- read_csv("data/goodreads_library_export_20200816.csv",
                  col_types = cols(`Date Read`=col_date(format="%D"),
                                   `Date Added`=col_date(format="%D")))


      
