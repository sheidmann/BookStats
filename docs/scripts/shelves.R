# shelves.R

# Sarah Heidmann
# Created 16 Aug 2020
# Last modified 30 Aug 2020

# Organize books by their bookshelves.

# Load libraries
library(tidyverse)

##### Import the data #####
# Date format: %Y/%m/%d or %D (m/d/yy)
books <- read_csv("data/goodreads_library_export_20200830.csv",
                  col_types = cols(`Date Read`=col_date(format="%Y/%m/%d"),
                                   `Date Added`=col_date(format="%Y/%m/%d")))
##### Tidy the dataset #####
widebooks <- books %>%
      separate_rows("Bookshelves", sep = ", ") %>%
      select(Bookshelves, Title) %>%
      mutate(shelflog = 1) %>%
      pivot_wider(names_from=Bookshelves,
                  values_from=shelflog) %>%
      mutate_all(replace_na,0)
widebooks

##### Stats #####
# Books on the greatest number of shelves
widebooks %>%
      mutate(Shelves=rowSums(select(.,-Title))) %>%
      select(Title, Shelves) %>%
      arrange(desc(Shelves))

# How many shelves are books on?
widebooks %>%
      mutate(Shelves=rowSums(select(.,-Title))) %>% select(Shelves) %>%
      summary()

# Which shelves have the most books?
widebooks %>%
      select(-Title) %>%
      colSums() %>% 
      bind_rows() %>%
      pivot_longer(everything(), names_to = "shelf", values_to = "books") %>%
      arrange(desc(books))

##### What book should I read? #####
widebooks %>%
   left_join(select(books,Title,`Average Rating`)) %>%
   mutate(highrating = ifelse(`Average Rating`>4,1,0)) %>%
   filter(`to-read`==1) %>%
   filter(kindle==1 | library==1) %>%
   mutate(ranking=`highly-recommended`+island+`life-changing`+ highrating +
             `top-100-lifetime`+`neil-degrasse-tyson`) %>%
   select(Title, ranking, kindle, library) %>%
   arrange(desc(ranking))
