# exploreGRdata.R

# Sarah Heidmann
# Created 16 Aug 2020
# Last modified 16 Aug 2020

# My first attempts at exploring Goodreads book data.
# Recreates Goodreads "stats" plots, and adds a few of my own.

# Load libraries
library(tidyverse)
library(lubridate)

##### Import the data #####
# Date format: %Y/%m/%d or %D (m/d/yy)
books <- read_csv("data/goodreads_library_export_20200816.csv",
                  col_types = cols(`Date Read`=col_date(format="%Y/%m/%d"),
                                   `Date Added`=col_date(format="%Y/%m/%d")))

##### Goodreads graphs, redone #####

# Books read by year
minYear <- year(min(books$`Date Read`, na.rm = TRUE))
maxYear <- year(max(books$`Date Read`, na.rm = TRUE))
books %>%
      filter(!is.na(`Date Read`)) %>%
      mutate(YearRead = year(`Date Read`)) %>%
      ggplot(data=.) +
      geom_histogram(aes(x=YearRead), binwidth=1,
                     fill= "turquoise", color = "blue") +
      scale_y_continuous(name="Number of Books Read", expand = c(0,0)) +
      scale_x_continuous(name= "",
                         breaks = seq(minYear,maxYear,1),
                         labels = seq(minYear,maxYear,1)) +
      theme(panel.background = element_blank(),
            axis.line = element_line())

# Pages read by year
books %>%
      filter(!is.na(`Date Read`)) %>%
      mutate(YearRead = year(`Date Read`)) %>%
      ggplot(data=.) +
      geom_bar(aes(x=YearRead, y=`Number of Pages`), stat="identity",
                     fill= "turquoise", color = "blue") +
      scale_y_continuous(name="Number of Pages Read", expand = c(0,0)) +
      scale_x_continuous(name= "",
                         breaks = seq(minYear,maxYear,1),
                         labels = seq(minYear,maxYear,1)) +
      theme(panel.background = element_blank(),
            axis.line = element_line())

# Publication year by date read
# I have a couple outliers, so set a minimum publication date
earliestPub <- 1800
books %>%
      filter(!is.na(`Date Read`)) %>%
      ggplot(data=.) +
      geom_point(aes(x=`Date Read`, y=`Original Publication Year`),
                 color = "blue") +
      scale_y_continuous(name="Publication Date", expand = c(0,0),
                         limits = c(earliestPub, maxYear)) +
      theme(panel.background = element_blank(),
            axis.line = element_line())

# Books over time
books %>%
      filter(!is.na(`Date Read`)) %>%
      mutate(MonthRead = floor_date(`Date Read`, "month")) %>%
      group_by(MonthRead) %>%
      summarize(BooksRead = length(Title)) %>%
      ggplot(data=.) +
      geom_bar(aes(x=MonthRead, y=BooksRead), stat="identity",
                     fill= "turquoise", color = "blue") +
      scale_y_continuous(name="Number of Books Read", expand = c(0,0)) +
      scale_x_date(date_breaks="1 month", date_labels="%Y-%b", expand=c(0,0)) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            axis.text.x =element_text(angle=-90))

# Pages over time
books %>%
      filter(!is.na(`Date Read`)) %>%
      mutate(MonthRead = floor_date(`Date Read`, "month")) %>%
      group_by(MonthRead) %>%
      summarize(PagesRead = sum(`Number of Pages`, na.rm = TRUE)) %>%
      ggplot(data=.) +
      geom_bar(aes(x=MonthRead, y=PagesRead), stat="identity",
               fill= "turquoise", color = "blue") +
      scale_y_continuous(name="Number of Pages Read", expand = c(0,0)) +
      scale_x_date(date_breaks="1 month", date_labels="%Y-%b", expand=c(0,0)) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            axis.text.x =element_text(angle=-90))

##### My additional graphs #####
# Most read books (top 10)
# For now it is all books I have read more than once
books %>%
      arrange(desc(`Read Count`)) %>%
      select(Title, `Read Count`) %>%
      filter(`Read Count` >1) %>%
      ggplot(data=.) +
      geom_bar(aes(x=reorder(Title, -`Read Count`), y=`Read Count`), 
               stat="identity",
               fill= "turquoise", color = "blue") +
      scale_y_continuous(name="Number of Times Read", expand = c(0,0)) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            axis.text.x =element_text(angle=-90),
            axis.title.x = element_blank())

      
# Books added/read in monthly bins
# I added much of my library in the first few days of using Goodreads.
# Set an upper limit to exclude those outlier days
upperLimit <- 40
ggplot(data=books) +
      geom_histogram(aes(x=`Date Added`), fill = NA,
                     color = "blue", binwidth = 30) +
      geom_histogram(aes(x=`Date Read`), fill = NA,
                     color = "green", binwidth = 30,
                     data = filter(books,!(is.na(`Date Read`)))) +
      scale_y_continuous(limits = c(0,upperLimit), name ="",
                         expand = c(0,0)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b") +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            axis.text.x =element_text(angle=-90))

# Rate at which books are added/read over time (monthly bins)
addDates <- books %>% 
   mutate(Month = floor_date(`Date Added`, "month")) %>%
   group_by(Month) %>% 
   summarize(BooksAdded=length(Month))
readDates <- books %>% 
   filter(!is.na(`Date Read`)) %>%
   mutate(Month = floor_date(`Date Read`, "month")) %>%
   group_by(Month) %>% 
   summarize(BooksRead=length(Month))
add_read <- full_join(addDates, readDates, by= "Month") %>% 
   replace_na(list(BooksAdded = 0, BooksRead = 0)) %>%
   mutate(diff = BooksAdded - BooksRead)
   
ggplot(data=add_read)+
   geom_line(aes(x=Month,y=diff)) +
   scale_y_continuous(name="Net Books",limits = c(-20,50)) +
   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b") +
   geom_hline(yintercept=0, color = "blue") +
   theme(panel.background=element_blank(),
         axis.line = element_line(),
         axis.text.x=element_text(angle=-90),
         axis.title.x=element_blank())

# Cumulative want to read over time
wtr <- add_read %>%
   # Take out the first two months when I added my to-read list
   arrange(Month) %>% slice(-1:-2) %>% 
   mutate(cumdiff=cumsum(diff))
# Use saved tibble to access max y-value
ggplot(data=wtr)+
   geom_line(aes(x=Month,y=cumdiff)) +
   scale_y_continuous(name="Want to Read") +
   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b", expand=c(0,0)) +
   geom_hline(yintercept=max(wtr$cumdiff), color="blue") +
   theme(panel.background=element_blank(),
         axis.line = element_line(),
         axis.text.x=element_text(angle=-90),
         axis.title.x=element_blank())

# How long is a book on my shelf before reading it?
# Histogram view
books %>%
   filter(!is.na(`Date Read`)) %>%
   mutate(daysonlist = as.numeric(`Date Read` - `Date Added`)) %>%
   filter(daysonlist>0) %>%
   ggplot(data=.) +
   geom_histogram(aes(x=daysonlist), binwidth=100,
                  color = "blue",fill="turquoise") +
   xlab("Days on Want to Read") + ylab("") +
   scale_y_continuous(expand=c(0,0))+
   theme(panel.background=element_blank(),
         axis.line= element_line())

# Average number of books read by month of the year
books %>%
      filter(!is.na(`Date Read`)) %>%
      mutate(MonthYear = floor_date(`Date Read`, "month"),
             Month = month(MonthYear, label = TRUE, abbr = TRUE)) %>%
      group_by(MonthYear, Month) %>%
      summarize(sumBooks = length(Title)) %>% ungroup() %>%
      group_by(Month) %>%
      summarize(meanBooks = mean(sumBooks), 
                nMonths = length(sumBooks),
                seBooks = sd(sumBooks)/sqrt(length(sumBooks))) %>% ungroup() %>%
      ggplot(data=.) +
      geom_bar(aes(x=Month, y=meanBooks), stat="identity",
                     fill= "turquoise", color = "blue") +
      geom_errorbar(aes(x=Month, 
                        ymax=meanBooks+seBooks, ymin=meanBooks-seBooks),
                    color = "blue", width = 0.25) +
      scale_y_continuous(name="Number of Books Read", expand = c(0,0)) +
      theme(panel.background = element_blank(),
            axis.line = element_line())
