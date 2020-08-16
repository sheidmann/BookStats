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
books <- read_csv("data/goodreads_library_export_20200816.csv",
                  col_types = cols(`Date Read`=col_date(format="%D"),
                                   `Date Added`=col_date(format="%D")))

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
