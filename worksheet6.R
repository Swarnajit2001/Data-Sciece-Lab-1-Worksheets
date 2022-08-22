#====================================================================
library(tidyverse)
library(rvest)


html <- read_html("https://www.iitk.ac.in/math/faculty")
html
name <- html_elements(html, ".head3")
name
name <- html_elements(name, "a")
name
name <- html_text(name)
name

name <- html_elements(html, ".head3 a")
name <- html_text(name)
name

name <- html %>% html_elements(".head3 a") %>% html_text()
name
#====================================================================


#WORKSHEET 6
#=============


#Q1
#----
html1 = read_html('https://iitk.ac.in/math/visitors-post-doctoral-fellow')
Post_Doc = html1 %>% html_elements(".head2") %>% html_text()
Post_Doc
#=============================

#Q2
#---

html2 <- read_html("https://www.imdb.com/chart/top/")

top250 = html2 %>% html_elements('.titleColumn a') %>% html_text()
top250

rating = as.numeric(html2 %>% html_elements('strong') %>% html_text())
rating

year.txt = html2 %>% html_elements('.secondaryInfo') %>% html_text()

year = as.numeric(year.txt %>% substring(first = 2, last = nchar(year.txt)-1))
year

rating = as.numeric(html2 %>% html_elements('span') %>% html_attr('name') %>% html_text())
rating
