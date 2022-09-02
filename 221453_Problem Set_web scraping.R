#Q10
#====
library(tidyverse)
library(rvest)
library(stringr)

html = read_html('https://stats.stackexchange.com/questions?tab=Votes')

title = html %>% html_elements('.s-post-summary--content-title a') %>% html_text()
title

views = as.numeric(gsub(' views','',
             html %>% html_elements('.s-post-summary--stats-item.is-supernova') %>% html_attr('title')))
views

answers =as.numeric((html %>% html_elements('.s-post-summary--stats-item-number') %>% html_text())[seq(2,45,3)])
answers

votes = as.numeric((html %>% html_elements('.s-post-summary--stats-item-number') %>% html_text())[seq(1,45,3)])
votes

Stcexchang = data.frame(title, views, answers, votes)
View(Stcexchang)
#======================================

#Q9

html2 = read_html('https://en.wikipedia.org/wiki/United_States_at_the_Olympics')

summer_medals = (html2 %>% html_table())[[4]][3:34, 1:8]
colnames(summer_medals)=(html2 %>% html_table())[[4]][2,]
View(summer_medals)

#======================================

#Q8
html = read_html('https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/')

html %>% html_table()

title = html %>% html_elements('.article_movie_title a') %>% html_text()
title

ranking = html %>% html_elements('.countdown-index') %>% html_text() %>% 
  substring(first = 2) %>% as.numeric()
ranking

tomatoscore = html %>% html_elements('.tMeterScore') %>% html_text() %>%  
      gsub(pattern = '%', replacement = '') %>%  as.numeric()
tomatoscore

year = html %>% html_elements('.subtle.start-year') %>% html_text() %>% str_remove_all(pattern = '[()]')  %>% as.numeric()


FINAL = data.frame(`Movie Title` = title,
                   Rank = ranking,
                   `Tomato % score` = tomatoscore,
                   `Year of Release` = year)
view(FINAL)

