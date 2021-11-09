library(tidyverse)
library(rvest)
library(scales)

#selecting Site for data scrape
results<-read_html('https://racesonline.com/events/littlerockmarathon/results/2020-marathon-half-marathon?age_group_id=&category_id=15751&commit=Search&division_id=5401&gender=&page=2&search_term_display=&utf8=✓')

#Idetnifying the table
df<-results %>% 
  html_node("table") %>% 
  html_table()

#function to scrape by page
get_data <- function(pg) {
  url <- paste0('https://racesonline.com/events/littlerockmarathon/results/2020-marathon-half-marathon?age_group_id=&category_id=15751&commit=Search&division_id=5401&gender=&page=',pg,'&search_term_display=&utf8=✓')
  url2<-read_html(url)
  df<- url2 %>% html_node("table") %>% html_table()
  return(df)
}

#mapping all 5 pages of finishers
final_dat<-1:5 %>% purrr::map(function(x) get_data(x))

#converting to data frame
bigresults <- plyr::ldply(final_dat,data.frame)

glimpse(bigresults)

#plotting times 
#plot1<-
  bigresults %>% filter() %>%  mutate(Gun.Time=hms(Gun.Time),
                      Elapsed.Time=hms(Elapsed.Time,roll=TRUE),
                      X10K.Split=hms(X10K.Split),
                      X13.1.Split=hms(X13.1.Split),
                      X22M.Split=hms(X22M.Split)) %>% 
  ggplot() +
  geom_point(aes(y=Elapsed.Time,x=X13.1.Split),color='red') +
  geom_point(aes(y=Elapsed.Time,x=X22M.Split),color='blue') +
  scale_x_time(breaks = scales::breaks_width("20 min"),
               labels = label_time('%H:%M'),minor_breaks = '%H:%M')+
  scale_y_time(breaks = scales::breaks_width("20 min"),
               labels = label_time('%H:%M')) +
    labs(y='Final Time',x='Split Time Red=13.1M, Blue=22M',
         title='2020 Little Rock Marathon Final Time by Split Time',
         caption='www.racesonline.com')+
    theme(
    panel.background = element_rect(fill = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major.x = element_line(size=.5,color='grey'),
    panel.grid.major.y = element_line( size=.5, color="grey" ),
    plot.background = element_rect(fill = "grey"))
  

  ggsave('plot1.png',path='/Users/jimauer/Documents/GitHub/Marathon',width=5,height=3,units='in')

plot2 <- bigresults %>% select(Bib, 
                      #X10K.Split,
                      X13.1.Split, 
                      X22M.Split, 
                      Elapsed.Time) %>% 
  mutate(#X10K.Split=hms(X10K.Split),
                      X13.1.Split=hms(X13.1.Split),
                      X22M.Split=hms(X22M.Split),
                      Elapsed.Time=hms(Elapsed.Time)) %>% 
  pivot_longer(!Bib,names_to = 'split',values_to = 'time') %>%
  mutate(split = factor(split, levels=c("X13.1.Split", "X22M.Split", "Elapsed.Time"))) %>%
  ggplot(aes(x=split,y=time)) + geom_boxplot() + 
  scale_y_time() 

ggsave('plot2.png',path='/Users/jimauer/Documents/GitHub/Marathon',width=5,height=3,units='in')  

smallresults %>% filter(split=='Elapsed.Time') %>% cut(.,breaks='30 minutes')

