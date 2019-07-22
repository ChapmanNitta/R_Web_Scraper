library("rvest")
library("googleVis")
library("tidyverse")
library("xml2")

#Access the specific movie theater's show times web page from your R code.
#. Import the HTML content of the show times web page into an R object called"Showtimes". 

URI <- 'https://www.cinemark.com/pennsylvania/cinemark-university-city-penn-6?showDate='

getShowtimes <- function(date) {
  URL <- paste0(URI, date)
  read_html(URL)
}

Showtimes <- getShowtimes('05/04/2019')
Showtimes

#Save this content to a file and explain in your report how to reload it (in case your grader 
#needs to reload the show times as they may change between after you submit your project but before your grader can grade it).
write_html(Showtimes, file = 'showtime.Rdata')

Showtimes <- xml2::read_html('showtime.Rdata')
#Scrape the show times web page to extract the list of movies currently playing.
#. Create an R list object called "Program" containing all movie titles currently showing in
#the theater. (Most likely you will get this from the "Showtimes" object).

getPrograms <- function(showtimes){
  html_nodes(showtimes, "div.showtimeMovieBlock h2") %>%
    html_text() %>%
    as.list()
}

Program <- getPrograms(Showtimes)
Program



#Choose a particular movie title and scrape the R object "Showtimes" To extract the times at which this particular movie is playing
Showtimes %>%
  html_nodes(xpath = '//*[@class="showtimeMovieBlock 76838 "]')%>% #Needed to use xpath as html_nodes alone do not support numerical values as an argument parameter
  html_nodes(., "div.showtime") %>%
  html_text() %>%
  { gsub("\r\n", "", .) } %>% # Need to encompass piped data in curly brackets and use a period as a data marker
  trimws()

#d) (25 points) Identify all of the links from R object "Showtimes" to the other web pages contained
#the movie titles currently playing in the theater.
#. Create an R list object out of them called "Page_Links" and save it to a file.

Page_Links <- html_nodes(Showtimes, "a.movieLink") %>%
  html_attr("href") %>%
  unique() %>%
  as.list()

save(Page_Links ,file = "Page_Links.RData")

load(file = "Page_Links.RData")

typeof(Page_Links)

Page_Links

  
#. Use a hierarchical visualization technique (perhaps gvisOrgChart()) to show the
#relationship between the web pages that were scraped.

df <- data.frame(matrix(unlist(Page_Links), nrow=length(Page_Links), byrow=T))

df <- cbind(df, 'https://www.cinemark.com/pennsylvania/cinemark-university-city-penn-6?showDate=05/04/2019', 'Cinemark')

colnames(df) <- c("Child", "Parent", "Placeholder")

de <- data.frame('https://www.cinemark.com/pennsylvania/cinemark-university-city-penn-6?showDate=05/04/2019', NA, 'Cinemark')
colnames(de) <- c("Child", "Parent", "Placeholder")

df <- rbind(df, de)

hierarchy <- gvisOrgChart(df, idvar = "Child", parentvar = "Parent", options=list(width="200px", height="300px"))

plot(hierarchy)

