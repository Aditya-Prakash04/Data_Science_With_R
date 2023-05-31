# QUESTION "e"

library(tidyverse)
library(rvest)
html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
Name_of_movie <- html %>% html_elements(".article_movie_title a") %>% html_text()
Name_of_movie
Year_of_movie <- html %>% html_elements(".subtle.start-year") %>% html_text() %>% substr(2,5) %>% as.numeric()
Year_of_movie
Tomato_score <- html %>% html_elements(".tMeterScore") %>% html_text()  
Tomato_score
Ranking <- html %>% html_elements(".countdown-index-resposive") %>% html_text()
Ranking
Best100movies <- data.frame("NAME" = Name_of_movie,"YEAR" = Year_of_movie,"RANKING" = Ranking,"SCORE" = Tomato_score)
view(Best100movies)

#-------------------------------------------------------------------------#

# QUESTION "d"
# 1st Part
MontyHall <- function(){
  doors <- c("car","goat","goat")
  shuffled_doors <- sample(doors,length(doors)) # shuffled the doors vector
  choosen_door <- sample(1:3,1) # randomly choosen a door
  opened_door <- which(shuffled_doors[-choosen_door] == "goat")# return at which index it equals to "goat"
  remaining_doors <- setdiff(1:3,c(opened_door,choosen_door))# the door which was left at the end 
  
  if("car" %in% shuffled_doors[remaining_doors]){ # checks whether car is present at that index
    return(1)
    
  }
  else{
    return(0)
  }
  
}
# 2nd Part

n <- 1000
wins <- 0
for(i in 1:1000){
  result <- MontyHall()
  wins <- wins + result
}
total_probabilty <- wins/n
total_probabilty
#-----------------------------------------------------------------------------#
#OUESTION "c"
# 1st Part
tennis <- function(p){
  x <- 0
  while (x<5) {
    if(runif(1)<p){
      x <- x +1
    }
    else{
      break
    }
    
  }
  return(x)
}
# 2
matches <- vector(length = 1000)
for(i in 1:1000)
{
  matches[i] <- tennis(0.70)
}
ans <- mean(matches)
ans

#-----------------------------------------------------------------------#

# OUESTION "a"

hmtl1 <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
a <- html %>% html_table()
Company_Name <- a[[1]]$`Company Name (M.Cap)`%>% str_remove_all("[\n]")
CMP <- a[[1]]$CMP
Price_Change <- a[[1]]$`Price Change`
Market_Cap <- a[[1]]$`Market Cap (Cr)`
WH <- a[[1]]$`52 Week High`
WL <- a[[1]]$`52 Week Low`
ROE <- a[[1]]$ROE
PE <- a[[1]]$`P/E`
pvb <- a[[1]]$`P/BV`
ev <- a[[1]]$`EV/EBITDA`
s <- a[[1]]$`5YSales Gr(%)`
p <- a[[1]]$`5YProfit Gr(%)`

TABLE <- data.frame("Company Name (M.Cap)" = Company_Name,"CMP" = CMP,"Price Change" = Price_Change,"MarketCap(Cr)" = Market_Cap,"52WeekHigh" = WH,"52WeekLow" = WL,"ROE" = ROE,"P/E" = PE,"P/BV" = pvb,"EV/EBITDA" = ev,"5YSalesGr(%)" = s,"5YProfitGr(%)" = p)
view(TABLE)

# "b"
# HCL Technologies Ltd
html2 <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/it-ites/it-software/hcl-tech/company-info")
A <- html2 %>% html_table(header = FALSE)
a <- A[[1]]$X1
b <- A[[3]]$X1
c <-c(a,b)
c <- c[-c(1,2,3,4,5,6,15,16)] 

a <- A[[1]]$X2
b <- A[[3]]$X2
C <- c(a,b)
C <- C[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X3
b <- A[[3]]$X3
d <-c(a,b)
d <- d[-c(1,2,3,4,5,6,15,16)]

a <-A[[1]]$X4
b <-A[[3]]$X4
D <- c(a,b)
D <- D[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X5
b <- A[[3]]$X5
e <-c(a,b)
e <- e[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X6
b <- A[[3]]$X6
E <-c(a,b)
E <- E[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X7
b <- A[[3]]$X7
f <-c(a,b)
f <- f[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X8
b <- A[[3]]$X8
F <-c(a,b)
F <- F[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X9
b <- A[[3]]$X9
g <-c(a,b)
g <- g[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X10
b <- A[[3]]$X10
G <-c(a,b)
G <- G[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X11
b <- A[[3]]$X11
h <-c(a,b)
h <- h[-c(1,2,3,4,5,6,15,16)]


HCL_data <- data.frame("." = c,"Jun'13" = C,"Jun'14"=d,"Jun'15"=D,"Mar'16"=e,"Mar'17"=E,"Mar'18"=f,"Mar'19"=F,"Mar'20"=g,"Mar'21"=G,"Mar'22"=h)
view(HCL_data)

#----------------------------------------------------------------------------#
# Tech Mahindra Ltd
html3 <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/it-ites/it-software/tech-mahindra/company-info")
A <- html3 %>% html_table(header =FALSE)
a <- A[[1]]$X1
b <- A[[3]]$X1
c <-c(a,b)
c <- c[-c(1,2,3,4,5,6,15,16)] # item name of hcl

a <- A[[1]]$X2
b <- A[[3]]$X2
C <- c(a,b)
C <- C[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X3
b <- A[[3]]$X3
d <-c(a,b)
d <- d[-c(1,2,3,4,5,6,15,16)]

a <-A[[1]]$X4
b <-A[[3]]$X4
D <- c(a,b)
D <- D[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X5
b <- A[[3]]$X5
e <-c(a,b)
e <- e[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X6
b <- A[[3]]$X6
E <-c(a,b)
E <- E[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X7
b <- A[[3]]$X7
f <-c(a,b)
f <- f[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X8
b <- A[[3]]$X8
F <-c(a,b)
F <- F[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X9
b <- A[[3]]$X9
g <-c(a,b)
g <- g[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X10
b <- A[[3]]$X10
G <-c(a,b)
G <- G[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X11
b <- A[[3]]$X11
h <-c(a,b)
h <- h[-c(1,2,3,4,5,6,15,16)]


TataMahindra_data <- data.frame("." = c,"Jun'13" = C,"Jun'14"=d,"Jun'15"=D,"Mar'16"=e,"Mar'17"=E,"Mar'18"=f,"Mar'19"=F,"Mar'20"=g,"Mar'21"=G,"Mar'22"=h)
view(TataMahindra_data)

#-----------------------------------------------------------------------------#

#Bharti Airtel Ltd

html4 <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/telecom/telecommunication-service-provider/bharti-airtel/company-info")
A <- html4 %>% html_table(header =FALSE)
a <- A[[1]]$X1
b <- A[[3]]$X1
c <-c(a,b)
c <- c[-c(1,2,3,4,5,6,15,16)] # item name of hcl

a <- A[[1]]$X2
b <- A[[3]]$X2
C <- c(a,b)
C <- C[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X3
b <- A[[3]]$X3
d <-c(a,b)
d <- d[-c(1,2,3,4,5,6,15,16)]

a <-A[[1]]$X4
b <-A[[3]]$X4
D <- c(a,b)
D <- D[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X5
b <- A[[3]]$X5
e <-c(a,b)
e <- e[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X6
b <- A[[3]]$X6
E <-c(a,b)
E <- E[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X7
b <- A[[3]]$X7
f <-c(a,b)
f <- f[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X8
b <- A[[3]]$X8
F <-c(a,b)
F <- F[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X9
b <- A[[3]]$X9
g <-c(a,b)
g <- g[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X10
b <- A[[3]]$X10
G <-c(a,b)
G <- G[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X11
b <- A[[3]]$X11
h <-c(a,b)
h <- h[-c(1,2,3,4,5,6,15,16)]


Airtel_data <- data.frame("." = c,"Jun'13" = C,"Jun'14"=d,"Jun'15"=D,"Mar'16"=e,"Mar'17"=E,"Mar'18"=f,"Mar'19"=F,"Mar'20"=g,"Mar'21"=G,"Mar'22"=h)
view(Airtel_data)

#---------------------------------------------------------------------------#

html5 <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/it-ites/it-software/wipro/company-info")
A <- html5 %>% html_table(header =FALSE)
a <- A[[1]]$X1
b <- A[[3]]$X1
c <-c(a,b)
c <- c[-c(1,2,3,4,5,6,15,16)] # item name of hcl

a <- A[[1]]$X2
b <- A[[3]]$X2
C <- c(a,b)
C <- C[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X3
b <- A[[3]]$X3
d <-c(a,b)
d <- d[-c(1,2,3,4,5,6,15,16)]

a <-A[[1]]$X4
b <-A[[3]]$X4
D <- c(a,b)
D <- D[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X5
b <- A[[3]]$X5
e <-c(a,b)
e <- e[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X6
b <- A[[3]]$X6
E <-c(a,b)
E <- E[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X7
b <- A[[3]]$X7
f <-c(a,b)
f <- f[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X8
b <- A[[3]]$X8
F <-c(a,b)
F <- F[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X9
b <- A[[3]]$X9
g <-c(a,b)
g <- g[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X10
b <- A[[3]]$X10
G <-c(a,b)
G <- G[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X11
b <- A[[3]]$X11
h <-c(a,b)
h <- h[-c(1,2,3,4,5,6,15,16)]


Wipro_data <- data.frame("." = c,"Jun'13" = C,"Jun'14"=d,"Jun'15"=D,"Mar'16"=e,"Mar'17"=E,"Mar'18"=f,"Mar'19"=F,"Mar'20"=g,"Mar'21"=G,"Mar'22"=h)
view(Wipro_data)

#-----------------------------------------------------------------------------#
html6 <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/healthcare/pharmaceuticals-drugs/cipla/company-info")
A <- html6 %>% html_table(header = FALSE)
a <- A[[1]]$X1
b <- A[[3]]$X1
c <-c(a,b)
c <- c[-c(1,2,3,4,5,6,15,16)] # item name of hcl

a <- A[[1]]$X2
b <- A[[3]]$X2
C <- c(a,b)
C <- C[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X3
b <- A[[3]]$X3
d <-c(a,b)
d <- d[-c(1,2,3,4,5,6,15,16)]

a <-A[[1]]$X4
b <-A[[3]]$X4
D <- c(a,b)
D <- D[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X5
b <- A[[3]]$X5
e <-c(a,b)
e <- e[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X6
b <- A[[3]]$X6
E <-c(a,b)
E <- E[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X7
b <- A[[3]]$X7
f <-c(a,b)
f <- f[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X8
b <- A[[3]]$X8
F <-c(a,b)
F <- F[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X9
b <- A[[3]]$X9
g <-c(a,b)
g <- g[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X10
b <- A[[3]]$X10
G <-c(a,b)
G <- G[-c(1,2,3,4,5,6,15,16)]

a <- A[[1]]$X11
b <- A[[3]]$X11
h <-c(a,b)
h <- h[-c(1,2,3,4,5,6,15,16)]


Cipla_data <- data.frame("." = c,"Jun'13" = C,"Jun'14"=d,"Jun'15"=D,"Mar'16"=e,"Mar'17"=E,"Mar'18"=f,"Mar'19"=F,"Mar'20"=g,"Mar'21"=G,"Mar'22"=h)
view(Cipla_data)

