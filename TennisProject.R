library(macleish)
library(tidyverse)
library(ggplot2)
library(mosaic)
library(mdsr)
library(dplyr)
library(backports)
library(rvest)
library(purrr)


#### Winning Percentage and Year End Rank (GRAPH 1) ####

TennisURL <- "http://www.tennisabstract.com/cgi-bin/player.cgi?p=RogerFederer"

Tennis0 <- TennisURL %>% 
  read_html() %>% 
  html_nodes("table")

Federer2021 <- Tennis0 %>%
  purrr::pluck(40) %>%
  html_table()

Federer2021

#Getting Federer Data
FedRaw <- read_sheet("https://docs.google.com/spreadsheets/d/1r6yZgFBGaA7XDERUiweZb6l0NbfN60bC94T2dVQXf2g/edit#gid=0")
Federer <- FedRaw %>%
  mutate(Year_Mod = as.integer(Year)) %>%
  mutate(PlayerID = 1)%>%
  mutate(Name = "Federer")
  
glimpse(Federer)

FedSummary = FedCareer[25,]

#Federer over time
ggplot(Federer, aes(x = Year_Mod, y =`Win%`))+
  geom_point() +
  geom_line

#Getting Djokovic Data
NovRaw <- read_sheet("https://docs.google.com/spreadsheets/d/1r6yZgFBGaA7XDERUiweZb6l0NbfN60bC94T2dVQXf2g/edit#gid=549441777", 
                     range = "NovCareer")
Djokovic <- NovRaw %>%
  mutate(Year_Mod = as.integer(Year)) %>%
  mutate(PlayerID = 2) %>%
  mutate(Name = "Djokovic")

glimpse(Djokovic)
FedSummary = FedCareer[25,]

ggplot(Djokovic, aes(x = Year_Mod, y =`Win%`))+
  geom_line() +
theme_light()+ 
  labs(
    x = "Year",
    title = "Winning Percentage Over Time",
    subtitle = "For Novak Djokovic",
    caption = "Data via Tennis Abstract (tennisabstract.com)"
  )


#Getting Nadal Data
NadalRaw <- read_sheet("https://docs.google.com/spreadsheets/d/1r6yZgFBGaA7XDERUiweZb6l0NbfN60bC94T2dVQXf2g/edit#gid=1490556656", 
                      range = "NadalCareer")
Nadal <- NadalRaw %>%
  mutate(Year_Mod = as.integer(Year)) %>%
  mutate(PlayerID = 3) %>%
  mutate(Name = "Nadal")

glimpse(Nadal)
RafaSummary = RafaCareer[25,]

ggplot(Nadal, aes(x = Year_Mod, y =`Win%`))+
  geom_point() + 
  geom_line()

#Combine into one dataset
CareersCombined <- rbind(Federer,Djokovic ,Nadal)
glimpse(CareersCombined)
#All on one plot looking at win percentage
ggplot(CareersCombined, aes (x = Year_Mod, y= `Win%`, color = Name)) +
  geom_line() +
  theme_light()+ 
  labs(
    x = "Year",
    title = "Match Winning Percentage Over Time",
    subtitle = "For the  'Big Three' Tennis Players",
    caption = "Data via Tennis Abstract (tennisabstract.com)"
  )

  
#Now calculating AGE of tennis player
CareersCombined <- CareersCombined %>% 
  mutate(Age = case_when(
    PlayerID == 1 ~ (Year_Mod - 1981),
    PlayerID == 2 ~ (Year_Mod - 1987),
    PlayerID == 3 ~ (Year_Mod - 1986),
    TRUE ~ 1000
    )
  )

#All on one plot looking at win percentage by AGE
ggplot(CareersCombined, aes (x = Age, y= `Win%`, color = Name)) +
  geom_line() +
  theme_light()+ 
  labs(
    x = "Age",
    title = "Match Winning Percentage by Age",
    subtitle = "For the  'Big Three' Tennis Players",
    caption = "Data via Tennis Abstract (tennisabstract.com)"
  )

#Now we want to do RANKINGS

#Getting Federer Data
FedRank <- read_sheet("https://docs.google.com/spreadsheets/d/1r6yZgFBGaA7XDERUiweZb6l0NbfN60bC94T2dVQXf2g/edit#gid=300540124", 
                      range = "FedRankings")
FedRank <- FedRank %>%
  mutate(Year_Mod = as.integer(Year)) %>%
  mutate(RankOne = case_when(
    `ATP Rank` == 1 ~ (TRUE),
    TRUE ~ FALSE
  ))

#Getting Nadal Data
RafaRank <- read_sheet("https://docs.google.com/spreadsheets/d/1r6yZgFBGaA7XDERUiweZb6l0NbfN60bC94T2dVQXf2g/edit#gid=300540124", 
                      range = "NadalRankings")
RafaRank <- RafaRank %>%
  mutate(Year_Mod = as.integer(Year)) %>%
  mutate(RankOne = case_when(
    `ATP Rank` == 1 ~ (TRUE),
    TRUE ~ FALSE
  ))

#Getting Djokovic Data
NovRank <- read_sheet("https://docs.google.com/spreadsheets/d/1r6yZgFBGaA7XDERUiweZb6l0NbfN60bC94T2dVQXf2g/edit#gid=300540124", 
                       range = "NovRankings")
NovRank <- NovRank %>%
  mutate(Year_Mod = as.integer(Year)) %>%
  mutate(RankOne = case_when(
    `ATP Rank` == 1 ~ (TRUE),
    TRUE ~ FALSE
  ))


#Now to inner join

#Federer
FedCombined <- Federer %>%
  inner_join(FedRank, by = c("Year_Mod" = "Year_Mod"))

#Nadal
RafaCombined <- Nadal %>%
  inner_join(RafaRank, by = c("Year_Mod" = "Year_Mod"))

#Djokovic
NovCombined <- Djokovic %>%
  inner_join(NovRank, by = c("Year_Mod" = "Year_Mod"))

CareerRankCombined <- rbind(FedCombined,RafaCombined ,NovCombined)

#Adding Age Again
CareerRankCombined <- CareerRankCombined %>% 
  mutate(Age = case_when(
    PlayerID == 1 ~ (Year_Mod - 1981),
    PlayerID == 2 ~ (Year_Mod - 1987),
    PlayerID == 3 ~ (Year_Mod - 1986),
    TRUE ~ 1000
  )
  )

glimpse(CareerRankCombined)
  

#All on one plot looking at win percentage WITH SHAPES
ggplot(CareerRankCombined, aes(x= Year_Mod, y = `Win%`))+
  geom_point(shape = CareerRankCombined$RankOne)+
  geom_line(aes(color = Name))+
  theme_light()+ 
  labs(
    x = "Year",
    title = "Match Winning Percentage Over Time",
    subtitle = "For the  'Big Three' Tennis Players, (circular point corresponds with #1 Rank)",
    caption = "Data via Tennis Abstract (tennisabstract.com)"
  )

#All on one plot looking at win percentage by age WITH SHAPES
ggplot(CareerRankCombined, aes(x= Age, y = `Win%`))+
  geom_point(shape = CareerRankCombined$RankOne)+
  geom_line(aes(color = Name))+
  theme_light()+ 
  labs(
    x = "Age",
    title = "Match Winning Percentage by Age",
    subtitle = "For the  'Big Three' Tennis Players, (circular point corresponds with #1 Rank)",
    caption = "Data via Tennis Abstract (tennisabstract.com)"
  )

FedNumOne <- CareerRankCombined %>%
  filter(RankOne == TRUE & Name == "Federer") %>%
  nrow()

NovNumOne <- CareerRankCombined %>%
  filter(RankOne == TRUE & Name == "Djokovic") %>%
  nrow()

RafaNumOne <- CareerRankCombined %>%
  filter(RankOne == TRUE & Name == "Nadal") %>%
  nrow()

Names = c("Federer", "Djokovic", "Nadal")
Years_Number_One = c(FedNumOne, NovNumOne, RafaNumOne)

YearsNumOne = data.frame(Names, Years_Number_One)
YearsNumOne <- YearsNumOne %>%
  arrange(desc(Years_Number_One))

#### End ####

#### Majors and Titles (GRAPH 2) ####

#Australian, Roland Garros, Wimbledon, US Open
#Federer, Djokovic, Nadal


#Federer Majors
FedMajors <- read_sheet("https://docs.google.com/spreadsheets/d/1r6yZgFBGaA7XDERUiweZb6l0NbfN60bC94T2dVQXf2g/edit#gid=0",
                     range = "FedMajors")

FedMajors <- FedMajors %>%
  mutate(PlayerID = 1)%>%
  mutate(Name = "Federer") %>%
  mutate(result = ...7) %>%
  mutate(WinLoss = "Win")


#Found this code to make the function easier! Via https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

for (n in 1:nrow(FedMajors)) {

  if(substrRight(FedMajors[n,]$result,7) == "Federer") {
    FedMajors[n,]$WinLoss = "Loss"
  }
}

glimpse(FedMajors)
FedMajorWins <- FedMajors %>%
  filter(WinLoss == "Win") %>%
  filter (Tournament == "Wimbledon" | Tournament == "Roland Garros" |Tournament ==  "Australian Open" | Tournament == "US Open")

FedNumTitles <- c(FedMajorWins %>% filter(Tournament == "Wimbledon") %>% nrow(),
                  FedMajorWins %>% filter(Tournament == "Roland Garros") %>% nrow(), 
                  FedMajorWins %>% filter(Tournament == "Australian Open") %>% nrow(), 
                  FedMajorWins %>% filter(Tournament == "US Open") %>% nrow()
                  )



#Nadal Majors
RafaMajors <- read_sheet("https://docs.google.com/spreadsheets/d/1r6yZgFBGaA7XDERUiweZb6l0NbfN60bC94T2dVQXf2g/edit#gid=0",
                        range = "NadalMajors")

RafaMajors <- RafaMajors %>%
  mutate(PlayerID = 3) %>%
  mutate(Name = "Nadal") %>%
  mutate(result = ...7) %>%
  mutate(WinLoss = "Win")

for (n in 1:nrow(RafaMajors)) {
  
  if(substrRight(RafaMajors[n,]$result,5) == "Nadal") {
    RafaMajors[n,]$WinLoss = "Loss"
  }
}

RafaMajorWins <- RafaMajors%>%
  filter(WinLoss == "Win") %>%
  filter (Tournament == "Wimbledon" | Tournament == "Roland Garros" |Tournament ==  "Australian Open" | Tournament == "US Open")

RafaMajorTitles <- c(RafaMajorWins %>% filter(Tournament == "Wimbledon") %>% nrow(),
                     RafaMajorWins %>% filter(Tournament == "Roland Garros") %>% nrow(), 
                     RafaMajorWins %>% filter(Tournament == "Australian Open") %>% nrow(), 
                     RafaMajorWins %>% filter(Tournament == "US Open") %>% nrow()
)

#Djokovic Majors
NovMajors <- read_sheet("https://docs.google.com/spreadsheets/d/1r6yZgFBGaA7XDERUiweZb6l0NbfN60bC94T2dVQXf2g/edit#gid=0",
                         range = "NovMajors")

NovMajors <- NovMajors %>%
  mutate(PlayerID = 3) %>%
  mutate(Name = "Djokovic") %>%
  mutate(result = ...7) %>%
  mutate(WinLoss = "Win")

for (n in 1:nrow(NovMajors)) {
  
  if(substrRight(NovMajors[n,]$result,8) == "Djokovic") {
    NovMajors[n,]$WinLoss = "Loss"
  }
}

NovMajorWins <- NovMajors%>%
  filter(WinLoss == "Win") %>%
  filter (Tournament == "Wimbledon" | Tournament == "Roland Garros" |Tournament ==  "Australian Open" | Tournament == "US Open")

NovMajorTitles <- c(NovMajorWins %>% filter(Tournament == "Wimbledon") %>% nrow(),
                    NovMajorWins %>% filter(Tournament == "Roland Garros") %>% nrow(), 
                    NovMajorWins %>% filter(Tournament == "Australian Open") %>% nrow(), 
                    NovMajorWins %>% filter(Tournament == "US Open") %>% nrow()
)

#Fitting into one place (data creation found on R graph gallery - https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2)

Name <- c(rep("Federer" , 4) , rep("Nadal" , 4) , rep("Djokovic" , 4))
Tournament <- rep(c("Wimbledon" , "Roland Garros" , "Australian Open", "US Open") , 3)
NumWins <- c(FedNumTitles, RafaMajorTitles, NovMajorTitles)
DataForBar <- data.frame(Name,Tournament,NumWins)

ggplot(DataForBar, aes(fill=Tournament, y=NumWins, x=Name)) + 
  geom_bar(position="stack", stat="identity") +
  theme_light() + 
  labs(
    x = "Name",
    y = "Number of Grand Slams Won",
    title = "Figure 2: Grand Slam Titles",
    subtitle = "For the  'Big Three' Tennis Players",
    caption = "Data via Tennis Abstract (tennisabstract.com)"
  )

#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2

#### End ####

