library(tidyverse)

#Import Data & check for missing data
soccer = read_csv("Data/Original Data/premier-league-standings.csv")
illinois = read_csv("Data/Original Data/uiuc-students-by-state.csv")

soccer %>% 
  dplyr::select(everything()) %>% 
  summarise_all(~sum(is.na(.))) %>% 
  print(width = Inf)

illinois %>% 
  dplyr::select(everything()) %>% 
  summarise_all(~sum(is.na(.))) %>% 
  print(width = Inf)

#Check distinct values
soccer %>% 
  dplyr::select(everything()) %>% 
  summarise_all(~n_distinct(.)) %>% 
  print(width = Inf)

illinois %>% 
  dplyr::select(everything()) %>% 
  summarise_all(~n_distinct(.)) %>% 
  print(width = Inf)

#Tidy
soccer = soccer %>% 
  dplyr::select(Season, Team, GD, Pts) %>% 
  separate_wider_position(Season, c(2, Season = 5)) %>% 
  filter(Season == "17-18" | Season == "18-19" | Season == "19-20" | Season == "20-21" | Season == "21-22")

illinois = illinois %>% 
  dplyr::select(State, Year, Total) %>% 
  filter(!(State == "Military" | State == "Guam" | State == "Unknown" | State == "Virgin Islands" | State == "Puerto Rico"))


#Create new data files
write_csv(soccer, "Recent Premier League Standings.csv")
write_csv(illinois, "UIUC Students by State.csv")

pl_teams = soccer %>% 
  distinct(Team) %>% 
  arrange(Team)
write_csv(pl_teams, "Premier League Teams.csv")
