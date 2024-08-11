library(shiny)
library(tidyverse)
library(bslib)
library(maps)
library(sf)
library(thematic)
library(DT)

PL = read_csv("Data/Recent Premier League Standings.csv")
teams = read_csv("Data/Premier League Teams.csv")
uiuc = read_csv("Data/UIUC Students by State.csv")

state_map = st_as_sf(map("state", plot = FALSE, fill = TRUE))
state_map = state_map %>%
  mutate(State = tolower(ID))

# Define UI for application
ui <- navbarPage(
  title = "Datasets",
  theme = bs_theme(bootswatch = "minty"),
  tabPanel(title = "Premier League Data",
           sidebarLayout(
             #Data input
             sidebarPanel(
               radioButtons("gdpts", 
                            "Select Display: Goal Difference or Points",
                            choices = c("GD", "Pts")),
               textInput("teamsuser",
                         "Enter name of team (Comma Separated):"),
               tableOutput("teamnames")
             ),
             
             #Visualizations
             mainPanel(
               plotOutput("barplot"),
               downloadButton("downloadplot", "Download Plot as PNG")
               )
             )
           ),
  tabPanel(title = "UIUC Student Data",
           sidebarLayout(
             sidebarPanel(
               sliderInput("year",
                           "Select Year:",
                           min = min(uiuc$Year),
                           max = max(uiuc$Year),
                           value = max(uiuc$Year),
                           step = 1,
                           sep = ""),
               DTOutput("statesdatatable")
             ),
             mainPanel(
               plotOutput("choroplethmap"),
               downloadButton("downloadmap", "Download Map as PNG")
               )
             )
           )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  thematic::thematic_shiny()
  
  output$barplot = renderPlot({
    req(input$gdpts, input$teamsuser)
    team_list = str_split(str_to_title(input$teamsuser), pattern = ",\\s*")[[1]]
    
    p1 = PL %>% 
      filter(Team %in% team_list) %>% 
      ggplot(aes(x = Season, 
                 y = .data[[input$gdpts]],
                 fill = Team)) +
      geom_bar(position = "dodge", stat = "identity")
    
    if (input$gdpts == "GD") {
      p1 = p1 +
        labs(title = "Premier League Teams Goal Difference in Last Five Seasons", 
             x = "Season", 
             y = "Goal Difference",
             caption = "Goal Difference is calculated as the number of goals scored minus the number of goals a team conceded. This is used as a tiebreaker between teams that finish with the same amount of points in a season or tournament.") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    } else {
      p1 = p1 +
        labs(title = "Premier League Teams Total Points in Last Five Seasons", 
             x = "Season", 
             y = "Points",
             caption = "Some teams don't appear in all seasons meaning they were in a league below the Premier League.") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.caption = element_text(size = 12))
    }
    
    p1
  })
  
  output$teamnames = renderTable({
    teams
  })
  
  filtered_data = reactive({
    uiuc %>%
      filter(Year == input$year) %>%
      group_by(State) %>%
      summarise(Total = sum(Total)) %>% 
      mutate(State = tolower(State))
  })
  
  output$choroplethmap = renderPlot({
    choropleth_data = state_map %>%
      left_join(filtered_data(), by = c("State" = "State"))
    
    ggplot(choropleth_data) +
      geom_sf(aes(fill = Total)) +
      scale_fill_viridis_c(trans = "log", 
                           breaks = c(0, 10, 20, 30, 40, 50, 100, 500, 1000, 5000, 40000), 
                           guide = guide_legend(keyheight = unit(10, units = "mm"))) +
      theme_void() +
      labs(title = paste("UIUC Students by State (", input$year, ")", sep = ""),
           fill = "Total Students")
  })
  
  output$statesdatatable <- renderDT({
    datatable(filtered_data(), 
              options = list(pageLength = 10), 
              rownames = FALSE)
  })
  
  output$downloadplot = downloadHandler(
    filename = function() {
      paste("Premier League Team Performace - ", Sys.time(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  output$downloadmap = downloadHandler(
    filename = function() {
      paste("UIUC Students by State (", input$year, ")- ", Sys.time(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
