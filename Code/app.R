library(shiny)
library(rsconnect)
library(shinythemes)
library(tidyverse) # Data Cleaning, manipulation, summarization, plotting
library(nflreadr)
library(nflplotR)
library(scales)
library(viridis)
library(ggrepel) # better labels
library(ggthemes) # custom pre-built themes

player <- read.csv("player.csv")
avg <- read.csv("avg.csv")

names <- unique(player$player)


prob <- fluidPage(theme = shinytheme("flatly"),
                  
  titlePanel("Pressure Probability Visualisation App"),
  
  sidebarLayout(
    sidebarPanel(fluidRow(
                   column(12, align = "center",
                          selectizeInput('name', 'Select Pass Rusher:', choices = sort(names), selected="Aaron Donald",
                                          multiple = TRUE, options = list(maxItems = 1)))
                   ),
                 downloadButton(outputId = "Download",
                                 label = "Download Plot")
                 ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
                tabPanel("Probability",
                          plotOutput(outputId = "prob_graph",
                                      width = "100%"))
        ))
    ))

server <- function(input, output) { 
  
  prob <- function(){
    
    player0 <- player |>
      filter(player==input$name)
    
    color1 <- unique(player0$team_color)
    #pos <- ifelse(max(player0$yfit)<0.3, 0.3, max(player0$yfit))
    #true_false <- unique(ifelse(player0$top55==1, T, F))
    
    avg |>
      ggplot(aes(x=seconds, y=prob)) +
      geom_smooth(data=player0, aes(x=seconds, y=prob), 
                  color = color1, show.legend = NA) +
      geom_nfl_logos(data=player0, aes(x = 5.15, y = last(yfit), team_abbr = tm), width = 0.05) +
      geom_smooth(show.legend = NA) +
      geom_text(aes(label = "NFL", x = 5.15, y = max(prob) + 0.007), 
                color="black", family = "sans", fontface = "bold", show.legend = FALSE) +
      labs(x = "Seconds into Play",
           y = "Probablity of Pressure",
           title = paste0(player0$plot_name, " Probability of Pressure & the NFL Average", sep = ""),
           subtitle = "Locally estimated scatterplot smoothing with 95% confidence intervals (2021)",
           caption = "Data: @Kaggle Plot: @PattonAnalytics") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(size = 12, face="bold"),
            axis.text.x = element_text(face="bold"),
            axis.text.y = element_text(face="bold")) +
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12),
            plot.caption = element_text(size = 10))+
      #make ticks look nice
      scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_percent()) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = 0.1))
    
    # plot + if(true_false){nflplotR::geom_nfl_headshots(data=player0, 
    #                                                    aes(x=1, y=pos-.075, player_gsis = gsis_id), height = 0.2)}
    
    
  }
  
  output$prob_graph <- renderPlot({
    
    req(prob())
    prob()
    
  }, height = 625, width = 921)
  
  output$Download <- downloadHandler(
    filename = "pressure_probability.png",
    content = function(file) {
      prob()
      ggsave(file, plot = prob(), device = "png", width = 11, height = 8)
    })   

  }


shinyApp(ui = prob, server = server)

