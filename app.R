#libs
#devtools::install_github("beatrizmilz/mananciais")
library(shiny)
library(mananciais)
library(dplyr)
library(ggplot2)


# Define UI for application that draws a histogram

data <-mananciais::mananciais
ui <- fluidPage(
  
  #Title and little text
  h2("The current and historic situation of São Paulo's water tanks"),
  
  h5("São Paulo is South America's largest city, and ranks among the world's most
     populated areas, with nothing short of 12 million inhabitants. All these people
     consume an enormous amount of water in their daily lives. To supply that water
     demand, 7 main water tanks are used by the city. In this webapp, you can check
     how these water tanks are doing in terms of volume of water and rainwater, 
     historically and on recent periods. The data comes from Beatriz Mills' amazing
     R package 'mananciais'. Beatriz is an Environmental Sciences Phd student and
     one of the most relevant R programmers in the brazilian R community."),
  
  #App intro
  h4("Choose a water tank to get data about it:"),
  #Input  
  selectInput('tank', "Water Tank", choices = unique(mananciais$sistema)),
  
  
  #Displaying outputs
  h4("Historic data"),
  textOutput('text1'),
  plotOutput('plot1'),
  
  #Displaying monthly rains
  textOutput('text2'),
  plotOutput('plot2')
  )



# Define server logic required to draw a histogram
server1 <- function(input, output, session) {

  #creating the reactive expression around the filtered dataset
  
  selected_tank <- reactive({data |> 
      #filtering the data for the selected water tank
                    dplyr::filter(sistema == input$tank) |>
      #filtering the data for the selected time period
                    dplyr::filter(data %in% as.Date('2018-01-01'):as.Date(Sys.Date())) |> 
                    dplyr::mutate(data = as.Date(data)) |> 
                    dplyr::mutate(month = stringr::str_sub(data,1,7))
      })
  
  grouped_selected_tank <- reactive({
    selected_tank() |> 
    dplyr::group_by(month) |> 
    dplyr::summarise(volume_percentage = median(volume_porcentagem),
                     monthly_rain = sum(pluviometria_dia)) |> 
      dplyr::ungroup()
  })
    
  #Percentage of total volume
  
  #create an output talkin bout the plot
  output$text1 <- renderText({glue::glue("This is the % of total volume evolution in 
                                   the {input$tank} water tank")})
  #create an output: a plot
  
  output$plot1 <- renderPlot({
    
      ggplot(grouped_selected_tank() ) +
      aes(x = month, y = volume_percentage, group = 1) +
      geom_line(size = 2, colour = "blue")+
      theme(axis.text.x = element_text(angle = 90))
    
    })
  
  #Monthly rains
  output$text2 <- renderText({glue::glue("This is the total rain volume in mm per month in 
                                   the {input$tank} water tank")})
  output$plot2 <- renderPlot({
    
    ggplot(grouped_selected_tank() ) +
      aes(x = month, weight =monthly_rain,  group = 1) +
      geom_bar(fill = "blue")+
      theme(axis.text.x = element_text(angle = 90))
    
  })
 
}




# Run the application 
shinyApp(ui = ui, server = server1)










