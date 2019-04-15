

library(shiny)
library(ggplot2)
library(reshape2)

future_value <- function(amount, rate, years){
  future <- amount * (1 + rate)^years
  return(future)
}

annuity <- function(contrib, rate, years){
  future_annuity <- contrib * ((((1 + rate)^years) - 1) / rate)
  return(future_annuity)
}

growing_annuity <- function(contrib, rate, growth, years){
  future_growing_annuity <- contrib * (((1 + rate)^years - (1 + growth)^years) / (rate - growth))
  return(future_growing_annuity)
}

ui <- fluidPage(
  
   titlePanel("Investing Modalities Calculator"),
   
   fluidRow(
     column(
       4,
       sliderInput(
         "initial",
         "Initial Amount",
         min = 0,
         max = 100000,
         value = 1000,
         step = 500
       ),
       
       sliderInput(
         "rate",
         "Return Rate (in %)",
         min = 0,
         max = 20,
         value = 5,
         step = 0.1
       )
     ),
     column(4,
            sliderInput(
              "year",
              "Years",
              min = 0,
              max = 50,
              value = 10,
              step = 1
            ),
     
     sliderInput(
       "contrib",
       "Annual Contribution",
       min = 0,
       max = 50000,
       value = 2000,
       step = 500
     )
   ),
   column(
     4,
     sliderInput(
       "growth",
       "Growth Rate (in %)",
       min = 0,
       max = 20,
       value = 2,
       step = 0.1
     ),
     
     selectInput(
       "facet",
       "Facet?",
       choices = c("YES", "NO"),
       selected = "NO"
     )
   )),
   
   
   fluidRow(column(10,
                   h4("Timelines"),
                   offset = 1,
                   plotOutput("distPlot"))),
   
   
   fluidRow(column(10,
                   h4("Balances"),
                   offset = 2,
                   tableOutput("balances")))
)



server <- function(input, output) {
  
  modalities_table <- reactive({
    no_contrib <- 0
    fixed_contrib <- 0
    growing_contrib <- 0
    year <- 0
    rate <- input$rate / 100
    growth <- input$growth / 100
    for (i in 1:input$year) {
      no_contrib[i] <- future_value(input$initial, rate, i - 1)
      fixed_contrib[i] <-
        annuity(input$contrib, rate, i - 1) + future_value(input$initial, rate, i - 1)
      growing_contrib[i] <-
        growing_annuity(input$contrib, rate, growth, i - 1) + future_value(input$initial, rate, i - 1)
      year[i] <- i - 1
    }
    
    
    modalities_table <-
      data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    modalities_table
    
  })
  
  output$distPlot <- renderPlot({
    melted_modalities <- melt(modalities_table(), id.vars = "year")
    
    if (input$facet == "NO") {
      gg1 <-
        ggplot(melted_modalities, aes(x = year, y = value)) +
        geom_line(aes(col = variable)) + 
        geom_point(aes(col = variable)) +
        ggtitle("Three Modes of Investing")
      gg1
    }
   else if (input$facet == "YES") {
      gg2 <-
        ggplot(melted_modalities, aes(x = year, y = value)) + 
        geom_line(aes(col = variable)) + 
        geom_point(aes(col = variable)) + 
        facet_wrap( ~ variable) + geom_area(aes(fill = variable), alpha = 0.4) +
        ggtitle("Three Modes of Investing") + theme_bw()
  
      gg2
    }
    
  })
  
  output$balances <- renderTable({
   modalities_table()
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

