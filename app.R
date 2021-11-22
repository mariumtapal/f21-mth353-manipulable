library(shiny)
library(reactable)
library(tidyverse)
library(combinat)
library(plotly)

# read in function
source("model.R")

# test inputs

species <- c(
  "ATGCTTAGC",
  "GTAAT",
  "GTACGTAAT",
  "GTAAATA"
)

stresses <- c(
  "heat",
  "light",
  "light",
  "food"
)

example_df <- map2_dfr(species, stresses, find_matches)


ui <- fluidPage(
  titlePanel("Visualising Changes in Anemone DNAs"),
  # p("This dashboard was designed to help users locate vaccine locations in Massachusetts", style = "font-family: 'times'; font-si16pt"),
  #strong("Find your nearest location and data about your state!"),
  # em("This website is currently being developed."),
  br(),
  mainPanel(
    #titlePanel("Proportion of people vaccinated in MA by age"),
    fluidRow(
      column(
        4, textAreaInput("dnasequences",
          label = "Enter DNA sequences of upto 9 characters seperated by commas:"),
          radioButtons("stress", choices = c("heat", "light", "food"), 
                       label = "Select the stress you want to see:"),
        submitButton(text = "Submit")
      )
    )
  ),
  column(8, reactableOutput("table"))
)



# reactable
server <- function(input, output) {
  req(input$dnasequences)
  
  dnasequences_input <- reactive({
    
     input$dnasequences %>% 
      strsplit(",") %>% 
      unlist() %>% 
      str_trim()
    return(dnasequences) })
    
    stress_input <- reactive({
      req(input$stress)
      input$stress
      return(stress)
      })
    
    
    func <- reactive({
    df <- map2_dfr(species = dnasequences_input(), stress = stress_input(), find_matches)
    })
    sum_table <- df %>% group_by(specie) %>%
      summarise(count_changes = n()) %>%
      mutate(total_nucleotides = nchar(specie),
             prob_changes = count_changes/total_nucleotides) %>%
      arrange(desc(prob_changes))
 
  
    output$table <- renderReactable({
    reactable(sum_table, searchable = TRUE, filterable = TRUE, theme = reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      searchInputStyle = list(width = "100%")
    ))
  })
    

  # 
  # output$plot <- renderPlotly({
  #   ggplotly(ggplot(
  #     newdata,
  #     aes(x = `Race/Ethnicity`, y = Proportion, fill = `Race/Ethnicity`)
  #   ) +
  #     geom_col() +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1)))
  # })

}

shinyApp(ui, server)
