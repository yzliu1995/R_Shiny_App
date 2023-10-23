# Reference: https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer
library(ggvis)

# Dropdown
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

fluidPage(
  titlePanel("Product Review Exploration"),
    fluidRow(
      # filter by variables
      column(4,
        wellPanel(
          selectInput("dataset", "Dataset",
                      c("Musical Instruments","CDs and Vinyl")
          ),
          sliderInput("input2", "Price",
            0, 599, c(0,100), step = 0.1),
          sliderInput("input3", "Average Rating",
                      2.7, 5, value = c(2.7,5), step = 0.1),
          sliderInput("input4", "Quantity Sold",
                      5, 163, c(5,163), step = 1),
          sliderInput("input5", "Sales",
                      0, 3725, c(0,3725), step = 20),
          textInput("auto1", "Category contains (e.g., Instrument Cables)"),
          textInput("auto2", "Description contains (e.g., 5 Foot Long MIDI to MIDI Connect Cable)"),
          textInput("auto3", "Product ID contains (e.g., B000068O1N)")
          )
        ),
      # interactive plot
      column(8,
             ggvisOutput("plot1")
      ),
      column(3, offset = 0.5,
        wellPanel(
          selectInput("xvar", "X-axis variable", axis_vars, selected = "price"),
          selectInput("yvar", "Y-axis variable", axis_vars, selected = "rate"),
          tags$small(paste0(
            "Note:", "\n",
            "Sales is a ratio quantity, ",
            "defined by the price of a product times its quantity sold. ",
            "Average rating is an average of all ratings of a product."
          ))
        )
      ),
      # Variable selection
      column(3, offset = 0.5,
             wellPanel(
               span("Correlation between X-axis variable and Y-axis variable:",
                    textOutput("corr")),
               span("Number of products:",
                    textOutput("n_reviews")),
               span("Number of categories:",
                    textOutput("n_cat")),
               tags$small(paste0(
                 "Note:", "\n",
                 "Color legend for category will be shown ",
                 "if the number of categories is larger than 0 ",
                 "but no more than 10."
               ))
             )
      )
    )
)
