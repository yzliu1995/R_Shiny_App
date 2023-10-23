# Reference: https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer
library(ggvis)
library(dplyr)
library(RColorBrewer)
if (FALSE) {
  library(RSQLite)
  library(dbplyr)
}

# Read in data

db <- src_sqlite("PR.db")

function(input, output, session) {

  
  reviews <- reactive({
    # Switch dataset
    if(input$dataset == 'CDs and Vinyl'){
      all_reviews <- tbl(db, "CDs_and_Vinyl_5")
    }else{
      all_reviews <- tbl(db, "Music_Instruments")
    }
    # Filter reviews
    minprice <- input$input2[1]
    maxprice <- input$input2[2]
    minrate <- input$input3[1]
    maxrate <- input$input3[2]
    minamount <- input$input4[1]
    maxamount <- input$input4[2]
    minsale <- input$input5[1]
    maxsale <- input$input5[2]
    # Apply filters and drop duplicates
    m <- all_reviews %>%
      filter(
        price >= minprice,
        price <= maxprice,
        amount >= minamount,
        amount <= maxamount,
        rate >= minrate,
        rate <= maxrate,
        sale >= minsale,
        sale <= maxsale
      ) %>%
      select(price, amount, rate, sale, Category, Product.ID, Description) %>%
      distinct()

    # Filter by Category
    if (!is.null(input$auto1) && input$auto1 != "") {
      category <- paste0("%", input$auto1, "%")
      m <- m %>% filter(Category %like% category)
    }
    
    # Filter by Description
    if (!is.null(input$auto2) && input$auto2 != "") {
      description <- paste0("%", input$auto2, "%")
      m <- m %>% filter(Description %like% description)
    }

    # Filter by Product.ID
    if (!is.null(input$auto3) && input$auto3 != "") {
      product.ID <- paste0("%", input$auto3, "%")
      m <- m %>% filter(Product.ID %like% product.ID)
    }
    
    m <- as.data.frame(m)
    m
  })

  
  # Hover
  review_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$Product.ID)) return(NULL)
    
    # Pick out a review with Product.ID
    all_reviews <- isolate(reviews())
    review <- all_reviews[all_reviews$Product.ID == x$Product.ID, ]
    
    paste0("Product ID: ", review$Product.ID, "<br>",
           "Category: <b>", review$Category, "</b><br>",
           "Description: ", review$Description,"<br>",
           "Price: $", format(review$price, big.mark = ",", scientific = FALSE),"<br>",
           "Quality Sold: ", format(review$amount, scientific = FALSE),"<br>",
           "Sale: <b>$", format(review$sale, big.mark = ",", scientific = FALSE),"</b><br>",
           "Average Rating: <b>", format(review$rate, scientific = FALSE),"</b>"
    )
  }
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Label axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]

    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    ncat <- length(unique(reviews()$Category))
    
    # Show color legend if the number of categories is larger than zero but no more than ten.
    if(ncat > 0 & ncat <= 10){
      reviews %>%
        ggvis(x = xvar, y = yvar) %>%
        layer_points(size := 50, size.hover := 200,
                     fillOpacity := 0.8, fillOpacity.hover := 0.5, fill = ~Category,
                     key := ~Product.ID) %>%
        add_tooltip(review_tooltip, "hover") %>%
        add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name)%>%
        add_legend("fill", title = "Category", values = unique(reviews()$Category)) %>%
        scale_nominal("fill", domain = unique(reviews()$Category),
                      range = brewer.pal(ncat, "Paired")[1:ncat]) %>%
        set_options(width = 500, height = 500)
    }else{
      reviews %>%
        ggvis(x = xvar, y = yvar) %>%
        layer_points(size := 50, size.hover := 200,
                     fillOpacity := 0.2, fillOpacity.hover := 0.5, key := ~Product.ID) %>%
        add_tooltip(review_tooltip, "hover") %>%
        add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name)%>%
        set_options(width = 500, height = 500)
      
    }
  })
  vis %>% bind_shiny("plot1")
  # Output correlation, number of products, and number of categories
  output$corr <- renderText({cor(reviews()[input$xvar],reviews()[input$yvar])})
  output$n_reviews <- renderText({nrow(reviews())})
  output$n_cat <- renderText({length(unique(reviews()$Category))})
}
