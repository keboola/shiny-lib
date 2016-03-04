
#' @export
dynamicFactorInput <- function(id) {
  ns <- NS(id)
  tagList(
    div(id=id, class="dynamicInput-container",
        div(class="dynamicInput",
            uiOutput(ns("dynamicFactorMain"))
        ),
        div(class="dynamicInput-ui-container",
            uiOutput(ns("dynamicFactorElementsUI"))
        )
    )
  )
}

#' @export
dynamicFactor <- function(input, output, session, data, config=NULL) {
  
  # this will return data with all range filters applied
  filteredData <- reactive({
    ns <- session$ns
    cols <- selectedCols()
    sd <- data()
    if (is.null(cols) || cols == "") { return(sd) }
    for (col in cols) {
      if (col %in% names(sd) && !is.null(input[[col]])) {
        sd <- sd[
          which(
            sd[,col] %in% input[[col]]
          ), ]    
      }
    }
    sd
  })
  
  # Returns the columns selected as range filters.  IE. the selected elements of the primary select input.
  selectedCols <- reactive({
    ns <- session$ns
    cols <- input$dynamicFactorSelect
    if (!is.null(config()) && !is.null(names(config()))) {
        cols <- config()[[ns("dynamicFactorSelect")]]
    }
    print(paste("dynamicFactor: selectedCols:", paste(cols,collapse="##")))
    cols
  })
  
  # UI output for the primary select input.
  output$dynamicFactorMain <- renderUI({
    ns <- session$ns
    selectInput(ns("dynamicFactorSelect"), "Category Filters", choices=c("None"="",names(data())), multiple=TRUE)
  })
  
  # UI output for dynamically created sliderInputs which were chosen in the primary select input above
  output$dynamicFactorElementsUI <- renderUI({
    ns <- session$ns
    sd <- data()
    
    config <- config()
    if (is.null(config)) { print("config is null") } else { print("dynamicFactorELUI: config NOT NULL") }
    
    lapply(selectedCols(), function(col) {
      
        if (!(col %in% names(sd))) return(NULL)
        choices <- levels(as.factor(sd[,col]))
      
        if (!is.null(input[[col]])) {
            selected <- input[[col]]
        } else if (ns(col) %in% names(config)) {
            print("found factor col in config")
            selected <- config[[ns(col)]]
        } else {
            selected <- c()
        }
        selectInput(ns(col), col, choices=choices, selected=selected, multiple=TRUE)  
    })  
  })
  
  # return the reactive data with filters applied
  return(filteredData)
}