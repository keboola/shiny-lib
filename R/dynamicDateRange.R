
#' @export
dynamicDateRangeInput <- function(id) {
  ns <- NS(id)
  tagList(
    div(id=id, class="dynamicInput-container",
        div(class="dynamicInput",
            uiOutput(ns("dynamicDateRangeMain"))
        ),
        div(class="dynamicInput-ui-container",
            uiOutput(ns("dynamicDateRangeElementsUI"))
        )
    )
  )
}

#' @export
dynamicDateRange <- function(input, output, session, data, config=NULL) {
  
  # this will return data with all range filters applied
  filteredData <- reactive({
    ns <- session$ns
    cols <- selectedCols()
    sd <- data()
    if (is.null(cols)) { return(sd) }
    for (col in cols) {
      dateInterval <- interval(input[[col]][1],input[[col]][2]) 
      
      sd[which(
        as.Date(sd[,col]) %within% dateInterval
      ), ]
    }
    sd
  })
  
  # Returns the columns selected as range filters.  IE. the selected elements of the primary select input.
  selectedCols <- reactive({
    ns <- session$ns
    cols <- input$dynamicDateRangeSelect
    print(input[[ns("dynamicDateRangeSelect")]])
    cols
  })
  
  # UI output for the primary select input.
  output$dynamicDateRangeMain <- renderUI({
    ns <- session$ns
    selectInput(ns("dynamicDateRangeSelect"), "Date Range Filters", choices=c("None"="",names(data())), multiple=TRUE)
  })
  
  # UI output for dynamically created sliderInputs which were chosen in the primary select input above
  output$dynamicDateRangeElementsUI <- renderUI({
    ns <- session$ns
    sd <- data()
    lapply(selectedCols(), function(col) {
      # config <- .self$selectedConfig()
      coldata <- as.Date(sd[,col])
      min <- min(coldata, na.rm=TRUE)
      max <- max(coldata, na.rm=TRUE)
      if (!is.null(input[[col]])) {
        start <- input[[col]][1]
        end <- input[[col]][2]
      } else if (col %in% names(config)) {
        start <- config[[col]][1]
        end <- config[[col]][2]
      } else {
        start <- min
        end <- max
      }
      dateRangeInput(ns(col), col, min=min, max=max, start=start, end=end)
    })  
  })
  
  # return the reactive data with filters applied
  return(filteredData)
}