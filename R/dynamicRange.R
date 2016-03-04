
#' @export
dynamicRangeInput <- function(id) {
    ns <- NS(id)
    tagList(
        div(id=id, class="dynamicInput-container",
            div(class="dynamicInput",
                uiOutput(ns("dynamicRangeMain"))
            ),
            div(class="dynamicInput-ui-container",
                uiOutput(ns("dynamicRangeElementsUI"))
            )
        )
    )
}

#' @export
dynamicRange <- function(input, output, session, data, config) {
    
    # this will return data with all range filters applied
    filteredData <- reactive({
        ns <- session$ns
        cols <- selectedCols()
        sd <- data()
        for (col in cols) {
            if (col %in% names(sd)){
                sd[,col] <- as.numeric(sd[,col])
                sd <- sd[
                    which(
                        sd[,col] >= as.numeric(input[[col]][1]) &
                            sd[,col] <= as.numeric(input[[col]][2])
                    ), 
                    # leaving the second argument empty like this means all columns will be selected
                    ]    
            }
        }
        sd
    })
    
    # Returns the columns selected as range filters.  IE. the selected elements of the primary select input.
    selectedCols <- reactive({
      ns <- session$ns
      cols <- input$dynamicRangeSelect
      if (!is.null(config()) && !is.null(names(config()))) {
          cols <- config()[[ns("dynamicRangeSelect")]]
      }
      cols
    })
    
    # UI output for the primary select input.
    output$dynamicRangeMain <- renderUI({
      ns <- session$ns
      selectInput(ns("dynamicRangeSelect"), "Range Filters", choices=c("None"="",names(data())), multiple=TRUE, selected=selectedCols())
    })
    
    # UI output for dynamically created sliderInputs which were chosen in the primary select input above
    output$dynamicRangeElementsUI <- renderUI({
        ns <- session$ns
        sd <- data()
        
        config <- config()
        if (is.null(config)) { print("config is null") } else { print("dynamicRangeELUI: config NOT NULL") }
        
        lapply(selectedCols(), function(col) {
            print(paste("rangeelementsUI:", col))
            # make sure that the data we're working with has been updated and contains col
            if (!(col %in% names(sd))) return(NULL)
            
            # attempt to cast the column as numeric
            colData <- suppressWarnings(as.numeric(sd[,col]))
            # get min/max values for the slider
            minval <- min(colData, na.rm=TRUE)
            maxval <- max(colData, na.rm=TRUE)
          
            print(paste("namespaced col", ns(col)))
            
            if (!is.null(input[[col]])) {
                # if the session already has values for this element, use them
                value <- c(input[[col]][1], input[[col]][2])
            } else if (ns(col) %in% names(config)) {
                # if the loaded config has values for this element, use them
                value <- c(config[[ns(col)]][1], config[[ns(col)]][2])
            } else {
                # use the min and max as default selected values
                value <- c(minval,maxval)
            }
            sliderInput(ns(col), col, min=minval, max=maxval, value=value)
        })  
    })
    
    # return the reactive data with filters applied
    return(filteredData)
}