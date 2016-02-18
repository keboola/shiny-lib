#' Page Template for Keboola Shiny Apps
#' Dynamic Inputs
#' This element is like a multi-select, but for each selection 
#' a range element is created for the selected column
#' 
#' @import shiny
#' @export
#' @param inputId character identifier of the element
#' @param label character label 
dynamicInput <- function(inputId, label) {
    
    tagList(
        div(id=inputId, class="dynamicInput-container",
            div(class="dynamicInput",
                shiny::selectInput(inputId, label, choices = c(), multiple = TRUE)
            ),
            div(class="dynamicInput-ui-container",
                uiOutput(paste0(inputId,"UI"))    
            )
        )
    )
}

