
#' Button to create modal dialogs for Keboola Shiny Apps
#' 
#' @param inputId -- input ID for the modal window
#' @param label -- Dialog title
#' @param icon -- optional dialog icon
#' @param header -- content for dialog header
#' @param content -- dialog content (can include input and output elements)
#' @import shiny
#' @export
keboolaModalButton <- function(inputId, label, icon = NULL, title = "", content = "")
{
    # original: https://gist.github.com/ijlyttle/9ace92f4f56246375859
    # updated to use with bootstrap 3.3.1
    
    # create the button  
    button <- tags$button(
        id = "kb_configModalButton",
        type = "button", 
        class = "btn btn-default btn-sm action-button shiny-bound-input", 
        `data-toggle` = "modal", 
        `data-target` = paste0("#", inputId, sep = ""), 
        list(icon, label)
    )
    
    # create the window 
    window <- tags$div(
        id = inputId, 
        class = "modal fade",
        tabindex = "-1",
        role = "dialog",
        `aria-labelledby` = paste0(inputId, "Label"),
        tags$div(
            class = "modal-dialog",
            tags$div(
                class = "modal-content",
                tags$div(
                    class = "modal-header",
                    tags$button(
                        type = "button",
                        class = "close",
                        `data-dismiss` = "modal", 
                        `aria-label` = "Close",
                        tags$span(
                            `aria-hidden` = "true",
                            "X"    
                        )
                    ),
                    tags$h4(class="modal-title", id = paste0(inputId, "Label"), title)
                ),
                tags$div(
                    class = "modal-body",
                    content        
                ),
                tags$div(
                    class = "modal-footer",
                    tags$button(
                        class = "btn",
                        `data-dismiss` = "modal",
                        "Close"
                    )
                )
            )
        )
    )
    tags$html(button, window)
}
