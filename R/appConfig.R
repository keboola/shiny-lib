# This should be the appConfig module
# maybe just the loading bit...

# UI portion:

#' @export
appConfigInput <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("saveConfigResultUI")),
        div(class="save-config-container",
            div(class="save-config-comment",
                textInput(ns("configComment"), "Name this setup:")),
            div(class="save-config-button",
                actionButton(ns("saveConfig"), "Save Current Settings", class="btn-primary"))
        ),
        wellPanel(
            uiOutput(ns("loadConfigResultUI")),
            uiOutput(ns("deleteConfigResultUI")),
            uiOutput(ns("selectConfigUI"))
        )    
    )
}

#' @export
appConfig <- function(input, output, session, kfig) {
    
    selectedConfig <- reactive({
        ns <- session$ns
        cfg <- input$loadConfig
        print(paste("LOAD config btn push:", cfg))
        isolate({
            selectedConfigId <- input$config
            print(paste("selected config is", selectedConfigId))
            if (is.null(selectedConfigId) || selectedConfigId == "None") {
                print("selected config is null or none")
                return(NULL)
            }
            configs <- kfig$configs()
            config <- lapply(configs,function(config) {
                if (config$id == selectedConfigId) {
                    # matches selected config, return configuration property as list
                    jsonlite::fromJSON(config$configuration)
                } else {
                    NULL
                }
            })
            # the config object is full of nulls for non-matches, 
            # so we remove them and return the matching elementt
            out <- Filter(Negate(is.null),config)[[1]]$config     
            print("got the configuration")
            print(out)
            print("clearing form?")
            #clearForm()
            #call the defaultConfigCallback
            kfig$defaultConfigCallback(out)
            out
        })
    })
    
    output$selectConfigUI <- renderUI({
        ns <- session$ns
        tagList(
            selectInput(ns("config"),"Configuration",c("None",kfig$configChoices()())),
            fluidRow(
                column(6,actionButton(ns("deleteConfig"), "Delete Selected Configuration", 
                                      class="btn-warning", 
                                      `data-toggle` = "kfig-alert", 
                                      `data-target` = paste0("#",ns("deleteConfigResultUI")))),
                column(6,actionButton(ns("loadConfig"), "Load Selected Configuration",
                                      `data-toggle` = "kfig-alert", 
                                      `data-target` = paste0("#",ns("loadConfigResultUI"))),
                       class="text-right")
            )    
        )
    })
    
    output$saveConfigResultUI <- renderUI({
        ns <- session$ns
        input$saveConfig
        isolate({
            
            if (nchar(input$configComment) > 0) {
                tryCatch({
                    kfig$saveConfig(input$configComment)    
                    return(
                        div(class = 'kfig-alert alert alert-success', "Configuration successfully saved.")
                    )
                }, error = function(e) {
                    div(class = 'kfig-alert alert alert-danger', paste0("Error saving configuration: ", e))
                })    
            } else {
                div(class = 'kfig-alert alert alert-warning', "Please enter a comment.")
            }    
        })
        
    })
    
    output$loadConfigResultUI <- renderUI({
        ns <- session$ns
        input$loadConfig
        tryCatch({
            print("pinging selected config")
            sc <- selectedConfig()
            div(class = 'alert alert-success', paste("Configuration", input$config, "was successfully loaded."))
        }, error = function(e) {
            div(class = 'alert alert-danger', paste0("Error loading configuration: ", e))
        })
    })
    
    # Clear all form elements.  Triggered on form load or exit
    clearForm <- function() {
        
        print("CLEARING FORM")
        lastSaveConfigValue <<- if (is.null(session$input$kb_saveConfigForReal)) { 0 } else { as.numeric(session$input$kb_saveConfigForReal) }
        lastLoadConfigValue <<- if (is.null(session$input$kb_loadConfig)) { 0 } else { as.numeric(session$input$kb_loadConfig) }
        lastConfirmDeleteValue <<- if (is.null(session$input$kb_confirmDelete)) { 0 } else { as.numeric(session$input$kb_confirmDelete) }
        lastConfirmCancelValue <<- if (is.null(session$input$kb_confirmCancel)) { 0 } else { as.numeric(session$input$kb_confirmCancel) }
        
        updateSelectInput(session, ns("config"), selected="None")
        updateTextInput(session, ns("configComment"), value="") 
        print("CLEARED FORM")
    }
    
    return(selectedConfig)
}
