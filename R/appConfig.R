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
            uiOutput(ns("confirmDeleteResultUI")),
            uiOutput(ns("selectConfigUI"))
        ),
        
        div(class="kb-hidden",
            textInput(ns("settingsMode"),"")
            )
    )
}

#' @export
appConfig <- function(input, output, session, kfig) {
    
    selectedConfig <- reactive({
        ns <- session$ns
        cfg <- input$loadConfig
        print(paste("LOAD config btn push:", cfg))
        print(paste("KB SETTINGS MODE in selectedConfig", input$settingsMode))
        isolate({
            selectedConfigId <- input$config
            print(paste("selected config is:::", selectedConfigId))
            if (input$settingsMode != 1 || is.null(selectedConfigId) || selectedConfigId == "None") {
                print("selected config is null or none or not in settingsMode")
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
            
            # load the config into the app
            kfig$defaultConfigCallback(out)
            out
        })
    })
    
    output$selectConfigUI <- renderUI({
        ns <- session$ns
        print(paste("KB SETTINGS MODE configUI", input$settingsMode))
        input$confirmDelete
        input$confirmCancel
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
            print("somebody wants to save the state")
            if (nchar(input$configComment) > 0) {
                tryCatch({
                    kfig$saveConfig(input$configComment)    
                    return(
                        div(class = 'kfig-alert alert alert-success', "Configuration successfully saved.")
                    )
                }, error = function(e) {
                    div(class = 'kfig-alert alert alert-danger', paste0("Error saving configuration: ", e))
                })    
            } 
        })
        
    })
    
    output$deleteConfigResultUI <- renderUI({
        ns <- session$ns
        input$deleteConfig
        isolate({
            if (is.null(input$deleteConfig) || input$deleteConfig < 1) {
                return(NULL)
                # div(class = 'alert alert-warning', "No configuration selected, I've nothing to delete.")
            } else {
                print("Good, a config is selected, and we can try to delete it, please confirm")
                choices <- kfig$configChoices()()
                mtch <- match(input$config,unlist(choices))
                choice <- names(choices)[mtch[1]]
                # return a confirmation dialog
                div(
                    class = 'alert alert-warning', 
                    paste("Are you sure you want to delete '", choice, "'?",sep=''),
                    actionButton(ns("confirmDelete"),'Yes'),
                    actionButton(ns("confirmCancel"),'No')
                )
            }
        })
    })
    
    output$confirmDeleteResultUI <- renderUI({
        ns <- session$ns
        input$confirmDelete
        input$cancelDelete
        isolate({
            print(paste("confirm val", input$confirmDelete, " cancel val", input$cancelDelete))
            if (!is.null(input$confirmDelete) && input$confirmDelete >= 1) {
                tryCatch({
                    resp <- kfig$deleteConfig(input$config)
                    # return a success alert
                    div(class = 'alert alert-success', "Configuration successfully deleted.")
                }, error = function(e) {
                    # return an error alert
                    div(class = 'alert alert-danger', paste0("Error deleting configuration: ", e))
                })    
            } else {
                return(NULL)
            }
        })
    })
    
    output$loadConfigResultUI <- renderUI({
        ns <- session$ns
        input$loadConfig
        tryCatch({
            print("pinging selected config")
            sc <- selectedConfig()
            print(paste("selected config is ", sc))
            if (!is.null(sc)) { 
                div(class = 'alert alert-success', paste("Configuration", input$config, "was successfully loaded."))    
            } else {
                div()
            }
        }, error = function(e) {
            div(class = 'alert alert-danger', paste0("Error loading configuration: ", e))
        })
    })
    
    # Clear all form elements.  Triggered on form load or exit
    clearForm <- reactive({
        ns <- session$ns
        input$settingsMode
        isolate({
            if (!is.null(input$settingsMode) && input$settingsMode == 0) {
                print("CLEAR FORM START")
                #lastSaveConfigValue <<- if (is.null(session$input$kb_saveConfigForReal)) { 0 } else { as.numeric(session$input$kb_saveConfigForReal) }
                #lastLoadConfigValue <<- if (is.null(session$input$kb_loadConfig)) { 0 } else { as.numeric(session$input$kb_loadConfig) }
                #lastConfirmDeleteValue <<- if (is.null(session$input$kb_confirmDelete)) { 0 } else { as.numeric(session$input$kb_confirmDelete) }
                #lastConfirmCancelValue <<- if (is.null(session$input$kb_confirmCancel)) { 0 } else { as.numeric(session$input$kb_confirmCancel) }

                updateSelectInput(session, ns("config"), selected="None")
                updateTextInput(session, ns("configComment"), value="") 
                print("CLEAR FORM END")        
            }
        })
        
    })
    
    return(selectedConfig)
}
