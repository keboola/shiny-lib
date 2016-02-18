#' This is the configuration component library for managing app configurations
#'
#' @import methods
#' @import shiny
#' @import jsonlite
#' @import keboola.sapi.r.client
#' @export KeboolaAppConfig
#' @exportClass KeboolaAppConfig
KeboolaAppConfig <- setRefClass(
    'KeboolaAppConfig',
    fields = list(
        session = 'ANY', # shiny server session
        client = 'ANY', # keboola.sapi.r.client::SapiClient
        # the following fields are helper hacks for ui flow control
        lastModalButtonValue = 'numeric',
        lastLoadConfigValue = 'numeric',
        lastSaveConfigValue = 'numeric',
        lastDeleteConfigValue = 'numeric',
        lastConfirmDeleteValue = 'numeric',
        lastConfirmCancelValue = 'numeric',
        registeredInputs = 'list',
        clearModal = 'logical',
        component = 'character',
        configId = 'character',
        validInputTypes = 'character'
    ),
    methods = list(
        initialize = function(sapiClient, component, configId, session = getDefaultReactiveDomain()) {
            "Constructor.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{sapiClient} Storage API client.}
            \\item{\\code{bucket} Bucket where config table is stored.}
            \\item{\\code{shinyUrl} Shiny Bundle API home URL.
            It will be read from command line argument.}
            }}"
            if (!inherits(session, "ShinySession"))
                stop("'session' is not a ShinySession object.")
            
            if (is.null(client)) {
                stop("Can not initialize KeboolaAppConfig.  No valid Sapi Client available.")
            }   
            session <<- session
            lastSaveConfigValue <<- 0
            lastLoadConfigValue <<- 0
            lastDeleteConfigValue <<- 0
            lastConfirmDeleteValue <<- 0
            lastConfirmCancelValue <<- 0
            lastModalButtonValue <<- 0
            clearModal <<- FALSE
            client <<- sapiClient
            component <<- component
            configId <<- configId
            validInputTypes <<- c("text","numeric","date","select","slider",
                                  "dateRange","checkbox","checkboxGroup","radioButtons",
                                  "dynamicRanges","dynamicDateRanges","dynamicFactors")
            registeredInputs <<- list()
        },
        
        configs = function() {
            "reactive wrapper around our config fetcher
            \\subsection{Return Value}{list of app input configurations}"
            reactive({
                .self$getConfigs()
            })
        },
        
        getConfigs = function() {
            "Get app configurations from the Shiny Bundle.
            \\subsection{Return Value}{List of app configurations.}"
            tryCatch({
                configs <- .self$client$listConfigurationRows(
                    .self$component,
                    .self$configId
                )
                return(configs)
            }, error = function(e) {
                # convert the error to a more descriptive message
                stop(paste0("Error loading app configs (", e, ')'))
            })    
        },
        
        configChoices = function() {
            "This returns a list of 'config name -- date created' with key configId
             the returned list is used to populate the options for the config select input
            \\subsection{Return Value}{array of configId -> 'configname -- date' }"
            reactive({
                configs <- .self$configs()()
                choices <- list()
                for (config in configs) {
                    #choices[[paste(config$id,config$dateCreated,sep=" -- ")]] <- config$id
                    choices[[config$id]] <- config$id
                }
                choices    
            })
        },
        
        selectedConfig = function() {
            "Uses the configId from the configuration select input to 
             return the currently selected configuration
            \\subsection{Return Value}{Currently selected app configuration}"
            selectedConfigId <- session$input$kb_config
            if (is.null(selectedConfigId) || selectedConfigId == "None") return(NULL)
            configs <- .self$configs()()
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
            Filter(Negate(is.null),config)[[1]]$config 
        },
        
        saveConfig = function() {
            "This method stores the entire session$input object as a row in the app configuration.
             Note that inputs with prefix kb_ will be omitted because they are system elements.
            \\subsection{Return Value}{TRUE, will throw an error if something goes wrong.}"
           if (is.null(.self$client)) {
               stop("Not connected to SAPI.")
           }
           tryCatch({
               configs <- list()
               for (name in names(session$input)) {
                   # only store non-system inputs
                   if (length(grep("^kb_", name)) == 0) {
                       configs[[name]] <- session$input[[name]]      
                   }
               }
               obj <- list()
               obj$comment <- session$input$kb_configComment
               obj$config <- configs
               resp <- .self$client$createConfigurationRow(
                   .self$component,
                   .self$configId,
                   gsub("-+","-", gsub("[^A-Za-z0-9_]", "-",obj$comment)), # replace non alphanumerics with dashes
                   jsonlite::toJSON(obj, auto_unbox=TRUE))
               TRUE
           }, error = function(e) {
               # convert the error to a more descriptive message
               stop(paste0("Error saving config (", e, ')'))
           })
        },
        
        deleteConfig = function(rowId) {
            "Delete the configuration
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{rowId} id of the configuration row}
            }}
            \\subsection{Return Value}{resp will be TRUE if successful. otherwise an error will be thrown.}"
            resp <- .self$client$deleteConfigurationRow(
                .self$component,
                .self$configId,
                rowId
            )
            resp
        },
        
        settingsModalButton = function() {
            "the toolbar button that brings up the configuration settings modal dialog
            \\subsection{Return Value}{list(button)}"
            list(
                keboolaModalButton(
                    "kb_configModal",
                    label = "",
                    icon = icon("gear"),
                    title = "Configuration Settings",
                    content = .self$configSettingsUI()
                )
            )
        },
        
        clearForm = function(input) {
            "Clear all form elements.  Triggered on form load or exit
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{input} TODO}
            }}
            \\subsection{Return Value}{TODO}"
            reactive({
                input$kb_configModalButton
                input$kb_config
                isolate({
                    if (!(is.null(input$kb_configModalButton)) && 
                            input$kb_configModalButton > 0 && 
                            input$kb_configModalButton > .self$lastModalButtonValue) {
                        lastModalButtonValue <<- as.numeric(input$kb_configModalButton)
                        clearModal <<- TRUE
                        TRUE
                    } else {
                        clearModal <<- FALSE
                        FALSE
                    }    
                })
            })
        },
        
        configSettingsUI = function() {
            "The main UI modal form
            \\subsection{Return Value}{The config settings modal form}"
            input <- session$input
            ret <- list(
                       div(style="text-align:right;padding:0 19px 15px 0;",
                           actionButton("kb_saveConfig", "Save Current Settings", class="btn-primary")
                       ),
                       uiOutput("kb_saveConfigUI"),
                       wellPanel(
                            uiOutput("kb_loadConfigResultUI"),
                            uiOutput("kb_deleteConfigResultUI"),
                            uiOutput("kb_configSelectorUI"),
                            fluidRow(
                                column(6,actionButton("kb_loadConfig", "Load Selected Configuration",
                                                      `data-toggle` = "kfig-alert", 
                                                      `data-target` = "#loadConfigResultUI")),
                                column(6,actionButton("kb_deleteConfig", "Delete Selected Configuration", 
                                                      class="btn-danger", `data-toggle` = "kfig-alert", 
                                                      `data-target` = "#deleteConfigResultUI"),
                                        class=" text-right")
                            )
                       )
                    )
            ret    
        },
        
        configSelectorUI = function() {
            "The config select element.
            \\subsection{Return Value}{select input with id=kb_config}"
            selectInput("kb_config","Configuration",c("None",configChoices()()))
        },
        
        saveConfigUI = function() {
            "sets DOM for the save configuration form.  text input and button
            \\subsection{Return Value}{DOM}"
            ret <- list()
            input <- session$input
            .self$clearForm(input)()
            if ((input$kb_saveConfig > 0) && (input$kb_saveConfig %% 2 == 1) && !.self$clearModal) {
                ret <- wellPanel(
                    uiOutput("kb_saveConfigResultUI"),
                    div(
                        textInput("kb_configComment", "Add a comment:"),
                        actionButton("kb_saveConfigForReal", "Save")
                    )
                )
            }
            ret
        },
        
        saveConfigResultUI = function() {
            "Saves the app input configuration
             returns the UI depending on the success of the operation
            \\subsection{Return Value}{list(UI elements)}"
            input <- session$input
            ret <- list()
            .self$clearForm(input)()
            if (input$kb_saveConfigForReal > 0 && input$kb_saveConfigForReal > .self$lastSaveConfigValue && !.self$clearModal) {
                lastSaveConfigValue <<- as.numeric(input$kb_saveConfigForReal)
                if (nchar(input$kb_configComment) > 0) {
                    tryCatch({
                        print("saving config")
                        .self$saveConfig()
                        print("config saved")
                        updateSelectInput(session,"kb_config", choices=c("None",configChoices()()))
                        ret <- list(ret,list(div(class = 'kfig-alert alert alert-success', "Configuration successfully saved.")))
                    }, error = function(e) {
                        write(paste("There was an error saving the config", e), stderr())
                        ret <- list(ret,list(div(class = 'kfig-alert alert alert-danger', paste0("Error saving configuration: ", e))))
                    })
                } else {
                    ret <- list(ret,list(div(class = 'kfig-alert alert alert-warning', "Please enter a comment.")))
                }
            }    
            return(ret) 
        },
        
        loadConfigResultUI = function(callback = .self$defaultConfigCallback) {
            "Returns DOM element depending on the success/failure of the config load
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{callback} The method to be executed with the loaded config.  
                    This method will generally be tasked with updating input elements with the values which were stored in the config.}
            }}
            \\subsection{Return Value}{DOM element}"
            input <- session$input
            ret <- list()
            .self$clearForm(input)()
            print(paste(
                "kb_loadConfig", input$kb_loadConfig,
                "selfLastLoadConfig", .self$lastLoadConfigValue,
                "kb_config", input$kb_config,
                "clearmodal", .self$clearModal
            ))
            if (!is.na(input$kb_loadConfig) && 
                    input$kb_loadConfig > 0 && 
                    input$kb_loadConfig > .self$lastLoadConfigValue && 
                    input$kb_config != "None" && 
                    !.self$clearModal) {
                tryCatch({
                    print("getting selected config")
                    config <- .self$selectedConfig()
                    print(config)
                    callback(config)
                    print("config callback executed")
                    ret <- list(ret,list(div(class = 'alert alert-success', "Configuration successfully loaded.")))
                }, error = function(e) {
                    ret <- list(ret,list(div(class = 'alert alert-danger', paste0("Error loading configuration: ", e))))
                })
                lastLoadConfigValue <<- as.numeric(input$kb_loadConfig)
            }
            ret
        },
        
        deleteConfigResultUI = function() {
            "Actually performs the delete and returns a DOM element indicating operation status
            \\subsection{Return Value}{DOM}"
            ret <- list()
            session$input$kb_deleteConfig
            session$input$kb_confirmDelete
            session$input$kb_confirmCancel
            session$input$kb_configModalButton
            isolate({
                input <- session$input
                .self$clearForm(input)()
                if (input$kb_deleteConfig > 0 && input$kb_deleteConfig %% 2 == 1
                    && (is.null(input$kb_confirmDelete) || input$kb_confirmDelete == .self$lastConfirmDeleteValue) 
                    && (is.null(input$kb_confirmCancel) || input$kb_confirmCancel == .self$lastConfirmCancelValue) 
                    && !.self$clearModal) {
                    
                    choices <- configChoices()()
                    mtch <- match(input$kb_config,unlist(choices))
                    choice <- names(choices)[mtch[1]]
                    ret <- div(class = 'alert alert-warning', paste("Are you sure you want to delete '", choice, "'?",sep=''),
                               actionButton("kb_confirmDelete",'Yes'),
                               actionButton("kb_confirmCancel",'No'))
                } else if (!is.null(input$kb_confirmDelete) && input$kb_confirmDelete > .self$lastConfirmDeleteValue && !.self$clearModal) {
                    print(paste0("Confirmed to delete: ", input$kb_config))
                    lastConfirmDeleteValue <<- as.numeric(input$kb_confirmDelete)
                    tryCatch({
                        print(paste("deleting config", input$kb_config))
                        resp <- .self$deleteConfig(input$kb_config)
                        print(paste("deleted config", input$kb_config))
                        updateSelectInput(session,"kb_config", choices=c("None",configChoices()()))
                        ret <- div(class = 'alert alert-success', "Configuration successfully deleted.")
                    }, error = function(e) {
                        ret <- div(class = 'alert alert-danger', paste0("Error deleting configuration: ", e))
                    })
                } else if (!is.null(input$kb_confirmCancel) && input$kb_confirmCancel > .self$lastConfirmCancelValue) {
                    lastConfirmCancelValue <<- as.numeric(input$kb_confirmCancel)
                    # Do nothing
                }    
            })
            ret
        },
        
        defaultConfigCallback = function(config) {
            "This method is called when an input configuration is loaded
            \\subsection{Return Value}{void}"
            
            print("in default config callback boyyyyy")
            print(config)
            print("was that a config?")
            if (length(.self$registeredInputs) == 0) {
                stop("No inputs were registered so I have nothing to do.")
            }
            print(paste("there are",length(.self$registeredInputs),"inputs"))
            for (i in 1:length(.self$registeredInputs)) {
                input <- .self$registeredInputs[[i]]
                if (input$id %in% names(config)) {
                    print(paste("going to fix em up for", input$id))
                    switch(input$type,
                           "select" = {
                               print(paste(input$id, "has type", input$type))
                               print(paste("should load", input$id,"as", config[[input$id]]))
                               updateSelectInput(session,input$id, selected=config[[input$id]])
                               },
                           "text" = updateTextInput(session, input$id, value=config[[input$id]]),
                           "slider" = updateSliderInput(session, input$id, value=c(config[[input$id]][1],config[[input$id]][2])),
                           "date" = updateDateInput(session, input$id, value=config[[input$id]]),
                           "dateRange" = updateDateRangeInput(session, input$id, start=config[[input$id]][1], end=config[[input$id]][2]),
                           "checkbox" = updateCheckboxInput(session, input$id, value=config[[input$id]]),
                           "checkboxGroup" = updateCheckboxGroupInput(session, input$id, selected=config[[input$id]]),
                           "numeric" = updateNumericInput(session, input$id, value=config[[input$id]]),
                           "radioButtons" = updateRadioButtons(session, input$id, selected=config[[input$id]]),
                           "dynamicRanges" = {
                               updateSelectInput(session, input$id, selected = config[[input$id]])
                               for (element in config[[input$id]]) {
                                   updateSliderInput(session, element, value=c(config[[element]][1],config[[element]][2]))
                               }
                           },
                           "dynamicDateRanges" = {
                               updateSelectInput(session, input$id, selected=config[[input$id]])
                               for (element in config[[input$id]]) {
                                   updateDateRangeInput(session, element, start=config[[element]][1], end=config[[element]][2])
                               }
                           },
                           "dynamicFactors" = {
                               updateSelectInput(session, input$id, selected=config[[input$id]])
                               for (element in config[[input$id]]) {
                                   updateSelectInput(session, element, selected=config[[input$id]])
                               }
                           },
                           stop(paste("Error loading configuration. Unknown input type given:", input$type, ".  Known types are: 'select', 'text', 'slider', and 'daterange'"))
                    )    
                }
            }
        },
        
        registerInputs = function(inputList) {
            print(inputList)
            validateInput <- function(input) {
                print(paste("Registering input", input))
                
                if (!class(input) == "list" 
                    | !("id" %in% names(input))
                    | !("type" %in% names(input))
                    | !(input$type %in% .self$validInputTypes)
                ) {
                    stop(paste(
                        "The argument must be a list with members 'id' and 'type' and type must be one of: ", 
                        paste(validInputs, collapse=", ")
                    ))
                }    
            }
            
            lapply(inputList, function(input) {
                validateInput(input)
                .self$registeredInputs[[length(.self$registeredInputs) + 1]] <- input
            })
            
            print("Inputs are valid and registered")
        }
        
    )
)
