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
        lastLoadConfigValue = 'numeric',
        lastSaveConfigValue = 'numeric',
        lastSaveConfirmValue = 'numeric',
        lastConfirmDeleteValue = 'numeric',
        lastConfirmCancelValue = 'numeric',
        registeredInputs = 'list',
        clearModal = 'logical',
        component = 'character',
        configId = 'character',
        configLoaded = 'numeric',
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
            lastSaveConfirmValue <<- 0
            lastLoadConfigValue <<- 0
            lastConfirmDeleteValue <<- 0
            lastConfirmCancelValue <<- 0
            
            configLoaded <<- 0
            clearModal <<- FALSE
            client <<- sapiClient
            component <<- component
            configId <<- configId
            validInputTypes <<- c("text","numeric","date","select","slider","actionButton",
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
            print("SELECTED CONFIG")
            
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
        
        clearForm = function() {
            "Clear all form elements.  Triggered on form load or exit
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{input} TODO}
            }}
            \\subsection{Return Value}{TODO}"
            reactive({
                session$input$kb_configModalButton
                isolate({
                    print("CLEARING FORM")
                    lastSaveConfigValue <<- if (is.null(session$input$kb_saveConfigForReal)) { 0 } else { as.numeric(session$input$kb_saveConfigForReal) }
                    lastLoadConfigValue <<- if (is.null(session$input$kb_loadConfig)) { 0 } else { as.numeric(session$input$kb_loadConfig) }
                    lastConfirmDeleteValue <<- if (is.null(session$input$kb_confirmDelete)) { 0 } else { as.numeric(session$input$kb_confirmDelete) }
                    lastConfirmCancelValue <<- if (is.null(session$input$kb_confirmCancel)) { 0 } else { as.numeric(session$input$kb_confirmCancel) }
                    
                    updateSelectInput(session, "kb_config", selected="None")
                    updateTextInput(session,"kb_configComment", value="") 
                    print("CLEARED FORM")
                })
            })
        },
        
        configSettingsUI = function() {
            "The main UI modal form
            \\subsection{Return Value}{The config settings modal form}"
            input <- session$input
            
            ret <- list(
                        uiOutput("kb_saveConfigResultUI"),
                       div(style="text-align:right;padding:0 19px 15px 0;",
                           div(style="display:inline-block",
                               textInput("kb_configComment", "Name this setup:")),
                           div(style="display:inline-block;",
                               actionButton("kb_saveConfig", "Save Current Settings", class="btn-primary"))
                       ),
                       wellPanel(
                            uiOutput("kb_loadConfigResultUI"),
                            uiOutput("kb_deleteConfigResultUI"),
                            uiOutput("kb_configSelectorUI"),
                            fluidRow(
                                column(6,actionButton("kb_deleteConfig", "Delete Selected Configuration", 
                                                      class="btn-danger", `data-toggle` = "kfig-alert", 
                                                      `data-target` = "#deleteConfigResultUI")),
                                column(6,actionButton("kb_loadConfig", "Load Selected Configuration",
                                                      `data-toggle` = "kfig-alert", 
                                                      `data-target` = "#loadConfigResultUI"),
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
        
        saveConfigResultUI = function() {
            "Saves the app input configuration
             returns the UI depending on the success of the operation
            \\subsection{Return Value}{list(UI elements)}"
            session$input$kb_saveConfig
            isolate({
                ret <- list()
                input <- session$input
                if (!is.null(input$kb_saveConfig) && input$kb_saveConfig > 0 && input$kb_saveConfig > .self$lastSaveConfigValue) {
                    print(paste("saving configuration:", input$kb_configComment))
                    lastSaveConfigValue <<- as.numeric(input$kb_saveConfigForReal)
                    if (nchar(input$kb_configComment) > 0) {
                        tryCatch({
                            .self$saveConfig()
                            updateSelectInput(session,"kb_config", choices=c("None",configChoices()()))
                            ret <- list(ret,list(div(class = 'kfig-alert alert alert-success', paste("Configuration successfully saved."))))
                        }, error = function(e) {
                            write(paste("There was an error saving the config", e), stderr())
                            ret <- list(ret,list(div(class = 'kfig-alert alert alert-danger', paste0("Error saving configuration: ", e))))
                        })
                    } else {
                        ret <- list(ret,list(div(class = 'kfig-alert alert alert-warning', "Please enter a comment.")))
                    }
                } else {
                    print("Config save button not pressed")
                }    
                return(ret)     
            })
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
                    print("WTF trying to call configCallback")
                    configLoaded <<- as.numeric(session$input$kb_configLoaded) + 1
                    config <- .self$selectedConfig()
                    print("trying to call configCallback")
                    res <- callback(config)
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
            isolate({
                input <- session$input
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
                        resp <- .self$deleteConfig(input$kb_config)
                        print(paste("deleted config:", input$kb_config))
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
        
        updateInputElement = function(elem, config = NULL) {
            if (is.null(config)) {
                newValue = ""
            } else {
                newValue <- config[[elem$id]]    
            }
            
            print(paste("updating input element", elem$id, "with value", paste(newValue, collapse="' ")))
            switch(elem$type,
                   "select" = updateSelectInput(session,elem$id, selected=newValue),
                   "text" = updateTextInput(session, elem$id, value=newValue),
                   "slider" = updateSliderInput(session, elem$id, value=c(newValue[1],newValue[2])),
                   "date" = updateDateInput(session, elem$id, value=newValue),
                   "dateRange" = updateDateRangeInput(session, elem$id, start=newValue[1], end=newValue[2]),
                   "checkbox" = updateCheckboxInput(session, elem$id, value=newValue),
                   "checkboxGroup" = updateCheckboxGroupInput(session, elem$id, selected=newValue),
                   "numeric" = updateNumericInput(session, elem$id, value=newValue),
                   "radioButtons" = updateRadioButtons(session, elem$id, selected=newValue),
                   "dynamicRanges" = {
                        updateSelectInput(session, elem$id, selected = newValue)
                        if (length(newValue) > 1 || newValue !="") {
                           for (element in newValue) {
                               if (!is.null(session$input[[element]])) {
                                   updateSliderInput(session, element, value=c(config[[element]][1],config[[element]][2]))    
                               }
                           }
                        }
                   },
                   "dynamicDateRanges" = {
                        updateSelectInput(session, elem$id, selected=newValue)
                        if (length(newValue) > 1 || newValue != "") {
                           for (element in newValue) {
                               updateDateRangeInput(session, element, start=config[[element]][1], end=config[[element]][2])
                           }
                        }
                   },
                   "dynamicFactors" = {
                       updateSelectInput(session, elem$id, selected=newValue)
                       if (length(newValue) > 1 || newValue != "") {
                           for (element in newValue) {
                               updateSelectInput(session, element, selected=config[[element]])
                           }        
                       }
                   },
                   stop(paste("Error loading configuration. Unknown input type given:", elem$type, ".  Valid types are:", paste(self$validInputTypes,collapse=", ")))
            )
        },
        
        defaultConfigCallback = function(config) {
            "This method is called when an input configuration is loaded
            \\subsection{Return Value}{void}"
         
            if (length(.self$registeredInputs) == 0) {
                stop("No inputs were registered so I have nothing to do.")
            }
            
            
            for (i in 1:length(.self$registeredInputs)) {
                elem <- .self$registeredInputs[[i]]
                if (elem$id %in% names(config)) {
                    .self$updateInputElement(elem, config)    
                } else {
                    # now, this element was set in the interface, but it does not appear in the config.
                    # we need to clear this element
                    .self$updateInputElement(elem)
                }
            }
            print(paste("DEFAULT ConfigCallback finished: ", .self$configLoaded))
        },
        
        registerInputs = function(inputList) {
            validateInput <- function(input) {              
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
        },
        
        
        generateDynamicUIs = function(data, strictTypes = FALSE) {
            print("generateDynamicUIs")
            lapply(.self$registeredInputs, function(elem){
                if (length(grep("^dynamic", elem$type)) > 0) {
                    elemUIid <- paste0(elem$id,"UI")
                    session$output[[paste0(elem$id,"UI")]] <- renderUI({ .self$getDynamicElementUI(data, elem$id, elem$type )})    
                }
            })  
        },
        
        getDynamicElementUI = function(data, inputId, type) {
            print(paste("getDynamicElUI for", inputId))
            if (length(session$input[[inputId]]) > 0) {
                for (i in 1:length(session$input[[inputId]])) {
                    print(paste(""))
                }
                # this function creates a list of inputs depending on the type with name equal to the selected column
                # Note, we aren't capturing this in a variable 
                # and there are no further statements so this is our return value
                lapply(seq_along(session$input[[inputId]]), function(x) {
                    elem <- session$input[[inputId]][x]
                    # return filter element depending on type
                    switch(type,
                           "dynamicRanges" = dynamicRangeFilter(data, elem),
                           "dynamicDateRanges" = dynamicDateRangeFilter(data, elem),
                           "dynamicFactors" = dynamicFactorFilter(data, elem)
                    )
                })
            } 
        },
        
        applyDynamicFilter = function(data, inputId, type) {
            
            if (length(session$input[[inputId]]) > 0) {
                for (i in 1:length(session$input[[inputId]])) {
                    filter <- session$input[[inputId]][i]
                    if (!is.null(session$input[[filter]])) {
                        data <- .self$applyDataFilter(data, filter, type)    
                    }
                }
            }
            data
        },
        
        
        
        dynamicRangeFilter = function(data, elem) {
            config <- .self$selectedConfig()
            # attempt to cast the column as numeric
            colData <- suppressWarnings(as.numeric(data[,elem]))
            # get min/max values for the slider
            minval <- min(colData, na.rm=TRUE)
            maxval <- max(colData, na.rm=TRUE)
            
            if (!is.null(session$input[[elem]])) {
                # if the session already has values for this element, use them
                value <- c(session$input[[elem]][1],session$input[[elem]][2])
            } else if (elem %in% names(config)) {
                # if the loaded config has values for this element, use them
                value <- c(config[[elem]][1], config[[elem]][2])
            } else {
                # use the min and max as default selected values
                value <- c(minval,maxval)
            }
            sliderInput(elem, elem, min=minval, max=maxval, value=value)
        },
        
        dynamicDateRangeFilter = function(data, elem) {
            config <- .self$selectedConfig()
            coldata <- as.Date(data[,elem])
            min <- min(coldata, na.rm=TRUE)
            max <- max(coldata, na.rm=TRUE)
            if (!is.null(session$input[[elem]])) {
                start <- session$input[[elem]][1]
                end <- session$input[[elem]][2]
            } else if (elem %in% names(config)) {
                start <- config[[elem]][1]
                end <- config[[elem]][2]
            } else {
                start <- min
                end <- max
            }
            dateRangeInput(elem, elem, min=min, max=max, start=start, end=end)    
        },
        
        dynamicFactorFilter = function(data, elem) {
            config <- .self$selectedConfig()
            choices <- levels(as.factor(data[,elem]))
            # the order here is important,  if the last thing the user did was load config, it should take precendence.
            if (!is.null(session$input[[elem]])) {
                selected <- session$input[[elem]]
            } else if (elem %in% names(config)) {
                selected <- config[[elem]]
            } else {
                selected <- c()
            }
            selectInput(elem, elem, choices=choices, selected=selected, multiple=TRUE)  
        },
        
        applyDataFilter = function(data, inputId, type) {
            output <- 
                switch(type,
                       "dynamicRanges"= {
                           data[,inputId] <- as.numeric(data[,inputId])
                           data[
                               which(
                                   data[,inputId] >= as.numeric(session$input[[inputId]][1]) &
                                       data[,inputId] <= as.numeric(session$input[[inputId]][2])
                               ), 
                               # leaving the second argument empty like this means all columns will be selected
                               ]
                       },
                       "dynamicDateRanges" = {
                           dateInterval <- interval(session$input[[inputId]][1],session$input[[inputId]][2]) 
                           
                           data[which(
                               as.Date(data[,inputId]) %within% dateInterval
                           ), ]
                       },
                       "dynamicFactors" = data[
                           which(
                               data[,inputId] %in% session$input[[inputId]]
                           ), ]  
                )
            output
        },
        
        
        applyDynamicFilters = function(data) {
            output <- data
            for (i in 1:length(.self$registeredInputs)) {
                elem <- .self$registeredInputs[[i]]
                if (length(grep("^dynamic", elem$type)) > 0 && !is.null(session$input[[elem$id]]) && session$input[[elem$id]] != "") {
                    childElementsExist <- FALSE
                    for (i in 1:length(session$input[[elem$id]])) {
                        if (!is.null(session$input[[session$input[[elem$id]][i]]])) {
                            childElementsExist <- TRUE
                        }
                    }
                    if (childElementsExist) {
                        print(paste("Applying filter", elem$id))
                        output <- .self$applyDynamicFilter(data,elem$id,elem$type)    
                    }
                }
            }
            output
        },
        
        updatedInterface = function() {
            config <- .self$selectedConfig()
            if (is.null(config)) { 
                print("config is null, nothing to update")
                return(TRUE) 
            }
            allupdated <- TRUE
            for (elem in names(config)) {
                print(paste("checking status of", elem))
                print(paste("what is the config val", config[[elem]]))
                if (is.null(session$input[[elem]]) || config[[elem]] != session$input[[elem]]) {
                    print(paste(elem,"not updated"))
                    allupdated <- FALSE
                } else {
                    print(paste(elem,"updated"))
                }
            }
            print(paste("Are we updated?", allupdated))
            allupdated
            
        }
    )
)
