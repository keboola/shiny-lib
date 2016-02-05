#' This is the configuration component library for managing app data loading / saving
#'
#' @import methods
#' @import shiny
#' @field session 
#' @field client Storage API client.
#' @field db Redshift driver connection.
#' @field bucket Bucket Id.
#' @field runId Run Id
#' @field lastSaveValue TODO
#' @field lastTable TODO
#' @field maxMemory TODO
#' @field sourceData TODO
#' @field memoryUsage TODO
#' @field loadList TODO 
#' @field allLoaded TODO 
#' @export KeboolaAppData
#' @exportClass KeboolaAppData
KeboolaAppData <- setRefClass(
    'KeboolaAppData',
    fields = list(
        session = 'ANY', # shiny server session
        client = 'ANY', # keboola.sapi.r.client::SapiClient
        db = 'ANY', # keboola.redshift.r.client::RedshiftDriver
        bucket = 'character',
        runId = 'character',
        lastSaveValue = 'numeric',
        lastTable = 'character', # last table loaded from SAPI 
        maxMemory = 'numeric', #maximum allowed table size in bytes
        sourceData = 'list', # Where we have the source data
        memoryUsage = 'numeric', 
        loadList = 'list',
        allLoaded = 'logical'
    ),
    methods = list(
        initialize = function(sapiClient, appConfig, dbConnection, maxMemory = 500000000, session = getDefaultReactiveDomain()) {
            "Constructor.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{sapiClient} Storage API client.}
            \\item{\\code{bucketId} Bucket where config table is stored.}
            \\item{\\code{run_id} The runId of the data to load.}
            \\item{\\code{dbConnection} An established database connection.}
            \\item{\\code{maxMemory} Maximum sourceData memory allocation.}
            }}"
            if (is.null(client)) {
                stop("Can not initialize KeboolaAppData.  No valid Sapi Client.")
            }
            
            if (!inherits(session, "ShinySession"))
                stop("'session' is not a ShinySession object.")
            
            session <<- session
            
            client <<- sapiClient 
            bucket <<- appConfig$bucket
            if (is.null(appConfig$runId)) {
                runId <<- ''
            } else {
                runId <<- appConfig$runId        
            }
            lastTable <<- ''
            lastSaveValue <<- 0
            db <<- dbConnection
            maxMemory <<- maxMemory
            sourceData <<- list()
            memoryUsage <<- 0
            loadList <<- list()
            allLoaded <<- FALSE
        },
        
        loadTable = function(prettyName, table) {
            "Internal method to retrieve table data from Redshift 
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{prettyName} Table name to be used in labels throughout the app.}
            \\item{\\code{table} Name of table in SAPI.}
            }}
            \\subsection{Return Value}{data.frame with table data.}"
            print(paste("loading table ", prettyName, table))
            session$sendCustomMessage(
                type = "updateProgress",
                message = list(
                    id=paste0(table,"_progress"), 
                    parentId="data_retrieval",
                    text=paste("Retrieving", prettyName, "table."), value="In Progress", valueClass="text-primary"))
            
            lastTable <<- paste0("\"", .self$bucket, "\".\"", table, "\"")
            opts <- NULL
            
            if (nchar(.self$runId) > 0) {
                print("loading runid included table")
                sourceData[[prettyName]] <<- .self$db$select(paste0("SELECT * FROM ", .self$lastTable, " WHERE ", .self$lastTable, ".\"run_id\" = ?;"), .self$runId)
            } else {
                sourceData[[prettyName]] <<- .self$db$select(paste0("SELECT * FROM ", .self$lastTable))
            }    
            session$sendCustomMessage(
                type = "updateProgress",
                message = list(
                    id=paste0(table,"_progress"),
                    parentId="data_retrieval",
                    text=paste("Retrieving", prettyName, "table."), value="Completed", valueClass="text-success"))
            
        },
        
        getRecipeTables = function(options) {
            "TODO
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{options} List of options to pass on to the loadTables method.}
            }}
            \\subsection{Return Value}{List of tables. each element has required name, and optional reducible.}"
            query <- paste0("SELECT * FROM \"", .self$bucket, "\".\"finalResults\" WHERE \"", .self$bucket, "\".\"finalResults\".\"item_type\" = 'table' AND \"", .self$bucket, "\".\"finalResults\".\"value\" != '';")
            print(paste("getRecipeTable Query", query))
            tablesList <- .self$db$select(query)    
            tables <- list()
            for (i in 1:nrow(tablesList)) {
                tables[[tablesList[i, c("item")]]] <- list(name = tablesList[i,c("value")])
                if (!is.null(options$irReducibles) && tablesList[i, "value"] %in% options$irReducibles) {
                    tables[[tablesList[i, "item"]]][[reducible]] = TRUE
                }
            }
            # return the table list
            tables
        },
        
        loadTables = function(tables, options = list(cleanData = FALSE, description = FALSE)) {
            "Load tables from SAPI.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{tables} List of tables key as R variable, value as table name (without bucket).}
            \\item{\\code{options} List with items:
            cleanData boolean TRUE to compute datatypes of cleanData table
            description boolean TRUE to include descriptor in returned dataSet.}
            }}
            \\subsection{Return Value}{List of data indexed by variable name given in tables argument keys.}"
            print(tables)
            tryCatch({
                for (name in names(tables)) {
                    print(paste("loading table", name))
                    loadTable(name,tables[[name]]$name)
                    print(paste("loaded table", name))
                }
                # Apply data type setting if the cleandata columns are present and the option is set
                if (options$cleanData && c("cleanTable", "columnTypes") %in% names(tables)) {
                    sourceData$columnTypes <<- .self$sourceData$columnTypes[,!names(.self$sourceData$columnTypes) %in% c("run_id")]
                    sourceData$cleanData <<- .self$getCleanData(.self$sourceData$columnTypes, .self$sourceData$cleanTable)
                }
                
                # Retrieve descriptor if option is set
                if (options$description) {
                    sourceData$descriptor <<- .self$getDescriptor()
                }  
                
                TRUE
            }, error = function(e) {
                # convert the error to a more descriptive message
                stop(paste0("Error loading table ", .self$lastTable, " from SAPI (", e, ')'))
            })
        },
        
        checkTables = function(tables) {
            "Checks the table list to see if maximum application memory will be exceeded by retrieving the data
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{tables} List of tables to load from SAPI.}
            }}
            \\subsection{Return Value}{If memory exceeded - a list of table metadata for each requested table, else NULL.}"
            tableMetaList <- list()
            for (table in names(tables)) {
                fullTableName <- paste0(.self$bucket, ".", tables[[table]]$name)
                tableMeta <- .self$client$getTable(fullTableName)
                tableMeta$shinyName <- table
                if (is.null(tables[[table]]$reducible)) {
                    tableMeta$reducible <- TRUE
                } else {
                    tableMeta$reducible <- tables[[table]]$reducible    
                }
                # check the table size
                print(paste("PRIOR mem usage at",.self$memoryUsage, "table",tableMeta$name, table))
                memoryUsage <<- .self$memoryUsage + as.numeric(tableMeta$dataSizeBytes)
                print(paste("POST mem usage at",.self$memoryUsage, "table",tableMeta$name))
                tableMetaList[[tables[[table]]$name]] <- tableMeta
            }
            if (.self$memoryUsage > .self$maxMemory) {
                print(paste("memoryUsage:", .self$memoryUsage, "maxMemory", .self$maxMemory))
                tableMetaList
            } else {
                NULL
            }
        },
                
        getDescriptor = function() {
            "Get results descriptor from SAPI
            \\subsection{Return Value}{Nested list of elements.}"
            print("getDescriptor")
            session$sendCustomMessage(
                type = "updateProgress",
                message = list(
                    id="descriptor_progress",
                    parentId="data_retrieval",
                    text="Retrieving summary table.", value="In Progress", valueClass="text-primary"))
            tryCatch({
                fullTableName <- paste0("\"", .self$bucket, "\".\"finalResults\"")
                columns <- c("item","run_id", "sequence", "value")
                fullColumns <- paste(paste0(fullTableName,".\"",columns,"\""),collapse=", ")
                
                print(paste("FULL COLUMNS",fullColumns))
                # fetch the data
                data <- .self$db$select(paste0(
                                "SELECT ", fullColumns, " FROM ", 
                                fullTableName,
                                " WHERE run_id = ?;"), .self$runId)
                
                # grab only descriptors - needs to deal with split values from Redshift
                descriptors <- data[which(data$item == 'descriptor'), ]
                # get indexes of duplicated values (can happen if the job is run multiple times)
                dupes <- duplicated(descriptors[, "sequence"])
                # get only non duplicate items
                descriptors <- descriptors[!dupes, ]
                descriptors$sequence <- as.integer(descriptors$sequence)
                # sort by sequence and get only the value
                descriptors <- descriptors[order(descriptors$sequence), 'value']
                # paste into a single string
                descriptor <- paste0(descriptors, collapse = '')
                # process JSON
                descriptor <- jsonlite::fromJSON(descriptor, simplifyVector = FALSE)
                print("getDescriptor finsihed")
                # inform the progress panel that we're finished
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(
                        id="descriptor_progress",
                        parentId="data_retrieval",
                        text="Retrieving summary table.", value="Completed", valueClass="text-success"))
                return(descriptor)
            }, error = function(e) {
                stop(paste0("I cannot load descriptor (from finalResults table) from SAPI (", e, ")"))
            })
        },
        
        processSection = function(section, level, customElements) {
            "Internal method that returns HTML content for the given section node.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{section} Node of the descriptor.}
            \\item{\\code{level} Heading level (1 = h2, 2 = h3).}
            \\item{\\code{customElements} Callback for processing custom elements.}
            }}
            \\subsection{Return Value}{List of HTML elements.}"
            print("processSection")
            sectionRet <- list()
            if (level == 1) {
                sectionRet[[length(sectionRet) + 1]] <- h2(section$title)
            } else {
                sectionRet[[length(sectionRet) + 1]] <- h3(section$title)
            }
            for (statement in section$statements) {
                if (length(statement) > 0) {
                    statementRet <- list()
                    if (statement$type == 'text') {
                        statementRet[[length(statementRet) + 1]] <- 
                            HTML(
                                paste0(
                                    '<p>',
                                    gsub('\\[(.*?)\\[([a-zA-Z0-9_]+)\\]\\]', '<span class="kb-hint" title="" id="\\2">\\1</span>', statement$content),
                                    '</p>'
                                )
                            )
                        for (hint in statement$hints) {
                            statementRet[[length(statementRet) + 1]] <- 
                                HTML(
                                    paste0("<script>$('#", hint$id, "').attr('title', '", gsub("'", "\\'", hint$hint, fixed = TRUE), "');</script>")
                                )
                        }
                    }
                    if (statement$type == 'image') {
                        statementRet[[length(statementRet) + 1]] <- img(src = statement$content, width = 900)
                    }
                    if (statement$type == 'table') {
                        df <- data.frame()
                        for (row in statement$content) {
                            if (class(row) == 'list') {
                                df <- rbind(df, unlist(row))
                                names(df) <- names(row)
                            } # else, ignore the row
                        }
                        
                        if (nrow(df) > 0) {
                            tid <- stringi::stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z]")
                            statementRet[[length(statementRet) + 1]] <- 
                                DT::datatable(
                                    df,
                                    class = 'table display',
                                    caption = statement$details
                                )
                        }
                    } 
                    if ((statement$type == 'plot') || (statement$type == 'custom')) {
                        if (!is.null(customElements)) {
                            statementRet[[length(statementRet) + 1]] <- p(customElements(statement$id, statement$content), class = 'lg-plot')
                        }
                    }
                    if (nchar(statement$example) > 0) {
                        statementRet[[length(statementRet) + 1]] <- p(statement$example, class = 'lg-example')
                    }
                    if (nchar(statement$details) > 0) {
                        # skip details for table, they have been already used as caption
                        statementRet[[length(statementRet) + 1]] <- tag('details', statement$details)
                    }
                    htmlDiv <- div(statementRet, class='lg-statement')
                    sectionRet[[length(sectionRet) + 1]] <- htmlDiv
                }
            }
            print("processSection end")
            return(sectionRet)
        },
        
        getDescription = function(appTitle, customElements, desc = NULL) {
            "Method returns HTML content for a descriptor.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{appTitle} Title of the app.}
            \\item{\\code{customElements} Callback for printing custom elements, 
            signature: function(elementId, content)
            function should return a single HTML element. Pass NULL to ignore custom elements.}
            \\item{\\code{desc} Descriptor contents. If NULL, then \\code{getDescriptor} will be used.}
            }}
            \\subsection{Return Value}{TODO}"
            print("getDescription kdat")
            if (is.null(.self$client)) {
                return(NULL)
            }
            
            if (!is.null(desc)) {
                descriptor <- desc
            } else {
                if (is.null(.self$sourceData$descriptor)) {
                    sourceData$descriptor <<- .self$getDescriptor()    
                }
                descriptor <- .self$sourceData$descriptor
            }
            
            oldOptions <- options(stringsAsFactors = FALSE)
            contentRet <- list()
            contentRet[[length(contentRet) + 1]] <- h1(appTitle)
            cntr <- 0
            for (section in descriptor$sections) {
                if (length(section$subsections) == 0) {
                    sectionRet <- list()
                    sectionRet[[length(sectionRet) + 1]] <- processSection(section, 1, customElements)
                    contentRet[[length(contentRet) + 1]] <- tag('section', sectionRet)
                } else {
                    sectionRet <- list()
                    groups <- c()
                    for (subsection in section$subsections) {
                        groups <- c(groups, subsection$title)
                    }
                    groups <- as.list(groups)
                    
                    for (subsection in section$subsections) {
                        ld <- list()
                        ld[[length(ld) + 1]] <- processSection(subsection, 2, customElements)
                        sectionRet[[length(sectionRet) + 1]] <- tag('li', ld)
                    }
                    sectionRet[['class']] <- 'kb-enumeration'
                    ul <- tag('ul', sectionRet)
                    sectionRet <- list()
                    sectionRet[[length(sectionRet) + 1]] <- processSection(section, 1, customElements)
                    sectionRet[[length(sectionRet) + 1]] <- ul
                    contentRet[[length(contentRet) + 1]] <- tag('section', sectionRet)
                }
                cntr <- cntr + 1
                # progressBar$set(value=40 + 60/length(descriptor$sections)*cntr)
                print(paste("processed section", cntr))
            }
            options(oldOptions)
            print("getDescription exit kdat")
            return(contentRet)
        },

        getConvertedDataType = function(type, mode) {
            "Convert LG type definition to an R data type.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{type} LG data type string ('integer', 'datetime', etc.).}
            \\item{\\code{mode} LG variable mode ('continuous', 'discrete').}
            }}
            \\subsection{Return Value}{R data type name.}"
            if (is.null(type) || is.na(type) || (length(type) == 0)) {
                ret <- 'character'
            } else if (type == "integer" || type == "float") {
                ret <- "numeric"
            } else if (type == "date") {
                ret <- "date"
            } else if (type == "datetime") {
                ret <- "posix"
            } else if (type == "string") {
                if (mode == "continuous") {
                    ret <- "character"
                } else {
                    ret <- "factor"
                }
            } else {
                ret <- "factor"
            }
            return(ret)
        },
        
        getCleanData = function(types, cleanData) {
            "Apply column types detected by LG to a data frame.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{types} Data frame with contents of table with LG datatypes 
            (this table is usually named 'VAI__1' in SAPI).}
            \\item{\\code{cleanData} A data frame with actual data, its columns are
            expected to be listed as rows in the types table.}
            }}
            \\subsection{Return Value}{data.frame supplied in cleanData parameter with applied data types.}"
            # remove columns run_id and _timestamp which are internal only
            cleanData <- cleanData[,!names(cleanData) %in% c("run_id", "_timestamp")]
            out <- lapply(
                1:length(cleanData),
                FUN = function(i) {
                    varName <- colnames(cleanData)[i]
                    varType <- types[which(types$var_name == varName),]
                    # there may be still multiple definitions if a job was executed repeatedly, so pick the first one
                    type <- .self$getConvertedDataType(varType[1, "data_type"], varType[1, "mode"])
                    FUN1 <- switch(
                        type,
                        "posix" = as.POSIXlt,
                        "date" = as.Date,
                        "character" = as.character,
                        "numeric" = as.numeric,
                        "factor" = as.factor
                    )
                    if (type == "date" || type == "posix") {
                        cleanData[which(cleanData[,i] == ""), i] <- NA
                    }
                    FUN1(cleanData[,i])
                }
            )
            names(out) <- colnames(cleanData)
            return(as.data.frame(out))
        },
        
        saveDataFormUI = function(dataToSave) {
            "Get the UI elements for the data saving form
            \\subsection{Return Value}{List of html elements that make up the form.}"
            buckets <- .self$client$listBuckets()
            bucketNames <- lapply(buckets, function(x) { x$id }) 
            colnames <- paste(as.character(names(dataToSave())), collapse=", ")
            ret <- div(style = 'margin-top: 20px',
                helpText(paste("Save your currently filtered data to SAPI.  The table will have the following columns:", colnames)),
                wellPanel(
                    uiOutput("kb_saveResultUI"),
                    div(
                        selectInput("kb_outBucket", "Storage bucket", choices = bucketNames, selected = .self$bucket),
                        textInput("kb_outTable", "Table Name"),
                        actionButton("kb_saveIt", "Save It")
                    )
                )
            )    
        },
        
        saveResultUI = function(dataToSave) {
            "This method attempts to save the data.frame given by dataToSave, 
             and returns a UI element which alerts to the success or failure of the operation
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{dataToSave} the name of the data.frame that will be saved to SAPI}
            }}
            \\subsection{Return Value}{The success or failure alert dom element}"
            if (is.null(dataToSave)) {
                return(div())
            }
            if (session$input$kb_saveIt > .self$lastSaveValue) {
                if (nchar(session$input$kb_outTable) > 0) {
                    lastSaveValue <<- as.numeric(session$input$kb_saveIt)
                    print("Saving data")
                    tryCatch({
                        .self$client$saveTable(dataToSave(), session$input$kb_outBucket, session$input$kb_outTable)
                        ret <- div(class = 'alert alert-success', paste0("Table successfully saved as ", session$input$kb_outBucket, '.', session$input$kb_outTable, "."))
                    }, error = function(e) {
                        ret <- div(class = 'alert alert-danger', 
                                   paste0("Error saving table: ", e, 
                                        "\n Please note that table names may only contain alphanumeric characters, dashes '-', and underscores '_'"))
                        write(paste("Error saving table:", e),stderr())
                    })
                } else {
                    ret <- div(class = 'alert alert-warning', "Please enter table name.")
                }
            } else {
                ret <- div()
            }
            return(ret)
        },
        
        dataModalButton = function(dataToSave) {
            "Method for the config button that opens the save data modal dialog
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{dataToSave} the name of the data.frame that will be saved to sapi. }
            }}
            \\subsection{Return Value}{TODO}"
            list(
                keboolaModalButton(
                    "kb_dataModal",
                    label = "",
                    icon = icon("save"),
                    title = "Save Data to SAPI",
                    content = .self$saveDataFormUI(dataToSave)
                )
            )
        },
        
        previewData = function(tableMeta) {
            "TODO
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{tableMeta} TODO}
            }}
            \\subsection{Return Value}{TODO}"
            reactive({
                session$output[[paste0("kb_",tableMeta$name,"_columnFiltersUI")]] <- renderUI({
                    lapply(session$input[[paste0("kb_",tableMeta$name,"_filters")]],function(arg){
                        if (!is.null(session$input[[paste0("kb_",arg,"_filter")]])) {
                            textInput(paste0("kb_",arg,"_filter"), paste(arg,"Filter"), value = session$input[[paste0("kb_",arg,"_filter")]])
                        } else {
                            textInput(paste0("kb_",arg,"_filter"), paste(arg,"Filter"))
                        }
                    })
                })
                refresh <- session$input[[paste0("kb_",tableMeta$name,"_refresh")]]
                load <- as.numeric(session$input[[paste0("kb_",tableMeta$name,"_load")]])
                isolate({
                    
                    fullTableName <- paste0("\"", tableMeta$bucket$id, "\".\"",tableMeta$name, "\"")
                    cols <- session$input[[paste0("kb_",tableMeta$name,"_columns")]]
                    fullColumnNames <- paste(paste0(fullTableName,".\"",cols,"\""),collapse=", ")
                    
                    filters <- session$input[[paste0("kb_",tableMeta$name,"_filters")]]
                    argstr <- ""
                    arglist <- list()
                    if (!is.null(filters) && length(filters) > 0) {
                        for (filter in filters) {
                            filterName <- paste0("kb_",filter,"_filter")
                            value <- session$input[[filterName]]
                            fullFilter <- paste0(fullTableName,".\"",filter,"\"")
                            arglist[[fullFilter]] <- value
                        }
                        argstrings <- paste(names(arglist)," != ?")
                        
                        argstr <- paste("WHERE",paste(argstrings,collapse=" AND "))
                        
                    }
                    if (load > .self$loadList[[tableMeta$name]]) {
                        print("laod button has been clicked")
                        limit <- ""
                        session$sendCustomMessage(
                            type = "updateProgress",
                            message = list(
                                id=paste0("kb_",tableMeta$name,"_progress"), 
                                parentId="data_retrieval",
                                text=paste("Retrieving", tableMeta$shinyName, "table."), value="In Progress", valueClass="text-primary"))
                        
                    } else {
                        limit <- " LIMIT 50"
                    }
                    query <- paste0("SELECT ",fullColumnNames," FROM ", fullTableName, argstr, limit)
                    print(paste("previewData query:",query))
                    
                    dat <- .self$db$select(query,arglist)
                    
                    
                    if (load > .self$loadList[[tableMeta$name]]) {
                        loadList[[tableMeta$name]] <<- load
                        
                        sourceData[[tableMeta$shinyName]] <<- dat
                        session$sendCustomMessage(
                            type = "updateProgress",
                            message = list(
                                id=paste0("kb_",tableMeta$name,"_progress"), 
                                parentId="data_retrieval",
                                text=paste("Retrieved", tableMeta$shinyName, "table.",print(object.size(dat),units='b')," bytes used."), value="Completed", valueClass="text-success"))
                        session$sendCustomMessage(
                            type = "renameButton",
                            message = list(
                                    buttonId=paste0("kb_",tableMeta$name,"_load"),
                                    text="Reload Selection"
                                ))
                        
                        dat <- data.frame(
                            status="Table successfully loaded.",
                            memory=format(object.size(dat),units='auto'),
                            rows=nrow(dat)
                        )
                    }
                    .self$setMemoryUsage(tableMeta)
                    if (.self$allLoaded) {
                        print(paste("ALL LOADED NAMES", names(.self$sourceData)))
                    }
                    dat
                })
            })
        },
        
        setMemoryUsage = function(tableMeta) {
            "TODO
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{tableMeta} TODO}
            }}
            \\subsection{Return Value}{TODO}"
            memoryUsage <<- 0
            
            print(paste("LOAD LIST",names(.self$loadList)))
            print(.self$loadList)
            print(paste("SOURCEDATA", names(.self$sourceData)))
            allLoaded <<- TRUE
            for (table in names(loadList)) {
                print(paste("CHECKING TABLE", table, " loaded? ", loadList[[table]]))
                if (loadList[[table]] == 0) {
                    print(paste("TABLE", table, " IS LOADED???  BUGGER"))
                    memoryUsage <<- .self$memoryUsage + tableMeta$dataSizeBytes
                    allLoaded <<- FALSE
                } else {
                    memoryUsage <<- .self$memoryUsage + as.numeric(object.size(.self$sourceData[[tableMeta$shinyName]]))
                    print(paste("SHINY NAME:", tableMeta$shinyName, "TOTAL MEM : ", .self$memoryUsage, "TABLE", table, "MEM", as.character(object.size(.self$sourceData[[tableMeta$shinyName]]))))
                }
            }
            print(paste("ALL LOADED?", .self$allLoaded, "mem usage", .self$memoryUsage, "maxmem", .self$maxMemory))
            if (.self$allLoaded && .self$memoryUsage < .self$maxMemory) {
                session$output$kb_detourMessage <- 
                    renderUI(
                        fluidRow(class="alert alert-success", 
                            column(9,div(
                                paste("The tables combine to (",as.character(.self$memoryUsage),"Bytes).  This is less than the memory limit (", .self$maxMemory, 
                                      "Bytes) for this application. "))),
                            column(3,actionButton("kb_continue", "Continue", class="navbar-right"))
                        )    
                    )
            }else {
                session$output$kb_detourMessage <- 
                    renderUI(
                        div(class="alert alert-warning", 
                            paste("The tables combine to (",as.character(.self$memoryUsage),"Bytes).  This is greater than the memory limit (", .self$maxMemory, 
                                  "Bytes) for this application. ",
                                  "Please discard any columns that you believe will not be of interest,",
                                  "and/or remove rows containing unwanted values."))    
                    )    
            }
        },
        
        problemTablesUI = function(problemTables) {
            "TODO
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{problemTables} TODO}
            }}
            \\subsection{Return Value}{TODO}"
            print(paste("trying to make tab for ", names(problemTables)))
            tabs <- lapply(names(problemTables),function(table){
                tableMeta <- problemTables[[table]]
                print(paste("loading table",tableMeta$name))
                if (tableMeta$reducible) {
                    loadList[[tableMeta$name]] <<- 0 #initiate the load button memory 
                    print(paste("adding tab for", table))
                    tabPanel(tableMeta$shinyName,.self$tableEditor(tableMeta))
                }else {
                    loaded <- .self$loadTable(tableMeta$shinyName, tableMeta$name)
                    loadList[[tableMeta$name]] <<- 1 # this table had reducible set so is auto-loaded
                    print(paste("loaded table",tableMeta$name))
                    NULL
                }
            })
            tabs <- Filter(Negate(is.null),tabs)
            print("made tabs")
            print(tabs)
            do.call(tabsetPanel, tabs)
        },
        
        tableEditor = function(tableMeta) {   
            "TODO
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{tableMeta} TODO}
            }}
            \\subsection{Return Value}{TODO}"
            print("setting up the tabs")
            tags$div(id=tableMeta$name, class="tableEditor",
                fluidRow(
                    column(4,
                        selectInput(paste0("kb_",tableMeta$name,"_columns"),"Keep These Columns",choices=tableMeta$columns,selected=tableMeta$columns,multiple=TRUE)
                    ),
                    column(8,
                        fluidRow(
                            helpText("Use these filters to EXCLUDE rows containing unwanted values for the selected variables"),
                            column(6,
                                selectInput(paste0("kb_",tableMeta$name,"_filters"),"Filter by a Column",choices=tableMeta$columns,multiple=TRUE)
                                
                            ),
                            column(6,
                                uiOutput(paste0("kb_",tableMeta$name,"_columnFiltersUI"))
                            )
                        )
                    )
                ),
                div(
                    actionButton(paste0("kb_",tableMeta$name,"_load"),"Load Selection", class='navbar-right btn-primary table-editor-btn'),
                    actionButton(paste0("kb_",tableMeta$name,"_refresh"),class="btn-primary navbar-right icon-refresh table-editor-btn",list(icon("refresh"),"Refresh Data Preview"))
                ),
                div(
                    h4("Data Preview"),
                    renderDataTable(previewData(tableMeta)(), options=list(searching=FALSE,info=FALSE))
                )
            )
        }
    )
)
