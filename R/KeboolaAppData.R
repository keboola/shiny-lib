#' This is the configuration component library for managing app data loading / saving
#'
#' @import methods
#' @import shiny
#' @import keboola.shiny.lib
#' @export KeboolaAppData
#' @exportClass KeboolaAppData
KeboolaAppData <- setRefClass(
    'KeboolaAppData',
    fields = list(
        client = 'ANY', # keboola.sapi.r.client::SapiClient
        bucket = 'character',
        runId = 'character',
        lastSaveValue = 'numeric',
        # last table loaded from SAPI 
        lastTable = 'character'
    ),
    methods = list(
        #' Constructor.
        #'
        #' @param sapiClient - Keboola.sapi.r.client::SapiClient
        #' @param bucketId - Bucket where config table is stored
        #' @param run_id - the runId of the data to load
        #'  it will be read from command line argument.
        #' @exportMethod
        initialize = function(sapiClient, bucketId, run_id) {
            if (is.null(client)) {
                stop("Can not initialize KeboolaAppData.  No valid Sapi Client.")
            }
            client <<- sapiClient 
            bucket <<- bucketId
            runId <<- run_id
            lastTable <<- ''
            lastSaveValue <<- 0
        },
        
        #' Load tables from SAPI
        #' @param tableNames Vector or character table names (without bucket) to be loaded
        #' @param progressBar Optional Shiny progress bar, it is assumed to be in range 1,100
        #' @return list of data indexed by table name.
        #' @exportMethod 
        loadTables = function(session, tables, options = list(progressBar = TRUE, cleanData = FALSE)) {
            tryCatch({
                if (options$progressBar) {
                    progressBar <- shiny::Progress$new(session, min = 1, max = 120)
                    progressBar$set(message = 'Retrieving Data', detail = 'This may take a while...')    
                    progressBar$set(value = 2)
                }
                ret <- list()
                cntr <- 0
                for (name in names(tables)) {
                    lastTable <<- paste0(.self$bucket, ".", tables[[name]])
                    ret[[name]] <- .self$client$importTable(
                        .self$lastTable,
                        options = list(whereColumn = "run_id", whereValues = .self$runId)
                    )
                    if (options$progressBar) {
                        cntr <- cntr + 1
                        progressBar$set(value = ((100 / length(tables)) * cntr))
                    }
                }
                if (options$cleanData && c("cleanData", "columnTypes") %in% names(tables)) {
                    ret$columnTypes <- ret$columnTypes[,!names(ret$columnTypes) %in% c("run_id")]
                    ret$cleanData <- .self$getCleanData(ret$columnTypes, ret$cleanData)
                }
                if (options$progressBar) {
                    progressBar$set(value = 100)
                    progressBar$close()    
                }
                return(ret)
            }, error = function(e) {
                # convert the error to a more descriptive message
                stop(paste0("Error loading table ", .self$lastTable, " from SAPI (", e, ')'))
            })
        },
        
        #' Get results descriptor from SAPI
        #' @return nested list of elements.
        getDescriptor = function() {
            print("getDescriptor")
            tryCatch({
                data <- .self$client$importTable(
                    paste0(.self$bucket, ".finalResults"), 
                    options = list(
                        columns = c("item", "run_id", "sequence", 'value'),
                        whereColumn = "run_id",
                        whereValues = .self$runId
                    )
                )
                
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
                return(descriptor)
            }, error = function(e) {
                stop(paste0("I cannot load descriptor (from finalResults table) from SAPI (", e, ")"))
            })
        },
        
        #' Internal method that returns HTML content for the given section node
        #' @param section - node of the descriptor
        #' @param level - heading level (1=h2, 2=h3)
        #' @param customElements - callback for processing custom elements
        #' @return list of HTML elements
        processSection = function(section, level, customElements) {
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
        
        #' Method returns HTML content for a descriptor 
        #' @param appTitle - title of the app
        #' @param customElements - callback for printing custom elements, signature: function(elementId, content)
        #'  function should return a single HTML element. Pass NULL to ignore custom elements.
        #'
        #' @exportMethod
        getDescription = function(appTitle, customElements, session) {
            print("getDescription kdat")
            if (is.null(.self$client)) {
                return(NULL)
            }
            progressBar <- shiny::Progress$new(session, min = 1, max = 100)
            progressBar$set(message = 'Initializing', detail = 'Preparing components...')    
            progressBar$set(value = 2)
            descriptor <- .self$getDescriptor()
            print("got descriptor kdat")
            progressBar$set(value=40)
            
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
                print(paste("counter", cntr))
                progressBar$set(value=40 + 60/length(descriptor$sections)*cntr)
                print("processed section kdat")
            }
            print("close progress bar kdat")
            progressBar$set(value=100)
            progressBar$close()
            options(oldOptions)
            print("getDescription exit")
            return(contentRet)
        },
        
        #' Convert LG type definition to an R data type
        #' @param type LG data type string ('integer', 'datetime', etc.)
        #' @param mode LG variable mode ('continuous', 'discrete')
        #' @return string R data type name.
        getConvertedDataType = function(type, mode) {
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
        
        #' Apply column types detected by LG to a data frame.
        #' @param types Data frame with contents of table with LG datatypes 
        #'  (this table is usually named 'VAI__1__Actual' in SAPI)
        #' @param cleanData A data frame with actual data, its columns are
        #'  expected to be listed as rows in the types table.
        #' @return data frame supplied in cleanData parameter with 
        #'  applied data types.
        #' @exportMethod 
        getCleanData = function(types, cleanData) {   
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
        
        #' Get the UI elements for the data saving form
        #' 
        #' @return List of html elements that make up the form
        #' @exportMethod
        saveDataFormUI = function() {
            buckets <- .self$client$listBuckets()
            bucketNames <- lapply(buckets, function(x) { x$id }) 
            ret <- div(style = 'margin-top: 20px',
                wellPanel(
                    uiOutput("saveResultUI"),
                    div(
                        selectInput("outBucket", "Storage bucket", choices = bucketNames, selected = .self$bucket),
                        textInput("outTable", "Table Name"),
                        actionButton("saveIt", "Save It")
                    )
                )
            )    
        },
        
        saveResultUI = function(session, dataToSave) {
            write("Attempting to save data",stderr())
            if (is.null(dataToSave)) {
                return(div())
            }
            if (session$input$saveIt > .self$lastSaveValue) {
                if (nchar(session$input$outTable) > 0) {
                    lastSaveValue <<- as.numeric(session$input$saveIt)
                    print("Saving data")
                    tryCatch({
                        .self$client$saveTable(dataToSave(), session$input$outBucket, session$input$outTable)
                        ret <- div(class = 'alert alert-success', paste0("Table successfully saved as ", session$input$outBucket, '.', session$input$outTable, "."))
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
        
        
        #' @exportMethod
        dataModalButton = function(session) {
            list(
                keboolaModalButton(
                    "dataModal",
                    label = "",
                    icon = icon("file"),
                    title = "Data",
                    content = .self$saveDataFormUI()
                )
            )
        }
    )
)

