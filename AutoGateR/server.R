#AutoGateR SERVER----
server <- function(input, output, session) {
  runjs("$('#drawGateName').attr('maxlength',18)")
  runjs(
    "$('#drawGateName').bind('keypress', function(event) {
      var regex=new RegExp('^[/]+$');
      if(regex.test(String.fromCharCode(!event.charCode ? event.which:
      event.charCode))) {
          event.preventDefault()
      }
  })"
  )
  runjs("$('#autodrawGateName').attr('maxlength',18)")
  runjs(
    "$('#autodrawGateName').bind('keypress', function(event) {
      var regex=new RegExp('^[/]+$');
      if(regex.test(String.fromCharCode(!event.charCode ? event.which:
      event.charCode))) {
          event.preventDefault()
      }
  })"
  )
  hide("saveMatrix")
  hide("cancelMatrix")
  hide("prolifSButt")
  hide("prolifLabel")
  hide("prolifGrid")
  hide("prolifTable")
  hide("prevProlifS")
  hide("nextProlifS")
  hide("applyProlif")
  hide("exportImageProlif")
  hide("exportTableProlif")
  hidetSNE()
  disable("ovSamples")
  disable("step2")
  disable("step3")
  disable("exportTable")
  disable("editResult")
  disable("exportImageAncestry")
  disable("autoexportImageAncestry")
  disable("exportImageOverlay")
  disable("savetSNEPlot")
  disable("saveFile")
  js$disableTab("plotTab")
  js$disableTab("autoPlotTab")
  js$disableTab("compTab")
  disableTabs()
  autodisableTabs()
  
  reactautoPar <- reactiveValues(d = "root")
  reactautoNameChange <- reactiveValues(d = NULL)
  reactautosinglet <- reactiveValues(d = FALSE)
  reactautoInterval <- reactiveValues(d = FALSE)
  reactauto2D <- reactiveValues(d = FALSE)
  reactautoQuadrant <- reactiveValues(d = FALSE)
  reactautoHover <- reactiveValues(d = 0)
  autohoverCoords <- reactiveValues(d = c(NULL))
  auto2DCoords <- reactiveValues(d = data.frame(NULL))
  autoplotActivator <- reactiveValues(d = 0)
  autohierarchyActivator <- reactiveValues(d = 0)
  autobgActivator <- reactiveValues(d = 0)
  autocompPlotActivator <- reactiveValues(d = 0)
  reactautoGateFont <- reactiveValues(d = 5)
  reactautoShowGateName <- reactiveValues(d = TRUE)
  reactautoAxisFont <- reactiveValues(d = 15)
  reactautoShowAxis <- reactiveValues(d = TRUE)
  autoovActivator <- reactiveValues(d = 0)
  reactautoAxisCustom <- reactiveValues(d = NULL)
  autopolygonCoords <- reactiveValues(d = data.frame(NULL))
  autoquadCoords <- reactiveValues(d = c(NULL))
  autointCoords <- reactiveValues(d = data.frame(NULL))
  autoellipseCoords <- reactiveValues(d = data.frame(NULL))
  autoshowOvPlot <- reactiveValues(d = FALSE)
  autointcoords <- reactiveValues(d = data.frame(NULL))
  reactautoOvSamples <- reactiveValues(d = c())
  reactautoOvAxisFont <- reactiveValues(d = 15)
  reactautoOvShowAxis <- reactiveValues(d = TRUE)
  reactautoSampleOrder <- reactiveValues(d = NULL)
  autosampAlert <- reactiveValues(d = "")
  reactautoNameChange <- reactiveValues(d = NULL)
  reactPar <- reactiveValues(d = "root")
  reactNameChange <- reactiveValues(d = NULL)
  reactRectangle <- reactiveValues(d = FALSE)
  reactInterval <- reactiveValues(d = FALSE)
  reactPolygon <- reactiveValues(d = FALSE)
  reactQuadrant <- reactiveValues(d = FALSE)
  reactHover <- reactiveValues(d = 0)
  hoverCoords <- reactiveValues(d = c(NULL))
  polygonCoords <- reactiveValues(d = data.frame(NULL))
  plotActivator <- reactiveValues(d = 0)
  hierarchyActivator <- reactiveValues(d = 0)
  bgActivator <- reactiveValues(d = 0)
  reactGateFont <- reactiveValues(d = 5)
  reactShowGateName <- reactiveValues(d = TRUE)
  reactAxisFont <- reactiveValues(d = 15)
  reactShowAxis <- reactiveValues(d = TRUE)
  ovActivator <- reactiveValues(d = 0)
  showOvPlot <- reactiveValues(d = FALSE)
  reactOvSamples <- reactiveValues(d = c())
  sampAlert <- reactiveValues(d = "")
  reactModalTitle <- reactiveValues(d = "")
  reactSampleOrder <- reactiveValues(d = NULL)
  compPlotActivator <- reactiveValues(d = 0)
  reactReadOnly <- reactiveValues(d = TRUE)
  ag$appliedMatrix <- "Cytometer-defined"
  reactAxisCustom <- reactiveValues(d = NULL)
  prolifReady <- reactiveValues(d = FALSE)
  reactTSCh <- reactiveValues(d = NULL)
  reactTSSamp <- reactiveValues(d = NULL)
  reactGroupNames <- reactiveValues(d = NULL)
  reactTSPar <- reactiveValues(d = NULL)
  tSPlotActiv <- reactiveValues(d = 0)
  reacttSIDs <- reactiveValues(d = NULL)
  reacttSNEPops <- reactiveValues(d = NULL)
  reactOvAxisFont <- reactiveValues(d = 15)
  reactOvShowAxis <- reactiveValues(d = TRUE)
  reactDir <- reactiveValues(d = NULL)
  reactAutoGateRFile <- reactiveValues(d = NULL)
  reactHeight <- reactiveValues(d = 200)
  reactOverw <- reactiveValues(d = NULL)
  reactTestData <- reactiveValues(d = FALSE)
  ag$currentTable <- NULL
  ag$entiretSNE <- NULL
  ag$betweenPeaks <- NULL
  ag$refPeaks <- NULL
  ag$adjustedL <- NULL
  ag$concatSamples <- NULL
  ag$tSNEListofGroups <- NULL
  ag$tSNEListofSamples <- NULL
  ag$loadingFile <- ag$ovBoolean <- FALSE
  ag$loadingFile <- ag$autoovBoolean <- FALSE
  
  #File----
  shinyFileChoose(
    input,
    "AutoGateRFileLoad",
    roots = c(Home = fs::path_home_r(), getVolumes()()),
    session = session,
    restrictions = system.file(package = "base"),
    filetypes = "RData"
  )
  shinyDirChoose(
    input,
    "directory",
    roots = c(Home = fs::path_home_r(), getVolumes()()),
    session = session,
    restrictions = system.file(package = "base"),
    allowDirCreate = FALSE,
    filetypes = "fcs"
  )
  
  observeEvent(input$directory, {
    if (!is.integer(input$directory[1])) {
      reactDir$d <- input$directory
    }
  })
  
  ###File Type Sort and Checkbox-----
  observeEvent(reactDir$d, {
    withProgress(
      message = "Please wait...",
      detail = "",
      value = 0,
      max = 100,
      {
        pars <- c(
          Home = fs::path_home_r(),
          "R Installation" = R.home(),
          getVolumes()()
        )
        ag$tempFilePath <- parseDirPath(pars, reactDir$d)
        ag$tempFileList <-
          list.files(ag$tempFilePath, pattern = ".fcs")
        if (length(ag$tempFileList) == 0) {
          alert("No FCS file was found in the selected folder.")
        } else {
          setwd(ag$tempFilePath)
          validityOut <- vapply(seq(ag$tempFileList), function(x) {
            list(tryCatch({
              keyword(load_cytoframe_from_fcs(ag$tempFileList[x]))
            }))
          }, list(1))
          verifier <- vapply(validityOut, function(x)
            length(x) > 0, TRUE)
          if (all(verifier)) {
            channelLengths <- sapply(ag$tempFileList, function(x)
              length(colnames(load_cytoframe_from_fcs(x))))
            if (length(unique(channelLengths)) == 1) {
              tubeNameList <- vapply(validityOut, function(x)
                list(x$`TUBE NAME`), list(1))
              if (is.null(unlist(tubeNameList))) {
                tubeNameList <- vapply(validityOut, function(x)
                  list(x$`TUBENAME`), list(1))
              }
              if (is.null(unlist(tubeNameList))) {
                tubeNameList <- vapply(validityOut, function(x)
                  list(x$`$WELLID`), list(1))
              }
              if (is.null(unlist(tubeNameList))) {
                tubeNameList <- vapply(validityOut, function(x)
                  list(x$TBNM), list(1))
              }
              
              if (is.null(unlist(tubeNameList))) {
                tubeNameList <- substr(ag$tempFileList, 1,
                                       nchar(ag$tempFileList) - 4)
              }
              verifier <- vapply(tubeNameList, function(x)
                ! is.null(x), TRUE)
              if (all(verifier)) {
                tempFileList2 <- ag$tempFileList[grep("Unstained", ag$tempFileList)]
                tempFileList3 <-
                  ag$tempFileList[grep("Control", ag$tempFileList)]
                tempFileList3 <-
                  tempFileList3[-grep("Unstained", tempFileList3)]
                tempFileLis4 <-
                  ag$tempFileList[grep("FMO", ag$tempFileList)]
                tempFileList5 <-
                  ag$tempFileList[-grep("Unstained", ag$tempFileList)]
                tempFileList5 <-
                  tempFileList5[-grep("Control", tempFileList5)]
                tempFileList5 <-
                  tempFileList5[-grep("FMO", tempFileList5)]
                ag$tempFileList <-
                  c(tempFileList2,
                    tempFileList3,
                    tempFileLis4,
                    tempFileList5)
                
                tubeNameList2 <-
                  tubeNameList[grep("Unstained", tubeNameList)]
                tubeNameList3 <-
                  tubeNameList[grep("Control", tubeNameList)]
                tubeNameList3 <-
                  tubeNameList3[-grep("Unstained", tubeNameList3)]
                
                tubeNameList4 <-
                  tubeNameList[grep("FMO", tubeNameList)]
                
                tubeNameList5 <-
                  tubeNameList[-grep("Unstained", tubeNameList)]
                tubeNameList5 <-
                  tubeNameList5[-grep("Control", tubeNameList5)]
                tubeNameList5 <-
                  tubeNameList5[-grep("FMO", tubeNameList5)]
                
                tubeNameList <-
                  c(tubeNameList2,
                    tubeNameList3,
                    tubeNameList4,
                    tubeNameList5)
                
                ag$tempCompIDs <- list()
                for (i in seq(ag$tempFileList)) {
                  index <- length(ag$tempCompIDs) + 1
                  nam <- tubeNameList[[i]]
                  tubeNam <- strsplit(nam, " ")[[1]]
                  if (length(grep("Control", tubeNam)) > 0) {
                    sta <- grep("Stained", tubeNam)
                    unSta <- grep("Unstained", tubeNam)
                    if (length(sta) > 0
                        || length(unSta) > 0) {
                      ag$tempCompIDs[[index]] <- i
                    }
                  }
                }
                ag$tempCompIDs <- unlist(ag$tempCompIDs)
                ag$tempCompIDs <- ag$tempCompIDs
                listSeq <- seq(ag$tempFileList)
                id <- ag$tempCompIDs # controls
                ag$tempFmoIDs <- list()
                for (q in seq(ag$tempFileList)) {
                  index2 <- length(ag$tempFmoIDs) + 1
                  nam2 <- tubeNameList[[q]]
                  tubeNam2 <- strsplit(nam2, " ")[[1]]
                  if (length(grep("FMO", tubeNam2)) > 0) {
                    fmos <- grep("FMO", tubeNam2)
                    if (length(fmos) > 0) {
                      ag$tempFmoIDs[[index2]] <- q
                    }
                  }
                }
                ag$tempFmoIDs <- unlist(ag$tempFmoIDs)
                ag$tempFmoIDs <- ag$tempFmoIDs
                id2 <- ag$tempFmoIDs # fmos
                id3 <- append(id, id2) # fmos + controls
                ag$tempOnlySampIDs <- listSeq[-id3]
                ag$finalSelect <- list(as.list(listSeq),
                                       as.list(listSeq),
                                       as.list(listSeq))
                if (!is.null(id)) {
                  #controls
                  ag$finalSelect[[1]][id] <- TRUE
                  ag$finalSelect[[1]][-id] <- FALSE
                  #fmos
                  ag$finalSelect[[3]][id2] <- TRUE
                  ag$finalSelect[[3]][-id2] <- FALSE
                  #samples
                  ag$finalSelect[[2]][listSeq[-id3]] <- TRUE
                  ag$finalSelect[[2]][-listSeq[-id3]] <- FALSE
                  if (length(listSeq[-id3]) == 0) {
                    ag$finalSelect[[2]][id] <- FALSE
                    ag$finalSelect[[2]][id2] <- FALSE
                  }
                  if (length(id2) == 0) {
                    ag$finalSelect[[3]][id] <- FALSE
                    ag$finalSelect[[3]][listSeq[-id3]] <- FALSE
                  }
                } else {
                  ag$finalSelect[[1]] <- lapply(ag$finalSelect[[1]], function(x)
                    FALSE)
                  ag$finalSelect[[3]] <-
                    lapply(ag$finalSelect[[3]], function(x)
                      FALSE)
                  ag$finalSelect[[2]] <-
                    lapply(ag$finalSelect[[2]], function(x)
                      TRUE)
                }
                sampleColum <- list()
                for (i in seq(ag$tempFileList)) {
                  sampleColum[[i]] <- list(br(), br(),
                                           ag$tempFileList[i])
                }
                groupColums <- list(list(), list(), list())
                for (j in seq(3)) {
                  for (i in seq(ag$tempFileList)) {
                    groupColums[[j]][[i]] <- checkboxInput(
                      inputId = paste0("column", j, "samp", i),
                      label = "",
                      value = ag$finalSelect[[j]][[i]]
                    )
                  }
                }
                checkBoxColumns <- list(column(
                  width = 2,
                  align = "center",
                  style = "margin-top: 10px;",
                  list(strong("Compensation Controls"),
                       groupColums[[1]])
                ))
                checkBoxColumns[[2]] <- column(
                  width = 2,
                  align = "center",
                  style = "margin-top: 10px;",
                  list(strong("Samples"), groupColums[[2]])
                )
                checkBoxColumns[[3]] <- column(
                  width = 2,
                  align = "center",
                  style = "margin-top: 10px;",
                  list(strong("FMOs"), groupColums[[3]])
                )
                showModal(
                  modalDialog(
                    strong(
                      "Please indicate/confirm compensation
                                   controls and sample files."
                    ),
                    br(),
                    fluidRow(
                      column(
                        width = 6,
                        align = "right",
                        list(sampleColum)
                      ),
                      checkBoxColumns
                    ),
                    footer = list(
                      actionButton(
                        inputId = "okSelectCompAndSamp",
                        label = "Ok",
                        style = ag$blackB
                      ),
                      actionButton(
                        inputId = "cancelModal",
                        label = "Cancel",
                        style = ag$whiteB
                      )
                    ),
                    easyClose = FALSE,
                    size = "l"
                  )
                )
              } else {
                tubeList <- vapply(validityOut, function(x)
                  list(x$`TUBE NAME`), list(1))
                if (is.null(unlist(tubeList))) {
                  tubeList <- vapply(validityOut, function(x)
                    list(x$`TUBENAME`), list(1))
                }
                if (is.null(unlist(tubeList))) {
                  tubeList <- vapply(validityOut, function(x)
                    list(x$`$WELLID`), list(1))
                }
                if (is.null(unlist(tubeList))) {
                  tubeList <- vapply(validityOut, function(x)
                    list(x$TBNM), list(1))
                }
                if (is.null(unlist(tubeList))) {
                  tubeList <- substr(ag$tempFileList, 1,
                                     nchar(ag$tempFileList) - 4)
                }
                verifier <- vapply(tubeList, function(x)
                  ! is.null(x), TRUE)
                if (all(tubeList)) {
                  alert(
                    "Loading failed... At least one FCS
                                file had its integrity altered. AutoGateR
                                supports only cytomer-defined
                                      unmodified FCS files."
                  )
                } else {
                  alert(
                    "Loading failed... At least one FCS
                                file is of an unsupported version
                                      or is corrupted."
                  )
                }
              }
            } else {
              alert(
                "Loading failed... This software requires all
                        FCS files to be consistent with each other in the
                              number of channels."
              )
            }
          } else {
            alert("Loading failed... At least one FCS file seems
                          to be corrupted.")
          }
        }
        reactDir$d <- NULL
      }
    )
  })
  
  
  observeEvent(input$okSelectCompAndSamp, {
    for (j in seq(3)) {
      for (i in seq(ag$tempFileList)) {
        index <- paste0("column", j, "samp", i)
        ag$finalSelect[[j]][[i]] <- input[[index]]
      }
    }
    displayAlert <- NULL
    for (i in seq(ag$tempFileList)) {
      if (ag$finalSelect[[1]][[i]] == FALSE
          && ag$finalSelect[[2]][[i]] == FALSE
          && ag$finalSelect[[3]][[i]] == FALSE) {
        displayAlert <- "At least one file has not been checked."
      } else if (ag$finalSelect[[1]][[i]] == TRUE
                 && ag$finalSelect[[2]][[i]] == TRUE
                 && ag$finalSelect[[3]][[i]] == TRUE) {
        displayAlert <- "At least one file has been checked twice."
      }
    }
    verifier <- sapply(ag$finalSelect[[2]], function(x)
      x == FALSE)
    if (all(verifier)) {
      displayAlert <- "Only compensation control files were loaded.
            This software requires at least a sample file to be loaded."
    }
    if (is.null(displayAlert)) {
      withProgress(
        message = "Please wait...",
        detail = "",
        value = 0,
        max = 100,
        {
          hide("okSelectCompAndSamp")
          hide("cancelModal")
          ag$filePath <- ag$tempFilePath
          ag$fileList <- ag$tempFileList
          preList <- unlist(ag$finalSelect[[1]])
          ag$compControlIDs <- seq(ag$fileList)[preList]
          preList <- unlist(ag$finalSelect[[2]])
          ag$onlySampleIDs <- seq(ag$fileList)[preList]
          initializing()
          initInpUpd()
          plotActivator$d <- plotActivator$d + 1
          hierarchyActivator$d <-
            hierarchyActivator$d + 1
          autoplotActivator$d <- autoplotActivator$d + 1
          autohierarchyActivator$d <-
            autohierarchyActivator$d + 1
          matChoices()
          updateSelectInput(
            inputId = "previewMatrix",
            choices = ag$choices,
            selected = ag$appliedMatrix
          )
          updateSelectInput(inputId = "bgType", selected = "")
          updateSelectInput(inputId = "ovTyp", selected = "")
          updateSelectInput(inputId = "autobgType", selected = "")
          updateSelectInput(inputId = "autoovTyp", selected = "")
          updateSelectInput(inputId = "tSEvs", selected = 0)
          disableTabs()
          autodisableTabs()
          hidetSNE()
          disable("editResult")
          disable("exportTable")
          if (reactTestData$d == FALSE) {
            enable("saveFile")
          } else {
            reactTestData$d <- FALSE
          }
          enable("tSNEGenerate")
          ag$betweenPeaks <- NULL
          ag$refPeaks <- NULL
          ag$entiretSNE <- NULL
          ag$tSNEListofGroups <- NULL
          ag$tSNEListofSamples <- NULL
          ag$concat <- NULL
          ag$concatSamples <- NULL
          ag$samples <- NULL
          ag$autosamples <- NULL
          reactTSPar$d <- NULL
          reactTSCh$d <- NULL
          reactGroupNames$d <- NULL
          reacttSIDs$d <- NULL
          reacttSNEPops$d <- NULL
          reactOvSamples$d <- NULL
          reactautoOvSamples$d <- NULL
          tSPlotActiv$d <- tSPlotActiv$d + 1
          if (length(ag$fluoCh) >= 10) {
            reactHeight$d <- 850
          } else if (length(ag$fluoCh) < 10
                     && length(ag$fluoCh) > 4) {
            reactHeight$d <- length(ag$fluoCh) * 85
          } else if (length(ag$fluoCh) <= 4) {
            reactHeight$d <- 350
          }
          alert("Files have been loaded successfully.")
          removeModal()
        }
      )
    } else {
      if (!is.null(ag$filePath)) {
        setwd(ag$filePath)
      }
      alert(displayAlert)
    }
  })
  
  
  ###Load File-----
  observeEvent(input$AutoGateRFileLoad, {
    if (!is.integer(input$AutoGateRFileLoad[1])) {
      reactAutoGateRFile$d <- input$AutoGateRFileLoad
    }
  })
  observeEvent(reactAutoGateRFile$d, {
    tempFilePath <- parseFilePaths(
      c(
        Home = fs::path_home(),
        "R Installation" = R.home(),
        getVolumes()()
      ),
      reactAutoGateRFile$d
    )
    tempStrSplit <- strsplit(tempFilePath[[4]][[1]], "")[[1]]
    end <- length(tempStrSplit)
    start <- end - length(strsplit(tempFilePath[[1]], "")[[1]])
    ag$filePath <- paste(tempStrSplit[-start:-end], collapse = "")
    if (ag$OS == "OSX") {
      tempDest <- paste0(ag$filePath, "/", tempFilePath[[1]])
    } else {
      tempDest <- file.path(ag$filePath,
                            tempFilePath[[1]],
                            fsep = "\\")
    }
    load(tempDest, tempEnv)
    tempList <- sapply(list.files(ag$filePath), function(x)
      strsplit(x, "")[[1]])
    IDs <- vapply(tempList, function(x)
      all(x[(length(x) - 3):length(x)] == c(".", "f", "c", "s")), TRUE)
    fpIDs <- list.files(ag$filePath)[IDs]
    verifier <-
      identical(tempEnv$fileList[order(tempEnv$fileList)], fpIDs[order(fpIDs)])
    if (all(verifier)) {
      ag$loadingFile <- TRUE
      withProgress(
        message = "Please wait...",
        detail = "",
        value = 0,
        max = 100,
        {
          setwd(ag$filePath)
          setOne <- "dataset-1.RData"
          setTwo <- "dataset-2.RData"
          if (reactAutoGateRFile$d[[1]] == setOne
              || reactAutoGateRFile$d[[1]] == setTwo) {
            if (ag$OS == "OSX") {
              tempDest <- paste0(ag$filePath, "/",
                                 reactAutoGateRFile$d)
            } else {
              tempDest <- file.path(ag$filePath,
                                    reactAutoGateRFile$d,
                                    fsep = "\\")
            }
            load(tempDest, ag)
          } else {
            load(paste0(ag$filePath, "/",
                        tempFilePath[[1]]), ag)
          }
          ag$sampleList <- ag$fileList[ag$onlySampleIDs]
          cs <- load_cytoset_from_fcs(ag$fileList)
          ag$uncompGS <- GatingSet(cs)
          ag$gs <- gs_clone(ag$uncompGS)
          compensate(ag$gs,
                     ag$compDFs[ag$appliedMatrix][[1]])
          # ag$auto_gs <- gs_clone(ag$gs)
          # compensate(ag$auto_gs,
          #            ag$compDFs[ag$appliedMatrix][[1]])
          preFluoCh <- colnames(ag$gs)[!grepl("FS",
                                              colnames(ag$gs))]
          preFluoCh <- preFluoCh[!grepl("SS", preFluoCh)]
          ag$fluoCh <- preFluoCh[preFluoCh != "Time"]
          completeFluoCh <- unlist(ag$ch)
          ind <- completeFluoCh != "Time"
          completeFluoCh <- completeFluoCh[ind]
          ag$completeFluoCh <-
            completeFluoCh[!grepl("FS", completeFluoCh)]
          ag$completeFluoCh <-
            ag$completeFluoCh[!grepl("SS", ag$completeFluoCh)]
          preD <- as.data.frame(ag$compDFs[[1]] * 100)
          ag$hTable <- reactiveValues(d = preD)
          for (i in seq(ag$fluoCh)) {
            pre <- ag$customAxis[[i]]
            trans <-
              flowjo_biexp_trans(
                channelRange = 4096,
                maxValue = 262144,
                pos = pre[1],
                neg = pre[2],
                widthBasis = pre[3]
              )
            tList <- transformerList(ag$fluoCh[i], trans)
            ag$gs <- transform(ag$gs, tList)
          }
          ag$auto_gs <- gs_clone(ag$gs)
          for (i in seq(ag$popGates)) {
            gs_pop_add(ag$gs, ag$popGates[[i]],
                       parent = ag$popParents[i])
          }
          recompute(ag$gs)
          for (i in seq(ag$autopopGates)) {
            gs_pop_add(ag$auto_gs, ag$autopopGates[[i]],
                       parent = ag$autopopParents[i])
          }
          recompute(ag$auto_gs)
          reactShowAxis$d <- ag$plotShowAxis
          reactAxisFont$d <- ag$plotAxisFont
          reactShowGateName$d <- ag$plotShowGateName
          reactGateFont$d <- ag$plotGateFont
          reactautoShowAxis$d <- ag$autoplotShowAxis
          reactautoAxisFont$d <- ag$autoplotAxisFont
          reactautoShowGateName$d <- ag$autoplotShowGateName
          reactautoGateFont$d <- ag$autoplotGateFont
          matChoices()
          updateSelectInput(
            inputId = "previewMatrix",
            choices = ag$choices,
            selected = ag$appliedMatrix
          )
          pops <- gs_get_pop_paths(ag$gs, path = 1)
          choices <- c("", ag$sampleList)
          updateSelectInput(
            inputId = "bgPreviewSample",
            choices = choices,
            selected = ag$bgPreviewSample
          )
          autopops <- gs_get_pop_paths(ag$auto_gs, path = 1)
          choices <- c("", ag$sampleList)
          updateSelectInput(
            inputId = "autobgPreviewSample",
            choices = choices,
            selected = ag$autobgPreviewSample
          )
          updateSelectInput(inputId = "bgType",
                            selected = ag$bgType)
          updateSelectInput(
            inputId = "bgPop",
            choices = c("", pops[-1]),
            selected = ag$bgPop
          )
          updateSelectInput(inputId = "autobgType",
                            selected = ag$autobgType)
          updateSelectInput(
            inputId = "autobgPop",
            choices = c("", autopops[-1]),
            selected = ag$autobgPop
          )
          
          showOvPlot$d <- ag$ovBoolean
          updateSelectInput(inputId = "ovTyp",
                            selected = ag$ovTyp)
          updateSelectInput(inputId = "ovTon",
                            selected = ag$ovTon)
          
          autoshowOvPlot$d <- ag$autoovBoolean
          updateSelectInput(inputId = "autoovTyp",
                            selected = ag$autoovTyp)
          updateSelectInput(inputId = "autoovTon",
                            selected = ag$autoovTon)
          updateSelectInput(
            inputId = "ovY",
            choices = c(ag$ch),
            selected = ag$ovY
          )
          updateSelectInput(
            inputId = "ovX",
            choices = c(ag$ch),
            selected = ag$ovX
          )
          updateSelectInput(
            inputId = "ovP",
            choices = pops,
            selected = ag$ovP
          )
          updateSelectInput(
            inputId = "autoovY",
            choices = c(ag$ch),
            selected = ag$autoovY
          )
          updateSelectInput(
            inputId = "autoovX",
            choices = c(ag$ch),
            selected = ag$autoovX
          )
          updateSelectInput(
            inputId = "autoovP",
            choices = autopops,
            selected = ag$autoovP
          )
          reactOvSamples$d <- ag$ovSamples
          reactOvShowAxis$d <- ag$ovShowAxis
          reactOvAxisFont$d <- ag$ovAxisFont
          prolifReady$d <- ag$prolifBool
          updateSelectInput(
            inputId = "prolifSButt",
            choices = ag$sampleList,
            selected = ag$prolifSButt
          )
          reactautoOvSamples$d <- ag$autoovSamples
          reactautoOvShowAxis$d <- ag$autoovShowAxis
          reactautoOvAxisFont$d <- ag$autoovAxisFont
          # updateSelectInput(
          #   inputId = "autoprolifSButt",
          #   choices = ag$sampleList,
          #   selected = ag$autoprolifSButt
          # )
          if (ag$prolifBool == TRUE) {
            updateCheckboxInput(inputId = "prolifLabel",
                                value = ag$prolifLabel)
            updateCheckboxInput(inputId = "prolifGrid",
                                value = ag$prolifGrid)
          }
          reactTSPar$d <- ag$tSPar
          reactTSCh$d <- ag$tSNEParameters
          if (is.null(ag$tSEvs)) {
            ag$tSEvs <- 0
          }
          updateSelectInput(inputId = "tSEvs",
                            selected = ag$tSEvs)
          reactGroupNames$d <- ag$tSNEGroupNames
          if (length(ag$tSNEListofGroups) < 2
              && length(ag$tSNEListofSamples) < 2) {
            choices <- c("Heatmap", "Overlay Populations")
            updateSelectInput(
              inputId = "tSNEMode",
              choices = choices,
              selected = ag$tSNEMode
            )
          } else {
            choices <- c("Heatmap",
                         "Overlay Groups or Samples",
                         "Overlay Populations")
            updateSelectInput(
              inputId = "tSNEMode",
              choices = choices,
              selected = ag$tSNEMode
            )
          }
          updateSelectInput(inputId = "tSHighl",
                            selected = ag$tSHighl)
          updateSelectInput(inputId = "tSGroupOrSamp",
                            selected = ag$tSGroupOrSamp)
          updateSelectInput(inputId = "tSGroupOrSampID",
                            selected = ag$tSGroupOrSampID)
          reacttSIDs$d <- ag$tSGroupOrSampIDs
          reacttSNEPops$d <- ag$tSNEPops
          updateSliderInput(inputId = "tSNEDotSize",
                            value = ag$tSNEDotSize)
          if (!is.null(ag$entiretSNE)) {
            showtSNE()
            ind <- unlist(ag$ch) != "Time"
            tempChannels <- unlist(ag$ch)[ind]
            ag$availableparameters <- list()
            for (i in seq(ag$tSNEParameters)) {
              preId <- ag$tSNEParameters[i]
              index <- which(tempChannels == preId)
              ag$availableparameters[[i]] <- index
            }
            indices <- unlist(ag$availableparameters)
            preParam <- tempChannels[indices]
            ag$availableparameters <- preParam
            choices <- c(ag$availableparameters)
            updateSelectInput(inputId = "tSHighl",
                              choices = choices)
            tSPlotActiv$d <- tSPlotActiv$d + 1
          } else {
            hidetSNE()
            enable("tSNEGenerate")
          }
        }
      )
      plotActivator$d <- plotActivator$d + 1
      autoplotActivator$d <- autoplotActivator$d + 1
      ag$X <- colnames(ag$gs)[grep("FS", colnames(ag$gs))[1]]
      ag$Y <- colnames(ag$gs)[grep("SS", colnames(ag$gs))[1]]
      ag$autoX <-
        colnames(ag$auto_gs)[grep("FS", colnames(ag$auto_gs))[1]]
      ag$autoY <-
        colnames(ag$auto_gs)[grep("SS", colnames(ag$auto_gs))[1]]
      updateSelectInput(
        inputId = "samp",
        choices = c("", ag$sampleList),
        selected = ag$sampleList[1]
      )
      updateSelectInput(
        inputId = "autosamp",
        choices = c("", ag$sampleList),
        selected = ag$sampleList[1]
      )
      updateSelectInput(inputId = "typ", selected = "Pseudocolor")
      updateSelectInput(
        inputId = "previewSample",
        choices = c("", ag$fileList),
        selected = ag$sampleList[1]
      )
      updateSelectInput(inputId = "autotyp", selected = "Pseudocolor")
      updateSelectInput(
        inputId = "autopreviewSample",
        choices = c("", ag$fileList),
        selected = ag$sampleList[1]
      )
      updateSelectInput(
        inputId = "Y",
        choices = c(ag$ch),
        selected = ag$Y
      )
      updateSelectInput(
        inputId = "X",
        choices = c(ag$ch),
        selected = ag$X
      )
      updateSelectInput(
        inputId = "autoY",
        choices = c(ag$ch),
        selected = ag$autoY
      )
      updateSelectInput(
        inputId = "autoX",
        choices = c(ag$ch),
        selected = ag$autoX
      )
      updateSelectInput(inputId = "rsCh",
                        choices = c("", ag$completeFluoCh))
      updateSelectInput(inputId = "autorsCh",
                        choices = c("", ag$completeFluoCh))
      js$enableTab("plotTab")
      js$enableTab("autoPlotTab")
      js$enableTab("compTab")
      disableTabs()
      autodisableTabs()
      removeModal()
      alert("File has been loaded successfully.")
      if (reactTestData$d == FALSE) {
        enable("saveFile")
      } else {
        reactTestData$d <- FALSE
      }
      reactPar$d <- "root"
      reactautoPar$d <- "root"
      hierarchyActivator$d <- hierarchyActivator$d + 1
      autohierarchyActivator$d <- autohierarchyActivator$d + 1
      
      reactAutoGateRFile$d <- NULL
      if (length(ag$fluoCh) >= 10) {
        reactHeight$d <- 850
      } else if (length(ag$fluoCh) < 10 && length(ag$fluoCh) > 4) {
        reactHeight$d <- length(ag$fluoCh) * 85
      } else if (length(ag$fluoCh) <= 4) {
        reactHeight$d <- 350
      }
    } else {
      alert(
        "Loading failed... Please certify this save file and all
                  FCS files connected to it are in the folder.
                  Extra FCS files added to the folder might interfere
                  in this verification."
      )
    }
  })
  ##save file-----
  observeEvent(input$saveFile, {
    if (file.exists(paste0(getwd(), "/AutoGateRSave.RData"))) {
      showModal(modalDialog(
        paste(
          "This will overwrite the existing AutoGateR file in",
          getwd(),
          ". Do you want to proceed?"
        ),
        footer = list(
          actionButton(
            inputId = "overwrite",
            label = "Save",
            style = ag$blackB
          ),
          actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = FALSE,
        size = "m"
      ))
    } else {
      if (is.null(reactOverw$d)) {
        reactOverw$d <- 0
      } else {
        reactOverw$d <- reactOverw$d + 1
      }
    }
  })
  ##save file overwrite-----
  observeEvent(input$overwrite, {
    if (is.null(reactOverw$d)) {
      reactOverw$d <- 0
    } else {
      reactOverw$d <- reactOverw$d + 1
    }
    removeModal()
  })
  
  observeEvent(reactOverw$d, {
    setwd(ag$filePath)
    popPaths <- gs_get_pop_paths(ag$gs)
    autopopPaths <- gs_get_pop_paths(ag$auto_gs)
    if (length(popPaths) == 1) {
      popParents <- NULL
      popGates <- NULL
    } else {
      popParents <- gs_pop_get_count_fast(ag$gs[1], "freq")[, 3][[1]]
      popGates <- vapply(popPaths[-1], function(x)
        list(gs_pop_get_gate(ag$gs, x)[1][[1]]),
        list(length(popPaths[-1])))
    }
    if (length(autopopPaths) == 1) {
      autopopParents <- NULL
      autopopGates <- NULL
    } else {
      autopopParents <- gs_pop_get_count_fast(ag$auto_gs[1], "freq")[, 3][[1]]
      autopopGates <- vapply(autopopPaths[-1], function(x)
        list(gs_pop_get_gate(ag$auto_gs, x)[1][[1]]),
        list(length(autopopPaths[-1])))
    }
    plotShowAxis <- reactShowAxis$d
    plotAxisFont <- reactAxisFont$d
    plotShowGateName <- reactShowGateName$d
    plotGateFont <- reactGateFont$d
    bgPreviewSample <- input$bgPreviewSample
    bgType <- input$bgType
    bgPop <- input$bgPop
    ovBoolean <- showOvPlot$d
    ovTyp <- input$ovTyp
    ovTon <- input$ovTon
    ovY <- input$ovY
    ovX <- input$ovX
    ovP <- input$ovP
    autoplotShowAxis <- reactautoShowAxis$d
    autoplotAxisFont <- reactautoAxisFont$d
    autoplotShowGateName <- reactautoShowGateName$d
    autoplotGateFont <- reactautoGateFont$d
    autobgPreviewSample <- input$autobgPreviewSample
    autobgType <- input$autobgType
    autobgPop <- input$autobgPop
    autoovBoolean <- autoshowOvPlot$d
    autoovTyp <- input$autoovTyp
    autoovTon <- input$autoovTon
    autoovY <- input$autoovY
    autoovX <- input$autoovX
    autoovP <- input$autoovP
    ovSamples <- reactOvSamples$d
    ovShowAxis <- reactOvShowAxis$d
    ovAxisFont <- reactOvAxisFont$d
    prolifBool <- prolifReady$d
    prolifSButt <- input$prolifSButt
    prolifLabel <- input$prolifLabel
    prolifGrid <- input$prolifGrid
    autoovSamples <- reactautoOvSamples$d
    autoovShowAxis <- reactautoOvShowAxis$d
    autoovAxisFont <- reactautoOvAxisFont$d
    # autoprolifBool <- autoprolifReady$d
    # autoprolifSButt <- input$autoprolifSButt
    # autoprolifLabel <- input$autoprolifLabel
    # autoprolifGrid <- input$autoprolifGrid
    tSPar <- reactTSPar$d
    tSNEParameters <- reactTSCh$d
    tSEvs <- input$tSEvs
    tSNEGroupNames <- reactGroupNames$d
    tSNEMode <- input$tSNEMode
    tSHighl <- input$tSHighl
    tSGroupOrSamp <- input$tSGroupOrSamp
    tSGroupOrSampID <- input$tSGroupOrSampID
    tSGroupOrSampIDs <- reacttSIDs$d
    tSNEPops <- reacttSNEPops$d
    tSNEDotSize <- input$tSNEDotSize
    fileList <- ag$fileList
    compControlIDs <- ag$compControlIDs
    onlySampleIDs <- ag$onlySampleIDs
    ch <- ag$ch
    compDFs <- ag$compDFs
    appliedMatrix <- ag$appliedMatrix
    prolifChannel <- ag$prolifChannel
    prolifParent <- ag$prolifParent
    # autoprolifChannel <- ag$autoprolifChannel
    # autoprolifParent <- ag$autoprolifParent
    betweenPeaks <- ag$betweenPeaks
    refPeaks <- ag$refPeaks
    adjustedL <- ag$adjustedL
    concat <- ag$concat
    entiretSNE <- ag$entiretSNE
    concatSamples <- ag$concatSamples
    tSNEListofGroups <- ag$tSNEListofGroups
    tSNEListofSamples <- ag$tSNEListofSamples
    results <- ag$results
    autoresults <- ag$autoresults
    customAxis <- ag$customAxis
    # customAxis <- ag$customAxis
    save(
      fileList,
      compControlIDs,
      onlySampleIDs,
      #Plot and Autoplot
      customAxis,
      popParents,
      popGates,
      plotShowAxis,
      plotAxisFont,
      plotShowGateName,
      plotGateFont,
      ch,
      # customAxis,
      autopopParents,
      autopopGates,
      autoplotShowAxis,
      autoplotAxisFont,
      autoplotShowGateName,
      autoplotGateFont,
      #Compensation
      compDFs,
      appliedMatrix,
      #Ancestry and Autoancestry
      bgPreviewSample,
      bgType,
      bgPop,
      autobgPreviewSample,
      autobgType,
      autobgPop,
      #Overlays and Auto Overlays
      ovBoolean,
      ovTyp,
      ovTon,
      ovY,
      ovX,
      ovP,
      ovSamples,
      ovShowAxis,
      ovAxisFont,
      autoovBoolean,
      autoovTyp,
      autoovTon,
      autoovY,
      autoovX,
      autoovP,
      autoovSamples,
      autoovShowAxis,
      autoovAxisFont,
      #Proliferation
      prolifChannel,
      prolifParent,
      prolifBool,
      betweenPeaks,
      refPeaks,
      adjustedL,
      prolifSButt,
      prolifLabel,
      prolifGrid,
      #t-SNE
      concat,
      entiretSNE,
      concatSamples,
      tSPar,
      tSNEParameters,
      tSEvs,
      tSNEGroupNames,
      tSNEMode,
      tSHighl,
      tSGroupOrSamp,
      tSGroupOrSampID,
      tSGroupOrSampIDs,
      tSNEPops,
      tSNEDotSize,
      tSNEListofGroups,
      tSNEListofSamples,
      #Results
      results,
      autoresults,
      
      file = paste0(getwd(), "/AutoGateRSave.RData")
    )
    alert(
      paste(
        "A AutoGateR file has been saved in the same folder of the
                FCS files:",
        ag$filePath
      )
    )
  })
  ##file help-----
  observeEvent(input$fileHelp, {
    showModal(
      modalDialog(
        title = "Help",
        strong("Note:"),
        "Saving the data will create a AutoGateRSave.RData file inside the
            FCS files folder.",
        br(),
        br(),
        strong("Important:"),
        "AutoGateR files are required to be inside the loaded FCS files
            folder before being uploaded.",
        footer = NULL,
        size = "m",
        easyClose = TRUE
      )
    )
  })
  
  #Compensation----
  ##left----
  observeEvent(input$createMatrix, {
    showModal(
      modalDialog(
        "Select the method.",
        br(),
        br(),
        if (is.null(ag$compDFs["AutoSpill"][[1]])) {
          actionButton(inputId = "createAutoSpill",
                       label = "AutoSpill*",
                       style = ag$blackB)
        },
        actionButton(
          inputId = "createmanually",
          label = "Manually",
          style = ag$blackB
        ),
        br(),
        br(),
        if (is.null(ag$compDFs["AutoSpill"][[1]])) {
          "*Automated compensation calculation and matrix generation.
                This might take a few minutes"
        },
        footer = actionButton(
          inputId = "cancelModal",
          label = "Cancel",
          style = ag$whiteB
        ),
        easyClose = FALSE,
        size = "m"
      )
    )
  })
  
  observeEvent(input$createAutoSpill, {
    if (!is.null(ag$compControlIDs)) {
      onlyControls = ag$fileList[ag$compControlIDs]
      verifier = vapply(onlyControls, function(x)
        length(grep(x, list.files(ag$filePath))) > 0, TRUE)
      verifier = all(verifier)
      if (length(ag$compControlIDs) >= length(ag$fluoCh) &&
          verifier) {
        hide("createmanually")
        hide("cancelModal")
        disable("createAutoSpill")
        message = "Please wait..."
        withProgress(
          message = message,
          detail = "",
          value = 0,
          max = 100,
          {
            controlDir = getwd()
            controlDefFile = paste0(getwd(), "/fcs_control.csv")
            unst = c(grep("Unstained", ag$fileList))
            onlyControls = onlyControls[-unst]
            cs = load_cytoset_from_fcs(ag$fileList)[onlyControls]
            ffs = flowSet_to_list(cytoset_to_flowSet(cs))
            tubeName = vapply(ffs, function(x)
              x@description$`TUBE NAME`, character(1))
            if (is.null(unlist(tubeName))) {
              tubeName = vapply(ffs, function(x)
                x@description$`TUBENAME`, character(1))
            }
            if (is.null(unlist(tubeName))) {
              tubeName = vapply(ffs, function(x)
                x@description$`$WELLID`, character(1))
            }
            if (is.null(unlist(tubeName))) {
              tubeName = vapply(ffs, function(x)
                x@description$TBNM, character(1))
            }
            if (is.null(unlist(tubeName))) {
              tubeName <- substr(ag$fileList, 1,
                                 nchar(ag$fileList) - 4)
            }
            ag$dye <- tryCatch({
              orderGuess1 = vapply(tubeName, function(x)
                paste0(strsplit(x, " Stained Control")[[1]], "-A"),
                character(1))
              
              
              orderGuess2 <- vapply(orderGuess1, function(x)
                strsplit(x, " ")[[1]], character(2))
              
              
              orderGuess = orderGuess2[2,]
              
              
              
            },
            error = function(x) {
              orderGuess4 = vapply(tubeName, function(x)
                paste0(strsplit(x, " Stained Control")[[1]], "-A"),
                character(1))
              
              orderGuess = orderGuess4
              
            })
            
            orderGuess <- ag$dye
            
            ag$antig <- tryCatch({
              orderGuess1 = vapply(tubeName, function(x)
                paste0(strsplit(x, " Stained Control")[[1]], "-A"),
                character(1))
              
              
              orderGuess2 <- vapply(orderGuess1, function(x)
                strsplit(x, " ")[[1]], character(2))
              
              
              orderGuess = orderGuess2[1,]
              
              
              
            },
            error = function(x) {
              rep(NA, length(orderGuess))
              
            })
            
            
            verifier = lapply(orderGuess, function(x)
              any(x == ag$fluoCh))
            verifier = all(unlist(verifier))
            if (verifier == FALSE) {
              orderGuess = ag$fluoCh[order(ag$fluoCh)]
            }
            
            ##^^^
            ###WHY DOES THIS NOT WORK?-----
            orderGuess <- ag$dye
            
            naVal = rep(NA, length(orderGuess))
            futureCsv = data.frame(
              "filename" = onlyControls,
              "dye" = orderGuess,
              "antigen" = ag$antig,
              "wavelength" = naVal
            )
            write.table(
              futureCsv,
              file = controlDefFile,
              sep = ",",
              row.names = FALSE
            )
            asp = get.autospill.param()
            flowControl = suppressWarnings(read.flow.control(controlDir, controlDefFile, asp))
            doGate <- getFromNamespace("do.gate", "autospill")
            flowGate <- lapply(flowControl$sample, function(samp)
              doGate(
                flowControl$expr.data.untr[flowControl$event.sample == samp,
                                           flowControl$scatter.parameter],
                flowControl$gate.parameter[[flowControl$marker.original[match(samp, flowControl$marker)]]],
                samp,
                flowControl,
                asp
              ))
            names(flowGate) <- flowControl$sample
            #global functions
            spill = autoGetMarker(TRUE, flowGate, flowControl, asp)
            refined = autoRefineSpill(spill, NULL, flowGate, flowControl, asp)
            
          }
        )
        rightOrder = vapply(flowControl$marker.original, function(x)
          which(x == ag$fluoCh), integer(1))
        reordered = cbind2(refined[[1]], rightOrder)
        reordered = reordered[order(reordered[, ncol(reordered)]),]
        reordered = reordered[,-ncol(reordered)]
        reordered = rbind2(reordered, rightOrder)
        reordered = reordered[, order(reordered[nrow(reordered),])]
        reordered = reordered[-nrow(reordered),]
        colnames(reordered) = rownames(reordered) = ag$fluoCh
        ag$compDFs["AutoSpill"][[1]] = reordered
        removeModal()
        choices = names(ag$compDFs)
        names(choices) = choices
        allNames = c()
        for (i in seq(choices)) {
          if (choices[i] == ag$appliedMatrix) {
            allNames[i] = paste(ag$appliedMatrix, "(applied)")
          } else {
            allNames[i] = choices[i]
          }
        }
        names(choices) = allNames
        updateSelectInput(inputId = "previewMatrix",
                          choices = choices,
                          selected = "AutoSpill")
      } else {
        alert("AutoSpill requires all compensation control
                      files to be loaded.")
      }
    } else {
      alert("AutoSpill requires all compensation control
                  files to be loaded.")
    }
  })
  
  observeEvent(input$createmanually, {
    js$disableTab("fileTab")
    js$disableTab("plotTab")
    js$disableTab("autoPlotTab")
    disableTabs()
    autodisableTabs()
    disable("createMatrix")
    disable("applyMatrix")
    shinyjs::show("saveMatrix")
    shinyjs::show("cancelMatrix")
    reactReadOnly$d <- FALSE
    removeModal()
  })
  
  observeEvent(input$cancelMatrix, {
    js$enableTab("fileTab")
    js$enableTab("plotTab")
    js$enableTab("autoPlotTab")
    if (length(gs_get_pop_paths(ag$gs)) > 1) {
      js$enableTab("ancestryTab")
      js$enableTab("autoancestryTab")
      js$enableTab("overlayTab")
      js$enableTab("autooverlayTab")
      js$enableTab("prolifTab")
      js$enableTab("tsneTab")
      js$enableTab("resultTab")
      js$enableTab("autoresultTab")
    }
    hide("saveMatrix")
    hide("cancelMatrix")
    enable("createMatrix")
    reactReadOnly$d <- TRUE
    compPlotActivator$d <- compPlotActivator$d + 1
  })
  
  observeEvent(input$saveMatrix, {
    js$enableTab("fileTab")
    js$enableTab("plotTab")
    js$enableTab("autoPlotTab")
    if (length(gs_get_pop_paths(ag$gs)) > 1) {
      js$enableTab("ancestryTab")
      js$enableTab("autoancestryTab")
      js$enableTab("overlayTab")
      js$enableTab("prolifTab")
      js$enableTab("tsneTab")
      js$enableTab("resultTab")
    }
    hide("saveMatrix")
    hide("cancelMatrix")
    enable("createMatrix")
    reactReadOnly$d <- TRUE
    index <- length(grep("Manual Matrix", names(ag$compDFs))) + 1
    matName <- paste("Manual Matrix", index)
    ag$compDFs[matName][[1]] <- ag$currentTable / 100
    matChoices()
    updateSelectInput(
      inputId = "previewMatrix",
      choices = ag$choices,
      selected = names(ag$compDFs)[length(ag$compDFs)]
    )
  })
  
  observeEvent(input$applyMatrix, {
    showModal(modalDialog(
      paste0(
        "The matrix '",
        input$previewMatrix,
        "' will be applied to all samples.
                   Do you want to proceed?"
      ),
      footer = list(
        actionButton(
          inputId = "OKapplymatrix",
          label = "Ok",
          style = ag$blackB
        ),
        actionButton(
          inputId = "cancelModal",
          label = "Cancel",
          style = ag$whiteB
        )
      ),
      easyClose = TRUE,
      size = "s"
    ))
  })
  
  observeEvent(input$OKapplymatrix, {
    plotActivator$d <- plotActivator$d + 1
    autoplotActivator$d <- autoplotActivator$d + 1
    ag$appliedMatrix <- input$previewMatrix
    popPaths <- gs_get_pop_paths(ag$gs)
    autopopPaths <- gs_get_pop_paths(ag$auto_gs)
    reAddPops()
    autoreAddPops()
    ag$gs <- gs_clone(tempEnv$gs)
    ag$auto_gs <- gs_clone(tempEnv$auto_gs)
    names(ag$customAxis) <- ag$fluoCh
    for (i in seq(ag$fluoCh)) {
      trans <- flowjo_biexp_trans(
        channelRange = 4096,
        maxValue = 262144,
        pos = ag$customAxis[[i]][1],
        neg = ag$customAxis[[i]][2],
        widthBasis = ag$customAxis[[i]][3]
      )
      ag$gs <-
        transform(ag$gs, transformerList(ag$fluoCh[i], trans))
    }
    for (i in seq(ag$fluoCh)) {
      autotrans <- flowjo_biexp_trans(
        channelRange = 4096,
        maxValue = 262144,
        pos = ag$customAxis[[i]][1],
        neg = ag$customAxis[[i]][2],
        widthBasis = ag$customAxis[[i]][3]
      )
      ag$auto_gs <-
        transform(ag$auto_gs, transformerList(ag$fluoCh[i], autotrans))
    }
    recompute(ag$gs)
    recompute(ag$auto_gs)
    matChoices()
    updateSelectInput(
      inputId = "previewMatrix",
      choices = ag$choices,
      selected = ag$appliedMatrix
    )
    removeModal()
    plotActivator$d
    autoplotActivator$d
  })
  
  observeEvent(c(input$previewMatrix, input$cancelMatrix), {
    if (ag$appliedMatrix == input$previewMatrix) {
      disable("applyMatrix")
    } else {
      enable("applyMatrix")
    }
  })
  
  ##main----
  observeEvent(input$comp, {
    handsonobject <- isolate(input$comp)
    invertedtable <- unlist(handsonobject$d)
    table <- vector(mode = "list", length = length(ag$fluoCh))
    index <- 0
    for (i in seq(ag$fluoCh)) {
      for (j in seq(ag$fluoCh)) {
        index <- index + 1
        table[[j]][i] <- invertedtable[[index]]
      }
    }
    table <- as.data.frame(table, row.names = ag$fluoCh)
    colnames(table) <- rownames(table)
    if (!is.null(ag$currentTable)
        && !identical(ag$currentTable, table)
        && reactReadOnly$d == FALSE) {
      compPlotActivator$d <- compPlotActivator$d + 1
    }
    ag$currentTable <- table
  })
  
  observe({
    output$compGen <- renderPlot({
      input$previewMatrix
      compPlotActivator$d
      ID <- match(input$previewSample, ag$fileList)
      if (isolate(reactReadOnly$d) == TRUE) {
        withProgress(
          message = "Please wait...",
          detail = "",
          value = 0,
          max = 100,
          {
            compGen(ID,
                    ag$compDFs[input$previewMatrix][[1]],
                    input$showUncomp)
          }
        )
      } else {
        withProgress(
          message = "Please wait...",
          detail = "",
          value = 0,
          max = 100,
          {
            compGen(ID,
                    ag$currentTable / 100,
                    input$showUncomp)
          }
        )
      }
    }, height = reactHeight$d, width = reactHeight$d)
  })
  
  
  #Plot----
  ##left----
  observeEvent(input$customizeAxisY, {
    reactAxisCustom$d <- input$Y
  })
  observeEvent(input$customizeAxisX, {
    reactAxisCustom$d <- input$X
  })
  
  observeEvent(reactAxisCustom$d, {
    ag$val <- ag$customAxis[[which(ag$fluoCh == reactAxisCustom$d)]]
    showModal(
      modalDialog(
        tags$style(HTML(
          paste0(
            "[for=posslider]+span>.irs>.irs-single,
                                   [for=posslider]",
            ag$sliderCol
          )
        )),
        tags$style(HTML(
          paste0(
            "[for=negslider]+span>.irs>.irs-single,
                                   [for=negslider]",
            ag$sliderCol
          )
        )),
        tags$style(HTML(
          paste0(
            "[for=widthslider]+span>.irs>.irs-single,
                                   [for=widthslider]",
            ag$sliderCol
          )
        )),
        fluidRow(
          column(
            width = 9,
            align = "right",
            plotOutput("axisplot", height = 440)
          ),
          column(
            width = 3,
            align = "left",
            style = "margin-top: 350px;",
            actionButton(
              inputId = "resetbutton",
              label = "Reset",
              style = ag$whiteB
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            align = "center",
            sliderInput(
              "posslider",
              label = "Positive Decades",
              min = 2,
              max = 7,
              step = 0.02,
              value = ag$val[1],
              ticks = FALSE
            ),
            sliderInput(
              "negslider",
              label = "Negative Decades",
              min = 0,
              max = 1,
              step = 0.1,
              value = ag$val[2],
              ticks = FALSE
            ),
            sliderInput(
              "widthslider",
              label = "Width Basis",
              min = 0,
              max = 3,
              step = 0.1,
              value = log10(abs(ag$val[3])),
              ticks = FALSE
            )
          )
        ),
        footer = list(
          actionButton(
            inputId = "saveaxis",
            label = "Ok",
            style = ag$blackB
          ),
          actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = FALSE,
        size = "l"
      )
    )
  })
  
  observeEvent(input$resetbutton, {
    updateSliderInput(inputId = "posslider", value = 4.42)
    updateSliderInput(inputId = "negslider", value = 0)
    updateSliderInput(inputId = "widthslider", value = 2)
  })
  
  output$axisplot <- renderPlot({
    par(mar = c(4, 6, 1, 1) + 0.1, lwd = 2)
    inverseT <- flowjo_biexp(
      channelRange = 4096,
      maxValue = 262144,
      pos = ag$val[1],
      neg = ag$val[2],
      widthBasis = ag$val[3],
      inverse = TRUE
    )
    ag$widthBasis <- -10 ^ input$widthslider
    if (input$widthslider < 1) {
      ag$widthBasis <- format(round(ag$widthBasis, 2), nsmall = 2)
    } else if (input$widthslider < 2) {
      ag$widthBasis <- format(round(ag$widthBasis, 1), nsmall = 1)
    } else if (input$widthslider <= 3) {
      ag$widthBasis <- round(ag$widthBasis)
    }
    trans <- flowjo_biexp(
      channelRange = 4096,
      maxValue = 262144,
      pos = input$posslider,
      neg = input$negslider,
      widthBasis = as.numeric(ag$widthBasis)
    )
    unTransDF <- inverseT(ag$dF[, reactAxisCustom$d])
    tempDF <- trans(unTransDF)
    xLim <- c(0, 4100)
    if (length(tempDF) > 1) {
      referenceHist <- hist(tempDF, breaks = 20, plot = FALSE)
      refCounts <- referenceHist$counts
      refDensity <- referenceHist$density
      multiplier <- refCounts / refDensity
      densLine <- density(tempDF)
      densLine$y <- densLine$y * multiplier[1]
      maxDens <- max(densLine[2]$y)
      plot(
        densLine,
        xaxt = "n",
        yaxt = "n",
        ann = FALSE,
        xlim = xLim,
        ylim = c(0, maxDens * 1.05),
        xaxs = "i",
        yaxs = "i"
      )
      polygon(densLine,
              col = "grey",
              border = "black",
              lwd = 1)
      axisTicks(2, "histo")
    } else {
      plot(
        0,
        xaxt = "n",
        yaxt = "n",
        ann = FALSE,
        xaxs = "i",
        yaxs = "i",
        cex = 0
      )
    }
    title(
      ylab = "Count",
      cex.lab = 1 + 15 / 10,
      line = 3,
      font.lab = 2
    )
    index <- which(colnames(ag$gs) == reactAxisCustom$d)
    title(
      xlab = names(ag$ch)[index],
      cex.lab = 1 + 15 / 10,
      line = 3,
      font.lab = 2
    )
    customAx <- c(input$posslider,
                  input$negslider,
                  as.numeric(ag$widthBasis))
    axisTicks(1, "log", customAx)
  }, height = 440, width = 470)
  
  observeEvent(input$saveaxis, {
    currentA <- ag$customAxis[[which(ag$fluoCh == reactAxisCustom$d)]]
    newA <- c(input$posslider,
              input$negslider,
              as.numeric(ag$widthBasis))
    if (!identical(currentA, newA)) {
      inverseT <- flowjo_biexp(
        channelRange = 4096,
        maxValue = 262144,
        pos = currentA[1],
        neg = currentA[2],
        widthBasis = currentA[3],
        inverse = TRUE
      )
      trans <- flowjo_biexp(
        channelRange = 4096,
        maxValue = 262144,
        pos = input$posslider,
        neg = input$negslider,
        widthBasis = as.numeric(ag$widthBasis)
      )
      ag$customAxis[[which(ag$fluoCh == reactAxisCustom$d)]] <- newA
      popPaths <- gs_get_pop_paths(ag$gs)
      reAddPops()
      ag$gs <- gs_clone(tempEnv$gs)
      names(ag$customAxis) <- ag$fluoCh
      for (i in ag$fluoCh[ag$fluoCh != reactAxisCustom$d]) {
        transSub <- flowjo_biexp_trans(
          channelRange = 4096,
          maxValue = 262144,
          pos = ag$customAxis[[i]][1],
          neg = ag$customAxis[[i]][2],
          widthBasis = ag$customAxis[[i]][3]
        )
        ag$gs <- transform(ag$gs, transformerList(i, transSub))
      }
      transSub <-
        flowjo_biexp_trans(
          channelRange = 4096,
          maxValue = 262144,
          pos = input$posslider,
          neg = input$negslider,
          widthBasis = as.numeric(ag$widthBasis)
        )
      ag$gs <- transform(ag$gs, transformerList(reactAxisCustom$d,
                                                transSub))
      recompute(ag$gs)
      for (i in seq(popPaths)[-1]) {
        gate <- gs_pop_get_gate(ag$gs, popPaths[i])[1][[1]]
        gateCh <- names(gate@parameters)
        for (j in seq(gateCh)) {
          if (names(gate@parameters)[j] == reactAxisCustom$d) {
            if (is(gate, "polygonGate")) {
              toInvertObj <- gate@boundaries[, reactAxisCustom$d]
              obj <- inverseT(toInvertObj)
              gate@boundaries[, reactAxisCustom$d] <- trans(obj)
            } else {
              if (gate@min[[j]] != "-Inf") {
                gate@min[[j]] <- trans(inverseT(gate@min[[j]]))
              } else {
                gate@min[[j]] <- -Inf
              }
              if (gate@max[[j]] != "Inf") {
                gate@max[[j]] <- trans(inverseT(gate@max[[j]]))
              } else {
                gate@max[[j]] <- Inf
              }
            }
            popgateList <- vapply(ag$fileList, function(x)
              list(gate), list(length(ag$fileList)))
            gs_pop_set_gate(ag$gs, gate@filterId, popgateList)
            recompute(ag$gs, gate@filterId)
          }
        }
      }
      reactAxisCustom$d <- NULL
      plotActivator$d <- plotActivator$d + 1
    } else {
      reactAxisCustom$d <- NULL
    }
    compPlotActivator$d <- compPlotActivator$d + 1
    hierarchyActivator$d <- hierarchyActivator$d + 1
    removeModal()
  })
  
  observeEvent(input$displayOptMain, {
    showModal(
      modalDialog(
        tags$style(HTML(
          paste0(
            "[for=displayAxisFont]
            +span>.irs>.irs-single, [for=displayAxisFont]",
            ag$sliderCol
          )
        )),
        checkboxInput("displayAxis", label = "Show axis titles",
                      value = reactShowAxis$d),
        sliderInput(
          "displayAxisFont",
          label = NULL,
          min = 10,
          max = 25,
          value = reactAxisFont$d,
          ticks = FALSE
        ),
        br(),
        tags$style(HTML(
          paste0(
            "[for=displayGateFont]
            +span>.irs>.irs-single, [for=displayGateFont]",
            ag$sliderCol
          )
        )),
        checkboxInput("displayGateName", label = "Show gate name",
                      value = reactShowGateName$d),
        sliderInput(
          "displayGateFont",
          label = NULL,
          min = 1,
          max = 30,
          value = reactGateFont$d,
          ticks = FALSE
        ),
        footer = list(
          actionButton(
            inputId = "reloadplotmodal",
            label = "Ok",
            style = ag$blackB
          ),
          actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = TRUE,
        size = "s"
      )
    )
    if (length(gs_get_pop_paths(ag$gs)) > 1) {
      shinyjs::show("displayGateName")
      shinyjs::show("displayGateFont")
    } else {
      hide("displayGateName")
      hide("displayGateFont")
    }
  })
  
  observeEvent(input$reloadplotmodal, {
    plotActivator$d <- plotActivator$d + 1
    removeModal()
    reactShowAxis$d <- input$displayAxis
    reactAxisFont$d <- input$displayAxisFont
    reactShowGateName$d <- input$displayGateName
    reactGateFont$d <- input$displayGateFont
  })
  
  observeEvent(c(input$displayAxis, input$displayOptMain) , {
    if (length(input$displayAxis) != 0) {
      if (input$displayAxis == TRUE) {
        shinyjs::show("displayAxisFont")
      } else {
        hide("displayAxisFont")
      }
    }
  })
  
  observeEvent(input$plotHelp, {
    showModal(
      modalDialog(
        title = "Help",
        "Use the buttons at the top right side of the plot to
            interactively draw gates.",
        br(),
        br(),
        "Double clicking the population inside the gate leads to
            a new plot with the sorted population.",
        footer = NULL,
        size = "m",
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$about, {
    showModal(
      modalDialog(
        title = "AutoGateR",
        "AutoGateR is an open-source project that provides an user-friendly,
            high-performance UI for automatically compensating and gating flow cytometry data in R.",
        br(),
        br(),
        "Built by Eric Brooks",
        size = "m",
        easyClose = TRUE
      )
    )
  })
  
  ##main----
  observeEvent(input$typ, {
    if (input$typ != "Histogram") {
      if (input$Y == "") {
        updateSelectInput(inputId = "Y", selected = ag$Y)
      } else {
        plotActivator$d <- plotActivator$d + 1
      }
      enable("Y")
      disable("interval")
      enable("rectang")
      enable("polygon")
      enable("quad")
    } else {
      updateSelectInput(inputId = "Y", selected = "")
      disable("Y")
      enable("interval")
      disable("rectang")
      disable("polygon")
      disable("quad")
    }
  })
  
  observeEvent(input$nextSample, {
    current <- match(input$samp, ag$sampleList)
    if (current + 1 <= length(ag$sampleList)) {
      select <- ag$sampleList[[current + 1]]
      updateSelectInput(inputId = "samp", selected = select)
    }
  })
  observeEvent(input$previousSample, {
    current <- match(input$samp, ag$sampleList)
    if (current > 1) {
      select <- ag$sampleList[[current - 1]]
      updateSelectInput(inputId = "samp", selected = select)
    }
  })
  
  observeEvent(c(input$samp, input$nextSample, input$previousSample), {
    current <- match(input$samp, ag$sampleList)
    if (input$samp != "") {
      if (current >= length(ag$sampleList)) {
        disable("nextSample")
      } else {
        enable("nextSample")
      }
      if (current <= 1) {
        disable("previousSample")
      } else {
        enable("previousSample")
      }
    }
  })
  
  observeEvent(c(
    input$samp,
    input$Y,
    input$X,
    reactPar$d,
    input$typ,
    input$gateCancel
  ),
  {
    reactRectangle$d <- FALSE
    reactInterval$d <- FALSE
    reactPolygon$d <- FALSE
    reactQuadrant$d <- FALSE
  })
  
  observeEvent(input$gateOk, {
    "%notin%" <- Negate("%in%")
    if (input$drawGateName %notin% gs_get_pop_paths(ag$gs, path = 1)) {
      if (reactRectangle$d == TRUE) {
        components <- c(
          input$main_brush$xmin,
          input$main_brush$xmax,
          input$main_brush$ymin,
          input$main_brush$ymax
        )
        colnames <- c(input$X, input$Y)
        mat <- matrix(components,
                      ncol = 2,
                      dimnames = list(c("min", "max"), colnames))
        gate <- rectangleGate(.gate = mat)
      } else if (isolate(reactPolygon$d) == TRUE) {
        mat <- matrix(ncol = 2, nrow = nrow(polygonCoords$d))
        for (i in seq(nrow(polygonCoords$d))) {
          mat[i,] <- c(polygonCoords$d$x[i], polygonCoords$d$y[i])
        }
        colnames(mat) <- c(input$X, input$Y)
        gate <- polygonGate(mat)
      } else if (reactInterval$d == TRUE) {
        components <- c(input$main_brush$xmin,
                        input$main_brush$xmax)
        mat <- matrix(components,
                      ncol = 1,
                      dimnames = list(c("min", "max"), c(input$X)))
        gate <- rectangleGate(.gate = mat)
      }
      par <- isolate(reactPar$d)
      gs_pop_add(ag$gs,
                 gate,
                 parent = par,
                 name = input$drawGateName)
      recompute(ag$gs)
      updateTextInput(inputId = "drawGateName", value = "")
      reactRectangle$d <- FALSE
      reactInterval$d <- FALSE
      reactPolygon$d <- FALSE
    } else {
      alert("Please choose a different gate name.")
    }
  })
  
  output$plotUI <- renderUI({
    if (input$typ == "Histogram") {
      if (reactInterval$d == TRUE) {
        brushOpts <- brushOpts(
          "main_brush",
          fill = "lightgrey",
          stroke = "black",
          opacity = 0.4,
          delay = 10,
          direction = "x"
        )
      } else {
        brushOpts <- NULL
      }
      hoverOpts <- NULL
    } else {
      if (reactRectangle$d == TRUE) {
        brushOpts <- brushOpts(
          "main_brush",
          fill = "lightgrey",
          stroke = "black",
          opacity = 0.4,
          delay = 10,
          direction = "xy"
        )
        hoverOpts <- NULL
      } else {
        brushOpts <- NULL
        if (isolate(reactPolygon$d) == TRUE
            || reactQuadrant$d == TRUE) {
          hoverOpts <- hoverOpts(
            id = "mainplot_hover",
            delay = 100,
            delayType = "debounce",
            nullOutside = TRUE
          )
        } else {
          hoverOpts <- NULL
        }
      }
    }
    plotOutput(
      "mainplot",
      height = 440,
      click = "main_click",
      dblclick = "main_dblclick",
      brush = brushOpts,
      hover = hoverOpts
    )
  })
  
  output$mainplot <- renderPlot({
    par(mar = c(4, 6, 1, 1) + 0.1, lwd = 2)
    polygonCoords$d
    input$gateOk
    input$gateCancel
    plotActivator$d
    updateSelectInput(inputId = "editGate", label = "Edit")
    disable("editGate")
    reactNameChange$d <- NULL
    session$resetBrush("main_brush")
    ID <- match(input$samp, ag$fileList)
    par <- isolate(reactPar$d)
    newPlot(
      ID,
      input$X,
      input$Y,
      par,
      isolate(input$typ),
      isolate(reactShowAxis$d),
      isolate(reactAxisFont$d)
    )
    detectGate(
      ID,
      input$X,
      input$Y,
      par,
      isolate(input$typ),
      "plot",
      isolate(reactShowGateName$d),
      isolate(reactGateFont$d)
    )
    if (isolate(reactPolygon$d) == TRUE) {
      click <- isolate({
        input$main_click
      })
      if (!is.null(click)) {
        if (abs(click$x) > 0 && abs(click$y) > 0) {
          coords <- polygonCoords$d
          for (i in seq(nrow(coords))) {
            if (i > 1) {
              segments(
                coords$x[i - 1],
                coords$y[i - 1],
                coords$x[i],
                coords$y[i],
                col = "red",
                lwd = 2
              )
            }
            points(
              coords$x[i],
              coords$y[i],
              pch = 19,
              cex = 2,
              col = "red"
            )
          }
        }
      }
    } else if (reactQuadrant$d == TRUE) {
      abline(
        v = hoverCoords$d[1],
        h = hoverCoords$d[2],
        lwd = 2,
        col = "red"
      )
    } else if (isolate(reactRectangle$d) == FALSE
               && isolate(reactInterval$d) == FALSE
               && isolate(reactPolygon$d) == FALSE
               && isolate(reactQuadrant$d) == FALSE) {
      hide("gateOk", TRUE, "fade")
      hide("gateCancel", TRUE, "fade")
      hide("drawGateName", TRUE, "fade")
      delay(500, shinyjs::show("rectang", TRUE, "fade"))
      delay(500, shinyjs::show("polygon", TRUE, "fade"))
      delay(500, shinyjs::show("quad", TRUE, "fade"))
      delay(500, shinyjs::show("interval", TRUE, "fade"))
      updateTextInput(inputId = "drawGateName",
                      value = "",
                      placeholder = "Please draw a gate")
      disable("drawGateName")
      polygonCoords$d <- data.frame(NULL)
    }
    output$events <- renderText({
      counts <- gs_pop_get_stats(ag$gs[input$samp])[, 3]
      shownEv <- counts[which(gs_get_pop_paths(ag$gs) == par)][[1]]
      totalEv <- counts[1][[1]]
      paste0(
        "Plotted events: ",
        format(shownEv, big.mark = ","),
        "/",
        format(totalEv, big.mark = ",")
      )
    })
    if (grepl("FS", input$X) ||
        grepl("SS", input$X) || input$X == "Time") {
      disable("customizeAxisX")
    } else {
      enable("customizeAxisX")
    }
    if (grepl("FS", input$Y) ||
        grepl("SS", input$Y) || input$Y == "Time"
        || isolate(input$typ) == "Histogram") {
      disable("customizeAxisY")
    } else {
      enable("customizeAxisY")
    }
  }, width = 470)
  
  observeEvent(c(input$rectang, input$polygon, input$quad, input$interval),
               {
                 if (!is.null(ag$gs)) {
                   detectGate(
                     1,
                     input$X,
                     input$Y,
                     reactPar$d,
                     input$typ,
                     ">= 4",
                     isolate(reactShowGateName$d),
                     isolate(reactGateFont$d)
                   )
                   if (length(ag$found) >= 4) {
                     showModal(
                       modalDialog(
                         "Only 4 gates are allowed on a plot. Would you like
                    to remove the 4 existing gates to draw new ones?",
                    footer = list(
                      actionButton(
                        inputId = "deleteAllModal",
                        label = "Delete existing gates",
                        style = ag$blackB
                      ),
                      actionButton(
                        inputId = "cancelModal",
                        label = "Cancel",
                        style = ag$whiteB
                      )
                    ),
                    easyClose = FALSE,
                    size = "s"
                       )
                     )
                   } else {
                     hideTools()
                   }
                 }
               })
  
  observeEvent(input$rectang, {
    detectGate(
      1,
      input$X,
      input$Y,
      reactPar$d,
      input$typ,
      ">= 4",
      isolate(reactShowGateName$d),
      isolate(reactGateFont$d)
    )
    if (length(ag$found) < 4) {
      reactRectangle$d <- TRUE
      secStep()
    }
  })
  
  observeEvent(input$polygon, {
    detectGate(
      1,
      input$X,
      input$Y,
      reactPar$d,
      input$typ,
      ">= 4",
      isolate(reactShowGateName$d),
      isolate(reactGateFont$d)
    )
    if (length(ag$found) < 4) {
      reactPolygon$d <- TRUE
      secStep()
    }
  })
  
  observeEvent(input$quad, {
    detectGate(
      1,
      input$X,
      input$Y,
      reactPar$d,
      input$typ,
      ">= 4",
      isolate(reactShowGateName$d),
      isolate(reactGateFont$d)
    )
    if (length(ag$found) < 4) {
      reactQuadrant$d <- TRUE
      delay(500, shinyjs::show("gateCancel", TRUE, "fade"))
    }
  })
  
  observeEvent(input$interval, {
    reactInterval$d <- TRUE
    hideTools()
    secStep()
  })
  
  observeEvent(input$mainplot_hover, {
    hover <- input$mainplot_hover
    coords <- hoverCoords$d
    if (!is.null(coords[1])) {
      if (max(hover$x, coords[1]) / min(hover$x, coords[1]) > 1.1
          ||
          max(hover$y, coords[2]) / min(hover$y, coords[2]) > 1.1) {
        reactHover$d <- reactHover$d + 1
        hoverCoords$d[1] <- hover$x
        hoverCoords$d[2] <- hover$y
      }
    } else {
      reactHover$d <- reactHover$d + 1
      hoverCoords$d[1] <- hover$x
      hoverCoords$d[2] <- hover$y
    }
  })
  
  observeEvent(input$main_click, {
    inputX <- input$main_click$x
    inputY <- input$main_click$y
    if (reactPolygon$d == TRUE) {
      coords <- polygonCoords$d
      if (!is.null(coords)) {
        if (nrow(coords) > 2) {
          maxX <- max(inputX, coords$x[1])
          minX <- min(inputX, coords$x[1])
          maxY <- max(inputY, coords$y[1])
          minY <- min(inputY, coords$y[1])
          if (grepl("FS", input$X) || grepl("SS", input$X)) {
            xVerifier <- maxX / minX < 2.5
          } else {
            xVerifier <- maxX / minX < 1.1
          }
          if (grepl("FS", input$Y) || grepl("SS", input$Y)) {
            yVerifier <- maxY / minY < 2.5
          } else {
            yVerifier <- maxY / minY < 1.1
          }
          if (xVerifier && yVerifier) {
            polygonCoords$d <- rbind(coords, c(coords$x[1],
                                               coords$y[1]))
            updateTextInput(inputId = "drawGateName",
                            placeholder = "Type gate name")
            enable("drawGateName")
          } else {
            nrowX <- coords$x[nrow(coords)]
            nrowY <- coords$y[nrow(coords)]
            if (nrowX != coords$x[1] && inputY != nrowY) {
              polygonCoords$d <- rbind(coords, c(inputX, inputY))
            }
          }
        } else {
          polygonCoords$d <- rbind(coords, c(inputX, inputY))
          colnames(polygonCoords$d) <- c("x", "y")
        }
      }
    } else if (reactQuadrant$d == TRUE) {
      mat <- matrix(c(inputX, inputY),
                    ncol = 2,
                    dimnames = list(c("value"),
                                    c(input$X, input$Y)))
      gate <- quadGate(.gate = mat)
      names <- c(
        paste0("Q1: ", input$X, "- ", input$Y, "+"),
        paste0("Q2: ", input$X, "+ ", input$Y, "+"),
        paste0("Q3: ", input$X, "+ ", input$Y, "-"),
        paste0("Q4: ", input$X, "- ", input$Y, "-")
      )
      gs_pop_add(ag$gs, gate, parent = reactPar$d, names = names)
      recompute(ag$gs)
      detectGate(
        1,
        input$X,
        input$Y,
        reactPar$d,
        input$typ,
        "OK",
        isolate(reactShowGateName$d),
        isolate(reactGateFont$d)
      )
      updateTextInput(inputId = "drawGateName", value = "")
      hierarchyActivator$d <- isolate(hierarchyActivator$d) + 1
      reactQuadrant$d <- FALSE
    }
  })
  
  observeEvent(c(input$main_brush, input$main_click, input$drawGateName),
               {
                 if (reactRectangle$d == TRUE || reactInterval$d == TRUE) {
                   if (is.null(input$main_brush)) {
                     disable("gateOk")
                     updateTextInput(inputId = "drawGateName",
                                     value = "",
                                     placeholder = "Please draw a gate")
                     disable("drawGateName")
                   } else {
                     if (input$drawGateName == "") {
                       disable("gateOk")
                       updateTextInput(inputId = "drawGateName",
                                       placeholder = "Type gate name")
                       enable("drawGateName")
                     } else {
                       enable("gateOk")
                     }
                   }
                 }
                 if (reactPolygon$d == TRUE) {
                   if (input$drawGateName == "") {
                     disable("gateOk")
                   } else {
                     enable("gateOk")
                   }
                 }
               })
  
  observeEvent(input$main_dblclick, {
    popPaths <- gs_get_pop_paths(ag$gs)
    if (length(popPaths) > 1) {
      inputX <- input$main_dblclick$x
      inputY <- input$main_dblclick$y
      ag$inputXx2 <- inputX
      ag$inputYy2 <- inputY
      popGates <- vapply(popPaths[-1], function(x)
        list(gs_pop_get_gate(ag$gs, x)[1][[1]]),
        list(length(popPaths[-1])))
      clickableGate <- list()
      for (i in seq(popGates)) {
        channels <- names(popGates[[i]]@parameters)
        if (length(channels) == 1) {
          channels[2] <- ag$Y
        }
        popParent <-
          gs_pop_get_parent(ag$gs, popGates[[i]]@filterId)
        if (input$typ != "Histogram") {
          if (channels[1] == input$X
              && channels[2] == input$Y
              && popParent == reactPar$d) {
            clickableGate[[i]] <- popGates[[i]]
          }
        } else {
          if (channels[1] == input$X
              && popParent == reactPar$d) {
            clickableGate[[i]] <- popGates[[i]]
          }
        }
      }
      updateParent <- FALSE
      singlePath <- gs_get_pop_paths(ag$gs, path = 1)
      clickableGate <- unlist(clickableGate)
      for (i in clickableGate) {
        index <- which(singlePath == i@filterId)
        if (is(i, "rectangleGate")) {
          if (length(names(i@parameters)) > 1) {
            if (inputX >= i@min[[1]]
                && inputX <= i@max[[1]]
                && inputY >= i@min[[2]]
                && inputY <= i@max[[2]]) {
              updateParent <- TRUE
              reactPar$d <- popPaths[index]
            }
          } else {
            if (inputX >= i@min[[1]]
                && inputX <= i@max[[1]]) {
              updateParent <- TRUE
              reactPar$d <- popPaths[index]
            }
          }
        }
        if (is(i, "polygonGate")) {
          if (inputX >= min(i@boundaries[, 1])
              && inputX <= max(i@boundaries[, 1])
              && inputY >= min(i@boundaries[, 2])
              && inputY <= max(i@boundaries[, 2])) {
            updateParent <- TRUE
            reactPar$d <- popPaths[index]
          }
        }
      }
      if (updateParent == TRUE) {
        if (reactPar$d != "root") {
          gate <- gs_pop_get_gate(ag$gs, reactPar$d)
          channels <- names(gate[[1]]@parameters)
        }
        popParents <-
          gs_pop_get_count_fast(ag$gs[1], "freq")[, 3][[1]]
        index <- which(popParents == reactPar$d)
        gatesWithThisParent <- popGates[index]
        if (length(gatesWithThisParent) > 0) {
          channels <- names(gatesWithThisParent[[1]]@parameters)
        }
        updateSelectInput(inputId = "X", selected = channels[1])
        if (length(channels) == 2) {
          if (input$typ == "Histogram") {
            updateSelectInput(inputId = "typ",
                              selected = "Pseudocolor")
          }
          updateSelectInput(inputId = "Y", selected = channels[2])
          if (input$X == channels[1]
              && input$Y == channels[2]) {
            plotActivator$d <- plotActivator$d + 1
          }
        } else {
          if (input$typ != "Histogram") {
            updateSelectInput(inputId = "Y", selected = "")
            updateSelectInput(inputId = "typ", selected = "Histogram")
          }
          if (input$X == channels) {
            plotActivator$d <- plotActivator$d + 1
          }
        }
      }
    }
  })
  
  output$saveMainPlot <- downloadHandler(function() {
    paste0(substr(input$samp, 1, nchar(input$samp) - 4), ".png")
  },
  function(file) {
    png(
      file,
      units = "in",
      height = 6,
      width = 6.44,
      res = 300
    )
    par(mar = c(4, 6, 1, 1) + 0.1, lwd = 2)
    ID <- match(input$samp, ag$fileList)
    newPlot(
      ID,
      input$X,
      input$Y,
      reactPar$d,
      input$typ,
      isolate(reactShowAxis$d),
      isolate(reactAxisFont$d)
    )
    detectGate(
      ID,
      input$X,
      input$Y,
      reactPar$d,
      input$typ,
      "plot",
      isolate(reactShowGateName$d),
      isolate(reactGateFont$d)
    )
    dev.off()
  })
  
  ##right----
  output$hierarchy <- renderPlot({
    input$gateOk
    hierarchyActivator$d
    input$deleteAllModal
    if (length(gs_get_pop_paths(ag$gs)) > 1) {
      hPlot <- plot(ag$gs)
      labels <- gs_get_pop_paths(ag$gs, path = 1)
      labels[1] <- "ungated"
      for (i in seq(labels)) {
        if (substr(labels[i], 1, 1) == "Q" && nchar(labels[i]) >= 13) {
          labels[i] <- substr(labels[i], 1, 2)
        }
      }
      names(labels) <- nodes(hPlot)
      nodeAttrs <- list(label = labels)
      attrs <-
        list(
          node = list(
            fillcolor = "white",
            shape = "box",
            width = 1,
            color = "gray90",
            style = "rounded"
          ),
          graph = list(rankdir = "TB")
        )
      index <- which(gs_get_pop_paths(ag$gs) == reactPar$d)
      if (is.null(reactNameChange$d)) {
        colour <- "black"
        names(colour) <- nodes(hPlot)[index]
      } else {
        if (reactPar$d == reactNameChange$d) {
          colour <- "red"
          names(colour) <- nodes(hPlot)[index]
        } else {
          colour <- c("black", "red")
          i2 <- which(gs_get_pop_paths(ag$gs) == reactNameChange$d)
          names(colour) <- c(nodes(hPlot)[index], nodes(hPlot)[i2])
        }
      }
      nodeAttrs$color <- colour
      ag$hPlot <- plot(hPlot, nodeAttrs = nodeAttrs, attrs = attrs)
      plot(hPlot, nodeAttrs = nodeAttrs, attrs = attrs)
      js$enableTab("ancestryTab")
      js$enableTab("autoancestryTab")
      js$enableTab("overlayTab")
      js$enableTab("prolifTab")
      js$enableTab("tsneTab")
      js$enableTab("resultTab")
    } else {
      nodes <- c("ungated", "B")
      edgeL <- list(ungated = "B", B = "ungated")
      graphNEL <- new(
        "graphNEL",
        nodes = nodes,
        edgemode = "directed",
        edgeL = edgeL
      )
      attrs <- list(node = list(
        fillcolor = "white",
        shape = "rectangle",
        width = 1
      ))
      edgeAttrs <- list(color = c("ungated~B" = "white"))
      nodeAttrs <-
        list(color = c("B" = "white"),
             fontcolor = c("B" = "white"))
      ag$hPlot <- plot(agopen(
        graphNEL,
        "",
        attrs = attrs,
        edgeAttrs = edgeAttrs,
        nodeAttrs = nodeAttrs
      ))
      disableTabs()
    }
    pops <- gs_get_pop_paths(ag$gs, path = 1)[-1]
    if (ag$loadingFile == FALSE) {
      updateSelectInput(inputId = "bgPop", choices = c("", pops[-1]))
      updateSelectInput(inputId = "ovP", choices = c("", pops))
    }
    updateSelectInput(inputId = "tSPar", choices = c("", pops))
    updateSelectInput(inputId = "rsParent", choices = c("", pops))
    ag$loadingFile <- FALSE
  })
  
  observeEvent(input$hierarchy_dblclick, {
    popPaths <- gs_get_pop_paths(ag$gs)
    if (length(popPaths) > 1) {
      agNode <- ag$hPlot@AgNode
      xy <- c(seq_len(length(agNode)))
      dataF <- data.frame(x = xy, y = xy)
      for (i in seq(agNode)) {
        rownames(dataF)[i] <- popPaths[i]
        dataF$x[i] <- agNode[[i]]@center@x
        dataF$y[i] <- agNode[[i]]@center@y
        dataF$short[i] <- agNode[[i]]@txtLabel@labelText
      }
      selected <- nearPoints(
        dataF,
        input$hierarchy_dblclick,
        xvar = "x",
        yvar = "y",
        threshold = 25,
        maxpoints = 1,
        addDist = TRUE
      )
      if (length(rownames(selected)) > 0) {
        popGates <- vapply(popPaths[-1], function(x)
          list(gs_pop_get_gate(ag$gs, x)[1][[1]]),
          list(length(popPaths[-1])))
        popParents <-
          gs_pop_get_count_fast(ag$gs[1], "freq")[, 3][[1]]
        checkingPop <- rownames(selected)
        if (checkingPop != reactPar$d) {
          reactPar$d <- checkingPop
          if (reactPar$d != "root") {
            gate <- gs_pop_get_gate(ag$gs, reactPar$d)
            channels <- names(gate[[1]]@parameters)
          }
          index <- which(popParents == reactPar$d)
          gatesWithThisParent <- popGates[index]
          if (length(gatesWithThisParent) > 0) {
            channels <- names(gatesWithThisParent[[1]]@parameters)
          }
          updateSelectInput(inputId = "X", selected = channels[1])
          if (length(channels) == 2) {
            if (input$typ == "Histogram") {
              updateSelectInput(inputId = "typ",
                                selected = "Pseudocolor")
            }
            updateSelectInput(inputId = "Y", selected = channels[2])
            if (input$X == channels[1]
                && input$Y == channels[2]) {
              plotActivator$d <- plotActivator$d + 1
            }
          } else {
            if (input$typ != "Histogram") {
              updateSelectInput(inputId = "Y",
                                selected = "")
              updateSelectInput(inputId = "typ",
                                selected = "Histogram")
            }
            if (input$X == channels) {
              plotActivator$d <- plotActivator$d + 1
            }
          }
        }
      }
    }
  })
  
  observeEvent(input$hierarchy_click, {
    popPaths <- gs_get_pop_paths(ag$gs)
    if (length(popPaths) > 1) {
      agNode <- ag$hPlot@AgNode
      xy <- c(seq_len(length(agNode)))
      dataF <- data.frame(x = xy, y = xy)
      for (i in seq(agNode)) {
        rownames(dataF)[i] <- popPaths[i]
        dataF$x[i] <- agNode[[i]]@center@x
        dataF$y[i] <- agNode[[i]]@center@y
        dataF$short[i] <- agNode[[i]]@txtLabel@labelText
      }
      selected <- nearPoints(
        dataF,
        input$hierarchy_click,
        xvar = "x",
        yvar = "y",
        threshold = 25,
        maxpoints = 1,
        addDist = TRUE
      )
      if (length(rownames(selected)) > 0
          && rownames(selected) != "ungated") {
        reactNameChange$d <- rownames(selected)
        updateSelectInput(
          inputId = "editGate",
          label = HTML("<span style=
                                         'color: red'>Edit</span>")
        )
        enable("editGate")
      } else {
        updateSelectInput(inputId = "editGate", label = "Edit")
        disable("editGate")
        reactNameChange$d <- NULL
      }
    }
  })
  
  observeEvent(input$editGate, {
    index <- which(gs_get_pop_paths(ag$gs) == reactNameChange$d)
    path <- gs_get_pop_paths(ag$gs, path = 1)[index]
    showModal(modalDialog(
      if (substr(path, 1, 1) != "Q" || substr(path, 3, 4) != ": ") {
        paste0("Please type a new name for gate '", path, "':")
      } else{
        "Quadrant gates can't have their names changed."
      },
      br(),
      br(),
      if (substr(path, 1, 1) != "Q" || substr(path, 3, 4) != ": ") {
        textInput(inputId = "newNameTextBox",
                  label = NULL,
                  width = "180px")
      },
      footer = list(
        if (substr(path, 1, 1) != "Q" || substr(path, 3, 4) != ": ") {
          actionButton(inputId = "changeNameModal",
                       label = "Ok",
                       style = ag$blackB)
        },
        actionButton(
          inputId = "cancelModal",
          label = "Cancel",
          style = ag$whiteB
        ),
        actionButton(
          inputId = "deleteGate",
          label = "Delete gate",
          style = ag$blackB
        )
      ),
      easyClose = TRUE,
      size = "m"
    ))
  })
  
  observeEvent(input$deleteGate, {
    rootPlotLoad <- FALSE
    popPaths <- gs_get_pop_paths(ag$gs, path = 1)
    index <- which(gs_get_pop_paths(ag$gs) == reactNameChange$d)
    gatetoDelete <- popPaths[index]
    i2 <- which(gs_get_pop_paths(ag$gs) == reactPar$d)
    showingParent <- popPaths[i2]
    popsAfter <-
      gh_pop_get_descendants(ag$gs[[1]], showingParent, path = 1)
    popsAfterIDs <- vapply(popsAfter, function(x)
      which(popPaths == x), integer(1))
    popsBefore <- popPaths[-popsAfterIDs]
    gs_pop_remove(ag$gs, gatetoDelete)
    if (gatetoDelete == showingParent ||
        gatetoDelete %in% popsBefore) {
      reactPar$d <- "root"
    }
    plotActivator$d <- plotActivator$d + 1
    hierarchyActivator$d <- isolate(hierarchyActivator$d) + 1
    removeModal()
    updateSelectInput(
      inputId = "editGate",
      label = HTML("<span style=
                                     'color: black'>Edit</span>")
    )
  })
  
  observeEvent(c(input$newNameTextBox, input$editGate), {
    if (!is.null(input$newNameTextBox)) {
      if (input$newNameTextBox == "") {
        disable("changeNameModal")
      } else {
        enable("changeNameModal")
      }
    }
  })
  
  observeEvent(input$changeNameModal, {
    popPaths <- gs_get_pop_paths(ag$gs, path = 1)
    if (input$newNameTextBox %in% popPaths) {
      alert("Please choose a different gate name.")
    } else {
      index <- which(gs_get_pop_paths(ag$gs) == reactNameChange$d)
      gs_pop_set_name(ag$gs, popPaths[index], input$newNameTextBox)
      removeModal()
      if (reactNameChange$d == reactPar$d) {
        reactPar$d <- paste0("/", input$newNameTextBox)
      }
      plotActivator$d <- plotActivator$d + 1
    }
  })
  
  observeEvent(input$deleteAllModal, {
    for (i in ag$found) {
      gs_pop_remove(ag$gs, i)
    }
    removeModal()
    plotActivator$d <- plotActivator$d + 1
  })
  
  observeEvent(input$cancelModal, {
    reactAxisCustom$d <- NULL
    removeModal()
  })
  
  output$exportImageGates <- downloadHandler("Gate hierarchy.png",
                                             function(file) {
                                               png(
                                                 file,
                                                 units = "in",
                                                 height = 6,
                                                 width = 6.44,
                                                 res = 300
                                               )
                                               par(mar = c(4, 6, 1, 1) + 0.1, lwd = 2)
                                               plot(ag$hPlot)
                                               dev.off()
                                             })
  
  observeEvent(input$parentHelp, {
    showModal(
      modalDialog(
        title = "Help",
        "This sidebar shows the gates hierarchy.",
        br(),
        br(),
        "The current parent is highlighted by a black line.",
        br(),
        br(),
        "Double clicking a parent box leads to a new plot with the
            sorted population.",
        br(),
        br(),
        "Clicking in a parent box selects it to allow for editing.",
        footer = NULL,
        size = "m",
        easyClose = TRUE
      )
    )
  })
  
  
  #Ancestry----
  ##left----
  observeEvent(input$bgType, {
    if (isolate(input$bgType) == "Backgating"
        && length(gs_get_pop_paths(ag$gs)) > 2) {
      enable("bgPop")
    } else {
      updateSelectInput(inputId = "bgPop", selected = "")
      disable("bgPop")
    }
  })
  
  observeEvent(input$bgPop, {
    if (input$bgType == "Backgating") {
      bgActivator$d <- isolate(bgActivator$d) + 1
    }
  })
  
  ##main----
  output$ancestry <- renderPlot({
    bgActivator$d
    ancestryGen(input$bgType,
                input$bgPreviewSample,
                isolate(input$bgPop))
  }, height = 480, width = 800)
  
  output$exportImageAncestry <-
    downloadHandler("Ancestry plots.png",
                    function(file) {
                      png(
                        file,
                        units = "in",
                        height = 6.6,
                        width = 11.05,
                        res = 300
                      )
                      ancestryGen(input$bgType, input$bgPreviewSample, input$bgPop)
                      dev.off()
                    })
  
  
  #Overlays----
  ##left----
  observeEvent(c(input$ovTyp, input$ovTon, input$ovY, input$ovX,
                 input$ovP),
               {
                 samples <- isolate(reactOvSamples$d)
                 if (input$ovTyp == "Overlaid histogram"
                     || input$ovTyp == "Offset histogram") {
                   if (input$ovY != "") {
                     updateSelectInput(inputId = "ovY", selected = "")
                   } else {
                     ovActivator$d <- ovActivator$d + 1
                   }
                   disable("ovY")
                   if (input$ovTyp == "Overlaid histogram") {
                     if (input$ovX != "" && input$ovP != "") {
                       enable("ovSamples")
                       if (length(samples) > 2) {
                         reactOvSamples$d <- samples[seq_len(3)]
                       }
                       if (length(samples) < 3) {
                         showOvPlot$d <- TRUE
                       } else {
                         showOvPlot$d <- FALSE
                       }
                       showOvPlot$d <- TRUE
                     } else {
                       disable("ovSamples")
                       showOvPlot$d <- FALSE
                     }
                     if (input$ovTon != "") {
                       updateSelectInput(inputId = "ovTon",
                                         selected = "")
                     } else {
                       ovActivator$d <- ovActivator$d + 1
                     }
                     disable("ovTon")
                   } else {
                     if (input$ovTon != ""
                         && input$ovX != ""
                         && input$ovP != "") {
                       enable("ovSamples")
                       ovActivator$d <- ovActivator$d + 1
                       showOvPlot$d <- TRUE
                     } else {
                       disable("ovSamples")
                       showOvPlot$d <- FALSE
                     }
                     enable("ovTon")
                     ovActivator$d <- ovActivator$d + 1
                   }
                 } else {
                   if (length(samples) > 2) {
                     reactOvSamples$d <- samples[seq_len(2)]
                   }
                   if (input$ovTyp != ""
                       && input$ovTon != ""
                       && input$ovY != ""
                       && input$ovX != ""
                       && input$ovP != "") {
                     enable("ovSamples")
                     ovActivator$d <- ovActivator$d + 1
                     showOvPlot$d <- TRUE
                   } else {
                     disable("ovSamples")
                     showOvPlot$d <- FALSE
                   }
                   enable("ovTon")
                   enable("ovY")
                   ovActivator$d <- ovActivator$d + 1
                 }
               })
  
  observeEvent(input$ovSamples, {
    sampAlert$d <- ""
    updateCheckboxGroupInput(inputId = "samplecheckbox",
                             selected = isolate(reactOvSamples$d))
    if (input$ovTyp == "Dot plot") {
      modaltitle <- "Select 2 samples for the Dot plot."
    } else if (input$ovTyp == "Overlaid histogram") {
      modaltitle <- "Select 2-3 samples for the Overlaid histogram."
    } else {
      modaltitle <- "Select 2-10 samples for the Offset histogram."
    }
    showModal(
      modalDialog(
        tags$style("#selectedsamplealert{color: red}"),
        strong(modaltitle),
        checkboxGroupInput(
          "samplecheckbox",
          label = "",
          choices = ag$sampleList,
          width = "100%"
        ),
        footer = fluidRow(
          column(
            width = 8,
            align = "left",
            textOutput("selectedsamplealert")
          ),
          column(
            width = 4,
            align = "right",
            list(
              actionButton(
                inputId = "ovokbutton",
                label = "Ok",
                style = ag$blackB
              ),
              actionButton(
                inputId = "cancelModal",
                label = "Cancel",
                style = ag$whiteB
              )
            ),
          )
        ),
        easyClose = TRUE,
        size = "m"
      )
    )
    disable("ovokbutton")
  })
  
  observeEvent(input$samplecheckbox, {
    len <- length(input$samplecheckbox)
    if (input$ovTyp == "Dot plot") {
      if (len == 2) {
        sampAlert$d <- ""
        enable("ovokbutton")
      } else {
        if (len > 2) {
          sampAlert$d <- "Only 2 samples are allowed for Dot plots."
        } else {
          sampAlert$d <- ""
        }
        disable("ovokbutton")
      }
    } else if (input$ovTyp == "Overlaid histogram") {
      sampAlert$d <- "Up to 3 samples are allowed for
            Overlaid histograms."
      if (len == 2 || len == 3) {
        sampAlert$d <- ""
        enable("ovokbutton")
      } else {
        if (len > 3) {
          sampAlert$d <- "Up to 3 samples are allowed for
                    Overlaid histograms."
        } else {
          sampAlert$d <- ""
        }
        disable("ovokbutton")
      }
    } else {
      sampAlert$d <- "Up to 10 samples are allowed for
            Overlaid histograms."
      if (len > 1 && len < 11) {
        sampAlert$d <- ""
        enable("ovokbutton")
      } else {
        if (len > 10) {
          sampAlert$d <- "Up to 10 samples are allowed for
                    Overlaid histograms."
        } else {
          sampAlert$d <- ""
        }
        disable("ovokbutton")
      }
    }
  })
  
  output$selectedsamplealert <- renderText({
    sampAlert$d
  })
  
  observeEvent(input$ovokbutton, {
    ovActivator$d <- ovActivator$d + 1
    reactOvSamples$d <- input$samplecheckbox
    removeModal()
  })
  
  observeEvent(input$displayOptOv, {
    t1 <- "[for=ovdisplayaxisfont]+span>.irs>.irs-single, "
    t2 <- "[for=ovdisplayaxisfont]"
    showModal(
      modalDialog(
        tags$style(HTML(paste0(
          t1, t2, ag$sliderCol
        ))),
        checkboxInput("ovdisplayaxis", label = "Show axis titles",
                      value = reactOvShowAxis$d),
        sliderInput(
          "ovdisplayaxisfont",
          label = NULL,
          min = 10,
          max = 25,
          value = reactOvAxisFont$d,
          ticks = FALSE
        ),
        footer = list(
          actionButton(
            inputId = "ovreloadplotmodal",
            label = "Ok",
            style = ag$blackB
          ),
          actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = TRUE,
        size = "s"
      )
    )
  })
  
  observeEvent(input$ovreloadplotmodal, {
    ovActivator$d <- ovActivator$d + 1
    removeModal()
    reactOvShowAxis$d <- input$ovdisplayaxis
    reactOvAxisFont$d <- input$ovdisplayaxisfont
  })
  
  observeEvent(c(input$ovdisplayaxis, input$displayOptOv) , {
    if (length(input$ovdisplayaxis) != 0) {
      if (input$ovdisplayaxis == TRUE) {
        shinyjs::show("ovdisplayaxisfont")
      } else {
        hide("ovdisplayaxisfont")
      }
    }
  })
  
  ##main----
  output$overlays <- renderPlot({
    ovActivator$d
    if (!is.null(reactOvSamples$d)) {
      if (isolate(showOvPlot$d) == TRUE) {
        enable("displayOptOv")
        enable("ovSampleOrder")
        enable("exportImageOverlay")
        if (isolate(input$ovP) == "ungated") {
          currentParent <- "root"
        } else {
          currentParent <- isolate(input$ovP)
        }
        ag$ovBoolean <- TRUE
        ID <- match(isolate(reactOvSamples$d), ag$fileList)
        overlay(
          ID,
          isolate(input$ovX),
          isolate(input$ovY),
          currentParent,
          isolate(input$ovTyp),
          isolate(input$ovTon),
          isolate(reactOvShowAxis$d),
          isolate(reactOvAxisFont$d)
        )
      } else {
        ag$ovBoolean <- FALSE
        disable("displayOptOv")
        disable("ovSampleOrder")
        disable("exportImageOverlay")
      }
    } else {
      ag$ovBoolean <- FALSE
      disable("displayOptOv")
      disable("ovSampleOrder")
      disable("exportImageOverlay")
    }
  }, height = 423, width = 800)
  
  observeEvent(input$ovSampleOrder, {
    reactModalTitle$d <- 1
    reactSampleOrder$d <- NULL
    choices <-
      showModal(
        modalDialog(
          strong(textOutput("ordermodaltitle")),
          radioButtons(
            "orderradiobutton",
            label = "",
            choices = rev(isolate(reactOvSamples$d)),
            width = "100%",
            selected = ""
          ),
          footer = actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          ),
          easyClose = TRUE,
          size = "m"
        )
      )
  })
  
  output$ordermodaltitle <- renderText({
    paste0("Select sample number ",
           reactModalTitle$d,
           " (from top to bottom).")
  })
  
  observeEvent(input$orderradiobutton, {
    reactModalTitle$d <- reactModalTitle$d + 1
    ID <- match(input$orderradiobutton, reactOvSamples$d)
    for (i in seq(input$orderradiobutton)) {
      reactSampleOrder$d <- append(reactSampleOrder$d, ID)
    }
    if (length(reactSampleOrder$d) != length(reactOvSamples$d)) {
      samplestoShow <- reactOvSamples$d
      for (i in rev(sort(reactSampleOrder$d))) {
        preShow <- samplestoShow != samplestoShow[i]
        samplestoShow <- samplestoShow[preShow]
      }
      updateRadioButtons(
        inputId = "orderradiobutton",
        choices = rev(samplestoShow),
        selected = ""
      )
    } else {
      reactOvSamples$d <- rev(reactOvSamples$d[reactSampleOrder$d])
      ovActivator$d <- ovActivator$d + 1
      removeModal()
    }
  })
  
  output$exportImageOverlay <- downloadHandler(function() {
    paste0(input$ovTyp, ".png")
  },
  function(file) {
    png(
      file,
      units = "in",
      height = 5.8,
      width = 11.05,
      res = 300
    )
    if (isolate(input$ovP) == "ungated") {
      currentParent <- "root"
    } else {
      currentParent <- isolate(input$ovP)
    }
    ID <- match(isolate(reactOvSamples$d), ag$fileList)
    overlay(
      ID,
      isolate(input$ovX),
      isolate(input$ovY),
      currentParent,
      isolate(input$ovTyp),
      isolate(input$ovTon),
      isolate(reactOvShowAxis$d),
      isolate(reactOvAxisFont$d)
    )
    dev.off()
  })
  
  
  #Auto plot----
  ##left----
  observeEvent(input$autocustomizeAxisY, {
    reactautoAxisCustom$d <- input$autoY
  })
  observeEvent(input$autocustomizeAxisX, {
    reactautoAxisCustom$d <- input$autoX
  })
  
  observeEvent(reactautoAxisCustom$d, {
    ag$val <-
      ag$customAxis[[which(ag$fluoCh == reactautoAxisCustom$d)]]
    showModal(
      modalDialog(
        tags$style(HTML(
          paste0(
            "[for=autoposslider]+span>.irs>.irs-single,
                                   [for=autoposslider]",
            ag$sliderCol
          )
        )),
        tags$style(HTML(
          paste0(
            "[for=autonegslider]+span>.irs>.irs-single,
                                   [for=autonegslider]",
            ag$sliderCol
          )
        )),
        tags$style(HTML(
          paste0(
            "[for=autowidthslider]+span>.irs>.irs-single,
                                   [for=autowidthslider]",
            ag$sliderCol
          )
        )),
        fluidRow(
          column(
            width = 9,
            align = "right",
            plotOutput("autoaxisplot", height = 440)
          ),
          column(
            width = 3,
            align = "left",
            style = "margin-top: 350px;",
            actionButton(
              inputId = "autoresetbutton",
              label = "Reset",
              style = ag$whiteB
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            align = "center",
            sliderInput(
              "autoposslider",
              label = "Positive Decades",
              min = 2,
              max = 7,
              step = 0.02,
              value = ag$val[1],
              ticks = FALSE
            ),
            sliderInput(
              "autonegslider",
              label = "Negative Decades",
              min = 0,
              max = 1,
              step = 0.1,
              value = ag$val[2],
              ticks = FALSE
            ),
            sliderInput(
              "autowidthslider",
              label = "Width Basis",
              min = 0,
              max = 3,
              step = 0.1,
              value = log10(abs(ag$val[3])),
              ticks = FALSE
            )
          )
        ),
        footer = list(
          actionButton(
            inputId = "autosaveaxis",
            label = "Ok",
            style = ag$blackB
          ),
          actionButton(
            inputId = "autocancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = FALSE,
        size = "l"
      )
    )
  })
  
  observeEvent(input$autoresetbutton, {
    updateSliderInput(inputId = "autoposslider", value = 4.42)
    updateSliderInput(inputId = "autonegslider", value = 0)
    updateSliderInput(inputId = "autowidthslider", value = 2)
  })
  
  output$autoaxisplot <- renderPlot({
    par(mar = c(4, 6, 1, 1) + 0.1, lwd = 2)
    inverseT <- flowjo_biexp(
      channelRange = 4096,
      maxValue = 262144,
      pos = ag$val[1],
      neg = ag$val[2],
      widthBasis = ag$val[3],
      inverse = TRUE
    )
    ag$autowidthBasis <- -10 ^ input$autowidthslider
    if (input$autowidthslider < 1) {
      ag$autowidthBasis <- format(round(ag$autowidthBasis, 2), nsmall = 2)
    } else if (input$autowidthslider < 2) {
      ag$autowidthBasis <- format(round(ag$autowidthBasis, 1), nsmall = 1)
    } else if (input$autowidthslider <= 3) {
      ag$autowidthBasis <- round(ag$autowidthBasis)
    }
    autotrans <- flowjo_biexp(
      channelRange = 4096,
      maxValue = 262144,
      pos = input$autoposslider,
      neg = input$autonegslider,
      widthBasis = as.numeric(ag$autowidthBasis)
    )
    autounTransDF <- inverseT(ag$auto_dF[, reactautoAxisCustom$d])
    autotempDF <- autotrans(autounTransDF)
    xLim <- c(0, 4100)
    if (length(autotempDF) > 1) {
      autoreferenceHist <- hist(autotempDF, breaks = 20, plot = FALSE)
      autorefCounts <- autoreferenceHist$counts
      autorefDensity <- autoreferenceHist$density
      automultiplier <- autorefCounts / autorefDensity
      autodensLine <- density(autotempDF)
      autodensLine$autoY <- autodensLine$autoY * automultiplier[1]
      automaxDens <- max(autodensLine[2]$y)
      plot(
        autodensLine,
        xaxt = "n",
        yaxt = "n",
        ann = FALSE,
        xlim = xLim,
        ylim = c(0, automaxDens * 1.05),
        xaxs = "i",
        yaxs = "i"
      )
      polygon(autodensLine,
              col = "grey",
              border = "black",
              lwd = 1)
      autoaxisTicks(2, "histo")
    } else {
      plot(
        0,
        xaxt = "n",
        yaxt = "n",
        ann = FALSE,
        xaxs = "i",
        yaxs = "i",
        cex = 0
      )
    }
    title(
      ylab = "Count",
      cex.lab = 1 + 15 / 10,
      line = 3,
      font.lab = 2
    )
    index <- which(colnames(ag$auto_gs) == reactautoAxisCustom$d)
    title(
      xlab = names(ag$ch)[index],
      cex.lab = 1 + 15 / 10,
      line = 3,
      font.lab = 2
    )
    autocustomAx <- c(input$autoposslider,
                      input$autonegslider,
                      as.numeric(ag$autowidthBasis))
    autoaxisTicks(1, "log", autocustomAx)
  }, height = 440, width = 470)
  
  observeEvent(input$autosaveaxis, {
    currentA <-
      ag$customAxis[[which(ag$fluoCh == reactautoAxisCustom$d)]]
    newA <- c(input$autoposslider,
              input$autonegslider,
              as.numeric(ag$autowidthBasis))
    if (!identical(currentA, newA)) {
      inverseT <- flowjo_biexp(
        channelRange = 4096,
        maxValue = 262144,
        pos = currentA[1],
        neg = currentA[2],
        widthBasis = currentA[3],
        inverse = TRUE
      )
      autotrans <- flowjo_biexp(
        channelRange = 4096,
        maxValue = 262144,
        pos = input$autoposslider,
        neg = input$autonegslider,
        widthBasis = as.numeric(ag$autowidthBasis)
      )
      ag$customAxis[[which(ag$fluoCh == reactautoAxisCustom$d)]] <-
        newA
      autopopPaths <- gs_get_pop_paths(ag$auto_gs)
      autoreAddPops()
      ag$auto_gs <- gs_clone(tempEnv$auto_gs)
      names(ag$customAxis) <- ag$fluoCh
      for (i in ag$fluoCh[ag$fluoCh != reactautoAxisCustom$d]) {
        autotransSub <- flowjo_biexp_trans(
          channelRange = 4096,
          maxValue = 262144,
          pos = ag$customAxis[[i]][1],
          neg = ag$customAxis[[i]][2],
          widthBasis = ag$customAxis[[i]][3]
        )
        ag$auto_gs <-
          transform(ag$auto_gs, transformerList(i, autotransSub))
      }
      autotransSub <-
        flowjo_biexp_trans(
          channelRange = 4096,
          maxValue = 262144,
          pos = input$autoposslider,
          neg = input$autonegslider,
          widthBasis = as.numeric(ag$autowidthBasis)
        )
      ag$auto_gs <-
        transform(ag$auto_gs,
                  transformerList(reactautoAxisCustom$d,
                                  autotransSub))
      recompute(ag$auto_gs)
      for (i in seq(autopopPaths)[-1]) {
        autogate <- gs_pop_get_gate(ag$auto_gs, autopopPaths[i])[1][[1]]
        autogateCh <- names(autogate@parameters)
        for (j in seq(autogateCh)) {
          if (names(autogate@parameters)[j] == reactautoAxisCustom$d) {
            if (is(autogate, "polygonGate")) {
              toInvertObj <- autogate@boundaries[, reactautoAxisCustom$d]
              obj <- inverseT(toInvertObj)
              autogate@boundaries[, reactautoAxisCustom$d] <-
                autotrans(obj)
            } else {
              if (autogate@min[[j]] != "-Inf") {
                autogate@min[[j]] <- autotrans(inverseT(autogate@min[[j]]))
              } else {
                autogate@min[[j]] <- -Inf
              }
              if (autogate@max[[j]] != "Inf") {
                autogate@max[[j]] <- autotrans(inverseT(autogate@max[[j]]))
              } else {
                autogate@max[[j]] <- Inf
              }
            }
            autopopgateList <- vapply(ag$fileList, function(x)
              list(autogate), list(length(ag$fileList)))
            gs_pop_set_gate(ag$auto_gs, autogate@filterId, autopopgateList)
            recompute(ag$auto_gs, autogate@filterId)
          }
        }
      }
      reactautoAxisCustom$d <- NULL
      autoplotActivator$d <- autoplotActivator$d + 1
    } else {
      reactautoAxisCustom$d <- NULL
    }
    autocompPlotActivator$d <- autocompPlotActivator$d + 1
    autohierarchyActivator$d <- autohierarchyActivator$d + 1
    removeModal()
  })
  
  observeEvent(input$autodisplayOptMain, {
    showModal(
      modalDialog(
        tags$style(HTML(
          paste0(
            "[for=autodisplayAxisFont]
            +span>.irs>.irs-single, [for=autodisplayAxisFont]",
            ag$sliderCol
          )
        )),
        checkboxInput("autodisplayAxis", label = "Show axis titles",
                      value = reactautoShowAxis$d),
        sliderInput(
          "autodisplayAxisFont",
          label = NULL,
          min = 10,
          max = 25,
          value = reactautoAxisFont$d,
          ticks = FALSE
        ),
        br(),
        tags$style(HTML(
          paste0(
            "[for=autodisplayGateFont]
            +span>.irs>.irs-single, [for=autodisplayGateFont]",
            ag$sliderCol
          )
        )),
        checkboxInput(
          "autodisplayGateName",
          label = "Show autogate name",
          value = reactautoShowGateName$d
        ),
        sliderInput(
          "autodisplayGateFont",
          label = NULL,
          min = 1,
          max = 30,
          value = reactautoGateFont$d,
          ticks = FALSE
        ),
        footer = list(
          actionButton(
            inputId = "autoreloadplotmodal",
            label = "Ok",
            style = ag$blackB
          ),
          actionButton(
            inputId = "autocancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = TRUE,
        size = "s"
      )
    )
    if (length(gs_get_pop_paths(ag$auto_gs)) > 1) {
      shinyjs::show("autodisplayGateName")
      shinyjs::show("autodisplayGateFont")
    } else {
      hide("autodisplayGateName")
      hide("autodisplayGateFont")
    }
  })
  
  observeEvent(input$autoreloadplotmodal, {
    autoplotActivator$d <- autoplotActivator$d + 1
    removeModal()
    reactautoShowAxis$d <- input$autodisplayAxis
    reactautoAxisFont$d <- input$autodisplayAxisFont
    reactautoShowGateName$d <- input$autodisplayGateName
    reactautoGateFont$d <- input$autodisplayGateFont
  })
  
  observeEvent(c(input$autodisplayAxis, input$autodisplayoptmain) , {
    if (length(input$autodisplayAxis) != 0) {
      if (input$autodisplayAxis == TRUE) {
        shinyjs::show("autodisplayAxisFont")
      } else {
        hide("autodisplayAxisFont")
      }
    }
  })
  
  observeEvent(input$autoplotHelp, {
    showModal(
      modalDialog(
        title = "Help",
        "Use the buttons at the top right side of the autoplot to
            select the autogate type.",
        br(),
        br(),
        "After selecting the autogate type click within the plot area to apply the gate, this may take a few minutes and may require the                user to name the gate, then click Ok",
        br(),
        br(),
        "Double clicking the population inside the hierarchy plot leads to
            a new autoplot with the sorted population.",
        footer = NULL,
        size = "m",
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$about, {
    showModal(
      modalDialog(
        title = "AutoGateR",
        "AutoGateR is an open-source project that provides an user-friendly,
            high-performance UI for automatically compensating and gating flow cytometry data in R.",
        br(),
        br(),
        "Built by Eric Brooks",
        footer = NULL,
        size = "m",
        easyClose = TRUE
      )
    )
  })
  
  ##main----
  observeEvent(input$autotyp, {
    if (input$autotyp != "Histogram") {
      if (input$autoY == "") {
        updateSelectInput(inputId = "autoY", selected = ag$autoY)
      } else {
        autoplotActivator$d <- autoplotActivator$d + 1
      }
      enable("autoY")
      disable("autointerval")
      enable("auto2D")
      enable("autosinglet")
      enable("autoquad")
    } else {
      updateSelectInput(inputId = "autoY", selected = "")
      disable("autoY")
      enable("autointerval")
      disable("autosinglet")
      disable("auto2D")
      disable("autoquad")
    }
  })
  
  observeEvent(input$autonextSample, {
    current <- match(input$autosamp, ag$sampleList)
    if (current + 1 <= length(ag$sampleList)) {
      select <- ag$sampleList[[current + 1]]
      updateSelectInput(inputId = "autosamp", selected = select)
    }
  })
  observeEvent(input$autopreviousSample, {
    current <- match(input$autosamp, ag$sampleList)
    if (current > 1) {
      select <- ag$sampleList[[current - 1]]
      updateSelectInput(inputId = "autosamp", selected = select)
    }
  })
  
  observeEvent(c(
    input$autosamp,
    input$autonextSample,
    input$autopreviousSample
  ),
  {
    current <- match(input$autosamp, ag$sampleList)
    if (input$autosamp != "") {
      if (current >= length(ag$sampleList)) {
        disable("autonextSample")
      } else {
        enable("autonextSample")
      }
      if (current <= 1) {
        disable("autopreviousSample")
      } else {
        enable("autopreviousSample")
      }
    }
  })
  
  observeEvent(
    c(
      input$autosamp,
      input$autoY,
      input$autoX,
      reactautoPar$d,
      input$autotyp,
      input$autogateCancel
    ),
    {
      reactautosinglet$d <- FALSE
      reactautoInterval$d <- FALSE
      reactauto2D$d <- FALSE
      reactautoQuadrant$d <- FALSE
    }
  )
  
  ###gate insert
  observeEvent(input$autogateOk, {
    "%notin%" <- Negate("%in%")
    if (input$autodrawGateName %notin% gs_get_pop_paths(ag$auto_gs, path =
                                                        1)) {
      if ((reactautosinglet$d == TRUE ||
           reactauto2D$d == TRUE || reactautoInterval$d == TRUE)) {
        autogate <- ag$autogate
        autopar <- isolate(reactautoPar$d)
        gs_pop_add(ag$auto_gs,
                   autogate,
                   parent = autopar,
                   name = input$autodrawGateName)
        
        
        recompute(ag$auto_gs)
        
        
      } else {
        ID2 <- match(ag$sampleList, ag$fileList)
        autogate <- ag$autogate
        autopar <- isolate(reactautoPar$d)
        for (gate in autogate) {
          gs_pop_add(ag$auto_gs, gate, parent = autopar)
        }
        recompute(ag$auto_gs)
      }
      updateTextInput(inputId = "autodrawGateName", value = "")
      reactautosinglet$d <- FALSE
      reactautoInterval$d <- FALSE
      reactauto2D$d <- FALSE
      reactautoQuadrant$d <- FALSE
    } else {
      alert("Please choose a different autogate name.")
    }
  })
  
  output$autoplotUI <- renderUI({
    plotOutput(
      "automainplot",
      height = 440,
      click = "automain_click"
      ,
      dblclick = "automain_dblclick"
    )
  })
  
  output$automainplot <- renderPlot({
    par(mar = c(4, 6, 1, 1) + 0.1, lwd = 2)
    autopolygonCoords$d
    input$autogateOk
    input$autogateCancel
    autoplotActivator$d
    updateSelectInput(inputId = "editautoGate", label = "Edit")
    disable("editautoGate")
    reactautoNameChange$d <- NULL
    ID <- match(input$autosamp, ag$fileList)
    autopar <- isolate(reactautoPar$d)
    autonewPlot(
      ID,
      input$autoX,
      input$autoY,
      autopar,
      isolate(input$autotyp),
      isolate(reactautoShowAxis$d),
      isolate(reactautoAxisFont$d)
    )
    autodetectGate(
      ID,
      input$autoX,
      input$autoY,
      autopar,
      isolate(input$autotyp),
      "autoplot",
      isolate(reactautoShowGateName$d),
      isolate(reactautoGateFont$d)
    )
    if (isolate(reactautosinglet$d) == FALSE
        && isolate(reactautoInterval$d) == FALSE
        && isolate(reactauto2D$d) == FALSE
        && isolate(reactautoQuadrant$d) == FALSE) {
      hide("autogateOk", TRUE, "fade")
      hide("autogateCancel", TRUE, "fade")
      hide("autodrawGateName", TRUE, "fade")
      delay(500, shinyjs::show("autosinglet", TRUE, "fade"))
      delay(500, shinyjs::show("auto2D", TRUE, "fade"))
      delay(500, shinyjs::show("autoquad", TRUE, "fade"))
      delay(500, shinyjs::show("autointerval", TRUE, "fade"))
      updateTextInput(inputId = "autodrawGateName",
                      value = "",
                      placeholder = "Please draw a autogate")
      disable("autodrawGateName")
      autopolygonCoords$d <- data.frame(NULL)
    }
    output$autoevents <- renderText({
      counts <- gs_pop_get_stats(ag$auto_gs[input$autosamp])[, 3]
      shownEv <-
        counts[which(gs_get_pop_paths(ag$auto_gs) == autopar)][[1]]
      totalEv <- counts[1][[1]]
      paste0(
        "Plotted autoevents: ",
        format(shownEv, big.mark = ","),
        "/",
        format(totalEv, big.mark = ",")
      )
    })
    if (grepl("FS", input$autoX) ||
        grepl("SS", input$autoX) || input$autoX == "Time") {
      disable("autocustomizeAxisX")
    } else {
      enable("autocustomizeAxisX")
    }
    if (grepl("FS", input$autoY) ||
        grepl("SS", input$autoY) || input$autoY == "Time"
        || isolate(input$autotyp) == "Histogram") {
      disable("autocustomizeAxisY")
    } else {
      enable("autocustomizeAxisY")
    }
  }, width = 470)
  
  observeEvent(c(
    input$autosinglet,
    input$auto2D,
    input$autoquad,
    input$autointerval
  ),
  {
    if (!is.null(ag$auto_gs)) {
      autodetectGate(
        1,
        input$autoX,
        input$autoY,
        reactautoPar$d,
        input$autotyp,
        ">= 4",
        isolate(reactautoShowGateName$d),
        isolate(reactautoGateFont$d)
      )
      if (length(ag$autofound) >= 4) {
        showModal(
          modalDialog(
            "Only 4 gates are allowed on a autoplot. Would you like
                    to remove the 4 existing gates to draw new ones?",
            footer = list(
              actionButton(
                inputId = "autodeleteAllModal",
                label = "Delete existing gates",
                style = ag$blackB
              ),
              actionButton(
                inputId = "autocancelModal",
                label = "Cancel",
                style = ag$whiteB
              )
            ),
            easyClose = FALSE,
            size = "s"
          )
        )
      } else {
        autohideTools()
      }
    }
  })
  
  observeEvent(input$autosinglet, {
    autodetectGate(
      1,
      input$autoX,
      input$autoY,
      reactautoPar$d,
      input$autotyp,
      ">= 4",
      isolate(reactautoShowGateName$d),
      isolate(reactautoGateFont$d)
    )
    if (length(ag$autofound) < 4) {
      reactautosinglet$d <- TRUE
      autosecStep()
    }
  })
  
  observeEvent(input$auto2D, {
    autodetectGate(
      1,
      input$autoX,
      input$autoY,
      reactautoPar$d,
      input$autotyp,
      ">= 4",
      isolate(reactautoShowGateName$d),
      isolate(reactautoGateFont$d)
    )
    if (length(ag$autofound) < 4) {
      reactauto2D$d <- TRUE
      autosecStep()
    }
  })
  
  observeEvent(input$autoquad, {
    autodetectGate(
      1,
      input$autoX,
      input$autoY,
      reactautoPar$d,
      input$autotyp,
      ">= 4",
      isolate(reactautoShowGateName$d),
      isolate(reactautoGateFont$d)
    )
    if (length(ag$autofound) < 4) {
      reactautoQuadrant$d <- TRUE
      delay(500, shinyjs::show("autogateCancel", TRUE, "fade"))
      delay(500, shinyjs::show("autogateOk", TRUE, "fade"))
    }
  })
  
  observeEvent(input$autointerval, {
    reactautoInterval$d <- TRUE
    autohideTools()
    autosecStep()
  })
  
  
  observeEvent(input$automain_click, {
    inputX <- input$automain_click$x
    inputY <- input$automain_click$y
    if (length(isolate(reactautoPar$d > 1))) {
      autopar <- isolate(reactautoPar$d)
    } else {
      autopar <- "root"
    }
    aa <- gs_pop_get_data(ag$auto_gs, parent = autopar)
    ID <- match(input$autosamp, ag$fileList)
    ag$ID <- ID
    displayAlert <- NULL
    if (reactautosinglet$d == TRUE) {
      sing <-
        singletGate(aa[[ID]], area = input$autoX, height =  input$autoY)
      mat <- matrix(sing@boundaries, ncol = 2)
      colnames(mat) <- c(input$autoX, input$autoY)
      ag$autogate <- polygonGate(mat)
      
      if (is.null(ag$autogate)) {
        displayAlert <- "Loading Auto Singlet Gate Failed"
      }
    }
    else if (reactauto2D$d == TRUE) {
      poly <-
        gate_flowclust_2d(aa[[ID]],
                          xChannel = input$autoX,
                          yChannel = input$autoY)
      ag$ellipse <- ""
      ag$ellipse$mean <- c(poly@mean[[1]], poly@mean[[2]])
      ag$ellipse$cov <-
        matrix(
          c(poly@cov[1, 1], poly@cov[1, 2], poly@cov[2, 1], poly@cov[2, 2]),
          ncol = 2,
          dimnames = list(
            c(input$autoX, input$autoY),
            c(input$autoX, input$autoY)
          )
        )
      ag$ellipse$dist <- poly@distance
      autoellipseCoords$d <- ag$ellipse
      ag$autogate <-
        ellipsoidGate(
          mean = autoellipseCoords$d$mean,
          .gate = autoellipseCoords$d$cov,
          distance = autoellipseCoords$d$dist
        )
      
      if (is.null(ag$autogate)) {
        displayAlert <- "Loading Auto2D Gate Failed"
      }
      
    } else if (reactautoInterval$d == TRUE) {
      dd <- cytoset_to_flowSet(aa)
      p <- getPeaks(dd[[ID]], channel = input$autoX)
      pl <- length(p$Peaks)
      autointgate <-
        gate_flowclust_1d(aa[[ID]], params = input$autoX, K = pl)
      autocomponents <- c(autointgate@min, autointgate@max)
      mat <- matrix(autocomponents,
                    ncol = 1,
                    dimnames = list(c("min", "max"), c(input$autoX)))
      ag$autogate <- rectangleGate(.gate = mat)
      
      if (is.null(ag$autogate)) {
        displayAlert <- "Loading Auto Interval Gate Failed"
      }
    }
    else if (reactautoQuadrant$d == TRUE) {
      ag$ID2 <- match(ag$sampleList, ag$fileList)
      ag$ID3 <- ag$fileList[ag$ID]
      ID4 <- as.character(unlist(ag$ID3))
      ID5 <- first(ag$ID2)
      ID6 <- last(ag$ID2)
      bb <- cytoset_to_flowSet(aa)
      cc <- bb[ID5:ID6]
      ag$names <-
        c(
          paste0("Q1: ", input$autoX, "- ", input$autoY, "+"),
          paste0("Q2: ", input$autoX, "+ ", input$autoY, "+"),
          paste0("Q3: ", input$autoX, "+ ", input$autoY, "-"),
          paste0("Q4: ", input$autoX, "- ", input$autoY, "-")
        )
      
      ag$input$autoX <- input$autoX
      ag$input$autoY <- input$autoX
      dd <- cytoset_to_flowSet(aa)
      p1 <- getPeaks(dd[[ID]], channel = input$autoX)
      p1l <- length(p1$Peaks)
      p2 <- getPeaks(dd[[ID]], channel = input$autoY)
      p2l <- length(p2$Peaks)
      ag$g1 <-
        gate_flowclust_1d(aa[[ID]], params = input$autoX, K = p1l)
      ag$g2 <-
        gate_flowclust_1d(aa[[ID]], params = input$autoY, K = p2l)
      g1min <- ag$g1@min[[1]] * 1.33
      g2min <- ag$g2@min[[1]] * 1.33
      ag$quad <-
        fsApply(cc, function(fr)
          custom_gate_quad_sequential(
            fr,
            gFunc = "gate_mindensity2",
            max = c(g1min, g2min),
            channels = c(input$autoX, input$autoY)
          ))
      
      reactautoQuadrant$d <- reactautoQuadrant$d + 1
      ag$q <- ag$quad[[ID4]]
      ag$q1$min <- matrix(ag$q$q1@min, nrow = 1)
      ag$q1$max <- matrix(ag$q$q1@max, nrow = 1)
      ag$q1mat <- rbind(ag$q1$min, ag$q1$max)
      colnames(ag$q1mat) <- c(input$autoX, input$autoY)
      ag$q2$min <- matrix(ag$q$q2@min, nrow = 1)
      ag$q2$max <- matrix(ag$q$q2@max, nrow = 1)
      ag$q2mat <- rbind(ag$q2$min, ag$q2$max)
      colnames(ag$q2mat) <- c(input$autoX, input$autoY)
      ag$q3$min <- matrix(ag$q$q3@min, nrow = 1)
      ag$q3$max <- matrix(ag$q$q3@max, nrow = 1)
      ag$q3mat <- rbind(ag$q3$min, ag$q3$max)
      colnames(ag$q3mat) <- c(input$autoX, input$autoY)
      ag$q4$min <- matrix(ag$q$q4@min, nrow = 1)
      ag$q4$max <- matrix(ag$q$q4@max, nrow = 1)
      ag$q4mat <- rbind(ag$q4$min, ag$q4$max)
      colnames(ag$q4mat) <- c(input$autoX, input$autoY)
      gateq1 <- rectangleGate(.gate = ag$q1mat)
      gateq2 <- rectangleGate(.gate = ag$q2mat)
      gateq3 <- rectangleGate(.gate = ag$q3mat)
      gateq4 <- rectangleGate(.gate = ag$q4mat)
      gateq1@filterId <- ag$names[1]
      gateq2@filterId <- ag$names[2]
      gateq3@filterId <- ag$names[3]
      gateq4@filterId <- ag$names[4]
      ag$autogate <- c(gateq1, gateq2, gateq3, gateq4)
      if (is.null(ag$autogate)) {
        displayAlert <- "Loading Auto Quadrant Gate Failed"
      }
    }
    if (is.null(displayAlert)) {
      withProgress(
        message = "Please wait...",
        detail = "",
        value = 0,
        max = 100,
        {
          if (reactautoQuadrant$d == TRUE) {
            updateTextInput(inputId = "autodrawGateName",
                            value = "")
          } else {
            # updateTextInput(inputId="autodrawGateName",
            #                 placeholder="Type gate name")
            enable("autodrawGateName")
          }
        }
      )
    } else {
      alert(displayAlert)
    }
  })
  
  observeEvent(c(input$automain_click, input$autodrawGateName), {
    if (reactautosinglet$d == TRUE ||
        reactautoInterval$d == TRUE ||
        reactauto2D$d == TRUE || reactautoQuadrant$d == TRUE) {
      if (input$autodrawGateName == "") {
        disable("autogateOk")
        updateTextInput(inputId = "autodrawGateName",
                        placeholder = "Type autogate name")
        enable("autodrawGateName")
      } else {
        enable("autogateOk")
      }
    }
  })
  
  observeEvent(input$automain_dblclick, {
    autopopPaths <- gs_get_pop_paths(ag$auto_gs)
    if (length(autopopPaths) > 1) {
      autoinputX <- input$automain_dblclick$x
      autoinputY <- input$automain_dblclick$y
      ag$autoinputXx <- autoinputX
      ag$autoinputYy <- autoinputY
      autopopGates <- vapply(autopopPaths[-1], function(x)
        list(gs_pop_get_gate(ag$auto_gs, x)[1][[1]]),
        list(length(autopopPaths[-1])))
      autoclickableGate <- list()
      for (i in seq(autopopGates)) {
        autochannels <- names(autopopGates[[i]]@parameters)
        if (length(autochannels) == 1) {
          autochannels[2] <- ag$autoY
        }
        autopopParent <-
          gs_pop_get_parent(ag$auto_gs, autopopGates[[i]]@filterId)
        if (input$autotyp != "Histogram") {
          if (autochannels[1] == input$autoX
              && autochannels[2] == input$autoY
              && autopopParent == reactautoPar$d) {
            autoclickableGate[[i]] <- autopopGates[[i]]
          }
        } else {
          if (autochannels[1] == input$autoX
              && autopopParent == reactautoPar$d) {
            autoclickableGate[[i]] <- autopopGates[[i]]
          }
        }
      }
      autoupdateParent <- FALSE
      autosinglePath <- gs_get_pop_paths(ag$auto_gs, path = 1)
      autoclickableGate <- unlist(autoclickableGate)
      for (i in autoclickableGate) {
        index <- which(autosinglePath == i@filterId)
        if (is(i, "rectangleGate")) {
          if (length(names(i@parameters)) > 1) {
            if (autoinputX >= i@min[[1]]
                && autoinputX <= i@max[[1]]
                && autoinputY >= i@min[[2]]
                && autoinputY <= i@max[[2]]) {
              autoupdateParent <- TRUE
              reactautoPar$d <- autopopPaths[index]
            }
          } else {
            if (autoinputX >= i@min[[1]]
                && autoinputX <= i@max[[1]]) {
              autoupdateParent <- TRUE
              reactautoPar$d <- autopopPaths[index]
            }
          }
        }
        if (is(i, "polygonGate")) {
          if (autoinputX >= min(i@boundaries[, 1])
              && autoinputX <= max(i@boundaries[, 1])
              && autoinputY >= min(i@boundaries[, 2])
              && autoinputY <= max(i@boundaries[, 2])) {
            autoupdateParent <- TRUE
            reactautoPar$d <- autopopPaths[index]
          }
        }
        if (is(i, "ellipsoidGate")) {
          if (autoinputX >= (i@mean[[1]] - i@distance[1])
              && autoinputX <= (i@mean[[1]] + i@distance[1])
              && autoinputY >= (i@mean[[2]] - i@distance[1])
              && autoinputY <= (i@mean[[1]] + i@distance[1])) {
            autoupdateParent <- TRUE
            reactautoPar$d <- autopopPaths[index]
          }
        }
      }
      if (autoupdateParent == TRUE) {
        if (reactautoPar$d != "root") {
          autogate <- gs_pop_get_gate(ag$auto_gs, reactautoPar$d)
          autochannels <- names(autogate[[1]]@parameters)
        }
        autopopParents <-
          gs_pop_get_count_fast(ag$auto_gs[1], "freq")[, 3][[1]]
        index <- which(autopopParents == reactautoPar$d)
        autogatesWithThisParent <- autopopGates[index]
        if (length(autogatesWithThisParent) > 0) {
          autochannels <- names(autogatesWithThisParent[[1]]@parameters)
        }
        updateSelectInput(inputId = "autoX", selected = autochannels[1])
        if (length(autochannels) == 2) {
          if (input$autotyp == "Histogram") {
            updateSelectInput(inputId = "autotyp",
                              selected = "Pseudocolor")
          }
          updateSelectInput(inputId = "autoY", selected = autochannels[2])
          if (input$autoX == autochannels[1]
              && input$autoY == autochannels[2]) {
            autoplotActivator$d <- autoplotActivator$d + 1
          }
        } else {
          if (input$autotyp != "Histogram") {
            updateSelectInput(inputId = "autoY", selected = "")
            updateSelectInput(inputId = "autotyp", selected = "Histogram")
          }
          if (input$autoX == autochannels) {
            autoplotActivator$d <- autoplotActivator$d + 1
          }
        }
      }
    }
  })
  
  output$autosaveMainPlot <- downloadHandler(function() {
    paste0(substr(input$autosamp, 1, nchar(input$autosamp) - 4), ".png")
  },
  function(file) {
    png(
      file,
      units = "in",
      height = 6,
      width = 6.44,
      res = 300
    )
    par(mar = c(4, 6, 1, 1) + 0.1, lwd = 2)
    ID <- match(input$autosamp, ag$fileList)
    autonewPlot(
      ID,
      input$autoX,
      input$autoY,
      reactautoPar$d,
      input$autotyp,
      isolate(reactautoShowAxis$d),
      isolate(reactautoAxisFont$d)
    )
    autodetectGate(
      ID,
      input$autoX,
      input$autoY,
      reactautoPar$d,
      input$autotyp,
      "autoplot",
      isolate(reactautoShowGateName$d),
      isolate(reactautoGateFont$d)
    )
    dev.off()
  })
  
  ##right autohierarchy----
  output$autohierarchy <- renderPlot({
    input$autogateOk
    autohierarchyActivator$d
    input$autodeleteAllModal
    if (length(gs_get_pop_paths(ag$auto_gs)) > 1) {
      autohplot <- plot(ag$auto_gs)
      labels <- gs_get_pop_paths(ag$auto_gs, path = 1)
      labels[1] <- "ungated"
      for (i in seq(labels)) {
        if (substr(labels[i], 1, 1) == "Q" && nchar(labels[i]) >= 13) {
          labels[i] <- substr(labels[i], 1, 2)
        }
      }
      names(labels) <- nodes(autohplot)
      autonodeAttrs <- list(label = labels)
      autoattrs <-
        list(
          node = list(
            fillcolor = "white",
            shape = "box",
            width = 1,
            color = "gray90",
            style = "rounded"
          ),
          graph = list(rankdir = "TB")
        )
      autoindex <-
        which(gs_get_pop_paths(ag$auto_gs) == reactautoPar$d)
      if (is.null(reactautoNameChange$d)) {
        colour <- "black"
        names(colour) <- nodes(autohplot)[autoindex]
      } else {
        if (reactautoPar$d == reactautoNameChange$d) {
          colour <- "red"
          names(colour) <- nodes(autohplot)[autoindex]
        } else {
          colour <- c("black", "red")
          i2 <-
            which(gs_get_pop_paths(ag$auto_gs) == reactautoNameChange$d)
          names(colour) <-
            c(nodes(autohplot)[autoindex], nodes(autohplot)[i2])
        }
      }
      autonodeAttrs$color <- colour
      ag$autohplot <-
        plot(autohplot, nodeAttrs = autonodeAttrs, attrs = autoattrs)
      plot(autohplot, nodeAttrs = autonodeAttrs, attrs = autoattrs)
      js$enableTab("ancestryTab")
      js$enableTab("autoancestryTab")
      js$enableTab("overlayTab")
      js$enableTab("autooverlayTab")
      js$enableTab("prolifTab")
      js$enableTab("tsneTab")
      js$enableTab("resultTab")
      js$enableTab("autoresultTab")
    } else {
      nodes <- c("ungated", "B")
      edgeL <- list(ungated = "B", B = "ungated")
      graphNEL <- new(
        "graphNEL",
        nodes = nodes,
        edgemode = "directed",
        edgeL = edgeL
      )
      autoattrs <-
        list(node = list(
          fillcolor = "white",
          shape = "rectangle",
          width = 1
        ))
      autoedgeAttrs <- list(color = c("ungated~B" = "white"))
      autonodeAttrs <-
        list(color = c("B" = "white"),
             fontcolor = c("B" = "white"))
      ag$autohplot <- plot(
        agopen(
          graphNEL,
          "",
          attrs = autoattrs,
          edgeAttrs = autoedgeAttrs,
          nodeAttrs = autonodeAttrs
        )
      )
      autodisableTabs()
    }
    autopops <- gs_get_pop_paths(ag$auto_gs, path = 1)[-1]
    if (ag$loadingFile == FALSE) {
      updateSelectInput(inputId = "autobgPop", choices = c("", autopops[-1]))
      updateSelectInput(inputId = "autoovP", choices = c("", autopops))
    }
    updateSelectInput(inputId = "autotSPar", choices = c("", autopops))
    updateSelectInput(inputId = "autorsParent", choices = c("", autopops))
    ag$loadingFile <- FALSE
  })
  
  observeEvent(input$autohierarchy_dblclick, {
    autopopPaths <- gs_get_pop_paths(ag$auto_gs)
    if (length(autopopPaths) > 1) {
      autoagNode <- ag$autohplot@AgNode
      xy <- c(seq_len(length(autoagNode)))
      autodataF <- data.frame(x = xy, y = xy)
      for (i in seq(autoagNode)) {
        rownames(autodataF)[i] <- autopopPaths[i]
        autodataF$x[i] <- autoagNode[[i]]@center@x
        autodataF$y[i] <- autoagNode[[i]]@center@y
        autodataF$short[i] <- autoagNode[[i]]@txtLabel@labelText
      }
      selected <-
        nearPoints(
          autodataF,
          input$autohierarchy_dblclick,
          xvar = "x",
          yvar = "y",
          threshold = 25,
          maxpoints = 1,
          addDist = TRUE
        )
      if (length(rownames(selected)) > 0) {
        autopopGates <- vapply(autopopPaths[-1], function(x)
          list(gs_pop_get_gate(ag$auto_gs, x)[1][[1]]),
          list(length(autopopPaths[-1])))
        autopopParents <-
          gs_pop_get_count_fast(ag$auto_gs[1], "freq")[, 3][[1]]
        autocheckingPop <- rownames(selected)
        if (autocheckingPop != reactautoPar$d) {
          reactautoPar$d <- autocheckingPop
          if (reactautoPar$d != "root") {
            autogate <- gs_pop_get_gate(ag$auto_gs, reactautoPar$d)
            autochannels <- names(autogate[[1]]@parameters)
          }
          autoindex <- which(autopopParents == reactautoPar$d)
          autogatesWithThisParent <- autopopGates[autoindex]
          if (length(autogatesWithThisParent) > 0) {
            autochannels <- names(autogatesWithThisParent[[1]]@parameters)
          }
          updateSelectInput(inputId = "autoX", selected = autochannels[1])
          if (length(autochannels) == 2) {
            if (input$autotyp == "Histogram") {
              updateSelectInput(inputId = "autotyp",
                                selected = "Pseudocolor")
            }
            updateSelectInput(inputId = "autoY", selected = autochannels[2])
            if (input$autoX == autochannels[1]
                && input$autoY == autochannels[2]) {
              autoplotActivator$d <- autoplotActivator$d + 1
            }
          } else {
            if (input$autotyp != "Histogram") {
              updateSelectInput(inputId = "autoY",
                                selected = "")
              updateSelectInput(inputId = "autotyp",
                                selected = "Histogram")
            }
            if (input$autoX == autochannels) {
              autoplotActivator$d <- autoplotActivator$d + 1
            }
          }
        }
      }
    }
  })
  
  observeEvent(input$autohierarchy_click, {
    autopopPaths <- gs_get_pop_paths(ag$auto_gs)
    if (length(autopopPaths) > 1) {
      autoagNode <- ag$autohplot@AgNode
      xy <- c(seq_len(length(autoagNode)))
      autodataF <- data.frame(x = xy, y = xy)
      for (i in seq(autoagNode)) {
        rownames(autodataF)[i] <- autopopPaths[i]
        autodataF$x[i] <- autoagNode[[i]]@center@x
        autodataF$y[i] <- autoagNode[[i]]@center@y
        autodataF$short[i] <- autoagNode[[i]]@txtLabel@labelText
      }
      selected <- nearPoints(
        autodataF,
        input$autohierarchy_click,
        xvar = "x",
        yvar = "y",
        threshold = 25,
        maxpoints = 1,
        addDist = TRUE
      )
      if (length(rownames(selected)) > 0
          && rownames(selected) != "ungated") {
        reactautoNameChange$d <- rownames(selected)
        updateSelectInput(
          inputId = "editautoGate",
          label = HTML("<span style=
                                           'color: red'>Edit</span>")
        )
        enable("editautoGate")
      } else {
        updateSelectInput(inputId = "editautoGate", label = "Edit")
        disable("editautoGate")
        reactautoNameChange$d <- NULL
      }
    }
  })
  
  observeEvent(input$editautoGate, {
    autoindex <-
      which(gs_get_pop_paths(ag$auto_gs) == reactautoNameChange$d)
    path <- gs_get_pop_paths(ag$auto_gs, path = 1)[autoindex]
    showModal(modalDialog(
      if (substr(path, 1, 1) != "Q" || substr(path, 3, 4) != ": ") {
        paste0("Please type a new name for autogate '", path, "':")
      } else{
        "Quadrant gates can't have their names changed."
      },
      br(),
      br(),
      if (substr(path, 1, 1) != "Q" || substr(path, 3, 4) != ": ") {
        textInput(inputId = "autonewNametextbox",
                  label = NULL,
                  width = "180px")
      },
      footer = list(
        if (substr(path, 1, 1) != "Q" || substr(path, 3, 4) != ": ") {
          actionButton(inputId = "autochangeNameModal",
                       label = "Ok",
                       style = ag$blackB)
        },
        actionButton(
          inputId = "autocancelModal",
          label = "Cancel",
          style = ag$whiteB
        ),
        actionButton(
          inputId = "autodeleteGate",
          label = "Delete Autogate",
          style = ag$blackB
        )
      ),
      easyClose = TRUE,
      size = "m"
    ))
  })
  
  observeEvent(input$autodeleteGate, {
    rootPlotLoad <- FALSE
    autopopPaths <- gs_get_pop_paths(ag$auto_gs, path = 1)
    index <-
      which(gs_get_pop_paths(ag$auto_gs) == reactautoNameChange$d)
    autogatetoDelete <- autopopPaths[index]
    i2 <- which(gs_get_pop_paths(ag$auto_gs) == reactautoPar$d)
    showingParent <- autopopPaths[i2]
    autopopsAfter <-
      gh_pop_get_descendants(ag$auto_gs[[1]], showingParent, path = 1)
    autopopsAfterIDs <- vapply(autopopsAfter, function(x)
      which(autopopPaths == x), integer(1))
    autopopsBefore <- autopopPaths[-autopopsAfterIDs]
    gs_pop_remove(ag$auto_gs, autogatetoDelete)
    if (autogatetoDelete == showingParent ||
        autogatetoDelete %in% autopopsBefore) {
      reactautoPar$d <- "root"
    }
    autoplotActivator$d <- autoplotActivator$d + 1
    autohierarchyActivator$d <-
      isolate(autohierarchyActivator$d) + 1
    removeModal()
    updateSelectInput(
      inputId = "editautoGate",
      label = HTML("<span style=
                                       'color: black'>Edit</span>")
    )
  })
  
  observeEvent(c(input$autonewNameTextBox, input$editautoGate), {
    if (!is.null(input$autonewNameTextBox)) {
      if (input$autonewNameTextBox == "") {
        disable("autochangeNameModal")
      } else {
        enable("autochangeNameModal")
      }
    }
  })
  
  observeEvent(input$autochangeNameModal, {
    autopopPaths <- gs_get_pop_paths(ag$auto_gs, path = 1)
    if (input$autonewNametextbox %in% autopopPaths) {
      alert("Please choose a different autogate name.")
    } else {
      index <-
        which(gs_get_pop_paths(ag$auto_gs) == reactautoNameChange$d)
      gs_pop_set_name(ag$auto_gs, autopopPaths[index], input$autonewNametextbox)
      removeModal()
      if (reactautoNameChange$d == reactautoPar$d) {
        reactautoPar$d <- paste0("/", input$autonewNametextbox)
      }
      autoplotActivator$d <- autoplotActivator$d + 1
    }
  })
  
  observeEvent(input$autodeleteAllModal, {
    for (i in ag$autofound) {
      gs_pop_remove(ag$auto_gs, i)
    }
    removeModal()
    autoplotActivator$d <- autoplotActivator$d + 1
  })
  
  observeEvent(input$autocancelModal, {
    reactautoAxisCustom$d <- NULL
    removeModal()
  })
  
  output$autoexportimagegates <-
    downloadHandler("autogate_hierarchy.png",
                    function(file) {
                      png(
                        file,
                        units = "in",
                        height = 6,
                        width = 6.44,
                        res = 300
                      )
                      par(mar = c(4, 6, 1, 1) + 0.1, lwd = 2)
                      plot(ag$autohplot)
                      dev.off()
                    })
  
  observeEvent(input$autoparentHelp, {
    showModal(
      modalDialog(
        title = "Help",
        "This sidebar shows the automatic gating hierarchy.",
        br(),
        br(),
        "The current parent is highlighted by a black line.",
        br(),
        br(),
        "Double clicking a parent box leads to a new autoplot with the
              sorted population.",
        br(),
        br(),
        "Clicking in a parent box selects it to allow for editing.",
        footer = NULL,
        size = "m",
        easyClose = TRUE
      )
    )
  })
  
  
  #Auto Ancestry----
  ##left----
  observeEvent(input$autobgType, {
    if (isolate(input$autobgType) == "Backgating"
        && length(gs_get_pop_paths(ag$auto_gs)) > 2) {
      enable("autobgPop")
    } else {
      updateSelectInput(inputId = "autobgPop", selected = "")
      disable("autobgPop")
    }
  })
  
  observeEvent(input$autobgPop, {
    if (input$autobgType == "Backgating") {
      autobgActivator$d <- isolate(autobgActivator$d) + 1
    }
  })
  
  ##main----
  output$autoancestry <- renderPlot({
    autobgActivator$d
    autoancestryGen(input$autobgType,
                input$autobgPreviewSample,
                isolate(input$autobgPop))
  }, height = 480, width = 800)
  
  output$autoexportImageAncestry <-
    downloadHandler("Auto Ancestry plots.png",
                    function(file) {
                      png(
                        file,
                        units = "in",
                        height = 6.6,
                        width = 11.05,
                        res = 300
                      )
                      autoancestryGen(input$autobgType,
                                  input$autobgPreviewSample,
                                  input$autobgPop)
                      dev.off()
                    })
  
  
  ##Auto Overlays----
  ###left----
  observeEvent(
    c(
      input$autoovTyp,
      input$autoovTon,
      input$autoovY,
      input$autoovX,
      input$autoovP
    ),
    {
      autosamples <- isolate(reactautoOvSamples$d)
      if (input$autoovTyp == "Overlaid histogram"
          || input$autoovTyp == "Offset histogram") {
        if (input$autoovY != "") {
          updateSelectInput(inputId = "autoovY", selected = "")
        } else {
          autoovActivator$d <- autoovActivator$d + 1
        }
        disable("autoovY")
        if (input$autoovTyp == "Overlaid histogram") {
          if (input$autoovX != "" && input$autoovP != "") {
            enable("autoovSamples")
            if (length(autosamples) > 2) {
              reactautoOvSamples$d <- autosamples[seq_len(3)]
            }
            if (length(autosamples) < 3) {
              autoshowOvPlot$d <- TRUE
            } else {
              autoshowOvPlot$d <- FALSE
            }
            autoshowOvPlot$d <- TRUE
          } else {
            disable("autoovSamples")
            autoshowOvPlot$d <- FALSE
          }
          if (input$autoovTon != "") {
            updateSelectInput(inputId = "autoovTon",
                              selected = "")
          } else {
            autoovActivator$d <- autoovActivator$d + 1
          }
          disable("autoovTon")
        } else {
          if (input$autoovTon != ""
              && input$autoovX != ""
              && input$autoovP != "") {
            enable("autoovSamples")
            autoovActivator$d <- autoovActivator$d + 1
            autoshowOvPlot$d <- TRUE
          } else {
            disable("autoovSamples")
            autoshowOvPlot$d <- FALSE
          }
          enable("autoovTon")
          autoovActivator$d <- autoovActivator$d + 1
        }
      } else {
        if (length(autosamples) > 2) {
          reactautoOvSamples$d <- autosamples[seq_len(2)]
        }
        if (input$autoovTyp != ""
            && input$autoovTon != ""
            && input$autoovY != ""
            && input$autoovX != ""
            && input$autoovP != "") {
          enable("autoovSamples")
          autoovActivator$d <- autoovActivator$d + 1
          autoshowOvPlot$d <- TRUE
        } else {
          disable("autoovSamples")
          autoshowOvPlot$d <- FALSE
        }
        enable("autoovTon")
        enable("autoovY")
        autoovActivator$d <- autoovActivator$d + 1
      }
    }
  )
  
  observeEvent(input$autoovSamples, {
    autosampAlert$d <- ""
    updateCheckboxGroupInput(inputId = "autosamplecheckbox",
                             selected = isolate(reactautoOvSamples$d))
    if (input$autoovTyp == "Dot plot") {
      modaltitle <- "Select 2 autosamples for the Dot plot."
    } else if (input$autoovTyp == "Overlaid histogram") {
      modaltitle <- "Select 2-3 autosamples for the Overlaid histogram."
    } else {
      modaltitle <- "Select 2-10 autosamples for the Offset histogram."
    }
    showModal(
      modalDialog(
        tags$style("#selectedsamplealert{color: red}"),
        strong(modaltitle),
        checkboxGroupInput(
          "autosamplecheckbox",
          label = "",
          choices = ag$sampleList,
          width = "100%"
        ),
        footer = fluidRow(
          column(
            width = 8,
            align = "left",
            textOutput("selectedsamplealert")
          ),
          column(
            width = 4,
            align = "right",
            list(
              actionButton(
                inputId = "autoovokbutton",
                label = "Ok",
                style = ag$blackB
              ),
              actionButton(
                inputId = "cancelModal",
                label = "Cancel",
                style = ag$whiteB
              )
            ),
          )
        ),
        easyClose = TRUE,
        size = "m"
      )
    )
    disable("autoovokbutton")
  })
  
  observeEvent(input$autosamplecheckbox, {
    len <- length(input$autosamplecheckbox)
    if (input$autoovTyp == "Dot plot") {
      if (len == 2) {
        autosampAlert$d <- ""
        enable("autoovokbutton")
      } else {
        if (len > 2) {
          autosampAlert$d <- "Only 2 autosamples are allowed for Dot plots."
        } else {
          autosampAlert$d <- ""
        }
        disable("autoovokbutton")
      }
    } else if (input$autoovTyp == "Overlaid histogram") {
      autosampAlert$d <- "Up to 3 autosamples are allowed for
            Overlaid histograms."
      if (len == 2 || len == 3) {
        autosampAlert$d <- ""
        enable("autoovokbutton")
      } else {
        if (len > 3) {
          autosampAlert$d <- "Up to 3 autosamples are allowed for
                    Overlaid histograms."
        } else {
          autosampAlert$d <- ""
        }
        disable("autoovokbutton")
      }
    } else {
      autosampAlert$d <- "Up to 10 autosamples are allowed for
            Overlaid histograms."
      if (len > 1 && len < 11) {
        autosampAlert$d <- ""
        enable("autoovokbutton")
      } else {
        if (len > 10) {
          autosampAlert$d <- "Up to 10 autosamples are allowed for
                    Overlaid histograms."
        } else {
          autosampAlert$d <- ""
        }
        disable("autoovokbutton")
      }
    }
  })
  
  output$selectedsamplealert <- renderText({
    autosampAlert$d
  })
  
  observeEvent(input$autoovokbutton, {
    autoovActivator$d <- autoovActivator$d + 1
    reactautoOvSamples$d <- input$autosamplecheckbox
    removeModal()
  })
  
  observeEvent(input$autoodisplayOptOV, {
    t3 <- "[for=autoovdisplayaxisfont]+span>.irs>.irs-single, "
    t4 <- "[for=autoovdisplayaxisfont]"
    showModal(
      modalDialog(
        tags$style(HTML(paste0(
          t3, t4, ag$sliderCol
        ))),
        checkboxInput(
          "autoovdisplayaxis",
          label = "Show axis titles",
          value = reactautoOvShowAxis$d
        ),
        sliderInput(
          "autoovdisplayaxisfont",
          label = NULL,
          min = 10,
          max = 25,
          value = reactautoOvAxisFont$d,
          ticks = FALSE
        ),
        footer = list(
          actionButton(
            inputId = "autoovreloadplotmodal",
            label = "Ok",
            style = ag$blackB
          ),
          actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = TRUE,
        size = "s"
      )
    )
  })
  
  observeEvent(input$autoovreloadplotmodal, {
    autoovActivator$d <- autoovActivator$d + 1
    removeModal()
    reactautoOvShowAxis$d <- input$autoovdisplayaxis
    reactautoOvAxisFont$d <- input$autoovdisplayaxisfont
  })
  
  observeEvent(c(input$autoovdisplayaxis, input$autoodisplayOptOV) , {
    if (length(input$autoovdisplayaxis) != 0) {
      if (input$autoovdisplayaxis == TRUE) {
        shinyjs::show("autoovdisplayaxisfont")
      } else {
        hide("autoovdisplayaxisfont")
      }
    }
  })
  
  ##main----
  output$autooverlays <- renderPlot({
    autoovActivator$d
    if (!is.null(reactautoOvSamples$d)) {
      if (isolate(autoshowOvPlot$d) == TRUE) {
        enable("autoodisplayOptOV")
        enable("autoovSampleOrder")
        enable("autoexportImageOverlay")
        if (isolate(input$autoovP) == "ungated") {
          autocurrentParent <- "root"
        } else {
          autocurrentParent <- isolate(input$autoovP)
        }
        ag$autoovBoolean <- TRUE
        ID <- match(isolate(reactautoOvSamples$d), ag$fileList)
        autooverlay(
          ID,
          isolate(input$autoovX),
          isolate(input$autoovY),
          autocurrentParent,
          isolate(input$autoovTyp),
          isolate(input$autoovTon),
          isolate(reactautoOvShowAxis$d),
          isolate(reactautoOvAxisFont$d)
        )
      } else {
        ag$ovBoolean <- FALSE
        disable("autoodisplayOptOV")
        disable("autoovSampleOrder")
        disable("autoexportImageOverlay")
      }
    } else {
      ag$ovBoolean <- FALSE
      disable("autoodisplayOptOV")
      disable("autoovSampleOrder")
      disable("autoexportImageOverlay")
    }
  }, height = 423, width = 800)
  
  observeEvent(input$autoovSampleOrder, {
    reactModalTitle$d <- 1
    reactautoSampleOrder$d <- NULL
    choices <-
      showModal(
        modalDialog(
          strong(textOutput("ordermodaltitle")),
          radioButtons(
            "autoorderradiobutton",
            label = "",
            choices = rev(isolate(reactautoOvSamples$d)),
            width = "100%",
            selected = ""
          ),
          footer = actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          ),
          easyClose = TRUE,
          size = "m"
        )
      )
  })
  
  output$ordermodaltitle <- renderText({
    paste0("Select sample number ",
           reactModalTitle$d,
           " (from top to bottom).")
  })
  
  observeEvent(input$autoorderradiobutton, {
    reactModalTitle$d <- reactModalTitle$d + 1
    ID <- match(input$autoorderradiobutton, reactautoOvSamples$d)
    for (i in seq(input$autoorderradiobutton)) {
      reactautoSampleOrder$d <- append(reactautoSampleOrder$d, ID)
    }
    if (length(reactautoSampleOrder$d) != length(reactautoOvSamples$d)) {
      samplestoShow <- reactautoOvSamples$d
      for (i in rev(sort(reactautoSampleOrder$d))) {
        preShow <- samplestoShow != samplestoShow[i]
        samplestoShow <- samplestoShow[preShow]
      }
      updateRadioButtons(
        inputId = "autoorderradiobutton",
        choices = rev(samplestoShow),
        selected = ""
      )
    } else {
      reactautoOvSamples$d <-
        rev(reactautoOvSamples$d[reactautoSampleOrder$d])
      autoovActivator$d <- autoovActivator$d + 1
      removeModal()
    }
  })
  
  output$autoexportImageOverlay <- downloadHandler(function() {
    paste0(input$autoovTyp, ".png")
  },
  function(file) {
    png(
      file,
      units = "in",
      height = 5.8,
      width = 11.05,
      res = 300
    )
    if (isolate(input$autoovP) == "ungated") {
      autocurrentParent <- "root"
    } else {
      autocurrentParent <- isolate(input$autoovP)
    }
    ID <- match(isolate(reactautoOvSamples$d), ag$fileList)
    overlay(
      ID,
      isolate(input$autoovX),
      isolate(input$autoovY),
      autocurrentParent,
      isolate(input$autoovTyp),
      isolate(input$autoovTon),
      isolate(reactautoOvShowAxis$d),
      isolate(reactautoOvAxisFont$d)
    )
    dev.off()
  })
  
  
  # #Proliferation----
  # #left----
  #  observeEvent(input$applyProlif, {
  #    prolifReady$d <- TRUE
  #  })
  #
  #  observeEvent(input$step1, {
  #    showModal(modalDialog(
  #      title = "Step 1 help",
  #      "Follow the instructions to start the proliferation tool.",
  #      footer=NULL,
  #      easyClose=TRUE,
  #      size="s"))
  #  })
  #  observeEvent(input$step2, {
  #    showModal(modalDialog(
  #      title = "Step 2 help",
  #      "Navigate through samples and choose the best fitting model
  #            to apply.",
  #      footer=NULL,
  #      easyClose=TRUE,
  #      size="s"))
  #  })
  #  observeEvent(input$step3, {
  #    showModal(modalDialog(
  #      title = "Step 3 help",
  #      "You can use the generated results or return to step 2
  #            to apply a new model.",
  #      footer=list(
  #        actionButton(inputId="goToStep2", label="Apply a new model",
  #                     style=ag$blackB),
  #        actionButton(inputId="cancelModal", label="Cancel",
  #                     style=ag$whiteB)
  #      ),
  #      easyClose=TRUE,
  #      size="s"))
  #  })
  #  observeEvent(input$goToStep2, {
  #    prolifReady$d <- FALSE
  #    removeModal()
  #  })
  #
  #  ##main----
  #  output$prolifStart <- renderUI({
  #    l0 <- "This tool automates the detection and quantification of
  #        division peaks from cell proliferation assays."
  #    l1 <- "To initialize the proliferation tool:"
  #    l2 <- "-go to the Plot tab;"
  #    l3 <- "-select the desired parent;"
  #    l4 <- "-select the proliferation dye in the X axis
  #        (i.e. FITC-A :: CFSE);"
  #    l5 <- "-select 'Histogram' in 'Plot type';"
  #    l6 <- "-make a narrow gate (that does not reach the X axis limit)
  #        on any sample for undivided cells (brightest peak);"
  #    l7 <- "-certify that the population is named as 'undivided';"
  #    l8 <- "-return to this tab."
  #    HTML("<br/>", paste(strong(l0)), "<br/>", "<br/>",
  #         paste(strong(l1),l2,l3,l4,l5,l6,l7,l8, sep="<br/>"))
  #  })
  #
  #  observeEvent(input$nextProlifS, {
  #    ID <- match(input$prolifSButt, ag$sampleList)
  #    if(ID + 1 <= length(ag$sampleList)) {
  #      updateSelectInput(inputId="prolifSButt",
  #                        selected=ag$sampleList[[ID + 1]])
  #    }
  #  })
  #  observeEvent(input$prevProlifS, {
  #    ID <- match(input$prolifSButt, ag$sampleList)
  #    if(ID > 1) {
  #      updateSelectInput(inputId="prolifSButt",
  #                        selected=ag$sampleList[[ID - 1]])
  #    }
  #  })
  #  observeEvent(c(input$prolifSButt, input$nextProlifS, input$prevProlifS), {
  #    if(input$prolifSButt != "") {
  #      ID <- match(input$prolifSButt, ag$sampleList)
  #      if(ID >= length(ag$sampleList)) {
  #        disable("nextProlifS")
  #      } else {
  #        enable("nextProlifS")
  #      }
  #      if(ID <= 1) {
  #        disable("prevProlifS")
  #      } else {
  #        enable("prevProlifS")
  #      }
  #    }
  #  })
  #
  #  output$prolifPlot <- renderPlot({
  #    input$tabs
  #    popPaths <- gs_get_pop_paths(ag$gs, path=1)
  #    if("undivided" %in% popPaths) {
  #      prolifGen(input$prolifSButt, isolate(reactShowAxis$d),
  #                isolate(reactAxisFont$d), prolifReady$d,
  #                input$prolifLabel, input$prolifGrid)
  #    } else {
  #      prolifOff()
  #      hide("prolifPlot")
  #      hide("applyProlif")
  #      disable("step2")
  #      hide("prolifSButt")
  #      hide("prevProlifS")
  #      hide("nextProlifS")
  #      shinyjs::show("prolifStart")
  #      enable("step1")
  #    }
  #  }, height=440, width=470)
  #
  #  output$exportImageProlif <- downloadHandler(
  #    function() {
  #      paste0(substr(input$prolifSButt, 1, nchar(input$prolifSButt) - 4),
  #             ".png")
  #    },
  #    function(file) {
  #      png(file, units="in", height=6, width=6.44, res=300)
  #      popPaths= gs_get_pop_paths(ag$gs, path=1)
  #      if("undivided" %in% popPaths) {
  #        prolifGen(input$prolifSButt, isolate(reactShowAxis$d),
  #                  isolate(reactAxisFont$d), prolifReady$d,
  #                  input$prolifLabel, input$prolifGrid)
  #      }
  #      dev.off()
  #    }
  #  )
  #
  #  ##right----
  #  output$prolifTable <- renderUI({
  #    input$tabs
  #    if(prolifReady$d == TRUE) {
  #      prolifTableGen(input$prolifSButt)
  #    }
  # })
  #
  #  output$exportTableProlif <- downloadHandler(
  #    "Proliferation results.csv",
  #    function(file) {
  #      colNames = c("Number of divisions", "Divided cells",
  #                   "Undivided cells")
  #      for(i in 2:nrow(ag$refPeaks)) {
  #        colNames[i+2] = paste0("Division ", (i - 1))
  #    }
  #      colNames[length(colNames) + 1] = "Percent divided"
  #      colNames[length(colNames) + 1] = "Percent undivided"
  #      constantColLenght = length(colNames)
  #      for(i in 2:nrow(ag$refPeaks)) {
  #        pasteString = paste0("Percent division ", (i - 1))
  #        colNames[i+(constantColLenght-1)] = pasteString
  #      }
  #      completeTable = data.frame(matrix("", ncol=length(colNames),
  #                                        nrow=length(ag$sampleList)))
  #      for(j in seq(ag$sampleList)) {
  #        cs = gs_pop_get_data(ag$gs[ag$sampleList[j]], ag$prolifParent)
  #        df = as.data.frame.array(cytoframe_to_flowFrame(cs[[1]])@exprs)
  #        dfX = df[,ag$prolifChannel]
  #        ag$divPercents = c()
  #        ag$divCounts = c()
  #        ag$total = length(dfX)
  #        for(i in seq(ag$gatecoords)) {
  #          filter = dfX[dfX > ag$gatecoords[[i]][1]]
  #          filter = filter[filter < ag$gatecoords[[i]][2]]
  #          ag$divCounts[i] = length(filter)
  #          ag$divPercents[i] = length(filter)*100/ag$total
  #        }
  #        ag$divCounts = rev(ag$divCounts)
  #        round = round(ag$divPercents, 2)
  #        ag$divPercents = rev(as.numeric(format(round, nsmall=2)))
  #        ag$totalDivCount = ag$total-ag$divCounts[1]
  #        ag$totalDivPerc = 100-ag$divPercents[1]
  #        columnValues = c((nrow(ag$refPeaks) - 1),
  #                         ag$totalDivCount, ag$divCounts[1])
  #        for(i in 2:nrow(ag$refPeaks)) {
  #          columnValues[i+2] = ag$divCounts[i]
  #        }
  #        ID = length(columnValues) + 1
  #        round = round(ag$totalDivPerc, 2)
  #        columnValues[ID] = as.numeric(format(round, nsmall=2))
  #        columnValues[ID+1] = ag$divPercents[1]
  #        constantColLenght = length(columnValues)
  #      for(i in 2:nrow(ag$refPeaks)) {
  #          columnValues[i+(constantColLenght-1)] = ag$divPercents[i]
  #        }
  #        completeTable[j,] = columnValues
  #      }
  #      colnames(completeTable) = colNames
  #      rownames(completeTable) = ag$sampleList
  #      write.csv(completeTable, file)
  #    }
  #  )
  
  #t-SNE----
  ##left----
  observeEvent(input$tSNEGroups, {
    if (length(reactTSSamp$d) != input$tSNEGroups) {
      reactTSSamp$d <- NULL
    }
  })
  
  observeEvent(input$tSNESamples, {
    sampleCol <- list()
    for (i in seq(ag$sampleList)) {
      sampleCol[[i]] <- list(br(), br(), ag$sampleList[i])
    }
    groupCols <- list()
    for (i in seq(input$tSNEGroups)) {
      groupCols[[i]] <- list()
    }
    if (!is.null(reactTSSamp$d)) {
      for (j in seq(input$tSNEGroups)) {
        for (i in seq(ag$sampleList)) {
          groupCols[[j]][[i]] <- checkboxInput(
            inputId = paste0("group", j, "samp", i),
            label = "",
            value = reactTSSamp$d[[j]][[i]]
          )
        }
      }
    } else {
      for (j in seq(input$tSNEGroups)) {
        for (i in seq(ag$sampleList)) {
          groupCols[[j]][[i]] <-
            checkboxInput(inputId = paste0("group", j, "samp", i),
                          label = "")
        }
      }
    }
    checkBoxCols <- list()
    for (i in seq(input$tSNEGroups)) {
      if (!is.null(reactGroupNames$d)
          && length(reactGroupNames$d) == input$tSNEGroups) {
        textGroup <- textInput(paste0("textGroup", i),
                               label = "",
                               value = reactGroupNames$d[[i]])
      } else {
        textGroup <- textInput(paste0("textGroup", i), label = "")
      }
      butts <-
        list(strong(paste("Group", i)), textGroup, groupCols[[i]])
      checkBoxCols[[i]] <- column(width = 1,
                                  align = "center",
                                  style = "margin-top: 10px;",
                                  butts)
    }
    showModal(modalDialog(
      strong("Select samples by group for concatenation."),
      br(),
      fluidRow(column(
        width = 5,
        align = "right",
        list(br(), br(), br(), br(), sampleCol)
      ),
      checkBoxCols),
      footer = list(
        actionButton(
          inputId = "tsneoksampbutton",
          label = "Ok",
          style = ag$blackB
        ),
        actionButton(
          inputId = "cancelModal",
          label = "Cancel",
          style = ag$whiteB
        )
      ),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  observeEvent(input$tsneoksampbutton, {
    reactTSSamp$d <- NULL
    reactGroupNames$d <- NULL
    for (i in seq(input$tSNEGroups)) {
      reactGroupNames$d[[i]] <- input[[paste0("textGroup", i)]]
      reactTSSamp$d[[i]] <- list()
    }
    for (j in seq(input$tSNEGroups)) {
      for (i in seq(ag$sampleList)) {
        index <- paste0("group", j, "samp", i)
        reactTSSamp$d[[j]][[i]] <- input[[index]]
      }
    }
    ag$concatSamples <- list()
    for (j in seq(input$tSNEGroups)) {
      ag$concatSamples[[j]] <- list()
    }
    for (j in seq(input$tSNEGroups)) {
      ag$concatSamples[[j]] <- which(reactTSSamp$d[[j]] == TRUE)
    }
    if (length(ag$concatSamples[[1]]) == 0) {
      reactTSSamp$d <- NULL
      reactGroupNames$d <- NULL
    } else {
      for (i in seq(ag$concatSamples)) {
        ag$concatSamples[[i]] <- sapply(ag$concatSamples[[i]], function(x)
          which(ag$fileList == ag$sampleList[x]))
      }
    }
    lens <- lapply(ag$concatSamples, function(x)
      length(x) == 0)
    if (length(grep(TRUE, unlist(lens))) > 0) {
      ag$concatSamples <- list()
      alert("Select at least one sample per group.")
    } else {
      if (is.null(reactGroupNames$d)) {
        ag$concatSamples <- list()
        alert("Please type the name of each group in the
                corresponding text boxes.")
      } else {
        names <- sapply(reactGroupNames$d, function(x)
          x != "")
        len <- length(reactGroupNames$d)
        if (length(grep(TRUE, names)) < length(ag$concatSamples)) {
          ag$concatSamples <- list()
          alert("Please type the name of each group in the
                    corresponding text boxes.")
        } else if (len > 1
                   && length(unique(reactGroupNames$d)) < len) {
          alert("Group names cannot be identical.")
        } else {
          removeModal()
        }
      }
    }
  })
  
  observeEvent(input$tSNEParameters, {
    updateCheckboxGroupInput(inputId = "tsneParCheckBox",
                             selected = isolate(reactTSCh$d))
    choices <- unlist(ag$ch)[unlist(ag$ch) != "Time"]
    showModal(
      modalDialog(
        strong("Select at least 2 parameters to include in the analysis"),
        checkboxGroupInput(
          "tsneParCheckBox",
          label = "",
          choices = choices,
          width = "100%"
        ),
        footer = list(
          actionButton(
            inputId = "tsneOkParButt",
            label = "Ok",
            style = ag$blackB
          ),
          actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  observeEvent(input$tsneOkParButt, {
    if (length(input$tsneParCheckBox) < 2) {
      alert("Please select at least 2 parameters.")
    } else {
      reactTSCh$d <- input$tsneParCheckBox
      removeModal()
    }
  })
  
  observeEvent(c(
    reactTSSamp$d,
    input$tSPar,
    reactTSCh$d,
    input$tSEvs,
    input$tabs
  ),
  {
    if (!is.null(reactTSSamp$d)
        && input$tSPar != ""
        && !is.null(reactTSCh$d)
        && input$tSEvs > 0) {
      enable("tSNEGenerate")
    } else {
      disable("tSNEGenerate")
    }
  })
  
  observeEvent(input$tSNEGenerate, {
    if (is.null(ag$entiretSNE)) {
      events <- as.numeric(input$tSEvs)
      verifier <- sapply(unlist(ag$concatSamples), function(x)
        nrow(gs_pop_get_data(ag$gs[x], input$tSPar)[[1]]) >= events)
      if (all(verifier)) {
        len <- length(unlist(ag$concatSamples))
        round <- round(len * events / 1000 * 2.5 / 60, 1)
        minutes <- as.numeric(format(round, nsmall = 1))
        showModal(
          modalDialog(
            "The t-SNE tool might require longer processing time
                    than other tools in this software.",
            br(),
            br(),
            "Before proceeding, make sure that your CPU is not
                    being used by heavy processes.",
            br(),
            br(),
            "Concatenation of the selected samples/number of
                    events and t-SNE generation are estimated to take ",
            strong(minutes, "-", minutes * 2, " minutes."),
            br(),
            br(),
            textOutput("question"),
            footer = list(
              actionButton(
                inputId = "tSNEGenOk",
                label = "Generate t-SNE",
                style = ag$blackB
              ),
              actionButton(
                inputId = "cancelModal",
                label = "Cancel",
                style = ag$whiteB
              )
            ),
            easyClose = FALSE,
            size = "m"
          )
        )
      } else {
        nums <- sapply(unlist(ag$concatSamples), function(x)
          nrow(gs_pop_get_data(ag$gs[x], input$tSPar)[[1]]))
        lowerNumber <- min(nums)
        if (lowerNumber < 1000) {
          alert(
            "There are not enough events (<1K) in at least
                    one of the selected samples/parent.
                    Try to change the samples or the Parent."
          )
        } else {
          if (lowerNumber < 5000) {
            maxEvents <- "1K"
          } else if (lowerNumber < 10000) {
            maxEvents <- "5K"
          } else if (lowerNumber < 25000) {
            maxEvents <- "10K"
          } else if (lowerNumber < 50000) {
            maxEvents <- "25K"
          }
          alert(
            paste(
              "At least one selected samples/parent
                    don't have enough events. For the selected
                                samples/parent, only",
              maxEvents,
              "events are possible."
            )
          )
        }
      }
    } else {
      showModal(
        modalDialog(
          "Do you want to make a new t-SNE?",
          footer = list(
            actionButton(
              inputId = "tsnemakenew",
              label = "Make new t-SNE",
              style = ag$blackB
            ),
            actionButton(
              inputId = "cancelModal",
              label = "Cancel",
              style = ag$whiteB
            )
          ),
          easyClose = TRUE,
          size = "m"
        )
      )
    }
  })
  
  output$question <- renderText("Do you want to proceed?")
  
  observeEvent(input$tsnemakenew, {
    ag$entiretSNE <- NULL
    hidetSNE()
    disable("tSNEGenerate")
    tSPlotActiv$d <- tSPlotActiv$d + 1
    removeModal()
  })
  
  ##main----
  observeEvent(input$tSNEGenOk, {
    setwd(ag$filePath)
    reactTSPar$d <- isolate(input$tSPar)
    pops <- gh_pop_get_descendants(ag$gs[[1]], isolate(input$tSPar),
                                   path = 1)
    for (i in seq(length(unlist(ag$concatSamples)))) {
      index <- unlist(ag$concatSamples)[i]
      cs <- gs_pop_get_data(ag$gs[index], "root")
      ff <- cytoframe_to_flowFrame(cs[[1]])@exprs
      if (i == 1) {
        preConcat <- as.data.frame.array(ff)
        for (k in c(1, seq(pops) + 1)) {
          if (k < length(c(1, seq(pops) + 1))) {
            IDs <- gh_pop_get_indices(ag$gs[[index]],
                                      pops[k])
            preConcat <- cbind2(preConcat, IDs)
          } else {
            IDs <- gh_pop_get_indices(ag$gs[[index]],
                                      isolate(input$tSPar))
            preConcat <- cbind2(preConcat, IDs)
          }
        }
        pre <- which(preConcat[ncol(preConcat)][[1]] == TRUE)
        preConcat <- preConcat[pre,]
        preConcat <- preConcat[,-length(preConcat)]
        pre <- sample.int(nrow(preConcat),
                          as.numeric(isolate(input$tSEvs)))
        preConcat <- preConcat[pre,]
      } else {
        tempConcat <- as.data.frame.array(ff)
        for (k in c(1, seq(pops) + 1)) {
          if (k < length(c(1, seq(pops) + 1))) {
            IDs <- gh_pop_get_indices(ag$gs[[index]],
                                      pops[k])
            tempConcat <- cbind2(tempConcat, IDs)
          } else {
            IDs <- gh_pop_get_indices(ag$gs[[index]],
                                      isolate(input$tSPar))
            tempConcat <- cbind2(tempConcat, IDs)
          }
        }
        temp <- which(tempConcat[ncol(tempConcat)][[1]] == TRUE)
        tempConcat <- tempConcat[temp,]
        tempConcat <- tempConcat[,-length(tempConcat)]
        temp <- sample.int(nrow(tempConcat),
                           as.numeric(isolate(input$tSEvs)))
        tempConcat <- tempConcat[temp,]
        preConcat <- rbind2(preConcat, tempConcat)
      }
    }
    for (i in seq(ag$fluoCh)) {
      trans <- flowjo_biexp(
        channelRange = 4096,
        maxValue = 262144,
        pos = ag$customAxis[[i]][1],
        neg = ag$customAxis[[i]][2],
        widthBasis = ag$customAxis[[i]][3],
        inverse = TRUE
      )
      preConcat[, ag$fluoCh[i]] <- trans(preConcat[, ag$fluoCh[i]])
    }
    ag$concat <- NULL
    for (i in seq(isolate(input$tsneParCheckBox))) {
      if (i == 1) {
        ag$concat <- preConcat[, isolate(input$tsneParCheckBox)[1]]
      } else {
        index <- isolate(input$tsneParCheckBox)[i]
        ag$concat <- cbind2(ag$concat, preConcat[, index])
      }
    }
    for (i in rev(ncol(preConcat) - (seq(pops) - 1))) {
      ag$concat <- cbind2(ag$concat, preConcat[, i])
    }
    ag$concat <- as.data.frame(ag$concat)
    colnames(ag$concat) <- c(isolate(input$tsneParCheckBox), pops)
    hide("tSNEGenOk")
    hide("cancelModal")
    hide("question")
    withProgress(
      message = "Generating t-SNE...",
      detail = "",
      value = 0,
      max = 100,
      {
        len <- length(isolate(input$tsneParCheckBox))
        index <- seq_len(len)
        df <- as.data.frame(ag$concat[, index])
        ag$entiretSNE <-
          Rtsne(df, check_duplicates = FALSE,
                verbose = TRUE)
        ag$entiretSNE <- ag$entiretSNE$Y
      }
    )
    showtSNE()
    removeModal()
    ag$tSNEListofGroups <- list()
    for (i in seq(ag$concatSamples)) {
      ag$tSNEListofGroups[[i]] <- i
      names(ag$tSNEListofGroups)[[i]] <- reactGroupNames$d[[i]]
    }
    ag$tSNEListofSamples <- list()
    for (i in seq(length(unlist(ag$concatSamples)))) {
      ag$tSNEListofSamples[[i]] <- i
      index <- unlist(ag$concatSamples)[i]
      names(ag$tSNEListofSamples)[[i]] <- ag$fileList[index]
    }
    if (length(ag$tSNEListofGroups) < 2
        && length(ag$tSNEListofSamples) < 2) {
      choices <- c("Heatmap", "Overlay Populations")
      updateSelectInput(inputId = "tSNEMode", choices = choices)
    } else {
      choices <- c("Heatmap",
                   "Overlay Groups or Samples",
                   "Overlay Populations")
      updateSelectInput(inputId = "tSNEMode", choices = choices)
    }
    tempChannels <- unlist(ag$ch)[unlist(ag$ch) != "Time"]
    ag$availableparameters <- list()
    for (i in seq(input$tsneParCheckBox)) {
      inp <- input$tsneParCheckBox[i]
      ag$availableparameters[[i]] <- which(tempChannels == inp)
    }
    ag$availableparameters <-
      tempChannels[unlist(ag$availableparameters)]
    updateSelectInput(inputId = "tSHighl",
                      choices =
                        c(ag$availableparameters))
    updateSelectInput(inputId = "tSNEMode", selected = "Heatmap")
    updateSelectInput(inputId = "tSGroupOrSamp", selected = "All")
    updateSelectInput(inputId = "tSGroupOrSampID", selected = "")
    tSPlotActiv$d <- tSPlotActiv$d + 1
  })
  
  observeEvent(input$tSGroupOrSamp, {
    if (exists("concatSamples", envir = ag)) {
      if (input$tSNEMode == "Heatmap"
          || input$tSNEMode == "Overlay Populations") {
        if (input$tSGroupOrSamp == "All") {
          disable("tSGroupOrSampID")
          updateSelectInput(
            inputId = "tSGroupOrSampID",
            choices = c(""),
            selected = ""
          )
        } else {
          enable("tSGroupOrSampID")
          if (input$tSGroupOrSamp == "Group") {
            updateSelectInput(
              inputId = "tSGroupOrSampID",
              choices = ag$tSNEListofGroups,
              selected = 1
            )
            if (isolate(input$tSGroupOrSampID) == 1) {
              tSPlotActiv$d <- tSPlotActiv$d + 1
            }
          } else if (input$tSGroupOrSamp == "Sample") {
            updateSelectInput(
              inputId = "tSGroupOrSampID",
              choices = ag$tSNEListofSamples,
              selected = 1
            )
            if (isolate(input$tSGroupOrSampID) == 1) {
              tSPlotActiv$d <- tSPlotActiv$d + 1
            }
          }
        }
      } else if (input$tSNEMode == "Overlay Groups or Samples") {
        if (input$tSGroupOrSamp != "All") {
          enable("tSGroupOrSampIDs")
        } else {
          disable("tSGroupOrSampIDs")
        }
        reacttSIDs$d <- NULL
        tSPlotActiv$d <- tSPlotActiv$d + 1
      }
    }
  })
  
  observeEvent(input$tSNEMode, {
    if (!is.null(ag$entiretSNE)) {
      if (input$tSNEMode == "Heatmap"
          || input$tSNEMode == "Overlay Populations") {
        if (input$tSGroupOrSamp != "All") {
          if (input$tSGroupOrSamp == "Group") {
            choices <- ag$tSNEListofGroups
          } else if (input$tSGroupOrSamp == "Sample") {
            choices <- ag$tSNEListofSamples
          }
          if (input$tSGroupOrSampID != 1) {
            updateSelectInput(
              inputId = "tSGroupOrSampID",
              choices = choices,
              selected = 1
            )
          } else {
            tSPlotActiv$d <- tSPlotActiv$d + 1
          }
          enable("tSGroupOrSampID")
        } else {
          if (input$tSGroupOrSampID != "") {
            updateSelectInput(
              inputId = "tSGroupOrSampID",
              choices = c(""),
              selected = ""
            )
          } else {
            tSPlotActiv$d <- tSPlotActiv$d + 1
          }
          disable("tSGroupOrSampID")
        }
      } else if (input$tSNEMode == "Overlay Groups or Samples") {
        if (input$tSGroupOrSamp != "All") {
          enable("tSGroupOrSampIDs")
        } else {
          disable("tSGroupOrSampIDs")
        }
      }
      if (input$tSNEMode == "Heatmap") {
        hide("tSNEPopulations")
        hide("tSGroupOrSampIDs")
        shinyjs::show("tSGroupOrSampID")
        shinyjs::show("tSGroupOrSamp")
        shinyjs::show("tSHighl")
      } else if (input$tSNEMode == "Overlay Groups or Samples") {
        hide("tSHighl")
        hide("tSGroupOrSampID")
        hide("tSNEPopulations")
        shinyjs::show("tSGroupOrSampIDs")
        reacttSIDs$d <- NULL
        tSPlotActiv$d <- tSPlotActiv$d + 1
      } else if (input$tSNEMode == "Overlay Populations") {
        hide("tSHighl")
        hide("tSGroupOrSampIDs")
        shinyjs::show("tSGroupOrSampID")
        shinyjs::show("tSNEPopulations")
      }
    }
  })
  
  observeEvent(input$tSGroupOrSampIDs, {
    if (is.null(reacttSIDs$d)) {
      if (input$tSGroupOrSamp == "Group") {
        listofgrouporsamples <- ag$tSNEListofGroups
      } else if (input$tSGroupOrSamp == "Sample") {
        listofgrouporsamples <- ag$tSNEListofSamples
      }
    } else {
      if (input$tSGroupOrSamp == "Group") {
        listofgrouporsamples <- ag$tSNEListofGroups
      } else if (input$tSGroupOrSamp == "Sample") {
        listofgrouporsamples <- ag$tSNEListofSamples
      }
    }
    showModal(
      modalDialog(
        "Select 2 groups/samples",
        checkboxGroupInput(
          "tsneIDs",
          label = "",
          choices = listofgrouporsamples,
          selected = reacttSIDs$d
        ),
        footer = list(
          actionButton(
            inputId = "tSNESetIDs",
            label = "Ok",
            style = ag$blackB
          ),
          actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = TRUE,
        size = "m"
      )
    )
    if (length(input$tsneIDs) == 0) {
      disable("tSNESetIDs")
    } else {
      if (length(input$tsneIDs) != 2) {
        disable("tSNESetIDs")
      } else {
        enable("tSNESetIDs")
      }
    }
  })
  
  observeEvent(input$tsneIDs, {
    if (input$tsneIDs[1] == "") {
      disable("tSNESetIDs")
    } else {
      if (length(input$tsneIDs) != 2) {
        disable("tSNESetIDs")
      } else {
        enable("tSNESetIDs")
      }
    }
  })
  
  observeEvent(input$tSNESetIDs, {
    reacttSIDs$d <- input$tsneIDs
    removeModal()
  })
  
  observeEvent(input$tSNEPopulations, {
    popOptions <-
      gh_pop_get_descendants(ag$gs[[1]], reactTSPar$d, path = 1)
    showModal(
      modalDialog(
        "Select at least 2-8 populations.",
        checkboxGroupInput(
          "tSNEPopIDs",
          label = "",
          choices = popOptions,
          selected = reacttSNEPops$d
        ),
        footer = list(
          actionButton(
            inputId = "tSNESetPopIDs",
            label = "Ok",
            style = ag$blackB
          ),
          actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = TRUE,
        size = "m"
      )
    )
    if (length(input$tSNEPopIDs) == 0) {
      disable("tSNESetPopIDs")
    } else {
      if (length(input$tSNEPopIDs) < 2 && length(input$tSNEPopIDs) > 8) {
        disable("tSNESetPopIDs")
      } else {
        enable("tSNESetPopIDs")
      }
    }
  })
  
  observeEvent(input$tSNEPopIDs, {
    if (input$tSNEPopIDs[1] == "") {
      disable("tSNESetPopIDs")
    } else {
      if (length(input$tSNEPopIDs) < 2 && length(input$tSNEPopIDs) > 8) {
        disable("tSNESetPopIDs")
      } else {
        enable("tSNESetPopIDs")
      }
    }
  })
  
  observeEvent(input$tSNESetPopIDs, {
    reacttSNEPops$d <- input$tSNEPopIDs
    removeModal()
  })
  
  output$tSNEPlot <- renderPlot({
    tSPlotActiv$d
    input$tSHighl
    input$tSGroupOrSampID
    reacttSIDs$d
    if (!is.null(ag$entiretSNE)) {
      tSNEGen(
        input$tSNEDotSize,
        isolate(input$tSNEMode),
        reacttSNEPops$d,
        isolate(input$tSGroupOrSamp),
        isolate(input$tSGroupOrSampID),
        input$tSHighl,
        reacttSIDs$d,
        isolate(input$tsneIDs),
        reacttSNEPops$d
      )
      output$showingtSNEEvents <- renderText({
        paste0("Plotted events: ", format(ag$toFormat, big.mark = ","))
      })
      enable("savetSNEPlot")
      enable("tSNEGenerate")
    } else {
      disable("savetSNEPlot")
    }
  }, height = 440, width = 800)
  
  output$savetSNEPlot <- downloadHandler("t-SNE plot.png",
                                         function(file) {
                                           png(
                                             file,
                                             units = "in",
                                             height = 6,
                                             width = 11.02,
                                             res = 300
                                           )
                                           tSNEGen(
                                             input$tSNEDotSize,
                                             isolate(input$tSNEMode),
                                             reacttSNEPops$d,
                                             isolate(input$tSGroupOrSamp),
                                             isolate(input$tSGroupOrSampID),
                                             input$tSHighl,
                                             reacttSIDs$d,
                                             isolate(input$tsneIDs),
                                             reacttSNEPops$d
                                           )
                                           dev.off()
                                         })
  
  #Results----
  ##left----
  observeEvent(input$rsStat, {
    if (isolate(input$rsStat) == "Freq. of parent"
        || isolate(input$rsStat) == "Count") {
      disable("rsParent")
      updateSelectInput(inputId = "rsCh",
                        choices = c("", ag$completeFluoCh))
      disable("rsCh")
    } else {
      if (isolate(input$rsStat) == "Freq. of...") {
        updateSelectInput(inputId = "rsCh",
                          choices = c("", ag$completeFluoCh))
        disable("rsCh")
      } else {
        updateSelectInput(inputId = "rsCh",
                          choices = c("", ag$completeFluoCh))
        enable("rsCh")
      }
    }
  })
  
  observeEvent(c(input$rsStat, input$rsParent, input$rsPop, input$rsCh), {
    if (isolate(input$rsPop) != "") {
      if (isolate(input$rsStat) == "Freq. of parent"
          || isolate(input$rsStat) == "Count") {
        enable("addResult")
      } else {
        if (isolate(input$rsParent) != ""
            || isolate(input$rsCh) != "") {
          enable("addResult")
        } else {
          disable("addResult")
        }
      }
    } else {
      disable("addResult")
    }
  })
  
  observeEvent(c(input$rsStat, input$rsPop), {
    if (!is.null(ag$gs)) {
      index <- which(gs_get_pop_paths(ag$gs, path = 1) == input$rsPop)
      fullPath <- gs_get_pop_paths(ag$gs)[index]
      possibleParents <- strsplit(fullPath, split = "/")
      if (length(possibleParents) > 0) {
        index <- seq_len(length(possibleParents[[1]]) - 1)
        possibleParents <- possibleParents[[1]][index]
      }
      if (isolate(input$rsStat) == "Freq. of..."
          && isolate(input$rsPop) != "") {
        updateSelectInput(inputId = "rsParent",
                          choices = c(possibleParents))
        enable("rsParent")
      } else {
        updateSelectInput(inputId = "rsParent", selected = "")
        disable("rsParent")
      }
    }
  })
  
  observeEvent(input$addResult, {
    updateSelectInput(inputId = "rsCh",
                      choices = c("", ag$completeFluoCh))
    if (is.na(ag$results[[1]][1])) {
      resultID <- 1
    } else {
      resultID <- ncol(ag$results) + 1
    }
    popPaths <- gs_get_pop_paths(ag$gs)
    popShortPaths <- gs_get_pop_paths(ag$gs, path = 1)
    subpop <- popPaths[which(popShortPaths == input$rsPop)]
    popStats <-
      gs_pop_get_count_fast(ag$gs, subpopulations = subpop)
    id <- ag$onlySampleIDs
    if (input$rsStat == "Freq. of parent") {
      num <- popStats[[4]] * 100 / popStats[[5]]
      ag$results[[resultID]] <- sprintf("%.1f", num)[id]
    # } else if (input$rsStat == "Freq. of total") {
    #   nums <- gs_pop_get_count_fast(ag$gs, "freq",
    #                                 subpopulations = subpop)
    #   ag$results[[resultID]] <- sprintf("%.1f",
    #                                     nums$Count$Frequency * 100)[id]
    } else if (input$rsStat == "Count") {
      ag$results[[resultID]] <- popStats[[4]][id]
    } else if (input$rsStat == "Freq. of...") {
      popPaths <- gs_get_pop_paths(ag$gs)
      popShortPaths <- gs_get_pop_paths(ag$gs, path = 1)
      subpop <- popPaths[which(popShortPaths == input$rsParent)]
      parentstats <-
        gs_pop_get_count_fast(ag$gs, subpopulations = subpop)
      num <- popStats[[4]] * 100 / parentstats[[4]]
      ag$results[[resultID]] <- sprintf("%.1f", num)[id]
    } else if (input$rsStat == "Median") {
      index <- which(input$rsCh == ag$fluoCh) + 2
      stats <- gs_pop_get_stats(ag$gs, input$rsPop, pop.MFI)
      ag$results[[resultID]] <- sprintf("%.1f", stats[[index]])
    }
    if (input$rsStat == "Median") {
      name1 <- "MFI"
    } else {
      name1 <- input$rsStat
    }
    if (input$rsParent != "") {
      name2 <- paste0(input$rsParent)
    } else {
      name2 <- ""
    }
    if (input$rsCh != "") {
      name4 <- paste0(" (", input$rsCh, ")")
    } else {
      name4 <- ""
    }
    colnames(ag$results)[resultID] <- paste0(name1, name2, "/",
                                             input$rsPop, name4)
  })
  
  ##main----
  output$results <- renderTable(rownames = TRUE, spacing = "xs", {
    input$addResult
    input$deleteModal
    input$tabs
    if (!is.na(ag$results[, 1][[1]])) {
      enable("exportTable")
      enable("editResult")
    }
    allPops <- gs_get_pop_paths(ag$gs, path = 1)
    allPops <- allPops[allPops != allPops[1]]
    updateSelectInput(inputId = "rsPop", choices = c("", allPops))
    ag$results
  })
  
  observeEvent(input$editResult, {
    choices <- setNames(as.list(seq(ncol(ag$results))),
                        colnames(ag$results))
    showModal(
      modalDialog(
        "Select the rows to be deleted.",
        checkboxGroupInput("checkdelete", label = "", choices = choices),
        footer = list(
          actionButton(
            inputId = "deleteModal",
            label = "Delete",
            style = ag$blackB
          ),
          actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = TRUE,
        size = "s"
      )
    )
  })
  
  observeEvent(input$deleteModal, {
    ag$results <- ag$results[-as.integer(input$checkdelete)]
    removeModal()
    if (ncol(ag$results) == 0) {
      ag$results[1] <- NA
      colnames(ag$results) <- NA
      disable("exportTable")
      disable("editResult")
    }
  })
  
  output$exportTable <- downloadHandler("Results.csv",
                                        function(file) {
                                          write.csv(ag$results, file)
                                        })
  
  
  #AutoResults----
  ##left----
  observeEvent(input$autorsStat, {
    if (isolate(input$autorsStat) == "Freq. of parent"
        || isolate(input$autorsStat) == "Count") {
      disable("autorsParent")
      updateSelectInput(inputId = "autorsCh",
                        choices = c("", ag$completeFluoCh))
      disable("autorsCh")
    } else {
      if (isolate(input$autorsStat) == "Freq. of...") {
        updateSelectInput(inputId = "autorsCh",
                          choices = c("", ag$completeFluoCh))
        disable("autorsCh")
      } else {
        updateSelectInput(inputId = "autorsCh",
                          choices = c("", ag$completeFluoCh))
        enable("autorsCh")
      }
    }
  })
  
  observeEvent(c(
    input$autorsStat,
    input$autorsParent,
    input$autorsPop,
    input$autorsCh
  ),
  {
    if (isolate(input$autorsPop) != "") {
      if (isolate(input$autorsStat) == "Freq. of parent"
          || isolate(input$autorsStat) == "Count") {
        enable("addautoResult")
      } else {
        if (isolate(input$autorsParent) != ""
            || isolate(input$autorsCh) != "") {
          enable("addautoResult")
        } else {
          disable("addautoResult")
        }
      }
    } else {
      disable("addautoResult")
    }
  })
  
  observeEvent(c(input$autorsStat, input$autorsPop), {
    if (!is.null(ag$auto_gs)) {
      index <-
        which(gs_get_pop_paths(ag$auto_gs, path = 1) == input$autorsPop)
      fullPath <- gs_get_pop_paths(ag$auto_gs)[index]
      autopossibleParents <- strsplit(fullPath, split = "/")
      if (length(autopossibleParents) > 0) {
        index <- seq_len(length(autopossibleParents[[1]]) - 1)
        autopossibleParents <- autopossibleParents[[1]][index]
      }
      if (isolate(input$autorsStat) == "Freq. of..."
          && isolate(input$autorsPop) != "") {
        updateSelectInput(inputId = "autorsParent",
                          choices = c(autopossibleParents))
        enable("autorsParent")
      } else {
        updateSelectInput(inputId = "autorsParent", selected = "")
        disable("autorsParent")
      }
    }
  })
  
  observeEvent(input$addautoResult, {
    updateSelectInput(inputId = "autorsCh",
                      choices = c("", ag$completeFluoCh))
    if (is.na(ag$autoresults[[1]][1])) {
      autoresultID <- 1
    } else {
      autoresultID <- ncol(ag$autoresults) + 1
    }
    autopopPaths <- gs_get_pop_paths(ag$auto_gs)
    autopopShortPaths <- gs_get_pop_paths(ag$auto_gs, path = 1)
    autosubpop <-
      autopopPaths[which(autopopShortPaths == input$autorsPop)]
    autopopStats <-
      gs_pop_get_count_fast(ag$auto_gs, subpopulations = autosubpop)
    id <- ag$onlySampleIDs
    if (input$autorsStat == "Freq. of parent") {
      autonum <- autopopStats[[4]] * 100 / autopopStats[[5]]
      ag$autoresults[[autoresultID]] <- sprintf("%.1f", autonum)[id]
    # } else if (input$autorsStat == "Freq. of total") {
    #   autonums <- gs_pop_get_count_fast(ag$auto_gs, "freq",
    #                                     subpopulations = autosubpop)
    #   ag$autoresults[[autoresultID]] <- sprintf("%.1f",
    #                                             autonums$Count$Frequency * 100)[id]
    } else if (input$autorsStat == "Count") {
      ag$autoresults[[autoresultID]] <- autopopStats[[4]][id]
    } else if (input$autorsStat == "Freq. of...") {
      autopopPaths <- gs_get_pop_paths(ag$auto_gs)
      autopopShortPaths <- gs_get_pop_paths(ag$auto_gs, path = 1)
      autosubpop <-
        autopopPaths[which(autopopShortPaths == input$autorsParent)]
      autoparentstats <-
        gs_pop_get_count_fast(ag$auto_gs, subpopulations = autosubpop)
      autonum <- autopopStats[[4]] * 100 / autoparentstats[[4]]
      ag$autoresults[[autoresultID]] <- sprintf("%.1f", autonum)[id]
    } else if (input$autorsStat == "Median") {
      index <- which(input$autorsCh == ag$fluoCh) + 2
      stats <-
        gs_pop_get_stats(ag$auto_gs, input$autorsPop, type = pop.MFI)
      ag$autoresults[[autoresultID]] <-
        sprintf("%.1f", stats[[index]])
    }
    if (input$autorsStat == "Median") {
      name1 <- "MFI"
    } else {
      name1 <- input$autorsStat
    }
    if (input$autorsParent != "") {
      name2 <- paste0(input$autorsParent)
    } else {
      name2 <- ""
    }
    if (input$autorsCh != "") {
      name4 <- paste0(" (", input$autorsCh, ")")
    } else {
      name4 <- ""
    }
    colnames(ag$autoresults)[autoresultID] <-
      paste0(name1, name2, "/",
             input$autorsPop, name4)
  })
  
  ##main----
  output$autoresults <-
    renderTable(rownames = TRUE, spacing = "xs", {
      input$addautoResult
      input$deleteModal
      input$tabs
      if (!is.na(ag$autoresults[, 1][[1]])) {
        enable("exportautoTable")
        enable("editautoResult")
      }
      allPops <- gs_get_pop_paths(ag$auto_gs, path = 1)
      allPops <- allPops[allPops != allPops[1]]
      updateSelectInput(inputId = "autorsPop", choices = c("", allPops))
      ag$autoresults
    })
  
  observeEvent(input$editautoResult, {
    choices <- setNames(as.list(seq(ncol(ag$autoresults))),
                        colnames(ag$autoresults))
    showModal(
      modalDialog(
        "Select the rows to be deleted.",
        checkboxGroupInput("checkdelete", label = "", choices = choices),
        footer = list(
          actionButton(
            inputId = "deleteModal",
            label = "Delete",
            style = ag$blackB
          ),
          actionButton(
            inputId = "cancelModal",
            label = "Cancel",
            style = ag$whiteB
          )
        ),
        easyClose = TRUE,
        size = "s"
      )
    )
  })
  
  observeEvent(input$deleteModal, {
    ag$autoresults <- ag$autoresults[-as.integer(input$checkdelete)]
    removeModal()
    if (ncol(ag$autoresults) == 0) {
      ag$autoresults[1] <- NA
      colnames(ag$autoresults) <- NA
      disable("exportautoTable")
      disable("editautoResult")
    }
  })
  
  output$exportautoTable <- downloadHandler("autoresults.csv",
                                            function(file) {
                                              write.csv(ag$autoresults, file)
                                            })
  
  #Between Tabs----
  observeEvent(input$tabs, {
    if (input$tabs == "compTab") {
      ag$uncompGSfortable <- gs_clone(ag$uncompGS)
      for (i in seq(ag$fluoCh)) {
        trans <- flowjo_biexp_trans(
          channelRange = 4096,
          maxValue = 262144,
          pos = ag$customAxis[[i]][1],
          neg = ag$customAxis[[i]][2],
          widthBasis = ag$customAxis[[i]][3]
        )
        ag$uncompGSfortable <- transform(ag$uncompGSfortable,
                                         transformerList(ag$fluoCh[i],
                                                         trans))
      }
      hide("saveMatrix")
      hide("cancelMatrix")
      enable("createMatrix")
      reactReadOnly$d <- TRUE
      cytomDef <- ag$compDFs["Cytometer-defined"]
      ag$hTable$d <- as.data.frame(cytomDef[[1]] * 100)
      output$comp <- renderRHandsontable({
        mat <- ag$compDFs[input$previewMatrix][[1]] * 100
        visualtable <- rhandsontable(
          mat,
          rowHeaderWidth = 100,
          stretchH = "all",
          readOnly = reactReadOnly$d
        ) %>%
          hot_validate_numeric(cols = seq(ag$fluoCh)) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_cols(
            format = "0",
            renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.NumericRenderer.apply(this,
                        arguments);
                     heatmap = ['#FFFFCC66', '#FFEDA066', '#FED97666',
                    '#FEB24C66', '#FD8D3C66', '#FC4E2A66', '#E31A1C66',
                    '#BD002666', '#80002666']
                    if (row == col) {
                      td.style.background = 'lightgrey';
                    } else if (Math.abs(value) > 88) {
                      td.style.background = heatmap[8];
                    } else if (Math.abs(value) > 77) {
                      td.style.background = heatmap[7];
                    } else if (Math.abs(value) > 66) {
                      td.style.background = heatmap[6];
                    } else if (Math.abs(value) > 55) {
                      td.style.background = heatmap[5];
                    } else if (Math.abs(value) > 44) {
                      td.style.background = heatmap[4];
                    } else if (Math.abs(value) > 33) {
                      td.style.background = heatmap[3];
                    } else if (Math.abs(value) > 22) {
                      td.style.background = heatmap[2];
                    } else if (Math.abs(value) > 11) {
                      td.style.background = heatmap[1];
                    } else if (Math.abs(value) > 0) {
                      td.style.background = heatmap[0];
                    } else if (Math.abs(value) <= 0) {
                      td.style.background = 'white'
                    }
            }
                   "
          )
        for (i in seq(ag$fluoCh)) {
          cell <- list(row = i - 1,
                       col = i - 1,
                       readOnly = TRUE)
          visualtable$x$cell <- c(visualtable$x$cell, list(cell))
        }
        onRender(visualtable, changeHook)
      })
    } else if (input$tabs == "prolifTab") {
      popPaths <- gs_get_pop_paths(ag$gs, path = 1)
      if ("undivided" %in% autopopPaths) {
        hide("prolifStart")
        shinyjs::show("prolifPlot")
      } else {
        hide("prolifPlot")
        shinyjs::show("prolifStart")
      }
    } else if (input$tabs == "autoprolifTab") {
        autopopPaths <- gs_get_pop_paths(ag$auto_gs, path = 1)
        if ("undivided" %in% autopopPaths) {
          hide("prolifStart")
          shinyjs::show("autoprolifPlot")
        } else {
          hide("autoprolifPlot")
          shinyjs::show("autoprolifStart")
        }
    } else if (input$tabs == "resultTab") {
      updateSelectInput(inputId = "rsStat", selected = "")
      updateSelectInput(inputId = "rsParent", selected = "")
      updateSelectInput(inputId = "rsPop", selected = "")
      updateSelectInput(inputId = "rsCh", selected = "")
      disable("rsParent")
      disable("rsCh")
    } else if (input$tabs == "autoresultTab") {
      updateSelectInput(inputId = "autorsStat", selected = "")
      updateSelectInput(inputId = "autorsParent", selected = "")
      updateSelectInput(inputId = "autorsPop", selected = "")
      updateSelectInput(inputId = "autorsCh", selected = "")
      disable("autorsParent")
      disable("autorsCh")
    }
  })
}