# Load Packages----
library(base)
library(Rgraphviz)
library(rhandsontable)
library(shiny)
library(shinyFiles)
library(shinyjs)
library(grDevices)
library(BiocGenerics)
library(flowCore)
library(flowWorkspace)
library(stats)
library(CytoML)
library(flowClust)
library(ggcyto)
library(openCyto)
library(graph)
library(methods)
library(dplyr)
library(ncdfFlow)
library(flowDensity)
library(flowViz)
library(graphics)
library(flowStats)
library(ggplot2)
library(utils)
library(autospill)
library(MASS)
library(mixtools)
library(moments)
library(data.table)
library(fs)
library(htmlwidgets)
library(Rtsne)
library(lattice)
library(base64enc)
library(magrittr)
library(scales)
library(flowAI)
library(gridExtra)
library(gtools)
library(glue)
library(CytoExploreR)

# options(shiny.fullstacktrace=T)
# shiny:: reactiveConsole(enabled = T)

#AutoGateR----
runAutoGateR <- function() {
  shinyApp(
    ui,
    server,
    onStart = function() {
      useShinyjs()
      extendShinyjs(text = jsCode,
                    functions = c('disableTab', 'enableTab'))
      inlineCSS(css)
    }
  )
}

#Init Variables----
script <- "
$('#comp tr td').each(function() {
  var cellValue=$(this).text();
  if (cellValue == 100.0000000) {
    $(this).css('background-color', 'lightgrey');
    $(this).css('font-weight', 'normal')
  } else if (cellValue == 0) {
    $(this).css('font-weight', 'normal')
  } else if (cellValue > 0 && cellValue < 10) {
    $(this).css('background-color', '#FFFFB2');
    $(this).css('font-weight', 'normal')
  } else if (cellValue >= 10 && cellValue < 20) {
    $(this).css('background-color', '#FED976');
    $(this).css('font-weight', 'normal')
  } else if (cellValue >= 20 && cellValue < 40) {
    $(this).css('background-color', '#FEB24C');
    $(this).css('font-weight', 'normal')
  } else if (cellValue >= 40 && cellValue < 70) {
    $(this).css('background-color', '#FD8D3C');
    $(this).css('font-weight', 'normal')
  } else if (cellValue >= 70 && cellValue < 100) {
    $(this).css('background-color', '#F03B20');
    $(this).css('font-weight', 'normal')
  } else if (cellValue > 100) {
    $(this).css('background-color', '#BD0026');
    $(this).css('font-weight', 'normal')
  }
})"
changeHook <- "
function(el,x) {
  var hot=this.hot;
  var cellChanges=[];
  var changefn=function(changes,source) {
    if (source === 'edit'
      || source === 'undo'
      || source === 'autofill'
      || source === 'paste') {
      row=changes[0][0];
      col=changes[0][1];
      oldval=changes[0][2];
      newval=changes[0][3];
      if (oldval !== newval) {
        var cell=hot.getCell(row, col);
        cell.style.color='red';
        cellChanges.push({'rowid':row, 'colid':col});
      }
    }
  }
  var renderfn=function(isForced) {
    for(i=0; i < cellChanges.length; i++) {
      var rowIndex=cellChanges[i]['rowid'];
      var columnIndex=cellChanges[i]['colid'];
      var cell=hot.getCell(rowIndex, columnIndex);
      cell.style.color='red';
    }
  }
  var loadfn=function(initialLoad) {
    for(i=0; i < cellChanges.length; i++) {
      var rowIndex=cellChanges[i]['rowid'];
      var columnIndex=cellChanges[i]['colid'];
      var cell=hot.getCell(rowIndex, columnIndex);
      cell.style.color='black';
    }
    cellChanges=[] }
  hot.addHook('afterChange', changefn);
  hot.addHook('afterRender', renderfn);
  hot.addHook('afterLoadData', loadfn);
}"
jsCode <- "
shinyjs.disableTab=function(name) {
  var tab=$('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}
shinyjs.enableTab=function(name) {
  var tab=$('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}"

css <- "
.nav li a.disabled {
  color: #737373 !important;
    cursor: not-allowed !important;
}"
ag <- new.env()
tempEnv <- new.env()
ag$fluoCh <- ""
ag$Y <- ""
ag$prolifChannel <- NULL
ag$prolifParent <- NULL
ag$concat <- NULL
ag$gs <- NULL
ag$auto_gs <- NULL
ag$blackB <-
  "color:white; background-color:black; border-color:black"
ag$whiteB <-
  "color:black; background-color:white; border-color:black"
ag$sliderCol <-
  "+span>.irs-bar {background: black; border-top: black;
        border-bottom: black}"

sysInf <- Sys.info()
if (!is.null(sysInf)) {
  OS <- sysInf['sysname']
  if (OS == 'Darwin') {
    OS <- "OSX"
  }
} else {
  OS <- .Platform$OS.type
  if (grepl("^darwin", R.version$OS)) {
    OS <- "OSX"
  }
}
ag$OS <- OS

#Internal funcs----
disableTabs <- function() {
  js$disableTab("ancestryTab")
  # js$disableTab("autoancestryTab")
  js$disableTab("overlayTab")
  # js$disableTab("autooverlayTab")
  # js$disableTab("prolifTab")
  js$disableTab("tsneTab")
  js$disableTab("resultTab")
  # js$disableTab("autoresultTab")
  
}

autodisableTabs <- function() {
  # js$disableTab("ancestryTab")
  js$disableTab("autoancestryTab")
  # js$disableTab("overlayTab")
  js$disableTab("autooverlayTab")
  # js$disableTab("prolifTab")
  # js$disableTab("tsneTab")
  # js$disableTab("resultTab")
  js$disableTab("autoresultTab")
  
}
custom_gate_quad_sequential <- 
  function (fr,
            channels,
            gFunc,
            min = NULL,
            max = NULL,
            ...)
  {
    if (missing(channels) || length(channels) != 2) {
      stop("two channels must be specified.")
    }
    if (!(is.null(min) && is.null(max))) {
      fr <- openCyto:::.truncate_flowframe(fr,
                                channels = channels,
                                min = min,
                                max = max)
    }
    res <- sapply(channels, function(channel) {
      x <- exprs(fr)[, channel]
      peaks <- openCyto:::.find_peaks(x, ..., num_peaks = 2)
      if (nrow(peaks) < 2)
        score <- 0
      else
        score <- abs(diff(peaks[, "x"]))
      list(peaks = peaks[, "x"], score = score)
    }, simplify = FALSE)
    channels.ordered <- names(sort(sapply(res, `[[`, "score"),
                                   decreasing = T))
    x.first <- all(channels == channels.ordered)
    chnl <- channels.ordered[1]
    thisFunc <- function(fr, chnl, ...) {
      thisCall <- substitute(
        f(data, channel, peaks = p, ...),
        list(
          f = as.symbol(gFunc),
          data = fr,
          channel = chnl,
          p = res[[chnl]][["peaks"]]
        )
      )
      eval(thisCall)
    }
    g1 <- thisFunc(fr, chnl, ...)
    coord1 <- c(g1@min, g1@max)
    cut.1 <- coord1[!is.infinite(coord1)]
    fres <- flowCore::filter(fr, g1)
    ind <- as(fres, "logical")
    chnl <- channels.ordered[2]
    g2 <- thisFunc(fr[ind, ], chnl, ...)
    coord2 <- c(g2@min, g2@max)
    cut.2.pos <- coord2[!is.infinite(coord2)]
    g3 <- thisFunc(fr[!ind, ], chnl, ...)
    coord3 <- c(g3@min, g3@max)
    cut.2.neg <- coord3[!is.infinite(coord3)]
    if (x.first)
      coords <- list(
        q1 = list(c(-Inf, cut.1), c(cut.2.neg,
                                    Inf)),
        q2 = list(c(cut.1, Inf), c(cut.2.pos, Inf)),
        q3 = list(c(cut.1, Inf), c(-Inf, cut.2.pos)),
        q4 = list(c(-Inf,
                    cut.1), c(-Inf, cut.2.neg))
      )
    else
      coords <- list(
        q1 = list(c(-Inf, cut.2.pos), c(cut.1,
                                        Inf)),
        q2 = list(c(cut.2.pos, Inf), c(cut.1, Inf)),
        q3 = list(c(cut.2.neg, Inf), c(-Inf, cut.1)),
        q4 = list(c(-Inf,
                    cut.2.neg), c(-Inf, cut.1))
      )
    
    gates <- lapply(coords, function(coord) {
      names(coord) <- as.character(channels)
      rectangleGate(coord)
    })
    filters(gates)
  }

# environment(custom_gate_quad_sequential) <- asNamespace('openCyto')
# assignInNamespace("gate_quad_sequential", custom_gate_quad_sequential, ns = "openCyto")


##File----
initializing <- function() {
  setwd(ag$filePath)
  cs <- load_cytoset_from_fcs(ag$fileList)
  ag$uncompGS <- GatingSet(cs)
  ag$gs <- gs_clone(ag$uncompGS)
  cf <- get_cytoframe_from_cs(ag$gs, ag$compControlIDs[1])
  colnames(ag$gs) <- colnames(cf)
  preFluoChannels <- colnames(ag$gs)[!grepl("FS", colnames(ag$gs))]
  preFluoChannels <- preFluoChannels[!grepl("SS", preFluoChannels)]
  preFluoChannels <- preFluoChannels[preFluoChannels != "Time"]
  preFluoChannels <- preFluoChannels[preFluoChannels != "Height"]
  preFluoChannels <- preFluoChannels[preFluoChannels != "Width"]
  ag$fluoCh <- preFluoChannels
  notNull <- vapply(spillover(cf), function(x) {
    !is.null(x)
  }, TRUE)
  spill <- spillover(cf)[notNull][[1]]
  if (is.null(spill)) {
    len <- length(ag$fluoCh)
    spill <- diag(1, len, len)
    colnames(spill) <- ag$fluoCh
  }
  ag$compDFs <- list(spill)
  names(ag$compDFs) <- "Cytometer-defined"
  rownames(ag$compDFs[[1]]) <- colnames(ag$compDFs[[1]])
  compensate(ag$gs, ag$compDFs[[1]])
  ag$customAxis <- rep(list(c(4.42, 0, -100)), length(ag$fluoCh))
  ag$customAxis <- ag$customAxis
  biexp <- flowjo_biexp_trans(
    channelRange = 4096,
    maxValue = 262144,
    pos = ag$customAxis[[1]][1],
    neg = ag$customAxis[[1]][2],
    widthBasis = ag$customAxis[[1]][3]
  )
  ag$gs <- transform(ag$gs, transformerList(ag$fluoCh, biexp))
  ff <- cytoframe_to_flowFrame(cs[[length(cs)]])
  ag$channelNames <- ff@parameters@data[, 2]
  ch <- vapply(seq(colnames(ag$gs)), function(x)
    paste(colnames(ag$gs)[x], ag$channelNames[x], sep = " :: "),
    character(1))
  na <- vapply(ag$channelNames, function(x)
    is.na(x), TRUE)
  ch[na] <- colnames(ag$gs)[na]
  ag$ch <- as.list(colnames(ag$gs))
  names(ag$ch) <- ch
  completeFluoCh <- unlist(ag$ch)
  completeFluoCh <- completeFluoCh[completeFluoCh != "Time"]
  ag$completeFluoCh <- completeFluoCh[!grepl("FS", completeFluoCh)]
  ag$completeFluoCh <-
    ag$completeFluoCh[!grepl("SS", ag$completeFluoCh)]
  ag$X <- colnames(ag$gs)[grep("FS", colnames(ag$gs))][1]
  ag$Y <- colnames(ag$gs)[grep("SS", colnames(ag$gs))][1]
  ag$auto_gs <- gs_clone(ag$gs)
  ag$autoX <-
    colnames(ag$auto_gs)[grep("FS", colnames(ag$auto_gs))][1]
  ag$autoY <-
    colnames(ag$auto_gs)[grep("SS", colnames(ag$auto_gs))][1]
  ag$sampleList <- ag$fileList[ag$onlySampleIDs]
  ag$results <-
    data.frame(matrix(ncol = 1, nrow = length(ag$sampleList)))
  rownames(ag$results) <-
    substr(ag$sampleList, 1, nchar(ag$sampleList) - 4)
  colnames(ag$results) <- NA
    ag$autoresults <-
    data.frame(matrix(ncol = 1, nrow = length(ag$sampleList)))
  rownames(ag$autoresults) <-
    substr(ag$sampleList, 1, nchar(ag$sampleList) - 4)
  colnames(ag$autoresults) <- NA
    ag$hTable <-
    reactiveValues(d = as.data.frame(ag$compDFs[[1]] * 100))
  ag$autohTable <-
    reactiveValues(d = as.data.frame(ag$compDFs[[1]] * 100))
  ag$choices <- "Cytometer-defined"
  ag$appliedMatrix <- "Cytometer-defined"
}

initInpUpd <- function() {
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
  updateSelectInput(
    inputId = "previewSample",
    choices = c("", ag$fileList),
    selected = ag$sampleList[1]
  )
  updateSelectInput(
      inputId = "autopreviewSample",
      choices = c("", ag$fileList),
      selected = ag$sampleList[1]
  )
  updateSelectInput(inputId = "bgPreviewSample", choices = c("", ag$sampleList)
                    )
  updateSelectInput(inputId = "autobgPreviewSample", choices = c("", ag$sampleList)
                    )
  updateSelectInput(
    inputId = "prolifSButt",
    choices = c("", ag$sampleList),
    selected = ag$sampleList[1]
  )
  updateSelectInput(inputId = "Y",
                    choices = c(ag$ch),
                    selected = ag$Y)
  updateSelectInput(inputId = "X",
                    choices = c(ag$ch),
                    selected = ag$X)
  updateSelectInput(inputId = "autoY",
                    choices = c(ag$ch),
                    selected = ag$Y)
  updateSelectInput(inputId = "autoX",
                    choices = c(ag$ch),
                    selected = ag$X)
  updateSelectInput(inputId = "ovY", choices = c("", ag$ch))
  updateSelectInput(inputId = "ovX", choices = c("", ag$ch))
  updateSelectInput(inputId = "rsCh",
                    choices = c("", ag$completeFluoCh))
  updateSelectInput(inputId = "autoovY", choices = c("", ag$ch))
  updateSelectInput(inputId = "autoovX", choices = c("", ag$ch))
  updateSelectInput(inputId = "autorsCh",
                    choices = c("", ag$completeFluoCh))
  js$enableTab("plotTab")
  js$enableTab("autoPlotTab")
  js$enableTab("compTab")
}

##Axis----
addYAxis <- function(Y, axis, font) {
  index <- which(ag$fluoCh == Y)
  # ag$index3 <- index
  if (Y == "Time") {
    axisTicks(2, "time", ag$customAxis[[index]])
  } else if (grepl("FS", Y) || grepl("SS", Y)) {
    axisTicks(2, "cont", ag$customAxis[[index]])
    if (axis == TRUE) {
      title(
        ylab = Y,
        cex.lab = (1 + font / 10),
        line = 3.3,
        font.lab = 2
      )
    }
  } else {
    axisTicks(2, "log", ag$customAxis[[index]])
    if (axis == TRUE) {
      id <- which(colnames(ag$gs) == Y)
      title(
        ylab = names(ag$ch)[id],
        cex.lab = (1 + font / 10),
        line = 2.6,
        font.lab = 2
      )
    }
  }
}

axisTicks <- function(axisID, typ, customAxis) {
  if (typ == "time") {
    axisTTime(axisID)
  } else if (typ == "log") {
    # ag$axisid3 <- axisID
    axisTLog(axisID, customAxis)
  } else if (typ == "cont") {
    axisTCont(axisID)
  } else if (typ == "histo") {
    axisTHist(axisID)
  } else if (typ == "Overlaid histogram") {
    axisTOv(axisID)
  }
}

axisTTime <- function(axisID) {
  majorT <- axis(axisID, labels = FALSE, lwd = 0)
  lastMajorInvT <- majorT[length(majorT)] + majorT[2] - majorT[1]
  minorT <- seq_len(lastMajorInvT / 100) * 100
  axis(axisID,
       at = majorT,
       lwd = 2,
       labels = FALSE)
  axis(
    axisID,
    at = minorT,
    lwd = 1,
    labels = FALSE,
    tck = -0.015
  )
  axis(
    axisID,
    at = majorT,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = majorT / 100
  )
}

axisTLog <- function(axisID, customAxis) {
  trans <- flowjo_biexp(
    channelRange = 4096,
    maxValue = 262144,
    pos = customAxis[1],
    neg = customAxis[2],
    widthBasis = customAxis[3]
  )
  majorT <- trans(c(rev(10 ^ seq_len(6) * -1), 0, 10 ^ seq_len(6)))
  labels <- expression(10 ^ -6,
                       10 ^ -5,
                       10 ^ -4,
                       10 ^ -3,
                       10 ^ -2,
                       10 ^ -1,
                       0,
                       10 ^ 1,
                       10 ^ 2,
                       10 ^ 3,
                       10 ^ 4,
                       10 ^ 5,
                       10 ^ 6)
  neg <- majorT[seq_len(7)]
  pos <- majorT[7:length(majorT)]
  minorneg <- vapply(seq(pos), function(i)
    (pos[i + 1] - pos[i]) * log10(seq_len(9)) + pos[i], numeric(9))
  minorpos <- vapply(seq(neg), function(i)
    (neg[i + 1] - neg[i]) * (1 - log10(rev(seq_len(
      9
    )))) + neg[i], numeric(9))
  minorT <- list(as.list(minorneg[,-7]), as.list(minorpos[,-7]))
  minorT <- as.list(unlist(minorT))
  axis(
    axisID,
    at = minorT,
    lwd = 2,
    labels = FALSE,
    tck = -0.015
  )
  adaptedLabels <- labels
  if (abs(majorT[6] - majorT[8]) < 600) {
    adaptedLabels[6] <- adaptedLabels[8] <- ""
  }
  if (abs(majorT[5] - majorT[9]) < 600) {
    adaptedLabels[5] <- adaptedLabels[9] <- ""
  }
  if (abs(majorT[4] - majorT[10]) < 600) {
    adaptedLabels[4] <- adaptedLabels[10] <- ""
  }
  if (abs(majorT[3] - majorT[11]) < 600) {
    adaptedLabels[3] <- adaptedLabels[11] <- ""
  }
  axis(axisID,
       at = majorT,
       lwd = 2,
       labels = FALSE)
  axis(
    axisID,
    at = majorT,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = adaptedLabels
  )
}

axisTCont <- function(axisID) {
  majorT <- seq(0, 250000, by = 50000)
  minorT <- seq(0, 260000, by = 10000)
  axis(axisID,
       at = majorT,
       lwd = 2,
       labels = FALSE)
  axis(
    axisID,
    at = minorT,
    lwd = 1,
    labels = FALSE,
    tck = -0.015
  )
  axis(
    axisID,
    at = majorT,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = c("0", "50K", "100K", "150K", "200K", "250K")
  )
}

axisTHist <- function(axisID) {
  majorT <- axis(2, labels = FALSE, lwd = 0)
  minorT <-
    seq(0, max(majorT) * 1.2, by = majorT[1] + majorT[2] / 5)
  axis(2,
       at = majorT,
       lwd = 2,
       labels = FALSE)
  axis(
    2,
    at = minorT,
    lwd = 1,
    labels = FALSE,
    tck = -0.015
  )
  shortL <- NULL
  for (i in seq(majorT)) {
    if (majorT[i] >= 1000) {
      n <- majorT[i]
      n2 <- n - as.numeric(substr(n, nchar(n) - 2, nchar(n)))
      diminishedNumber <- n - n2
      if (diminishedNumber / 1000 == 0) {
        shortL[[i]] <- paste0(n2 / 1000, "K")
      } else {
        shortL[[i]] <- paste0(n2 / 1000, ".", diminishedNumber / 100, "K")
      }
    } else {
      shortL[[i]] <- majorT[i]
    }
  }
  axis(
    2,
    at = majorT,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = as.character(majorT)
  )
}

axisTOv <- function(axisID) {
  majorT <- axis(2, labels = FALSE, lwd = 0)
  axis(2,
       at = majorT,
       lwd = 2,
       labels = FALSE)
  axis(
    2,
    at = seq(0, 1.2, by = majorT[1] + majorT[2] / 4),
    lwd = 1,
    tck = -0.015,
    labels = FALSE
  )
  axis(
    2,
    at = majorT,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = (majorT * 100)
  )
}

axisTtSNE <- function(xLim, yLim) {
  seqX1 <- seq(xLim[1] * 0.9, xLim[2] * 0.9, length.out = 6)
  seqX2 <- seq(xLim[1] * 0.9, xLim[2] * 0.9, length.out = 21)
  axis(1,
       at = seqX1,
       lwd = 2,
       labels = FALSE)
  axis(
    1,
    at = seqX2,
    lwd = 1,
    labels = FALSE,
    tck = -0.015
  )
  axis(
    1,
    at = seqX1,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = as.character(seq(0, 100, by = 20))
  )
  seqY1 <- seq(yLim[1] * 0.9, yLim[2] * 0.9, length.out = 6)
  seqY2 <- seq(yLim[1] * 0.9, yLim[2] * 0.9, length.out = 21)
  axis(2,
       at = seqY1,
       lwd = 2,
       labels = FALSE)
  axis(
    2,
    at = seqY2,
    lwd = 1,
    labels = FALSE,
    tck = -0.015
  )
  axis(
    2,
    at = seqY1,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = as.character(seq(0, 100, by = 20))
  )
  title(
    xlab = "t-SNE 1",
    ylab = "t-SNE 2",
    cex.lab = 1.5,
    line = 2.5,
    font.lab = 2
  )
}

##autoaxis----
autoaddYAxis <- function(Y, axis, font) {
  index <- which(ag$fluoCh == Y)
  if (Y == "Time") {
    autoaxisTicks(2, "time", ag$customAxis[[index]])
  } else if (grepl("FS", Y) || grepl("SS", Y)) {
    autoaxisTicks(2, "cont", ag$customAxis[[index]])
    if (axis == TRUE) {
      title(
        ylab = Y,
        cex.lab = (1 + font / 10),
        line = 3.3,
        font.lab = 2
      )
    }
  } else {
    autoaxisTicks(2, "log", ag$customAxis[[index]])
    if (axis == TRUE) {
      id <- which(colnames(ag$auto_gs) == Y)
      title(
        ylab = names(ag$ch)[id],
        cex.lab = (1 + font / 10),
        line = 2.6,
        font.lab = 2
      )
    }
  }
}

autoaxisTicks <- function(axisID, autotyp, customAxis) {
  if (autotyp == "time") {
    autoaxisTTime(axisID)
  } else if (autotyp == "log") {
    # ag$axisid2 <- axisID
    # ag$customaxis2 <- customAxis
    autoaxisTLog(axisID, customAxis)
  } else if (autotyp == "cont") {
    autoaxisTCont(axisID)
  } else if (autotyp == "histo") {
    autoaxisTHist(axisID)
  } else if (autotyp == "Overlaid histogram") {
    autoaxisdTOv(axisID)
  }
}

autoaxisTTime <- function(axisID) {
  majorT <- axis(axisID, labels = FALSE, lwd = 0)
  lastMajorInvT <- majorT[length(majorT)] + majorT[2] - majorT[1]
  minorT <- seq_len(lastMajorInvT / 100) * 100
  axis(axisID,
       at = majorT,
       lwd = 2,
       labels = FALSE)
  axis(
    axisID,
    at = minorT,
    lwd = 1,
    labels = FALSE,
    tck = -0.015
  )
  axis(
    axisID,
    at = majorT,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = majorT / 100
  )
}

autoaxisTLog <- function(axisID, customAxis) {
  autotrans <- flowjo_biexp(
    channelRange = 4096,
    maxValue = 262144,
    pos = customAxis[1],
    neg = customAxis[2],
    widthBasis = customAxis[3]
  )
  majorT <- autotrans(c(rev(10 ^ seq_len(6) * -1), 0, 10 ^ seq_len(6)))
  labels <- expression(10 ^ -6,
                       10 ^ -5,
                       10 ^ -4,
                       10 ^ -3,
                       10 ^ -2,
                       10 ^ -1,
                       0,
                       10 ^ 1,
                       10 ^ 2,
                       10 ^ 3,
                       10 ^ 4,
                       10 ^ 5,
                       10 ^ 6)
  neg <- majorT[seq_len(7)]
  pos <- majorT[7:length(majorT)]
  minorneg <- vapply(seq(pos), function(i)
    (pos[i + 1] - pos[i]) * log10(seq_len(9)) + pos[i], numeric(9))
  minorpos <- vapply(seq(neg), function(i)
    (neg[i + 1] - neg[i]) * (1 - log10(rev(seq_len(
      9
    )))) + neg[i], numeric(9))
  minorT <- list(as.list(minorneg[,-7]), as.list(minorpos[,-7]))
  minorT <- as.list(unlist(minorT))
  axis(
    axisID,
    at = minorT,
    lwd = 2,
    labels = FALSE,
    tck = -0.015
  )
  adaptedLabels <- labels
  if (abs(majorT[6] - majorT[8]) < 600) {
    adaptedLabels[6] <- adaptedLabels[8] <- ""
  }
  if (abs(majorT[5] - majorT[9]) < 600) {
    adaptedLabels[5] <- adaptedLabels[9] <- ""
  }
  if (abs(majorT[4] - majorT[10]) < 600) {
    adaptedLabels[4] <- adaptedLabels[10] <- ""
  }
  if (abs(majorT[3] - majorT[11]) < 600) {
    adaptedLabels[3] <- adaptedLabels[11] <- ""
  }
  axis(axisID,
       at = majorT,
       lwd = 2,
       labels = FALSE)
  axis(
    axisID,
    at = majorT,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = adaptedLabels
  )
}

autoaxisTCont <- function(axisID) {
  majorT <- seq(0, 250000, by = 50000)
  minorT <- seq(0, 260000, by = 10000)
  axis(axisID,
       at = majorT,
       lwd = 2,
       labels = FALSE)
  axis(
    axisID,
    at = minorT,
    lwd = 1,
    labels = FALSE,
    tck = -0.015
  )
  axis(
    axisID,
    at = majorT,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = c("0", "50K", "100K", "150K", "200K", "250K")
  )
}

autoaxisTHist <- function(axisID) {
  majorT <- axis(2, labels = FALSE, lwd = 0)
  minorT <-
    seq(0, max(majorT) * 1.2, by = majorT[1] + majorT[2] / 5)
  axis(2,
       at = majorT,
       lwd = 2,
       labels = FALSE)
  axis(
    2,
    at = minorT,
    lwd = 1,
    labels = FALSE,
    tck = -0.015
  )
  shortL <- NULL
  for (i in seq(majorT)) {
    if (majorT[i] >= 1000) {
      n <- majorT[i]
      n2 <- n - as.numeric(substr(n, nchar(n) - 2, nchar(n)))
      diminishedNumber <- n - n2
      if (diminishedNumber / 1000 == 0) {
        shortL[[i]] <- paste0(n2 / 1000, "K")
      } else {
        shortL[[i]] <- paste0(n2 / 1000, ".", diminishedNumber / 100, "K")
      }
    } else {
      shortL[[i]] <- majorT[i]
    }
  }
  axis(
    2,
    at = majorT,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = as.character(majorT)
  )
}

autoaxisdTOv <- function(axisID) {
  majorT <- axis(2, labels = FALSE, lwd = 0)
  axis(2,
       at = majorT,
       lwd = 2,
       labels = FALSE)
  axis(
    2,
    at = seq(0, 1.2, by = majorT[1] + majorT[2] / 4),
    lwd = 1,
    tck = -0.015,
    labels = FALSE
  )
  axis(
    2,
    at = majorT,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = (majorT * 100)
  )
}

autoaxisTtSNE <- function(xLim, yLim) {
  seqX1 <- seq(xLim[1] * 0.9, xLim[2] * 0.9, length.out = 6)
  seqX2 <- seq(xLim[1] * 0.9, xLim[2] * 0.9, length.out = 21)
  axis(1,
       at = seqX1,
       lwd = 2,
       labels = FALSE)
  axis(
    1,
    at = seqX2,
    lwd = 1,
    labels = FALSE,
    tck = -0.015
  )
  axis(
    1,
    at = seqX1,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = as.character(seq(0, 100, by = 20))
  )
  seqY1 <- seq(yLim[1] * 0.9, yLim[2] * 0.9, length.out = 6)
  seqY2 <- seq(yLim[1] * 0.9, yLim[2] * 0.9, length.out = 21)
  axis(2,
       at = seqY1,
       lwd = 2,
       labels = FALSE)
  axis(
    2,
    at = seqY2,
    lwd = 1,
    labels = FALSE,
    tck = -0.015
  )
  axis(
    2,
    at = seqY1,
    lwd = 0,
    cex.axis = 1.1,
    line = -0.3,
    las = 1,
    labels = as.character(seq(0, 100, by = 20))
  )
  title(
    xlab = "t-SNE 1",
    ylab = "t-SNE 2",
    cex.lab = 1.5,
    line = 2.5,
    font.lab = 2
  )
}

##Compensation----
autoGetMarker <-
  function(scale.untransformed,
           flow.gate,
           flow.control,
           asp) {
    if (scale.untransformed)
      expr.data <- flow.control$expr.data.untr
    else
      expr.data <- flow.control$expr.data.tran
    marker.spillover.zero <- rep(0, flow.control$marker.n)
    names(marker.spillover.zero) <- flow.control$marker
    marker.spillover <- lapply(flow.control$sample, function(samp) {
      marker.proper <- samp
      id <-
        which(flow.control$event.sample == samp)[flow.gate[[samp]]]
      marker.proper.expr <- expr.data[id, marker.proper]
      marker.expr.n <- length(marker.proper.expr)
      id2 <- round(marker.expr.n * asp$rlm.trim.factor)
      marker.proper.expr.low <- sort(marker.proper.expr)[id2]
      marker.proper.expr.high <- sort(marker.proper.expr,
                                      decreasing = TRUE)[id2]
      marker.spillover.inte <- marker.spillover.zero
      marker.spillover.coef <- marker.spillover.zero
      
      for (marker in flow.control$marker)
        if (marker == marker.proper)
          marker.spillover.coef[marker] <- 1
      else {
        marker.expr <- expr.data[id, marker]
        marker.expr.low <- sort(marker.expr)[id2]
        marker.expr.high <-
          sort(marker.expr, decreasing = TRUE)[id2]
        Fir <- marker.proper.expr > marker.proper.expr.low
        Sec <- marker.proper.expr < marker.proper.expr.high
        Thi <- marker.expr > marker.expr.low
        For <- marker.expr < marker.expr.high
        expr.trim.idx <- which(Fir & Sec & Thi & For)
        marker.proper.expr.trim <- marker.proper.expr[expr.trim.idx]
        marker.expr.trim <- marker.expr[expr.trim.idx]
        fitRobust <- getFromNamespace("fit.robust.linear.model",
                                      "autospill")
        spillover.model.result <- fitRobust(marker.proper.expr.trim,
                                            marker.expr.trim,
                                            marker.proper,
                                            marker,
                                            asp)
        marker.spillover.inte[marker] <-
          spillover.model.result[1, 1]
        marker.spillover.coef[marker] <-
          spillover.model.result[2, 1]
      }
      c(marker.spillover.inte, marker.spillover.coef)
    })
    marker.spillover <- do.call(rbind, marker.spillover)
    rownames(marker.spillover) <- flow.control$marker
    list(inte = marker.spillover[, 1:flow.control$marker.n],
         coef = marker.spillover[, 1:flow.control$marker.n +
                                   flow.control$marker.n])
  }

autoRefineSpill <-
  function(spill.unt,
           spill.tran,
           f.gate,
           f.contr,
           asp) {
    rs.convergence <- rs.exit <- rs.iter.last <- FALSE
    rs.iter <- 0
    rs.iter.width <- floor(log10(asp$rs.iter.max)) + 1
    rs.lambda <- asp$rs.lambda.coarse
    rs.delta <- -1
    rs.delta.threshold <- asp$rs.delta.threshold.untr
    rs.delta.history <- rep(-1, asp$rs.delta.history.n)
    rs.scale.untransformed <- TRUE
    rs.conv.log <- data.frame(
      iter = numeric(),
      scale = character(),
      lambda = numeric(),
      delta = numeric(),
      delta.max = numeric(),
      delta.change = numeric(),
      stringsAsFactors = FALSE
    )
    spillover.curr <- diag(f.contr$marker.n)
    spillover.update <- spill.unt$coef - diag(f.contr$marker.n)
    while (!rs.exit) {
      spillover.curr <- spillover.curr + spillover.update
      spillover.curr <-
        sweep(spillover.curr, 1, diag(spillover.curr), "/")
      compensation.curr <- solve(spillover.curr)
      spill.curr.orig <- spillover.curr
      rownames(spill.curr.orig) <- f.contr$marker.original
      colnames(spill.curr.orig) <- f.contr$marker.original
      comp.curr.orig <- compensation.curr
      rownames(comp.curr.orig) <- f.contr$marker.original
      colnames(comp.curr.orig) <- f.contr$marker.original
      if ((rs.iter == 0 && asp$rs.save.table.initial)
          || (asp$rs.save.table.every > 0
              && rs.iter %% asp$rs.save.table.every == 0)
          || rs.iter.last) {
        if (!is.null(asp$table.spillover.dir)) {
          tab.spill.name <- ifelse(
            rs.iter.last,
            sprintf("%s.csv", asp$spillover.file.name),
            sprintf(
              "%s_%0*d.csv",
              asp$spillover.file.name,
              rs.iter.width,
              rs.iter
            )
          )
          write.csv(spill.curr.orig,
                    file = file.path(asp$table.spillover.dir, tab.spill.name))
        }
        if (!is.null(asp$table.compensation.dir)) {
          tab.comp.name <- ifelse(
            rs.iter.last,
            sprintf("%s.csv", asp$compensation.file.name),
            sprintf(
              "%s_%0*d.csv",
              asp$compensation.file.name,
              rs.iter.width,
              rs.iter
            )
          )
          write.csv(comp.curr.orig,
                    file = file.path(asp$table.compensation.dir, tab.comp.name))
        }
        plotMat <-
          utils::getFromNamespace("plot.matrix", "autospill")
        if (!is.null(asp$figure.spillover.dir)) {
          fig.spill.lab <- ifelse(rs.iter.last,
                                  "",
                                  sprintf("_%0*d", rs.iter.width, rs.iter))
          plotMat(spillover.curr,
                  TRUE,
                  asp$figure.spillover.dir,
                  fig.spill.lab,
                  f.contr,
                  asp)
        }
        if (!is.null(asp$figure.compensation.dir)) {
          figure.compensation.file.label <- ifelse(rs.iter.last,
                                                   "",
                                                   sprintf("_%0*d", rs.iter.width, rs.iter))
          plotMat(
            compensation.curr,
            FALSE,
            asp$figure.compensation.dir,
            figure.compensation.file.label,
            f.contr,
            asp
          )
        }
      }
      if (rs.scale.untransformed) {
        expr.data.unco <- f.contr$expr.data.untr
        marker.spillover.unco <- spill.unt
      }
      else {
        expr.data.unco <- f.contr$expr.data.tran
        marker.spillover.unco <- spill.tran
      }
      flow.set.comp <- lapply(f.contr$flow.set,
                              compensate,
                              compensation(spill.curr.orig))
      if (!rs.scale.untransformed)
        flow.set.comp <- lapply(flow.set.comp,
                                transform,
                                transformList(names(f.contr$transform),
                                              f.contr$transform))
      getFlow <-
        getFromNamespace("get.flow.expression.data", "autospill")
      expr.data.comp <- getFlow(flow.set.comp, f.contr)
      m <- "internal error: inconsistent event or dye names"
      checkCrit <- getFromNamespace("check.critical", "autospill")
      checkCrit(identical(dimnames(expr.data.comp),
                          dimnames(expr.data.unco)), m)
      if ((rs.iter == 0 && asp$rs.plot.figure.initial)
          || (asp$rs.plot.figure.every > 0
              && rs.iter %% asp$rs.plot.figure.every == 0)
          || rs.iter.last) {
        plot.scatter.figure <- TRUE
        figure.scatter.file.label <- ifelse(rs.iter.last,
                                            "final",
                                            sprintf("%0*d", rs.iter.width, rs.iter))
      }
      else {
        plot.scatter.figure <- FALSE
        figure.scatter.file.label <- NULL
      }
      compensation.error <- autoErr(
        expr.data.unco,
        expr.data.comp,
        marker.spillover.unco,
        rs.scale.untransformed,
        plot.scatter.figure,
        figure.scatter.file.label,
        f.gate,
        f.contr,
        asp
      )
      slope.error <-
        compensation.error$slop - diag(f.contr$marker.n)
      rs.delta.prev <- rs.delta
      rs.delta <- sd(slope.error)
      rs.delta.max <- max(abs(slope.error))
      if (rs.delta.prev >= 0)
        rs.delta.history[rs.iter %% asp$rs.delta.history.n +
                           1] <- rs.delta - rs.delta.prev
      else
        rs.delta.history[rs.iter %% asp$rs.delta.history.n +
                           1] <- -1
      rs.delta.change <- mean(rs.delta.history)
      rs.conv.log[rs.iter + 1, ] <- list(
        rs.iter,
        ifelse(rs.scale.untransformed,
               "linear", "bi-exp"),
        rs.lambda,
        rs.delta,
        rs.delta.max,
        rs.delta.change
      )
      if (asp$verbose) {
        cat(
          sprintf(
            "iter %0*d, %s scale, lambda %.1f, delta %g,
                        delta.max %g, delta.change %g\n",
            rs.iter.width,
            rs.iter,
            ifelse(rs.scale.untransformed,
                   "linear", "bi-exp"),
            rs.lambda,
            rs.delta,
            rs.delta.max,
            rs.delta.change
          )
        )
      }
      if ((rs.iter == 0 && asp$rs.save.table.initial)
          || (asp$rs.save.table.every > 0
              && rs.iter %% asp$rs.save.table.every == 0)
          || rs.iter.last) {
        if (!is.null(asp$table.slope.error.dir)) {
          table.slop.err.name <- ifelse(
            rs.iter.last,
            sprintf("%s.csv", asp$slop.err.name),
            sprintf(
              "%s_%0*d.csv",
              asp$slop.err.name,
              rs.iter.width,
              rs.iter
            )
          )
          write.csv(slope.error,
                    file = file.path(asp$table.slope.error.dir,
                                     table.slop.err.name))
        }
        plotDens <-
          utils::getFromNamespace("plot.density.log", "autospill")
        if (!is.null(asp$figure.slope.error.dir)) {
          fig.slop.err.name <- sprintf("%s%s.png",
                                       asp$slop.err.name,
                                       ifelse(
                                         rs.iter.last,
                                         "",
                                         sprintf("_%0*d",
                                                 rs.iter.width,
                                                 rs.iter)
                                       ))
          plotDens(
            slope.error,
            "compensation error",
            file.path(asp$figure.slope.error.dir,
                      fig.slop.err.name),
            asp
          )
        }
        if (!is.null(asp$table.skewness.dir)) {
          tab.skew.name <- ifelse(
            rs.iter.last,
            sprintf("%s.csv",
                    asp$skewness.file.name),
            sprintf(
              "%s_%0*d.csv",
              asp$skewness.file.name,
              rs.iter.width,
              rs.iter
            )
          )
          write.csv(compensation.error$skew,
                    file = file.path(asp$table.skewness.dir,
                                     tab.skew.name))
        }
        ver <- f.contr$autof.marker.idx
        if (!is.null(asp$figure.skewness.dir)) {
          if (!is.null(ver))
            spillover.skewness <- compensation.error$skew[-ver, -ver]
          else
            spillover.skewness <- compensation.error$skew
          figure.skewness.file.name <- sprintf("%s%s.png",
                                               asp$skewness.file.name,
                                               ifelse(
                                                 rs.iter.last,
                                                 "",
                                                 sprintf("_%0*d",
                                                         rs.iter.width,
                                                         rs.iter)
                                               ))
          plotDens(
            spillover.skewness,
            "spillover skewness",
            file.path(asp$figure.skewness.dir,
                      figure.skewness.file.name),
            asp
          )
        }
      }
      if (rs.scale.untransformed &&
          rs.delta.max < rs.delta.threshold) {
        rs.scale.untransformed <- FALSE
        rs.delta.threshold <- asp$rs.delta.threshold.tran
        rs.lambda <- asp$rs.lambda.coarse
        rs.delta <- -1
        rs.delta.history <- rep(-1, asp$rs.delta.history.n)
        rs.delta.change <- -1
      }
      if (rs.delta.change > -asp$rs.delta.threshold.change &&
          rs.lambda == asp$rs.lambda.coarse) {
        rs.lambda <- asp$rs.lambda.fine
        rs.delta <- -1
        rs.delta.history <- rep(-1, asp$rs.delta.history.n)
        rs.delta.change <- -1
      }
      rs.convergence <- !rs.scale.untransformed &&
        (
          rs.delta.max < rs.delta.threshold ||
            rs.delta.change > -asp$rs.delta.threshold.change
        )
      rs.exit <- (rs.convergence && rs.iter.last) ||
        (rs.delta.change > -asp$rs.delta.threshold.change &&
           rs.scale.untransformed) ||
        (!rs.convergence && rs.iter == asp$rs.iter.max) ||
        rs.iter > asp$rs.iter.max
      rs.iter.last <- rs.convergence
      rs.iter <- rs.iter + 1
      spillover.update <-
        rs.lambda * (slope.error %*% spillover.curr)
    }
    if (!is.null(asp$table.convergence.dir))
      write.csv(
        rs.conv.log,
        file = file.path(
          asp$table.convergence.dir,
          sprintf("%s.csv",
                  asp$convergence.file.name)
        ),
        row.names = FALSE
      )
    if (!is.null(asp$figure.convergence.dir))
      plot_convergence(rs.conv.log, NULL, asp)
    m <- "no convergence in refinement of spillover matrix"
    checkCrit <- getFromNamespace("check.critical", "autospill")
    checkCrit(rs.convergence, m)
    list(
      spillover = spillover.curr,
      compensation = compensation.curr,
      error = compensation.error,
      convergence = rs.conv.log
    )
  }

autoErr <-
  function(unc,
           comp,
           sp.unc,
           scal,
           fig,
           lab,
           f.gate,
           f.cont,
           asp) {
    m.spill.zero <- rep(0, f.cont$marker.n)
    names(m.spill.zero) <- f.cont$marker
    marker.spillover.comp <- lapply(f.cont$sample, function(samp) {
      marker.proper <- samp
      preID <- f.cont$event.sample == samp
      id <- which(f.cont$event.sample == samp)[f.gate[[samp]]]
      mark.p.e.unco <- unc[id, marker.proper]
      mark.p.e.comp <- comp[id, marker.proper]
      mar.exp.n <- length(mark.p.e.unco)
      trim.n <- round(mar.exp.n * asp$rlm.trim.factor)
      mark.p.e.low.unco <- sort(mark.p.e.unco)[trim.n]
      mark.p.e.high.unco <-
        sort(mark.p.e.unco, decreasing = TRUE)[trim.n]
      mark.p.e.low.comp <- sort(mark.p.e.comp)[trim.n]
      mark.p.e.high.comp <-
        sort(mark.p.e.comp, decreasing = TRUE)[trim.n]
      m.spill.c.inte <- m.spill.c.coef <- m.spill.zero
      m.spill.c.slop <- m.spill.c.skew <- m.spill.zero
      for (marker in f.cont$marker) {
        mar.exp.unco <- unc[id, marker]
        mar.exp.comp <- comp[id, marker]
        mar.exp.low.unco <- sort(mar.exp.unco)[trim.n]
        mar.exp.high.unco <-
          sort(mar.exp.unco, decreasing = TRUE)[trim.n]
        mar.exp.low.comp <- sort(mar.exp.comp)[trim.n]
        mar.exp.high.comp <-
          sort(mar.exp.comp, decreasing = TRUE)[trim.n]
        expr.trim.idx.unco <- which(
          mark.p.e.unco > mark.p.e.low.unco
          &
            mark.p.e.unco < mark.p.e.high.unco
          & mar.exp.unco > mar.exp.low.unco
          &
            mar.exp.unco < mar.exp.high.unco
        )
        expr.trim.idx.comp <- which(
          mark.p.e.comp > mark.p.e.low.comp
          &
            mark.p.e.comp < mark.p.e.high.comp
          & mar.exp.comp > mar.exp.low.comp
          &
            mar.exp.comp < mar.exp.high.comp
        )
        mark.p.e.trim.unco <- mark.p.e.unco[expr.trim.idx.unco]
        mar.exp.trim.unco <- mar.exp.unco[expr.trim.idx.unco]
        mark.p.e.trim.comp <- mark.p.e.comp[expr.trim.idx.comp]
        mar.exp.trim.comp <- mar.exp.comp[expr.trim.idx.comp]
        if (marker == marker.proper) {
          m.spill.c.coef[marker] <- 1
          m.spill.c.slop[marker] <- 1
        }
        else {
          fitRobust <- getFromNamespace("fit.robust.linear.model",
                                        "autospill")
          spillover.model.result <- fitRobust(mark.p.e.trim.comp,
                                              mar.exp.trim.comp,
                                              marker.proper,
                                              marker,
                                              asp)
          m.spill.c.inte[marker] <- spillover.model.result[1, 1]
          m.spill.c.coef[marker] <- spillover.model.result[2, 1]
          if (scal) {
            m.spill.c.slop[marker] <- m.spill.c.coef[marker]
            m.spill.c.skew[marker] <- skewness(mar.exp.trim.comp)
          }
          else {
            id1 <- f.cont$marker.original[match(marker, f.cont$marker)]
            id2 <- f.cont$marker.original[match(marker.proper,
                                                f.cont$marker)]
            x.transform.inv <- f.cont$transform.inv[[id1]]
            y.transform.inv <- f.cont$transform.inv[[id2]]
            y1p <- min(mark.p.e.trim.comp)
            y2p <- max(mark.p.e.trim.comp)
            x1p <- m.spill.c.inte[marker] +
              m.spill.c.coef[marker] * y1p
            x2p <- m.spill.c.inte[marker] +
              m.spill.c.coef[marker] * y2p
            if (y1p == y2p || x1p == x2p)
              m.spill.c.slop[marker] <- 0
            else {
              y1 <- y.transform.inv(y1p)
              y2 <- y.transform.inv(y2p)
              x1 <- x.transform.inv(x1p)
              x2 <- x.transform.inv(x2p)
              m.spill.c.slop[marker] <- m.spill.c.coef[marker] *
                (x2 - x1) * (y2p - y1p) / ((x2p - x1p) *
                                             (y2 - y1))
            }
            m.spill.c.skew[marker] <-
              skewness(x.transform.inv(mar.exp.trim.comp))
          }
        }
        if (fig && !is.null(f.cont$figure.scatter.dir)) {
          plotScatt <- utils::getFromNamespace("plot.scatter",
                                               "autospill")
          plotScatt(
            unc[preID, marker],
            unc[preID, marker.proper],
            comp[preID, marker],
            comp[preID, marker.proper],
            sp.unc$inte[marker.proper, marker],
            sp.unc$coef[marker.proper, marker],
            m.spill.c.inte[marker],
            m.spill.c.coef[marker],
            m.spill.c.slop[marker],
            range(c(
              mar.exp.trim.unco, mar.exp.trim.comp
            )),
            range(c(
              mark.p.e.trim.unco, mark.p.e.trim.comp
            )),
            samp,
            marker,
            marker.proper,
            scal,
            lab,
            f.gate,
            f.cont,
            asp
          )
        }
      }
      c(m.spill.c.inte,
        m.spill.c.coef,
        m.spill.c.slop,
        m.spill.c.skew)
    })
    marker.spillover.comp <- do.call(rbind, marker.spillover.comp)
    rownames(marker.spillover.comp) <- f.cont$marker
    preID <- f.cont$marker.n
    list(
      inte = marker.spillover.comp[, 1:preID],
      coef = marker.spillover.comp[, 1:preID + preID],
      slop = marker.spillover.comp[, 1:preID + 2 * preID],
      skew = marker.spillover.comp[, 1:preID + 3 * preID]
    )
  }
###compGen-----
compGen <- function(ID, newMatrix, ov, parent = "root") {
  setwd(ag$filePath)
  plotN <- length(ag$fluoCh) - 1
  mfRow <- c(plotN, plotN)
  par(
    oma = c(1, 2, 2, 1),
    mar = c(0, 0, 0, 0),
    mfrow = mfRow,
    lwd = 2
  )
  cf <- load_cytoframe_from_fcs(ag$fileList[ID])
  originalMatrix <- ag$compDFs[[1]]
  rownames(originalMatrix) <- colnames(originalMatrix)
  if (is.null(newMatrix)) {
    compensate(cf, originalMatrix)
  } else {
    compensate(cf, newMatrix)
  }
  for (i in seq(ag$fluoCh)) {
    trans <- flowjo_biexp_trans(
      channelRange = 4096,
      maxValue = 262144,
      pos = ag$customAxis[[i]][1],
      neg = ag$customAxis[[i]][2],
      widthBasis = ag$customAxis[[i]][3]
    )
    cf <- transform(cf, transformerList(ag$fluoCh[i], trans))
  }
  df <- as.data.frame(cytoframe_to_flowFrame(cf)@exprs)
  uncDF <-
    as.data.frame.array(cytoframe_to_flowFrame(gs_pop_get_data(ag$uncompGSfortable[ID], parent)[[1]])@exprs)
  col <- c(
    "#FFFFCC",
    "#FFEDA0",
    "#FED976",
    "#FEB24C",
    "#FD8D3C",
    "#FC4E2A",
    "#E31A1C",
    "#BD0026",
    "#800026"
  )
  col <- alpha(col, alpha = 0.4)
  byRows <- vapply(seq(plotN), function(i)
    list(as.numeric(newMatrix[i,])[seq_len(i) * -1] * 100), list(plotN))
  byCols <- vapply(seq(plotN), function(i)
    list(as.numeric(newMatrix[, i])[seq_len(i) * -1] * 100), list(plotN))
  same <- vapply(seq(unlist(byRows)), function(i)
    list(c(unlist(byRows)[i], unlist(byCols)[i])), list(2))
  range <- vapply(seq_len(11), function(i)
    list(c(9 * i - 9, 9 * i)), list(2))
  lim <- c(0, 4100)
  id <- 0
  for (i in seq(plotN)) {
    for (j in seq(plotN + 1 - i)) {
      id <- id + 1
      compPlot(i, j, same, range, lim, id, plotN,
               df, uncDF, col, ov)
    }
    if (i != plotN) {
      for (k in seq(i)) {
        plot.new()
      }
    }
  }
}

compPlot <-
  function(i,
           j,
           same,
           range,
           lim,
           id,
           plotN,
           df,
           uncDF,
           col,
           ov) {
    X <- ag$fluoCh[j + i]
    Y <- ag$fluoCh[i]
    set.seed(3)
    if (nrow(df) > 1000) {
      sampledDF <- df[sample.int(nrow(df), 1000),]
      uncsampledDF <- uncDF[sample.int(nrow(df), 1000),]
    } else {
      sampledDF <- df
      uncsampledDF <- uncDF
    }
    dfX <- sampledDF[, X]
    dfY <- sampledDF[, Y]
    plot(
      dfX,
      dfY,
      xaxt = "n",
      yaxt = "n",
      ann = FALSE,
      pch = 20,
      cex = 0.5,
      lwd = 0,
      xlim = lim,
      ylim = lim,
      xaxs = "i",
      yaxs = "i"
    )
    prod <- same[[id]]
    for (l in seq_len(11)) {
      if (max(prod) == 0) {
        bgCol <- "white"
      } else {
        if (max(abs(prod)) > range[[l]][1]
            && max(abs(prod)) <= range[[l]][2]
            && max(abs(prod)) <= 81) {
          bgCol <- col[l]
        } else if ((max(abs(prod)) > 81)) {
          bgCol <- col[9]
        }
      }
    }
    rLim <- par("usr")
    rect(rLim[1], rLim[3], rLim[2], rLim[4], col = bgCol)
    points(
      dfX,
      dfY,
      xaxt = "n",
      yaxt = "n",
      ann = FALSE,
      pch = 20,
      cex = 0.5,
      lwd = 0,
      xlim = lim,
      ylim = lim,
      xaxs = "i",
      yaxs = "i"
    )
    if (ov == TRUE) {
      points(
        uncsampledDF[, X],
        uncsampledDF[, Y],
        col = "grey",
        ann = FALSE,
        xaxt = "n",
        yaxt = "n",
        pch = 20,
        cex = 0.5,
        lwd = 0,
        xlim = lim,
        ylim = lim,
        xaxs = "i",
        yaxs = "i"
      )
    }
    if (length(ag$fluoCh) >= 10) {
      textSize <- 0.6
    } else if (length(ag$fluoCh) < 10 && length(ag$fluoCh) > 4) {
      textSize <- rev(5:9 * 0.12)[length(ag$fluoCh) - 4]
    } else if (length(ag$fluoCh) <= 4) {
      textSize <- 1.2
    }
    if (i == 1) {
      mtext(X,
            side = 3,
            padj = -0.5,
            cex = textSize)
    }
    if (j == 1 || i > j && i == j - 1) {
      mtext(Y,
            side = 2,
            padj = -0.5,
            cex = textSize)
    }
  }

matChoices <- function() {
  ag$choices <- names(ag$compDFs)
  names(ag$choices) <- ag$choices
  allNames <- c()
  for (i in seq(ag$choices)) {
    if (ag$choices[i] == ag$appliedMatrix) {
      allNames[i] <- paste(ag$appliedMatrix, "(applied)")
    } else {
      allNames[i] <- ag$choices[i]
    }
  }
  names(ag$choices) <- allNames
}
##Plot----
newPlot <-
  function(ID,
           X,
           Y,
           parent,
           typ,
           axis,
           font,
           bgDF = NULL,
           bg = FALSE) {
    ff <-
      cytoframe_to_flowFrame(gs_pop_get_data(ag$gs[ID], parent)[[1]])
    if (length(ff@exprs) > 0) {
      if (X == "Time") {
        xLim <- NULL
      } else if (grepl("FS", X) || grepl("SS", X)) {
        xLim <- c(0, 264000)
      } else {
        xLim <- c(0, 4100)
      }
      ag$dF <- as.data.frame.array(ff@exprs)
      dfX <- ag$dF[, X]
      dfX[dfX < 0] <- 0
      
      if (grepl("FS", X) || grepl("SS", X)) {
        dfX[dfX > 264000] <- 264000
      }
      if (typ != "Histogram") {
        newDotPlot(dfX, X, Y, typ, axis, font, bgDF, xLim)
      } else {
        newHist(Y, dfX, axis, font, xLim)
      }
      index <- which(ag$fluoCh == X)
      if (X == "Time") {
        axisTicks(1, "time", ag$customAxis[[index]])
      } else if (grepl("FS", X) || grepl("SS", X)) {
        axisTicks(1, "cont", ag$customAxis[[index]])
      } else {
        axisTicks(1, "log", ag$customAxis[[index]])
      }
      if (axis == TRUE) {
        if (bg == TRUE) {
          lin <- 2
        } else {
          lin <- 3
        }
        id <- which(colnames(ag$gs) == X)
        title(
          xlab = names(ag$ch)[id],
          cex.lab = (1 + font / 10),
          line = lin,
          font.lab = 2
        )
      }
    } else {
      alert(
        paste0(
          "The selected sample has 0 events under the selected
                     parent. Please choose another sample or change the
                     parent."
        )
      )
    }
  }

newDotPlot <- function(dfX, X, Y, typ, axis, font, bgDF, xLim) {
  dfY <- ag$dF[, Y]
  dfY[dfY < 0] <- 0
  if (Y == "Time") {
    yLim <- NULL
  } else if (grepl("FS", Y) || grepl("SS", Y)) {
    yLim <- c(0, 264000)
    dfY[dfY > 264000] <- 264000
  } else {
    yLim <- c(0, 4100)
  }
  if (typ == "Backgating") {
    plot(
      dfX,
      dfY,
      xaxt = "n",
      yaxt = "n",
      ann = FALSE,
      pch = 20,
      cex = 0.5,
      lwd = 0,
      col = "grey",
      xlim = xLim,
      ylim = yLim,
      xaxs = "i",
      yaxs = "i"
    )
    if (!is.null(bgDF)) {
      points(
        bgDF[, X],
        bgDF[, Y],
        col = "red",
        pch = 20,
        cex = 0.5,
        lwd = 0
      )
    }
  } else {
    if (typ != "Contour") {
      if (typ == "Density") {
        col <- c("grey0",
                 "grey20",
                 "grey40",
                 "grey60",
                 "grey80",
                 "grey100")
      } else {
        col <- c(
          "#5E4FA2",
          "#3288BD",
          "#66C2A5",
          "#ABDDA4",
          "#E6F598",
          "#FFFFBF",
          "#FEE08B",
          "#FDAE61",
          "#F46D43",
          "#D53E4F",
          "#9E0142"
        )
      }
      colRamp <- colorRampPalette(col)
      density <-
        suppressWarnings(densCols(dfX, dfY, colramp = colRamp))
      plot(
        dfX,
        dfY,
        xaxt = "n",
        yaxt = "n",
        ann = FALSE,
        pch = 20,
        cex = 0.5,
        lwd = 0,
        col = density,
        xlim = xLim,
        ylim = yLim,
        xaxs = "i",
        yaxs = "i"
      )
    }
    if (typ == "Contour" || typ == "Pseudocolor + Contour") {
      addContour(dfX, dfY, X, Y, typ, xLim, yLim)
    }
  }
  addYAxis(Y, axis, font)
}

addContour <- function(dfX, dfY, X, Y, typ, xLim, yLim) {
  if (nrow(ag$dF) > 5000) {
    set.seed(1)
    sampledDF <- ag$dF[sample.int(nrow(ag$dF), 5000),]
  } else {
    sampledDF <- ag$dF
  }
  ag$z <- kde2d(sampledDF[, X], sampledDF[, Y], n = 100)
  if (typ == "Contour") {
    plot(
      dfX,
      dfY,
      xaxt = "n",
      yaxt = "n",
      ann = FALSE,
      pch = 20,
      cex = 0.5,
      lwd = 0,
      col = "black",
      xlim = xLim,
      ylim = yLim,
      xaxs = "i",
      yaxs = "i"
    )
    for (i in contourLines(ag$z, nlevels = 12)) {
      if (i$level != 5e-08) {
        polygon(i$x, i$y, col = "white", lwd = 1.2)
      }
    }
  } else {
    contour(
      ag$z,
      drawlabels = FALSE,
      xaxt = "n",
      yaxt = "n",
      add = TRUE,
      ann = FALSE,
      lwd = 1.5,
      nlevels = 12
    )
  }
}

newHist <- function(Y, dfX, axis, font, xLim) {
  if (length(dfX) > 1) {
    ag$referenceHist <- hist(dfX, breaks = 20, plot = FALSE)
    refCounts <- ag$referenceHist$counts
    refDensity <- ag$referenceHist$density
    multiplier <- refCounts / refDensity
    ag$densLine <- stats::density(dfX)
    ag$densLine$y <- ag$densLine$y * multiplier[1]
    ag$maxDens <- max(ag$densLine[2]$y)
    if (Y == "Prolif") {
      multiplier <- 1.2
    } else {
      multiplier <- 1.05
    }
    plot(
      ag$densLine,
      xaxt = "n",
      yaxt = "n",
      ann = FALSE,
      xaxs = "i",
      yaxs = "i",
      xlim = xLim,
      ylim = c(0, ag$maxDens * multiplier)
    )
    polygon(ag$densLine,
            col = "grey",
            border = "black",
            lwd = 1)
    axisTicks(2, "histo", NULL)
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
  if (axis == TRUE) {
    splitRefCounts <- strsplit(as.character(max(refCounts)), "")[[1]]
    line <- length(splitRefCounts) * 0.5 + 1.5
    title(
      ylab = "Count",
      cex.lab = (1 + font / 10),
      line = line,
      font.lab = 2
    )
  }
}

detectGate <-
  function(ID, X, Y, par, typ, obs, show, font, bg = FALSE) {
    popPaths <- gs_get_pop_paths(ag$gs)
    found <- NULL
    detectedTypes <- NULL
    if (length(popPaths) > 1) {
      for (i in seq(popPaths)[-1]) {
        gatesandPopsInfo <- gs_pop_get_gate(ag$gs, popPaths[i])[ID][[1]]
        detectedTypes[i] <- class(gatesandPopsInfo)[1]
        infoPar <- gatesandPopsInfo@parameters
        chX <- names(infoPar[1])
        if (typ != "Histogram") {
          if (length(infoPar) > 1) {
            chY <- names(infoPar[2])
          } else {
            chY <- "none"
          }
        } else {
          chY <- "Histogram"
        }
        if (X == chX && Y == chY
            || X == chY && Y == chX
            ||
            typ == "Histogram" &&
            length(infoPar) == 1 && X == chX) {
          detectedID <- which(popPaths == popPaths[i])
          preDetec <- gs_pop_get_count_fast(ag$gs[ID], "freq")
          detectedParent <- preDetec[detectedID - 1][, 3][[1]]
          if (par == detectedParent) {
            found[i - 1] <- gatesandPopsInfo@filterId
          }
        }
      }
      if (obs == "plot" || obs == ">= 4") {
        ag$found <- found[!is.na(found)]
        detectedTypes <- detectedTypes[!is.na(detectedTypes)]
        if (obs == "plot" && !is.null(ag$found)) {
          plotGate(ID, X, Y, typ, detectedTypes, show,
                   font, bg)
        }
      }
    }
  }

plotGate <- function(ID, X, Y, typ, types, show, font, bg = FALSE) {
  for (i in seq(ag$found)) {
    coords <- gs_pop_get_gate(ag$gs, ag$found[i])[[ID]]
    if (names(coords@parameters[1]) == X) {
      x <- 1
      y <- 2
    } else {
      x <- 2
      y <- 1
    }
    axisXLim <- par("usr")[2]
    axisYLim <- par("usr")[4]
    if (typ != "Histogram") {
      gateHist(coords, x, y, axisXLim, show)
      if (is(coords, "rectangleGate")) {
        rect(
          xleft = ag$xLeft,
          xright = ag$xRight,
          ybottom = ag$yBottom,
          ytop = ag$yTop,
          lwd = 3
        )
      }
    } else {
      segments(coords@min[[x]],
               axisYLim * 0.75,
               coords@max[[x]],
               axisYLim * 0.75,
               lwd = 3)
      segments(coords@min[[x]],
               axisYLim * 0.72,
               coords@min[[x]],
               axisYLim * 0.78,
               lwd = 3)
      segments(coords@max[[x]],
               axisYLim * 0.72,
               coords@max[[x]],
               axisYLim * 0.78,
               lwd = 3)
    }
    popPaths <- gs_get_pop_paths(ag$gs)
    popShortPaths <- gs_get_pop_paths(ag$gs, path = 1)
    subpop <- popPaths[which(popShortPaths == ag$found[i])]
    gatedPopInfo <- gs_pop_get_count_fast(ag$gs[ID],
                                          subpopulations = subpop)
    preFreq <- gatedPopInfo[, 4][[1]] * 100 / gatedPopInfo[, 5][[1]]
    freq <- paste0(sprintf("%.1f", preFreq), "%")
    gateLeg(ID, X, Y, typ, show, font, bg, freq, coords, x, i)
  }
}

gateHist <- function(coords, x, y, axisXLim, show) {
  if (is(coords, "polygonGate")) {
    bound <- coords@boundaries
    ag$xLeft <- min(bound[, 1])
    ag$xRight <- max(bound[, 1])
    ag$yBottom <- min(bound[, 2])
    ag$yTop <- max(bound[, 2])
    for (j in seq(nrow(bound))) {
      if (j > 1) {
        segments(bound[, 1][j - 1], bound[, 2][j - 1], bound[, 1][j],
                 bound[, 2][j], lwd = 3)
      }
    }
  } else {
    if (coords@min[[x]] == "-Inf") {
      ag$xLeft <- -1000
    } else {
      ag$xLeft <- coords@min[[x]]
    }
    if (coords@max[[x]] == "Inf") {
      ag$xRight <- par("usr")[2] * 1.1
    } else {
      ag$xRight <- coords@max[[x]]
    }
    if (coords@min[[y]] == "-Inf") {
      ag$yBottom <- -1000
    } else {
      ag$yBottom <- coords@min[[y]]
    }
    if (coords@max[[y]] == "Inf") {
      axisYLim <- par("usr")[4]
      ag$yTop <- axisYLim * 1.1
    } else {
      ag$yTop <- coords@max[[y]]
    }
  }
  if ((ag$yBottom + ag$yTop) / 2 < max(axis(y, labels = FALSE, lwd = 0)) /
      2) {
    if (show == TRUE) {
      ag$yJust <- -0.5
    } else {
      ag$yJust <- -1.2
    }
  } else {
    if (show == TRUE) {
      ag$yJust <- 1.5
    } else {
      ag$yJust <- 2.2
    }
  }
}

gateLeg <-
  function(ID,
           X,
           Y,
           typ,
           show,
           font,
           bg,
           freq,
           coords,
           x,
           i) {
    if (show == TRUE) {
      title <- ag$found[i]
    } else {
      title <- NULL
    }
    col <- rgb(1, 1, 1, 0.6)
    axisYLim <- par("usr")[4]
    if (length(ag$found) == 4) {
      if (bg == TRUE) {
        qXCoords <- c(0.2, 0.8, 0.8, 0.2)
        qYCoords <- c(1.1, 1.1, 0.5, 0.5)
      } else {
        if (names(coords@min)[1] == X) {
          qXCoords <- c(0.1, 0.9, 0.9, 0.1)
          qYCoords <- c(1.05, 1.05, 0.25, 0.25)
        } else {
          qXCoords <- c(0.9, 0.9, 0.1, 0.1)
          qYCoords <- c(0.25, 1.05, 1.05, 0.25)
        }
      }
      if (show == TRUE) {
        if (ag$found[i] == paste0("Q1: ", X, "- ", Y, "+")
            || ag$found[i] == paste0("Q2: ", X, "+ ", Y, "+")
            || ag$found[i] == paste0("Q3: ", X, "+ ", Y, "-")
            || ag$found[i] == paste0("Q4: ", X, "- ", Y, "-")) {
          title <- substring(ag$found[i], 1, 2)
        }
      }
      legX <- par("usr")[2] * qXCoords[i]
      legY <- axisYLim * qYCoords[i]
      legend(
        legX,
        legY,
        title = title,
        legend = freq,
        cex = 1 + font / 10,
        bg = col,
        box.lwd = 0,
        x.intersp = -0.5,
        y.intersp = 0.8,
        text.font = 2,
        xjust = 0.5,
        yjust = 1.5
      )
    } else {
      if (typ != "Histogram") {
        legX <- (ag$xLeft + ag$xRight) / 2
        legY <- (ag$yBottom + ag$yTop) / 2
        legend(
          legX,
          legY,
          title = title,
          legend = freq,
          cex = 1 + font / 10,
          bg = col,
          box.lwd = 0,
          x.intersp = -0.5,
          y.intersp = 0.8,
          text.font = 2,
          xjust = 0.5,
          yjust = ag$yJust
        )
      } else {
        legX <- (coords@min[[x]] + coords@max[[x]]) / 2
        legY <- axisYLim * 0.95
        legend(
          legX,
          legY,
          title = title,
          legend = freq,
          cex = 1 + font / 10,
          bg = col,
          box.lwd = 0,
          x.intersp = -0.5,
          y.intersp = 0.8,
          text.font = 2,
          xjust = 0.5
        )
      }
    }
  }

hideTools <- function() {
  hide("rectang", TRUE, "fade")
  hide("polygon", TRUE, "fade")
  hide("quad", TRUE, "fade")
  hide("interval", TRUE, "fade")
}

secStep <- function() {
  delay(500, shinyjs::show("gateOk", TRUE, "fade"))
  delay(500, shinyjs::show("gateCancel", TRUE, "fade"))
  delay(500, shinyjs::show("drawGateName", TRUE, "fade"))
  disable("gateOk")
}

reAddPops <- function() {
  setwd(ag$filePath)
  popPaths <- gs_get_pop_paths(ag$gs)
  tempEnv$gs <- gs_clone(ag$uncompGS)
  popParents <- gs_pop_get_count_fast(ag$gs[1], "freq")[, 3][[1]]
  popGates <- vapply(popPaths[-1], function(x)
    list(gs_pop_get_gate(ag$gs, x)[1][[1]]),
    list(length(popPaths[-1])))
  compensate(tempEnv$gs, ag$compDFs[ag$appliedMatrix][[1]])
  for (i in seq(popGates)) {
    gs_pop_add(tempEnv$gs, popGates[[i]], parent = popParents[i])
  }
  recompute(tempEnv$gs)
}
##autoPlot----
autonewPlot <-
  function(ID,
           X,
           Y,
           parent,
           autotyp,
           axis,
           font,
           autobgDF = NULL,
           bg = FALSE) {
    ag$auto_ff <-
      cytoframe_to_flowFrame(gs_pop_get_data(ag$auto_gs[ID], parent)[[1]])
    if (length(ag$auto_ff@exprs) > 0) {
      if (X == "Time") {
        xLim <- NULL
      } else if (grepl("FS", X) || grepl("SS", X)) {
        xLim <- c(0, 264000)
      } else {
        xLim <- c(0, 4100)
      }
      ag$auto_dF <- as.data.frame.array(ag$auto_ff@exprs)
      autodfX <- ag$auto_dF[, X]
      autodfX[autodfX < 0] <- 0
      
      if (grepl("FS", X) || grepl("SS", X)) {
        autodfX[autodfX > 264000] <- 264000
      }
      if (autotyp != "Histogram") {
        autonewDotPlot(autodfX, X, Y, autotyp, axis, font, autobgDF, xLim)
      } else {
        autonewHist(Y, autodfX, axis, font, xLim)
      }
      index <- which(ag$fluoCh == X)
      if (X == "Time") {
        autoaxisTicks(1, "time", ag$customAxis[[index]])
      } else if (grepl("FS", X) || grepl("SS", X)) {
        autoaxisTicks(1, "cont", ag$customAxis[[index]])
      } else {
        autoaxisTicks(1, "log", ag$customAxis[[index]])
      }
      if (axis == TRUE) {
        if (bg == TRUE) {
          lin <- 2
        } else {
          lin <- 3
        }
        id <- which(colnames(ag$auto_gs) == X)
        title(
          xlab = names(ag$ch)[id],
          cex.lab = (1 + font / 10),
          line = lin,
          font.lab = 2
        )
      }
    } else {
      alert(
        paste0(
          "The selected sample has 0 events under the selected
                     parent. Please choose another sample or change the
                     parent."
        )
      )
    }
  }

autonewDotPlot <-
  function(autodfX, X, Y, autotyp, axis, font, autobgDF, xLim) {
    autodfY <- ag$auto_dF[, Y]
    autodfY[autodfY < 0] <- 0
    if (Y == "Time") {
      yLim <- NULL
    } else if (grepl("FS", Y) || grepl("SS", Y)) {
      yLim <- c(0, 264000)
      autodfY[autodfY > 264000] <- 264000
    } else {
      yLim <- c(0, 4100)
    }
    if (autotyp == "Backgating") {
      plot(
        autodfX,
        autodfY,
        xaxt = "n",
        yaxt = "n",
        ann = FALSE,
        pch = 20,
        cex = 0.5,
        lwd = 0,
        col = "grey",
        xlim = xLim,
        ylim = yLim,
        xaxs = "i",
        yaxs = "i"
      )
      if (!is.null(autobgDF)) {
        points(
          autobgDF[, X],
          autobgDF[, Y],
          col = "red",
          pch = 20,
          cex = 0.5,
          lwd = 0
        )
      }
    } else {
      if (autotyp != "Contour") {
        if (autotyp == "Density") {
          col <- c("grey0",
                   "grey20",
                   "grey40",
                   "grey60",
                   "grey80",
                   "grey100")
        } else {
          col <- c(
            "#5E4FA2",
            "#3288BD",
            "#66C2A5",
            "#ABDDA4",
            "#E6F598",
            "#FFFFBF",
            "#FEE08B",
            "#FDAE61",
            "#F46D43",
            "#D53E4F",
            "#9E0142"
          )
        }
        colRamp <- colorRampPalette(col)
        density <-
          suppressWarnings(densCols(autodfX, autodfY, colramp = colRamp))
        plot(
          autodfX,
          autodfY,
          xaxt = "n",
          yaxt = "n",
          ann = FALSE,
          pch = 20,
          cex = 0.5,
          lwd = 0,
          col = density,
          xlim = xLim,
          ylim = yLim,
          xaxs = "i",
          yaxs = "i"
        )
      }
      if (autotyp == "Contour" ||
          autotyp == "Pseudocolor + Contour") {
        autoaddContour(autodfX, autodfY, X, Y, autotyp, xLim, yLim)
      }
    }
    autoaddYAxis(Y, axis, font)
  }

autoaddContour <- function(autodfX, autodfY, X, Y, autotyp, xLim, yLim) {
  if (nrow(ag$auto_dF) > 5000) {
    set.seed(1)
    sampledDF2 <- ag$auto_dF[sample.int(nrow(ag$auto_dF), 5000),]
  } else {
    sampledDF2 <- ag$auto_dF
  }
  ag$auto_z <- kde2d(sampledDF2[, X], sampledDF2[, Y], n = 100)
  if (autotyp == "Contour") {
    plot(
      autodfX,
      autodfY,
      xaxt = "n",
      yaxt = "n",
      ann = FALSE,
      pch = 20,
      cex = 0.5,
      lwd = 0,
      col = "black",
      xlim = xLim,
      ylim = yLim,
      xaxs = "i",
      yaxs = "i"
    )
    for (i in contourLines(ag$auto_z, nlevels = 12)) {
      if (i$level != 5e-08) {
        polygon(i$x, i$y, col = "white", lwd = 1.2)
      }
    }
  } else {
    contour(
      ag$auto_z,
      drawlabels = FALSE,
      xaxt = "n",
      yaxt = "n",
      add = TRUE,
      ann = FALSE,
      lwd = 1.5,
      nlevels = 12
    )
  }
}

autonewHist <- function(Y, autodfX, axis, font, xLim) {
  if (length(autodfX) > 1) {
    ag$autoreferenceHist <- hist(autodfX, breaks = 20, plot = FALSE)
    autorefCounts <- ag$autoreferenceHist$counts
    autorefDensity <- ag$autoreferenceHist$density
    automultiplier <- autorefCounts / autorefDensity
    ag$autodensLine <- stats::density(autodfX)
    ag$autodensLine$y <- ag$autodensLine$y * automultiplier[1]
    ag$automaxDens <- max(ag$autodensLine[2]$y)
    if (Y == "Prolif") {
      automultiplier <- 1.2
    } else {
      automultiplier <- 1.05
    }
    plot(
      ag$autodensLine,
      xaxt = "n",
      yaxt = "n",
      ann = FALSE,
      xaxs = "i",
      yaxs = "i",
      xlim = xLim,
      ylim = c(0, ag$automaxDens * automultiplier)
    )
    polygon(ag$autodensLine,
            col = "grey",
            border = "black",
            lwd = 1)
    autoaxisTicks(2, "histo", NULL)
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
  if (axis == TRUE) {
    splitRefCounts <-
      strsplit(as.character(max(autorefCounts)), "")[[1]]
    line <- length(splitRefCounts) * 0.5 + 1.5
    title(
      ylab = "Count",
      cex.lab = (1 + font / 10),
      line = line,
      font.lab = 2
    )
  }
}

autodetectGate <-
  function(ID,
           X,
           Y,
           autopar,
           autotyp,
           obs,
           show,
           font,
           bg = FALSE) {
    autopopPaths <- gs_get_pop_paths(ag$auto_gs)
    autofound <- NULL
    autodetectedTypes <- NULL
    if (length(autopopPaths) > 1) {
      for (i in seq(autopopPaths)[-1]) {
        autogatesandPopsInfo <-
          gs_pop_get_gate(ag$auto_gs, autopopPaths[i])[ID][[1]]
        autodetectedTypes[i] <- class(autogatesandPopsInfo)[1]
        autoinfoPar <- autogatesandPopsInfo@parameters
        chX <- names(autoinfoPar[1])
        if (autotyp != "Histogram") {
          if (length(autoinfoPar) > 1) {
            chY <- names(autoinfoPar[2])
          } else {
            chY <- "none"
          }
        } else {
          chY <- "Histogram"
        }
        if (X == chX && Y == chY
            || X == chY && Y == chX
            ||
            autotyp == "Histogram" &&
            length(autoinfoPar) == 1 && X == chX) {
          autodetectedID <- which(autopopPaths == autopopPaths[i])
          autopreDetec <- gs_pop_get_count_fast(ag$auto_gs[ID], "freq")
          autodetectedParent <- autopreDetec[autodetectedID - 1][, 3][[1]]
          if (autopar == autodetectedParent) {
            autofound[i - 1] <- autogatesandPopsInfo@filterId
          }
        }
      }
      if (obs == "autoplot" || obs == ">= 4") {
        ag$autofound <- autofound[!is.na(autofound)]
        autodetectedTypes <- autodetectedTypes[!is.na(autodetectedTypes)]
        if (obs == "autoplot" && !is.null(ag$autofound)) {
          autoplotGate(ID, X, Y, autotyp, autodetectedTypes, show,
                       font, bg)
        }
      }
    }
  }

autoplotGate <-
  function(ID, X, Y, autotyp, types, show, font, bg = FALSE) {
    for (i in seq(ag$autofound)) {
      autocoords <- gs_pop_get_gate(ag$auto_gs, ag$autofound[i])[[ID]]
      if (names(autocoords@parameters[1]) == X) {
        x <- 1
        y <- 2
      } else {
        x <- 2
        y <- 1
      }
      autoaxisXLim <- par("usr")[2]
      autoaxisYLim <- par("usr")[4]
      if (autotyp != "Histogram") {
        autogateHist(autocoords, x, y, autoaxisXLim, show)
        if (is(autocoords, "polygonGate")) {
          polygon(
            x = c(ag$autoxLeft, ag$autoxRight),
            y = c(ag$autoyBottom, ag$autoyTop),
            lwd = 3
          )
        } else if (is(autocoords, "ellipsoidGate")) {
          ellipse(mu = ag$mean,
                  sigma = ag$cov,
                  lwd = 3)
          
        } else if (is(autocoords, "rectangleGate")) {
          rect(
            xleft = ag$autoxLeft,
            ybottom = ag$autoyBottom,
            xright = ag$autoxRight,
            ytop = ag$autoyTop,
            lwd = 3
          )
        }
        
      } else {
        segments(
          x0 = autocoords@min[[x]],
          y0 = 0,
          x1 = autocoords@min[[x]],
          y1 = 100000,
          lwd = 3
        )
      }
      
      autopopPaths <- gs_get_pop_paths(ag$auto_gs)
      popShortPaths <- gs_get_pop_paths(ag$auto_gs, path = 1)
      subpop <-
        autopopPaths[which(popShortPaths == ag$autofound[i])]
      gatedPopInfo <- gs_pop_get_count_fast(ag$auto_gs[ID],
                                            subpopulations = subpop)
      preFreq <-
        gatedPopInfo[, 4][[1]] * 100 / gatedPopInfo[, 5][[1]]
      freq <- paste0(sprintf("%.1f", preFreq), "%")
      autogateLeg(ID, X, Y, autotyp, show, font, bg, freq, autocoords, x, i)
    }
  }

autogateHist <- function(autocoords, x, y, autoaxisXLim, show) {
  if (is(autocoords, "polygonGate")) {
    autobound <- autocoords@boundaries
    ag$autoxLeft <- min(autobound[, 1])
    ag$autoxRight <- max(autobound[, 1])
    ag$autoyBottom <- min(autobound[, 2])
    ag$autoyTop <- max(autobound[, 2])
    for (j in seq(nrow(autobound))) {
      if (j > 1) {
        segments(autobound[, 1][j - 1], autobound[, 2][j - 1], autobound[, 1][j],
                 autobound[, 2][j], lwd = 3)
      }
    }
  } else if (is(autocoords, "ellipsoidGate")) {
    ag$mean <- autocoords@mean
    ag$cov <- autocoords@cov
    ag$distance <- autocoords@distance
    ag$autoyBottom <- as.numeric(ag$mean[2] - ag$distance * 2)
    ag$autoyTop <- as.numeric(ag$mean[2] + ag$distance * 2)
  }
  else {
    if (autocoords@min[[x]] == "-Inf") {
      ag$autoxLeft <- -1000
    } else {
      ag$autoxLeft <- autocoords@min[[x]]
    }
    if (autocoords@max[[x]] == "Inf") {
      ag$autoxRight <- par("usr")[2] * 1.1
    } else {
      ag$autoxRight <- autocoords@max[[x]]
    }
    if (autocoords@min[[y]] == "-Inf") {
      ag$autoyBottom <- -1000
    } else {
      ag$autoyBottom <- autocoords@min[[y]]
    }
    if (autocoords@max[[y]] == "Inf") {
      autoaxisYLim <- par("usr")[4]
      ag$autoyTop <- autoaxisYLim * 1.1
    } else {
      ag$autoyTop <- autocoords@max[[y]]
    }
  }
  if ((ag$autoyBottom + ag$autoyTop) / 2 < max(axis(y, labels = FALSE, lwd =
                                                    0)) / 2) {
    if (show == TRUE) {
      ag$autoyJust <- -0.5
    } else {
      ag$autoyJust <- -1.2
    }
  } else {
    if (show == TRUE) {
      ag$autoyJust <- 1.5
    } else {
      ag$autoyJust <- 2.2
    }
  }
}

autogateLeg <-
  function(ID,
           X,
           Y,
           autotyp,
           show,
           font,
           bg,
           freq,
           autocoords,
           x,
           i) {
    if (show == TRUE) {
      title <- ag$autofound[i]
    } else {
      title <- NULL
    }
    col <- rgb(1, 1, 1, 0.6)
    autoaxisYLim <- par("usr")[4]
    if (length(ag$autofound) == 4) {
      if (bg == TRUE) {
        autoqXCoords <- c(0.2, 0.8, 0.8, 0.2)
        autoqYCoords <- c(1.1, 1.1, 0.5, 0.5)
      } else {
        if (names(autocoords@min)[1] == X) {
          autoqXCoords <- c(0.1, 0.9, 0.9, 0.1)
          autoqYCoords <- c(1.05, 1.05, 0.25, 0.25)
        } else {
          autoqXCoords <- c(0.9, 0.9, 0.1, 0.1)
          autoqYCoords <- c(0.25, 1.05, 1.05, 0.25)
        }
      }
      if (show == TRUE) {
        if (ag$autofound[i] == paste0("Q1: ", X, "- ", Y, "+")
            || ag$autofound[i] == paste0("Q2: ", X, "+ ", Y, "+")
            || ag$autofound[i] == paste0("Q3: ", X, "+ ", Y, "-")
            || ag$autofound[i] == paste0("Q4: ", X, "- ", Y, "-")) {
          title <- substring(ag$autofound[i], 1, 2)
        }
      }
      autolegX <- par("usr")[2] * autoqXCoords[i]
      autolegY <- autoaxisYLim * autoqYCoords[i]
      legend(
        autolegX,
        autolegY,
        title = title,
        legend = freq,
        cex = 1 + font / 10,
        bg = col,
        box.lwd = 0,
        x.intersp = -0.5,
        y.intersp = 0.8,
        text.font = 2,
        xjust = 0.5,
        yjust = 1.5
      )
    } else {
      if (autotyp != "Histogram") {
        if (is(autocoords, "ellipsoidGate")) {
          autolegX <- as.numeric(ag$mean[1] + ag$distance * 2)
          autolegY <- as.numeric(ag$mean[2] + ag$distance * 2)
          legend(
            autolegX,
            autolegY,
            title = title,
            legend = freq,
            cex = 1 + font / 10,
            bg = col,
            box.lwd = 0,
            x.intersp = -0.5,
            y.intersp = 0.8,
            text.font = 2,
            xjust = 0.5,
            yjust = ag$autoyJust
          )
        } else {
          autolegX <- (ag$autoxLeft + ag$autoxRight) / 2
          autolegY <- (ag$autoyBottom + ag$autoyTop) / 2
          legend(
            autolegX,
            autolegY,
            title = title,
            legend = freq,
            cex = 1 + font / 10,
            bg = col,
            box.lwd = 0,
            x.intersp = -0.5,
            y.intersp = 0.8,
            text.font = 2,
            xjust = 0.5,
            yjust = ag$autoyJust
          )
        }
      } else {
        autolegX <- (autocoords@min[[x]] + autocoords@max[[x]]) / 2
        autolegY <- autoaxisYLim * 0.95
        legend(
          autolegX,
          autolegY,
          title = title,
          legend = freq,
          cex = 1 + font / 10,
          bg = col,
          box.lwd = 0,
          x.intersp = -0.5,
          y.intersp = 0.8,
          text.font = 2,
          xjust = 0.5
        )
      }
    }
  }

autohideTools <- function() {
  hide("autosinglet", TRUE, "fade")
  hide("auto2D", TRUE, "fade")
  hide("autoquad", TRUE, "fade")
  hide("autointerval", TRUE, "fade")
}

autosecStep <- function() {
  delay(500, shinyjs::show("autogateOk", TRUE, "fade"))
  delay(500, shinyjs::show("autogateCancel", TRUE, "fade"))
  delay(500, shinyjs::show("autodrawGateName", TRUE, "fade"))
  disable("autogateOk")
}

autoreAddPops <- function() {
  setwd(ag$filePath)
  autopopPaths <- gs_get_pop_paths(ag$auto_gs)
  tempEnv$auto_gs <- gs_clone(ag$uncompGS)
  autopopParents <-
    gs_pop_get_count_fast(ag$auto_gs[1], "freq")[, 3][[1]]
  autopopGates <- vapply(autopopPaths[-1], function(x)
    list(gs_pop_get_gate(ag$auto_gs, x)[1][[1]]),
    list(length(autopopPaths[-1])))
  compensate(tempEnv$auto_gs, ag$compDFs[ag$appliedMatrix][[1]])
  for (i in seq(autopopGates)) {
    gs_pop_add(tempEnv$auto_gs, autopopGates[[i]], parent = autopopParents[i])
  }
  recompute(tempEnv$auto_gs)
}

##Ancestry----
ancestryGen <- function(typ, previewSamp, bgPop) {
  par(mfrow = c(3, 5),
      mar = c(4, 5, 2, 1) + 0.1,
      lwd = 2)
  if (typ != "" && previewSamp != "") {
    ID <- match(previewSamp, ag$fileList)
    pop <- bgPop
    if (typ == "Backgating" && pop != "") {
      cs <- gs_pop_get_data(ag$gs[ID], pop)
      ff <- cytoframe_to_flowFrame(cs[[1]])
      bgDF <- as.data.frame.array(ff@exprs)
    } else {
      bgDF <- NULL
    }
    popPaths <- gs_get_pop_paths(ag$gs)
    gates <- vapply(popPaths[-1], function(x)
      list(gs_pop_get_gate(ag$gs, x)[[1]]), list(1))
    gateChs <- vapply(gates, function(x)
      list(names(x@parameters)), list(1))
    seq1 <- seq(gateChs)[duplicated(gateChs)]
    seq2 <- seq(gateChs)[duplicated(gateChs, fromLast = TRUE)]
    duplicateIDs <- sort(unique(c(seq1, seq2)))
    data <- gs_pop_get_count_fast(ag$gs[[1]])
    duplicateParents <- data[, "Parent"][[1]][duplicateIDs]
    if (length(duplicateIDs) > 0) {
      index <- duplicateIDs[duplicated(duplicateParents)]
      plotSeq <- seq(gateChs)[-index]
    } else {
      plotSeq <- seq(gateChs)
    }
    dat <- data.frame(x = numeric(0), y = numeric(0))
    acPlotGen(dat, plotSeq, gates, popPaths, typ, ID, bgDF, pop)
    enable("exportImageAncestry")
  } else {
    disable("bgPop")
  }
}

acPlotGen <-
  function(dat,
           plotSeq,
           gates,
           popPaths,
           typ,
           ID,
           bgDF,
           pop) {
    max <- length(plotSeq) / 10
    withProgress(
      message = "Generating",
      detail = "plot 0",
      value = 0,
      max = max,
      {
        for (i in (plotSeq + 1)) {
          dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
          detail <- paste("plot", which((plotSeq + 1) == i))
          incProgress(0.1, detail = detail)
          Sys.sleep(0.1)
          gateInfo <- gates[popPaths[i]][[1]]
          xCh <- names(gateInfo@parameters[1])
          if (length(gateInfo@parameters) > 1) {
            yCh <- names(gateInfo@parameters[2])
            type <- typ
          } else {
            yCh <- "Histogram"
            type <- "Histogram"
          }
          shortPath <- gs_get_pop_paths(ag$gs, path = 1)
          index <- which(shortPath == gateInfo@filterId)
          gateFullName <- popPaths[index]
          if (i > 2) {
            stSp <- strsplit(gateFullName, split = "/")
            end <- tail(stSp[[1]], 2)[1]
            i2 <- which(shortPath == end)
            parent <- popPaths[i2]
          } else {
            parent <- "root"
          }
          newPlot(ID, xCh, yCh, parent, type, TRUE, 2, bgDF, TRUE)
          detectGate(ID, xCh, yCh, parent, type, "plot", TRUE, 2, TRUE)
          if (parent == "root") {
            parent <- "ungated"
          } else {
            parent <- shortPath[which(popPaths == parent)]
          }
          if (typ == "Backgating" && pop != "") {
            if (!is.null(bgDF)) {
              if (parent == pop) {
                title(main = list(parent, cex = 1.5, col = "red"))
              } else {
                title(main = list(parent, cex = 1.5, col = "black"))
              }
            }
          } else {
            title(main = list(parent, cex = 1.5, col = "black"))
          }
        }
      }
    )
  }

##Auto Ancestry----
autoancestryGen <- function(autotyp, autopreviewSamp, autobgPop) {
  par(mfrow = c(3, 5),
      mar = c(4, 5, 2, 1) + 0.1,
      lwd = 2)
  if (autotyp != "" && autopreviewSamp != "") {
    autoID <- match(autopreviewSamp, ag$fileList)
    autopop <- autobgPop
    if (autotyp == "Backgating" && autopop != "") {
      autocs <- gs_pop_get_data(ag$auto_gs[autoID], autopop)
      autoff <- cytoframe_to_flowFrame(autocs[[1]])
      autobgDF <- as.data.frame.array(autoff@exprs)
    } else {
      autobgDF <- NULL
    }
    autopopPaths <- gs_get_pop_paths(ag$auto_gs)
    autogates <- vapply(autopopPaths[-1], function(x)
      list(gs_pop_get_gate(ag$auto_gs, x)[[1]]), list(1))
    autogateChs <- vapply(autogates, function(x)
      list(names(x@parameters)), list(1))
    autoseq1 <- seq(autogateChs)[duplicated(autogateChs)]
    autoseq2 <- seq(autogateChs)[duplicated(autogateChs, fromLast = TRUE)]
    autoduplicateIDs <- sort(unique(c(autoseq1, autoseq2)))
    autodata <- gs_pop_get_count_fast(ag$auto_gs[[1]])
    autoduplicateParents <- autodata[, "Parent"][[1]][autoduplicateIDs]
    if (length(autoduplicateIDs) > 0) {
      index <- autoduplicateIDs[duplicated(autoduplicateParents)]
      autoplotSeq <- seq(autogateChs)[-index]
    } else {
      autoplotSeq <- seq(autogateChs)
    }
    autodat <- data.frame(x = numeric(0), y = numeric(0))
    autoacPlotGen(autodat, autoplotSeq, autogates, autopopPaths, autotyp, autoID, autobgDF, autopop)
    enable("autoexportImageAncestry")
  } else {
    disable("autobgPop")
  }
}

autoacPlotGen <-
  function(autodat,
           autoplotSeq,
           autogates,
           autopopPaths,
           autotyp,
           autoID,
           autobgDF,
           autopop) {
    max <- length(autoplotSeq) / 10
    withProgress(
      message = "Generating",
      detail = "plot 0",
      value = 0,
      max = max,
      {
        for (i in (autoplotSeq + 1)) {
          autodat <- rbind(autodat, data.frame(x = rnorm(1), y = rnorm(1)))
          detail <- paste("plot", which((autoplotSeq + 1) == i))
          incProgress(0.1, detail = detail)
          Sys.sleep(0.1)
          autogateInfo <- autogates[autopopPaths[i]][[1]]
          auto_xCh <- names(autogateInfo@parameters[1])
          if (length(autogateInfo@parameters) > 1) {
            auto_yCh <- names(autogateInfo@parameters[2])
            autotype <- autotyp
          } else {
            auto_yCh <- "Histogram"
            autotype <- "Histogram"
          }
          shortPath <- gs_get_pop_paths(ag$auto_gs, path = 1)
          index <- which(shortPath == autogateInfo@filterId)
          autogateFullName <- autopopPaths[index]
          if (i > 2) {
            autostSp <- strsplit(autogateFullName, split = "/")
            autoend <- tail(autostSp[[1]], 2)[1]
            autoi2 <- which(shortPath == autoend)
            parent <- autopopPaths[autoi2]
          } else {
            parent <- "root"
          }
          autonewPlot(autoID, auto_xCh, auto_yCh, parent, autotype, TRUE, 2, autobgDF, TRUE)
          autodetectGate(autoID, auto_xCh, auto_yCh, parent, autotype, "autoplot", TRUE, 2, TRUE)
          if (parent == "root") {
            parent <- "ungated"
          } else {
            parent <- shortPath[which(autopopPaths == parent)]
          }
          if (autotyp == "Backgating" && autopop != "") {
            if (!is.null(autobgDF)) {
              if (parent == autopop) {
                title(main = list(parent, cex = 1.5, col = "red"))
              } else {
                title(main = list(parent, cex = 1.5, col = "black"))
              }
            }
          } else {
            title(main = list(parent, cex = 1.5, col = "black"))
          }
        }
      }
    )
  }


##Overlays----
overlay <- function(IDs, X, Y, parent, typ, tone, axis, font) {
  cfs <- vapply(IDs, function(i)
    list(gs_pop_get_data(ag$gs[i], parent)[[1]]), list(length(IDs)))
  shortName <- substr(ag$fileList, 1, nchar(ag$fileList) - 4)
  ag$sampleNam <- shortName[IDs]
  if (all(sapply(cfs, function(x)
    nrow(x) > 0))) {
    par(mar = c(4, 6, 1, 25), lwd = 2)
    if (X == "Time") {
      ag$xLim <- NULL
    } else if (grepl("FS", X) || grepl("SS", X)) {
      ag$xLim <- c(0, 264000)
    } else {
      ag$xLim <- c(0, 4100)
    }
    setColor(IDs, typ, tone)
    dfs <- vapply(cfs, function(x)
      list(as.data.frame(cytoframe_to_flowFrame(x)@exprs)),
      list(length(cfs)))
    dfsX <- vapply(dfs, function(x)
      list(x[, X]), list(length(dfs)))
    dfsX <- lapply(dfsX, function(x) {
      x[x < 0] <- 0
      x
    })
    if (typ == "Dot plot") {
      ovDotPlot(IDs, Y, dfs, dfsX)
    } else {
      ovHist(IDs, dfsX, typ)
    }
    multip <- 1.2
    ovAxis(IDs, X, Y, typ, axis, font, multip)
  } else {
    alert(
      paste0(
        "The selected sample has 0 events under the selected
                     parent. Please choose another sample or change the
                     parent."
      )
    )
  }
}

setColor <- function(IDs, typ, tone) {
  if (typ == "Overlaid histogram") {
    transparency <- 0.6
    ag$colorList <- c(
      "grey" = "transparent",
      "transparent" = "black",
      "transparent" = "black"
    )[seq_len(length(IDs))]
    ag$lty <- c(0, 1, 2)
    ag$lwd <- c(1, 3, 2)
  } else if (typ == "Offset histogram") {
    transparency <- 0.5
    ag$yLim <- c(0, 0)
    ag$offsets <- c(0)
    if (tone == "Colorful") {
      preColors <- c(
        "grey",
        "red2",
        "green4",
        "blue3",
        "orange",
        "black",
        "wheat3",
        "brown",
        "orchid3",
        "salmon"
      )
    } else {
      prepreColors <- c(
        "grey0",
        "grey10",
        "grey20",
        "grey30",
        "grey40",
        "grey50",
        "grey60",
        "grey70",
        "grey80",
        "grey90"
      )
      multip <- length(prepreColors) / length(IDs)
      preColors <- vapply(seq(IDs), function(i)
        prepreColors[multip * i], character(1))
      preColors[1] <- prepreColors[1]
      preColors[length(IDs)] <- prepreColors[length(prepreColors)]
    }
    alphaColors <- vapply(seq(IDs), function(i)
      rgb(
        col2rgb(preColors[i])[1],
        col2rgb(preColors[i])[2],
        col2rgb(preColors[i])[3],
        255 * transparency,
        maxColorValue = 255
      ),
      character(1))
    ag$colorList <- rep("black", length(IDs))
    names(ag$colorList) <- alphaColors
    ag$lty <- ag$lwd <- rep(1, length(IDs))
  } else if (typ == "Dot plot") {
    if (tone == "Colorful") {
      ag$colorList <- c("black", "red2")
    } else {
      ag$colorList <- c("black", "grey")
    }
  }
}

ovDotPlot <- function(IDs, Y, dfs, dfsX) {
  dfsY <- vapply(dfs, function(y)
    list(y[, Y]), list(length(dfs)))
  dfsY <- lapply(dfsY, function(y) {
    y[y < 0] <- 0
    y
  })
  if (Y == "Time") {
    ag$yLim <- NULL
  } else if (grepl("FS", Y) || grepl("SS", Y)) {
    ag$yLim <- c(0, 264000)
  } else {
    ag$yLim <- c(0, 4100)
  }
  dfFinal <- data.frame("X" = dfsX[[1]],
                        "Y" = dfsY[[1]],
                        "color" = ag$colorList[1])
  dfFinal <- rbind(dfFinal,
                   data.frame(
                     "X" = dfsX[[2]],
                     "Y" = dfsY[[2]],
                     "color" = ag$colorList[2]
                   ))
  set.seed(2)
  shuffledDF <- dfFinal[sample(nrow(dfFinal)), ]
  plot(
    shuffledDF[[1]],
    shuffledDF[[2]],
    xaxt = "n",
    yaxt = "n",
    ann = FALSE,
    pch = 20,
    cex = 0.5,
    lwd = 0,
    col = shuffledDF[[3]],
    xlim = ag$xLim,
    ylim = ag$yLim,
    xaxs = "i",
    yaxs = "i"
  )
}

ovHist <- function(IDs, dfsX, typ) {
  for (i in seq(IDs)) {
    densLine <- density(dfsX[[i]])
    densLine$y <- densLine$y / max(densLine$y)
    maxDens <- max(densLine$y)
    if (typ == "Overlaid histogram") {
      ag$yLim <- c(0, maxDens * 1.05)
      if (i == 1) {
        plot(
          densLine,
          col = "transparent",
          xaxt = "n",
          yaxt = "n",
          ann = FALSE,
          xlim = ag$xLim,
          ylim = c(0, maxDens * 1.05),
          xaxs = "i",
          yaxs = "i"
        )
      }
      polygon(
        densLine,
        col = names(ag$colorList[i]),
        border = ag$colorList[i],
        lwd = ag$lwd[i],
        lty = ag$lty[i]
      )
    } else {
      if (i != 1) {
        ag$offsets[i] <- ag$yLim[2]
      }
      ag$yLim[2] <- ag$yLim[2] + maxDens * 0.7
    }
  }
  if (typ == "Offset histogram") {
    ag$yLim[2] <- ag$yLim[2] + maxDens * 0.7 / 2 * 1.1
    plot(
      0,
      xaxt = "n",
      yaxt = "n",
      ann = FALSE,
      xlim = ag$xLim,
      ylim = ag$yLim,
      xaxs = "i",
      yaxs = "i",
      cex = 0
    )
    for (i in rev(seq(IDs))) {
      densLine <- stats::density(dfsX[[i]])
      densLine$y <- densLine$y / max(densLine$y) + ag$offsets[i]
      polygon(
        densLine,
        col = names(ag$colorList[i]),
        border = NA,
        lwd = 1.5,
        lty = 1
      )
      lines(densLine, lwd = 1.5, lty = 1)
    }
  }
}

ovAxis <- function(IDs, X, Y, typ, axis, font, multip) {
  if (X == "Time") {
    axisTicks(1, "time", NULL)
  } else if (grepl("FS", X) || grepl("SS", X)) {
    axisTicks(1, "cont", NULL)
  } else {
    axisTicks(1, "log", ag$customAxis[[which(ag$fluoCh == X)]])
  }
  if (typ == "Dot plot") {
    if (Y == "Time") {
      distance <- 2.8
      axisTicks(2, "time", NULL)
    } else if (grepl("FS", Y) || grepl("SS", Y)) {
      distance <- 3.3
      axisTicks(2, "cont", NULL)
    } else {
      distance <- 2.8
      axisTicks(2, "log", ag$customAxis[[which(ag$fluoCh == Y)]])
    }
  } else if (typ == "Overlaid histogram") {
    axisTicks(2, typ, NULL)
    pch <- c(22, NA, NA)[seq_len(length(IDs))]
    lty <- c(0, 1, 2)[seq_len(length(IDs))]
  }
  if (axis == TRUE) {
    xLab <- names(ag$ch)[which(colnames(ag$gs) == X)]
    title(
      xlab = xLab,
      cex.lab = (1 + font / 10),
      line = 3,
      font.lab = 2
    )
    if (typ == "Overlaid histogram") {
      title(
        ylab = "% of max",
        cex.lab = (1 + font / 10),
        line = 2.5,
        font.lab = 2
      )
    } else if (typ != "Offset histogram") {
      yLab <- names(ag$ch)[which(colnames(ag$gs) == Y)]
      title(
        ylab = yLab,
        cex.lab = (1 + font / 10),
        line = distance,
        font.lab = 2
      )
    }
  }
  if (typ != "Overlaid histogram") {
    pch <- 22
    lty <- 0
  }
  if (typ == "Overlaid histogram" || typ == "Offset histogram") {
    ptBg <- names(rev(ag$colorList))
  } else {
    ptBg <- rev(ag$colorList)
  }
  leg <-
    legend(
      "topright",
      bty = "n",
      inset = c(-0.25, -0.025),
      pch = rev(pch),
      text.width = strwidth("100"),
      lty = rev(lty),
      lwd = 2,
      cex = 1.3,
      pt.cex = 4,
      y.intersp = 1.6,
      pt.bg = ptBg,
      col = rev(ag$colorList),
      legend = rep(NA, length(ag$sampleNam)),
      xpd = TRUE
    )
  text(
    strwidth(rev(ag$sampleNam)) * multip + leg$text$x * 1.01,
    leg$text$y,
    rev(ag$sampleNam),
    pos = 2,
    xpd = NA,
    font = 2,
    cex = multip
  )
}
##Auto Overlays----
autooverlay <-
  function(IDs, X, Y, parent, autotyp, tone, axis, font) {
    cfs <- vapply(IDs, function(i)
      list(gs_pop_get_data(ag$auto_gs[i], parent)[[1]]), list(length(IDs)))
    shortName <- substr(ag$fileList, 1, nchar(ag$fileList) - 4)
    ag$sampleNam <- shortName[IDs]
    if (all(sapply(cfs, function(x)
      nrow(x) > 0))) {
      par(mar = c(4, 6, 1, 25), lwd = 2)
      if (X == "Time") {
        ag$xLim <- NULL
      } else if (grepl("FS", X) || grepl("SS", X)) {
        ag$xLim <- c(0, 264000)
      } else {
        ag$xLim <- c(0, 4100)
      }
      autosetColor(IDs, autotyp, tone)
      dfs <- vapply(cfs, function(x)
        list(as.data.frame(cytoframe_to_flowFrame(x)@exprs)),
        list(length(cfs)))
      dfsX <- vapply(dfs, function(x)
        list(x[, X]), list(length(dfs)))
      dfsX <- lapply(dfsX, function(x) {
        x[x < 0] <- 0
        x
      })
      if (autotyp == "Dot plot") {
        autoOvDotPlot(IDs, Y, dfs, dfsX)
      } else {
        autoOvHist(IDs, dfsX, autotyp)
      }
      multip <- 1.2
      autoOvAxis(IDs, X, Y, autotyp, axis, font, multip)
    } else {
      alert(
        paste0(
          "The selected sample has 0 events under the selected
                     parent. Please choose another sample or change the
                     parent."
        )
      )
    }
  }

autosetColor <- function(IDs, autotyp, tone) {
  if (autotyp == "Overlaid histogram") {
    transparency <- 0.6
    ag$colorList <- c(
      "grey" = "transparent",
      "transparent" = "black",
      "transparent" = "black"
    )[seq_len(length(IDs))]
    ag$lty <- c(0, 1, 2)
    ag$lwd <- c(1, 3, 2)
  } else if (autotyp == "Offset histogram") {
    transparency <- 0.5
    ag$yLim <- c(0, 0)
    ag$offsets <- c(0)
    if (tone == "Colorful") {
      preColors <- c(
        "grey",
        "red2",
        "green4",
        "blue3",
        "orange",
        "black",
        "wheat3",
        "brown",
        "orchid3",
        "salmon"
      )
    } else {
      prepreColors <- c(
        "grey0",
        "grey10",
        "grey20",
        "grey30",
        "grey40",
        "grey50",
        "grey60",
        "grey70",
        "grey80",
        "grey90"
      )
      multip <- length(prepreColors) / length(IDs)
      preColors <- vapply(seq(IDs), function(i)
        prepreColors[multip * i], character(1))
      preColors[1] <- prepreColors[1]
      preColors[length(IDs)] <- prepreColors[length(prepreColors)]
    }
    alphaColors <- vapply(seq(IDs), function(i)
      rgb(
        col2rgb(preColors[i])[1],
        col2rgb(preColors[i])[2],
        col2rgb(preColors[i])[3],
        255 * transparency,
        maxColorValue = 255
      ),
      character(1))
    ag$colorList <- rep("black", length(IDs))
    names(ag$colorList) <- alphaColors
    ag$lty <- ag$lwd <- rep(1, length(IDs))
  } else if (autotyp == "Dot plot") {
    if (tone == "Colorful") {
      ag$colorList <- c("black", "red2")
    } else {
      ag$colorList <- c("black", "grey")
    }
  }
}

autoOvDotPlot <- function(IDs, Y, dfs, dfsX) {
  dfsY <- vapply(dfs, function(y)
    list(y[, Y]), list(length(dfs)))
  dfsY <- lapply(dfsY, function(y) {
    y[y < 0] <- 0
    y
  })
  if (Y == "Time") {
    ag$yLim <- NULL
  } else if (grepl("FS", Y) || grepl("SS", Y)) {
    ag$yLim <- c(0, 264000)
  } else {
    ag$yLim <- c(0, 4100)
  }
  dfFinal <- data.frame("X" = dfsX[[1]],
                        "Y" = dfsY[[1]],
                        "color" = ag$colorList[1])
  dfFinal <- rbind(dfFinal,
                   data.frame(
                     "X" = dfsX[[2]],
                     "Y" = dfsY[[2]],
                     "color" = ag$colorList[2]
                   ))
  set.seed(2)
  shuffledDF <- dfFinal[sample(nrow(dfFinal)), ]
  plot(
    shuffledDF[[1]],
    shuffledDF[[2]],
    xaxt = "n",
    yaxt = "n",
    ann = FALSE,
    pch = 20,
    cex = 0.5,
    lwd = 0,
    col = shuffledDF[[3]],
    xlim = ag$xLim,
    ylim = ag$yLim,
    xaxs = "i",
    yaxs = "i"
  )
}

autoOvHist <- function(IDs, dfsX, autotyp) {
  for (i in seq(IDs)) {
    densLine <- density(dfsX[[i]])
    densLine$y <- densLine$y / max(densLine$y)
    maxDens <- max(densLine$y)
    if (autotyp == "Overlaid histogram") {
      ag$yLim <- c(0, maxDens * 1.05)
      if (i == 1) {
        plot(
          densLine,
          col = "transparent",
          xaxt = "n",
          yaxt = "n",
          ann = FALSE,
          xlim = ag$xLim,
          ylim = c(0, maxDens * 1.05),
          xaxs = "i",
          yaxs = "i"
        )
      }
      polygon(
        densLine,
        col = names(ag$colorList[i]),
        border = ag$colorList[i],
        lwd = ag$lwd[i],
        lty = ag$lty[i]
      )
    } else {
      if (i != 1) {
        ag$offsets[i] <- ag$yLim[2]
      }
      ag$yLim[2] <- ag$yLim[2] + maxDens * 0.7
    }
  }
  if (autotyp == "Offset histogram") {
    ag$yLim[2] <- ag$yLim[2] + maxDens * 0.7 / 2 * 1.1
    plot(
      0,
      xaxt = "n",
      yaxt = "n",
      ann = FALSE,
      xlim = ag$xLim,
      ylim = ag$yLim,
      xaxs = "i",
      yaxs = "i",
      cex = 0
    )
    for (i in rev(seq(IDs))) {
      densLine <- stats::density(dfsX[[i]])
      densLine$y <- densLine$y / max(densLine$y) + ag$offsets[i]
      polygon(
        densLine,
        col = names(ag$colorList[i]),
        border = NA,
        lwd = 1.5,
        lty = 1
      )
      lines(densLine, lwd = 1.5, lty = 1)
    }
  }
}

autoOvAxis <- function(IDs, X, Y, autotyp, axis, font, multip) {
  if (X == "Time") {
    autoaxisTicks(1, "time", NULL)
  } else if (grepl("FS", X) || grepl("SS", X)) {
    autoaxisTicks(1, "cont", NULL)
  } else {
    autoaxisTicks(1, "log", ag$customAxis[[which(ag$fluoCh == X)]])
  }
  if (autotyp == "Dot plot") {
    if (Y == "Time") {
      distance <- 2.8
      autoaxisTicks(2, "time", NULL)
    } else if (grepl("FS", Y) || grepl("SS", Y)) {
      distance <- 3.3
      autoaxisTicks(2, "cont", NULL)
    } else {
      distance <- 2.8
      autoaxisTicks(2, "log", ag$customAxis[[which(ag$fluoCh == Y)]])
    }
  } else if (autotyp == "Overlaid histogram") {
    autoaxisTicks(2, autotyp, NULL)
    pch <- c(22, NA, NA)[seq_len(length(IDs))]
    lty <- c(0, 1, 2)[seq_len(length(IDs))]
  }
  if (axis == TRUE) {
    xLab <- names(ag$ch)[which(colnames(ag$auto_gs) == X)]
    title(
      xlab = xLab,
      cex.lab = (1 + font / 10),
      line = 3,
      font.lab = 2
    )
    if (autotyp == "Overlaid histogram") {
      title(
        ylab = "% of max",
        cex.lab = (1 + font / 10),
        line = 2.5,
        font.lab = 2
      )
    } else if (autotyp != "Offset histogram") {
      yLab <- names(ag$ch)[which(colnames(ag$auto_gs) == Y)]
      title(
        ylab = yLab,
        cex.lab = (1 + font / 10),
        line = distance,
        font.lab = 2
      )
    }
  }
  if (autotyp != "Overlaid histogram") {
    pch <- 22
    lty <- 0
  }
  if (autotyp == "Overlaid histogram" ||
      autotyp == "Offset histogram") {
    ptBg <- names(rev(ag$colorList))
  } else {
    ptBg <- rev(ag$colorList)
  }
  leg <-
    legend(
      "topright",
      bty = "n",
      inset = c(-0.25, -0.025),
      pch = rev(pch),
      text.width = strwidth("100"),
      lty = rev(lty),
      lwd = 2,
      cex = 1.3,
      pt.cex = 4,
      y.intersp = 1.6,
      pt.bg = ptBg,
      col = rev(ag$colorList),
      legend = rep(NA, length(ag$sampleNam)),
      xpd = TRUE
    )
  text(
    strwidth(rev(ag$sampleNam)) * multip + leg$text$x * 1.01,
    leg$text$y,
    rev(ag$sampleNam),
    pos = 2,
    xpd = NA,
    font = 2,
    cex = multip
  )
}
##Proliferation----
prolifGen <- function(prolifSB, show, font, ready, lab, grid) {
  undividedGate <- gs_pop_get_gate(ag$gs, "undivided")[[1]]
  ag$prolifChannel <- names(undividedGate@parameters)
  popPaths <- gs_get_pop_paths(ag$gs)
  popShortPaths <- gs_get_pop_paths(ag$gs, path = 1)
  fullUndivided <- popPaths[which(popShortPaths == "undivided")]
  ag$prolifParent <- gs_pop_get_count_fast(ag$gs[ag$sampleList[1]])
  ID <- which(ag$prolifParent[, 2] == fullUndivided)
  ag$prolifParent <- ag$prolifParent[, 3][ID][[1]]
  par(mar = c(4, 6, 1, 1) + 0.1, lwd = 2)
  ID <- match(prolifSB, ag$fileList)
  newPlot(ID,
          ag$prolifChannel,
          "Prolif",
          ag$prolifParent,
          "Histogram",
          show,
          font)
  shinyjs::show("prolifSButt")
  shinyjs::show("prevProlifS")
  shinyjs::show("nextProlifS")
  if (ready == FALSE) {
    prolifNotReady(undividedGate)
  } else {
    hide("applyProlif")
    disable("step1")
    disable("step2")
    shinyjs::show("exportTableProlif")
    shinyjs::show("exportImageProlif")
    shinyjs::show("prolifLabel")
    shinyjs::show("prolifGrid")
    shinyjs::show("prolifTable")
    enable("step3")
    dfX <- ag$dF[, ag$prolifChannel]
    ref <- hist(dfX, breaks = 150, plot = FALSE)
    prolifReady(dfX, ref, lab, grid)
  }
}

prolifNotReady <- function(undividedGate) {
  prolifOff()
  disable("step1")
  shinyjs::show("prolifSButt")
  shinyjs::show("prevProlifS")
  shinyjs::show("nextProlifS")
  shinyjs::show("applyProlif")
  enable("step2")
  undividedLimits <-
    c(undividedGate@min[[1]], undividedGate@max[[1]])
  ID <- ag$referenceHist$breaks > undividedLimits[1]
  approxD0X <- ag$referenceHist$breaks[ID]
  approxD0X <- approxD0X[approxD0X < undividedLimits[2]]
  peakID <- which(diff(sign(diff(ag$densLine$y))) == -2)
  xPeaks <- ag$densLine$x[peakID]
  yPeaks <- ag$densLine$y[peakID]
  ID <- which(diff(sign(diff(ag$densLine$y))) == 2)
  ag$betweenPeaks <- ag$densLine$x[ID]
  ID <- ag$betweenPeaks < mean(approxD0X) * 1.05
  ag$betweenPeaks <- ag$betweenPeaks[ID]
  ag$refPeaks <- data.frame(x = NA, y = NA)
  for (i in seq(xPeaks)) {
    ag$refPeaks[i,] <- c(xPeaks[[i]], yPeaks[[i]])
  }
  IDs <- which(ag$refPeaks[[1]] > mean(approxD0X) * 1.05)
  if (length(IDs) > 0) {
    ag$refPeaks <- ag$refPeaks[-IDs,]
  }
  ag$adjustedL <- c()
  for (i in seq(nrow(ag$refPeaks))) {
    if (i == 1) {
      mean <- mean(ag$refPeaks[, 1] - ag$refPeaks[, 1][1]) / 3
      ag$adjustedL[i] <- ag$betweenPeaks[1] - mean
    } else if (i == nrow(ag$refPeaks)) {
      ag$adjustedL[i] <- ag$refPeaks[, 1][i]
    } else {
      mean <- mean(c(ag$betweenPeaks[i], ag$betweenPeaks[i - 1]))
      ag$adjustedL[i] <- mean
    }
  }
  for (i in seq(ag$refPeaks[, 1])) {
    text(rev(ag$adjustedL)[i],
         ag$maxDens * 1.1,
         labels = i - 1,
         cex = 1.5)
    abline(v = ag$betweenPeaks[i], lty = 2)
  }
}

prolifReady <- function(dfX, ref, lab, grid) {
  gap <-
    mean(ag$refPeaks[, 1] - ag$refPeaks[, 1][1]) / nrow(ag$refPeaks)
  ag$gatecoords <- list()
  for (i in seq(ag$refPeaks[, 1])) {
    if (lab == FALSE) {
      dX <- c(ag$refPeaks[, 1][i] - gap, ag$refPeaks[, 1][i] + gap)
      dBreaks <- ref$breaks[ref$breaks > dX[1]]
      dBreaks <- dBreaks[dBreaks < dX[2]]
      dCounts <- c()
      for (j in seq(dBreaks)) {
        ID <- which(ref$breaks == dBreaks[j])
        dCounts[j] <- ref$counts[ID]
      }
      y <- mean(dCounts) * 13
      if (!is.na(y)) {
        if (y > ag$maxDens * 1.1) {
          y <- ag$maxDens * 1.1
        }
      }
      labels <- abs(i - nrow(ag$refPeaks))
      text(ag$adjustedL[i], y, labels = labels, cex = 1.5)
    } else {
      y <- ag$maxDens * 1.1
      text(rev(ag$adjustedL)[i],
           y,
           labels = i - 1,
           cex = 1.5)
    }
    if (grid == TRUE) {
      abline(v = ag$betweenPeaks[i], lty = 2)
    }
    if (i == 1) {
      ag$gatecoords[[i]] <- c(0, ag$betweenPeaks[1])
    } else if (i == nrow(ag$refPeaks)) {
      ag$gatecoords[[i]] <- c(ag$betweenPeaks[i - 1], 4100)
    } else {
      ag$gatecoords[[i]] <- c(ag$betweenPeaks[i - 1],
                              ag$betweenPeaks[i])
    }
  }
}

prolifTableGen <- function(prolifSB) {
  ID <- match(prolifSB, ag$fileList)
  ff <- cytoframe_to_flowFrame(gs_pop_get_data(ag$gs[ID],
                                               ag$prolifParent)[[1]])
  dF <- as.data.frame.array(ff@exprs)
  dfX <- dF[, ag$prolifChannel]
  ag$divPercents <- c()
  ag$divCounts <- c()
  ag$total <- length(dfX)
  for (i in seq(ag$gatecoords)) {
    filter <- dfX[dfX > ag$gatecoords[[i]][1]]
    filter <- filter[filter < ag$gatecoords[[i]][2]]
    ag$divCounts[i] <- length(filter)
    ag$divPercents[i] <- length(filter) * 100 / ag$total
  }
  ag$divCounts <- rev(ag$divCounts)
  round <- round(ag$divPercents, 2)
  ag$divPercents <- rev(as.numeric(format(round, nsmall = 2)))
  ag$totalDivCount <- ag$total - ag$divCounts[1]
  ag$totalDivPerc <- 100 - ag$divPercents[1]
  l0 <- "Proliferation assay data:"
  l1 <- paste("Number of divisions:", (nrow(ag$refPeaks) - 1))
  l2 <- paste("Divided cells:", ag$totalDivCount)
  l3 <- paste("Undivided cells:", ag$divCounts[1])
  lines <- c(l1, l2, l3)
  for (i in 2:nrow(ag$refPeaks)) {
    lines[i + 2] <- paste0("Division ", (i - 1), ": ", ag$divCounts[i])
  }
  number <-
    as.numeric(format(round(ag$totalDivPerc, 2), nsmall = 2))
  lines[length(lines) + 1] <- paste("Percent divided:", number)
  number <- ag$divPercents[1]
  lines[length(lines) + 1] <- paste("Percent undivided:", number)
  constantlinesln <- length(lines)
  for (i in 2:nrow(ag$refPeaks)) {
    lines[i + (constantlinesln - 1)] <-
      paste0("Percent division ", (i - 1),
             ": ", ag$divPercents[i])
  }
  part2 <- paste(lines, collapse = "<br/>", sep = "<br/>")
  HTML("<br/>", paste(strong(l0)), "<br/>", part2)
}

prolifOff <- function() {
  hide("exportTableProlif")
  hide("exportImageProlif")
  hide("prolifLabel")
  hide("prolifGrid")
  hide("prolifTable")
  disable("step3")
}
##t-SNE----
tSNEGen <-
  function(dots,
           mode,
           pops,
           gOrS,
           ID,
           tSHighl,
           react,
           tIDs,
           tPops) {
    par(mar = c(4, 6, 3, 26), lwd = 2)
    currentEnttS <- ag$entiretSNE
    x <- currentEnttS[, 1]
    y <- currentEnttS[, 2]
    xLim <- c(min(x) * 1.1, max(x) * 1.1)
    yLim <- c(min(y) * 1.1, max(y) * 1.1)
    nrows <- nrow(currentEnttS)
    cex <- dots / 10
    if (mode == "Overlay Groups or Samples") {
      if (gOrS == "All") {
        enable("savetSNEPlot")
        plot(
          x,
          y,
          col = "black",
          xaxt = "n",
          yaxt = "n",
          ann = FALSE,
          pch = 20,
          cex = cex,
          lwd = 0,
          xlim = xLim,
          ylim = yLim,
          xaxs = "i",
          yaxs = "i"
        )
        axisTtSNE(xLim, yLim)
      } else {
        if (!is.null(react)) {
          ag$tIDs <- tIDs
          tSGenGroupORS(currentEnttS,
                        nrows,
                        cex,
                        xLim,
                        yLim,
                        as.integer(react),
                        gOrS)
        }
      }
      hide("showingtSNEEvents")
    } else {
      if (gOrS == "All") {
        dfRows <- c(1, nrows)
        currenttS <- currentEnttS
      } else {
        if (gOrS == "Group") {
          interval <- nrows / length(ag$concatSamples)
        } else if (gOrS == "Sample") {
          interval <- nrows / length(unlist(ag$concatSamples))
        }
        num <- interval * as.numeric(ID)
        dfRows <- c(num - interval + 1, num)
        currenttS <- currentEnttS[dfRows[1]:dfRows[2],]
      }
      if (mode == "Heatmap") {
        tSGenHeat(cex, xLim, yLim, tSHighl, dfRows, currenttS)
        shinyjs::show("showingtSNEEvents")
      } else if (mode == "Overlay Populations"
                 && !is.null(pops)) {
        tSGenOvPo(dfRows, currentEnttS, cex, xLim, yLim, tPops)
      }
      ag$toFormat <- as.integer((dfRows[2] - dfRows[1]) + 1)
    }
  }

tSGenGroupORS <-
  function(currentEnttS,
           nrows,
           cex,
           xLim,
           yLim,
           ids,
           gOrS) {
    if (gOrS == "Group") {
      interval <- nrows / length(ag$concatSamples)
      listGroupORSamp <- names(ag$tSNEListofGroups[ids])
    } else if (gOrS == "Sample") {
      interval <- nrows / length(unlist(ag$concatSamples))
      listGroupORSamp <- names(ag$tSNEListofSamples[ids])
    }
    firs <- interval * as.numeric(ag$tIDs[1])
    sec <- interval * as.numeric(ag$tIDs[2])
    dfRows <- list(c(firs - interval + 1, firs),
                   c(sec - interval + 1, sec))
    currenttS <- list(currentEnttS[dfRows[[1]][1]:dfRows[[1]][2],],
                      currentEnttS[dfRows[[2]][1]:dfRows[[2]][2],])
    dfFinal <- data.frame()
    colorList <- c("black", "red")
    for (i in seq(ag$tIDs)) {
      dfFinal <- rbind(dfFinal,
                       data.frame(
                         "X" = currenttS[[i]][, 1],
                         "Y" = currenttS[[i]][, 2],
                         "color" = colorList[i]
                       ))
    }
    set.seed(4)
    shuffledDF <- dfFinal[sample(nrow(dfFinal)), ]
    enable("savetSNEPlot")
    plot(
      shuffledDF[[1]],
      shuffledDF[[2]],
      col = shuffledDF[[3]],
      xaxt = "n",
      yaxt = "n",
      ann = FALSE,
      pch = 20,
      cex = cex,
      lwd = 0,
      xlim = xLim,
      ylim = yLim,
      xaxs = "i",
      yaxs = "i"
    )
    axisTtSNE(xLim, yLim)
    leg <- rep(NA, length(listGroupORSamp))
    tempLegend <- legend(
      "topright",
      bty = "n",
      inset = c(-0.25, -0.025),
      pch = 15,
      lty = 0,
      legend = leg,
      cex = 1.3,
      pt.cex = 4,
      y.intersp = 1.6,
      text.width = strwidth("100"),
      xpd = TRUE,
      lwd = 2,
      pt.bg = colorList,
      col = colorList
    )
    multiplier <- 1.2
    strW <- strwidth(listGroupORSamp) * multiplier
    tX <- strW + tempLegend$text$x * 1.01
    tY <- tempLegend$text$y
    text(
      tX,
      tY,
      listGroupORSamp,
      pos = 2,
      xpd = NA,
      font = 2,
      cex = multiplier
    )
  }

tSGenHeat <- function(cex, xLim, yLim, tSHighl, dfRows, currenttS) {
  col1 <- colorRampPalette(c("#5E4FA2", "#3288BD"))(10)
  col2 <- colorRampPalette(c("#3288BD", "#ABDDA4"))(10)
  col3 <- rep("#ABDDA4", 5)
  col4 <- colorRampPalette(c("#ABDDA4", "#E6F598"))(10)
  col5 <- colorRampPalette(c("#E6F598", "#D53E4F"))(10)
  col <- c(col1, col2, col3, col4, col5)
  dataF <- list()
  for (i in seq(ncol(ag$concat))) {
    dataF[[i]] <- ag$concat[, i]
  }
  dataF <- data.frame(unlist(dataF))
  dataF <- cbind2(dataF, seq_len(nrow(dataF)))
  colnames(dataF) <- c("value", "ID")
  dataF <- dataF %>% arrange(dataF$value)
  dataF <- cbind2(dataF, colorRampPalette(col)(nrow(dataF)))
  dataF <- dataF %>% arrange(dataF$ID)
  colnames(dataF)[3] <- "col"
  legRange <- c(min(dataF[, 1]), max(dataF[, 1]))
  if (tSHighl == "") {
    highlight <- ag$availableparameters[[1]]
  } else {
    highlight <- tSHighl
  }
  highID <- which(colnames(ag$concat) == highlight)
  intStart <- (nrow(ag$concat) * (highID - 1)) + 1
  intEnd <- nrow(ag$concat) * highID
  dataF <- dataF[intStart:intEnd,]
  dataF["ID"] <- rownames(dataF) <- seq_len(nrow(dataF))
  enable("savetSNEPlot")
  cols <- dataF$col[dfRows[1]:dfRows[2]]
  plot(
    currenttS,
    xaxt = "n",
    yaxt = "n",
    ann = FALSE,
    pch = 20,
    cex = cex,
    lwd = 0,
    col = cols,
    xlim = xLim,
    ylim = yLim,
    xaxs = "i",
    yaxs = "i"
  )
  axisTtSNE(xLim, yLim)
  legendCol <- colorRampPalette(col)(80)
  len <- length(legendCol)
  barXCoords <- seq(xLim[1], xLim[2], length.out = len)
  y <- yLim[2] * 1.15
  for (i in seq(legendCol)) {
    x <- barXCoords[i] * 0.5
    points(
      x,
      y,
      pch = 15,
      col = legendCol[i],
      xpd = TRUE,
      cex = 2
    )
  }
  tX <- barXCoords[1] * 0.6
  labs <- format(round(legRange[1], 2), nsmall = 2)
  text(
    tX,
    y,
    xpd = TRUE,
    labels = labs,
    adj = 1,
    cex = 1.1
  )
  tX <- barXCoords[length(barXCoords)] * 0.6
  labs <- format(round(legRange[2], 2), nsmall = 2)
  text(
    tX,
    y,
    xpd = TRUE,
    labels = labs,
    adj = 0,
    cex = 1.1
  )
}

tSGenOvPo <-
  function(dfRows,
           currentEnttS,
           cex,
           xLim,
           yLim,
           tPops) {
    groupSampIndices <- seq(dfRows[1], dfRows[2])
    currenttS <- list()
    for (i in seq(tPops)) {
      id <- which(ag$concat[, tPops[i]] == 1)
      index <- intersect(groupSampIndices, id)
      currenttS[[i]] <- currentEnttS[index,]
    }
    dfFinal <- data.frame()
    colorList <- c("red2",
                   "green4",
                   "blue3",
                   "orange",
                   "black",
                   "wheat3",
                   "brown",
                   "orchid3",
                   "salmon")
    for (i in seq(tPops)) {
      dfFinal <- rbind(dfFinal,
                       data.frame(
                         "X" = currenttS[[i]][, 1],
                         "Y" = currenttS[[i]][, 2],
                         "color" = colorList[i]
                       ))
    }
    set.seed(5)
    shuffledDF <- dfFinal[sample(nrow(dfFinal)), ]
    enable("savetSNEPlot")
    plot(
      shuffledDF[[1]],
      shuffledDF[[2]],
      col = shuffledDF[[3]],
      xaxt = "n",
      yaxt = "n",
      ann = FALSE,
      pch = 20,
      cex = cex,
      lwd = 0,
      xlim = xLim,
      ylim = yLim,
      xaxs = "i",
      yaxs = "i"
    )
    axisTtSNE(xLim, yLim)
    cols <- colorList[seq_len(length(tPops))]
    leg <- rep(NA, length(tPops))
    tempLegend <-
      legend(
        "topright",
        bty = "n",
        inset = c(-0.25, -0.025),
        pch = 15,
        lty = 0,
        lwd = 2,
        cex = 1.3,
        pt.cex = 4,
        xpd = TRUE,
        y.intersp = 1.6,
        text.width = strwidth("100"),
        pt.bg = cols,
        col = cols,
        legend = leg
      )
    multiplier <- 1.2
    strW <- strwidth(tPops)
    tX <- strW * multiplier + tempLegend$text$x * 1.01
    tY <- tempLegend$text$y
    text(
      tX,
      tY,
      tPops,
      pos = 2,
      xpd = NA,
      font = 2,
      cex = multiplier
    )
    hide("showingtSNEEvents")
  }

showtSNE <- function() {
  hide("tSNEGroups")
  hide("tSNESamples")
  hide("tSPar")
  hide("tSNEParameters")
  hide("tSEvs")
  shinyjs::show("tSNEMode")
  shinyjs::show("tSNEDotSize")
  shinyjs::show("tSHighl")
  shinyjs::show("tSGroupOrSamp")
  shinyjs::show("tSGroupOrSampID")
  shinyjs::show("showingtSNEEvents")
  shinyjs::show("savetSNEPlot")
  enable("tSNEGenerate")
}

hidetSNE <- function() {
  hide("tSNEMode")
  hide("tSNEDotSize")
  hide("tSHighl")
  hide("tSGroupOrSamp")
  hide("tSGroupOrSampID")
  hide("tSGroupOrSampIDs")
  hide("tSNEPopulations")
  hide("savetSNEPlot")
  hide("showingtSNEEvents")
  shinyjs::show("tSNEGroups")
  shinyjs::show("tSNESamples")
  shinyjs::show("tSPar")
  shinyjs::show("tSNEParameters")
  shinyjs::show("tSEvs")
}