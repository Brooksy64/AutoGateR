#AutoGateR UI----
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(
    text = jsCode,
    functions = c('disableTab', 'enableTab')
  ),
  inlineCSS(css),
  tags$style("#about {border-color:white; font-size:12px}"),
  headerPanel(
    fluidRow(column(
      width = 3,
      "AutoGateR"),
      column(
        width = 2,
        offset = 7,
        align = "right",
        actionButton(inputId = "about", label = "About AutoGateR")
      )
    ),
    ),
    tags$style("#tabs {border-color:black; top:25px}"),
    tags$style(HTML(".tabbable > .nav > li > a {color:black}")),
    tags$style(
      HTML(".tabbable > .nav > li[class=active] > a
                   {color:black; border-top-color:black;
                   border-left-color:black; border-right-color:black}")
    ),
    tags$style("#exportimagePlot {border-color:black}"),
    tags$style("#plotHelp {border-color:white}"),
    tags$style("#parentHelp {border-color:white}"),
    tags$style("#nextSample .fa{font-size: 11px}"),
    tags$style("#previousSample .fa{font-size: 11px}"),
    tags$style("#nextProlifS .fa{font-size: 11px}"),
    tags$style("#prevProlifS .fa{font-size: 11px}"),
    tabsetPanel(
      id = "tabs",
      ##File----
      tabPanel(div(icon("cloud-arrow-up"), "File"),
               value = "fileTab",
               br(),
               fluidRow(
                 column(
                   width = 4,
                   offset = 4,
                   sidebarPanel(
                     id = "Fileleft",
                     align = "center",
                     shinyDirButton(
                       "directory",
                       "Load FCS files",
                       icon = icon("file-arrow-up"),
                       "Please select a folder with your
                                       FCS files"
                     ),
                     br(),
                     br(),
                     strong("or"),
                     br(),
                     br(),
                     shinyFilesButton(
                       "AutoGateRFileLoad",
                       "Load AutoGateR file",
                       icon = icon("file-arrow-up"),
                       "Please select a AutoGateR file (it
                                         needs to be located in the same
                                         folder of the FCS files)",
                       multiple = FALSE
                     ),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     actionButton(inputId = "saveFile",
                                  label =
                                    div(icon("floppy-disk"),
                                        "Save")),
                     br(),
                     br(),
                     tags$style("#fileHelp {border-color:white}"),
                     fluidRow(column(
                       width = 3,
                       offset = 9,
                       actionButton(inputId = "fileHelp",
                                    icon("circle-question"))
                     )),
                     width = 16
                   ),
                 )
               ),),
      
      ##Compensation----
      tabPanel(
        div(icon("table"), "Compensation"),
        value = "compTab",
        br(),
        sidebarPanel(
          selectInput(
            inputId = "previewSample",
            label = div(icon("vial"), "Preview sample"),
            choices = c("")
          ),
          checkboxInput(
            inputId = "showUncomp",
            label = strong("Overlay uncompensated"),
            value = TRUE
          ),
          selectInput(
            inputId = "previewMatrix",
            label = div(icon("table"), "Preview Matrix"),
            choices = c("Cytometer-defined (applied)" =
                          "Cytometer-defined")
          ),
          tags$style("#createMatrix {border-color:black}"),
          actionButton(inputId = "createMatrix",
                       label = "Create New Matrix",
                       icon("square-plus")),
          br(),
          br(),
          tags$style(paste0("#saveMatrix {", ag$blackB, "}")),
          actionButton(inputId = "saveMatrix",
                       label = "Save New Matrix",
                       icon("sd-card")),
          br(),
          br(),
          tags$style("#cancelMatrix {border-color:black}"),
          actionButton(inputId = "cancelMatrix",
                       label = "Cancel"),
          br(),
          br(),
          tags$style("#applyMatrix {border-color:black}"),
          actionButton(
            inputId = "applyMatrix",
            label = "Apply to Samples",
            icon("share-from-square")
          ),
          width = 3
        ),
        mainPanel(fluidRow(column(
          width = 12, rHandsontableOutput("comp")
        )),
        br(),
        br(),
        fluidRow(
          column(width = 12, align = "center", plotOutput("compGen"))
        ),
        width = 9)
      ),
      
      ##Plot----
      tabPanel(
        div(icon("chart-area"), "Plot"),
        value = "plotTab",
        br(),
        tags$style(".well {background-color:white; border-color:black}"),
        sidebarPanel(
          id = "Plotleft",
          selectInput(
            inputId = "samp",
            label = div(icon("vial"), "Sample"),
            choices = c("")
          ),
          tags$style("#customizeAxisY {border-color:black}"),
          fluidRow(
            column(width = 8,
                   selectInput(
                     inputId = "Y",
                     label = "Y axis",
                     choices = c("")
                   )),
            column(
              width = 4,
              style = "margin-top: 25px;",
              actionButton(inputId = "customizeAxisY", label = "Scale")
            )
          ),
          tags$style("#customizeAxisX {border-color:black}"),
          fluidRow(
            column(width = 8,
                   selectInput(
                     inputId = "X",
                     label = "X axis",
                     choices = c("")
                   )),
            column(
              width = 4,
              style = "margin-top: 25px;",
              actionButton(inputId = "customizeAxisX", label = "Scale")
            )
          ),
          selectInput(
            inputId = "typ",
            label = div(icon("chart-area"),
                        "Plot type"),
            choices = c(
              "Pseudocolor",
              "Contour",
              "Density",
              "Pseudocolor + Contour",
              "Histogram"
            )
          ),
          tags$style("#displayOptMain {border-color:black}"),
          fluidRow(
            column(width = 4,
                   actionButton(
                     inputId = "displayOptMain",
                     label = div(icon("bars"),
                                 "Display options")
                   )),
            column(
              width = 2,
              offset = 5,
              actionButton(inputId = "plotHelp",
                           icon("circle-question"))
            )
          ),
          width = 3
        ),
        mainPanel(
          id = "Plotmiddle",
          fluidRow(
            column(
              width = 3,
              div(
                style = "display:inline-block; position:fixed",
                actionButton(
                  inputId = "previousSample",
                  label = "Previous Sample",
                  width = "140px",
                  # icon("chevron-left"),
                  style = ag$blackB
                ),
                actionButton(
                  inputId = "nextSample",
                  label = "Next Sample",
                  width = "140px",
                  # icon("chevron-right"),
                  style = ag$blackB
                )
              )
            ),
            column(
              width = 5,
              offset = 3,
              align = "left",
              id = "gatetools",
              div(
                style = "display:inline-block; position:fixed",
                actionButton(
                  inputId = "rectang",
                  label = "Rectangle",
                  width = "100px",
                  style = ag$blackB
                ),
                actionButton(
                  inputId = "polygon",
                  label = "Polygon",
                  width = "90px",
                  style = ag$blackB
                ),
                actionButton(
                  inputId = "quad",
                  label = "Quad",
                  width = "70px",
                  style = ag$blackB
                ),
                actionButton(
                  inputId = "interval",
                  label = "Interval",
                  width = "90px",
                  style = ag$blackB
                ),
              )
            ),
            column(
              width = 5,
              offset = 5,
              div(
                style = "display:inline-block; position:fixed",
                textInput(
                  inputId = "drawGateName",
                  label = NULL,
                  placeholder = "Please draw a gate",
                  width = "180px"
                ),
                
              )
            ),
            column(
              width = 1,
              offset = 0,
              div(
                style = "display:inline-block; position:fixed",
                actionButton(
                  inputId = "gateOk",
                  label = "Ok",
                  style = ag$blackB
                ),
                actionButton(
                  inputId = "gateCancel",
                  label = "Cancel",
                  style = ag$whiteB
                )
              )
            ),
          ),
          br(),
          br(),
          uiOutput("plotUI"),
          tags$style("#saveMainPlot {border-color:black}"),
          div(
            style = "display:inline-block",
            downloadButton(outputId = "saveMainPlot",
                           label = "Export image")
          ),
          div(style = "display:inline-block;vertical-align:top;
                    width: 20px;",
              HTML("<br>")),
          div(style = "display:inline-block", textOutput("events")),
          width = 5
        ),
        sidebarPanel(
          id = "Plotright",
          strong(div(icon("sitemap"), "Parent")),
          plotOutput(
            "hierarchy",
            click = "hierarchy_click",
            dblclick = "hierarchy_dblclick",
            height = 450
          ),
          fluidRow(
            column(
              width = 5,
              tags$style("#exportImageGates {border-color:black}"),
              downloadButton(outputId = "exportImageGates",
                             label = "Export image"),
            ),
            column(
              width = 4,
              offset = 1,
              tags$style("#editGate {border-color:black}"),
              actionButton(inputId = "editGate",
                           label = "Edit", icon("fas fa-pen")),
            ),
            column(width = 1,
                   actionButton(inputId = "parentHelp",
                                icon("circle-question")))
          ),
          width = 4
        )
      ),
      ##Ancestry----
      tabPanel(
        div(icon("chart-area"), "Ancestry plots"),
        value = "ancestryTab",
        br(),
        sidebarPanel(
          selectInput(
            inputId = "bgPreviewSample",
            label = div(icon("vial"), "Sample"),
            choices = c("")
          ),
          selectInput(
            inputId = "bgType",
            label = div(icon("chart-area"), "Plot type"),
            choices = c("", "Pseudocolor", "Contour", "Density",
                        "Backgating"),
            selected = ""
          ),
          selectInput(
            inputId = "bgPop",
            label = div(
              tags$i(class = "fas fa-clone"),
              HTML("Backgating <span style='color: red'
                                                      >Population</span>")
            ),
            choices = c("")
          ),
          width = 3
        ),
        mainPanel(
          plotOutput("ancestry", height = 550),
          tags$style("#exportImageAncestry {border-color:black}"),
          downloadButton(outputId = "exportImageAncestry",
                         label = "Export image"),
          width = 9
        )
      ),
      ##Overlays----
      tabPanel(
        div(icon("chart-area"), "Overlays"),
        value = "overlayTab",
        br(),
        sidebarPanel(
          selectInput(
            inputId = "ovTyp",
            label = div(icon("chart-area"), "plot type"),
            choices = c("", "Overlaid histogram",
                        "Offset histogram", "Dot plot")
          ),
          selectInput(
            inputId = "ovTon",
            label = div(icon("chart-area"), "Tone"),
            choices = c("", "Colorful", "B&W")
          ),
          selectInput(
            inputId = "ovY",
            label = "Y axis",
            choices = c("")
          ),
          selectInput(
            inputId = "ovX",
            label = "X axis",
            choices = c("")
          ),
          selectInput(
            inputId = "ovP",
            label = div(icon("sitemap"), "Parent"),
            choices = c("", "ungated")
          ),
          tags$style("#ovSamples {border-color:black}"),
          actionButton(inputId = "ovSamples",
                       label = "Select samples", icon("square-plus")),
          br(),
          br(),
          tags$style("#displayOptOv {border-color:black}"),
          actionButton(
            inputId = "displayOptOv",
            label = div(icon("bars"), "Display options")
          ),
          width = 3
        ),
        mainPanel(
          plotOutput("overlays"),
          br(),
          br(),
          br(),
          tags$style("#ovSampleOrder {border-color:black}"),
          actionButton(inputId = "ovSampleOrder",
                       label = "Edit order", icon("fas fa-pen")),
          tags$style("#exportImageOverlay {border-color:black}"),
          downloadButton(outputId = "exportImageOverlay",
                         label = "Export image"),
          width = 9
        ),
      ),
      ##AutoPlot----
      tabPanel(
        div(icon("chart-area"), "Auto-Plot"),
        value = "autoPlotTab",
        br(),
        tags$style(".well {background-color:white; border-color:black}"),
        sidebarPanel(
          id = "autoPlotleft",
          selectInput(
            inputId = "autosamp",
            label = div(icon("vial"), "Sample"),
            choices = c("")
          ),
          tags$style("#customizeAxisY {border-color:black}"),
          fluidRow(
            column(
              width = 8,
              selectInput(
                inputId = "autoY",
                label = "Y axis",
                choices = c("")
              )
            ),
            column(
              width = 4,
              style = "margin-top: 25px;",
              actionButton(inputId = "autocustomizeAxisY", label = "Scale")
            )
          ),
          tags$style("#customizeAxisX {border-color:black}"),
          fluidRow(
            column(
              width = 8,
              selectInput(
                inputId = "autoX",
                label = "X axis",
                choices = c("")
              )
            ),
            column(
              width = 4,
              style = "margin-top: 25px;",
              actionButton(inputId = "autocustomizeAxisX", label = "Scale")
            )
          ),
          selectInput(
            inputId = "autotyp",
            label = div(icon("chart-area"),
                        "Auto Plot type"),
            choices = c(
              "Pseudocolor",
              "Contour",
              "Density",
              "Pseudocolor + Contour",
              "Histogram"
            )
          ),
          tags$style("#displayOptMain {border-color:black}"),
          fluidRow(
            column(width = 4,
                   actionButton(
                     inputId = "autodisplayOptMain",
                     label = div(icon("bars"),
                                 "Display options")
                     )),
            column(
              width = 2,
              offset = 5,
              actionButton(inputId = "autoplotHelp",
                           icon("circle-question"))
            )
          ),
          width = 3
        ),
        mainPanel(
          id = "autoPlotmiddle",
          fluidRow(
            column(
              width = 3,
              div(
                style = "display:inline-block; position:fixed",
                actionButton(
                  inputId = "autopreviousSample",
                  label = "Previous Sample",
                  width = "140px",
                  style = ag$blackB
                ),
                actionButton(
                  inputId = "autonextSample",
                  label = "Next Sample",
                  width = "140px",
                  style = ag$blackB
                )
              )
            ),
            column(
              width = 5,
              offset = 3,
              align = "left",
              id = "autogatetools",
              div(
                style = "display:inline-block; position:fixed",
                actionButton(
                  inputId = "auto2D",
                  label = "Auto 2D",
                  width = "80px",
                  style = ag$blackB
                ),
                actionButton(
                  inputId = "autosinglet",
                  label = "Auto Singlet",
                  width = "100px",
                  style = ag$blackB
                ),
                actionButton(
                  inputId = "autoquad",
                  label = "Auto Quad",
                  width = "90px",
                  style = ag$blackB
                ),
                actionButton(
                  inputId = "autointerval",
                  label = "Auto Interval",
                  width = "100px",
                  style = ag$blackB
                ),
              )
            ),
            column(
              width = 5,
              offset = 5,
              div(
                style = "display:inline-block; position:fixed",
                textInput(
                  inputId = "autodrawGateName",
                  label = NULL,
                  placeholder = "Autogating...",
                  width = "180px"
                ),
              )
            ),
            column(
              width = 1,
              offset = 0,
              div(
                style = "display:inline-block; position:fixed",
                actionButton(
                  inputId = "autogateOk",
                  label = "Ok",
                  style = ag$blackB
                ),
                actionButton(
                  inputId = "autogateCancel",
                  label = "Cancel",
                  style = ag$whiteB
                )
              )
            ),
          ),
          br(),
          br(),
          uiOutput("autoplotUI"),
          tags$style("#saveMainPlot {border-color:black}"),
          div(
            style = "display:inline-block",
            downloadButton(outputId = "autosaveMainPlot",
                           label = "Export image")
          ),
          div(style = "display:inline-block;vertical-align:top;
                    width: 20px;",
              HTML("<br>")),
          div(style = "display:inline-block", textOutput("autoevents")),
          width = 5
        ),
        sidebarPanel(
          id = "autoPlotright",
          strong(div(icon("sitemap"), "Parent")),
          plotOutput(
            "autohierarchy",
            click = "autohierarchy_click",
            dblclick = "autohierarchy_dblclick",
            height = 450
          ),
          fluidRow(
            column(
              width = 5,
              tags$style("#exportImageGates {border-color:black}"),
              downloadButton(outputId = "autoexportimagegates",
                             label = "Export image"),
            ),
            column(
              width = 4,
              offset = 1,
              tags$style("#editGate {border-color:black}"),
              actionButton(inputId = "editautoGate",
                           label = "Edit", icon("fas fa-pen")),
            ),
            column(width = 1,
                   actionButton(inputId = "autoparentHelp",
                                icon("circle-question")))
          ),
          width = 4
        )
      ),
      ##auto Ancestry----
      tabPanel(
        div(icon("chart-area"), "Auto-Ancestry plots"),
        value = "autoancestryTab",
        br(),
        sidebarPanel(
          selectInput(
            inputId = "autobgPreviewSample",
            label = div(icon("vial"), "Sample"),
            choices = c("")
          ),
          selectInput(
            inputId = "autobgType",
            label = div(icon("chart-area"), "Plot type"),
            choices = c("", "Pseudocolor", "Contour", "Density",
                        "Backgating"),
            selected = ""
          ),
          selectInput(
            inputId = "autobgPop",
            label = div(
              tags$i(class = "fas fa-clone"),
              HTML("Backgating <span style='color: red'
                                                      >Population</span>")
            ),
            choices = c("")
          ),
          width = 3
        ),
        mainPanel(
          plotOutput("autoancestry", height = 550),
          tags$style("#exportImageAncestry {border-color:black}"),
          downloadButton(outputId = "autoexportImageAncestry",
                         label = "Export image"),
          width = 9
        )
      ),
      ##Auto Overlays----
      tabPanel(
        div(icon("chart-area"), "Auto-Overlays"),
        value = "autooverlayTab",
        br(),
        sidebarPanel(
          selectInput(
            inputId = "autoovTyp",
            label = div(icon("chart-area"), "plot type"),
            choices = c("", "Overlaid histogram",
                        "Offset histogram", "Dot plot")
          ),
          selectInput(
            inputId = "autoovTon",
            label = div(icon("chart-area"), "Tone"),
            choices = c("", "Colorful", "B&W")
          ),
          selectInput(
            inputId = "autoovY",
            label = "Y axis",
            choices = c("")
          ),
          selectInput(
            inputId = "autoovX",
            label = "X axis",
            choices = c("")
          ),
          selectInput(
            inputId = "autoovP",
            label = div(icon("sitemap"), "Parent"),
            choices = c("", "ungated")
          ),
          tags$style("#ovSamples {border-color:black}"),
          actionButton(inputId = "autoovSamples",
                       label = "Select samples", icon("square-plus")),
          br(),
          br(),
          tags$style("#displayOptOv {border-color:black}"),
          actionButton(
            inputId = "autoodisplayOptOV",
            label = div(icon("bars"), "Display options")
          ),
          width = 3
        ),
        mainPanel(
          plotOutput("autooverlays"),
          br(),
          br(),
          br(),
          tags$style("#ovSampleOrder {border-color:black}"),
          actionButton(inputId = "autoovSampleOrder",
                       label = "Edit order", icon("fas fa-pen")),
          tags$style("#exportImageOverlay {border-color:black}"),
          downloadButton(outputId = "autoexportImageOverlay",
                         label = "Export image"),
          width = 9
        ),
      ),
      ##Proliferation----
      # tabPanel(
      #   div(icon("chart-area"), "Proliferation"), value="prolifTab",
      #   br(),
      #   sidebarPanel(
      #     tags$style("#step1 {border-color:black}"),
      #     actionButton(inputId="step1",
      #                  label=div("Step 1: gate undivided",
      #                            tags$i(class="far fa-circle-question"))
      #     ),
      #     br(),
      #     br(),
      #     tags$style("#step2 {border-color:black}"),
      #     actionButton(inputId="step2",
      #                  label=div("Step 2: apply model",
      #                            tags$i(class="far fa-circle-question"))
      #     ),
      #     br(),
      #     br(),
      #     tags$style("#step3 {border-color:black}"),
      #     actionButton(inputId="step3",
      #                  label=div("Step 3: results",
      #                            tags$i(class="far fa-circle-question"))
      #     ),
      #     br(),
      #     br(),
      #     selectInput(inputId="prolifSButt",
      #                 label=div(icon("vial"), "Sample"),
      #                 choices=c("")),
      #     checkboxInput(inputId="prolifLabel", label="Numbers on top",
      #                   value=TRUE),
      #     checkboxInput(inputId="prolifGrid", label="Vertical lines",
      #                   value=TRUE),
      #     br(),
      #     br(),
      #     tags$style("#applyProlif {border-color:black}"),
      #     actionButton(inputId="applyProlif", label="Apply to Samples",
      #                  icon("share-from-square")),
      #     width=3
      #   ),
      #   mainPanel(
      #     htmlOutput("prolifStart"),
      #     fluidRow(
      #       column(
      #         width=3,
      #         div(style="display:inline-block; position:fixed",
      #             actionButton(inputId="prevProlifS", width="40px",
      #                          icon("chevron-left"),
      #                          style=ag$blackB),
      #             actionButton(inputId="nextProlifS", width="40px",
      #                          icon("chevron-right"),
      #                          style=ag$blackB)
      #         )
      #       ),
      #     ),
      #     br(),
      #     br(),
      #     fluidRow(
      #       column(
      #         width=8,
      #         plotOutput("prolifPlot", height=450),
      #       ),
      #       column(
      #         width=4,
      #         htmlOutput("prolifTable"),
      #       ),
      #     ),
      #     fluidRow(
      #       column(width=8,
      #              tags$style("#exportImageProlif {border-color:black}"
      #              ),
      #              downloadButton(outputId="exportImageProlif",
      #                             label="Export image"),
      #       ),
      #       column(width=4,
      #              tags$style("#exportTableProlif {border-color:black}"
      #              ),
      #              downloadButton(outputId="exportTableProlif",
      #                             label="Export complete table"),
      #       ),
      #     ),
      #     width=9
      #   )
      # ),
      ##t-SNE----
      tabPanel(
        div(icon("chart-pie"), "t-SNE"),
        value = "tsneTab",
        br(),
        sidebarPanel(
          selectInput(
            inputId = "tSNEMode",
            label = div(icon("laptop-code"), "Mode"),
            choices = c("Heatmap", "Overlay Groups or Samples",
                        "Overlay Populations")
          ),
          selectInput(
            inputId = "tSHighl",
            label = div(icon("highlighter"), "Highlight"),
            choices = c("")
          ),
          selectInput(
            inputId = "tSGroupOrSamp",
            label = div(icon("vial"), "Events to show"),
            choices = c("All", "Group", "Sample")
          ),
          selectInput(
            inputId = "tSGroupOrSampID",
            label = div(icon("vial"), "Group/Sample"),
            choices = c("")
          ),
          actionButton(inputId = "tSGroupOrSampIDs",
                       label = "Groups/Samples", icon("square-plus")),
          actionButton(inputId = "tSNEPopulations",
                       label = "Populations", icon("square-plus")),
          tags$style(
            HTML(
              "[for=tSNEGroups]+span>.irs>.irs-single,
                        [for=tSNEGroups]+span>.irs-bar {background: black;
                        border-top: black; border-bottom: black}"
            )
          ),
          sliderInput(
            "tSNEGroups",
            label = "Number of groups",
            min = 1,
            max = 6,
            step = 1,
            value = 1,
            ticks = FALSE
          ),
          actionButton(inputId = "tSNESamples", label = "Select samples",
                       icon("square-plus")),
          br(),
          br(),
          tags$style(
            HTML(
              "[for=tSNEDotSize]+span>.irs>.irs-single,
                        [for=tSNEDotSize]+span>.irs-bar {background: black;
                        border-top: black; border-bottom: black}"
            )
          ),
          sliderInput(
            "tSNEDotSize",
            label = "Dot size",
            min = 2,
            max = 18,
            step = 1,
            value = 8,
            ticks = FALSE
          ),
          selectInput(
            inputId = "tSPar",
            label = div(icon("sitemap"), "Parent"),
            choices = c("", "ungated")
          ),
          actionButton(inputId = "tSNEParameters",
                       label = "Select parameters", icon("square-plus")),
          br(),
          br(),
          selectInput(
            inputId = "tSEvs",
            label = div(icon("ellipsis-vertical"), "Events per sample"),
            choices = c(
              " " = 0,
              "1k" = 1000,
              "5k" = 5000,
              "10k" = 10000,
              "25k" = 25000,
              "50k" = 50000
            )
          ),
          br(),
          br(),
          actionButton(inputId = "tSNEGenerate", label = "Generate t-SNE",
                       icon("circle-play")),
          width = 3
        ),
        mainPanel(
          plotOutput("tSNEPlot"),
          br(),
          br(),
          br(),
          tags$style("#savetSNEPlot {border-color:black}"),
          div(
            style = "display:inline-block",
            downloadButton(outputId = "savetSNEPlot",
                           label = "Export image")
          ),
          div(style = "display:inline-block;vertical-align:top;
                    width: 20px;",
              HTML("<br>")),
          div(style = "display:inline-block",
              textOutput("showingtSNEEvents")),
          width = 9
        )
      ),
      ##Results----
      tabPanel(
        div(icon("table"), "Results"),
        value = "resultTab",
        br(),
        sidebarPanel(
          selectInput(
            inputId = "rsStat",
            label = div(icon("percent"), "Statistic"),
            choices = c(
              "",
              "Freq. of parent",
              "Freq. of...",
              # "Median",
              "Count"
            )
          ),
          selectInput(
            inputId = "rsParent",
            label = div(icon("sitemap"),
                        "Parent"),
            choices = c("")
          ),
          selectInput(
            inputId = "rsPop",
            label = div(tags$i(class = "fas fa-clone"),
                        "Population"),
            choices = c("")
          ),
          # selectInput(
          #   inputId = "rsCh",
          #   label = "Parameter",
          #   choices = c("")
          # ),
          tags$style("#addResult {border-color:black}"),
          actionButton(inputId = "addResult",
                       label = "Add to table",
                       icon("square-plus")),
          tags$style("#editResult {border-color:black}"),
          width = 3
        ),
        mainPanel(
          tags$head(tags$style(
            HTML("#results td, #results th
                                  {border-color:black}")
          )),
          tableOutput("results"),
          actionButton(inputId = "editResult",
                       label = "Edit", icon("fas fa-pen")),
          tags$style("#exportTable {border-color:black}"),
          downloadButton(outputId = "exportTable", label = "Export table"),
          width = 9
        )
      ),
      ##Auto Results----
      tabPanel(
        div(icon("table"), "Auto-Results"),
        value = "autoresultTab",
        br(),
        sidebarPanel(
          selectInput(
            inputId = "autorsStat",
            label = div(icon("percent"), "Statistic"),
            choices = c(
              "",
              "Freq. of parent",
              "Freq. of...",
              # "Median",
              "Count"
            )
          ),
          selectInput(
            inputId = "autorsParent",
            label = div(icon("sitemap"),
                        "Parent"),
            choices = c("")
          ),
          selectInput(
            inputId = "autorsPop",
            label = div(tags$i(class = "fas fa-clone"),
                        "Population"),
            choices = c("")
          ),
          # selectInput(
          #   inputId = "autorsCh",
          #   label = "Parameter",
          #   choices = c("")
          # ),
          tags$style("#addResult {border-color:black}"),
          actionButton(inputId = "addautoResult",
                       label = "Add to table",
                       icon("square-plus")),
          tags$style("#editResult {border-color:black}"),
          width = 3
        ),
        mainPanel(
          tags$head(tags$style(
            HTML("#results td, #results th
                                  {border-color:black}")
          )),
          tableOutput("autoresults"),
          actionButton(inputId = "editautoResult",
                       label = "Edit", icon("fas fa-pen")),
          tags$style("#exportTable {border-color:black}"),
          downloadButton(outputId = "exportautoTable", label = "Export table"),
          width = 9
        )
      )
    )
  )