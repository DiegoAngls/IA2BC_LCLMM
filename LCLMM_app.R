if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(shiny)

if (!requireNamespace("sp", quietly = TRUE)) {
  install.packages("sp")
}
library(sp)

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!requireNamespace("plyr", quietly = TRUE)) {
  install.packages("plyr")
}
library(plyr)

if (!requireNamespace("hablar", quietly = TRUE)) {
  install.packages("hablar")
}
library(hablar)

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}
library(data.table)

if (!requireNamespace("cowplot", quietly = TRUE)) {
  install.packages("cowplot")
}
library(cowplot)

if (!requireNamespace("ggpubr", quietly = TRUE)) {
  install.packages("ggpubr")
}
library(ggpubr)

if (!requireNamespace("lme4", quietly = TRUE)) {
  install.packages("lme4")
}
library(lme4)

if (!requireNamespace("performance", quietly = TRUE)) {
  install.packages("performance")
}
library(performance)

if (!requireNamespace("cluster", quietly = TRUE)) {
  install.packages("cluster")
}
library(cluster)

if (!requireNamespace("factoextra", quietly = TRUE)) {
  install.packages("factoextra")
}
library(factoextra)

if (!requireNamespace("lcmm", quietly = TRUE)) {
  install.packages("lcmm")
}
library(lcmm)

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)

if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}
library(DT)

if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  install.packages("shinyWidgets")
}
library(shinyWidgets)

if (!requireNamespace("shinyBS", quietly = TRUE)) {
  install.packages("shinyBS")
}
library(shinyBS)

if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)

ui <- fluidPage(
  titlePanel("Calculate subgroups using latent class mixed models"),
  sidebarLayout(
    sidebarPanel(width = 2,
                 fileInput("input_file", "Select the input file:", accept = ".csv"),
                 textInput("dep_input", "Dependent variable (e.g. main intake):", value = "main_intake"),
                 textInput("indepr_input", "Independent variable (e.g. Age):", value = "Age"),
                 textInput("covs_input", "Covariates separated by + (e.g. Sex + Batch):", value = "Sex"),
                 textInput("ran_input", "Random variable (e.g. Batch):", value = "Batch"),
                 textInput("ID_input", "Subject ID (e.g. RID):", value = "RID"),
                 textInput("seed_input", "Set seed for reproducibility:", value = "42"),
                 textInput("num_input", "How many classification groups? (e.g. 2):", value = "2"),
                 materialSwitch(
                   inputId = "export_result",
                   label = "Export?",
                   value = FALSE,
                   status = "primary"
                 ),
                 conditionalPanel(
                   condition = "input.export_result == true",
                   textInput("output_folder", "Output Folder Path:", value = "")
                 ),
                 actionButton("submit_btn", "Submit")
    ),
    mainPanel(
      tabsetPanel(
        id = "results_tab",
        tabPanel("Table", DTOutput("table_output")),
        tabPanel("Graph",
                 fluidRow(
                   column(10,
                          plotlyOutput("graph_output", height = "650px")
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  results <- eventReactive(input$submit_btn, {
    tryCatch({
      req(input$input_file)
      if (file.exists(paste0(input$output_folder,"/Subgroups.csv"))) {
        file.remove(paste0(input$output_folder,"/Subgroups.csv"))
      }
      
      fileInfo <- input$input_file
      filePath <- fileInfo$datapath
      
      input_file <- read.csv(filePath)# %>% drop_na()
      
      if (length(input_file) == 0) stop("No input file found.")
      
      set.seed(as.numeric(input$seed_input))
      lcmm_first <- lcmm(as.formula(paste0(input$dep_input," ~ ",input$indepr_input," + ", input$covs_input)),
                           random = as.formula(paste0(" ~ ",input$ran_input)),
                           subject = paste0(input$ID_input),
                           ng=1,
                           link = "linear",
                           data= input_file)

      set.seed(as.numeric(input$seed_input))
      lcmm_iteration <- gridsearch(rep = 30, maxiter = 200, minit = lcmm_first,
                                    lcmm(as.formula(paste0(input$dep_input," ~ ",input$indepr_input," + ", input$covs_input)),
                                         random = as.formula(paste0(" ~ ",input$ran_input)),
                                         mixture = as.formula(paste0(" ~ ",input$indepr_input)),
                                         subject = paste0(input$ID_input),
                                         data=input_file,
                                         maxiter=200,
                                         link = "linear",
                                         ng= as.numeric(input$num_input)))

      Group_class_all <- full_join(input_file, as.data.frame(lcmm_iteration$pprob[,1:2]), by = paste0(input$ID_input)) %>%
        mutate(class = factor(class)) %>% as_tibble()
      
      if (input$export_result == TRUE) {
        write.csv(Group_class_all,paste0(input$output_folder,"/Subgroups.csv"))
      }
      
      Group_class_all
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  output$table_output <- renderDT({
    req(results())
    data.table(results())
  }, options = list(pageLength = 12, scrollX = TRUE, autoWidth = FALSE))
  
  output$graph_output <- renderPlotly({
    req(results())
    Group_class_all <- results()
    ggplotly(ggline(Group_class_all, x = "Age", y =  "main_intake", group = "class", point.size = 1.5,
                    add = c("mean_se"), size=1.5, color = "class"))
  })
  
}

shinyApp(ui = ui, server = server)