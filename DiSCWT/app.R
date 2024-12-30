################################################################################
# Shiny App:       DiSCWT (Discrete Specific Conductance and Water Temperature)
# Author:          J. Darling
# Date Created:    Dec 2024
# Last Modified:   Dec 26, 2024
# Version:         1.0
#
# Purpose:
#   This Shiny app processes discrete specific conductance (SC) and water 
#   temperature (WT) data from SVMAQ XML files. It allows users to upload XML 
#   files, process the data, and download cleaned outputs for Aquarius Samples.
#
# Key Features:
#   - Upload multiple XML files for batch processing.
#   - View processing logs and status messages.
#   - Download processed qwsample and qwresult .txt files.
#
# Dependencies:
#   - R version 4.0+
#   - Libraries: shiny, shinythemes,shinyjs, dplyr, tidyr, lubridate
#   - Custom Scripts: processing_functions.R (contains core processing logic)
#
# Instructions:
#   1. Run this app in RStudio or a Shiny server.
#   2. Follow the on-screen instructions to upload and process files.
#   3. Use the provided download buttons to retrieve processed outputs.
################################################################################


# Load required libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library("xml2")
library("dplyr")
library(stringr)
library(lubridate)




#Source the XML processing functions
#setwd("~/DiSCWT/v1.1")

source("functions/processing_functions.R")


#
#define the user interface
#

ui <- fluidPage(
  shinyjs::useShinyjs(),  # Initialize shinyjs
  #theme = shinytheme("cerulean"),
  tags$style(HTML("
    h1, h2, h4, h5, h6 {
      color: black;
    }
    body {
      margin-top: 20px;
      background-color: white;
      border-radius: 5px;
    }
  .btn-grey { 
    background-color: white; 
      color: black; 
      border: 1px solid #708090; 
    border-radius: 2px; 
      box-shadow: 1px 1px 2px rgba(0, 0, 0, 0.2); 
      width: auto;
      padding: 5px 15px;
  }
  .sidebarPanel {
      color: black;
      background-color: #f9f9f9 !important;
      border: 1px solid #808080; 
      border-radius: 5px; 
      padding: 15px; /* 
      box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.1); 
  }
")),
  

  # Title
  div(
    h3(strong("DiSCWT"), style = "display: inline; margin-right: -4px; face: bold; color: green;"), 
    tags$span("beta", style = "font-size: 14px; display: inline; color: green;")
  ),
  h5("Discrete Specific Conductance & Water Temperature processing tool", 
     style = "color: grey; margin-top: 2px;"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      class = "sidebarPanel",
      h4("Details"),
      p("Select your field office, upload your SVMAQ XML files, click 'Process Files', and 
        download the qwsample and qwresult TXT files using the buttons below."),
      #select field office with action buttons
      radioButtons("field_office_suffix", label = NULL,
                   choices = c("MOAB", "CEDAR", "SLC"),
                   inline = TRUE),
      fileInput("files", "Upload XML Files", multiple = TRUE, accept = c(".xml")),
      actionButton("process", "Process Files", class = "btn-grey", disabled = TRUE),  # Initially disabled
      br(), br(),
      h4("Status"),
      textOutput("status"),
      br(),
      downloadButton("download_samples", "QWsample", disabled = TRUE, class = "btn-grey"),  # Initially disabled
      downloadButton("download_results", "QWresult", disabled = TRUE, class = "btn-grey"),  # Initially disabled
      br(), br(),
      actionButton("clear", "Clear Files", class = "btn-grey"),
      br(), br()
    ),
    
    mainPanel(
      h5("Uploaded Files"),
      verbatimTextOutput("uploaded_files"),  # Display original file names
      br(),
      h4("Processing Log"),
      verbatimTextOutput("log")  # Display runtime log messages
    )
  ),
  
  # Notes below the sidebar
  div(
    style = "margin-left: 15px; margin-top: 10px; padding: 15px; border-radius: 5px;",
    h4("Processing Notes:"),
    div(
      p("The tool uses specific xml paths and regex patterns to capture necessary information while allowing some flexibility in how the data may appear in unsupported XML comments. Some of the logic used to extract the data is explained in", style = "display: inline;"),
      tags$a("README", href = "README.html", target = "_blank", style = "color: blue; text-decoration: underline; display: inline;"))
  ),
  
  # Footer
  div(class = "footer",
      style = "margin-left: 15px; margin-top: 20px; padding: 15px; border-radius: 5px;",
      # Title
      div(
        h5(strong("Contact: "), style = "display: inline; margin-right: 5px; face: bold; color: black;"), 
        tags$span("direct questions or comments to", 
                  a("jdarling@usgs.gov", href = "mailto:jdarling@usgs.gov")))
      ),
)




#
# Define the server
#

server <- function(input, output, session) {
  
  # Reactive field_office_suffix to pass to the processing functions
  reactive_field_office_suffix <- reactive({
    req(input$field_office_suffix)  # Ensure a selection is made
    input$field_office_suffix
  })
  
  # Observe the field office selection (optional for debugging or additional logic)
  # Reactive values to store processed data and logs
  processed_data <- reactiveValues(
    samples_df = data.frame(),
    results_df = data.frame()
  )
  
  log_data <- reactiveVal("")  # Store processing log messages
  status_message <- reactiveVal("")  # Store status messages
  uploaded_files <- reactiveVal(NULL)  # Store original file names
  
  # Observe file uploads and process files
  observeEvent(input$files, {
    if (length(input$files$name) > 0) {
      shinyjs::enable("process")  # Enable the Process Files button
    } else {
      shinyjs::disable("process")  # Disable if no files are uploaded
    }
  })
  
  # Observe file uploads and process files
  observeEvent(input$process, {
    req(input$files)  # Ensure files are uploaded
    req(input$field_office_suffix)  # Ensure a field office is selected
    
    # Store the selected field office suffix
    field_office_suffix <- input$field_office_suffix
    
    # Update the status message
    status_message("Processing files...")
    
    # Store the original file names
    uploaded_files(input$files$name)
    
    # Initialize log messages
    log_messages <- character()
    
    # Get the uploaded file paths
    files <- input$files$datapath
    filenames <- input$files$name  # Original file names
    
    # Initialize empty data frames
    all_samples <- data.frame()
    all_results <- data.frame()
    
    # Show a progress bar
    withProgress(message = "Processing files...", value = 0, {
      total_files <- length(files)
      
      for (i in seq_along(files)) {
        file <- files[i]
        original_name <- filenames[i]
        
        # Capture log output for this file
        file_log <- capture.output({
          cat("---- Processing File: ", original_name, " ----\n")
          
          # Process samples
          sample_data <- tryCatch({
            process_samples_file(file, field_office_suffix)
          }, error = function(e) {
            cat("Error processing samples for", original_name, ":", e$message, "\n")
            return(data.frame())
          })
          
          # Process results
          result_data <- tryCatch({
            process_results_file(file)
          }, error = function(e) {
            cat("Error processing results for", original_name, ":", e$message, "\n")
            return(data.frame())
          })
          
          # Combine results
          all_samples <- bind_rows(all_samples, sample_data)
          all_results <- bind_rows(all_results, result_data)
        })
        
        # Append file-specific log messages
        log_messages <- c(log_messages, file_log, "\n----------------------------------------\n")
        incProgress(1 / total_files, detail = paste("Processed", i, "of", total_files, "files"))
      }
    })
    
    # Update reactive values
    processed_data$samples_df <- all_samples
    processed_data$results_df <- all_results
    log_data(paste(log_messages, collapse = "\n"))
    
    # Update the status message
    status_message("Processing complete! Have a great day!")
    
    # Enable download buttons
    shinyjs::enable("download_samples")
    shinyjs::enable("download_results")
  })
  
  # Clear functionality for resetting app state
  observeEvent(input$clear, {
    processed_data$samples_df <- data.frame()
    processed_data$results_df <- data.frame()
    log_data("")  # Clear the log
    status_message("")  # Clear the status message
    uploaded_files(NULL)  # Clear uploaded file names
    
    #reset file input
    shinyjs::reset("files")
    
    # Disable download buttons
    shinyjs::disable("download_samples")
    shinyjs::disable("download_results")
  
    # Disable the Process Files button
    shinyjs::disable("process")
  })
  
  # Render the processing log
  output$log <- renderText({
    log_data()
  })
  
  # Display the uploaded file names
  output$uploaded_files <- renderText({
    if (!is.null(uploaded_files())) {
      paste("Uploaded files:\n", paste(uploaded_files(), collapse = ",\n"))
    } else {
      "No files uploaded."
    }
  })
  
  # Render the status message
  output$status <- renderText({
    status_message()
  })
  
  # Download handler for samples_df
  output$download_samples <- downloadHandler(
    filename = function() {
      "qwsample.txt"
    },
    content = function(file) {
      req(processed_data$samples_df)  # Ensure data is available
      write.table(
        processed_data$samples_df,
        file,
        sep = "\t",
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE
      )
    }
  )
  
  # Download handler for results_df
  output$download_results <- downloadHandler(
    filename = function() {
      "qwresult.txt"
    },
    content = function(file) {
      req(processed_data$results_df)  # Ensure data is available
      write.table(
        processed_data$results_df,
        file,
        sep = "\t",
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE
      )
    }
  )
}




shinyApp(ui = ui, server = server)

