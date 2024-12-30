################################################################################
# Script Name:    XML Data Processing Tool
# Author:         J Darling
# Date Created:   2024
# Last Modified:  12/23/2024
# Purpose:        
#   This script processes discrete specific conductance (SC) and water 
#   temperature (WT) data for Aquarius Samples. It reads XML files, extracts
#   relevant data, and outputs cleaned data files ("qwsample" and "qwresult").
#   The data are processed using two functions: "process_samples_file" and 
#   "process_results_file"
################################################################################


# Set to TRUE for debugging
# Set to FALSE for running the app

DEBUG_MODE <- FALSE   

#load XMLs used for debugging
if (DEBUG_MODE) {
  # Specify the directory containing XML files
  xml_dir <- "C:/Users/jdarling/OneDrive - DOI/Documents/DiSCWT/test_xmls/"
  # List all XML files in the directory
  xml_files <- list.files(xml_dir, pattern = "\\.xml$", ignore.case = TRUE, full.names = TRUE)
}

  ###########################


# Load Libraries: 
library(xml2)
library(dplyr)
library(lubridate)
library(stringr)
#install.packages("stringdist")
library(stringdist)
#field_office_suffix = #SOURCE THIS FROM THE RADIO BUTTONS IN THE UI OF THE APP

# Source additional funciton
source("functions/time_processing.R")
source("functions/temp_processing.R")
source("functions/sc_processing.R")


  ###########################


  #########################################
# Function to process a single XML file to make samples_df
  ##########################################
process_samples_file <- function(xml_file, field_office_suffix = "CEDAR") {
  # Read and parse the XML file
  xml_data <- read_xml(xml_file)
  
  # Convert XML to a list structure
  xml_list <- as_list(xml_data)
  
  # Extract the measurement method code
  Q_method_code <- xml_list$UsgsHydroML$Site$SiteData$SiteVisit$DischargeMeasurement$Channel$MeasurementMethodCode[[1]]

  
  # Extract StartDateTime
  start_date_time <- xml_list$UsgsHydroML$Site$SiteData$SiteVisit$StartDateTime[[1]]
  
  # Extract organization code and site number
  organization_code <- attr(xml_list$UsgsHydroML$Site$SiteIdentifier, "organizationCode")
  site_number <- xml_list$UsgsHydroML$Site$SiteIdentifier[[1]]
  #cat("Station ID:", site_number, "\n")
  timezone <- attr(xml_list$UsgsHydroML$Site$SiteData$SiteVisit$StartDateTime, "timeDatumCode")
  
  datetime_identifier <- process_datetime(xml_data, xml_list, start_date_time)
  
  reading_date <- format(datetime_identifier, "%Y%m%d%H%M")  # Format to YYYYMMDDHHMM
  
  # Generate unique ID by appending '02' to reading_date
  unique_id <- paste0(reading_date, "02")  # Unique ID format
  # Output debug information (optional)
  cat("Unique ID:", unique_id, "\n")
  
  
  # Calculate the water year
  reading_date_wy <- as.Date(datetime_identifier, tz = "America/Denver")
  
  if (format(reading_date_wy, "%m") %in% c("10", "11", "12")) {
    water_year <- as.integer(format(reading_date_wy, "%Y")) + 1  # October to December
  } else {
    water_year <- as.integer(format(reading_date_wy, "%Y"))  # January to September
  }
  
  # Construct the Code value
  state <- "UT"  # Should dynamically be provided; currently hardcoded
  code <- paste0(state, sprintf("%02d", water_year %% 100),field_office_suffix)  # Last two digits of the water year
  
  # Combine all extracted data into a list
  sample_data <- list(
    SINT = unique_id,
    `User Code` = "UT",
    Agency_code = organization_code, 
    Site_no = site_number,
    Sample_start_dt = reading_date,
    Sample_end_dt = "", #placeholder
    Medium_cd = "WS",
    Lab_id = "", #placeholder
    Project_cd = code,
    Aqfr_cd = "", #placeholder
    Samp_type_cd = "9",
    Anl_stat_cd = "U",
    Blank = "", #placeholder
    Hyd_cond_cd = "9",
    Hyd_event_cd = "9",
    Tissue_id = "",  # Placeholder
    Body_part_id = "",  # Placeholder
    Lab_smp_com = "", #placeholder, 
    Field_smp_com = "", #placeholder
    sample_tz_cd = timezone,
    tm_datum_rlblty_cd = "K",
    Collect_Agen_Cd = "USGS-WRD"
  )
  
  # Return the sample data as a data frame
  return(as.data.frame(sample_data, stringsAsFactors = FALSE))
}


if (DEBUG_MODE) {
  # Initialize an empty data frame to store all results
  samples_df <- data.frame()
  
  # Loop through each XML file and process it
  for (xml_file in xml_files) {
    sample_data <- process_samples_file(xml_file)  # Call the processing function
    samples_df <- bind_rows(samples_df, sample_data)  # Combine results
    }
  
  # Print out the results data frame
  print(samples_df)
}




##################################################

##Function to process a single XML file to make results_df

#################################################


# Define a mapping of measurement method codes to desired codes
discharge_code_mapping <- c(
  "QMIDSECTION" = "QSCMM",
  "QVOLUMETRIC" = "QVOLM",  
  "QFLUME" = "QFLUM",
  "QESTIMATED" = "Q-EST")

process_results_file <- function(xml_file) {
  # Read and parse the XML file
  xml_data <- read_xml(xml_file)
  
  # Convert XML to a list structure
  xml_list <- as_list(xml_data)
  
  # Extract StartDateTime
  start_date_time <- xml_list$UsgsHydroML$Site$SiteData$SiteVisit$StartDateTime[[1]]
  
  # Extract the SINT (Sample ID)
  timezone <-attr(xml_list$UsgsHydroML$Site$SiteData$SiteVisit$StartDateTime, "timeDatumCode")
  datetime_identifier <- process_datetime(xml_data, xml_list, start_date_time)
  
  reading_date <- format(datetime_identifier, "%Y%m%d%H%M")  # Format to YYYYMMDDHHMM

  # Generate unique ID by appending '02' to reading_date
  unique_id <- paste0(reading_date, "02")  # Unique ID format
  # Output debug information (optional)
  #cat("Unique ID:", unique_id, "\n")
  
  # Initialize an empty results dataframe
  results_df <- data.frame(SINT = character(), parameter_cd = character(), results_va = numeric(), method_cd = character(), stringsAsFactors = FALSE)
  
  
  
##############################################
  #Parameter 00003 - Measurement depth, feet
#############################################
  param_00003_value <- xml_list$UsgsHydroML$Site$SiteData$SiteVisit$DischargeMeasurement$Channel$MidSectionDischargeMeasurement$AcousticChannelReadings$Reading$Comment[[1]]
  
  if (!is.null(param_00003_value)) {
    # Use regular expression to extract the depth
    depth_value <- str_extract(param_00003_value, "\\d+\\.\\d+(?=\\s*ft\\s*depth|\\s*depth)")  # Extract depth value
    
    if (!is.na(depth_value)) {  # Check if a depth value was found
      # Store the extracted depth value in the results dataframe
      results_df <- bind_rows(results_df, data.frame(SINT = unique_id, parameter_cd = "00003", results_va = as.numeric(depth_value), method_cd = ""))
      cat("00003 - Measurement depth:", depth_value, "ft\n")
    }
  } else {
    
    ##########
    # Fallback: Extract from the site visit <comment>
    ##########
    site_visit_comment <- xml_text(xml_find_first(xml_data, "//d1:Comment", ns = xml_ns(xml_data)))
    
    if (!is.null(site_visit_comment) && site_visit_comment != "") {
      # Use regular expression to extract the depth from the site visit comment
      depth_value_fallback <- str_extract(site_visit_comment, "\\d+\\.\\d+(?=\\s*ft\\s*depth|\\s*depth)")  # Extract depth value
      
      if (!is.na(depth_value_fallback)) {  # Check if a depth value was found in the fallback
        # Store the extracted depth value in the results dataframe
        results_df <- bind_rows(results_df, data.frame(SINT = unique_id, parameter_cd = "00003", results_va = as.numeric(depth_value_fallback), method_cd = ""))
        cat("00003 - Measurement depth:", depth_value_fallback, "ft \n")
      } else {
        cat("00003 - *** No Measurement depth found. \n")
      }
    } else {
      cat("*** No site visit comment found in", xml_file, "\n")
    }
  }  # This closes the if statement for param_00003_value
  
  

  ##############################################
  ## Parameter 00004 - Stream Width, feet
  ##############################################
  param_00004_value <- xml_list$UsgsHydroML$Site$SiteData$SiteVisit$DischargeMeasurement$Channel$MidSectionDischargeMeasurement$WidthMeasure[[1]]  # Adjust path based on your XML
  
  if (!is.null(param_00004_value)) {
    # Convert param_00004_value to numeric
    param_00004_value <- as.numeric(param_00004_value)
    results_df <- bind_rows(results_df, data.frame(SINT = unique_id, parameter_cd = "00004", results_va = param_00004_value, method_cd = ""))
    cat("00004 - Stream width:", param_00004_value, "ft\n")
  } else {
    
    ##########
    # Fallback: Extract from the site visit <comment>
    ##########
    site_visit_comment <- xml_text(xml_find_first(xml_data, "//d1:Comment", ns = xml_ns(xml_data)))
    
    if (!is.null(site_visit_comment) && site_visit_comment != "") {
      # Use regular expression to extract the width from the site visit comment
      width_value_fallback <- str_match(site_visit_comment, "(?i)width\\s*=?\\s*(\\d+\\.\\d+)\\s*ft")
      
      if (!is.na(width_value_fallback[1, 2])) {
        # Assign fallback value to param_00004_value
        param_00004_value <- as.numeric(width_value_fallback[1, 2])
        results_df <- bind_rows(results_df, data.frame(SINT = unique_id, parameter_cd = "00004", results_va = param_00004_value, method_cd = ""))
        cat("00004 - Stream Width:", param_00004_value, "ft \n")
      } else {
        cat("00004 - *** No stream width found. \n")
        param_00004_value <- NA  # Explicitly set to NA if no fallback
      }
    } else {
      cat("*** No site visit comment found for fallback.\n")
      param_00004_value <- NA  # Explicitly set to NA if no comment
    }
  }
  
  
  ##############################################
  ## Parameter 00009 - Distance from LEW
  ##############################################
  param_00009 <- NA  # Initialize this variable
  
  # Extract primary comment from <Reading><Comment>
  comment_value <- xml_list$UsgsHydroML$Site$SiteData$SiteVisit$DischargeMeasurement$Channel$MidSectionDischargeMeasurement$AcousticChannelReadings$Reading$Comment[[1]]
  
  # Fallback to <SiteVisit><Comment> if primary comment is missing
  if (is.null(comment_value) || comment_value == "") {
    site_visit_comment <- xml_text(xml_find_first(xml_data, "//d1:Comment", ns = xml_ns(xml_data)))
    comment_value <- site_visit_comment  # Use fallback comment
  }
  
  # Debugging output
  #cat("Extracted comment value:", comment_value, "\n")
  #cat("Stream width (param_00004_value):", param_00004_value, "\n")
  
  # Check if param_00004_value (stream width) is available
  if (!is.null(param_00004_value) && !is.na(param_00004_value)) {
    # Process the comment for distance from LEW
    if (!is.null(comment_value) && comment_value != "") {
      # Check for the keyword "middle" in the comment
      if (str_detect(comment_value, "(?i)middle|mid|mid chan")) {
        # If "middle" is found, divide the width by 2
        param_00009 <- as.numeric(param_00004_value) / 2
        cat("00009 - *** Detected 'middle' in comment. Distance from left edge calculated as half of channel width:", param_00009, "ft\n")
      } else {
        # Adjust regex to capture distance and reference (LEW or RB/REW)
        extracted_value <- str_extract(comment_value, "(?i)(\\d+)\\s*ft\\s*from\\s*(lb|lew|loew|r[e]?w|rb)")
        if (!is.na(extracted_value)) {
          # Extract the numeric part from the result
          numeric_value <- as.numeric(str_extract(extracted_value, "\\d+"))
          if (!is.na(numeric_value)) {
            if (str_detect(extracted_value, "(?i)(r[e]?w|rb)")) {
              # If distance is from REW/RB, convert using width
              param_00009 <- as.numeric(param_00004_value) - numeric_value
              cat("00009 - Distance was entered as", numeric_value, "ft from Right Bank and was converted to", param_00009, "ft from Left Bank.\n")
            } else {
              # Distance is already from LEW, use as-is
              param_00009 <- numeric_value
              cat("00009 - Distance from left edge water:", param_00009, "ft \n")
            }
          }
        } else {
          cat("00009 - *** No distance from left edge found.\n")
        }
      }
    } else {
      cat("00009 - *** No valid comment found for distance from left edge.\n")
    }
  } else {
    cat("00009 - *** Cannot calculate distance from left edge due to missing WidthMeasure (param_00004_value).\n")
  }
  
  # Add param_00009 to the results dataframe if valid
  if (!is.na(param_00009)) {
    results_df <- bind_rows(results_df, data.frame(
      SINT = unique_id,
      parameter_cd = "00009",
      results_va = param_00009,
      method_cd = ""
    ))
  }
  

 ##############################################
## Parameter 00010 - Water Temperature  
##############################################
  param_00010_value <- xml_list$UsgsHydroML$Site$SiteData$SiteVisit$DischargeMeasurement$Channel$MidSectionDischargeMeasurement$AcousticChannelReadings$Reading$VerificationTemperatureMeasure[[1]]
  temperature_units <- xml_text(xml_find_first(xml_data, "//d1:TemperatureUnits", ns = xml_ns(xml_data)))
  
  # Check if param_00010_value is not NA or null
  if (!is.null(param_00010_value) && !is.na(param_00010_value)) {
    # Convert Fahrenheit to Celsius if needed
    if (temperature_units == "F") {
      original_temp_F <- as.numeric(param_00010_value)
      param_00010_value <- (original_temp_F - 32) * 5/9  # Convert to Celsius
      temperature_units <- "C"  # Update the units
      
      # Log the conversion
      cat("00010 - Temperature of", original_temp_F, "°F was converted to", round(param_00010_value, 2), "deg C\n")
    } else {
      # Log the temperature as-is
      cat("00010 - Water temperature:", param_00010_value, "deg", temperature_units, "\n")
    }
    
    # Add the temperature value to the results dataframe
    results_df <- bind_rows(results_df, data.frame(
      SINT = unique_id,
      parameter_cd = "00010",
      results_va = as.numeric(param_00010_value),  # Store the temperature value here
      method_cd = "THM01"
    ))
  } else {
    # Fallback to look in site visit comments if not found in nested XML structure
    site_visit_comment <- xml_text(xml_find_first(xml_data, "//d1:Comment", ns = xml_ns(xml_data)))
    
    if (!is.null(site_visit_comment) && site_visit_comment != "") {
      # Apply ext_cvrt_temp to extract temperature and unit
      temp_data <- ext_cvrt_temp(site_visit_comment)
      
      # Extract the numeric temperature value and assign it
      param_00010_value <- temp_data$Temperature  # Extract numeric temperature value
      
      # Add the temperature value to the results dataframe
      results_df <- bind_rows(results_df, data.frame(
        SINT = unique_id,
        parameter_cd = "00010",
        results_va = param_00010_value,  # Store the temperature value here
        method_cd = "THM01"
      ))
    } else {
      cat("*** No site visit comment found.\n")
    }
  }
  
  
  ##############################################
## parameter 00061 - Discharge
  ##############################################
  discharge_value <- xml_list$UsgsHydroML$Site$SiteData$SiteVisit$DischargeMeasurement$Channel$DischargeMeasure[[1]]  # Pulling discharge data
  method_code <- xml_list$UsgsHydroML$Site$SiteData$SiteVisit$DischargeMeasurement$Channel$MeasurementMethodCode[[1]]  # Extracting method code
  cat("00061 - Discharge:", discharge_value, "cfs,",method_code,"\n")
  
  if (!is.null(discharge_value) && !is.null(method_code)) {
    # Map the method code to the corresponding code in discharge_code_mapping
    mapped_method_code <- discharge_code_mapping[method_code]
    
    if (!is.na(mapped_method_code)) {
      results_df <- bind_rows(results_df, data.frame(SINT = unique_id, parameter_cd = "00061", results_va = as.numeric(discharge_value), method_cd = mapped_method_code))
    } else {
      cat("00061 - *** No valid mapping found for method code", method_code, "in", xml_file, "\n")
    }
  } else {
    cat("00061 - *** No discharge value or method code found in", xml_file, "\n")
  }
  
  ##############################################
## Parameter 00063 - Number of Sampling Points
  ##############################################
  results_df <- bind_rows(results_df, data.frame(SINT = unique_id, parameter_cd = "00063", results_va = 1, method_cd = ""))  # Fixed value of 1
  
  ##############################################
## #Parameter 00065 - Gage Height
  ##############################################
  
  gage_height_value <- xml_text(xml_find_first(xml_data, "//d1:StageMeasure", ns = xml_ns(xml_data)))
  #gage_height_value <- xml_text(xml_list$UsgsHydroML$Site$SiteData$SiteVisit$DischargeMeasurement$StageMeasure[[1]])
  
  if (!is.null(gage_height_value) && gage_height_value != "") {
    # Extract method codes using <LocationSensor> nodes
    ns <- xml_ns(xml_data)
    location_sensors <- xml_find_all(xml_data, "//d1:LocationSensor", ns = ns)
    
    sensor_data <- do.call(rbind, lapply(location_sensors, function(x) {
      data.frame(
        Parameter = xml_text(xml_find_first(x, ".//d1:Parameter/d1:Name", ns = ns)),
        MethodCode = xml_text(xml_find_first(x, ".//d1:SensorType/d1:MethodCode", ns = ns)),
        SensorName = xml_text(xml_find_first(x, ".//d1:Sensor/d1:SensorName", ns = ns)),
        stringsAsFactors = FALSE
      )
    }))
    
    # Filter for relevant SensorName and MethodCode
    reference_stage <- sensor_data %>%
      filter(SensorName %in% c("WWG", "RP", "Staff"))
    
    if (nrow(reference_stage) > 0) {
      method_code <- toupper(reference_stage$SensorName[1])  # Take the first match, convert to uppercase
      results_df <- bind_rows(results_df, data.frame(
        SINT = unique_id,
        parameter_cd = "00065",
        results_va = as.numeric(gage_height_value),
        method_cd = method_code
      ))
      cat("00065 - Gage height:",gage_height_value,"ft,",method_code, "\n")
    } else {
      cat("00065 - *** No matching SensorName (RP, WWG, Staff) found for gage height \n")
    }
  } else {
    cat("00065 - *** No gage height associated with discharge measurment - possibly ice-affected \n")
  }

  ##############################################
  ## Parameter 00095 - Specific Conductance
  ##############################################
  # Extract SC from param_00003_value first
  specific_conductance <- extract_sc(param_00003_value, param_00010_value)
  
  # Fallback to site visit comment if no SC found
  if (is.na(specific_conductance)) {
    # Extract site visit comment
    site_visit_comment <- xml_text(xml_find_first(xml_data, "//d1:Comment", ns = xml_ns(xml_data)))
    specific_conductance <- extract_sc(site_visit_comment, param_00010_value)
    
    if (is.na(specific_conductance)) {
      cat("00095 - *** No SC value found in fallback check.\n")
    }
  }
  
  # Add SC to results dataframe if valid
  if (!is.na(specific_conductance)) {
    results_df <- bind_rows(results_df, data.frame(
      SINT = unique_id,
      parameter_cd = "00095",
      results_va = signif(specific_conductance, 3),  # Round to 3 significant figures
      method_cd = "SC001"
    ))
  } else {
    cat("00095 - *** No SC value found after primary and fallback checks.\n")
  }
  
  
  
  ##############################################
  ## Default parameters - possibly have UI beable to change
  ##############################################
  
  # Add default parameters
  default_params <- data.frame(
    SINT = unique_id,
    parameter_cd = c("50280", "71999", "82398", "84164", "99111"),
    results_va = c(1001, 10, 50, 8000, 1),
    method_cd = ""  # Leave method code empty for now
  )
  
  results_df <- bind_rows(results_df, default_params)
  
  ##############################################
  
  #format results_df to match needed for qwresults to load into aqsamples
  results_df <- results_df %>%
    mutate(
      col4 = "", col5 = "",  # Add empty columns for positions 4 and 5
      col7 = "", col8 = "", col9 = "", col10 = "", col11 = "", col12 = "",
      col13 = "", col14 = "", col15 = "", col16 = "", col17 = "", col18 = "",
      col19 = "",  # Empty columns from 7 to 19
      Ana_Enty_Cd = "USGS-WRD" # Add final column
    ) %>%
    select(
      SINT, parameter_cd, results_va, 
      col4, col5, method_cd, col7, col8, col9, col10, col11, col12, col13, col14, col15, 
      col16, col17, col18, col19, Ana_Enty_Cd
    )  # Reorder columns
  
  return(results_df)  # Return the results dataframe
}


############################
###########################

# Loop through each XML file to get results
if (DEBUG_MODE) {
  # Initialize an empty data frame for results
  all_results_df <- data.frame()
  
  #loop through files loaded in the directory , at top
  for (xml_file in xml_files) {
    results_data <- tryCatch({
      process_results_file(xml_file)  # Call the processing function for results
      }, error = function(e) {
        cat("Error processing", xml_file, ":", e$message, "\n")
        return(data.frame())  # Return an empty dataframe to not disrupt the loop
        })
    
    all_results_df <- bind_rows(all_results_df, results_data)  # Combine results
    }
  ##Print out the results data frame
  print(all_results_df, row.names = FALSE)
  all_results_df %>% filter(parameter_cd =="00010")
  
  # Initialize an empty data frame for results
  all_results_df <- data.frame()
}

