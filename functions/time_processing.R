# Load necessary packages
# Load necessary libraries
library(xml2)
library(lubridate)
library(stringr)
process_datetime <- function(xml_data, xml_list, start_date_time) {
  # Extract datetime_identifier from XML
  datetime_identifier <- xml_list$UsgsHydroML$Site$SiteData$SiteVisit$DischargeMeasurement$Channel$MidSectionDischargeMeasurement$AcousticChannelReadings$Reading$ReadingDateTime[[1]]
  
  # Check if datetime_identifier is valid and in correct format
  if (!is.null(datetime_identifier) && datetime_identifier != "") {
    parsed_datetime <- tryCatch(
      ymd_hms(datetime_identifier, tz = "America/Denver"),
      error = function(e) {
        cat("Error parsing datetime_identifier: ", e$message)
        return(NA)
      }
    )
    
    if (!is.na(parsed_datetime)) {
      return(parsed_datetime)  # Use valid datetime_identifier
    } else {
      cat("Invalid format for datetime_identifier. Falling back to comment.")
    }
  }
  
  # Fallback to comment if datetime_identifier is missing
  if (is.null(datetime_identifier) || datetime_identifier == "") {
    # Extract the comment from XML
    site_visit_comment <- xml_text(xml_find_first(xml_data, "//d1:Comment", ns = xml_ns(xml_data)))
    
    # Regex to extract time from the comment (handles both 24-hour and AM/PM times)
    datetime_match <- str_extract(site_visit_comment, 
                                  "(?i)(?:@\\s*|at\\s*|time\\s*=\\s*|\\s)(\\d{1,2}:\\d{2}|\\d{4})(\\s*(AM|PM|mdt|mst|hrs)?)?")
    
    # If time is found in the comment
    if (!is.na(datetime_match)) {
      # Clean the extracted time to ensure it's in HH:mm format
      time_cleaned <- str_trim(datetime_match)  # Trim whitespace
      
      # If the time doesn't have a colon, add one
      if (nchar(time_cleaned) == 4) {
        time_cleaned <- paste0(substr(time_cleaned, 1, 2), ":", substr(time_cleaned, 3, 4))  # Convert to HH:mm
      }
      
      # Check if the time contains AM or PM and convert to 24-hour time if needed
      if (grepl("(AM|PM)", time_cleaned, ignore.case = TRUE)) {
        # Try to convert the time only if it contains AM/PM
        time_24hr <- tryCatch({
          strptime(time_cleaned, format = "%I:%M %p")  # Convert to 24-hour format
        }, error = function(e) {
          # If strptime fails, print the error and return NA
          message("Error with strptime conversion: ", e$message)
          return(NA)
        })
        
        # If conversion was successful (not NA), update the time
        if (!is.na(time_24hr)) {
          time_cleaned <- format(time_24hr, "%H:%M")  # Convert to 24-hour time (HH:mm)
        }
      }
      
      # Extract the date from StartDateTime
      start_date_obj <- ymd_hms(start_date_time, tz = "America/Denver")
      
      # Combine the date from StartDateTime with the cleaned time
      combined_datetime <- paste0(format(start_date_obj, "%Y-%m-%d"), " ", time_cleaned)
      
      # Create a POSIXct object in America/Denver timezone
      datetime_identifier <- ymd_hm(combined_datetime, tz = "America/Denver")
    } else {
      # If no time is found in the comment, use StartDateTime
      datetime_identifier <- ymd_hms(start_date_time, tz = "America/Denver")
    }
  } else {
    # If datetime_identifier is available, convert it
    datetime_identifier <- ymd_hms(datetime_identifier, tz = "America/Denver")
  }
  
  # Return datetime_identifier (time in correct format)
  return(datetime_identifier)
}
