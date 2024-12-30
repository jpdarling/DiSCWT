
#Load Libraries: 
library(xml2)
library(stringr)
library(stringdist)


# Define a function to extract and process SC or conductivity values
extract_sc <- function(input_string, temperature_value = NA) {
  # Initialize variables
  specific_conductance <- NA
  alpha <- 0.0191  # Temperature compensation factor
  
  if (!is.null(input_string) && input_string != "") {
    # First check for "SC" as it's the highest priority
    sc_match <- str_extract(input_string, "(?i)sc\\s*(=|:|\\s)*\\s*[0-9,]+(\\s*(uS|us)/?cm|\\s*(uS|us)|\\s*(mS|ms)/?cm)?")
    
    if (!is.na(sc_match)) {
      # Extract numeric SC value directly
      numeric_sc <- as.numeric(str_remove_all(str_extract(sc_match, "[0-9,]+"), ","))
      
      # Check for units (only uS/cm, uS, or mS/cm as valid units)
      unit_match <- str_extract(sc_match, "(?i)(uS|us)/?cm|(?i)(uS|us)|(?i)(mS|ms)/?cm")
      
      if (is.na(unit_match)) {
        cat("00095 - No specific conductance units provided. Assuming μS/cm.\n")
        unit_match <- "uS/cm"  # Default to uS/cm
      } else if (grepl("mS|ms", unit_match)) {
        # If mS or ms is found, it's a non-standard unit
        cat("00095 *** Non standard units are provided. Possible conversion needed.\n")
        unit_match <- NA  # Mark this as invalid
      } else if (grepl("uS|us", unit_match) && !grepl("/cm", unit_match)) {
        # If uS or us found without /cm, assume it's uS/cm
        unit_match <- paste0(unit_match, "/cm")
      }
      
      specific_conductance <- numeric_sc
      cat("00095 - Specific conductance:", specific_conductance, "μS/cm \n")
      
    } else {
      # If no match for "SC", check for "spec cond" or "specific cond"
      spec_cond_match <- str_extract(input_string, "(?i)(spec|specific)\\s*(cond|conductivity)\\s*(=|:|\\s)*\\s*[0-9,]+(\\s*(uS|us)/?cm|\\s*(uS|us)|\\s*(mS|ms)/?cm)?")
      
      if (!is.na(spec_cond_match)) {
        # If "spec cond" or "specific cond" is found, treat it as SC
        numeric_sc <- as.numeric(str_remove_all(str_extract(spec_cond_match, "[0-9,]+"), ","))
        
        # Check for units (only uS/cm, uS, or mS/cm as valid units)
        unit_match <- str_extract(spec_cond_match, "(?i)(uS|us)/?cm|(?i)(uS|us)|(?i)(mS|ms)/?cm")
        
        if (is.na(unit_match)) {
          cat("00095 - No units provided. Assuming μS/cm.\n")
          unit_match <- "uS/cm"  # Default to uS/cm
        } else if (grepl("mS|ms", unit_match)) {
          # If mS or ms is found, it's a non-standard unit
          cat("00095 *** Non standard units are provided. Possible conversion needed.\n")
          unit_match <- NA  # Mark this as invalid
        } else if (grepl("uS|us", unit_match) && !grepl("/cm", unit_match)) {
          # If uS or us found without /cm, assume it's uS/cm
          unit_match <- paste0(unit_match, "/cm")
        }
        
        specific_conductance <- numeric_sc
        cat("00095 - Specific conductance:", specific_conductance, "μS/cm\n")
      } else {
        # If no match for "spec cond" or "specific cond", check for "conductivity" or "cond"
        conductivity_match <- str_extract(input_string, "(?i)(conductivity|cond|C)\\s*(=|:|\\s)*\\s*[0-9,]+(\\s*(uS|us)/?cm|\\s*(uS|us)|\\s*(mS|ms)/?cm)?")
        
        if (!is.na(conductivity_match)) {
          # Extract numeric value (ignore commas) and handle temperature adjustment if available
          numeric_conductance <- as.numeric(str_remove_all(str_extract(conductivity_match, "[0-9,]+"), ","))
          
          # Check for units (only uS/cm, uS, or mS/cm as valid units)
          unit_match <- str_extract(conductivity_match, "(?i)(uS|us)/?cm|(?i)(uS|us)|(?i)(mS|ms)/?cm")
          
          if (is.na(unit_match)) {
            cat("00095 - No specific conductance units provided. Assuming μS/cm.\n")
            unit_match <- "uS/cm"  # Default to uS/cm
          } else if (grepl("mS|ms", unit_match)) {
            # If mS or ms is found, it's a non-standard unit
            cat("00095 *** Non standard units are provided. Possible conversion needed.\n")
            unit_match <- NA  # Mark this as invalid
          } else if (grepl("uS|us", unit_match) && !grepl("/cm", unit_match)) {
            # If uS or us found without /cm, assume it's uS/cm
            unit_match <- paste0(unit_match, "/cm")
          }
          
          if (!is.na(temperature_value)) {
            # Apply temperature adjustment if temperature value is provided
            specific_conductance <- numeric_conductance / (1 + alpha * (as.numeric(temperature_value) - 25))
            cat("00095 - Conductivity value of", numeric_conductance, "μS/cm at", temperature_value, "°C converted to SC:", specific_conductance, "μS/cm \n")
          } else {
            # Use conductivity value without temperature adjustment
            specific_conductance <- numeric_conductance
            cat("00095 *** Conductivity value not temperature adjusted:", specific_conductance," μS/cm \n")
          }
        }
      }
    }
  }
  
  return(specific_conductance)
}





 


  # Test cases
#  input1 <- "C 254 uS/cm"
#  input2 <- "SC:726 uS/cm"
#  input3 <- "conductivity= 320"
  
  # Apply function
#  sc1 <- extract_sc(input1, 20)
#  sc2 <- extract_sc(input2, 20)
#  sc3 <- extract_sc(input3, 20)
  












