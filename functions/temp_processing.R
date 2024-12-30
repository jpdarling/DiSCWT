
#Load Libraries: 
library(xml2)
library(stringr)
library(stringdist)


###THIS PART WILL BE INPUT BUT THE UI 
# Specify the directory containing XML files
#xml_dir <- "C:/Users/jdarling/OneDrive - DOI/Documents/DiSCWT/test_xmls/non-midsection"
# List all XML files in the directory
#xml_files <- list.files(xml_dir, pattern = "\\.xml$", ignore.case = TRUE, full.names = TRUE)


ext_cvrt_temp  <- function(text) {
  # Pattern to capture temperature and unit (with or without space between number and unit)
  # Added (?i) to make the regex case-insensitive for units
  pattern1 <- "(?:wt|temp|temperature|water temp)\\s*=\\s*(-?\\d+(?:\\.\\d+)?)\\s*(deg\\s*(?i)(C|F)|(?i)([CF]))?"
  
  # Pattern to capture numeric value followed by "deg" and valid unit (C or F)
  pattern2 <- "(-?\\d+(?:\\.\\d+)?)\\s*deg\\s*(?i)(C|F)?"
  
  # Pattern to capture single characters for units (C or F) following the numeric value
  # Exclude "f" if it follows directly after "ft" using a negative lookahead
  pattern3 <- "(?<=\\d)\\s*(?i)(C|F)(?!\\s*t)"
  
  # Match all occurrences
  matches1 <- str_match_all(text, pattern1)
  matches2 <- str_match_all(text, pattern2)
  matches3 <- str_match_all(text, pattern3)
  
  
  # Extract temperatures from pattern1 (keyword-based)
  temps1 <- sapply(matches1, function(x) {
    if (nrow(x) > 0) {
      temp_col <- x[, 2]  # Temperature value (group 2)
      temp_col[!is.na(temp_col)][1]  # Take the first valid temperature
    } else {
      NA
    }
  })
  
  # Extract temperatures from pattern2 (numeric followed by "deg")
  temps2 <- sapply(matches2, function(x) {
    if (nrow(x) > 0) {
      temp_col <- x[, 2]  # Numeric value before "deg"
      temp_col[!is.na(temp_col)][1]  # Take the first valid temperature
    } else {
      NA
    }
  })
  
  # Combine both temperature lists
  temps <- ifelse(!is.na(temps1), temps1, temps2)
  
  # Ensure temps is numeric
  temps <- as.numeric(temps)
  
  # Extract units from pattern1 (the unit after the temperature value)
  units1 <- sapply(matches1, function(x) {
    if (nrow(x) > 0) {
      unit_col <- x[, 4]  # Unit from group 4 (C or F)
      unit_col[!is.na(unit_col)][1]  # Take the first valid unit
    } else {
      NA
    }
  })
  
  # Extract units from pattern2 (if "deg" is followed by a unit)
  units2 <- sapply(matches2, function(x) {
    if (nrow(x) > 0) {
      unit_col <- x[, 3]  # Unit (C or F) after "deg"
      unit_col[!is.na(unit_col)][1]  # Take the first valid unit
    } else {
      NA
    }
  })
  
  # Extract units from pattern3 (C or F after a numeric value)
  units3 <- sapply(matches3, function(x) {
    if (nrow(x) > 0) {
      unit_col <- x[, 1]  # Single character C or F
      unit_col[!is.na(unit_col)][1]  # Take the first valid unit
    } else {
      NA
    }
  })
  
  # Combine all units (from pattern1, pattern2, and pattern3)
  units <- ifelse(!is.na(units1), units1, ifelse(!is.na(units2), units2, units3))
  
  # Handle cases where no unit is found (e.g., "temp=4.8"), explicitly assign NA to unit
  units[is.na(units) & !grepl("deg", text)] <- NA
  
  # Ensure that "deg" without "C" or "F" is treated as NA
  units[grepl("deg", units) & !grepl("C|F", units)] <- NA
  
  # Handle case when unit is NA: assign "C" and print message
  if (any(is.na(units))) {
    units[is.na(units)] <- "C"
    cat("00010 - *** No valid temperature unit found. Assuming °C.\n")
  }
  
  # Convert Fahrenheit to Celsius if unit is "F" and print message
  converted <- FALSE  # Flag to track whether conversion happened
  if (any(units == "F")) {
    # Capture original Fahrenheit temperatures before conversion
    original_temps_F <- temps[units == "F"]
    
    # Convert Fahrenheit to Celsius
    temps[units == "F"] <- (temps[units == "F"] - 32) * 5/9
    units[units == "F"] <- "C"
    
    # Print the message with the original Fahrenheit and converted Celsius values
    cat("00010 - Temperature of", original_temps_F, "°F was converted to", temps[units == "C"], "°C\n")
    converted <- TRUE  # Mark as converted
  }
  
  # Only print "Water temperature (00010):" if no conversion happened
  if (!converted) {
    cat("00010 - Water temperature:", as.numeric(temps), "°C\n")
  }
  
  # Return results
  data.frame(
    Temperature = round(as.numeric(temps), 2),
    Unit = units
  )
}





# Example strings
#texts <- c(
#  "routine visit. pre clean measurement. @ 08:47 MDT WT = 8.3 deg C, SC = 308 uS/cm . 0.24 ft depth, middle of flume.",
#  "SC=726, water temp=4.8, time= 1255. No HWM found.",
#  "Cond= 254, water temp=32.2F, time= 1455. No HWM found.",
#  "routine visit. no new hwms. @ 10:53 cond = 230uS/cm, temp = 1.2 degF, from middle of channel width = 1.2 ft, and 0.4 ft depth",
#  "routine visit. gage is ice-free. sc= 269 uS/cm, 5.5 deg , 1 ft from rew 0.5 ft depth.", 
#  "routine visit. plus external field review.  no new hwms. @ 11:34, conductivity = 185 us/cm , temp= 0.3 deg C
#  . point measurement in flume, 0.3 depth 0.5 ft from LEW."
#)

# Apply the function
#results <- do.call(rbind, lapply(texts, ext_cvrt_temp ))

# View results
#print(results)



















