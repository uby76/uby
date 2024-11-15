#' Process, Merge, and Analyze CSV Files
#' jianwei363@gmail.com
#' This function processes all CSV files in a specified directory, merges intensity data,
#' combines additional information columns, and integrates the data into a peakData object
#' for further analysis.
#' R version:4.3.2
#'
#' @param input_dir Character. Path to the directory containing CSV files.
#' @param intensity_output Character. Path to save the merged intensity data CSV.
#' @param combined_output Character. Path to save the combined information CSV.
#' @param meta_file Character. Path to the metadata CSV file.
#' @return A peakData object for further analysis.
#' @export
process_csv_files <- function(input_dir, intensity_output, combined_output, meta_file) {
  # Installation of dependency packages
  install_dependencies <- function() {
    # Install the devtools package
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools")
    }
    
    # Install the necessary packages
    required_packages <- c("readr", "dplyr", "tools", "ftmsRanalysis", "uby")
    
    for (pkg in required_packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
      }
    }
    
    # Installing a specific version of the ftmsRanalysis package
    if (!requireNamespace("ftmsRanalysis", quietly = TRUE)) {
      devtools::install_github("EMSL-Computing/ftmsRanalysis@1.0.0")
    }
    
    # Installing the uby package
    if (!requireNamespace("uby", quietly = TRUE)) {
      install.packages("uby")
    }
  }
  
  # Installation of dependency packages
  install_dependencies()
  
  # Load the necessary packages
  library(readr)
  library(dplyr)
  library(tools)
  library(ftmsRanalysis)
  
  # Setting up the working directory
  setwd(input_dir)
  
  # List all CSV files in a folder
  file_list <- list.files(pattern = "\\.csv$")
  
  # Initialize an empty data frame
  merged_data <- NULL
  combined_data <- NULL
  
  # Iterate through each file
  for (file in file_list) {
    # Read each CSV file
    sample_data <- read_csv(file, show_col_types = FALSE)
    
    # Processing merged_data
    intensity_col_name <- paste0(file_path_sans_ext(file), "")
    if ("Intensity" %in% colnames(sample_data)) {
      sample_data_merged <- sample_data %>%
        select(Mass, Intensity = matches("Intensity")) %>%
        rename_at("Intensity", ~ intensity_col_name)
      sample_data_merged$Mass <- as.character(sample_data_merged$Mass)
      if (is.null(merged_data)) {
        merged_data <- sample_data_merged
      } else {
        merged_data <- full_join(merged_data, sample_data_merged, by = "Mass")
      }
    }
    
    # Processing combined_data
    file_name <- file_path_sans_ext(file)
    if (ncol(sample_data) > 2) {
      sample_data_combined <- sample_data %>%
        select(Mass, starts_with("s") | (3:ncol(sample_data)))
      sample_data_combined$Mass <- as.character(sample_data_combined$Mass)
      combined_data <- bind_rows(combined_data, sample_data_combined)
    }
  }
  
  # Replace NA value with 0
  merged_data[is.na(merged_data)] <- 0
  
  # Save merged data
  write.csv(merged_data, intensity_output, row.names = FALSE)
  
  # De-duplication and preservation of other information data
  combined_data <- combined_data %>% distinct(Mass, .keep_all = TRUE)
  write.csv(combined_data, combined_output, row.names = FALSE)
  
  # Import data
  data_fticrms <- read.csv(intensity_output) # Intensity data for each sample
  e_fticrms <- read.csv(combined_output)    # Information on Mass Spectrometry
  emeta <- read.csv(meta_file)              # metadata file
  
  # Build e_fticrmsdata data frame
  columns_to_extract <- c("Mass", "NeutralMass", "Error", "C", "H", "O", "N", "C13", "S", "P", "Na")
  e_fticrmsdata <- e_fticrms %>%
    select(all_of(intersect(columns_to_extract, names(e_fticrms)))) %>%
    bind_cols(
      setNames(lapply(setdiff(columns_to_extract, names(e_fticrms)), function(col) {
        rep(0, nrow(e_fticrms))  # The length is the number of rows and the value is 0
      }), setdiff(columns_to_extract, names(e_fticrms)))
    ) %>%
    select(all_of(columns_to_extract))
  
  # Create peakData object
  peakObj <- as.peakData(
    e_data = data_fticrms,
    f_data = emeta,
    e_meta = e_fticrmsdata,
    edata_cname = "Mass",
    mass_cname = "Mass",
    fdata_cname = "SampleID",
    c_cname = "C",
    h_cname = "H",
    o_cname = "O",
    n_cname = "N",
    s_cname = "S",
    p_cname = "P"
  )
  
  return(peakObj)
}
