#' Process and Merge CSV Files
#'
#' This function processes all CSV files in a specified directory, merges intensity data,
#' and combines additional information columns.
#'
#' @param input_dir Character. Path to the directory containing CSV files.
#' @param intensity_output Character. Path to save the merged intensity data CSV.
#' @param combined_output Character. Path to save the combined information CSV.
#' @export
process_csv_files <- function(input_dir, intensity_output, combined_output) {
  # 加载必要的包
  library(readr)
  library(dplyr)
  library(tools)
  
  # 设置工作目录
  setwd(input_dir)
  
  # 列出文件夹中的所有 CSV 文件
  file_list <- list.files(pattern = "\\.csv$")
  
  # 初始化空的数据框
  merged_data <- NULL
  combined_data <- NULL
  
  # 遍历每个文件
  for (file in file_list) {
    # 读取每个 CSV 文件
    sample_data <- read_csv(file, show_col_types = FALSE)
    
    # 处理 merged_data
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
    
    # 处理 combined_data
    file_name <- file_path_sans_ext(file)
    if (ncol(sample_data) > 2) {
      sample_data_combined <- sample_data %>%
        select(Mass, starts_with("s") | (3:ncol(sample_data)))
      sample_data_combined$Mass <- as.character(sample_data_combined$Mass)
      combined_data <- bind_rows(combined_data, sample_data_combined)
    }
  }
  
  # 将 NA 值替换为 0
  merged_data[is.na(merged_data)] <- 0
  
  # 保存合并后的数据
  write.csv(merged_data, intensity_output, row.names = FALSE)
  
  # 去重并保存其他信息数据
  combined_data <- combined_data %>% distinct(Mass, .keep_all = TRUE)
  write.csv(combined_data, combined_output, row.names = FALSE)
}
