#' Process, Merge, and Analyze CSV Files
#'
#' This function processes all CSV files in a specified directory, merges intensity data,
#' combines additional information columns, and integrates the data into a peakData object
#' for further analysis.
#'
#' @param input_dir Character. Path to the directory containing CSV files.
#' @param intensity_output Character. Path to save the merged intensity data CSV.
#' @param combined_output Character. Path to save the combined information CSV.
#' @param meta_file Character. Path to the metadata CSV file.
#' @return A peakData object for further analysis.
#' @export
process_csv_files <- function(input_dir, intensity_output, combined_output, meta_file) {
  # 加载必要的包
  library(readr)
  library(dplyr)
  library(tools)
  library(ftmsRanalysis)
  
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
  
  # 导入数据
  data_fticrms <- read.csv(intensity_output) # 每一个样本的强度数据
  e_fticrms <- read.csv(combined_output)    # 质谱的信息
  emeta <- read.csv(meta_file)              # 元数据文件
  
  # 构建 e_fticrmsdata 数据框
  columns_to_extract <- c("Mass", "NeutralMass", "Error", "C", "H", "O", "N", "C13", "S", "P", "Na")
  e_fticrmsdata <- e_fticrms %>%
    select(all_of(intersect(columns_to_extract, names(e_fticrms)))) %>%
    bind_cols(
      setNames(lapply(setdiff(columns_to_extract, names(e_fticrms)), function(col) {
        rep(0, nrow(e_fticrms))  # 长度为行数，值为 0
      }), setdiff(columns_to_extract, names(e_fticrms)))
    ) %>%
    select(all_of(columns_to_extract))
  
  # 创建 peakData 对象
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
