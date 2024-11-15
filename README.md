# FTICRMS data

# Data preprocessing for FTICRMS is used to generate the files needed for ftmsRanalysis and metabodirect.


#R version: 4.3.2
# install.packages("devtools")
devtools::install_github("uby76/uby", force = TRUE)

library(uby)

peakObj <- process_csv_files(
  input_dir = "E:/data/test", #Import all data: all data
  meta_file = "E:/data/test/meta/meta.csv",#Sample Data
  intensity_output = "E:/data/test/Intensity_data.csv",#Expression Data
  combined_output = "E:/data/test/Infor_data.csv"#Molecular Identification Data
)
