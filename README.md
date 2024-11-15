# FTICRMS data

# Data preprocessing for FTICRMS is used to generate the files needed for ftmsRanalysis and metabodirect.



peakObj <- process_csv_files(
input_dir = "E:/FTIRCMS/lee_icrms/icrms/data", #Import all data: all data
meta_file = "E:/FTIRCMS/lee_icrms/icrms/data/meta/meta.csv",#Sample Data
intensity_output = "E:/FTIRCMS/lee_icrms/icrms/data/FTICRMS_output/Intensity_data.csv",#Expression Data
combined_output = "E:/FTIRCMS/lee_icrms/icrms/data/FTICRMS_output/Infor_data.csv"#Molecular Identification Data
)

