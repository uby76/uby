# FTICRMS data

# Data preprocessing for FTICRMS is used to generate the files needed for ftmsRanalysis(https://emsl-computing.github.io/ftmsRanalysis/index.html) and metabodirect.

```R
#R version: 4.3.2
# install.packages("devtools")
#devtools::install_github("uby76/uby", force = TRUE)
library(uby)

peakObj <- process_csv_files(
  input_dir = "E:/data/test_data", #Import all data: all data
  meta_file = "E:/data/test_data/meta/meta.csv",#Sample Data
  intensity_output = "E:/data/test_data/output/Intensity_data.csv",#Expression Data
  combined_output = "E:/data/test_data/output/Infor_data.csv"#Molecular Identification Data
)
```

