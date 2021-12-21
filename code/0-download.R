# load libraries
library(tidyverse)
library(haven)

# download 2019 BRFSS data from CDC website
# codebook here: https://www.cdc.gov/brfss/annual_data/2019/pdf/codebook19_llcp-v2-508.HTML
brfss_url = "https://www.cdc.gov/brfss/annual_data/2019/files/LLCP2019XPT.zip"
download.file(url = brfss_url, destfile = "../data/raw/LLCP2019XPT.zip")
brfss_2019_raw = read_xpt("../data/raw/LLCP2019XPT.zip")

# write raw data to file
write_xpt(brfss_2019_raw, "../data/raw/brfss_2019_raw.xpt")

# download 2019 NMHSS data from SAMHSA website
# codebook here: https://www.datafiles.samhsa.gov/sites/default/files/field-uploads-protected/studies/N-MHSS-2019/N-MHSS-2019-datasets/N-MHSS-2019-DS0001/N-MHSS-2019-DS0001-info/N-MHSS-2019-DS0001-info-codebook.pdf
nmhss_url = "https://www.datafiles.samhsa.gov/sites/default/files/field-uploads-protected/studies/N-MHSS-2019/N-MHSS-2019-datasets/N-MHSS-2019-DS0001/N-MHSS-2019-DS0001-bundles-with-study-info/N-MHSS-2019-DS0001-bndl-data-tsv.zip"
download.file(url = nmhss_url, destfile = "../data/raw/nmhss-puf-2019-csv.csv")
nmhss_2019_raw = read_csv("../data/raw/nmhss-puf-2019-csv.csv")

# write raw data to file
write_csv(nmhss_2019_raw, "../data/raw/nmhss_2019_raw.csv")
