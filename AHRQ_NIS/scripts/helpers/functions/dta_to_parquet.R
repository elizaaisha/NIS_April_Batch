# Load libraries
library(tidyverse)
library(parquetize)
library(arrow)

# Import NIS dataset
table_to_parquet(path_to_file = "./nis_data/NIS_2020_mega.dta", path_to_parquet = "./nis_data/NIS_2020.parquet", max_memory = 4096)

table_to_parquet(path_to_file = "./nis_data/NIS_2019_mega.dta", path_to_parquet = "./nis_data/NIS_2019.parquet", max_memory = 4096)

table_to_parquet(path_to_file = "./nis_data/NIS_2018_mega.dta", path_to_parquet = "./nis_data/NIS_2018.parquet", max_memory = 4096)
