library(openxlsx)
library(tidyverse)

source('read-sheets.R')

lookup <- read.xlsx('coc_lookup.xlsx')

# long form
all_df <- all_df |> 
  left_join(lookup, by = c('state', 'hud_num')) |> 
  select(-coc) |> 
  rename(coc = latest_coc) |> 
  arrange(hud_num)

# all_df_wide <- all_df |> 
#   pivot_wider(id_cols = new_colnames[1:3], names_from = "year", values_from = "total_hmis_count")
