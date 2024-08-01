library(openxlsx)
library(tidyverse)

filename <- 'System-Performance-Measures-Data.xlsx'
sheet_names <- getSheetNames(filename)

lookup <- read.xlsx('coc_lookup.xlsx')

# index of Total HMIS Count
ind <- list(a = 20, b = 55, c = 55)
new_colnames <- c('state', 'coc', 'hud_num', 'total_hmis_count', 'year')

# extract and bind into master table
all_df <- NULL
all_notes <- NULL

for(sheet in sheet_names) {
  if(sheet %in% as.character(2015:2017)) {
    x <- ind$a
  } else if (sheet %in% "2018") {
    x <- ind$b
  } else {
    x <- ind$c 
  }
  
  pos <- c(1:3, x)
  raw <- read.xlsx(filename, sheet = sheet, startRow = 2)
  
  df <- raw |> 
    select(all_of(pos)) |> 
    mutate(year = sheet) 
  
  names(df) <- new_colnames
  
  notes <- df |> 
    filter(is.na(hud_num))
  
  df <- df |> 
    filter(!is.na(hud_num))
  
  ifelse(is.null(all_df), all_df <- df, all_df <- bind_rows(all_df, df))
  ifelse(is.null(all_notes), all_notes <- notes, all_notes <- bind_rows(all_notes, notes))
}

# long form
all_df <- all_df |> 
  left_join(lookup, by = c('state', 'hud_num')) |> 
  select(-coc) |> 
  rename(coc = latest_coc) |> 
  arrange(hud_num)

# all_df_wide <- all_df |> 
#   pivot_wider(id_cols = new_colnames[1:3], names_from = "year", values_from = "total_hmis_count")
