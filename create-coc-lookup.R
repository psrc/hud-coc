library(openxlsx)
library(tidyverse)

source('read-sheets.R')

# QC & create name lookup ----

# Test with pivot wider
all_df_wide <- all_df |> 
  pivot_wider(id_cols = new_colnames[1:3], names_from = "year", values_from = "total_hmis_count")

# wa <- all_df_wide |> 
#   filter(state == "WA")

## Find multi-record entries
dup <- all_df_wide |> 
  group_by(hud_num) |> 
  filter(n() > 1) |> 
  arrange(hud_num)

non_dup <- all_df_wide |> 
  group_by(hud_num) |> 
  filter(n() == 1) |> 
  arrange(hud_num) 

## identify coc name to use amongst multiple entries

# standardized name is based on latest year
name_standard <- dup |> 
  pivot_longer(cols = all_of(sheet_names),
               names_to = 'year') |>
  filter(!is.na(value)) |> 
  select(hud_num, year) |> 
  group_by(hud_num) |> 
  mutate(year = list(year)) |> 
  distinct() |> 
  mutate(latest_year = map(year, last)) |> 
  mutate(latest_year = as.character(latest_year))

names_final_b <- all_df |> 
  semi_join(name_standard, by = c("hud_num", "year" = "latest_year")) |> 
  select(state, latest_coc = coc, hud_num) |> 
  arrange(hud_num)

names_final_a <- non_dup |> 
  select(state, latest_coc = coc, hud_num) |> 
  ungroup()

coc_lookup <- bind_rows(names_final_a, names_final_b)
write.xlsx(coc_lookup, "coc_lookup.xlsx")


qc_lookup <- function(table, lookup) {
  df <- table |> 
    left_join(lookup, by = c('state', 'hud_num')) |> 
    select(-coc) |> 
    rename(coc = latest_coc)
  
  df_wide <- df |> 
    pivot_wider(id_cols = colnames(table)[1:3], names_from = "year", values_from = "total_hmis_count") |> 
    arrange(hud_num)
  
  missing_names <- df_wide |> 
    filter(is.na(coc))
  
  print(missing_names)
  return(df_wide)
}

# mydf <- qc_lookup(all_df, coc_lookup)

