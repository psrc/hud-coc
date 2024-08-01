library(openxlsx)
library(tidyverse)

filename <- 'System-Performance-Measures-Data.xlsx'
sheet_names <- getSheetNames(filename)

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

# QC & create name lookup ----

# Test with pivot wider
all_df_wide <- all_df |> 
  pivot_wider(id_cols = new_colnames[1:3], names_from = "year", values_from = "total_hmis_count")

wa <- all_df_wide |> 
  filter(state == "WA")

## find duplicates
dup <- all_df_wide |> 
  group_by(hud_num) |> 
  filter(n() > 1) |> 
  arrange(hud_num)

non_dup <- all_df_wide |> 
  group_by(hud_num) |> 
  filter(n() == 1) |> 
  arrange(hud_num) 

var <- sheet_names[length(sheet_names)]

current_name <- dup |> 
  filter(!is.na(.data[[var]])) |> 
  select(state, coc, hud_num)

coc_lookup <- bind_rows(current_name, non_dup |> select(state, coc, hud_num)) |> 
  arrange(hud_num) |> 
  rename(latest_coc = coc)



# reformat ----

new_df <- all_df |> 
  left_join(coc_lookup, by = c('state', 'hud_num'))

new_df <- new_df |> 
  select(-coc) |> 
  rename(coc = latest_coc)

all_df_wide2 <- new_df |> 
  pivot_wider(id_cols = new_colnames[1:3], names_from = "year", values_from = "total_hmis_count")

wa2 <- all_df_wide2 |> 
  filter(state == "WA")

# find missing coc names (missing b/c latest data is 2020 or earlier)
missing_names <- all_df_wide2 |> 
  filter(is.na(coc))

missing_names2 <- all_df_wide |> 
  filter(hud_num %in% missing_names$hud_num)

missing_names3 <- all_df |> 
  filter(hud_num %in% missing_names$hud_num) |> 
  filter(!is.na(total_hmis_count))

test_missing_names <- missing_names3 |> 
  select(hud_num, year) |> 
  group_by(hud_num) |> 
  mutate(year = list(year)) |> 
  distinct()

test_missing_names2 <- test_missing_names |> 
  mutate(latest_year = map(year, last))

names_df <- test_missing_names2 |> 
  mutate(latest_year = as.character(latest_year)) |> 
  select(hud_num, latest_year)

missing_names_final <- all_df |> 
  semi_join(names_df, by = c("hud_num", "year" = "latest_year")) |> 
  select(state, latest_coc = coc, hud_num)

coc_lookup <- bind_rows(coc_lookup, missing_names_final) |> 
  arrange(hud_num)
write.xlsx(coc_lookup, "coc_lookup.xlsx")

# QC part 2 ----

new_df2 <- all_df |> 
  left_join(coc_lookup, by = c('state', 'hud_num'))

new_df2 <- new_df2 |> 
  select(-coc) |> 
  rename(coc = latest_coc)

all_df_wide3 <- new_df2 |> 
  pivot_wider(id_cols = new_colnames[1:3], names_from = "year", values_from = "total_hmis_count")

missing_names_qc <- all_df_wide3 |> 
  filter(is.na(coc))
