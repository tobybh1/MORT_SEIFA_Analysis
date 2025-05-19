library(tidyverse)
library(readxl)
library(janitor)

# https://www.aihw.gov.au/getmedia/a96b853b-e8c5-4cc0-baec-6da16b161099/AIHW-PHE-229-MORT_LGA_2017-2021.xlsx
mortality_raw <- read_xlsx("raw_data/AIHW-MORT_LGA_2017-2021.xlsx", 
                           sheet = "Table 2")
# https://www.abs.gov.au/statistics/people/people-and-communities/socio-economic-indexes-areas-seifa-australia/2021/Local%20Government%20Area%2C%20Indexes%2C%20SEIFA%202021.xlsx
seifa_raw <- read_xlsx("raw_data/Local Government Area, Indexes, SEIFA 2021.xlsx", 
                       sheet = "Table 1")

# Data cleaning applied to the raw mortality data
# Note: this dataframe is never used for any visualisations, however several other
# dataframes are built off of it.
mortality_df <- mortality_raw |>
  slice(-1:-5) |>
  select(22:29) |>
  row_to_names(row_number = 1) |>
  rename("LGA" = "Name", "Crude_rate" = "Crude rate (per 100,000)") |>
  mutate(across(
    .cols = -all_of(c("LGA", "Cause of death")),
    .fns = ~ as.numeric(as.character(.))
  ))

# Modifying the column headers and disease titles
names(mortality_df) <- gsub(" ", "_", names(mortality_df))
mortality_df <- mortality_df |>
  mutate(Cause_of_death = recode(Cause_of_death,
                                 "Coronary heart disease (I20–I25)" = "Coronary Heart Disease",
                                 "Dementia including Alzheimer disease (F01, F03, G30)" = "Dementia / Alzheimer's",
                                 "Cerebrovascular disease (I60–I69)" = "Stroke / Cerebrovascular Disease",
                                 "Lung cancer (C33, C34)" = "Lung Cancer",
                                 "Chronic obstructive pulmonary disease (COPD) (J40–J44)" = "Chronic Obstructive Pulmonary Disease",
                                 "Colorectal cancer (C18–C20, C26.0)" = "Colorectal Cancer",
                                 "Diabetes (E10–E14)" = "Diabetes",
                                 "Prostate cancer (C61)" = "Prostate Cancer",
                                 "Heart failure and complications and ill-defined heart disease (I50–I51)" = "Heart Failure / Ill-defined Heart Disease",
                                 "Accidental falls (W00–W19)" = "Accidental Falls",
                                 "Suicide (X60–X84, Y87.0)" = "Suicide",
                                 "Pancreatic cancer (C25)" = "Pancreatic Cancer",
                                 "Breast cancer (C50)" = "Breast Cancer",
                                 "Influenza and pneumonia (J09–J18)" = "Influenza / Pneumonia",
                                 "Cancer of unknown or ill-defined primary site (C26, C39, C76–C80 excl. C26.0)" = "Unknown / Ill-defined Cancer",
                                 "Cardiac arrhythmias (I47–I49)" = "Cardiac Arrhythmias",
                                 "Kidney failure (N17–N19)" = "Kidney Failure",
                                 "Other ill-defined causes (R00–R94, R96–R99, I46.9, I95.9, I99, J96.0, J96.9, P28.5)" = "Other Ill-defined Causes",
                                 "Hypertensive disease (I10–I15)" = "Hypertensive Disease",
                                 "Liver cancer (C22)" = "Liver Cancer"))
write.csv(mortality_df, "processed_data/mortality_df.csv", row.names = F)

# Data cleaning applied to the raw seifa data
seifa_df <- seifa_raw |>
  slice(-c(1:5, (n() - 2):n())) |>
  rename("LGA" = "...2","IRSD" = "...3", "IRSAD" = "...5", "IER" = "...7",
         "IEO" = "...9") |>
  select(LGA, IRSD, IRSAD, IER, IEO) |>
  mutate(across(c(IRSD, IRSAD, IER, IEO), as.numeric))
write.csv(seifa_df, "processed_data/seifa_df.csv", row.names = F)

# Finding the leading cause of death for each LGA
leading_cause_df <- mortality_df |>
  distinct(LGA) |>
  left_join(
    mortality_df |> 
      filter(Rank == 1) |> 
      select(LGA, Leading_cause = Cause_of_death),
    by = "LGA"
  )
write.csv(leading_cause_df, "processed_data/leading_cause_df.csv", row.names = F)

# Filtering out unnecessary data from mortality_df
mortality_subs_df <- mortality_df |>
  select(LGA, Cause_of_death, Crude_rate) |>
  filter(Cause_of_death != "All causes", Cause_of_death != "Top 20 leading causes")

# Merging the data with the seifa dataframe
comb_allc_df <- mortality_subs_df |>
  inner_join(seifa_df, by = "LGA")

# Finding the top causes of death for the whole of Australia
top_causes_df <- mortality_df |>
  select(LGA, Cause_of_death, Deaths) |>
  filter(LGA == "Australia (total)")
write.csv(top_causes_df, "processed_data/top_causes_df.csv", row.names = F)

# Filtering comb_allc_df and ensuring that all LGAs are accounted for in the
# dataframe (meaning that if there exists no record of diabetes deaths for an 
# LGA its crude rate is NA)
diab_seifa_df <- comb_allc_df[comb_allc_df$Cause_of_death == "Diabetes",-2]
diab_seifa_df <- diab_seifa_df |>
  right_join(seifa_df, by = c("LGA", "IRSD", "IRSAD", "IER", "IEO"))
