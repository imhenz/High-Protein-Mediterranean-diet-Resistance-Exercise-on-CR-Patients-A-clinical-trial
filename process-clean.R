library(dplyr)
library(stringr)
library(tidyr)

## ========== Load and Clean Biomarkers Data ==========

# Load the biomarkers dataset
cholesterol <- readxl::read_xlsx("Nightingale - PRiMER data.xlsx", 
                                 sheet = "Results", skip = 10)

# Drop redundant columns
cholesterol$...1 <- NULL
cholesterol$...2 <- NULL

# The "Results" sheet started from the "Quality control tags and notes" sheets
# The participants sample id all starts with PR
biomarkers <- cholesterol |> filter(startsWith(`Sample id`, "PR"))

# remove the cholesterol variable
rm(cholesterol)

# Separate the sample Id and Period (i.e. PRC001a = PRC001 + a)
biomarkers$Period <- str_extract(biomarkers$`Sample id`, "[ab]")
biomarkers$`Sample id` <- str_remove(biomarkers$`Sample id`, "[ab]")

# Clean the column names
names(biomarkers) <- names(biomarkers) |>
  str_replace("%", "pct") |>
  str_trim() |>
  make.names()


# Rearrange the columns
biomarkers <- biomarkers |> 
  select(Sample.id, Period, everything())


# view the data types
# glimpse(biomarkers)

# convert the columns to the appropriate types
biomarkers <- biomarkers |> 
  mutate(across(Total.C:S.HDL.TG.pct, as.double))

# Find count (sum) /percentage (mean) of Nas in columns 
aggregate_NAs <- function(df, agg="sum", ...) {
  sapply(df, function(column) {
    has_nas <- is.na(column)
    do.call(agg, list(x=has_nas, na.rm=TRUE))
  })
}

# filter columns with NAs
has_Na <- function(df, agg="sum", n = 0) {
  v <- aggregate_NAs(df, agg)
  names(v[v > n])
}

# view the Nas
# biomarkers |> select(has_Na(biomarkers)) |> View()


## ============ Load and Clean Lab Visits Data ==============

# Load the lab visits dataset
labv <- readxl::read_xlsx("Lab Visits and Data.xlsx", 
                                sheet = "Data", skip = 1, n_max = 28)

# participants to exclude
# Ivn <- paste0("PRI", str_pad(c(1, 2, 7, 4, 9, 10, 12, 15), 3, pad=0))
# remove PRI016 entirely (improper intervention)
Ivn <- paste0("PRI", str_pad(c(4, 9, 10, 12, 15, 16), 3, pad=0))
Ctrl <- paste0("PRC", str_pad(c(6, 10), 3, pad=0))

# remove invalid records
labv <- labv |>
  filter(!`Participant ID` %in% c(Ivn, Ctrl))


# find columns with >= 50% empty cells 
remove_cols <- has_Na(labv, "mean", n = .5)

# remove them
labv_clean <- labv |> 
  select(-all_of(remove_cols)) |>
  # Also remove DXA Left|Right Arm|Leg Lean Mass
  select(-contains("DXA Left"), -contains("DXA Right"))

# names(labv_clean)

# create a copy of the demographics (columns 4:13)
sociodemographics <- labv_clean |> 
  select(1, 4:13) |> 
  rename(Sample.id = `Participant ID`)

# remove demographic characteristics (columns 4:13)
labv_clean <- labv_clean |> select(1:2, 14:59)

# view the data types
# glimpse(labv_clean)

# convert to the appropriate types
labv_clean <- labv_clean |>
  mutate(across(everything(), function(col){
    ifelse(col == "NA", NA, col)
  })) |> 
  mutate(across(`% Body Fat T0`:`HbA1c T1`, as.double))

# participants with missing data
participants <- data.frame(
  id=labv_clean$`Participant ID`, 
  na=apply(labv_clean, 1, \(x) sum(is.na(x)))
) |>
  filter(na > 0)

# participants

# labv_clean |>
#   filter(`Participant ID` %in% participants$id) |>
#   select(`Participant ID`, has_Na(labv_clean)) |> 
#   View()


# Converting to a long format
# before
lab_baseline <- labv_clean |> 
  select(`Participant ID`, `Intervention/ Control`, ends_with("T0")) |> 
  mutate(Period = "a")

# clean names
names(lab_baseline) <- names(lab_baseline) |> 
  str_remove_all("T0") |> 
  str_trim()

# after
lab_after <- labv_clean |> 
  select(`Participant ID`, `Intervention/ Control`, ends_with("T1")) |> 
  mutate(Period = "b")

# clean names
names(lab_after) <- names(lab_after) |> 
  str_remove_all("T1") |> 
  str_trim()

# Participant PRI003 has a DXA Total Lean Mass outlier
# set it to NA
lab_after[lab_after$`Participant ID`== "PRI003", "DXA Total Lean Mass"] <- NA

# PRI003 ALM 6kg change is an outlier
# set it NA
lab_after[lab_after$`Participant ID`== "PRI003", "DXA Total Appendicular Lean Mass"] <- NA


# dimensions are the same
dim(lab_baseline)
dim(lab_after)

# columns are in the same order
sum(names(lab_baseline) == names(lab_after))

# participants are in the same order
sum(lab_baseline$`Participant ID` == lab_after$`Participant ID`)

# If either baseline or endline is NA for a particular participant
# Make both baseline and endline NA
for (participant in lab_baseline$`Participant ID`) {
  for (variable in names(lab_baseline)) {
    
    if(variable %in% c("Participant ID", "Period", "Intervention/ Control")) {
      next
    }
    
    is_participant <- lab_baseline$`Participant ID` == participant
    
    T0 <- lab_baseline[is_participant, variable, drop = TRUE]
    T1 <- lab_after[is_participant, variable, drop = TRUE]
    
    # check if either T0 or T1 is NA
    if (is.na(T0) || is.na(T1)) {
      
      lab_baseline[is_participant, variable] <- NA
      lab_after[is_participant, variable] <- NA
    }
  }
  
}


labv_clean <- lab_baseline |> 
  # mutate(HbA1c = as.numeric(HbA1c)) |> 
  bind_rows(lab_after) |> 
  select(`Participant ID`, Period, everything()) |>
  # rename to sample.id for join sake
  rename(Sample.id = `Participant ID`) |> 
  arrange(Sample.id, Period)


# labv_clean |>
#   filter(Sample.id %in% participants$id) |>
#   arrange(Sample.id, Period) |> 
#   select(Sample.id, Period, has_Na(labv_clean_long)) |>
#   View()


## ================== Combine Both Datasets ==============
# all dataset
df <- labv_clean |> left_join(biomarkers)

# check if these are present
samples <- c("PRI018", "PRI011", "PRC003", "PRC011")

# samples %in% df$Sample.id

# rename
df <- df |> 
  rename(Group = `Intervention/ Control`) |> 
  mutate(
    Period = ifelse(Period == "a", "T0", "T1") |> as.factor(),
    
    Group = ifelse(Group == "C", "Control", "Intervention") |> as.factor()
  ) |>
  select(Sample.id, Period, Group, everything()) |>
  arrange(Sample.id)


# The colored (invalid participants) in the lab datasets
# invalids <- c(Ivn, Ctrl)

# check which invalid samples are in the overall dataset
# intersect(invalids, df$Sample.id)

# clean column names
names(df) <- names(df) |>
  # change % to pct. Add extra space for cases where there's no
  # space between variable name and %
  str_replace("%", " pct ") |> 
  str_trim() |> 
  # replace double spacing with single space
  str_replace("\\s\\s", " ") |> 
  # remove brackets
  str_remove_all("\\(|\\)") |> 
  # format the variable names to suit R
  make.names()


# replace missing values with median
# has_Nas <- sapply(df, \(x) sum(is.na(x)))
# has_Nas <- names(has_Nas[has_Nas > 0])
# 
# df <- df |> 
#   group_by(Period, Int.Ctrl) |> 
#   mutate(across(all_of(has_Nas), function(col) {
#     med <- median(col, na.rm = TRUE)
#     col <- ifelse(is.na(col), med, col)
#     col
#   })) |> 
#   ungroup()


# df |> readr::write_csv("all_data-no-demographics.csv")
# sociodemographics |> readr::write_csv("demographics.csv")

ignore <- c("DXA.Left.Arm.Lean.Mass",
            "DXA.Right.Arm.Lean.Mass",
            "DXA.Left.Leg.Lean.Mass",
            "DXA.Right.Leg.Lean.Mass")

# confirm these columns are not in the dataset
!ignore %in% names(df)

# rearrange the columns
df <- df |> 
  select(Sample.id:Group, Total.C:S.HDL.TG.pct, Weight:HbA1c)


# ================== Generate T1 - T0 Dataset ==================
# select Sample.id, Group, and Period
identifiers <- df |> select(1:3) |> names()
variables <- setdiff(names(df), identifiers)

difference <- function(df, col) {
  wide_df <- df |>
    select(all_of(c(identifiers, col))) |>
    pivot_wider(names_from = "Period", values_from = all_of(col))
  
  wide_df[[col]] <- round(wide_df$T1 - wide_df$T0, 6)
  
  wide_df |> mutate(T0 = NULL, T1 = NULL)
}

# Compute the differences between variables
df_diff <- lapply(variables, function(col) {
  df |> difference(col)
}) |>
  plyr::join_all(c("Sample.id", "Group"))


# save the dataset
# df_diff |> readr::write_csv("df_diff.csv")


# ================ COMPUTE DF FOR WATERFALL PLOT ===============

# variables of interest
# ALM (DXA), handgrip strength, SarQol and MedDiet Score

# for HDL size, ApoB/ApoA1, SBP, and waist circumference as well - would it be possible to create them, please?

# ratio of total appendicular lean mass change / DXA VAT mass change
df_diff <- df_diff |> 
  mutate(
    ALM.VAT.Mass = DXA.Total.Appendicular.Lean.Mass / DXA.VAT.mass
  )

others <- c("HDL.size", "ApoB.ApoA1", "Blood.Pressure.Sys",
            "Blood.Pressure.Dia", "Waist.Circumference", 
            "ALM.VAT.Mass",
            "Total.C", "Clinical.LDL.C", "Total.TG", "HbA1c")

# variables for waterfall plots
waterfall_variables <- df_diff |> 
  select(
    starts_with("DXA"), 
    Dominant.hand.grip.strength, 
    Med.Diet.Score, SarQol.Score,
    starts_with("BIA")
  ) |> 
  select(-ends_with("pct")) |>
  names() |> 
  c(others)


# Add units 
get_units <- function(waterfall_variables) {
  wv <- tolower(waterfall_variables)
  
  mm <- "(mmol/l)"
  kg <- "(kg)"
  
  case_when(
    wv == "alm.vat.mass" ~ "",
    wv == "total.c" ~ mm,
    wv == "clinical.ldl.c" ~ mm,
    wv == "total.tg" ~ mm,
    endsWith(wv, "mass") ~ kg,
    endsWith(wv, "ffm") ~ kg,
    endsWith(wv, "kg") ~ kg,
    endsWith(wv, "pct") ~ "(%)",
    endsWith(wv, "kg.m2") ~ "(kg/m2)",
    endsWith(wv, "strength") ~ kg,
    startsWith(wv, "blood") ~ "(mmHg)",
    endsWith(wv, "circumference") ~ "(cm)",
    wv == "hdl.size" ~ "(nm)",
    .default = ""
  )
  
}

# add proper labels
get_labels <- function(waterfall_variables) {
  wv <- waterfall_variables |> 
    str_replace_all("\\.", " ") |> 
    str_remove_all("m2|kg") |> 
    str_trim()
  
  case_when(
    str_detect(wv, "mass") ~ str_replace(wv, "mass", "Mass"),
    str_detect(wv, "strength") ~ str_to_title(wv),
    endsWith(tolower(wv), "sys") ~ "Systolic B.P",
    endsWith(tolower(wv), "dia") ~ "Diastolic B.P",
    tolower(wv) == "hdl size" ~ "HDL Size",
    wv == "ApoB ApoA1" ~ "ApoB/ApoA1",
    wv == "ALM VAT Mass" ~ "DXA Total ALM/DXA VAT Mass",
    tolower(wv) == "total c" ~ "Total Cholesterol",
    tolower(wv) == "clinical ldl c" ~ "Clinical LDL Cholesterol",
    tolower(wv) == "total tg" ~ "Total Triglycerides",
    .default = wv
  )
}

# variable units and labels for waterfall plots
variable_units <- sapply(waterfall_variables, get_units)
variable_labels <- sapply(waterfall_variables, get_labels)

# find outliers
find_outlier <- function(x) {
  # find Q1, Q3, and interquartile range for values in points column
  Q1 <- quantile(x, .25, na.rm=TRUE)
  Q3 <- quantile(x, .75, na.rm=TRUE)
  IQR <- IQR(x, na.rm=TRUE)
  
  c(which(x < (Q1 - 1.5 * IQR)), which(x > (Q3 + 1.5 * IQR)))
}

# sapply(df_diff |> select(all_of(waterfall_variables)), function(x) {
#   df_diff$Sample.id[find_outlier(x)]
# })

# df_diff[-find_outlier(df_diff$DXA.Total.Fat.Mass), ]

# keep these variables/objects
keep <- c("df", "sociodemographics", "df_diff",
          "waterfall_variables", "variable_units", 
          "variable_labels", "find_outlier")

# remove irrelevant variables from the global space
rm(list = setdiff(ls(), keep))


