library(Seurat)
library(dplyr)

#LorenzChua Dataset
df <- readRDS("~/PhD/Project/Data/Sara - Covid/Nasal/LorenzChua/covid_nbt_main.rds")

#Cleaning
df[["integrated"]] <- NULL #removal of integrated assay

#Droppable Columns
# 'dps', 'virus_pos', 'location', 'series', 'infection'
df@meta.data <- df@meta.data %>%
  select(-dps, -virus_pos, -location, -series, -infection)

#Renaming Columns
df@meta.data <- df@meta.data %>%
  rename(
    Age = age,
    Sex = sex,
    CellType = celltype,
    Status = severity
  )

#Renaming Entries - severity
df@meta.data <- df@meta.data %>%
  mutate(Status = case_when(
    Status == "moderate" ~ "Moderate Covid",
    Status == "critical" ~ "Severe Covid",
    Status == 'control' ~ "Healthy Control"
  ))

#Renaming Entries
df@meta.data <- df@meta.data %>%
  mutate(Sex = case_when(
    Sex == 'm' ~ 'Male',
    Sex == 'f' ~ 'Female'
  ))

df@meta.data$Covid_Status <- 'Positive'

#save processed object
saveRDS(df, "~/PhD/Project/Data/Sara - Covid/Nasal/LorenzChua/raw_covid_nbt_main.rds")

#Loske Dataset
df <- readRDS("~/PhD/Project/Data/Sara - Covid/Nasal/Loske/210716_figshare_upload_ag_lehmann.rds")

#Droppable Columns
df@meta.data <- df@meta.data %>%
  select(-covid_kid1, -covid)

#Renaming Columns
df@meta.data <- df@meta.data %>%
  rename(
    Age = paper_age,
    Sex = gender,
    CellType = detailed_clusters,
    Status = WHO_class
  )

#Renaming Entries - severity
df@meta.data <- df@meta.data %>%
  mutate(Status = case_when(
    Status == "moderate" ~ "Moderate Covid",
    Status == "mild" ~ "Mild Covid",
    Status == 'neg' ~ "Healthy Control",
    Status == 'asymptomatic' ~ "Asymptomatic"
  ))

#Renaming Entries
df@meta.data <- df@meta.data %>%
  mutate(Sex = case_when(
    Sex == 'm' ~ 'Male',
    Sex == 'f' ~ 'Female'
  ))

df@meta.data <- df@meta.data %>%
  mutate(Covid_Status = ifelse(grepl("Covid", Status, ignore.case = TRUE), "Positive", "Negative"))

df <- saveRDS(df, "~/PhD/Project/Data/Sara - Covid/Nasal/Loske/210716_figshare_upload_ag_lehmann.rds")

#Ziegler Dataset
metadata_df <- read.delim("~/PhD/Project/Data/Sara - Covid/Nasal/Ziegler/20210701_NasalSwab_MetaData.txt")

#remove unnecessary row 
metadata_df <- metadata_df[-1, ]

#Cleaning
#Droppable Columns
metadata_df <- metadata_df %>%
  select(-Bloody_Swab, -SARSCoV2_PCR_Status, -library_preparation_protocol, -library_preparation_protocol__ontology_label, -organ,
         -organ__ontology_label, -species__ontology_label, -species, -biosample_id, -SingleCell_SARSCoV2_RNA_Status,
         -SARSCoV2_Unspliced_TRS_Total_Corrected,
         -SARSCoV2_Spliced_TRS_Total_Corrected,
         -SARSCoV2_NegativeStrand_Total_Corrected,
         -SARSCoV2_PositiveStrand_Total_Corrected,
         -SARSCoV2_Total_Corrected, -Peak_Respiratory_Support_WHO_Score)

#Renaming Columns
metadata_df <- metadata_df %>%
  rename(
    Age = age,
    Sex = sex,
    CellType = Detailed_Cell_Annotations,
    Status = Cohort_Disease_WHO_Score
  )

#Renaming Entries - severity
metadata_df<- metadata_df %>%
  mutate(Status = case_when(
    Status == "COVID19_WHO_6-8" ~ "Moderate Covid",
    Status == "COVID19_WHO_1-5" ~ "Mild Covid",
    Status == 'Control_WHO_0' ~ "Healthy Control",
    Status == 'asymptomatic' ~ "Asymptomatic",
    Status == 'ConvalescentCOVID19_WHO_0' ~ "Recovering"
  ))

#Renaming Entries
metadata_df <- metadata_df %>%
  mutate(Sex = case_when(
    Sex == 'male' ~ 'Male',
    Sex == 'female' ~ 'Female'
  ))

metadata_df <- metadata_df %>%
  mutate(Age = case_when(
    Age == "50-59" ~ '55',
    Age == "30-39" ~ '35',
    Age == "60-69" ~ '65',
    Age == "70-79" ~ '75',
    Age == "40-49" ~ '45',
    Age == "19-29" ~ '24',
    Age == "80-89" ~ '85'
  ))

file_path <- "~/PhD/Project/Data/Sara - Covid/Nasal/Ziegler/metadata_df.csv"
write.csv(metadata_df, file_path, row.names = FALSE)

#Trump
df <- readRDS("~/PhD/Project/Data/Sara - Covid/Nasal/Trump/Cardio_final.rds")
View(df@meta.data)

#Remove integrated assay
df[['integrated']] <- NULL

#Renaming Columns
df@meta.data <- df@meta.data %>%
  rename(
    Age = age,
    Sex = sex,
    CellType = celltype,
    Status = severity,
    Covid = covid
  )

#Renaming Entries
df@meta.data <- df@meta.data %>%
  mutate(Sex = case_when(
    Sex == 'm' ~ 'Male',
    Sex == 'f' ~ 'Female'
  ))

#Renaming Entries - severity
df@meta.data <- df@meta.data %>%
  mutate(Status = case_when(
    Status == "critical" ~ "Critical Covid",
    Status == "severe" ~ "Severe Covid",
    Status == 'control_healthy' ~ "Healthy Control",
  ))

# Rename the column from 'Covid' to 'Covid_Status'
colnames(df@meta.data)[colnames(df@meta.data) == "Covid"] <- "Covid_Status"

# Now, update the entries in 'Covid_Status'
df@meta.data <- df@meta.data %>%
  mutate(Covid_Status = case_when(
    Covid_Status == "pos" ~ "Positive",
    Covid_Status == "neg" ~ "Negative",
    TRUE ~ Covid_Status  # Keeps the original value if it's neither 'pos' nor 'neg'
  ))

#save modified df 
saveRDS(df, "~/PhD/Project/Data/Sara - Covid/Nasal/Trump/Cardio_final.rds")
