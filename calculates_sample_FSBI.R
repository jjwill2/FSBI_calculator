################################################################################
# Calculates sample FSBI from input data
# Jason Williams, Idaho DEQ Lewiston Office
# last update: March 2023
################################################################################

# uses renv for package dependencies
library(renv)

renv::restore()

# loads required packages from renv cache
library(dplyr)
library(readxl)
library(writexl)

source("user_input_for_FSBI_calculator.R")

################################################################################
# STEP 1 - Read in Data
################################################################################

# read in data------------------------------------------------------------------

macro_data <-read_excel(paste0("./fsbi_calculator_inputs/", filename), 
                               col_names = TRUE, sheet = macro.data.tab)
str(macro_data)


taxa_fsbi <-read_excel(paste0("./fsbi_calculator_inputs/", filename), 
                       col_names = TRUE, sheet = taxa.fsbi.tab)
str(taxa_fsbi)

relyea_fsbi <-read_excel(paste0("./fsbi_calculator_inputs/", filename), 
                       col_names = TRUE, sheet = "relyea_fsbi")
str(relyea_fsbi)

################################################################################
# Step 2 - data checks
################################################################################

# merge data
merged <-
  merge(macro_data, taxa_fsbi, 
        by = c("agency", "project", "taxa"), all.x = TRUE)
  
# flag taxa in macro data, but not in taxa fsbi list (taxa_misssing_flag)
merged2 <-
  merged %>%
  mutate(taxa_missing_flag = ifelse(taxa %in% taxa_fsbi$taxa, NA, "Y"))


# are there duplicate sample/taxa combos with FSBI score? 
duplicates <-
  merged2 %>%
  filter(!is.na(taxa_FSBI)) %>%
  group_by(agency, project, site, sample, taxa) %>%
  filter(n() >1)

################################################################################
# Step 3 - calculate FSBI
################################################################################


calculated <-
  merged2 %>%
  filter(!is.na(taxa_FSBI)) %>% 
  distinct(.keep_all = TRUE) %>%
  group_by(agency, project, site, sample) %>%
  summarize(n_FSBI_taxa = n(),
            sample_FSBI = sum(taxa_FSBI))
  
  
################################################################################
# Step 3 - create output excel file
################################################################################

# creates data frame with user inputs-------------------------------------------

parameter <-c("calculation", "filename", "run date")
user_input <-c(calculation.name, filename, date())

user_inputs <-data.frame(cbind(parameter, user_input))

sheets <-list("user_inputs" = user_inputs, "input_macro_data" = macro_data,
              "input_taxa_fsbi" = taxa_fsbi, "relyea_taxa_fsbi" = relyea_fsbi,
              "merged" = merged2, "calculated_sample_fsbi" = calculated,
              "duplicate sample taxa" = duplicates)

write_xlsx(sheets, paste0("./fsbi_calculator_output/fsbi_output_", calculation.name,
                          ".xlsx"))
