
# Setup -------------------------------------------------------------------

# Clear environment
rm(list = ls())
gc()

# Set working directory
setwd("C:/users/brooksd/OneDrive - World Health Organization/Documents/GitHub/diptheria_vaccination")
path_save <- "C:/Users/brooksd/OneDrive - World Health Organization/Files/03 Diphtheria/01_analysis/visuals/"
export_visuals <- TRUE
export_tables <- TRUE

# Load packages
library("tidyverse")
library("readxl")
library("writexl")
library("lubridate")
library("scales")
library("countrycode")
library("ggtext")
library("whomapper")
library("sf")
library("stringr")


# Extract -----------------------------------------------------------------

# Extract historical, annual WUENIC coverage estimates
raw_wuenic <- 
  data.frame(read.csv("input/data_export_WIISE_AD_COVERAGES.csv"))

# Extract UNPOP population base data
raw_population <-
  data.frame(read.csv("input/data_export_WIISE_REF_POPULATIONS.csv"))

# Extract entity detail data
raw_entity_details <-
  data.frame(read.csv("input/data_export_WIISE_REF_COUNTRIES.csv"))

# Extract 'Big Catch Up' activity data
raw_bcu <- data.frame(
  read_excel("input/base_bcu.xlsx",
             sheet = "Sheet4")
)

# Extract general map data
raw_map <- map_data("world")
raw_map$iso <- countrycode(
  raw_map$region,
  origin = "country.name",
  destination = "iso3c",
  warn = FALSE
)

# Extract WHO map data
raw_sfs <- whomapper::pull_sfs(adm_level = 0, query_server = FALSE)


# Transform - entity details ----------------------------------------------

# Select, rename, and modify relevant columns
tran_entity_details <- raw_entity_details %>%
  select(
    CODE,
    ABREVPUBLEN,
    WHOREGIONC,
    WHO_LEGAL_STATUS_TITLE,
    WBINCOMESTATUS,
    GRP_GAVI57
  ) %>%
  rename(
    iso = CODE,
    name = ABREVPUBLEN,
    region_who = WHOREGIONC,
    status_who = WHO_LEGAL_STATUS_TITLE,
    income_group = WBINCOMESTATUS,
    status_gavi = GRP_GAVI57
  ) %>%
  filter(status_who == "Member State") %>%
  mutate(region_who = substr(region_who, 1, nchar(region_who) - 1),
         income_group = case_when(
           grepl("OECD", income_group) ~ "High income",
           TRUE ~ income_group
         ))

# Add current outbreak, bordering outbreak, WHE focus status
tran_entity_details <- tran_entity_details %>%
  mutate(status_outbreak = case_when(
    iso == "NGA" ~ "True",
    iso == "NER" ~ "True",
    iso == "GIN" ~ "True",
    iso == "VNM" ~ "True",
    iso == "DZA" ~ "True",
    TRUE ~ "False"
  ),
  status_border = case_when(
    iso == "BEN" ~ "True",
    iso == "CMR" ~ "True",
    iso == "TCD" ~ "True",
    iso == "BFA" ~ "True",
    iso == "MLI" ~ "True",
    iso == "DZA" ~ "True",
    iso == "LBY" ~ "True",
    iso == "CIV" ~ "True",
    iso == "SEN" ~ "True",
    iso == "SLE" ~ "True",
    iso == "LBR" ~ "True",
    iso == "GNB" ~ "True",
    iso == "CHN" ~ "True",
    iso == "LAO" ~ "True",
    iso == "KHM" ~ "True",
    iso == "TUN" ~ "True",
    iso == "LBY" ~ "True",
    iso == "MRT" ~ "True",
    TRUE ~ "False"
  ),
  status_whe = case_when(
    iso == "NGA" ~ "True",
    iso == "GIN" ~ "True",
    iso == "NER" ~ "True",
    iso == "TCD" ~ "True",
    iso == "CAF" ~ "True",
    iso == "SOM" ~ "True",
    iso == "AGO" ~ "True",
    iso == "ETH" ~ "True",
    iso == "MLI" ~ "True",
    TRUE ~ "False"
  ))


# Transform - population figures ------------------------------------------

# Prepare 2023 total population figures
tran_pop <- raw_population %>%
  filter(
    YEAR == 2023,
    GENDER_FK == "BOTH",
    AGEGROUP_FK == "ALL",
    POP_SOURCE_FK == "UNPD2022"
  ) %>%
  group_by(COUNTRY_FK) %>%
  summarize(pop = sum(VALUE, na.rm = TRUE))

# Prepare 2023 under-5 population data
tran_pop_u5 <- raw_population %>%
  filter(
    YEAR == 2023,
    GENDER_FK == "BOTH",
    POP_SOURCE_FK == "UNPD2022",
    AGEGROUP_FK %in% c("Y0", "Y01", "Y02", "Y03", "Y04")
  ) %>%
  group_by(COUNTRY_FK) %>%
  summarize(pop_u5 = sum(VALUE, na.rm = TRUE), .groups = 'drop')

# Prepare 2023 under-7 population data
tran_pop_u7 <- raw_population %>%
  filter(
    YEAR == 2023,
    GENDER_FK == "BOTH",
    POP_SOURCE_FK == "UNPD2022",
    AGEGROUP_FK %in% c("Y0", "Y01", "Y02", "Y03", "Y04", "Y05", "Y06")
  ) %>%
  group_by(COUNTRY_FK) %>%
  summarize(pop_u7 = sum(VALUE, na.rm = TRUE), .groups = 'drop')

# Prepare 2023 7-14 age range population data
tran_pop_714 <- raw_population %>%
  filter(
    YEAR == 2023,
    GENDER_FK == "BOTH",
    POP_SOURCE_FK == "UNPD2022",
    AGEGROUP_FK %in% c("Y07", "Y08", "Y09", "Y10", "Y11", "Y12", "Y13", "Y14")
  ) %>%
  group_by(COUNTRY_FK) %>%
  summarize(pop_714 = sum(VALUE, na.rm = TRUE), .groups = 'drop')

# Prepare 2023 under-15 population data
tran_pop_u15 <- raw_population %>%
  filter(
    YEAR == 2023,
    GENDER_FK == "BOTH",
    POP_SOURCE_FK == "UNPD2022",
    AGEGROUP_FK == "Y0" |
      AGEGROUP_FK == "Y01" |
      AGEGROUP_FK == "Y02" |
      AGEGROUP_FK == "Y03" |
      AGEGROUP_FK == "Y04" |
      AGEGROUP_FK == "Y05" |
      AGEGROUP_FK == "Y06" |
      AGEGROUP_FK == "Y07" |
      AGEGROUP_FK == "Y08" |
      AGEGROUP_FK == "Y09" |
      AGEGROUP_FK == "Y10" |
      AGEGROUP_FK == "Y11" |
      AGEGROUP_FK == "Y12" |
      AGEGROUP_FK == "Y13" |
      AGEGROUP_FK == "Y14"
  ) %>%
  group_by(COUNTRY_FK) %>%
  summarize(pop_u15 = sum(VALUE, na.rm = TRUE))

tran_pop_u15_year <- raw_population %>%
  filter(
    YEAR == 2023,
    GENDER_FK == "BOTH",
    POP_SOURCE_FK == "UNPD2022",
    AGEGROUP_FK == "Y0" |
      AGEGROUP_FK == "Y01" |
      AGEGROUP_FK == "Y02" |
      AGEGROUP_FK == "Y03" |
      AGEGROUP_FK == "Y04" |
      AGEGROUP_FK == "Y05" |
      AGEGROUP_FK == "Y06" |
      AGEGROUP_FK == "Y07" |
      AGEGROUP_FK == "Y08" |
      AGEGROUP_FK == "Y09" |
      AGEGROUP_FK == "Y10" |
      AGEGROUP_FK == "Y11" |
      AGEGROUP_FK == "Y12" |
      AGEGROUP_FK == "Y13" |
      AGEGROUP_FK == "Y14"
  ) %>%
  mutate(year = case_when(
    AGEGROUP_FK == "Y0" ~ 2023,
    AGEGROUP_FK == "Y01" ~ 2022,
    AGEGROUP_FK == "Y02" ~ 2021,
    AGEGROUP_FK == "Y03" ~ 2020,
    AGEGROUP_FK == "Y04" ~ 2019,
    AGEGROUP_FK == "Y05" ~ 2018,
    AGEGROUP_FK == "Y06" ~ 2017,
    AGEGROUP_FK == "Y07" ~ 2016,
    AGEGROUP_FK == "Y08" ~ 2015,
    AGEGROUP_FK == "Y09" ~ 2014,
    AGEGROUP_FK == "Y10" ~ 2013,
    AGEGROUP_FK == "Y11" ~ 2012,
    AGEGROUP_FK == "Y12" ~ 2011,
    AGEGROUP_FK == "Y13" ~ 2010,
    AGEGROUP_FK == "Y14" ~ 2009
  )) %>%
  select(
    COUNTRY_FK,
    year,
    VALUE
  )


# Transform - 'Big Catch Up' dose forecast figures ------------------------

raw_bcu$iso <- countrycode(
  raw_bcu$country,
  origin = "country.name",
  destination = "iso3c",
  warn = FALSE
)

raw_bcu <- raw_bcu %>%
  filter(is.na(iso) == FALSE,
         doses_2023 != 0 | doses_2024 != 0)

tran_bcu <- raw_bcu %>%
  mutate(doses = doses_2023 + doses_2024) %>%
  select(iso,
         doses,
         product) %>%
  mutate(product = case_when(
    product == "DTwP-HepB-Hib-IPV" ~ "DTP",
    product == "DTP-HepB/Hib" ~ "DTP",
    TRUE ~ product
    )) %>%
  spread(
    product,
    doses
  )


# Transform - susceptible population estimates -----------------------------

# Overall preparation
tran_wuenic <- raw_wuenic %>%
  select(
    COUNTRY,
    YEAR,
    COVERAGE_CATEGORY,
    REVISION_YEAR,
    VACCINECODE,
    PERCENTAGE,
    TARGETNUMBER
  ) %>%
  rename(
    iso = COUNTRY,
    year = YEAR,
    cat_cov = COVERAGE_CATEGORY,
    year_revis = REVISION_YEAR,
    code_vaccine = VACCINECODE,
    cov = PERCENTAGE,
    target = TARGETNUMBER
  )

tran_wuenic_dtp <- tran_wuenic %>%
  filter(cat_cov == "WUENIC",
         code_vaccine == "DTPCV1" | code_vaccine == "DTPCV3",
         year_revis == 2022,
         year >= 2009) %>%
  left_join(., tran_entity_details, by = "iso")


# Under-15 estimates ------------------------------------------------------

# Create under-15 susceptible population (high risk) estimates
tran_pop_sus_dtp1_u15_23 <- tran_wuenic %>%
  filter(cat_cov == "WUENIC",
         code_vaccine == "DTPCV1",
         year_revis == 2022,
         year == 2022) %>%
  mutate(year := 2023)

tran_pop_sus_dtp1_u15 <- tran_wuenic %>%
  filter(cat_cov == "WUENIC",
         code_vaccine == "DTPCV1",
         year_revis == 2022,
         year >= 2009) %>%
  rbind(., tran_pop_sus_dtp1_u15_23) %>%
  left_join(., tran_pop_u15_year, by = c("iso" = "COUNTRY_FK", "year" = "year")) %>%
  mutate(pop_dtp1_new = cov * VALUE / 100,
         pop_dtp1 = round((cov * target) / 100),
         pop_sus_high = round(target - pop_dtp1))

# Create under-15 susceptible population (at risk) estimates
tran_pop_sus_dtp3_u15_23 <- tran_wuenic %>%
  filter(cat_cov == "WUENIC",
         code_vaccine == "DTPCV3",
         year_revis == 2022,
         year == 2022) %>%
  mutate(year := 2023)

tran_pop_sus_dtp3_u15 <- tran_wuenic %>%
  filter(cat_cov == "WUENIC",
         code_vaccine == "DTPCV3",
         year_revis == 2022,
         year >= 2009) %>%
  rbind(., tran_pop_sus_dtp3_u15_23) %>%
  left_join(., tran_pop_u15_year, by = c("iso" = "COUNTRY_FK", "year" = "year")) %>%
  mutate(pop_dtp3_new = cov * VALUE / 100,
         pop_dtp3 = round((cov * target) / 100),
         pop_sus_risk = round(target - pop_dtp3)) %>%
  select(iso,
         year,
         pop_dtp3,
         pop_sus_risk)


# Under 7 estimates -------------------------------------------------------

# Create under-7 susceptible population (high risk) estimates
tran_pop_sus_dtp1_u7_23 <- tran_wuenic %>%
  filter(cat_cov == "WUENIC",
         code_vaccine == "DTPCV1",
         year_revis == 2022,
         year == 2022) %>%
  mutate(year := 2023)

tran_pop_sus_dtp1_u7 <- tran_wuenic %>%
  filter(cat_cov == "WUENIC",
         code_vaccine == "DTPCV1",
         year_revis == 2022,
         year >= 2017) %>%
  rbind(., tran_pop_sus_dtp1_u7_23) %>%
  left_join(., tran_pop_u15_year, by = c("iso" = "COUNTRY_FK", "year" = "year")) %>%
  mutate(pop_dtp1_u7_new = cov * VALUE / 100,
         pop_dtp1_u7 = round((cov * target) / 100),
         pop_sus_high_u7 = round(target - pop_dtp1_u7)) %>%
  select(iso,
         year,
         pop_dtp1_u7,
         pop_sus_high_u7)

# Create under-7 susceptible population (at risk) estimates
tran_pop_sus_dtp3_u7_23 <- tran_wuenic %>%
  filter(cat_cov == "WUENIC",
         code_vaccine == "DTPCV3",
         year_revis == 2022,
         year == 2022) %>%
  mutate(year := 2023)

tran_pop_sus_dtp3_u7 <- tran_wuenic %>%
  filter(cat_cov == "WUENIC",
         code_vaccine == "DTPCV3",
         year_revis == 2022,
         year >= 2017) %>%
  rbind(., tran_pop_sus_dtp3_u7_23) %>%
  left_join(., tran_pop_u15_year, by = c("iso" = "COUNTRY_FK", "year" = "year")) %>%
  mutate(pop_dtp3_u7_new = cov * VALUE / 100,
         pop_dtp3_u7 = round((cov * target) / 100),
         pop_sus_risk_u7 = round(target - pop_dtp3_u7)) %>%
  select(iso,
         year,
         pop_dtp3_u7,
         pop_sus_risk_u7)


# 7-14 estimates ----------------------------------------------------------

# Create 7-14 susceptible population (high risk) estimates
tran_pop_sus_dtp1_714 <- tran_wuenic %>%
  filter(cat_cov == "WUENIC",
         code_vaccine == "DTPCV1",
         year_revis == 2022,
         year <= 2016 & year >= 2009) %>%
  left_join(., tran_pop_u15_year, by = c("iso" = "COUNTRY_FK", "year" = "year")) %>%
  mutate(pop_dtp1_714_new = cov * VALUE / 100,
         pop_dtp1_714 = round((cov * target) / 100),
         pop_sus_high_714 = round(target - pop_dtp1_714)) %>%
  select(iso,
         year,
         pop_dtp1_714,
         pop_sus_high_714)

# Create 7-14 susceptible population (at risk) estimates
tran_pop_sus_dtp3_714 <- tran_wuenic %>%
  filter(cat_cov == "WUENIC",
         code_vaccine == "DTPCV3",
         year_revis == 2022,
         year <= 2016 & year >= 2009) %>%
  left_join(., tran_pop_u15_year, by = c("iso" = "COUNTRY_FK", "year" = "year")) %>%
  mutate(pop_dtp3_714_new = cov * VALUE / 100,
         pop_dtp3_714 = round((cov * target) / 100),
         pop_sus_risk_714 = round(target - pop_dtp3_714)) %>%
  select(iso,
         year,
         pop_dtp3_714,
         pop_sus_risk_714)


# Annual DTP throughput ---------------------------------------------------

# Calculate annual DTP throughput for 2023
tran_pop_vx_23_dtp1 <- tran_pop_sus_dtp1_u15_23 %>%
  left_join(., tran_pop_u15_year, by = c("iso" = "COUNTRY_FK", "year" = "year")) %>%
  mutate(pop_dtp1_new = cov * VALUE / 100,
         pop_dtp1 = round((cov * target) / 100)) %>%
  select(iso,
         pop_dtp1)

tran_pop_vx_23 <- tran_pop_sus_dtp3_u15_23 %>%
  left_join(., tran_pop_u15_year, by = c("iso" = "COUNTRY_FK", "year" = "year")) %>%
  mutate(pop_dtp3_new = cov * VALUE / 100,
         pop_dtp3 = round((cov * target) / 100)) %>%
  select(iso,
         pop_dtp3) %>%
  left_join(., tran_pop_vx_23_dtp1, by = "iso") %>%
  mutate(pop_dtp1_only = pop_dtp1 - pop_dtp3,
         pop_dtp2 = pop_dtp1_only * 0.5,
         pop_dtp1_est = pop_dtp2,
         doses_admin = round((pop_dtp3 * 3) + (pop_dtp2 * 2) + pop_dtp1_est)) %>%
  select(iso,
         doses_admin)


# Load --------------------------------------------------------------------

# Merge high / at risk data tables, summarize, merge with additional details
df_1 <- left_join(tran_pop_sus_dtp1_u15, tran_pop_sus_dtp3_u15, by = c("iso", "year")) %>%
  left_join(., tran_pop_sus_dtp1_u7, by = c("iso", "year")) %>%
  left_join(., tran_pop_sus_dtp3_u7, by = c("iso", "year")) %>%
  left_join(., tran_pop_sus_dtp1_714, by = c("iso", "year")) %>%
  left_join(., tran_pop_sus_dtp3_714, by = c("iso", "year")) %>%
  group_by(iso) %>%
  summarize(pop_dtp1 = sum(pop_dtp1, na.rm = TRUE),
            pop_dtp3 = sum(pop_dtp3, na.rm = TRUE),
            pop_sus_high = sum(pop_sus_high, na.rm = TRUE),
            pop_sus_risk = sum(pop_sus_risk, na.rm = TRUE),
            pop_sus_high_u7 = sum(pop_sus_high_u7, na.rm = TRUE),
            pop_sus_risk_u7 = sum(pop_sus_risk_u7, na.rm = TRUE),
            pop_sus_high_714 = sum(pop_sus_high_714, na.rm = TRUE),
            pop_sus_risk_714 = sum(pop_sus_risk_714, na.rm = TRUE),
            target = sum(target, na.rm = TRUE)) %>%
  left_join(., tran_pop, by = c("iso" = "COUNTRY_FK")) %>%
  left_join(., tran_pop_u15, by = c("iso" = "COUNTRY_FK")) %>%
  left_join(., tran_pop_u7, by = c("iso" = "COUNTRY_FK")) %>%
  left_join(., tran_pop_714, by = c("iso" = "COUNTRY_FK")) %>%
  left_join(., tran_pop_vx_23, by = "iso") %>%
  left_join(., tran_entity_details, by = "iso") %>%
  left_join(., tran_bcu, by = "iso")


# Load - analyze ----------------------------------------------------------

# Filter for Member States, calculate coverage goals, and dose needs
analyze <- df_1 %>%
  filter(status_who == "Member State") %>%
  mutate(DTP = if_else(is.na(DTP) == TRUE, 0, DTP),
         pop_dtp1_only = pop_dtp1 - pop_dtp3,
         pop_dtp2 = pop_dtp1_only * 0.5,
         pop_dtp1_est = pop_dtp2,
         doses_per = ((pop_dtp3 * 3) + (pop_dtp2 * 2) + pop_dtp1_est) / target,
         doses_per_bcu = DTP / pop_u15,
         pop_nontar = pop_u15 - pop_dtp3,
         pop_dtp1_prop = pop_dtp1_est / pop_nontar,
         pop_dtp2_prop = pop_dtp2 / pop_nontar,
         pop_none_prop = pop_sus_high / pop_nontar,
         bcu_zd = DTP * pop_none_prop,
         bcu_dtp1 = (DTP * pop_dtp1_prop) / 2,
         bcu_dtp2 = (DTP * pop_dtp2_prop) / 3,
         cov_bcu = (bcu_zd + bcu_dtp1 + bcu_dtp2) / pop_u15,
         cov_dtp1 = pop_dtp1 / pop_u15,
         cov_dtp3 = pop_dtp3 / pop_u15,
         pop_sus_high_per = pop_sus_high / pop_u15,
         pop_sus_risk_per = pop_sus_risk / pop_u15,
         pop_sus_diff = pop_sus_risk - pop_sus_high,
         pop_sus_diff_per = pop_sus_diff / pop_u15,
         pop_sus_high_u7_prop = pop_sus_high_u7 / pop_sus_high,
         pop_sus_high_714_prop = pop_sus_high_714 / pop_sus_high,
         pop_sus_risk_u7_prop = pop_sus_risk_u7 / pop_sus_risk,
         pop_sus_risk_714_prop = pop_sus_risk_714 / pop_sus_risk,
         goal_cov_80 = round(pop_u15 * 0.2),
         goal_cov_60 = round(pop_u15 * 0.4),
         goal_cov_40 = round(pop_u15 * 0.6),
         number_dtp1_80 = round(pmax(pop_sus_high - goal_cov_80, 0)),
         number_dtp1_60 = round(pmax(pop_sus_high - goal_cov_60, 0)),
         number_dtp1_40 = round(pmax(pop_sus_high - goal_cov_40, 0)),
         number_dtp1_80_u7 = round(number_dtp1_80 * pop_sus_high_u7_prop, 0),
         number_dtp1_60_u7 = round(number_dtp1_60 * pop_sus_high_u7_prop, 0),
         number_dtp1_40_u7 = round(number_dtp1_40 * pop_sus_high_u7_prop, 0),
         number_dtp1_80_714 = round(number_dtp1_80 * pop_sus_high_714_prop, 0),
         number_dtp1_60_714 = round(number_dtp1_60 * pop_sus_high_714_prop, 0),
         number_dtp1_40_714 = round(number_dtp1_40 * pop_sus_high_714_prop, 0),
         number_dtp3_80 = round(pmax(pop_sus_risk - goal_cov_80, 0)),
         number_dtp3_60 = round(pmax(pop_sus_risk - goal_cov_60, 0)),
         number_dtp3_40 = round(pmax(pop_sus_risk - goal_cov_40, 0)),
         number_dtp23_80 = number_dtp3_80 - number_dtp1_80,
         number_dtp23_60 = number_dtp3_60 - number_dtp1_60,
         number_dtp23_40 = number_dtp3_40 - number_dtp1_40,
         number_dtp23_80_u7 = round(number_dtp23_80 * pop_sus_risk_u7_prop, 0),
         number_dtp23_60_u7 = round(number_dtp23_60 * pop_sus_risk_u7_prop, 0),
         number_dtp23_40_u7 = round(number_dtp23_40 * pop_sus_risk_u7_prop, 0),
         number_dtp23_80_714 = round(number_dtp23_80 * pop_sus_risk_714_prop, 0),
         number_dtp23_60_714 = round(number_dtp23_60 * pop_sus_risk_714_prop, 0),
         number_dtp23_40_714 = round(number_dtp23_40 * pop_sus_risk_714_prop, 0),
         doses_80 = round((number_dtp1_80 * 3) + (number_dtp23_80 * 1.5)),
         doses_60 = round((number_dtp1_60 * 3) + (number_dtp23_60 * 1.5)),
         doses_40 = round((number_dtp1_40 * 3) + (number_dtp23_40 * 1.5)),
         doses_80_u7 = round((number_dtp1_80_u7 * 3) + (number_dtp23_80_u7 * 1.5)),
         doses_60_u7 = round((number_dtp1_60_u7 * 3) + (number_dtp23_60_u7 * 1.5)),
         doses_40_u7 = round((number_dtp1_40_u7 * 3) + (number_dtp23_40_u7 * 1.5)),
         doses_80_714 = round((number_dtp1_80_714 * 3) + (number_dtp23_80_714 * 1.5)),
         doses_60_714 = round((number_dtp1_60_714 * 3) + (number_dtp23_60_714 * 1.5)),
         doses_40_714 = round((number_dtp1_40_714 * 3) + (number_dtp23_40_714 * 1.5)),
         doses_admin_incr_80 = doses_80 / doses_admin,
         doses_80_u7_adj = doses_80_u7 - DTP,
         doses_60_u7_adj = doses_60_u7 - DTP,
         doses_40_u7_adj = doses_40_u7 - DTP,
         doses_80_adj = pmax(doses_80_u7_adj + doses_80_714, 0),
         doses_60_adj = pmax(doses_60_u7_adj + doses_60_714, 0),
         doses_40_adj = pmax(doses_40_u7_adj + doses_40_714, 0),
         tab_high = paste0(format(round(pop_sus_high), big.mark=","), " (", scales::percent(pop_sus_high_per, accuracy = 1), ")"),
         tab_diff = paste0(format(round(pop_sus_diff), big.mark=","), " (", scales::percent(pop_sus_diff_per, accuracy = 1), ")"),
         doses_80_per = doses_80_adj / pop_u15,
         doses_final_per = doses_per + doses_per_bcu + doses_80_per,
         doses_needed_80 = (pop_u15 * 0.8) * 3,
         doses_bcu_per = if_else(is.na(DTP / doses_80), 0, 
                                 if_else(is.infinite(DTP / doses_80), 0,
                                         DTP / doses_80)),
         doses_obr_per = if_else(is.na(doses_80_adj / doses_80), 0, doses_80_adj / doses_80),
         test = doses_bcu_per + doses_obr_per,
         number_dtp3_per = number_dtp3_80 / pop_u15,
         test2 = number_dtp3_per + cov_dtp3,
         cov_test = pmax(0.8 - cov_dtp3, 0),
         cov_dtp3_tar = pop_dtp3 / target,
         cov_dtp3_theo = cov_dtp3_tar * pop_u15,
         cov_test_theo = pmax(0.8 - cov_dtp3_theo, 0),
         cov_bcu = number_dtp3_per * doses_bcu_per,
         cov_obr = number_dtp3_per * doses_obr_per,
         diff = target - pop_u15)

test <- analyze %>%
  select(iso,
         diff,
         cov_test,
         cov_dtp3_tar,
         cov_test_theo,
         number_dtp3_per,
         test2,
         DTP,
         doses_80,
         doses_bcu_per,
         doses_obr_per,
         test,
         cov_dtp3,
         cov_bcu,
         cov_obr) %>%
  mutate(cov_tot = cov_dtp3 + cov_bcu + cov_obr)

test2 <- analyze %>%
  select(
    iso,
    pop_u15,
    pop_dtp1,
    pop_sus_high,
    pop_dtp3,
    DTP,
    cov_dtp1,
    cov_dtp3,
    doses_per,
    doses_per_bcu
  )

test <- analyze %>%
  select(iso,
         doses_per,
         doses_per_bcu,
         doses_80_per,
         doses_final_per)

# Add risk levels
analyze <- analyze %>%
  mutate(risk_level1 = case_when(
    pop_sus_high_per >= 0.2 & pop_sus_risk_per >= 0.2 ~ "High",
    pop_sus_high_per <= 0.2 & pop_sus_risk_per >= 0.2 ~ "Moderate",
    pop_sus_high_per <= 0.2 & pop_sus_risk_per <= 0.2 ~ "Lower",
    TRUE ~ NA
  ),
  risk_level2 = case_when(
    status_outbreak == "True" ~ "Active outbreak",
    risk_level1 == "High" & status_border == "True" ~ "Highest",
    risk_level1 == "Moderate" & status_border == "True" ~ "High",
    TRUE ~ risk_level1
  ))

# Create dataframe for table visualization on slides
summary_tables <- analyze %>%
  select(risk_level2,
         name,
         region_who,
         tab_high,
         tab_diff,
         DTP,
         doses_60,
         doses_60_adj,
         doses_80,
         doses_80_adj,
         doses_admin,
         doses_admin_incr_80,
         pop_sus_high,
         pop_sus_diff,
         pop_u15,
         pop_sus_high_per) %>%
  mutate(DTP = format(round(DTP), big.mark=","),
         doses_60 = format(round(doses_60, digits = -3), big.mark=","),
         doses_60_adj = format(round(doses_60_adj, digits = -3), big.mark=","),
         doses_80 = format(round(doses_80, digits = -3), big.mark=","),
         doses_80_adj = format(round(doses_80_adj, digits = -3), big.mark=","),
         doses_admin = format(round(doses_admin), big.mark=","),
         doses_admin_incr_80 = scales::percent(doses_admin_incr_80, accuracy = 1))

if (export_tables) {
write_xlsx(summary_tables, paste0(path_save,"/DIPH_formatted tables.xlsx"))
}

# Summarize by risk level
summary_risk <- analyze %>%
  # group_by(risk_level2) %>%
  summarize(count = n(),
            pop_sus_high = sum(pop_sus_high, na.rm = TRUE),
            pop_sus_risk = sum(pop_sus_risk, na.rm = TRUE),
            pop_u15 = sum(pop_u15, na.rm = TRUE),
            doses_bcu = sum(DTP, na.rm = TRUE),
            dtp1_all_80 = sum(number_dtp1_80, na.rm = TRUE),
            dtp3_all_80 = sum(number_dtp3_80, na.rm = TRUE),
            dtp1_all_60 = sum(number_dtp1_60, na.rm = TRUE),
            dtp3_all_60 = sum(number_dtp3_60, na.rm = TRUE),
            dtp1_all_40 = sum(number_dtp1_40, na.rm = TRUE),
            dtp3_all_40 = sum(number_dtp3_40, na.rm = TRUE),
            doses_80 = round(sum(doses_80, na.rm = TRUE), digits = -3),
            doses_60 = round(sum(doses_60, na.rm = TRUE), digits = -3),
            doses_40 = sum(doses_40, na.rm = TRUE),
            doses_80_u7 = sum(doses_80_u7, na.rm = TRUE),
            doses_60_u7 = sum(doses_60_u7, na.rm = TRUE),
            doses_40_u7 = sum(doses_40_u7, na.rm = TRUE),
            doses_80_714 = sum(doses_80_714, na.rm = TRUE),
            doses_60_714 = sum(doses_60_714, na.rm = TRUE),
            doses_40_714 = sum(doses_40_714, na.rm = TRUE),
            doses_80_adj = round(sum(doses_80_adj, na.rm = TRUE), digits = -3),
            doses_60_adj = round(sum(doses_60_adj, na.rm = TRUE), digits = -3),
            doses_40_adj = sum(doses_40_adj, na.rm = TRUE),
            doses_admin = sum(doses_admin, na.rm = TRUE),
            ) %>%
  mutate(pop_sus_high_per = pop_sus_high / pop_u15,
         pop_sus_risk_per = pop_sus_risk / pop_u15,
         pop_sus_diff = pop_sus_risk - pop_sus_high,
         pop_sus_diff_per = pop_sus_diff / pop_u15,
         doses_admin_incr = doses_80 / doses_admin)

if (export_tables) {
  write_xlsx(summary_risk, paste0(path_save,"/tables_sum.xlsx"))
}

# Load - prepare for visualization ----------------------------------------

vis_per_risk <- analyze %>%
  filter(risk_level2 != "Lower") %>%
  select(
    name,
    pop_sus_high_per,
    pop_sus_diff_per,
    risk_level2
  ) %>%
  gather(
    key = indicator,
    value = value,
    -name, -risk_level2
  )

vis_dose_per <- analyze %>%
  filter(risk_level2 != "Lower",
         status_whe == "True") %>%
  select(
    name,
    doses_per,
    doses_per_bcu,
    doses_80_per,
    risk_level2,
    pop_sus_high_per
  ) %>%
  gather(
    key = indicator,
    value = value,
    -name, -risk_level2, -pop_sus_high_per
  )

vis_cov_per <- analyze %>%
  filter(risk_level2 != "Lower") %>%
  select(
    name,
    cov_dtp3,
    cov_bcu,
    cov_obr,
    risk_level2,
    pop_sus_high_per
  ) %>%
  gather(
    key = indicator,
    value = value,
    -name, -risk_level2, -pop_sus_high_per
  )

# Visualize - map, risk level ---------------------------------------------

sf_risk <- left_join(
  raw_sfs$adm0,
  analyze %>% select(iso, risk_level2),
  by = c("iso_3_code" = "iso")
)

map_risk_level <- sf_risk %>%
  ggplot() +
  geom_sf_who_poly(aes(fill = risk_level2)) +
  scale_fill_manual(name = "Risk level",
                    breaks = c("Active outbreak",
                               "Highest",
                               "High",
                               "Moderate",
                               "Lower"),
                    labels = c("Active outbreak",
                               "Highest",
                               "High",
                               "Moderate",
                               "Lower"),
                    values = c("darkred",
                               "red2",
                               "orange2",
                               "yellow2",
                               "chartreuse3")) +
  labs(title = "Assessed risk of diphtheria outbreak across WHO Member States") +
  theme(legend.position = "bottom") +
  who_map_pipeline(sf = raw_sfs, include_adm0_line = TRUE) 

map_risk_level

if(export_visuals) {
who_map_save(paste0(path_save, "map_risk_level.png"), 
             map_risk_level, 
             width = 10, 
             height = 6, 
             dpi = 700)
}

# Visualize - bar, sus. pop., by risk, by dtp1, % under 15, horizontal ------

bar_popsus_risk_peru15 <- vis_per_risk %>%
  mutate(name_factor = factor(name, levels = unique(name[indicator == 'pop_sus_high_per'][order(-value[indicator == 'pop_sus_high_per'])])),
         risk_level2 = factor(risk_level2, levels = c("Active outbreak",
                                                      "Highest",
                                                      "High",
                                                      "Moderate",
                                                      "Lower"))) %>%
  ggplot(aes(x = name_factor, 
             y = value, fill = indicator)) + 
  geom_col(position = position_stack(), color = "white", width = 1) +
  scale_fill_manual(values = c("#008dc9", "#0f2d5b"),
                    labels = c("At moderate risk of infection", 
                               "At high risk of infection")) + 
  scale_x_discrete("name", breaks = vis_per_risk$name, labels = vis_per_risk$name) +
  facet_grid(.~ risk_level2, scales = "free_x", space = "free_x") +
  scale_y_continuous(labels = percent_format()) +
  coord_cartesian(ylim = c(0, 0.7)) +
  geom_hline(aes(yintercept = 0.2), linetype = "solid", color = "orange", linewidth = 1.5) +
  theme_test() +
  labs(title = "Estimated, aggregate under-15 individuals susceptible to diphtheria infection by risk category",
       subtitle = "WHO Member States assessed as at highest, high, and moderate risk of large diphtheria infection",
       y = "% of under-15 population",
       caption = "<b>Data source:</b> WHO-UNICEF Joint Reporting Form<br>
       <b>Figure production:</b>WHO Immunization Analysis & Insights Unit") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_markdown(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

bar_popsus_risk_peru15

if(export_visuals){
ggsave(
  path = path_save,
  filename = "popsus_risk_peru15.jpg",
  plot = bar_popsus_risk_peru15,
  width = 15,
  height = 6.5,
  dpi = 300
)
}

bar_popsus_dose_peru15 <- vis_dose_per %>%
  mutate(name_factor = factor(name, levels = unique(name[indicator == 'doses_per'][order(-value[indicator == 'doses_per'])])),
         risk_level2 = factor(risk_level2, levels = c("Active outbreak",
                                                      "Highest",
                                                      "High",
                                                      "Moderate",
                                                      "Lower")),
         indicator = factor(indicator, levels = c("doses_80_per",
                                                  "doses_per_bcu",
                                                  "doses_per"))) %>%
  ggplot(aes(x = name_factor, 
             y = value * 100, fill = indicator)) + 
  geom_col(position = position_stack(), color = "white", width = 0.75) +
  scale_fill_manual(values = c("#0099DD", "#0044C4", "#00205C"),
                    labels = c("Additional doses required to reduce outbreak risk", 
                               "Forecasted 'Big Catch Up' doses",
                               "Doses administered historically")) + 
  scale_x_discrete("name", breaks = vis_dose_per$name, labels = vis_dose_per$name) +
  # facet_grid(.~ risk_level2, scales = "free_x", space = "free_x") +
  coord_cartesian(ylim = c(0, 300)) +
  geom_hline(aes(yintercept = 240), linetype = "solid", color = "orange", linewidth = 1.5) +
  theme_test() +
  labs(title = str_wrap("Estimated current coverage, 'Big Catch Up' forecasted coverage, and additional coverage needs to reduce large diphtheria outbreak risk"),
       subtitle = "WHO Member States assessed as at highest risk of large diphtheria outbreak",
       y = "Diph.-containing vaccine doses admin. per 100 persons under-15",
       caption = "<b>Data sources:</b> WHO-UNICEF Joint Reporting Form; UNICEF Supply Division Catch Up Supply Forecasts<br>
       <b>Figure production:</b>WHO Immunization Analysis & Insights Unit") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_markdown(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

bar_popsus_dose_peru15

if(export_visuals){
  ggsave(
    path = path_save,
    filename = "popsus_dose_peru15_whe.jpg",
    plot = bar_popsus_dose_peru15,
    width = 10,
    height = 6.5,
    dpi = 300
  )
}

bar_cov_bcu <- vis_cov_per %>%
  mutate(risk_level2 = factor(risk_level2, levels = c("Active outbreak",
                                                      "Highest",
                                                      "High",
                                                      "Moderate",
                                                      "Lower")),
         indicator = factor(indicator, levels = c("doses_80_per",
                                                  "doses_per_bcu",
                                                  "doses_per"))) %>%
  ggplot(aes(x = reorder(name, -pop_sus_high_per), 
             y = value, fill = indicator)) + 
  geom_col(position = position_stack(), color = "white", width = 1) +
  scale_fill_manual(values = c("#0099DD", "#0044C4", "#00205C"),
                    labels = c("Additional doses required to reduce outbreak risk", 
                               "Forecasted 'Big Catch Up' doses",
                               "Doses administered historically")) + 
  scale_x_discrete("name", breaks = vis_cov_per$name, labels = vis_cov_per$name) +
  facet_grid(.~ risk_level2, scales = "free_x", space = "free_x") +
  coord_cartesian(ylim = c(0, 1)) +
  geom_hline(aes(yintercept = 0.8), linetype = "solid", color = "orange", linewidth = 1.5) +
  theme_test() +
  labs(title = "Estimated, aggregate under-15 individuals susceptible to diphtheria infection by risk category",
       subtitle = "WHO Member States assessed as at highest, high, and moderate risk of large diphtheria outbreak",
       y = "Diph.-containing vaccine doses admin. per 100 persons under-15",
       caption = "<b>Data sources:</b> WHO-UNICEF Joint Reporting Form; UNICEF Supply Division Catch Up Supply Forecasts<br>
       <b>Figure production:</b>WHO Immunization Analysis & Insights Unit") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_markdown(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

bar_cov_bcu

if(export_visual){
  ggsave(
    path = path_save,
    filename = "popsus_risk_peru15.jpg",
    plot = bar_popsus_risk_peru15,
    width = 15,
    height = 6.5,
    dpi = 300
  )
}

# Visualize - small line, historical DTP coverage --------------------------

line_dtp13_afr <- tran_wuenic_dtp %>%
  filter(region_who == "AFR",
         status_who == "Member State") %>%
  ggplot(aes(x = year, y = cov, color = code_vaccine)) +
  geom_point() +
  geom_line(linewidth = 1) +
  facet_wrap(~name, ncol = 10) +
  scale_color_manual(values = c("#008dc9","#0f2d5b"),
                     labels = c("DTP1", "DTP3")) +
  labs(title = "Historical DTP1 & DTP3 coverage, 2009-2022",
       y = "Vaccination coverage (% live births)",
       caption = "<b>Data source:</b> WHO-UNICEF Joint Reporting Form<br>
       <b>Figure production:</b>WHO Immunization Analysis & Insights Unit") +
  theme_test() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_markdown(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",)

line_dtp13_afr

if (export_visuals) {
ggsave(
  path = path_save,
  filename = "time_dtp13_afr.jpg",
  plot = line_dtp13_afr,
  width = 15,
  height = 6.5,
  dpi = 300
)
}

# End ---------------------------------------------------------------------

