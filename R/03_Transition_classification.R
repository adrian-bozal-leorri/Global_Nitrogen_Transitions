# ============================================================
# 03_Transition_classification.R
#
# Divergent nitrogen transition pathways during
# global agricultural development
#
# PURPOSE
# -------
# Reconstruct the first-to-last national transition
# classification using exact annual endpoints:
#
#   initial = 1992
#   final   = 2022
#
# The classification is based on relative changes in:
#
#   1. Total protein supply
#   2. Cropland N surplus
#   3. N surplus per unit of protein supply
#
# IMPORTANT
# ---------
# This script uses the DEFINITIVE mass-harmonized indicator:
#
#   N_surplus_per_protein_mass =
#
#       total territorial cropland N surplus (kg N yr-1)
#       --------------------------------------------------
#       total national protein availability (kg protein yr-1)
#
# Units:
#   kg N kg protein-1
#
# The OLD variable:
#
#   N_surplus_per_protein
#
# is retained only as a legacy/QC variable and is NEVER used
# for the definitive classification.
#
# ------------------------------------------------------------
# MAIN CLASSIFICATION THRESHOLD
# ------------------------------------------------------------
#
# Main threshold = 5%
#
# Absolute decoupling:
#   protein supply > +5%
#   AND
#   cropland N surplus < -5%
#
# Relative decoupling:
#   protein supply > +5%
#   AND
#   N surplus per unit protein supply < -5%
#   BUT
#   not already classified as absolute decoupling
#
# Inefficient intensification:
#   protein supply > +5%
#   AND
#   N surplus per unit protein supply > +5%
#
# Coupled intensification:
#   protein supply > +5%
#   AND
#   cropland N surplus > +5%
#   AND
#   not already classified as inefficient intensification
#
# Absolute N-surplus reduction without protein growth:
#   cropland N surplus < -5%
#   AND
#   protein supply does not increase > +5%
#
# Other / no clear transition:
#   all remaining cases, including protein increases associated
#   with approximately stable absolute N surplus.
#
# ------------------------------------------------------------
# IMPORTANT DISTINCTION
# ------------------------------------------------------------
#
# These are FIRST-TO-LAST TRANSITION CATEGORIES.
#
# They are NOT the same construct as the PCA-based trajectory
# configurations analysed later.
#
# First-to-last categories:
#   summarize net directional change between 1992 and 2022.
#
# PCA-based configurations:
#   summarize similarities in the full multivariate structure
#   of national trajectories.
#
# ------------------------------------------------------------
# THRESHOLD SENSITIVITY
# ------------------------------------------------------------
#
# Main classification:
#   5%
#
# Sensitivity:
#   0%, 1%, 2%, ..., 10%
#
# The sensitivity analysis is NOT used to identify an "optimal"
# threshold. It evaluates whether broad conclusions depend
# strongly on the operational definition of substantive change.
#
# ------------------------------------------------------------
# EXPECTED VALIDATED MAIN RESULTS
# ------------------------------------------------------------
#
# With the corrected mass-harmonized indicator, previous checks
# gave:
#
#   Absolute decoupling        = 46
#   Relative decoupling        = 32
#   Inefficient intensification = 28
#
# These values are checked here.
#
# The remaining category counts are NOT hard-coded and are
# recalculated from the definitive data.
#
# ============================================================


# ============================================================
# 1. SETUP
# ============================================================

rm(list = ls())

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

set.seed(123)


# ============================================================
# 2. PACKAGES
# ============================================================

required_packages <- c(
  "tidyverse",
  "scales",
  "patchwork"
)


missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]


if (length(missing_packages) > 0) {
  
  stop(
    "Install the following packages before running this script: ",
    paste(
      missing_packages,
      collapse = ", "
    )
  )
}


suppressPackageStartupMessages({
  
  library(tidyverse)
  library(scales)
  library(patchwork)
  
})


# ============================================================
# 3. PATHS
# ============================================================

base_dir <- "."


data_dir <- file.path(
  base_dir,
  "data"
)


results_dir <- file.path(
  base_dir,
  "results"
)


plots_dir <- file.path(
  base_dir,
  "plots"
)


input_file <- file.path(
  data_dir,
  "Data_Final_31.csv"
)


out_res_dir <- file.path(
  results_dir,
  "03_Transition_classification"
)


out_plot_dir <- file.path(
  plots_dir,
  "03_Transition_classification"
)


dir.create(
  out_res_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  out_plot_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 4. CONSTANTS
# ============================================================

study_start <- 1992

study_end <- 2022

main_threshold <- 5


# Complete 0-10% sensitivity in 1-percentage-point increments
thresholds <- 0:10


expected_n_countries <- 130

expected_n_years <- study_end - study_start + 1


# ------------------------------------------------------------
# Only the three corrected counts already validated previously
# are used as mandatory reference checks.
# ------------------------------------------------------------

expected_validated_counts <- c(
  
  absolute_decoupling =
    46,
  
  relative_decoupling =
    32,
  
  inefficient_intensification =
    28
)


# ============================================================
# 5. TRANSITION CATEGORIES
# ============================================================

transition_order <- c(
  
  "absolute_decoupling",
  
  "relative_decoupling",
  
  "absolute_reduction_without_protein_growth",
  
  "inefficient_intensification",
  
  "coupled_intensification",
  
  "other_or_no_clear_transition"
)


transition_labels <- c(
  
  "absolute_decoupling" =
    "Absolute decoupling",
  
  "relative_decoupling" =
    "Relative decoupling",
  
  "absolute_reduction_without_protein_growth" =
    "Absolute N-surplus reduction without protein growth",
  
  "inefficient_intensification" =
    "Inefficient intensification",
  
  "coupled_intensification" =
    "Coupled intensification",
  
  "other_or_no_clear_transition" =
    "Other / no clear transition"
)


# ============================================================
# 6. PALETTE FOR SUPPLEMENTARY SENSITIVITY FIGURES
# ============================================================

transition_palette <- c(
  
  "absolute_decoupling" =
    "#0072B2",
  
  "relative_decoupling" =
    "#56B4E9",
  
  "absolute_reduction_without_protein_growth" =
    "#D55E00",
  
  "inefficient_intensification" =
    "#009E73",
  
  "coupled_intensification" =
    "#E69F00",
  
  "other_or_no_clear_transition" =
    "grey55"
)


# ============================================================
# 7. THEME
# ============================================================

theme_nature <- function(
    base_size = 10
) {
  
  theme_classic(
    base_size =
      base_size
  ) +
    
    theme(
      
      axis.title =
        element_text(
          color =
            "black"
        ),
      
      axis.text =
        element_text(
          color =
            "black"
        ),
      
      plot.title =
        element_text(
          face =
            "bold",
          size =
            base_size + 1
        ),
      
      legend.position =
        "bottom",
      
      legend.title =
        element_blank(),
      
      legend.text =
        element_text(
          size =
            base_size - 0.5
        ),
      
      plot.margin =
        margin(
          7,
          9,
          7,
          9
        )
    )
}


# ============================================================
# 8. HELPER: SAFE RELATIVE CHANGE
# ============================================================
#
# Relative change:
#
#   100 * (final - initial) / abs(initial)
#
# abs(initial) is used deliberately because cropland N surplus
# may be negative.
#
# If initial value is effectively zero, relative change is not
# numerically meaningful and is returned as NA.
# ============================================================

safe_rel_change <- function(
    initial,
    final,
    tolerance = 1e-9
) {
  
  if_else(
    
    is.finite(initial) &
      is.finite(final) &
      abs(initial) >
      tolerance,
    
    100 *
      (final - initial) /
      abs(initial),
    
    NA_real_
  )
}


# ============================================================
# 9. HELPER: CLASSIFICATION FUNCTION
# ============================================================
#
# Important:
#
# The order of case_when() makes the categories mutually
# exclusive.
#
# In particular:
#
# - absolute decoupling takes priority over relative decoupling;
# - inefficient intensification takes priority over coupled
#   intensification when both N-surplus intensity and absolute
#   N surplus increase.
#
# This reproduces the conceptual hierarchy of the previous
# classification while using the corrected indicator.
# ============================================================

classify_threshold <- function(
    country_changes,
    threshold
) {
  
  country_changes %>%
    
    mutate(
      
      threshold =
        threshold,
      
      
      # ------------------------------------------------------
      # Directional flags
      # ------------------------------------------------------
      
      protein_increase =
        rel_delta_protein >
        threshold,
      
      
      protein_not_increasing =
        !protein_increase,
      
      
      surplus_increase =
        rel_delta_surplus >
        threshold,
      
      
      surplus_decrease =
        rel_delta_surplus <
        -threshold,
      
      
      surplus_stable =
        !surplus_increase &
        !surplus_decrease,
      
      
      nsp_increase =
        rel_delta_nsp >
        threshold,
      
      
      nsp_decrease =
        rel_delta_nsp <
        -threshold,
      
      
      nsp_stable =
        !nsp_increase &
        !nsp_decrease,
      
      
      # ------------------------------------------------------
      # Non-exclusive diagnostic flags
      # ------------------------------------------------------
      
      absolute_decoupling_flag =
        protein_increase &
        surplus_decrease,
      
      
      relative_decoupling_flag =
        protein_increase &
        nsp_decrease,
      
      
      inefficient_intensification_flag =
        protein_increase &
        nsp_increase,
      
      
      coupled_intensification_flag =
        protein_increase &
        surplus_increase,
      
      
      absolute_reduction_without_protein_growth_flag =
        surplus_decrease &
        !protein_increase,
      
      
      protein_gain_surplus_stable_flag =
        protein_increase &
        surplus_stable,
      
      
      # ------------------------------------------------------
      # MUTUALLY EXCLUSIVE FINAL CLASSIFICATION
      # ------------------------------------------------------
      
      transition_class =
        case_when(
          
          absolute_decoupling_flag ~
            "absolute_decoupling",
          
          
          relative_decoupling_flag ~
            "relative_decoupling",
          
          
          inefficient_intensification_flag ~
            "inefficient_intensification",
          
          
          coupled_intensification_flag ~
            "coupled_intensification",
          
          
          absolute_reduction_without_protein_growth_flag ~
            "absolute_reduction_without_protein_growth",
          
          
          TRUE ~
            "other_or_no_clear_transition"
        ),
      
      
      # ------------------------------------------------------
      # Explicit mutually exclusive final-category flags
      # ------------------------------------------------------
      
      absolute_decoupling =
        transition_class ==
        "absolute_decoupling",
      
      
      relative_decoupling =
        transition_class ==
        "relative_decoupling",
      
      
      inefficient_intensification =
        transition_class ==
        "inefficient_intensification",
      
      
      coupled_intensification =
        transition_class ==
        "coupled_intensification",
      
      
      absolute_reduction_without_protein_growth =
        transition_class ==
        "absolute_reduction_without_protein_growth",
      
      
      other_or_no_clear_transition =
        transition_class ==
        "other_or_no_clear_transition"
    )
}


# ============================================================
# 10. LOAD DATA
# ============================================================

if (!file.exists(input_file)) {
  
  stop(
    "\nInput file not found:\n",
    input_file
  )
}


df_raw <- readr::read_csv(
  input_file,
  show_col_types = FALSE
)


# ============================================================
# 11. CHECK REQUIRED VARIABLES
# ============================================================

required_vars <- c(
  
  "ISO3",
  
  "Country",
  
  "Year",
  
  "Population_total",
  
  "Cropland_area",
  
  "Total_protein_supply",
  
  "N_surplus",
  
  "N_surplus_per_protein"
)


missing_vars <- setdiff(
  required_vars,
  names(df_raw)
)


if (length(missing_vars) > 0) {
  
  stop(
    "Required variables missing from Data_Final_31.csv: ",
    paste(
      missing_vars,
      collapse = ", "
    )
  )
}


# ============================================================
# 12. PREPARE CORE DATA
# ============================================================

df <- df_raw %>%
  
  mutate(
    
    ISO3 =
      as.character(ISO3),
    
    Country =
      as.character(Country),
    
    Year =
      as.integer(Year),
    
    Population_total =
      as.numeric(
        Population_total
      ),
    
    Cropland_area =
      as.numeric(
        Cropland_area
      ),
    
    Total_protein_supply =
      as.numeric(
        Total_protein_supply
      ),
    
    N_surplus =
      as.numeric(
        N_surplus
      ),
    
    N_surplus_per_protein =
      as.numeric(
        N_surplus_per_protein
      )
  ) %>%
  
  rename(
    
    N_surplus_per_protein_legacy =
      N_surplus_per_protein
  ) %>%
  
  mutate(
    
    # --------------------------------------------------------
    # Total territorial cropland N surplus
    # --------------------------------------------------------
    
    Total_N_surplus_kg_yr =
      if_else(
        
        is.finite(N_surplus) &
          is.finite(Cropland_area) &
          Cropland_area > 0,
        
        N_surplus *
          Cropland_area,
        
        NA_real_
      ),
    
    
    # --------------------------------------------------------
    # Total national protein availability
    # --------------------------------------------------------
    
    Total_protein_kg_yr =
      if_else(
        
        is.finite(Total_protein_supply) &
          is.finite(Population_total) &
          Population_total > 0,
        
        Total_protein_supply *
          Population_total *
          365 /
          1000,
        
        NA_real_
      ),
    
    
    # --------------------------------------------------------
    # Definitive N-surplus/protein metric
    # --------------------------------------------------------
    
    N_surplus_per_protein_mass =
      if_else(
        
        is.finite(Total_N_surplus_kg_yr) &
          is.finite(Total_protein_kg_yr) &
          Total_protein_kg_yr > 0,
        
        Total_N_surplus_kg_yr /
          Total_protein_kg_yr,
        
        NA_real_
      )
  ) %>%
  
  arrange(
    ISO3,
    Year
  )


# ============================================================
# 13. STRUCTURAL QC
# ============================================================

duplicate_country_year <- df %>%
  
  count(
    ISO3,
    Year,
    name =
      "n"
  ) %>%
  
  filter(
    n > 1
  )


write_csv(
  duplicate_country_year,
  file.path(
    out_res_dir,
    "00_duplicate_country_years.csv"
  )
)


if (nrow(duplicate_country_year) > 0) {
  
  stop(
    "Duplicated ISO3-Year observations detected."
  )
}


# ============================================================
# 14. BALANCED-PANEL QC
# ============================================================

country_coverage <- df %>%
  
  group_by(
    ISO3,
    Country
  ) %>%
  
  summarise(
    
    n_rows =
      n(),
    
    n_years =
      n_distinct(
        Year
      ),
    
    first_year =
      min(
        Year
      ),
    
    last_year =
      max(
        Year
      ),
    
    has_1992 =
      any(
        Year ==
          study_start
      ),
    
    has_2022 =
      any(
        Year ==
          study_end
      ),
    
    .groups =
      "drop"
  )


write_csv(
  country_coverage,
  file.path(
    out_res_dir,
    "01_country_endpoint_coverage_QC.csv"
  )
)


if (
  n_distinct(
    df$ISO3
  ) !=
  expected_n_countries
) {
  
  stop(
    "Expected ",
    expected_n_countries,
    " countries, but found ",
    n_distinct(
      df$ISO3
    ),
    "."
  )
}


if (
  any(
    country_coverage$n_years !=
    expected_n_years
  )
) {
  
  stop(
    "At least one country does not contain exactly ",
    expected_n_years,
    " study years."
  )
}


if (
  any(
    !country_coverage$has_1992 |
    !country_coverage$has_2022
  )
) {
  
  stop(
    "At least one country is missing the exact 1992 or 2022 endpoint."
  )
}


# ============================================================
# 15. METRIC RECONSTRUCTION QC
# ============================================================

metric_qc_df <- df %>%
  
  mutate(
    
    Cropland_ha_per_capita =
      Cropland_area /
      Population_total,
    
    N_surplus_per_protein_mass_identity =
      N_surplus_per_protein_legacy *
      Cropland_ha_per_capita *
      1000 /
      365
  )


metric_qc <- tibble(
  
  check =
    "Mass-harmonized indicator algebraic identity",
  
  max_absolute_difference =
    max(
      abs(
        metric_qc_df$N_surplus_per_protein_mass -
          metric_qc_df$N_surplus_per_protein_mass_identity
      ),
      na.rm =
        TRUE
    )
)


write_csv(
  metric_qc,
  file.path(
    out_res_dir,
    "02_mass_metric_reconstruction_QC.csv"
  )
)


# ============================================================
# 16. EXTRACT EXACT 1992 AND 2022 ENDPOINTS
# ============================================================
#
# IMPORTANT:
#
# No rolling means are used here.
#
# Figure 2 uses 3-year rolling means for visualization.
# This classification uses exact annual values.
# ============================================================

endpoint_vars <- c(
  
  "Total_protein_supply",
  
  "N_surplus",
  
  "N_surplus_per_protein_mass"
)


endpoints_long <- df %>%
  
  filter(
    Year %in%
      c(
        study_start,
        study_end
      )
  ) %>%
  
  select(
    ISO3,
    Country,
    Year,
    all_of(
      endpoint_vars
    )
  )


endpoint_completeness <- endpoints_long %>%
  
  group_by(
    ISO3,
    Country
  ) %>%
  
  summarise(
    
    n_endpoint_rows =
      n(),
    
    protein_complete =
      all(
        is.finite(
          Total_protein_supply
        )
      ),
    
    surplus_complete =
      all(
        is.finite(
          N_surplus
        )
      ),
    
    nsp_complete =
      all(
        is.finite(
          N_surplus_per_protein_mass
        )
      ),
    
    .groups =
      "drop"
  )


write_csv(
  endpoint_completeness,
  file.path(
    out_res_dir,
    "03_endpoint_variable_completeness_QC.csv"
  )
)


if (
  any(
    endpoint_completeness$n_endpoint_rows !=
    2
  )
) {
  
  stop(
    "At least one country does not have exactly two endpoint rows."
  )
}


if (
  any(
    !endpoint_completeness$protein_complete |
    !endpoint_completeness$surplus_complete |
    !endpoint_completeness$nsp_complete
  )
) {
  
  stop(
    "At least one country has incomplete endpoint variables."
  )
}


# ============================================================
# 17. CREATE COUNTRY-LEVEL ENDPOINT TABLE
# ============================================================

country_endpoints <- endpoints_long %>%
  
  pivot_wider(
    
    id_cols =
      c(
        ISO3,
        Country
      ),
    
    names_from =
      Year,
    
    values_from =
      all_of(
        endpoint_vars
      ),
    
    names_glue =
      "{.value}_{Year}"
  ) %>%
  
  arrange(
    ISO3
  )


if (
  nrow(
    country_endpoints
  ) !=
  expected_n_countries
) {
  
  stop(
    "Country endpoint table does not contain exactly ",
    expected_n_countries,
    " countries."
  )
}


write_csv(
  country_endpoints,
  file.path(
    out_res_dir,
    "04_country_endpoints_1992_2022.csv"
  )
)


# ============================================================
# 18. CALCULATE ABSOLUTE AND RELATIVE CHANGES
# ============================================================

country_changes <- country_endpoints %>%
  
  mutate(
    
    # --------------------------------------------------------
    # Protein supply
    # --------------------------------------------------------
    
    delta_protein =
      Total_protein_supply_2022 -
      Total_protein_supply_1992,
    
    
    rel_delta_protein =
      safe_rel_change(
        Total_protein_supply_1992,
        Total_protein_supply_2022
      ),
    
    
    # --------------------------------------------------------
    # Cropland N surplus
    # --------------------------------------------------------
    
    delta_surplus =
      N_surplus_2022 -
      N_surplus_1992,
    
    
    rel_delta_surplus =
      safe_rel_change(
        N_surplus_1992,
        N_surplus_2022
      ),
    
    
    # --------------------------------------------------------
    # N surplus per unit protein supply
    # --------------------------------------------------------
    
    delta_nsp =
      N_surplus_per_protein_mass_2022 -
      N_surplus_per_protein_mass_1992,
    
    
    rel_delta_nsp =
      safe_rel_change(
        N_surplus_per_protein_mass_1992,
        N_surplus_per_protein_mass_2022
      )
  )


write_csv(
  country_changes,
  file.path(
    out_res_dir,
    "05_country_first_last_changes.csv"
  )
)


saveRDS(
  country_changes,
  file.path(
    out_res_dir,
    "05_country_first_last_changes.rds"
  )
)


# ============================================================
# 19. RELATIVE-CHANGE DENOMINATOR QC
# ============================================================
#
# Relative changes become unstable when the initial value is
# extremely close to zero.
#
# We do NOT change the classification here.
# We simply audit these cases explicitly.
# ============================================================

denominator_qc <- country_changes %>%
  
  transmute(
    
    ISO3,
    
    Country,
    
    
    initial_protein =
      Total_protein_supply_1992,
    
    
    initial_N_surplus =
      N_surplus_1992,
    
    
    initial_N_surplus_per_protein =
      N_surplus_per_protein_mass_1992,
    
    
    abs_initial_surplus =
      abs(
        N_surplus_1992
      ),
    
    
    abs_initial_nsp =
      abs(
        N_surplus_per_protein_mass_1992
      ),
    
    
    N_surplus_within_1_of_zero =
      abs(
        N_surplus_1992
      ) <
      1,
    
    
    N_surplus_within_5_of_zero =
      abs(
        N_surplus_1992
      ) <
      5,
    
    
    nsp_within_0_01_of_zero =
      abs(
        N_surplus_per_protein_mass_1992
      ) <
      0.01
  ) %>%
  
  arrange(
    abs_initial_surplus
  )


write_csv(
  denominator_qc,
  file.path(
    out_res_dir,
    "06_relative_change_denominator_QC.csv"
  )
)


denominator_qc_summary <- denominator_qc %>%
  
  summarise(
    
    countries =
      n(),
    
    initial_surplus_abs_lt_1 =
      sum(
        N_surplus_within_1_of_zero,
        na.rm =
          TRUE
      ),
    
    initial_surplus_abs_lt_5 =
      sum(
        N_surplus_within_5_of_zero,
        na.rm =
          TRUE
      ),
    
    initial_nsp_abs_lt_0_01 =
      sum(
        nsp_within_0_01_of_zero,
        na.rm =
          TRUE
      )
  )


write_csv(
  denominator_qc_summary,
  file.path(
    out_res_dir,
    "06_relative_change_denominator_QC_summary.csv"
  )
)


# ============================================================
# 20. MAIN 5% CLASSIFICATION
# ============================================================

classification_5 <- classify_threshold(
  
  country_changes =
    country_changes,
  
  threshold =
    main_threshold
) %>%
  
  mutate(
    
    transition_class =
      factor(
        transition_class,
        levels =
          transition_order
      ),
    
    transition_category =
      recode(
        as.character(
          transition_class
        ),
        !!!transition_labels
      )
  ) %>%
  
  arrange(
    transition_class,
    Country
  )


# ============================================================
# 21. MAIN CLASSIFICATION MANDATORY CHECKS
# ============================================================

if (
  nrow(
    classification_5
  ) !=
  expected_n_countries
) {
  
  stop(
    "Main classification does not contain exactly ",
    expected_n_countries,
    " countries."
  )
}


if (
  any(
    is.na(
      classification_5$transition_class
    )
  )
) {
  
  stop(
    "NA transition classes detected."
  )
}


# ------------------------------------------------------------
# Every country must belong to exactly one final category
# ------------------------------------------------------------

exclusive_check <- classification_5 %>%
  
  transmute(
    
    ISO3,
    
    Country,
    
    n_final_categories =
      rowSums(
        cbind(
          absolute_decoupling,
          relative_decoupling,
          inefficient_intensification,
          coupled_intensification,
          absolute_reduction_without_protein_growth,
          other_or_no_clear_transition
        )
      )
  )


if (
  any(
    exclusive_check$n_final_categories !=
    1
  )
) {
  
  write_csv(
    exclusive_check,
    file.path(
      out_res_dir,
      "ERROR_nonexclusive_classification.csv"
    )
  )
  
  stop(
    "Classification is not mutually exclusive."
  )
}


# ============================================================
# 22. MAIN CATEGORY COUNTS
# ============================================================

main_counts <- classification_5 %>%
  
  count(
    transition_class,
    name =
      "n_countries",
    .drop =
      FALSE
  ) %>%
  
  complete(
    
    transition_class =
      factor(
        transition_order,
        levels =
          transition_order
      ),
    
    fill =
      list(
        n_countries =
          0
      )
  ) %>%
  
  mutate(
    
    percentage =
      100 *
      n_countries /
      sum(
        n_countries
      ),
    
    transition_category =
      recode(
        as.character(
          transition_class
        ),
        !!!transition_labels
      )
  )


write_csv(
  main_counts,
  file.path(
    out_res_dir,
    "07_transition_category_counts_5pct.csv"
  )
)


write_csv(
  classification_5,
  file.path(
    out_res_dir,
    "08_country_transition_classification_5pct.csv"
  )
)


# ============================================================
# 23. CHECK THE THREE PREVIOUSLY VALIDATED COUNTS
# ============================================================

validated_count_check <- tibble(
  
  transition_class =
    names(
      expected_validated_counts
    ),
  
  expected_n =
    as.integer(
      expected_validated_counts
    )
) %>%
  
  left_join(
    
    main_counts %>%
      
      transmute(
        
        transition_class =
          as.character(
            transition_class
          ),
        
        observed_n =
          n_countries
      ),
    
    by =
      "transition_class"
  ) %>%
  
  mutate(
    
    matches_expected =
      expected_n ==
      observed_n
  )


write_csv(
  validated_count_check,
  file.path(
    out_res_dir,
    "09_validated_main_count_check.csv"
  )
)


if (
  !all(
    validated_count_check$matches_expected
  )
) {
  
  stop(
    paste0(
      "\nCRITICAL: the corrected 5% classification does not reproduce ",
      "the previously validated counts for absolute decoupling, ",
      "relative decoupling and inefficient intensification.\n",
      "Inspect 09_validated_main_count_check.csv before proceeding."
    )
  )
}


# ============================================================
# 24. NON-EXCLUSIVE LOGICAL FLAGS QC
# ============================================================
#
# This table helps us understand overlapping conditions before
# priority rules are imposed.
#
# Example:
# a country may simultaneously show protein growth,
# absolute N-surplus increase and worsening N-surplus intensity.
#
# The final transition_class remains mutually exclusive.
# ============================================================

flag_overlap_qc <- classification_5 %>%
  
  transmute(
    
    ISO3,
    
    Country,
    
    absolute_decoupling_flag,
    
    relative_decoupling_flag,
    
    inefficient_intensification_flag,
    
    coupled_intensification_flag,
    
    absolute_reduction_without_protein_growth_flag,
    
    protein_gain_surplus_stable_flag,
    
    final_transition_class =
      as.character(
        transition_class
      )
  )


write_csv(
  flag_overlap_qc,
  file.path(
    out_res_dir,
    "10_nonexclusive_condition_flags_QC.csv"
  )
)


# ============================================================
# 25. COMPLETE 0-10% THRESHOLD SENSITIVITY
# ============================================================

classifications_all_thresholds <- purrr::map_dfr(
  
  thresholds,
  
  ~ classify_threshold(
    country_changes,
    .x
  )
) %>%
  
  mutate(
    
    transition_class =
      factor(
        transition_class,
        levels =
          transition_order
      ),
    
    transition_category =
      recode(
        as.character(
          transition_class
        ),
        !!!transition_labels
      )
  )


# ============================================================
# 26. THRESHOLD-SENSITIVITY QC
# ============================================================

threshold_n_check <- classifications_all_thresholds %>%
  
  count(
    threshold,
    name =
      "n_countries"
  )


if (
  any(
    threshold_n_check$n_countries !=
    expected_n_countries
  )
) {
  
  stop(
    "At least one threshold does not contain exactly ",
    expected_n_countries,
    " countries."
  )
}


threshold_duplicate_check <- classifications_all_thresholds %>%
  
  count(
    threshold,
    ISO3,
    name =
      "n_rows"
  ) %>%
  
  filter(
    n_rows !=
      1
  )


if (
  nrow(
    threshold_duplicate_check
  ) >
  0
) {
  
  write_csv(
    threshold_duplicate_check,
    file.path(
      out_res_dir,
      "ERROR_threshold_duplicate_classifications.csv"
    )
  )
  
  stop(
    "At least one country has more than one row at a threshold."
  )
}


# ============================================================
# 27. SAVE FULL THRESHOLD CLASSIFICATIONS
# ============================================================

write_csv(
  classifications_all_thresholds,
  file.path(
    out_res_dir,
    "11_threshold_sensitivity_all_country_classifications_long.csv"
  )
)


saveRDS(
  classifications_all_thresholds,
  file.path(
    out_res_dir,
    "11_threshold_sensitivity_all_country_classifications_long.rds"
  )
)


# ============================================================
# 28. CATEGORY PREVALENCE ACROSS THRESHOLDS
# ============================================================

threshold_counts <- classifications_all_thresholds %>%
  
  count(
    threshold,
    transition_class,
    name =
      "n_countries",
    .drop =
      FALSE
  ) %>%
  
  complete(
    
    threshold =
      thresholds,
    
    transition_class =
      factor(
        transition_order,
        levels =
          transition_order
      ),
    
    fill =
      list(
        n_countries =
          0
      )
  ) %>%
  
  group_by(
    threshold
  ) %>%
  
  mutate(
    
    percentage =
      100 *
      n_countries /
      sum(
        n_countries
      )
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    transition_category =
      recode(
        as.character(
          transition_class
        ),
        !!!transition_labels
      )
  )


write_csv(
  threshold_counts,
  file.path(
    out_res_dir,
    "12_threshold_sensitivity_category_counts.csv"
  )
)


# ------------------------------------------------------------
# Wide manuscript-friendly version
# ------------------------------------------------------------

threshold_counts_wide <- threshold_counts %>%
  
  mutate(
    
    n_pct =
      paste0(
        n_countries,
        " (",
        sprintf(
          "%.1f",
          percentage
        ),
        "%)"
      ),
    
    threshold_label =
      paste0(
        threshold,
        "%"
      )
  ) %>%
  
  select(
    transition_category,
    threshold_label,
    n_pct
  ) %>%
  
  pivot_wider(
    
    names_from =
      threshold_label,
    
    values_from =
      n_pct
  )


write_csv(
  threshold_counts_wide,
  file.path(
    out_res_dir,
    "13_threshold_sensitivity_category_counts_wide.csv"
  )
)


# ============================================================
# 29. COUNTRY-LEVEL THRESHOLD STABILITY
# ============================================================

country_classifications_wide <- classifications_all_thresholds %>%
  
  transmute(
    
    ISO3,
    
    Country,
    
    threshold_col =
      paste0(
        "category_",
        threshold
      ),
    
    transition_class =
      as.character(
        transition_class
      )
  ) %>%
  
  pivot_wider(
    
    names_from =
      threshold_col,
    
    values_from =
      transition_class
  )


category_columns <- paste0(
  "category_",
  thresholds
)


country_classifications_wide <- country_classifications_wide %>%
  
  rowwise() %>%
  
  mutate(
    
    n_unique_categories =
      n_distinct(
        c_across(
          all_of(
            category_columns
          )
        ),
        na.rm =
          TRUE
      ),
    
    same_all_thresholds =
      n_unique_categories ==
      1
  ) %>%
  
  ungroup()


write_csv(
  country_classifications_wide,
  file.path(
    out_res_dir,
    "14_threshold_sensitivity_country_stability.csv"
  )
)


# ============================================================
# 30. OVERALL STABILITY SUMMARY
# ============================================================

overall_stability_summary <- country_classifications_wide %>%
  
  summarise(
    
    n_countries =
      n(),
    
    n_same_all_thresholds =
      sum(
        same_all_thresholds
      ),
    
    pct_same_all_thresholds =
      100 *
      mean(
        same_all_thresholds
      ),
    
    n_with_1_unique_category =
      sum(
        n_unique_categories ==
          1
      ),
    
    n_with_2_unique_categories =
      sum(
        n_unique_categories ==
          2
      ),
    
    n_with_3_unique_categories =
      sum(
        n_unique_categories ==
          3
      ),
    
    n_with_4plus_unique_categories =
      sum(
        n_unique_categories >=
          4
      )
  )


write_csv(
  overall_stability_summary,
  file.path(
    out_res_dir,
    "15_threshold_sensitivity_overall_stability_summary.csv"
  )
)


# ============================================================
# 31. STABILITY RELATIVE TO MAIN 5% CLASSIFICATION
# ============================================================

main_category_reference <- classifications_all_thresholds %>%
  
  filter(
    threshold ==
      main_threshold
  ) %>%
  
  transmute(
    
    ISO3,
    
    main_5pct_class =
      as.character(
        transition_class
      )
  )


agreement_with_5pct <- classifications_all_thresholds %>%
  
  transmute(
    
    ISO3,
    
    Country,
    
    threshold,
    
    transition_class =
      as.character(
        transition_class
      )
  ) %>%
  
  left_join(
    main_category_reference,
    by =
      "ISO3"
  ) %>%
  
  mutate(
    
    same_as_5pct =
      transition_class ==
      main_5pct_class
  ) %>%
  
  group_by(
    threshold
  ) %>%
  
  summarise(
    
    n_countries =
      n(),
    
    n_same_as_5pct =
      sum(
        same_as_5pct
      ),
    
    percentage_same_as_5pct =
      100 *
      mean(
        same_as_5pct
      ),
    
    .groups =
      "drop"
  )


write_csv(
  agreement_with_5pct,
  file.path(
    out_res_dir,
    "16_threshold_agreement_with_main_5pct.csv"
  )
)


# ============================================================
# 32. TRANSITION MATRICES RELATIVE TO 5%
# ============================================================
#
# These show which categories countries enter when the operational
# threshold is changed.
# ============================================================

make_threshold_transition <- function(
    classifications,
    comparison_threshold
) {
  
  reference <- classifications %>%
    
    filter(
      threshold ==
        main_threshold
    ) %>%
    
    transmute(
      
      ISO3,
      
      class_5pct =
        as.character(
          transition_class
        )
    )
  
  
  comparison <- classifications %>%
    
    filter(
      threshold ==
        comparison_threshold
    ) %>%
    
    transmute(
      
      ISO3,
      
      class_comparison =
        as.character(
          transition_class
        )
    )
  
  
  reference %>%
    
    inner_join(
      comparison,
      by =
        "ISO3"
    ) %>%
    
    count(
      
      from_5pct =
        class_5pct,
      
      to_comparison =
        class_comparison,
      
      name =
        "n"
    ) %>%
    
    complete(
      
      from_5pct =
        transition_order,
      
      to_comparison =
        transition_order,
      
      fill =
        list(
          n =
            0
        )
    ) %>%
    
    mutate(
      
      comparison_threshold =
        comparison_threshold
    )
}


transition_matrices <- purrr::map_dfr(
  
  thresholds[
    thresholds !=
      main_threshold
  ],
  
  ~ make_threshold_transition(
    classifications_all_thresholds,
    .x
  )
)


write_csv(
  transition_matrices,
  file.path(
    out_res_dir,
    "17_threshold_transition_matrices_vs_5pct.csv"
  )
)


# ============================================================
# 33. KEY-CATEGORY SENSITIVITY SUMMARY
# ============================================================

key_category_sensitivity <- threshold_counts %>%
  
  filter(
    
    as.character(
      transition_class
    ) %in%
      c(
        "absolute_decoupling",
        "relative_decoupling",
        "inefficient_intensification"
      )
  ) %>%
  
  select(
    
    threshold,
    
    transition_class,
    
    n_countries,
    
    percentage
  ) %>%
  
  pivot_wider(
    
    names_from =
      transition_class,
    
    values_from =
      c(
        n_countries,
        percentage
      )
  ) %>%
  
  arrange(
    threshold
  )


write_csv(
  key_category_sensitivity,
  file.path(
    out_res_dir,
    "18_key_category_threshold_sensitivity.csv"
  )
)


# ============================================================
# 34. SUPPLEMENTARY FIGURE
#     CATEGORY PREVALENCE ACROSS THRESHOLDS
# ============================================================

p_threshold_prevalence <- threshold_counts %>%
  
  mutate(
    
    transition_class =
      factor(
        transition_class,
        levels =
          transition_order
      )
  ) %>%
  
  ggplot(
    
    aes(
      
      x =
        threshold,
      
      y =
        percentage,
      
      color =
        transition_class,
      
      group =
        transition_class
    )
  ) +
  
  geom_vline(
    
    xintercept =
      main_threshold,
    
    linetype =
      "dashed",
    
    linewidth =
      0.55,
    
    color =
      "grey35"
  ) +
  
  geom_line(
    linewidth =
      0.9
  ) +
  
  geom_point(
    size =
      2
  ) +
  
  scale_color_manual(
    
    values =
      transition_palette,
    
    labels =
      transition_labels,
    
    drop =
      FALSE
  ) +
  
  scale_x_continuous(
    
    breaks =
      thresholds,
    
    labels =
      paste0(
        thresholds,
        "%"
      )
  ) +
  
  scale_y_continuous(
    
    labels =
      label_number(
        accuracy =
          1,
        suffix =
          "%"
      ),
    
    expand =
      expansion(
        mult =
          c(
            0.02,
            0.08
          )
      )
  ) +
  
  labs(
    
    title =
      "Sensitivity of transition-category prevalence to classification threshold",
    
    x =
      "Relative-change threshold",
    
    y =
      "Countries (%)",
    
    color =
      NULL
  ) +
  
  theme_nature()


print(
  p_threshold_prevalence
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Supplementary_transition_threshold_sensitivity.png"
  ),
  
  p_threshold_prevalence,
  
  width =
    8,
  
  height =
    5.5,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Supplementary_transition_threshold_sensitivity.pdf"
  ),
  
  p_threshold_prevalence,
  
  width =
    8,
  
  height =
    5.5
)


# ============================================================
# 35. OPTIONAL SUPPLEMENTARY FIGURE
#     AGREEMENT WITH MAIN 5% CLASSIFICATION
# ============================================================
#
# This is useful for QC and potentially Supplementary Information.
# It does not need to enter the paper unless informative.
# ============================================================

p_threshold_agreement <- ggplot(
  
  agreement_with_5pct,
  
  aes(
    x =
      threshold,
    y =
      percentage_same_as_5pct
  )
) +
  
  geom_vline(
    
    xintercept =
      main_threshold,
    
    linetype =
      "dashed",
    
    linewidth =
      0.55,
    
    color =
      "grey35"
  ) +
  
  geom_line(
    linewidth =
      0.9,
    color =
      "black"
  ) +
  
  geom_point(
    size =
      2,
    color =
      "black"
  ) +
  
  scale_x_continuous(
    
    breaks =
      thresholds,
    
    labels =
      paste0(
        thresholds,
        "%"
      )
  ) +
  
  scale_y_continuous(
    
    limits =
      c(
        0,
        100
      ),
    
    labels =
      label_percent(
        scale =
          1,
        accuracy =
          1
      )
  ) +
  
  labs(
    
    title =
      "Country-level agreement with the main 5% classification",
    
    x =
      "Relative-change threshold",
    
    y =
      "Countries retaining the 5% category"
  ) +
  
  theme_nature()


ggsave(
  
  file.path(
    out_plot_dir,
    "OPTIONAL_threshold_agreement_with_5pct.png"
  ),
  
  p_threshold_agreement,
  
  width =
    7,
  
  height =
    5,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "OPTIONAL_threshold_agreement_with_5pct.pdf"
  ),
  
  p_threshold_agreement,
  
  width =
    7,
  
  height =
    5
)


# ============================================================
# 36. MANUSCRIPT ANCHORS
# ============================================================

main_anchor_counts <- main_counts %>%
  
  transmute(
    
    result =
      paste0(
        as.character(
          transition_class
        ),
        "_n"
      ),
    
    value =
      as.numeric(
        n_countries
      ),
    
    units =
      "countries"
  )


main_anchor_percentages <- main_counts %>%
  
  transmute(
    
    result =
      paste0(
        as.character(
          transition_class
        ),
        "_percentage"
      ),
    
    value =
      percentage,
    
    units =
      "%"
  )


manuscript_anchors <- bind_rows(
  
  tibble(
    
    result = c(
      
      "classification_initial_year",
      
      "classification_final_year",
      
      "main_relative_change_threshold",
      
      "countries_classified"
    ),
    
    value = c(
      
      study_start,
      
      study_end,
      
      main_threshold,
      
      expected_n_countries
    ),
    
    units = c(
      
      "year",
      
      "year",
      
      "%",
      
      "countries"
    )
  ),
  
  main_anchor_counts,
  
  main_anchor_percentages
)


write_csv(
  manuscript_anchors,
  file.path(
    out_res_dir,
    "manuscript_anchors.csv"
  )
)


# ============================================================
# 37. SAVE PRIMARY CLASSIFICATION OBJECT
# ============================================================
#
# This is the main object that later scripts should load.
#
# In particular:
#
# - Figure 3 / PCA configurations
# - cross-tabulation
# - Figure 4 / absolute decoupling
#
# should use this saved object rather than recomputing the
# classification independently.
# ============================================================

transition_classification_object <- list(
  
  study_start =
    study_start,
  
  study_end =
    study_end,
  
  main_threshold =
    main_threshold,
  
  threshold_grid =
    thresholds,
  
  country_endpoints =
    country_endpoints,
  
  country_changes =
    country_changes,
  
  classification_5pct =
    classification_5,
  
  main_counts =
    main_counts,
  
  classifications_all_thresholds =
    classifications_all_thresholds,
  
  threshold_counts =
    threshold_counts,
  
  country_threshold_stability =
    country_classifications_wide,
  
  overall_stability_summary =
    overall_stability_summary,
  
  agreement_with_5pct =
    agreement_with_5pct,
  
  transition_matrices =
    transition_matrices,
  
  key_category_sensitivity =
    key_category_sensitivity,
  
  metric_qc =
    metric_qc,
  
  denominator_qc =
    denominator_qc,
  
  denominator_qc_summary =
    denominator_qc_summary
)


saveRDS(
  
  transition_classification_object,
  
  file.path(
    out_res_dir,
    "transition_classification.rds"
  )
)


# ============================================================
# 38. SAVE MAIN CLASSIFICATION ALONE AS RDS
# ============================================================
#
# Convenient lightweight object for later scripts.
# ============================================================

saveRDS(
  
  classification_5,
  
  file.path(
    out_res_dir,
    "country_transition_classification_5pct.rds"
  )
)


# ============================================================
# 39. REPRODUCIBILITY
# ============================================================

package_versions <- tibble(
  
  package =
    required_packages,
  
  version =
    purrr::map_chr(
      
      required_packages,
      
      ~ as.character(
        packageVersion(
          .x
        )
      )
    )
)


write_csv(
  package_versions,
  file.path(
    out_res_dir,
    "package_versions.csv"
  )
)


capture.output(
  
  sessionInfo(),
  
  file =
    file.path(
      out_res_dir,
      "sessionInfo.txt"
    )
)


# ============================================================
# 40. FINAL CONSOLE SUMMARY
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "FIRST-TO-LAST TRANSITION CLASSIFICATION\n"
)

cat(
  "============================================================\n"
)


cat(
  "\n--- MASS-HARMONIZED METRIC QC ---\n"
)

print(
  metric_qc,
  n = Inf,
  width = Inf
)


cat(
  "\n--- ENDPOINT COVERAGE QC ---\n"
)

print(
  
  country_coverage %>%
    summarise(
      
      countries =
        n(),
      
      all_have_31_years =
        all(
          n_years ==
            expected_n_years
        ),
      
      all_have_1992 =
        all(
          has_1992
        ),
      
      all_have_2022 =
        all(
          has_2022
        )
    ),
  
  width =
    Inf
)


cat(
  "\n--- RELATIVE-CHANGE DENOMINATOR QC ---\n"
)

print(
  denominator_qc_summary,
  n = Inf,
  width = Inf
)


cat(
  "\n--- MAIN 5% TRANSITION CATEGORIES ---\n"
)

print(
  main_counts,
  n = Inf,
  width = Inf
)


cat(
  "\n--- VALIDATED COUNT CHECK ---\n"
)

print(
  validated_count_check,
  n = Inf,
  width = Inf
)


cat(
  "\n--- THRESHOLD SENSITIVITY COUNTS ---\n"
)

print(
  threshold_counts_wide,
  n = Inf,
  width = Inf
)


cat(
  "\n--- AGREEMENT WITH MAIN 5% CLASSIFICATION ---\n"
)

print(
  agreement_with_5pct,
  n = Inf,
  width = Inf
)


cat(
  "\n--- OVERALL COUNTRY-LEVEL STABILITY ---\n"
)

print(
  overall_stability_summary,
  n = Inf,
  width = Inf
)


cat(
  "\n--- KEY CATEGORY SENSITIVITY ---\n"
)

print(
  key_category_sensitivity,
  n = Inf,
  width = Inf
)


cat(
  "\n============================================================\n"
)

cat(
  "INTERPRETATION REMINDERS\n"
)

cat(
  "============================================================\n"
)


cat(
  "- Classification uses exact annual endpoints: 1992 and 2022.\n"
)

cat(
  "- Figure 2 rolling means are NOT used here.\n"
)

cat(
  "- The corrected N_surplus_per_protein_mass indicator is used exclusively.\n"
)

cat(
  "- Absolute and relative decoupling are mutually exclusive final categories.\n"
)

cat(
  "- Absolute decoupling takes priority when both absolute and intensity reductions occur.\n"
)

cat(
  "- Threshold sensitivity evaluates robustness; it does not identify an optimal threshold.\n"
)

cat(
  "- First-to-last transition categories are distinct from future PCA-based trajectory configurations.\n"
)


cat(
  "\nPrimary classification saved to:\n"
)

cat(
  file.path(
    out_res_dir,
    "transition_classification.rds"
  ),
  "\n"
)


cat(
  "\nResults saved to:\n",
  out_res_dir,
  "\n"
)


cat(
  "\nPlots saved to:\n",
  out_plot_dir,
  "\n"
)


cat(
  "\nScript completed successfully.\n"
)

# ============================================================
# END OF 03_Transition_classification.R
# ============================================================