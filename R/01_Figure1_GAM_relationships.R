# ============================================================
# 01_Figure1_GAM_relationships.R
#
# Divergent nitrogen transition pathways during
# global agricultural development
#
# PURPOSE
# -------
# Reconstruct all analyses associated with Figure 1:
#
#   A) GDP per capita -> N surplus per unit of protein supply
#   B) N input intensity -> cropland N surplus
#   C) Total protein supply -> cropland N surplus
#
# INPUT
# -----
# data/Data_Final_31.csv
#
# IMPORTANT
# ---------
# Data_Final_31.csv contains the OLD mixed-scale indicator:
#
#   N_surplus_per_protein = N_surplus / Total_protein_supply
#
# This script renames it:
#
#   N_surplus_per_protein_legacy
#
# and calculates the DEFINITIVE mass-harmonized indicator:
#
#   N_surplus_per_protein_mass =
#       total territorial cropland N surplus (kg N yr-1)
#       ----------------------------------------------------
#       total national protein availability (kg protein yr-1)
#
# with:
#
#   Total_N_surplus_kg_yr =
#       N_surplus * Cropland_area
#
#   Total_protein_kg_yr =
#       Total_protein_supply *
#       Population_total *
#       365 / 1000
#
# Units:
#   kg N kg protein-1
#
# The definitive indicator:
# - is NOT a consumption-based nitrogen footprint;
# - does NOT explicitly correct for international trade;
# - relates territorial cropland N surplus to nationally
#   available protein.
#
# OUTPUTS
# -------
# MAIN:
# - Figure 1
# - GAM statistics
# - GAM vs linear comparisons
# - descriptive model-derived anchors
#
# SUPPLEMENTARY:
# - First derivatives
# - sequential exclusion of China, India and USA
# - temporal sensitivity: 1992-2007 vs 2008-2022
#
# QC:
# - indicator reconstruction
# - units / finite-value checks
# - sample sizes
# - basis-dimension diagnostics
# - concurvity
# - residual diagnostics
# - within-country lag-1 residual correlations
#
# REPRODUCIBILITY:
# - model .rds files
# - prediction / derivative CSVs
# - manuscript_anchors.csv
# - sessionInfo()
#
# MODEL SPECIFICATION
# -------------------
# Model A:
#   log1p(N_surplus_per_protein_mass) ~
#       s(log10_GDP, k = 8) +
#       s(ISO3, bs = "re")
#
# Model B:
#   log1p(N_surplus) ~
#       s(log10_N_input_per_ha, k = 10) +
#       s(ISO3, bs = "re")
#
# Model C:
#   log1p(N_surplus) ~
#       s(Total_protein_supply, k = 8) +
#       s(ISO3, bs = "re")
#
# Final GAMs use REML.
#
# GAM-vs-linear comparisons are refitted using ML so AIC
# comparisons are performed under the same estimation framework.
#
# No Year smooth is included.
# No percentile/outlier filtering is applied.
#
# DERIVATIVE INTERPRETATION
# -------------------------
# Model A:
# The vertical line in the main figure identifies the beginning
# of the FINAL significantly negative derivative interval that
# extends to the upper GDP boundary.
#
# Model B:
# The vertical line identifies the predictor value where the
# FIRST derivative is maximal, i.e. the steepest fitted slope.
#
# This is NOT:
# - a threshold;
# - a breakpoint;
# - a tipping point;
# - "maximum acceleration".
#
# Formal acceleration would require a second derivative.
#
# Supplementary derivative panels contain NO vertical lines.
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
  "mgcv",
  "gratia",
  "patchwork",
  "scales"
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
  library(mgcv)
  library(gratia)
  library(patchwork)
  library(scales)
  
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


# ------------------------------------------------------------
# Output directories
# ------------------------------------------------------------

out_res_dir <- file.path(
  results_dir,
  "01_Figure1_GAM_relationships"
)

out_plot_dir <- file.path(
  plots_dir,
  "01_Figure1_GAM_relationships"
)

model_dir <- file.path(
  out_res_dir,
  "models"
)

diag_res_dir <- file.path(
  out_res_dir,
  "diagnostics"
)

diag_plot_dir <- file.path(
  out_plot_dir,
  "diagnostics"
)

supp_res_dir <- file.path(
  out_res_dir,
  "supplementary"
)

supp_plot_dir <- file.path(
  out_plot_dir,
  "supplementary"
)


dirs_to_create <- c(
  out_res_dir,
  out_plot_dir,
  model_dir,
  diag_res_dir,
  diag_plot_dir,
  supp_res_dir,
  supp_plot_dir
)

walk(
  dirs_to_create,
  ~ dir.create(
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)


# ============================================================
# 4. LOAD INPUT DATA
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
# 5. CHECK REQUIRED INPUT VARIABLES
# ============================================================

required_input_vars <- c(
  "ISO3",
  "Country",
  "Year",
  "GDP_pc_constant2015USD",
  "Population_total",
  "Cropland_area",
  "Total_N_input",
  "Total_protein_supply",
  "N_surplus",
  "N_surplus_per_protein"
)


missing_input_vars <- setdiff(
  required_input_vars,
  names(df_raw)
)


if (length(missing_input_vars) > 0) {
  
  stop(
    "Required variables missing from Data_Final_31.csv: ",
    paste(
      missing_input_vars,
      collapse = ", "
    )
  )
}


# ============================================================
# 6. STANDARDIZE CORE VARIABLES
# ============================================================

df <- df_raw %>%
  mutate(
    
    ISO3 =
      as.character(ISO3),
    
    Country =
      as.character(Country),
    
    Year =
      as.integer(Year),
    
    GDP_pc_constant2015USD =
      as.numeric(
        GDP_pc_constant2015USD
      ),
    
    Population_total =
      as.numeric(
        Population_total
      ),
    
    Cropland_area =
      as.numeric(
        Cropland_area
      ),
    
    Total_N_input =
      as.numeric(
        Total_N_input
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
  )


# ============================================================
# 7. COUNTRY-YEAR UNIQUENESS QC
# ============================================================

duplicate_country_years <- df %>%
  count(
    ISO3,
    Year,
    name = "n"
  ) %>%
  filter(
    n > 1
  )


if (nrow(duplicate_country_years) > 0) {
  
  write_csv(
    duplicate_country_years,
    file.path(
      diag_res_dir,
      "00_duplicate_country_years.csv"
    )
  )
  
  stop(
    "Duplicated ISO3-Year combinations detected. ",
    "Inspect diagnostics/00_duplicate_country_years.csv."
  )
}


# ============================================================
# 8. CREATE DEFINITIVE DERIVED VARIABLES
# ============================================================
#
# Expected units:
#
# Population_total
#   persons
#
# Cropland_area
#   ha
#
# Total_N_input
#   tonnes N yr-1
#
# Total_protein_supply
#   g protein capita-1 day-1
#
# N_surplus
#   kg N ha-1 yr-1
#
# ------------------------------------------------------------
#
# N input intensity:
#
# Total_N_input (tonnes N yr-1) * 1000
# --------------------------------------
# Cropland_area (ha)
#
# = kg N ha-1 yr-1
#
# ------------------------------------------------------------
#
# Total territorial cropland N surplus:
#
# N_surplus (kg N ha-1 yr-1) *
# Cropland_area (ha)
#
# = kg N yr-1
#
# ------------------------------------------------------------
#
# Total national protein availability:
#
# Total_protein_supply (g capita-1 day-1) *
# Population_total *
# 365 / 1000
#
# = kg protein yr-1
#
# ------------------------------------------------------------
#
# Definitive N-surplus/protein indicator:
#
# Total_N_surplus_kg_yr /
# Total_protein_kg_yr
#
# = kg N kg protein-1
# ============================================================

df <- df %>%
  
  rename(
    N_surplus_per_protein_legacy =
      N_surplus_per_protein
  ) %>%
  
  mutate(
    
    # --------------------------------------------------------
    # N input intensity
    # --------------------------------------------------------
    
    N_input_per_ha =
      if_else(
        is.finite(Total_N_input) &
          is.finite(Cropland_area) &
          Cropland_area > 0,
        
        (Total_N_input * 1000) /
          Cropland_area,
        
        NA_real_
      ),
    
    
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
    # DEFINITIVE metric
    # --------------------------------------------------------
    
    N_surplus_per_protein_mass =
      if_else(
        is.finite(Total_N_surplus_kg_yr) &
          is.finite(Total_protein_kg_yr) &
          Total_protein_kg_yr > 0,
        
        Total_N_surplus_kg_yr /
          Total_protein_kg_yr,
        
        NA_real_
      ),
    
    
    # --------------------------------------------------------
    # Useful QC quantity
    # --------------------------------------------------------
    
    Cropland_ha_per_capita =
      if_else(
        is.finite(Cropland_area) &
          is.finite(Population_total) &
          Population_total > 0,
        
        Cropland_area /
          Population_total,
        
        NA_real_
      ),
    
    
    # --------------------------------------------------------
    # Reconstruction of OLD indicator
    # --------------------------------------------------------
    
    N_surplus_per_protein_legacy_reconstructed =
      if_else(
        is.finite(N_surplus) &
          is.finite(Total_protein_supply) &
          Total_protein_supply != 0,
        
        N_surplus /
          Total_protein_supply,
        
        NA_real_
      ),
    
    
    # --------------------------------------------------------
    # Algebraic identity for definitive metric
    # --------------------------------------------------------
    
    N_surplus_per_protein_mass_identity =
      N_surplus_per_protein_legacy *
      Cropland_ha_per_capita *
      1000 /
      365
  )


# ============================================================
# 9. QC OF INDICATOR RECONSTRUCTION
# ============================================================

qc_indicator_reconstruction <- tibble(
  
  check = c(
    "Legacy indicator reconstructed from N_surplus / protein supply",
    "Mass-harmonized indicator algebraic identity"
  ),
  
  max_absolute_difference = c(
    
    max(
      abs(
        df$N_surplus_per_protein_legacy -
          df$N_surplus_per_protein_legacy_reconstructed
      ),
      na.rm = TRUE
    ),
    
    max(
      abs(
        df$N_surplus_per_protein_mass -
          df$N_surplus_per_protein_mass_identity
      ),
      na.rm = TRUE
    )
  )
)


print(
  qc_indicator_reconstruction
)


write_csv(
  qc_indicator_reconstruction,
  file.path(
    out_res_dir,
    "00_QC_indicator_reconstruction.csv"
  )
)


# ============================================================
# 10. GENERAL DATA QC
# ============================================================

dataset_qc <- tibble(
  
  metric = c(
    
    "Rows",
    "Countries",
    "Minimum year",
    "Maximum year",
    
    "Missing GDP",
    "Missing population",
    "Missing cropland area",
    "Missing total N input",
    "Missing protein supply",
    "Missing cropland N surplus",
    
    "Missing N input intensity",
    "Missing definitive N surplus per protein",
    
    "Non-positive GDP",
    "Non-positive population",
    "Non-positive cropland area",
    "Negative N input intensity",
    
    "N surplus <= -1",
    "Definitive N surplus per protein <= -1"
  ),
  
  value = c(
    
    nrow(df),
    
    n_distinct(
      df$ISO3
    ),
    
    min(
      df$Year,
      na.rm = TRUE
    ),
    
    max(
      df$Year,
      na.rm = TRUE
    ),
    
    sum(
      !is.finite(
        df$GDP_pc_constant2015USD
      )
    ),
    
    sum(
      !is.finite(
        df$Population_total
      )
    ),
    
    sum(
      !is.finite(
        df$Cropland_area
      )
    ),
    
    sum(
      !is.finite(
        df$Total_N_input
      )
    ),
    
    sum(
      !is.finite(
        df$Total_protein_supply
      )
    ),
    
    sum(
      !is.finite(
        df$N_surplus
      )
    ),
    
    sum(
      !is.finite(
        df$N_input_per_ha
      )
    ),
    
    sum(
      !is.finite(
        df$N_surplus_per_protein_mass
      )
    ),
    
    sum(
      is.finite(
        df$GDP_pc_constant2015USD
      ) &
        df$GDP_pc_constant2015USD <= 0
    ),
    
    sum(
      is.finite(
        df$Population_total
      ) &
        df$Population_total <= 0
    ),
    
    sum(
      is.finite(
        df$Cropland_area
      ) &
        df$Cropland_area <= 0
    ),
    
    sum(
      is.finite(
        df$N_input_per_ha
      ) &
        df$N_input_per_ha < 0
    ),
    
    sum(
      is.finite(
        df$N_surplus
      ) &
        df$N_surplus <= -1
    ),
    
    sum(
      is.finite(
        df$N_surplus_per_protein_mass
      ) &
        df$N_surplus_per_protein_mass <= -1
    )
  )
)


write_csv(
  dataset_qc,
  file.path(
    out_res_dir,
    "00_dataset_QC.csv"
  )
)


# ============================================================
# 11. SAVE AUGMENTED ANALYTICAL DATASET
# ============================================================
#
# This is NOT intended to replace a future dedicated
# 00_Data_preparation.R master dataset.
#
# It is saved here so every value used in Figure 1 can be traced
# exactly to the analytical dataset used by this script.
# ============================================================

write_csv(
  df,
  file.path(
    out_res_dir,
    "00_Figure1_analytical_dataset.csv"
  )
)


saveRDS(
  df,
  file.path(
    out_res_dir,
    "00_Figure1_analytical_dataset.rds"
  )
)


# ============================================================
# 12. TRANSFORMATIONS FOR GAMs
# ============================================================

df <- df %>%
  mutate(
    
    log10_GDP =
      if_else(
        is.finite(
          GDP_pc_constant2015USD
        ) &
          GDP_pc_constant2015USD > 0,
        
        log10(
          GDP_pc_constant2015USD
        ),
        
        NA_real_
      ),
    
    
    log10_N_input_per_ha =
      if_else(
        is.finite(
          N_input_per_ha
        ) &
          N_input_per_ha >= 0,
        
        log10(
          N_input_per_ha + 1
        ),
        
        NA_real_
      ),
    
    
    log1p_N_surplus_per_protein =
      if_else(
        is.finite(
          N_surplus_per_protein_mass
        ) &
          N_surplus_per_protein_mass > -1,
        
        log1p(
          N_surplus_per_protein_mass
        ),
        
        NA_real_
      ),
    
    
    log1p_N_surplus =
      if_else(
        is.finite(
          N_surplus
        ) &
          N_surplus > -1,
        
        log1p(
          N_surplus
        ),
        
        NA_real_
      )
  )


# ============================================================
# 13. HELPER FUNCTIONS
# ============================================================

model_relationship <- function(
    model_id
) {
  
  switch(
    
    model_id,
    
    A =
      "GDP per capita -> N surplus per unit protein supply",
    
    B =
      "N input intensity -> cropland N surplus",
    
    C =
      "Protein supply -> cropland N surplus",
    
    stop(
      "Unknown model_id: ",
      model_id
    )
  )
}


focal_smooth_name <- function(
    model_id
) {
  
  switch(
    
    model_id,
    
    A =
      "s(log10_GDP)",
    
    B =
      "s(log10_N_input_per_ha)",
    
    C =
      "s(Total_protein_supply)",
    
    stop(
      "Unknown model_id."
    )
  )
}


prepare_model_data <- function(
    data,
    model_id
) {
  
  if (model_id == "A") {
    
    out <- data %>%
      filter(
        is.finite(
          log10_GDP
        ),
        is.finite(
          log1p_N_surplus_per_protein
        ),
        !is.na(
          ISO3
        )
      )
    
  } else if (model_id == "B") {
    
    out <- data %>%
      filter(
        is.finite(
          log10_N_input_per_ha
        ),
        is.finite(
          log1p_N_surplus
        ),
        !is.na(
          ISO3
        )
      )
    
  } else if (model_id == "C") {
    
    out <- data %>%
      filter(
        is.finite(
          Total_protein_supply
        ),
        is.finite(
          log1p_N_surplus
        ),
        !is.na(
          ISO3
        )
      )
    
  } else {
    
    stop(
      "Unknown model_id: ",
      model_id
    )
  }
  
  
  out %>%
    mutate(
      ISO3 =
        droplevels(
          factor(
            ISO3
          )
        )
    )
}


fit_gam_model <- function(
    data,
    model_id,
    method = "REML"
) {
  
  if (model_id == "A") {
    
    gam(
      
      log1p_N_surplus_per_protein ~
        
        s(
          log10_GDP,
          k = 8
        ) +
        
        s(
          ISO3,
          bs = "re"
        ),
      
      data =
        data,
      
      method =
        method
    )
    
  } else if (model_id == "B") {
    
    gam(
      
      log1p_N_surplus ~
        
        s(
          log10_N_input_per_ha,
          k = 10
        ) +
        
        s(
          ISO3,
          bs = "re"
        ),
      
      data =
        data,
      
      method =
        method
    )
    
  } else if (model_id == "C") {
    
    gam(
      
      log1p_N_surplus ~
        
        s(
          Total_protein_supply,
          k = 8
        ) +
        
        s(
          ISO3,
          bs = "re"
        ),
      
      data =
        data,
      
      method =
        method
    )
    
  } else {
    
    stop(
      "Unknown model_id: ",
      model_id
    )
  }
}


fit_linear_model <- function(
    data,
    model_id,
    method = "ML"
) {
  
  if (model_id == "A") {
    
    gam(
      
      log1p_N_surplus_per_protein ~
        
        log10_GDP +
        
        s(
          ISO3,
          bs = "re"
        ),
      
      data =
        data,
      
      method =
        method
    )
    
  } else if (model_id == "B") {
    
    gam(
      
      log1p_N_surplus ~
        
        log10_N_input_per_ha +
        
        s(
          ISO3,
          bs = "re"
        ),
      
      data =
        data,
      
      method =
        method
    )
    
  } else if (model_id == "C") {
    
    gam(
      
      log1p_N_surplus ~
        
        Total_protein_supply +
        
        s(
          ISO3,
          bs = "re"
        ),
      
      data =
        data,
      
      method =
        method
    )
    
  } else {
    
    stop(
      "Unknown model_id: ",
      model_id
    )
  }
}


# ============================================================
# 14. MAIN ANALYSIS DATASETS
# ============================================================

df_A <- prepare_model_data(
  df,
  "A"
)

df_B <- prepare_model_data(
  df,
  "B"
)

df_C <- prepare_model_data(
  df,
  "C"
)


sample_summary <- tibble(
  
  Model =
    c(
      "A",
      "B",
      "C"
    ),
  
  Relationship =
    c(
      model_relationship("A"),
      model_relationship("B"),
      model_relationship("C")
    ),
  
  n_rows =
    c(
      nrow(df_A),
      nrow(df_B),
      nrow(df_C)
    ),
  
  n_countries =
    c(
      n_distinct(
        df_A$ISO3
      ),
      n_distinct(
        df_B$ISO3
      ),
      n_distinct(
        df_C$ISO3
      )
    ),
  
  first_year =
    c(
      min(df_A$Year),
      min(df_B$Year),
      min(df_C$Year)
    ),
  
  last_year =
    c(
      max(df_A$Year),
      max(df_B$Year),
      max(df_C$Year)
    )
)


write_csv(
  sample_summary,
  file.path(
    out_res_dir,
    "01_main_model_sample_summary.csv"
  )
)


# ============================================================
# 15. FIT MAIN GAMs
# ============================================================

mA <- fit_gam_model(
  df_A,
  "A",
  method = "REML"
)

mB <- fit_gam_model(
  df_B,
  "B",
  method = "REML"
)

mC <- fit_gam_model(
  df_C,
  "C",
  method = "REML"
)


main_models <- list(
  
  A = mA,
  B = mB,
  C = mC
)


saveRDS(
  main_models,
  file.path(
    model_dir,
    "01_main_GAM_models_REML.rds"
  )
)


# ============================================================
# 16. SAVE MODEL SUMMARIES
# ============================================================

walk2(
  
  main_models,
  names(
    main_models
  ),
  
  function(
    model,
    model_id
  ) {
    
    capture.output(
      
      summary(
        model
      ),
      
      file =
        file.path(
          diag_res_dir,
          paste0(
            "Model_",
            model_id,
            "_summary.txt"
          )
        )
    )
    
    
    capture.output(
      
      gam.check(
        model
      ),
      
      file =
        file.path(
          diag_res_dir,
          paste0(
            "Model_",
            model_id,
            "_gam_check.txt"
          )
        )
    )
    
    
    capture.output(
      
      concurvity(
        model,
        full = TRUE
      ),
      
      file =
        file.path(
          diag_res_dir,
          paste0(
            "Model_",
            model_id,
            "_concurvity.txt"
          )
        )
    )
  }
)


# ============================================================
# 17. EXTRACT MAIN GAM STATISTICS
# ============================================================

extract_model_stats <- function(
    model,
    model_id
) {
  
  sm <- summary(
    model
  )
  
  st <- sm$s.table
  
  target_smooth <-
    focal_smooth_name(
      model_id
    )
  
  row_index <- which(
    rownames(
      st
    ) ==
      target_smooth
  )
  
  
  if (length(row_index) != 1) {
    
    stop(
      "Could not uniquely identify ",
      target_smooth,
      " in Model ",
      model_id
    )
  }
  
  
  tibble(
    
    Model =
      model_id,
    
    Relationship =
      model_relationship(
        model_id
      ),
    
    focal_smooth =
      target_smooth,
    
    EDF =
      st[
        row_index,
        "edf"
      ],
    
    Ref_df =
      st[
        row_index,
        "Ref.df"
      ],
    
    F =
      st[
        row_index,
        "F"
      ],
    
    P_smooth =
      st[
        row_index,
        "p-value"
      ],
    
    adjusted_R2 =
      sm$r.sq,
    
    deviance_explained =
      sm$dev.expl,
    
    n =
      nobs(
        model
      )
  )
}


main_model_stats <- bind_rows(
  
  extract_model_stats(
    mA,
    "A"
  ),
  
  extract_model_stats(
    mB,
    "B"
  ),
  
  extract_model_stats(
    mC,
    "C"
  )
)


write_csv(
  main_model_stats,
  file.path(
    out_res_dir,
    "02_main_GAM_statistics.csv"
  )
)


# ============================================================
# 18. BASIS-DIMENSION QC
# ============================================================

extract_k_check <- function(
    model,
    model_id
) {
  
  kc <- mgcv::k.check(
    model
  )
  
  as.data.frame(
    kc
  ) %>%
    rownames_to_column(
      "smooth"
    ) %>%
    as_tibble() %>%
    mutate(
      Model =
        model_id,
      .before =
        1
    )
}


k_check_main <- bind_rows(
  
  extract_k_check(
    mA,
    "A"
  ),
  
  extract_k_check(
    mB,
    "B"
  ),
  
  extract_k_check(
    mC,
    "C"
  )
)


write_csv(
  k_check_main,
  file.path(
    diag_res_dir,
    "01_main_GAM_k_dimension_checks.csv"
  )
)


# ============================================================
# 19. GAM vs LINEAR MODEL COMPARISON
# ============================================================
#
# Both alternatives are fitted with ML for AIC comparison.
#
# Main inferential/predictive GAMs remain REML.
# ============================================================

compare_gam_vs_linear <- function(
    data,
    model_id
) {
  
  gam_ml <- fit_gam_model(
    data,
    model_id,
    method = "ML"
  )
  
  
  linear_ml <- fit_linear_model(
    data,
    model_id,
    method = "ML"
  )
  
  
  tibble(
    
    Model =
      model_id,
    
    Relationship =
      model_relationship(
        model_id
      ),
    
    AIC_linear_ML =
      AIC(
        linear_ml
      ),
    
    AIC_GAM_ML =
      AIC(
        gam_ml
      ),
    
    delta_AIC_linear_minus_GAM =
      AIC(
        linear_ml
      ) -
      AIC(
        gam_ml
      ),
    
    preferred_model =
      if_else(
        AIC(
          gam_ml
        ) <
          AIC(
            linear_ml
          ),
        "GAM",
        "Linear"
      )
  )
}


gam_vs_linear_main <- bind_rows(
  
  compare_gam_vs_linear(
    df_A,
    "A"
  ),
  
  compare_gam_vs_linear(
    df_B,
    "B"
  ),
  
  compare_gam_vs_linear(
    df_C,
    "C"
  )
)


write_csv(
  gam_vs_linear_main,
  file.path(
    out_res_dir,
    "03_GAM_vs_linear_AIC_ML.csv"
  )
)


# ============================================================
# 20. RESIDUAL DIAGNOSTICS
# ============================================================

save_residual_diagnostics <- function(
    model,
    data,
    model_id
) {
  
  diag_df <- data %>%
    mutate(
      
      fitted_link =
        fitted(
          model
        ),
      
      deviance_residual =
        residuals(
          model,
          type = "deviance"
        ),
      
      pearson_residual =
        residuals(
          model,
          type = "pearson"
        )
    )
  
  
  write_csv(
    
    diag_df %>%
      select(
        ISO3,
        Country,
        Year,
        fitted_link,
        deviance_residual,
        pearson_residual
      ),
    
    file.path(
      diag_res_dir,
      paste0(
        "Model_",
        model_id,
        "_residuals.csv"
      )
    )
  )
  
  
  # ----------------------------------------------------------
  # Residuals vs fitted
  # ----------------------------------------------------------
  
  p_res_fit <- ggplot(
    diag_df,
    aes(
      x =
        fitted_link,
      y =
        deviance_residual
    )
  ) +
    
    geom_point(
      alpha =
        0.25,
      size =
        0.7
    ) +
    
    geom_hline(
      yintercept =
        0,
      linetype =
        "dashed",
      linewidth =
        0.5
    ) +
    
    geom_smooth(
      method =
        "loess",
      se =
        FALSE,
      linewidth =
        0.8
    ) +
    
    labs(
      title =
        paste0(
          "Model ",
          model_id,
          ": residuals vs fitted"
        ),
      x =
        "Fitted value",
      y =
        "Deviance residual"
    ) +
    
    theme_classic()
  
  
  ggsave(
    file.path(
      diag_plot_dir,
      paste0(
        "Model_",
        model_id,
        "_residuals_vs_fitted.png"
      )
    ),
    p_res_fit,
    width =
      6,
    height =
      4.5,
    dpi =
      600
  )
  
  
  # ----------------------------------------------------------
  # QQ plot
  # ----------------------------------------------------------
  
  p_qq <- ggplot(
    diag_df,
    aes(
      sample =
        deviance_residual
    )
  ) +
    
    stat_qq(
      alpha =
        0.35,
      size =
        0.8
    ) +
    
    stat_qq_line(
      linewidth =
        0.7
    ) +
    
    labs(
      title =
        paste0(
          "Model ",
          model_id,
          ": residual QQ plot"
        ),
      x =
        "Theoretical quantiles",
      y =
        "Observed residual quantiles"
    ) +
    
    theme_classic()
  
  
  ggsave(
    file.path(
      diag_plot_dir,
      paste0(
        "Model_",
        model_id,
        "_QQ_plot.png"
      )
    ),
    p_qq,
    width =
      6,
    height =
      4.5,
    dpi =
      600
  )
  
  
  # ----------------------------------------------------------
  # Within-country lag-1 residual correlation
  # ----------------------------------------------------------
  
  lag1_by_country <- diag_df %>%
    arrange(
      ISO3,
      Year
    ) %>%
    group_by(
      ISO3,
      Country
    ) %>%
    summarise(
      
      n =
        sum(
          is.finite(
            deviance_residual
          )
        ),
      
      lag1_residual_correlation = {
        
        r <- deviance_residual[
          is.finite(
            deviance_residual
          )
        ]
        
        if (length(r) >= 5) {
          
          cor(
            r[-1],
            r[-length(r)],
            use =
              "complete.obs"
          )
          
        } else {
          
          NA_real_
        }
      },
      
      .groups =
        "drop"
    )
  
  
  lag1_summary <- lag1_by_country %>%
    summarise(
      
      Model =
        model_id,
      
      n_countries =
        sum(
          is.finite(
            lag1_residual_correlation
          )
        ),
      
      median_lag1 =
        median(
          lag1_residual_correlation,
          na.rm = TRUE
        ),
      
      q25_lag1 =
        quantile(
          lag1_residual_correlation,
          0.25,
          na.rm = TRUE,
          names = FALSE
        ),
      
      q75_lag1 =
        quantile(
          lag1_residual_correlation,
          0.75,
          na.rm = TRUE,
          names = FALSE
        )
    )
  
  
  write_csv(
    lag1_by_country,
    file.path(
      diag_res_dir,
      paste0(
        "Model_",
        model_id,
        "_within_country_lag1_residuals.csv"
      )
    )
  )
  
  
  invisible(
    lag1_summary
  )
}


lag1_main <- bind_rows(
  
  save_residual_diagnostics(
    mA,
    df_A,
    "A"
  ),
  
  save_residual_diagnostics(
    mB,
    df_B,
    "B"
  ),
  
  save_residual_diagnostics(
    mC,
    df_C,
    "C"
  )
)


write_csv(
  lag1_main,
  file.path(
    diag_res_dir,
    "02_main_GAM_within_country_lag1_summary.csv"
  )
)


# ============================================================
# 21. PREDICTION GRID
# ============================================================

make_prediction_grid <- function(
    data,
    model_id,
    n = 500
) {
  
  if (model_id == "A") {
    
    tibble(
      
      log10_GDP =
        seq(
          min(
            data$log10_GDP,
            na.rm = TRUE
          ),
          max(
            data$log10_GDP,
            na.rm = TRUE
          ),
          length.out =
            n
        ),
      
      ISO3 =
        data$ISO3[1]
    )
    
  } else if (model_id == "B") {
    
    tibble(
      
      log10_N_input_per_ha =
        seq(
          min(
            data$log10_N_input_per_ha,
            na.rm = TRUE
          ),
          max(
            data$log10_N_input_per_ha,
            na.rm = TRUE
          ),
          length.out =
            n
        ),
      
      ISO3 =
        data$ISO3[1]
    )
    
  } else if (model_id == "C") {
    
    tibble(
      
      Total_protein_supply =
        seq(
          min(
            data$Total_protein_supply,
            na.rm = TRUE
          ),
          max(
            data$Total_protein_supply,
            na.rm = TRUE
          ),
          length.out =
            n
        ),
      
      ISO3 =
        data$ISO3[1]
    )
    
  } else {
    
    stop(
      "Unknown model_id."
    )
  }
}


predict_main_smooth <- function(
    model,
    data,
    model_id,
    n = 500
) {
  
  newdata <- make_prediction_grid(
    data,
    model_id,
    n
  )
  
  
  pr <- predict(
    
    model,
    
    newdata =
      newdata,
    
    type =
      "link",
    
    se.fit =
      TRUE,
    
    exclude =
      "s(ISO3)"
  )
  
  
  out <- newdata %>%
    mutate(
      
      fit_link =
        as.numeric(
          pr$fit
        ),
      
      se_link =
        as.numeric(
          pr$se.fit
        ),
      
      lower_link =
        fit_link -
        1.96 *
        se_link,
      
      upper_link =
        fit_link +
        1.96 *
        se_link,
      
      fit =
        expm1(
          fit_link
        ),
      
      lower =
        expm1(
          lower_link
        ),
      
      upper =
        expm1(
          upper_link
        )
    )
  
  
  if (model_id == "A") {
    
    out <- out %>%
      mutate(
        x_plot =
          10^log10_GDP
      )
    
  } else if (model_id == "B") {
    
    out <- out %>%
      mutate(
        x_plot =
          10^log10_N_input_per_ha -
          1
      )
    
  } else {
    
    out <- out %>%
      mutate(
        x_plot =
          Total_protein_supply
      )
  }
  
  
  out %>%
    mutate(
      Model =
        model_id,
      .before =
        1
    )
}


pred_A <- predict_main_smooth(
  mA,
  df_A,
  "A"
)

pred_B <- predict_main_smooth(
  mB,
  df_B,
  "B"
)

pred_C <- predict_main_smooth(
  mC,
  df_C,
  "C"
)


main_predictions <- bind_rows(
  pred_A,
  pred_B,
  pred_C
)


write_csv(
  main_predictions,
  file.path(
    out_res_dir,
    "04_main_GAM_predictions.csv"
  )
)


# ============================================================
# 22. FIRST DERIVATIVES
# ============================================================

extract_derivative <- function(
    model,
    model_id,
    n = 1000
) {
  
  target_smooth <-
    focal_smooth_name(
      model_id
    )
  
  
  deriv <- gratia::derivatives(
    
    model,
    
    select =
      target_smooth,
    
    n =
      n
  ) %>%
    as_tibble()
  
  
  expected_x <- switch(
    
    model_id,
    
    A =
      "log10_GDP",
    
    B =
      "log10_N_input_per_ha",
    
    C =
      "Total_protein_supply"
  )
  
  
  possible_columns <- c(
    expected_x,
    "data",
    ".data"
  )
  
  
  x_column <-
    possible_columns[
      possible_columns %in%
        names(
          deriv
        )
    ][1]
  
  
  if (is.na(x_column)) {
    
    stop(
      "Could not identify derivative predictor for Model ",
      model_id,
      ". Columns returned by gratia: ",
      paste(
        names(
          deriv
        ),
        collapse = ", "
      )
    )
  }
  
  
  deriv <- deriv %>%
    mutate(
      x_model =
        as.numeric(
          .data[[
            x_column
          ]]
        )
    )
  
  
  if (model_id == "A") {
    
    deriv <- deriv %>%
      mutate(
        x_plot =
          10^x_model
      )
    
  } else if (model_id == "B") {
    
    deriv <- deriv %>%
      mutate(
        x_plot =
          10^x_model -
          1
      )
    
  } else {
    
    deriv <- deriv %>%
      mutate(
        x_plot =
          x_model
      )
  }
  
  
  deriv %>%
    mutate(
      
      Model =
        model_id,
      
      Relationship =
        model_relationship(
          model_id
        ),
      
      .before =
        1
    )
}


deriv_A <- extract_derivative(
  mA,
  "A"
)

deriv_B <- extract_derivative(
  mB,
  "B"
)

deriv_C <- extract_derivative(
  mC,
  "C"
)


main_derivatives <- bind_rows(
  deriv_A,
  deriv_B,
  deriv_C
)


write_csv(
  main_derivatives,
  file.path(
    out_res_dir,
    "05_main_GAM_first_derivatives.csv"
  )
)


saveRDS(
  main_derivatives,
  file.path(
    model_dir,
    "02_main_GAM_first_derivatives.rds"
  )
)


# ============================================================
# 23. DERIVATIVE SIGNIFICANCE INTERVALS
# ============================================================

extract_significance_intervals <- function(
    derivative_df,
    model_id
) {
  
  d <- derivative_df %>%
    arrange(
      x_plot
    ) %>%
    mutate(
      
      significance_state =
        case_when(
          
          .upper_ci < 0 ~
            "significantly_negative",
          
          .lower_ci > 0 ~
            "significantly_positive",
          
          TRUE ~
            "not_significant"
        )
    )
  
  
  rr <- rle(
    d$significance_state
  )
  
  
  d$run_id <- rep(
    seq_along(
      rr$lengths
    ),
    rr$lengths
  )
  
  
  d %>%
    group_by(
      run_id,
      significance_state
    ) %>%
    summarise(
      
      n_grid_points =
        n(),
      
      x_start =
        first(
          x_plot
        ),
      
      x_end =
        last(
          x_plot
        ),
      
      derivative_start =
        first(
          .derivative
        ),
      
      derivative_end =
        last(
          .derivative
        ),
      
      lower_ci_start =
        first(
          .lower_ci
        ),
      
      upper_ci_start =
        first(
          .upper_ci
        ),
      
      lower_ci_end =
        last(
          .lower_ci
        ),
      
      upper_ci_end =
        last(
          .upper_ci
        ),
      
      .groups =
        "drop"
    ) %>%
    mutate(
      
      Model =
        model_id,
      
      Relationship =
        model_relationship(
          model_id
        ),
      
      reaches_upper_boundary =
        run_id ==
        max(
          run_id
        ),
      
      .before =
        1
    )
}


derivative_intervals <- bind_rows(
  
  extract_significance_intervals(
    deriv_A,
    "A"
  ),
  
  extract_significance_intervals(
    deriv_B,
    "B"
  ),
  
  extract_significance_intervals(
    deriv_C,
    "C"
  )
)


write_csv(
  derivative_intervals,
  file.path(
    out_res_dir,
    "06_derivative_significance_intervals.csv"
  )
)


# ============================================================
# 24. MODEL A: TERMINAL SIGNIFICANT GDP DECLINE
# ============================================================

get_terminal_negative_anchor <- function(
    derivative_df,
    min_run_points = 10
) {
  
  d <- derivative_df %>%
    arrange(
      x_plot
    ) %>%
    mutate(
      sig_negative =
        .upper_ci <
        0
    )
  
  
  rr <- rle(
    d$sig_negative
  )
  
  
  run_id <- rep(
    seq_along(
      rr$lengths
    ),
    rr$lengths
  )
  
  
  d$run_id <-
    run_id
  
  
  last_run_id <-
    max(
      run_id
    )
  
  
  last_run_value <-
    rr$values[
      last_run_id
    ]
  
  
  last_run_length <-
    rr$lengths[
      last_run_id
    ]
  
  
  if (
    isTRUE(
      last_run_value
    ) &&
    last_run_length >=
    min_run_points
  ) {
    
    anchor <- d %>%
      filter(
        run_id ==
          last_run_id
      ) %>%
      slice(
        1
      ) %>%
      pull(
        x_plot
      )
    
  } else {
    
    anchor <-
      NA_real_
  }
  
  
  tibble(
    
    min_run_points =
      min_run_points,
    
    GDP_terminal_significant_decline_start =
      anchor,
    
    terminal_run_points =
      if_else(
        is.finite(
          anchor
        ),
        as.integer(
          last_run_length
        ),
        NA_integer_
      ),
    
    GDP_gradient_max =
      max(
        d$x_plot,
        na.rm = TRUE
      ),
    
    final_derivative =
      tail(
        d$.derivative,
        1
      ),
    
    final_lower_ci =
      tail(
        d$.lower_ci,
        1
      ),
    
    final_upper_ci =
      tail(
        d$.upper_ci,
        1
      )
  )
}


GDP_terminal_anchor <- get_terminal_negative_anchor(
  deriv_A,
  min_run_points = 10
)


GDP_anchor_sensitivity <- map_dfr(
  
  c(
    5,
    10,
    20,
    30,
    50
  ),
  
  ~ get_terminal_negative_anchor(
    deriv_A,
    min_run_points =
      .x
  )
)


write_csv(
  GDP_terminal_anchor,
  file.path(
    out_res_dir,
    "07_Model_A_GDP_terminal_decline_anchor.csv"
  )
)


write_csv(
  GDP_anchor_sensitivity,
  file.path(
    out_res_dir,
    "08_Model_A_GDP_anchor_runlength_sensitivity.csv"
  )
)


# ============================================================
# 25. MODEL B: MAXIMUM FIRST DERIVATIVE
# ============================================================
#
# This is the steepest fitted slope.
# It is NOT formal maximum acceleration.
# ============================================================

Ninput_max_slope <- deriv_B %>%
  filter(
    is.finite(
      .derivative
    )
  ) %>%
  slice_max(
    order_by =
      .derivative,
    n =
      1,
    with_ties =
      FALSE
  ) %>%
  transmute(
    
    N_input_intensity_at_maximum_first_derivative =
      x_plot,
    
    maximum_first_derivative =
      .derivative,
    
    lower_ci =
      .lower_ci,
    
    upper_ci =
      .upper_ci,
    
    derivative_significantly_positive =
      .lower_ci >
      0
  )


write_csv(
  Ninput_max_slope,
  file.path(
    out_res_dir,
    "09_Model_B_steepest_fitted_slope.csv"
  )
)


# ============================================================
# 26. FIGURE THEME
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
            base_size + 1,
          hjust =
            0
        ),
      
      plot.tag =
        element_text(
          face =
            "bold",
          size =
            base_size + 2
        ),
      
      legend.position =
        "bottom",
      
      plot.margin =
        margin(
          8,
          8,
          8,
          8
        )
    )
}


log1p_axis_trans <- scales::trans_new(
  
  name =
    "log1p",
  
  transform =
    log1p,
  
  inverse =
    expm1,
  
  domain =
    c(
      0,
      Inf
    )
)

# ============================================================
# 27. MAIN-FIGURE FUNCTION
# ============================================================

plot_main_GAM <- function(
    raw_data,
    prediction_data,
    model_id,
    vertical_anchor = NA_real_
) {
  
  if (model_id == "A") {
    
    title_lab <-
      "a  Economic development and N surplus intensity"
    
    x_raw <-
      "GDP_pc_constant2015USD"
    
    y_raw <-
      "N_surplus_per_protein_mass"
    
    x_lab <-
      "GDP per capita, constant 2015 US$"
    
    y_lab <-
      expression(
        "N surplus per unit protein supply (kg N kg"^-1*" protein)"
      )
    
  } else if (model_id == "B") {
    
    title_lab <-
      "b  Agricultural intensification and N surplus"
    
    x_raw <-
      "N_input_per_ha"
    
    y_raw <-
      "N_surplus"
    
    x_lab <-
      expression(
        "N input intensity (kg N ha"^-1*" yr"^-1*")"
      )
    
    y_lab <-
      expression(
        "Cropland N surplus (kg N ha"^-1*" yr"^-1*")"
      )
    
  } else {
    
    title_lab <-
      "c  Protein supply and N surplus"
    
    x_raw <-
      "Total_protein_supply"
    
    y_raw <-
      "N_surplus"
    
    x_lab <-
      expression(
        "Protein supply (g capita"^-1*" day"^-1*")"
      )
    
    y_lab <-
      expression(
        "Cropland N surplus (kg N ha"^-1*" yr"^-1*")"
      )
  }
  
  
  p <- ggplot() +
    
    geom_point(
      
      data =
        raw_data,
      
      aes(
        x =
          .data[[
            x_raw
          ]],
        y =
          .data[[
            y_raw
          ]]
      ),
      
      alpha =
        0.12,
      
      size =
        0.65,
      
      color =
        "grey45"
    ) +
    
    geom_ribbon(
      
      data =
        prediction_data,
      
      aes(
        x =
          x_plot,
        ymin =
          lower,
        ymax =
          upper
      ),
      
      fill =
        "grey80",
      
      alpha =
        0.65
    ) +
    
    geom_line(
      
      data =
        prediction_data,
      
      aes(
        x =
          x_plot,
        y =
          fit
      ),
      
      color =
        "black",
      
      linewidth =
        1.05
    ) +
    
    labs(
      title =
        title_lab,
      x =
        x_lab,
      y =
        y_lab
    ) +
    
    theme_nature()
  
  
  if (is.finite(vertical_anchor)) {
    
    p <- p +
      
      geom_vline(
        
        xintercept =
          vertical_anchor,
        
        linetype =
          "dashed",
        
        linewidth =
          0.65,
        
        color =
          "black"
      )
  }
  
  
  if (model_id == "A") {
    
    p <- p +
      
      scale_x_log10(
        labels =
          label_number(
            big.mark =
              ","
          )
      )
    
  } else if (model_id == "B") {
    
    p <- p +
      
      scale_x_continuous(
        
        trans =
          log1p_axis_trans,
        
        breaks =
          c(
            0,
            10,
            25,
            50,
            100,
            200,
            400,
            800
          ),
        
        labels =
          label_number(
            big.mark =
              ","
          )
      )
  }
  
  
  p
}

# ============================================================
# 28. MAIN FIGURE
# ============================================================

GDP_anchor_value <-
  GDP_terminal_anchor$
  GDP_terminal_significant_decline_start[
    1
  ]


Ninput_anchor_value <-
  Ninput_max_slope$
  N_input_intensity_at_maximum_first_derivative[
    1
  ]


p_main_A <- plot_main_GAM(
  
  raw_data =
    df_A,
  
  prediction_data =
    pred_A,
  
  model_id =
    "A",
  
  vertical_anchor =
    GDP_anchor_value
)


p_main_B <- plot_main_GAM(
  
  raw_data =
    df_B,
  
  prediction_data =
    pred_B,
  
  model_id =
    "B",
  
  vertical_anchor =
    Ninput_anchor_value
)


p_main_C <- plot_main_GAM(
  
  raw_data =
    df_C,
  
  prediction_data =
    pred_C,
  
  model_id =
    "C"
)


Figure1 <- (
  
  p_main_A |
    p_main_B |
    p_main_C
  
) 

print(
  Figure1
)


ggsave(
  file.path(
    out_plot_dir,
    "Figure1_GAM_relationships.png"
  ),
  Figure1,
  width = 14,
  height = 4.7,
  dpi = 600
)


ggsave(
  file.path(
    out_plot_dir,
    "Figure1_GAM_relationships.pdf"
  ),
  Figure1,
  width = 14,
  height = 4.7
)


# ============================================================
# 29. SUPPLEMENTARY FIRST-DERIVATIVE FIGURE
# ============================================================
#
# No vertical model-derived anchors are shown here.
# Panel letters are integrated directly into panel titles.
# ============================================================

plot_derivative <- function(
    derivative_data,
    model_id
) {
  
  if (model_id == "A") {
    
    title_lab <-
      "a  Economic development and N surplus intensity"
    
    x_lab <-
      "GDP per capita, constant 2015 US$"
    
  } else if (model_id == "B") {
    
    title_lab <-
      "b  Agricultural intensification and N surplus"
    
    x_lab <-
      expression(
        "N input intensity (kg N ha"^-1*" yr"^-1*")"
      )
    
  } else {
    
    title_lab <-
      "c  Protein supply and N surplus"
    
    x_lab <-
      expression(
        "Protein supply (g capita"^-1*" day"^-1*")"
      )
  }
  
  
  p <- ggplot(
    derivative_data,
    aes(
      x = x_plot,
      y = .derivative
    )
  ) +
    
    geom_ribbon(
      aes(
        ymin = .lower_ci,
        ymax = .upper_ci
      ),
      fill = "grey80",
      alpha = 0.65
    ) +
    
    geom_line(
      color = "black",
      linewidth = 0.9
    ) +
    
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.55
    ) +
    
    labs(
      title = title_lab,
      x = x_lab,
      y = "First derivative"
    ) +
    
    theme_nature()
  
  
  if (model_id == "A") {
    
    p <- p +
      
      scale_x_log10(
        labels =
          label_number(
            big.mark = ","
          )
      )
    
  } else if (model_id == "B") {
    
    p <- p +
      
      scale_x_continuous(
        trans = log1p_axis_trans,
        breaks = c(
          0,
          10,
          25,
          50,
          100,
          200,
          400,
          800
        ),
        labels =
          label_number(
            big.mark = ","
          )
      )
  }
  
  
  p
}


p_deriv_A <- plot_derivative(
  deriv_A,
  "A"
)

p_deriv_B <- plot_derivative(
  deriv_B,
  "B"
)

p_deriv_C <- plot_derivative(
  deriv_C,
  "C"
)


Supplementary_derivatives <- (
  p_deriv_A |
    p_deriv_B |
    p_deriv_C
)


print(
  Supplementary_derivatives
)


ggsave(
  file.path(
    supp_plot_dir,
    "Supplementary_Figure_derivatives.png"
  ),
  Supplementary_derivatives,
  width = 14,
  height = 4.5,
  dpi = 600
)


ggsave(
  file.path(
    supp_plot_dir,
    "Supplementary_Figure_derivatives.pdf"
  ),
  Supplementary_derivatives,
  width = 14,
  height = 4.5
)

# ============================================================
# 30. GENERIC SENSITIVITY FUNCTION
# ============================================================

run_sensitivity_fit <- function(
    data,
    model_id,
    scenario
) {
  
  model_data <- prepare_model_data(
    data,
    model_id
  )
  
  
  reml_model <- fit_gam_model(
    model_data,
    model_id,
    method =
      "REML"
  )
  
  
  stats <- extract_model_stats(
    reml_model,
    model_id
  ) %>%
    mutate(
      Scenario =
        scenario,
      .before =
        1
    )
  
  
  aic_comparison <- compare_gam_vs_linear(
    model_data,
    model_id
  ) %>%
    mutate(
      Scenario =
        scenario,
      .before =
        1
    )
  
  
  predictions <- predict_main_smooth(
    reml_model,
    model_data,
    model_id
  ) %>%
    mutate(
      Scenario =
        scenario,
      .before =
        1
    )
  
  
  derivative <- extract_derivative(
    reml_model,
    model_id
  ) %>%
    mutate(
      Scenario =
        scenario,
      .before =
        1
    )
  
  
  list(
    
    model =
      reml_model,
    
    data =
      model_data,
    
    statistics =
      stats,
    
    AIC_comparison =
      aic_comparison,
    
    predictions =
      predictions,
    
    derivatives =
      derivative
  )
}


# ============================================================
# 31. COUNTRY-EXCLUSION SENSITIVITY
# ============================================================

country_scenarios <- list(
  
  "All countries" =
    character(0),
  
  "Exclude China" =
    c(
      "CHN"
    ),
  
  "Exclude China + India" =
    c(
      "CHN",
      "IND"
    ),
  
  "Exclude China + India + USA" =
    c(
      "CHN",
      "IND",
      "USA"
    )
)


country_sensitivity_results <- list()


for (
  scenario_name in
  names(
    country_scenarios
  )
) {
  
  excluded_codes <-
    country_scenarios[[
      scenario_name
    ]]
  
  
  data_scenario <- df %>%
    filter(
      !ISO3 %in%
        excluded_codes
    )
  
  
  for (
    model_id in
    c(
      "A",
      "B",
      "C"
    )
  ) {
    
    result <- run_sensitivity_fit(
      
      data =
        data_scenario,
      
      model_id =
        model_id,
      
      scenario =
        scenario_name
    )
    
    
    country_sensitivity_results[[
      paste(
        scenario_name,
        model_id,
        sep = "__"
      )
    ]] <-
      result
  }
}


saveRDS(
  country_sensitivity_results,
  file.path(
    model_dir,
    "03_country_exclusion_sensitivity_models.rds"
  )
)


country_sensitivity_stats <- bind_rows(
  map(
    country_sensitivity_results,
    "statistics"
  )
)


country_sensitivity_AIC <- bind_rows(
  map(
    country_sensitivity_results,
    "AIC_comparison"
  )
)


country_sensitivity_predictions <- bind_rows(
  map(
    country_sensitivity_results,
    "predictions"
  )
)


country_sensitivity_derivatives <- bind_rows(
  map(
    country_sensitivity_results,
    "derivatives"
  )
)


write_csv(
  country_sensitivity_stats,
  file.path(
    supp_res_dir,
    "01_country_exclusion_GAM_statistics.csv"
  )
)


write_csv(
  country_sensitivity_AIC,
  file.path(
    supp_res_dir,
    "02_country_exclusion_GAM_vs_linear.csv"
  )
)


write_csv(
  country_sensitivity_predictions,
  file.path(
    supp_res_dir,
    "03_country_exclusion_predictions.csv"
  )
)


write_csv(
  country_sensitivity_derivatives,
  file.path(
    supp_res_dir,
    "04_country_exclusion_derivatives.csv"
  )
)


# ============================================================
# 32. COUNTRY-EXCLUSION SUPPLEMENTARY FIGURE
# ============================================================

country_scenario_levels <- c(
  "All countries",
  "Exclude China",
  "Exclude China + India",
  "Exclude China + India + USA"
)


country_sensitivity_predictions <-
  country_sensitivity_predictions %>%
  
  mutate(
    Scenario =
      factor(
        Scenario,
        levels = country_scenario_levels
      )
  )


# ------------------------------------------------------------
# Generic plotting function for sensitivity curves
# ------------------------------------------------------------

plot_sensitivity_curves <- function(
    predictions,
    model_id
) {
  
  dat <- predictions %>%
    filter(
      Model == model_id
    )
  
  
  if (model_id == "A") {
    
    title_lab <-
      "a  Economic development and N surplus intensity"
    
    x_lab <-
      "GDP per capita, constant 2015 US$"
    
    y_lab <-
      expression(
        "N surplus per unit protein supply (kg N kg protein"^-1*")"
      )
    
  } else if (model_id == "B") {
    
    title_lab <-
      "b  Agricultural intensification and N surplus"
    
    x_lab <-
      expression(
        "N input intensity (kg N ha"^-1*" yr"^-1*")"
      )
    
    y_lab <-
      expression(
        "Cropland N surplus (kg N ha"^-1*" yr"^-1*")"
      )
    
  } else {
    
    title_lab <-
      "c  Protein supply and N surplus"
    
    x_lab <-
      expression(
        "Protein supply (g capita"^-1*" day"^-1*")"
      )
    
    y_lab <-
      expression(
        "Cropland N surplus (kg N ha"^-1*" yr"^-1*")"
      )
  }
  
  
  p <- ggplot(
    dat,
    aes(
      x = x_plot,
      y = fit,
      linetype = Scenario
    )
  ) +
    
    geom_line(
      linewidth = 0.9
    ) +
    
    labs(
      title = title_lab,
      x = x_lab,
      y = y_lab,
      linetype = NULL
    ) +
    
    theme_nature()
  
  
  # ----------------------------------------------------------
  # Panel-specific x-axis formatting
  # ----------------------------------------------------------
  
  if (model_id == "A") {
    
    p <- p +
      
      scale_x_log10(
        labels =
          label_number(
            big.mark = ","
          )
      )
    
  } else if (model_id == "B") {
    
    p <- p +
      
      scale_x_continuous(
        trans = log1p_axis_trans,
        breaks = c(
          0,
          10,
          25,
          50,
          100,
          200,
          400,
          800
        ),
        labels =
          label_number(
            big.mark = ","
          )
      )
  }
  
  
  p
}


# ------------------------------------------------------------
# Build panels
# ------------------------------------------------------------

p_country_A <- plot_sensitivity_curves(
  country_sensitivity_predictions,
  "A"
)

p_country_B <- plot_sensitivity_curves(
  country_sensitivity_predictions,
  "B"
)

p_country_C <- plot_sensitivity_curves(
  country_sensitivity_predictions,
  "C"
)


# ------------------------------------------------------------
# Combine panels
# ------------------------------------------------------------

Supplementary_country_exclusion <- (
  
  p_country_A |
    p_country_B |
    p_country_C
  
) +
  
  plot_layout(
    guides = "collect"
  ) &
  
  theme(
    legend.position = "bottom"
  )


print(
  Supplementary_country_exclusion
)


# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

ggsave(
  file.path(
    supp_plot_dir,
    "Supplementary_Figure_country_exclusion_sensitivity.png"
  ),
  Supplementary_country_exclusion,
  width = 14,
  height = 4.8,
  dpi = 600
)


ggsave(
  file.path(
    supp_plot_dir,
    "Supplementary_Figure_country_exclusion_sensitivity.pdf"
  ),
  Supplementary_country_exclusion,
  width = 14,
  height = 4.8
)

# ============================================================
# 33. TEMPORAL SENSITIVITY
# ============================================================

temporal_scenarios <- list(
  
  "1992-2007" =
    c(
      1992,
      2007
    ),
  
  "2008-2022" =
    c(
      2008,
      2022
    )
)


temporal_sensitivity_results <- list()


for (
  scenario_name in
  names(
    temporal_scenarios
  )
) {
  
  year_range <-
    temporal_scenarios[[
      scenario_name
    ]]
  
  
  data_period <- df %>%
    filter(
      Year >=
        year_range[1],
      Year <=
        year_range[2]
    )
  
  
  for (
    model_id in
    c(
      "A",
      "B",
      "C"
    )
  ) {
    
    result <- run_sensitivity_fit(
      
      data =
        data_period,
      
      model_id =
        model_id,
      
      scenario =
        scenario_name
    )
    
    
    temporal_sensitivity_results[[
      paste(
        scenario_name,
        model_id,
        sep = "__"
      )
    ]] <-
      result
  }
}


saveRDS(
  temporal_sensitivity_results,
  file.path(
    model_dir,
    "04_temporal_sensitivity_models.rds"
  )
)


temporal_sensitivity_stats <- bind_rows(
  map(
    temporal_sensitivity_results,
    "statistics"
  )
)


temporal_sensitivity_AIC <- bind_rows(
  map(
    temporal_sensitivity_results,
    "AIC_comparison"
  )
)


temporal_sensitivity_predictions <- bind_rows(
  map(
    temporal_sensitivity_results,
    "predictions"
  )
)


temporal_sensitivity_derivatives <- bind_rows(
  map(
    temporal_sensitivity_results,
    "derivatives"
  )
)


write_csv(
  temporal_sensitivity_stats,
  file.path(
    supp_res_dir,
    "05_temporal_sensitivity_GAM_statistics.csv"
  )
)


write_csv(
  temporal_sensitivity_AIC,
  file.path(
    supp_res_dir,
    "06_temporal_sensitivity_GAM_vs_linear.csv"
  )
)


write_csv(
  temporal_sensitivity_predictions,
  file.path(
    supp_res_dir,
    "07_temporal_sensitivity_predictions.csv"
  )
)


write_csv(
  temporal_sensitivity_derivatives,
  file.path(
    supp_res_dir,
    "08_temporal_sensitivity_derivatives.csv"
  )
)


# ============================================================
# 34. TEMPORAL-SENSITIVITY SUPPLEMENTARY FIGURE
# ============================================================
#
# Models are fitted independently using all available observations
# within each temporal period.
#
# For visualization, fitted curves are compared only across the
# predictor range shared by both periods. This avoids interpreting
# differences caused simply by one period extending further into
# predictor space than the other.
# ============================================================


# ------------------------------------------------------------
# 34.1 Factor ordering
# ------------------------------------------------------------

temporal_sensitivity_predictions <-
  temporal_sensitivity_predictions %>%
  
  mutate(
    Scenario =
      factor(
        Scenario,
        levels = c(
          "1992-2007",
          "2008-2022"
        )
      )
  )


# ------------------------------------------------------------
# 34.2 Calculate predictor support in each temporal period
# ------------------------------------------------------------

temporal_support <- bind_rows(
  
  # ==========================================================
  # MODEL A
  # ==========================================================
  
  df %>%
    filter(
      Year >= 1992,
      Year <= 2007,
      is.finite(GDP_pc_constant2015USD),
      GDP_pc_constant2015USD > 0,
      is.finite(N_surplus_per_protein_mass)
    ) %>%
    summarise(
      Model = "A",
      Period = "1992-2007",
      xmin = min(
        GDP_pc_constant2015USD,
        na.rm = TRUE
      ),
      xmax = max(
        GDP_pc_constant2015USD,
        na.rm = TRUE
      )
    ),
  
  
  df %>%
    filter(
      Year >= 2008,
      Year <= 2022,
      is.finite(GDP_pc_constant2015USD),
      GDP_pc_constant2015USD > 0,
      is.finite(N_surplus_per_protein_mass)
    ) %>%
    summarise(
      Model = "A",
      Period = "2008-2022",
      xmin = min(
        GDP_pc_constant2015USD,
        na.rm = TRUE
      ),
      xmax = max(
        GDP_pc_constant2015USD,
        na.rm = TRUE
      )
    ),
  
  
  # ==========================================================
  # MODEL B
  # ==========================================================
  
  df %>%
    filter(
      Year >= 1992,
      Year <= 2007,
      is.finite(N_input_per_ha),
      N_input_per_ha >= 0,
      is.finite(log1p_N_surplus)
    ) %>%
    summarise(
      Model = "B",
      Period = "1992-2007",
      xmin = min(
        N_input_per_ha,
        na.rm = TRUE
      ),
      xmax = max(
        N_input_per_ha,
        na.rm = TRUE
      )
    ),
  
  
  df %>%
    filter(
      Year >= 2008,
      Year <= 2022,
      is.finite(N_input_per_ha),
      N_input_per_ha >= 0,
      is.finite(log1p_N_surplus)
    ) %>%
    summarise(
      Model = "B",
      Period = "2008-2022",
      xmin = min(
        N_input_per_ha,
        na.rm = TRUE
      ),
      xmax = max(
        N_input_per_ha,
        na.rm = TRUE
      )
    ),
  
  
  # ==========================================================
  # MODEL C
  # ==========================================================
  
  df %>%
    filter(
      Year >= 1992,
      Year <= 2007,
      is.finite(Total_protein_supply),
      is.finite(log1p_N_surplus)
    ) %>%
    summarise(
      Model = "C",
      Period = "1992-2007",
      xmin = min(
        Total_protein_supply,
        na.rm = TRUE
      ),
      xmax = max(
        Total_protein_supply,
        na.rm = TRUE
      )
    ),
  
  
  df %>%
    filter(
      Year >= 2008,
      Year <= 2022,
      is.finite(Total_protein_supply),
      is.finite(log1p_N_surplus)
    ) %>%
    summarise(
      Model = "C",
      Period = "2008-2022",
      xmin = min(
        Total_protein_supply,
        na.rm = TRUE
      ),
      xmax = max(
        Total_protein_supply,
        na.rm = TRUE
      )
    )
)


# ------------------------------------------------------------
# 34.3 Determine common support
# ------------------------------------------------------------

common_temporal_support <- temporal_support %>%
  
  group_by(
    Model
  ) %>%
  
  summarise(
    
    common_xmin =
      max(
        xmin
      ),
    
    common_xmax =
      min(
        xmax
      ),
    
    .groups =
      "drop"
  )


cat(
  "\n============================================================\n"
)

cat(
  "TEMPORAL PREDICTOR SUPPORT\n"
)

cat(
  "============================================================\n"
)

print(
  temporal_support,
  n = Inf,
  width = Inf
)


cat(
  "\n============================================================\n"
)

cat(
  "COMMON TEMPORAL PREDICTOR SUPPORT\n"
)

cat(
  "============================================================\n"
)

print(
  common_temporal_support,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# Save support information
# ------------------------------------------------------------

write_csv(
  temporal_support,
  file.path(
    supp_res_dir,
    "09_temporal_predictor_support.csv"
  )
)


write_csv(
  common_temporal_support,
  file.path(
    supp_res_dir,
    "10_temporal_common_predictor_support.csv"
  )
)


# ------------------------------------------------------------
# 34.4 Restrict predictions ONLY for visualization
# ------------------------------------------------------------

temporal_predictions_common <-
  temporal_sensitivity_predictions %>%
  
  left_join(
    common_temporal_support,
    by = "Model"
  ) %>%
  
  filter(
    x_plot >= common_xmin,
    x_plot <= common_xmax
  )


write_csv(
  temporal_predictions_common,
  file.path(
    supp_res_dir,
    "11_temporal_predictions_common_support.csv"
  )
)


# ------------------------------------------------------------
# 34.5 Build panels
#
# Uses the SAME plot_sensitivity_curves() function defined for
# the country-exclusion figure, therefore panel titles and axis
# formatting remain identical across supplementary figures.
# ------------------------------------------------------------

p_time_A <- plot_sensitivity_curves(
  temporal_predictions_common,
  "A"
)

p_time_B <- plot_sensitivity_curves(
  temporal_predictions_common,
  "B"
)

p_time_C <- plot_sensitivity_curves(
  temporal_predictions_common,
  "C"
)


# ------------------------------------------------------------
# 34.6 Combine panels
# ------------------------------------------------------------

Supplementary_temporal <- (
  
  p_time_A |
    p_time_B |
    p_time_C
  
) +
  
  plot_layout(
    guides = "collect"
  ) &
  
  theme(
    legend.position = "bottom"
  )


print(
  Supplementary_temporal
)


# ------------------------------------------------------------
# 34.7 Save
# ------------------------------------------------------------

ggsave(
  file.path(
    supp_plot_dir,
    "Supplementary_Figure_temporal_sensitivity.png"
  ),
  Supplementary_temporal,
  width = 14,
  height = 4.8,
  dpi = 600
)


ggsave(
  file.path(
    supp_plot_dir,
    "Supplementary_Figure_temporal_sensitivity.pdf"
  ),
  Supplementary_temporal,
  width = 14,
  height = 4.8
)

# ============================================================
# 35. MANUSCRIPT-READY ANCHORS
# ============================================================

main_stats_for_anchors <- main_model_stats %>%
  
  left_join(
    
    gam_vs_linear_main %>%
      select(
        Model,
        AIC_linear_ML,
        AIC_GAM_ML,
        delta_AIC_linear_minus_GAM,
        preferred_model
      ),
    
    by =
      "Model"
  )


manuscript_anchors <- bind_rows(
  
  main_stats_for_anchors %>%
    
    transmute(
      
      section =
        "Figure 1 GAM",
      
      result =
        paste0(
          "Model ",
          Model,
          " EDF"
        ),
      
      value =
        EDF,
      
      units =
        NA_character_,
      
      interpretation =
        "Effective degrees of freedom of focal smooth"
    ),
  
  
  main_stats_for_anchors %>%
    
    transmute(
      
      section =
        "Figure 1 GAM",
      
      result =
        paste0(
          "Model ",
          Model,
          " F"
        ),
      
      value =
        F,
      
      units =
        NA_character_,
      
      interpretation =
        "Approximate F statistic of focal smooth"
    ),
  
  
  main_stats_for_anchors %>%
    
    transmute(
      
      section =
        "Figure 1 GAM",
      
      result =
        paste0(
          "Model ",
          Model,
          " smooth P"
        ),
      
      value =
        P_smooth,
      
      units =
        NA_character_,
      
      interpretation =
        paste(
          "P value of focal smooth;",
          "not by itself a formal test of nonlinearity"
        )
    ),
  
  
  main_stats_for_anchors %>%
    
    transmute(
      
      section =
        "Figure 1 GAM",
      
      result =
        paste0(
          "Model ",
          Model,
          " adjusted R2"
        ),
      
      value =
        adjusted_R2,
      
      units =
        NA_character_,
      
      interpretation =
        "Adjusted R-squared"
    ),
  
  
  main_stats_for_anchors %>%
    
    transmute(
      
      section =
        "Figure 1 GAM",
      
      result =
        paste0(
          "Model ",
          Model,
          " deviance explained"
        ),
      
      value =
        deviance_explained,
      
      units =
        "proportion",
      
      interpretation =
        "Proportion of deviance explained"
    ),
  
  
  main_stats_for_anchors %>%
    
    transmute(
      
      section =
        "Figure 1 model comparison",
      
      result =
        paste0(
          "Model ",
          Model,
          " delta AIC linear minus GAM"
        ),
      
      value =
        delta_AIC_linear_minus_GAM,
      
      units =
        "AIC units",
      
      interpretation =
        "Positive values favour the GAM"
    ),
  
  
  tibble(
    
    section =
      "Figure 1 derivative anchor",
    
    result =
      "GDP terminal significant decline start",
    
    value =
      GDP_anchor_value,
    
    units =
      "constant 2015 US$ per capita",
    
    interpretation =
      paste(
        "Beginning of final derivative interval whose 95% CI",
        "remains below zero to the upper GDP boundary;",
        "descriptive model-derived anchor, not threshold"
      )
  ),
  
  
  tibble(
    
    section =
      "Figure 1 derivative anchor",
    
    result =
      "N input intensity at maximum first derivative",
    
    value =
      Ninput_anchor_value,
    
    units =
      "kg N ha-1 yr-1",
    
    interpretation =
      paste(
        "N-input value where first derivative is maximal;",
        "steepest fitted slope, not threshold or maximum acceleration"
      )
  )
)


write_csv(
  manuscript_anchors,
  file.path(
    out_res_dir,
    "manuscript_anchors.csv"
  )
)


# ============================================================
# 36. PACKAGE VERSIONS AND SESSION INFO
# ============================================================

package_versions <- tibble(
  
  package =
    required_packages,
  
  version =
    map_chr(
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
# 37. FINAL CONSOLE SUMMARY
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "FIGURE 1 - GAM RELATIONSHIPS\n"
)

cat(
  "============================================================\n\n"
)


cat(
  "--- INDICATOR RECONSTRUCTION QC ---\n"
)

print(
  qc_indicator_reconstruction
)


cat(
  "\n--- DATASET QC ---\n"
)

print(
  dataset_qc,
  n = Inf
)


cat(
  "\n--- MODEL SAMPLE SIZES ---\n"
)

print(
  sample_summary
)


cat(
  "\n--- MAIN GAM STATISTICS ---\n"
)

print(
  main_model_stats
)


cat(
  "\n--- GAM vs LINEAR: ML AIC COMPARISON ---\n"
)

print(
  gam_vs_linear_main
)


cat(
  "\n--- BASIS-DIMENSION CHECKS ---\n"
)

print(
  k_check_main
)


cat(
  "\n--- WITHIN-COUNTRY LAG-1 RESIDUAL CORRELATIONS ---\n"
)

print(
  lag1_main
)


cat(
  "\n--- MODEL A: GDP TERMINAL DECLINE ---\n"
)

print(
  GDP_terminal_anchor
)


cat(
  "\n--- MODEL A: RUN-LENGTH SENSITIVITY ---\n"
)

print(
  GDP_anchor_sensitivity
)


cat(
  "\n--- MODEL B: MAXIMUM FIRST DERIVATIVE ---\n"
)

print(
  Ninput_max_slope
)


cat(
  "\n--- COUNTRY-EXCLUSION SENSITIVITY ---\n"
)

print(
  country_sensitivity_stats,
  n = Inf
)


cat(
  "\n--- TEMPORAL SENSITIVITY ---\n"
)

print(
  temporal_sensitivity_stats,
  n = Inf
)


cat(
  "\n--- MANUSCRIPT ANCHORS ---\n"
)

print(
  manuscript_anchors,
  n = Inf
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
  "- The legacy N_surplus_per_protein variable is NOT used in any final GAM.\n"
)

cat(
  "- Model A uses N_surplus_per_protein_mass exclusively.\n"
)

cat(
  "- Main GAMs use REML.\n"
)

cat(
  "- GAM-vs-linear AIC comparisons use ML fits of both alternatives.\n"
)

cat(
  "- Smooth P values are not standalone tests of nonlinearity.\n"
)

cat(
  "- Curvature should be evaluated using EDF, model shape and GAM-vs-linear support jointly.\n"
)

cat(
  "- The GDP anchor is descriptive and model-derived, not a universal economic threshold.\n"
)

cat(
  "- The N-input anchor is the maximum FIRST derivative, i.e. the steepest fitted slope.\n"
)

cat(
  "- Do not describe the N-input anchor as maximum acceleration unless a second derivative is analysed.\n"
)

cat(
  "- Country exclusions are robustness analyses and are not population-weighting analyses.\n"
)

cat(
  "- Derivative supplementary panels intentionally contain no vertical anchors.\n"
)

cat(
  "- N surplus per unit protein supply is not a consumption-based nitrogen footprint.\n"
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
# END
# ============================================================