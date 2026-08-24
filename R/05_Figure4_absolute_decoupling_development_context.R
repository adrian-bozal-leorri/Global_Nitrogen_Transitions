# ============================================================
# 05_Figure4_absolute_decoupling_development_context.R
#
# Divergent nitrogen transition pathways during
# global agricultural development
#
# FIGURE 4
# Economic-development context and nitrogen-pressure
# trajectories of absolute decoupling
#
# ============================================================
#
# PURPOSE
# -------
# Examine:
#
# 1. The economic-development context of the first-to-last
#    transition outcomes defined in Script 03.
#
# 2. Whether absolute decouplers converge toward similarly low
#    final cropland N surplus.
#
# 3. Whether the magnitude of absolute cropland N-surplus
#    reduction varies across the economic-development gradient.
#
# 4. Whether the probability of absolute decoupling is associated
#    with median GDP per capita.
#
# ============================================================
#
# MAIN FIGURE 4
# -------------
#
# a  Economic context of transition outcomes
#
# b  Initial and final N surplus among absolute decouplers
#
# c  Magnitude of N-surplus reduction
#
#
# SUPPLEMENTARY FIGURE
# --------------------
#
# a  Economic-development level and change
#
# b  Probability of absolute decoupling
#
# ============================================================
#
# IMPORTANT ANALYTICAL DISTINCTIONS
# ---------------------------------
#
# FIRST-TO-LAST TRANSITION CLASSIFICATION
#
# - Loaded directly from Script 03.
# - NOT recalculated here.
# - Uses exact annual 1992 and 2022 endpoints.
#
#
# GDP CONTEXT
#
# GDP_median =
#   median annual GDP per capita across 1992-2022.
#
# It represents the economic-development context in which a
# national trajectory unfolded.
#
# It must NOT be interpreted as:
#
# - the GDP level at which decoupling occurred;
# - a causal threshold;
# - a within-country effect of increasing GDP.
#
#
# N-SURPLUS ENDPOINTS
#
# N_surplus_initial = exact annual 1992 value.
# N_surplus_final   = exact annual 2022 value.
#
# N_surplus_reduction =
#   N_surplus_initial - N_surplus_final
#
# Positive values therefore represent absolute reductions.
#
#
# STATISTICAL ANALYSES
# --------------------
#
# Across all countries:
#
#   logistic GLM:
#
#   absolute decoupling ~ log10(median GDP)
#
#
# Among absolute decouplers:
#
#   Spearman correlations:
#
#   median GDP vs initial N surplus
#   median GDP vs final N surplus
#   median GDP vs magnitude of N-surplus reduction
#
#
# Black/coloured lines in panels b/c are linear fits against
# log10-transformed median GDP and are used for visualization.
#
# Reported rho and P values correspond to Spearman correlations.
#
#
# NON-LINEARITY CHECK
# -------------------
#
# A quadratic logistic model is fitted only as a diagnostic:
#
#   absolute decoupling ~
#     log10(GDP) + log10(GDP)^2
#
# It is compared with the primary linear-logit specification
# using AIC, BIC and a likelihood-ratio test.
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
  "broom",
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
  library(broom)
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


classification_file <- file.path(
  results_dir,
  "03_Transition_classification",
  "country_transition_classification_5pct.rds"
)


out_res_dir <- file.path(
  results_dir,
  "05_Figure4_absolute_decoupling_development_context"
)


out_plot_dir <- file.path(
  plots_dir,
  "05_Figure4_absolute_decoupling_development_context"
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

expected_n_countries <- 130

expected_n_absolute_decouplers <- 46


# ============================================================
# 5. TRANSITION TERMINOLOGY
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
    "Absolute N reduction without protein growth",
  
  "inefficient_intensification" =
    "Inefficient intensification",
  
  "coupled_intensification" =
    "Coupled intensification",
  
  "other_or_no_clear_transition" =
    "Other / no clear transition"
)


transition_palette <- c(
  
  "Absolute decoupling" =
    "#0072B2",
  
  "Relative decoupling" =
    "#56B4E9",
  
  "Absolute N reduction without protein growth" =
    "#D55E00",
  
  "Inefficient intensification" =
    "#009E73",
  
  "Coupled intensification" =
    "#E69F00",
  
  "Other / no clear transition" =
    "grey60"
)


# ============================================================
# 6. INITIAL / FINAL PALETTE
# ============================================================

state_palette <- c(
  
  "Initial" =
    "grey45",
  
  "Final" =
    "#0072B2"
)


# ============================================================
# 7. HELPER FUNCTIONS
# ============================================================

format_p <- function(p) {
  
  if (
    is.na(p) ||
    !is.finite(p)
  ) {
    
    return(
      "P = NA"
    )
  }
  
  
  if (p < 0.0001) {
    
    return(
      "P < 0.0001"
    )
  }
  
  
  if (p < 0.001) {
    
    return(
      "P < 0.001"
    )
  }
  
  
  paste0(
    
    "P = ",
    
    formatC(
      p,
      format =
        "f",
      digits =
        3
    )
  )
}


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
          face =
            "plain",
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
      
      legend.title =
        element_blank(),
      
      legend.position =
        "bottom",
      
      plot.margin =
        margin(
          7,
          7,
          7,
          7
        )
    )
}


safe_quantile <- function(
    x,
    p
) {
  
  if (
    all(
      !is.finite(x)
    )
  ) {
    
    return(
      NA_real_
    )
  }
  
  quantile(
    x,
    probs =
      p,
    na.rm =
      TRUE,
    names =
      FALSE
  )
}


# ============================================================
# 8. INPUT CHECKS
# ============================================================

if (!file.exists(input_file)) {
  
  stop(
    "\nInput data file not found:\n",
    input_file
  )
}


if (!file.exists(classification_file)) {
  
  stop(
    "\nTransition classification RDS not found:\n",
    classification_file,
    "\n\nRun 03_Transition_classification.R first."
  )
}


# ============================================================
# 9. LOAD RAW DATA
# ============================================================

df <- readr::read_csv(
  input_file,
  show_col_types =
    FALSE
) %>%
  
  mutate(
    
    ISO3 =
      as.character(
        ISO3
      ),
    
    Country =
      as.character(
        Country
      ),
    
    Year =
      as.integer(
        Year
      ),
    
    GDP_pc_constant2015USD =
      as.numeric(
        GDP_pc_constant2015USD
      ),
    
    N_surplus =
      as.numeric(
        N_surplus
      )
  ) %>%
  
  arrange(
    ISO3,
    Year
  )


# ============================================================
# 10. LOAD DEFINITIVE SCRIPT 03 CLASSIFICATION
# ============================================================

classes <- readRDS(
  classification_file
) %>%
  
  mutate(
    
    ISO3 =
      as.character(
        ISO3
      ),
    
    Country =
      as.character(
        Country
      ),
    
    transition_class =
      as.character(
        transition_class
      )
  )


# ============================================================
# 11. CLASSIFICATION INPUT QC
# ============================================================

required_class_cols <- c(
  
  "ISO3",
  
  "Country",
  
  "transition_class",
  
  "absolute_decoupling"
)


missing_class_cols <- setdiff(
  required_class_cols,
  names(
    classes
  )
)


if (
  length(
    missing_class_cols
  ) > 0
) {
  
  stop(
    "Required columns missing from Script 03 classification: ",
    paste(
      missing_class_cols,
      collapse =
        ", "
    )
  )
}


if (
  anyDuplicated(
    classes$ISO3
  ) > 0
) {
  
  stop(
    "Duplicated ISO3 codes detected in Script 03 classification."
  )
}


if (
  nrow(
    classes
  ) !=
  expected_n_countries
) {
  
  stop(
    "Expected ",
    expected_n_countries,
    " classified countries but found ",
    nrow(
      classes
    ),
    "."
  )
}


unknown_classes <- setdiff(
  
  unique(
    classes$transition_class
  ),
  
  transition_order
)


if (
  length(
    unknown_classes
  ) > 0
) {
  
  stop(
    "Unexpected transition classes detected: ",
    paste(
      unknown_classes,
      collapse =
        ", "
    )
  )
}


# ============================================================
# 12. RAW DATA STRUCTURAL QC
# ============================================================

duplicate_country_years <- df %>%
  
  dplyr::count(
    
    ISO3,
    
    Year,
    
    name =
      "n"
  ) %>%
  
  dplyr::filter(
    n > 1
  )


if (
  nrow(
    duplicate_country_years
  ) > 0
) {
  
  write_csv(
    duplicate_country_years,
    file.path(
      out_res_dir,
      "00_duplicate_country_years.csv"
    )
  )
  
  stop(
    "Duplicated ISO3-Year observations detected."
  )
}


dataset_qc <- df %>%
  
  summarise(
    
    n_rows =
      n(),
    
    n_countries =
      n_distinct(
        ISO3
      ),
    
    first_year =
      min(
        Year,
        na.rm =
          TRUE
      ),
    
    last_year =
      max(
        Year,
        na.rm =
          TRUE
      ),
    
    missing_GDP =
      sum(
        !is.finite(
          GDP_pc_constant2015USD
        )
      ),
    
    nonpositive_GDP =
      sum(
        is.finite(
          GDP_pc_constant2015USD
        ) &
          GDP_pc_constant2015USD <=
          0,
        na.rm =
          TRUE
      ),
    
    missing_N_surplus =
      sum(
        !is.finite(
          N_surplus
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
# 13. EXACT ENDPOINT COVERAGE QC
# ============================================================

endpoint_coverage <- df %>%
  
  group_by(
    ISO3,
    Country
  ) %>%
  
  summarise(
    
    n_years =
      n_distinct(
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
    
    GDP_1992_available =
      any(
        Year ==
          study_start &
          is.finite(
            GDP_pc_constant2015USD
          ) &
          GDP_pc_constant2015USD >
          0
      ),
    
    GDP_2022_available =
      any(
        Year ==
          study_end &
          is.finite(
            GDP_pc_constant2015USD
          ) &
          GDP_pc_constant2015USD >
          0
      ),
    
    N_surplus_1992_available =
      any(
        Year ==
          study_start &
          is.finite(
            N_surplus
          )
      ),
    
    N_surplus_2022_available =
      any(
        Year ==
          study_end &
          is.finite(
            N_surplus
          )
      ),
    
    .groups =
      "drop"
  )


write_csv(
  endpoint_coverage,
  file.path(
    out_res_dir,
    "01_endpoint_coverage_QC.csv"
  )
)


if (
  any(
    !endpoint_coverage$has_1992 |
    !endpoint_coverage$has_2022
  )
) {
  
  stop(
    "At least one country lacks exact 1992 or 2022 observations."
  )
}


# ============================================================
# 14. COUNTRY-LEVEL GDP METRICS
# ============================================================
#
# Exact 1992 and 2022 values are used for GDP change.
#
# GDP_median is calculated across the complete study period and
# is the primary economic-context variable.
# ============================================================

gdp_country <- df %>%
  
  group_by(
    ISO3,
    Country
  ) %>%
  
  summarise(
    
    GDP_initial =
      GDP_pc_constant2015USD[
        Year ==
          study_start
      ][1],
    
    GDP_median =
      median(
        GDP_pc_constant2015USD[
          is.finite(
            GDP_pc_constant2015USD
          ) &
            GDP_pc_constant2015USD >
            0
        ],
        na.rm =
          TRUE
      ),
    
    GDP_final =
      GDP_pc_constant2015USD[
        Year ==
          study_end
      ][1],
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    
    log10_GDP_initial =
      if_else(
        is.finite(
          GDP_initial
        ) &
          GDP_initial >
          0,
        log10(
          GDP_initial
        ),
        NA_real_
      ),
    
    log10_GDP_median =
      if_else(
        is.finite(
          GDP_median
        ) &
          GDP_median >
          0,
        log10(
          GDP_median
        ),
        NA_real_
      ),
    
    log10_GDP_final =
      if_else(
        is.finite(
          GDP_final
        ) &
          GDP_final >
          0,
        log10(
          GDP_final
        ),
        NA_real_
      ),
    
    delta_log10_GDP =
      log10_GDP_final -
      log10_GDP_initial,
    
    GDP_fold_change =
      GDP_final /
      GDP_initial,
    
    GDP_pct_change =
      100 *
      (
        GDP_final -
          GDP_initial
      ) /
      GDP_initial
  )


write_csv(
  gdp_country,
  file.path(
    out_res_dir,
    "02_country_GDP_metrics.csv"
  )
)


# ============================================================
# 15. COUNTRY-LEVEL CROPLAND N-SURPLUS METRICS
# ============================================================
#
# IMPORTANT:
#
# Initial = exact annual 1992 value.
# Final   = exact annual 2022 value.
#
# Positive N_surplus_reduction means N surplus decreased.
# ============================================================

N_surplus_country <- df %>%
  
  group_by(
    ISO3,
    Country
  ) %>%
  
  summarise(
    
    N_surplus_initial =
      N_surplus[
        Year ==
          study_start
      ][1],
    
    N_surplus_median =
      median(
        N_surplus,
        na.rm =
          TRUE
      ),
    
    N_surplus_final =
      N_surplus[
        Year ==
          study_end
      ][1],
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    
    N_surplus_reduction =
      N_surplus_initial -
      N_surplus_final,
    
    delta_N_surplus =
      N_surplus_final -
      N_surplus_initial
  )


write_csv(
  N_surplus_country,
  file.path(
    out_res_dir,
    "03_country_N_surplus_metrics.csv"
  )
)


# ============================================================
# 16. BUILD MASTER COUNTRY TABLE
# ============================================================

country_master <- classes %>%
  
  select(
    ISO3,
    Country,
    everything()
  ) %>%
  
  left_join(
    
    gdp_country %>%
      select(
        -Country
      ),
    
    by =
      "ISO3"
  ) %>%
  
  left_join(
    
    N_surplus_country %>%
      select(
        -Country
      ),
    
    by =
      "ISO3"
  ) %>%
  
  mutate(
    
    transition_category =
      recode(
        transition_class,
        !!!transition_labels
      ),
    
    transition_category =
      factor(
        
        transition_category,
        
        levels =
          unname(
            transition_labels[
              transition_order
            ]
          )
      ),
    
    absolute_decoupling_binary =
      as.integer(
        transition_class ==
          "absolute_decoupling"
      )
  )


if (
  nrow(
    country_master
  ) !=
  expected_n_countries
) {
  
  stop(
    "Master country table does not contain exactly ",
    expected_n_countries,
    " countries."
  )
}


write_csv(
  country_master,
  file.path(
    out_res_dir,
    "04_country_transition_GDP_Nsurplus_master.csv"
  )
)


saveRDS(
  country_master,
  file.path(
    out_res_dir,
    "04_country_transition_GDP_Nsurplus_master.rds"
  )
)


# ============================================================
# 17. CLASSIFICATION + ANALYTICAL COVERAGE CHECK
# ============================================================

classification_check <- country_master %>%
  
  summarise(
    
    n_countries =
      n(),
    
    n_with_GDP_median =
      sum(
        is.finite(
          GDP_median
        )
      ),
    
    n_with_complete_GDP_endpoints =
      sum(
        is.finite(
          GDP_initial
        ) &
          is.finite(
            GDP_final
          )
      ),
    
    n_with_complete_N_surplus_endpoints =
      sum(
        is.finite(
          N_surplus_initial
        ) &
          is.finite(
            N_surplus_final
          )
      ),
    
    n_absolute_decoupling =
      sum(
        transition_class ==
          "absolute_decoupling"
      )
  )


write_csv(
  classification_check,
  file.path(
    out_res_dir,
    "05_classification_and_coverage_check.csv"
  )
)


if (
  classification_check$n_absolute_decoupling !=
  expected_n_absolute_decouplers
) {
  
  stop(
    "Expected ",
    expected_n_absolute_decouplers,
    " absolute decouplers but found ",
    classification_check$n_absolute_decoupling,
    "."
  )
}


# ============================================================
# 18. TRANSITION CATEGORY COUNTS
# ============================================================

transition_counts <- country_master %>%
  
  dplyr::count(
    
    transition_category,
    
    name =
      "n_countries",
    
    .drop =
      FALSE
  ) %>%
  
  mutate(
    
    percentage =
      100 *
      n_countries /
      sum(
        n_countries
      )
  )


write_csv(
  transition_counts,
  file.path(
    out_res_dir,
    "06_transition_category_counts.csv"
  )
)


# ============================================================
# 19. DESCRIPTIVE SUMMARY BY TRANSITION CATEGORY
# ============================================================

transition_summary <- country_master %>%
  
  group_by(
    transition_category
  ) %>%
  
  summarise(
    
    n =
      n(),
    
    median_GDP =
      median(
        GDP_median,
        na.rm =
          TRUE
      ),
    
    q25_GDP =
      safe_quantile(
        GDP_median,
        0.25
      ),
    
    q75_GDP =
      safe_quantile(
        GDP_median,
        0.75
      ),
    
    median_delta_log10_GDP =
      median(
        delta_log10_GDP,
        na.rm =
          TRUE
      ),
    
    median_N_surplus_initial =
      median(
        N_surplus_initial,
        na.rm =
          TRUE
      ),
    
    median_N_surplus_final =
      median(
        N_surplus_final,
        na.rm =
          TRUE
      ),
    
    median_N_surplus_reduction =
      median(
        N_surplus_reduction,
        na.rm =
          TRUE
      ),
    
    .groups =
      "drop"
  )


write_csv(
  transition_summary,
  file.path(
    out_res_dir,
    "07_transition_category_summary.csv"
  )
)


# ============================================================
# 20. PRIMARY LOGISTIC GLM
#     ABSOLUTE DECOUPLING ~ MEDIAN GDP
# ============================================================
#
# This is a cross-country association.
#
# It does NOT estimate a causal within-country GDP effect.
# ============================================================

glm_data <- country_master %>%
  
  filter(
    
    is.finite(
      log10_GDP_median
    ),
    
    !is.na(
      absolute_decoupling_binary
    )
  )


glm_absolute <- glm(
  
  absolute_decoupling_binary ~
    log10_GDP_median,
  
  data =
    glm_data,
  
  family =
    binomial(
      link =
        "logit"
    )
)


saveRDS(
  glm_absolute,
  file.path(
    out_res_dir,
    "08_GLM_absolute_decoupling_vs_GDP.rds"
  )
)


capture.output(
  
  summary(
    glm_absolute
  ),
  
  file =
    file.path(
      out_res_dir,
      "08_GLM_absolute_decoupling_summary.txt"
    )
)


glm_absolute_tidy <- broom::tidy(
  
  glm_absolute,
  
  conf.int =
    TRUE
) %>%
  
  mutate(
    
    odds_ratio =
      exp(
        estimate
      ),
    
    OR_conf_low =
      exp(
        conf.low
      ),
    
    OR_conf_high =
      exp(
        conf.high
      )
  )


write_csv(
  glm_absolute_tidy,
  file.path(
    out_res_dir,
    "08_GLM_absolute_decoupling_coefficients.csv"
  )
)


glm_absolute_fit <- broom::glance(
  glm_absolute
)


write_csv(
  glm_absolute_fit,
  file.path(
    out_res_dir,
    "08_GLM_absolute_decoupling_fit.csv"
  )
)


# ============================================================
# 21. ODDS RATIO PER DOUBLING OF MEDIAN GDP
# ============================================================

beta_GDP <- coef(
  glm_absolute
)[[
  "log10_GDP_median"
]]


se_GDP <- sqrt(
  
  vcov(
    glm_absolute
  )[
    "log10_GDP_median",
    "log10_GDP_median"
  ]
)


log10_two <- log10(
  2
)


logOR_double <-
  beta_GDP *
  log10_two


se_logOR_double <-
  se_GDP *
  log10_two


OR_doubling <- tibble(
  
  contrast =
    "Twofold increase in median GDP per capita",
  
  log_odds_change =
    logOR_double,
  
  SE =
    se_logOR_double,
  
  odds_ratio =
    exp(
      logOR_double
    ),
  
  OR_conf_low =
    exp(
      logOR_double -
        1.96 *
        se_logOR_double
    ),
  
  OR_conf_high =
    exp(
      logOR_double +
        1.96 *
        se_logOR_double
    )
)


write_csv(
  OR_doubling,
  file.path(
    out_res_dir,
    "09_GLM_OR_per_GDP_doubling.csv"
  )
)


# ============================================================
# 22. DIAGNOSTIC NON-LINEARITY CHECK
#     LINEAR VS QUADRATIC LOGISTIC GLM
# ============================================================

glm_quadratic <- glm(
  
  absolute_decoupling_binary ~
    log10_GDP_median +
    I(
      log10_GDP_median^2
    ),
  
  data =
    glm_data,
  
  family =
    binomial(
      link =
        "logit"
    )
)


saveRDS(
  glm_quadratic,
  file.path(
    out_res_dir,
    "10_GLM_quadratic_diagnostic.rds"
  )
)


glm_model_comparison <- bind_rows(
  
  broom::glance(
    glm_absolute
  ) %>%
    mutate(
      model =
        "linear_log10_GDP"
    ),
  
  broom::glance(
    glm_quadratic
  ) %>%
    mutate(
      model =
        "quadratic_log10_GDP"
    )
) %>%
  
  select(
    
    model,
    
    AIC,
    
    BIC,
    
    deviance,
    
    df.residual,
    
    nobs,
    
    everything()
  )


write_csv(
  glm_model_comparison,
  file.path(
    out_res_dir,
    "10_GLM_linear_vs_quadratic_fit.csv"
  )
)


lrt_linear_vs_quadratic <- anova(
  
  glm_absolute,
  
  glm_quadratic,
  
  test =
    "Chisq"
) %>%
  
  as.data.frame() %>%
  
  tibble::rownames_to_column(
    "model_step"
  ) %>%
  
  tibble::as_tibble()


write_csv(
  lrt_linear_vs_quadratic,
  file.path(
    out_res_dir,
    "10_GLM_linear_vs_quadratic_LRT.csv"
  )
)


# ============================================================
# 23. LOGISTIC GLM PREDICTIONS
# ============================================================

GDP_grid <- tibble(
  
  GDP_median =
    exp(
      
      seq(
        
        log(
          min(
            glm_data$GDP_median,
            na.rm =
              TRUE
          )
        ),
        
        log(
          max(
            glm_data$GDP_median,
            na.rm =
              TRUE
          )
        ),
        
        length.out =
          400
      )
    )
) %>%
  
  mutate(
    
    log10_GDP_median =
      log10(
        GDP_median
      )
  )


pred_glm <- predict(
  
  glm_absolute,
  
  newdata =
    GDP_grid,
  
  type =
    "link",
  
  se.fit =
    TRUE
)


glm_predictions <- GDP_grid %>%
  
  mutate(
    
    fit_link =
      as.numeric(
        pred_glm$fit
      ),
    
    se_link =
      as.numeric(
        pred_glm$se.fit
      ),
    
    probability =
      plogis(
        fit_link
      ),
    
    lower =
      plogis(
        fit_link -
          1.96 *
          se_link
      ),
    
    upper =
      plogis(
        fit_link +
          1.96 *
          se_link
      )
  )


write_csv(
  glm_predictions,
  file.path(
    out_res_dir,
    "11_GLM_absolute_decoupling_predictions.csv"
  )
)


# ============================================================
# 24. REFERENCE GDP PREDICTIONS
# ============================================================

reference_GDP <- tibble(
  
  GDP_median =
    c(
      2000,
      5000,
      10000,
      20000,
      50000
    )
) %>%
  
  mutate(
    
    log10_GDP_median =
      log10(
        GDP_median
      )
  )


reference_pred <- predict(
  
  glm_absolute,
  
  newdata =
    reference_GDP,
  
  type =
    "link",
  
  se.fit =
    TRUE
)


reference_predictions <- reference_GDP %>%
  
  mutate(
    
    fit_link =
      as.numeric(
        reference_pred$fit
      ),
    
    se_link =
      as.numeric(
        reference_pred$se.fit
      ),
    
    probability =
      plogis(
        fit_link
      ),
    
    lower =
      plogis(
        fit_link -
          1.96 *
          se_link
      ),
    
    upper =
      plogis(
        fit_link +
          1.96 *
          se_link
      )
  )


write_csv(
  reference_predictions,
  file.path(
    out_res_dir,
    "11_probability_reference_GDP.csv"
  )
)


# ============================================================
# 25. ABSOLUTE-DECOUPLER SUBSET
# ============================================================

abs_dec <- country_master %>%
  
  filter(
    
    transition_class ==
      "absolute_decoupling",
    
    is.finite(
      GDP_median
    ),
    
    GDP_median >
      0,
    
    is.finite(
      N_surplus_initial
    ),
    
    is.finite(
      N_surplus_final
    ),
    
    is.finite(
      N_surplus_reduction
    )
  )


if (
  nrow(
    abs_dec
  ) !=
  expected_n_absolute_decouplers
) {
  
  stop(
    "Expected ",
    expected_n_absolute_decouplers,
    " complete absolute decouplers but found ",
    nrow(
      abs_dec
    ),
    "."
  )
}


write_csv(
  abs_dec,
  file.path(
    out_res_dir,
    "12_absolute_decoupler_country_data.csv"
  )
)


# ============================================================
# 26. SPEARMAN: GDP VS INITIAL N SURPLUS
# ============================================================

cor_initial <- cor.test(
  
  abs_dec$GDP_median,
  
  abs_dec$N_surplus_initial,
  
  method =
    "spearman",
  
  exact =
    FALSE
)


lm_initial <- lm(
  
  N_surplus_initial ~
    log10_GDP_median,
  
  data =
    abs_dec
)


# ============================================================
# 27. SPEARMAN: GDP VS FINAL N SURPLUS
# ============================================================

cor_final <- cor.test(
  
  abs_dec$GDP_median,
  
  abs_dec$N_surplus_final,
  
  method =
    "spearman",
  
  exact =
    FALSE
)


lm_final <- lm(
  
  N_surplus_final ~
    log10_GDP_median,
  
  data =
    abs_dec
)


# ============================================================
# 28. SPEARMAN: GDP VS MAGNITUDE OF N-SURPLUS REDUCTION
# ============================================================

cor_reduction <- cor.test(
  
  abs_dec$GDP_median,
  
  abs_dec$N_surplus_reduction,
  
  method =
    "spearman",
  
  exact =
    FALSE
)


lm_reduction <- lm(
  
  N_surplus_reduction ~
    log10_GDP_median,
  
  data =
    abs_dec
)


# ============================================================
# 29. ABSOLUTE-DECOUPLER DIAGNOSTIC TABLE
# ============================================================

absolute_decoupler_diagnostics <- tibble(
  
  relationship = c(
    
    "GDP vs initial N surplus",
    
    "GDP vs final N surplus",
    
    "GDP vs N surplus reduction"
  ),
  
  n = c(
    nrow(
      abs_dec
    ),
    nrow(
      abs_dec
    ),
    nrow(
      abs_dec
    )
  ),
  
  spearman_rho = c(
    
    unname(
      cor_initial$estimate
    ),
    
    unname(
      cor_final$estimate
    ),
    
    unname(
      cor_reduction$estimate
    )
  ),
  
  spearman_p = c(
    
    cor_initial$p.value,
    
    cor_final$p.value,
    
    cor_reduction$p.value
  ),
  
  lm_beta_log10GDP = c(
    
    coef(
      lm_initial
    )[[
      "log10_GDP_median"
    ]],
    
    coef(
      lm_final
    )[[
      "log10_GDP_median"
    ]],
    
    coef(
      lm_reduction
    )[[
      "log10_GDP_median"
    ]]
  ),
  
  lm_p = c(
    
    summary(
      lm_initial
    )$coefficients[
      "log10_GDP_median",
      "Pr(>|t|)"
    ],
    
    summary(
      lm_final
    )$coefficients[
      "log10_GDP_median",
      "Pr(>|t|)"
    ],
    
    summary(
      lm_reduction
    )$coefficients[
      "log10_GDP_median",
      "Pr(>|t|)"
    ]
  ),
  
  lm_R2 = c(
    
    summary(
      lm_initial
    )$r.squared,
    
    summary(
      lm_final
    )$r.squared,
    
    summary(
      lm_reduction
    )$r.squared
  )
)


write_csv(
  absolute_decoupler_diagnostics,
  file.path(
    out_res_dir,
    "13_absolute_decoupler_GDP_diagnostics.csv"
  )
)


# ============================================================
# 30. ABSOLUTE-DECOUPLER SUMMARY
# ============================================================

absolute_decoupler_summary <- abs_dec %>%
  
  summarise(
    
    n =
      n(),
    
    median_GDP =
      median(
        GDP_median,
        na.rm =
          TRUE
      ),
    
    q25_GDP =
      safe_quantile(
        GDP_median,
        0.25
      ),
    
    q75_GDP =
      safe_quantile(
        GDP_median,
        0.75
      ),
    
    median_N_surplus_initial =
      median(
        N_surplus_initial,
        na.rm =
          TRUE
      ),
    
    q25_N_surplus_initial =
      safe_quantile(
        N_surplus_initial,
        0.25
      ),
    
    q75_N_surplus_initial =
      safe_quantile(
        N_surplus_initial,
        0.75
      ),
    
    median_N_surplus_final =
      median(
        N_surplus_final,
        na.rm =
          TRUE
      ),
    
    q25_N_surplus_final =
      safe_quantile(
        N_surplus_final,
        0.25
      ),
    
    q75_N_surplus_final =
      safe_quantile(
        N_surplus_final,
        0.75
      ),
    
    median_N_surplus_reduction =
      median(
        N_surplus_reduction,
        na.rm =
          TRUE
      ),
    
    q25_N_surplus_reduction =
      safe_quantile(
        N_surplus_reduction,
        0.25
      ),
    
    q75_N_surplus_reduction =
      safe_quantile(
        N_surplus_reduction,
        0.75
      )
  )


write_csv(
  absolute_decoupler_summary,
  file.path(
    out_res_dir,
    "14_absolute_decoupler_summary.csv"
  )
)


# ============================================================
# 31. STATISTICAL LABELS
# ============================================================

rho_initial <- unname(
  cor_initial$estimate
)


p_initial <- cor_initial$p.value


rho_final <- unname(
  cor_final$estimate
)


p_final <- cor_final$p.value


rho_reduction <- unname(
  cor_reduction$estimate
)


p_reduction <- cor_reduction$p.value


glm_GDP_p <- summary(
  glm_absolute
)$coefficients[
  "log10_GDP_median",
  "Pr(>|z|)"
]


label_panel_b <- paste0(
  
  "Initial: Spearman \u03c1 = ",
  sprintf(
    "%.2f",
    rho_initial
  ),
  ", ",
  format_p(
    p_initial
  ),
  "\n",
  
  "Final: Spearman \u03c1 = ",
  sprintf(
    "%.2f",
    rho_final
  ),
  ", ",
  format_p(
    p_final
  )
)


label_panel_c <- paste0(
  
  "Spearman \u03c1 = ",
  sprintf(
    "%.2f",
    rho_reduction
  ),
  ", ",
  format_p(
    p_reduction
  )
)


label_glm <- format_p(
  glm_GDP_p
)


# ============================================================
# 32. MAIN FIGURE PANEL A DATA
# ============================================================

panel_a_data <- country_master %>%
  
  filter(
    
    is.finite(
      GDP_median
    ),
    
    GDP_median >
      0,
    
    !is.na(
      transition_category
    )
  )


# ============================================================
# 33. MAIN FIGURE PANEL A
#     ECONOMIC CONTEXT OF TRANSITION OUTCOMES
# ============================================================

p4a <- ggplot(
  
  panel_a_data,
  
  aes(
    
    x =
      transition_category,
    
    y =
      GDP_median
  )
  
) +
  
  geom_boxplot(
    
    width =
      0.58,
    
    outlier.shape =
      NA,
    
    fill =
      "grey95",
    
    color =
      "black",
    
    linewidth =
      0.5
  ) +
  
  geom_jitter(
    
    aes(
      color =
        transition_category
    ),
    
    width =
      0.14,
    
    height =
      0,
    
    alpha =
      0.68,
    
    size =
      1.8,
    
    show.legend =
      FALSE
  ) +
  
  scale_color_manual(
    
    values =
      transition_palette,
    
    drop =
      FALSE
  ) +
  
  scale_y_log10(
    
    labels =
      label_number(
        big.mark =
          ",",
        accuracy =
          1
      )
  ) +
  
  labs(
    
    title =
      "a  Economic context of transition outcomes",
    
    x =
      NULL,
    
    y =
      "Median GDP per capita, constant 2015 US$"
  ) +
  
  theme_nature() +
  
  theme(
    
    axis.text.x =
      element_text(
        angle =
          32,
        hjust =
          1,
        vjust =
          1,
        size =
          8
      )
  )


# ============================================================
# 34. PREPARE INITIAL / FINAL LONG DATA
# ============================================================

abs_dec_long <- abs_dec %>%
  
  select(
    
    ISO3,
    
    Country,
    
    GDP_median,
    
    N_surplus_initial,
    
    N_surplus_final
  ) %>%
  
  pivot_longer(
    
    cols =
      c(
        N_surplus_initial,
        N_surplus_final
      ),
    
    names_to =
      "state",
    
    values_to =
      "N_surplus"
  ) %>%
  
  mutate(
    
    state =
      recode(
        
        state,
        
        "N_surplus_initial" =
          "Initial",
        
        "N_surplus_final" =
          "Final"
      ),
    
    state =
      factor(
        
        state,
        
        levels =
          c(
            "Initial",
            "Final"
          )
      )
  )


# ============================================================
# 35. PANEL B ANNOTATION POSITION
# ============================================================

x_b_text <-
  
  min(
    abs_dec$GDP_median,
    na.rm =
      TRUE
  ) *
  1.08


y_b_text <-
  
  max(
    c(
      abs_dec$N_surplus_initial,
      abs_dec$N_surplus_final
    ),
    na.rm =
      TRUE
  ) *
  0.97


# ============================================================
# 36. MAIN FIGURE PANEL B
#     INITIAL AND FINAL N SURPLUS
# ============================================================

p4b <- ggplot() +
  
  # ----------------------------------------------------------
# Country-specific connection between 1992 and 2022
# ----------------------------------------------------------

geom_segment(
  
  data =
    abs_dec,
  
  aes(
    
    x =
      GDP_median,
    
    xend =
      GDP_median,
    
    y =
      N_surplus_initial,
    
    yend =
      N_surplus_final
  ),
  
  color =
    "grey78",
  
  linewidth =
    0.38,
  
  alpha =
    0.65
) +
  
  # ----------------------------------------------------------
# Initial/final observations
# ----------------------------------------------------------

geom_point(
  
  data =
    abs_dec_long,
  
  aes(
    
    x =
      GDP_median,
    
    y =
      N_surplus,
    
    color =
      state
  ),
  
  size =
    2.2,
  
  alpha =
    0.86
) +
  
  # ----------------------------------------------------------
# Initial/final relationships
#
# Grouping by state is automatic through color/fill.
# ----------------------------------------------------------

geom_smooth(
  
  data =
    abs_dec_long,
  
  aes(
    
    x =
      GDP_median,
    
    y =
      N_surplus,
    
    color =
      state,
    
    fill =
      state
  ),
  
  method =
    "lm",
  
  formula =
    y ~ log10(x),
  
  se =
    TRUE,
  
  linewidth =
    0.9,
  
  alpha =
    0.13
) +
  
  # ----------------------------------------------------------
# Spearman statistics
# ----------------------------------------------------------

annotate(
  
  "text",
  
  x =
    x_b_text,
  
  y =
    y_b_text,
  
  label =
    label_panel_b,
  
  hjust =
    0,
  
  vjust =
    1,
  
  size =
    3,
  
  lineheight =
    1.08
) +
  
  scale_x_log10(
    
    labels =
      label_number(
        big.mark =
          ",",
        accuracy =
          1
      ),
    
    expand =
      expansion(
        mult =
          c(
            0.03,
            0.08
          )
      )
  ) +
  
  scale_color_manual(
    
    values =
      state_palette,
    
    drop =
      FALSE
  ) +
  
  scale_fill_manual(
    
    values =
      state_palette,
    
    drop =
      FALSE,
    
    guide =
      "none"
  ) +
  
  labs(
    
    title =
      "b  Initial and final N surplus among absolute decouplers",
    
    x =
      "Median GDP per capita, constant 2015 US$",
    
    y =
      expression(
        "Cropland N surplus (kg N ha"^-1*" yr"^-1*")"
      ),
    
    color =
      NULL
  ) +
  
  theme_nature() +
  
  theme(
    
    legend.position =
      "bottom",
    
    legend.direction =
      "horizontal",
    
    legend.justification =
      "center"
  )


# ============================================================
# 37. PANEL C ANNOTATION POSITION
# ============================================================

x_c_text <-
  
  min(
    abs_dec$GDP_median,
    na.rm =
      TRUE
  ) *
  1.08


y_c_text <-
  
  max(
    abs_dec$N_surplus_reduction,
    na.rm =
      TRUE
  ) *
  0.97


# ============================================================
# 38. MAIN FIGURE PANEL C
#     MAGNITUDE OF N-SURPLUS REDUCTION
# ============================================================

p4c <- ggplot(
  
  abs_dec,
  
  aes(
    
    x =
      GDP_median,
    
    y =
      N_surplus_reduction
  )
  
) +
  
  geom_hline(
    
    yintercept =
      0,
    
    linetype =
      "dashed",
    
    color =
      "grey55",
    
    linewidth =
      0.45
  ) +
  
  geom_point(
    
    size =
      2.2,
    
    alpha =
      0.72
  ) +
  
  geom_smooth(
    
    method =
      "lm",
    
    formula =
      y ~ log10(x),
    
    se =
      TRUE,
    
    color =
      "black",
    
    fill =
      "grey78",
    
    linewidth =
      0.95
  ) +
  
  annotate(
    
    "text",
    
    x =
      x_c_text,
    
    y =
      y_c_text,
    
    label =
      label_panel_c,
    
    hjust =
      0,
    
    vjust =
      1,
    
    size =
      3
  ) +
  
  scale_x_log10(
    
    labels =
      label_number(
        big.mark =
          ",",
        accuracy =
          1
      ),
    
    expand =
      expansion(
        mult =
          c(
            0.03,
            0.08
          )
      )
  ) +
  
  labs(
    
    title =
      "c  Magnitude of N-surplus reduction",
    
    x =
      "Median GDP per capita, constant 2015 US$",
    
    y =
      expression(
        "N surplus reduction (1992 - 2022; kg N ha"^-1*" yr"^-1*")"
      )
  ) +
  
  theme_nature()


# ============================================================
# 39. ASSEMBLE MAIN FIGURE 4
# ============================================================

Figure4 <- (
  
  p4a |
    
    p4b |
    
    p4c
  
) +
  
  plot_layout(
    
    widths =
      c(
        1,
        1,
        1
      )
  )


print(
  Figure4
)


# ============================================================
# 40. SAVE MAIN FIGURE 4
# ============================================================

ggsave(
  
  file.path(
    out_plot_dir,
    "Figure4_absolute_decoupling_development_context.png"
  ),
  
  Figure4,
  
  width =
    16,
  
  height =
    5.7,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Figure4_absolute_decoupling_development_context.pdf"
  ),
  
  Figure4,
  
  width =
    16,
  
  height =
    5.7
)


# ============================================================
# 41. SAVE INDIVIDUAL MAIN PANELS
# ============================================================

ggsave(
  
  file.path(
    out_plot_dir,
    "Figure4a_economic_context_transition_outcomes.png"
  ),
  
  p4a,
  
  width =
    6,
  
  height =
    5,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Figure4b_initial_final_N_surplus_absolute_decouplers.png"
  ),
  
  p4b,
  
  width =
    6.5,
  
  height =
    5,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Figure4c_magnitude_N_surplus_reduction.png"
  ),
  
  p4c,
  
  width =
    6.5,
  
  height =
    5,
  
  dpi =
    600
)


# ============================================================
# 42. SUPPLEMENTARY PANEL A
#     ECONOMIC-DEVELOPMENT LEVEL AND CHANGE
# ============================================================

pS_a <- country_master %>%
  
  filter(
    
    is.finite(
      GDP_median
    ),
    
    GDP_median >
      0,
    
    is.finite(
      delta_log10_GDP
    ),
    
    !is.na(
      transition_category
    )
  ) %>%
  
  ggplot(
    
    aes(
      
      x =
        GDP_median,
      
      y =
        delta_log10_GDP,
      
      color =
        transition_category
    )
    
  ) +
  
  geom_hline(
    
    yintercept =
      0,
    
    linetype =
      "dashed",
    
    color =
      "grey50",
    
    linewidth =
      0.45
  ) +
  
  geom_point(
    
    size =
      2,
    
    alpha =
      0.75
  ) +
  
  scale_x_log10(
    
    labels =
      label_number(
        big.mark =
          ",",
        accuracy =
          1
      ),
    
    expand =
      expansion(
        mult =
          c(
            0.03,
            0.08
          )
      )
  ) +
  
  scale_color_manual(
    
    values =
      transition_palette,
    
    drop =
      FALSE
  ) +
  
  labs(
    
    title =
      "a  Economic-development level and change",
    
    x =
      "Median GDP per capita, constant 2015 US$",
    
    y =
      expression(
        Delta*"log"[10]*"(GDP per capita)"
      ),
    
    color =
      NULL
  ) +
  
  theme_nature() +
  
  theme(
    
    legend.position =
      "bottom",
    
    legend.text =
      element_text(
        size =
          7.8
      )
  )


# ============================================================
# 43. SUPPLEMENTARY PANEL B
#     PROBABILITY OF ABSOLUTE DECOUPLING
# ============================================================
#
# No jittered binary observations are displayed.
#
# The curve and 95% CI communicate the model result more cleanly.
# ============================================================

pS_b <- ggplot(
  
  glm_predictions,
  
  aes(
    x =
      GDP_median
  )
  
) +
  
  geom_ribbon(
    
    aes(
      
      ymin =
        lower,
      
      ymax =
        upper
    ),
    
    fill =
      "grey78",
    
    alpha =
      0.55
  ) +
  
  geom_line(
    
    aes(
      y =
        probability
    ),
    
    color =
      "black",
    
    linewidth =
      1.05
  ) +
  
  annotate(
    
    "text",
    
    x =
      -Inf,
    
    y =
      Inf,
    
    label =
      label_glm,
    
    hjust =
      -0.10,
    
    vjust =
      1.35,
    
    size =
      3.5
  ) +
  
  scale_x_log10(
    
    labels =
      label_number(
        big.mark =
          ",",
        accuracy =
          1
      ),
    
    expand =
      expansion(
        mult =
          c(
            0.03,
            0.08
          )
      )
  ) +
  
  scale_y_continuous(
    
    limits =
      c(
        0,
        0.75
      ),
    
    breaks =
      seq(
        0,
        0.75,
        by =
          0.15
      ),
    
    labels =
      label_percent(
        accuracy =
          1
      )
  ) +
  
  labs(
    
    title =
      "b  Probability of absolute decoupling",
    
    x =
      "Median GDP per capita, constant 2015 US$",
    
    y =
      "Estimated probability of absolute decoupling"
  ) +
  
  theme_nature()


# ============================================================
# 44. ASSEMBLE SUPPLEMENTARY FIGURE
# ============================================================

Supplementary_development_context <- (
  
  pS_a |
    
    pS_b
  
) +
  
  plot_layout(
    
    widths =
      c(
        1,
        1
      ),
    
    guides =
      "collect"
  ) &
  
  theme(
    
    legend.position =
      "bottom"
  )


print(
  Supplementary_development_context
)


# ============================================================
# 45. SAVE SUPPLEMENTARY FIGURE
# ============================================================

ggsave(
  
  file.path(
    out_plot_dir,
    "Supplementary_development_context_absolute_decoupling.png"
  ),
  
  Supplementary_development_context,
  
  width =
    11.5,
  
  height =
    5.3,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Supplementary_development_context_absolute_decoupling.pdf"
  ),
  
  Supplementary_development_context,
  
  width =
    11.5,
  
  height =
    5.3
)


# ============================================================
# 46. SAVE INDIVIDUAL SUPPLEMENTARY PANELS
# ============================================================

ggsave(
  
  file.path(
    out_plot_dir,
    "Supplementary_economic_development_level_and_change.png"
  ),
  
  pS_a,
  
  width =
    6,
  
  height =
    5,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Supplementary_probability_absolute_decoupling.png"
  ),
  
  pS_b,
  
  width =
    6,
  
  height =
    5,
  
  dpi =
    600
)


# ============================================================
# 47. MANUSCRIPT-READY STATISTICAL ANCHORS
# ============================================================

glm_gdp_row <- glm_absolute_tidy %>%
  
  filter(
    term ==
      "log10_GDP_median"
  )


manuscript_anchors <- tibble(
  
  result = c(
    
    "Countries classified as absolute decoupling",
    
    "Percentage classified as absolute decoupling",
    
    "GLM coefficient: absolute decoupling vs log10 median GDP",
    
    "GLM P value: absolute decoupling vs log10 median GDP",
    
    "Odds ratio per doubling of median GDP",
    
    "Odds ratio lower 95% CI per GDP doubling",
    
    "Odds ratio upper 95% CI per GDP doubling",
    
    "Absolute decouplers: GDP vs initial N surplus Spearman rho",
    
    "Absolute decouplers: GDP vs initial N surplus P",
    
    "Absolute decouplers: GDP vs final N surplus Spearman rho",
    
    "Absolute decouplers: GDP vs final N surplus P",
    
    "Absolute decouplers: GDP vs N-surplus reduction Spearman rho",
    
    "Absolute decouplers: GDP vs N-surplus reduction P"
  ),
  
  value = c(
    
    sum(
      country_master$absolute_decoupling_binary,
      na.rm =
        TRUE
    ),
    
    100 *
      mean(
        country_master$absolute_decoupling_binary,
        na.rm =
          TRUE
      ),
    
    glm_gdp_row$estimate,
    
    glm_gdp_row$p.value,
    
    OR_doubling$odds_ratio,
    
    OR_doubling$OR_conf_low,
    
    OR_doubling$OR_conf_high,
    
    rho_initial,
    
    p_initial,
    
    rho_final,
    
    p_final,
    
    rho_reduction,
    
    p_reduction
  ),
  
  units = c(
    
    "countries",
    
    "%",
    
    "log-odds per 10-fold GDP increase",
    
    "P value",
    
    "odds ratio",
    
    "odds ratio",
    
    "odds ratio",
    
    "Spearman rho",
    
    "P value",
    
    "Spearman rho",
    
    "P value",
    
    "Spearman rho",
    
    "P value"
  )
)


write_csv(
  manuscript_anchors,
  file.path(
    out_res_dir,
    "15_manuscript_ready_statistical_anchors.csv"
  )
)


# ============================================================
# 48. HUMAN-READABLE MANUSCRIPT ANCHORS
# ============================================================

manuscript_anchors_readable <- tibble(
  
  result = c(
    
    "Absolute decoupling",
    
    "GDP association with absolute decoupling",
    
    "OR per doubling of median GDP",
    
    "GDP vs initial N surplus among absolute decouplers",
    
    "GDP vs final N surplus among absolute decouplers",
    
    "GDP vs magnitude of N-surplus reduction among absolute decouplers"
  ),
  
  result_text = c(
    
    paste0(
      
      sum(
        country_master$absolute_decoupling_binary
      ),
      
      " / ",
      
      nrow(
        country_master
      ),
      
      " (",
      
      round(
        
        100 *
          mean(
            country_master$absolute_decoupling_binary
          ),
        
        1
      ),
      
      "%)"
    ),
    
    
    paste0(
      
      "beta = ",
      
      round(
        glm_gdp_row$estimate,
        3
      ),
      
      "; ",
      
      format_p(
        glm_gdp_row$p.value
      )
    ),
    
    
    paste0(
      
      "OR = ",
      
      round(
        OR_doubling$odds_ratio,
        3
      ),
      
      " (95% CI ",
      
      round(
        OR_doubling$OR_conf_low,
        3
      ),
      
      "-",
      
      round(
        OR_doubling$OR_conf_high,
        3
      ),
      
      ")"
    ),
    
    
    paste0(
      
      "rho = ",
      
      round(
        rho_initial,
        3
      ),
      
      "; ",
      
      format_p(
        p_initial
      )
    ),
    
    
    paste0(
      
      "rho = ",
      
      round(
        rho_final,
        3
      ),
      
      "; ",
      
      format_p(
        p_final
      )
    ),
    
    
    paste0(
      
      "rho = ",
      
      round(
        rho_reduction,
        3
      ),
      
      "; ",
      
      format_p(
        p_reduction
      )
    )
  )
)


write_csv(
  manuscript_anchors_readable,
  file.path(
    out_res_dir,
    "16_manuscript_ready_statistical_anchors_readable.csv"
  )
)


# ============================================================
# 49. SAVE CONSOLIDATED FIGURE 4 OBJECT
# ============================================================
#
# This object is intended for the future final-figure script.
#
# It allows the figure to be reformatted without refitting the
# statistical analyses.
# ============================================================

figure4_objects <- list(
  
  raw_data =
    df,
  
  classifications =
    classes,
  
  country_master =
    country_master,
  
  transition_counts =
    transition_counts,
  
  transition_summary =
    transition_summary,
  
  absolute_decouplers =
    abs_dec,
  
  absolute_decoupler_summary =
    absolute_decoupler_summary,
  
  absolute_decoupler_diagnostics =
    absolute_decoupler_diagnostics,
  
  glm_absolute =
    glm_absolute,
  
  glm_quadratic =
    glm_quadratic,
  
  glm_model_comparison =
    glm_model_comparison,
  
  glm_lrt =
    lrt_linear_vs_quadratic,
  
  OR_doubling =
    OR_doubling,
  
  glm_predictions =
    glm_predictions,
  
  reference_predictions =
    reference_predictions,
  
  cor_initial =
    cor_initial,
  
  cor_final =
    cor_final,
  
  cor_reduction =
    cor_reduction,
  
  lm_initial =
    lm_initial,
  
  lm_final =
    lm_final,
  
  lm_reduction =
    lm_reduction,
  
  transition_palette =
    transition_palette,
  
  state_palette =
    state_palette,
  
  p4a =
    p4a,
  
  p4b =
    p4b,
  
  p4c =
    p4c,
  
  Figure4 =
    Figure4,
  
  pS_a =
    pS_a,
  
  pS_b =
    pS_b,
  
  Supplementary_development_context =
    Supplementary_development_context
)


saveRDS(
  
  figure4_objects,
  
  file.path(
    out_res_dir,
    "Figure4_objects.rds"
  )
)


# ============================================================
# 50. SAVE LIGHTWEIGHT ABSOLUTE-DECOUPLER OBJECT
# ============================================================

saveRDS(
  
  abs_dec,
  
  file.path(
    out_res_dir,
    "absolute_decoupler_country_data.rds"
  )
)


# ============================================================
# 51. REPRODUCIBILITY
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
# 52. FINAL CONSOLE SUMMARY
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "FIGURE 4 - ABSOLUTE DECOUPLING DEVELOPMENT CONTEXT\n"
)

cat(
  "============================================================\n"
)


cat(
  "\n--- DATASET QC ---\n"
)

print(
  dataset_qc,
  n = Inf,
  width = Inf
)


cat(
  "\n--- CLASSIFICATION AND COVERAGE CHECK ---\n"
)

print(
  classification_check,
  n = Inf,
  width = Inf
)


cat(
  "\n--- TRANSITION CATEGORY COUNTS ---\n"
)

print(
  transition_counts,
  n = Inf,
  width = Inf
)


cat(
  "\n--- TRANSITION CATEGORY SUMMARY ---\n"
)

print(
  transition_summary,
  n = Inf,
  width = Inf
)


cat(
  "\n--- ABSOLUTE DECOUPLER SUMMARY ---\n"
)

print(
  absolute_decoupler_summary,
  n = Inf,
  width = Inf
)


cat(
  "\n--- ABSOLUTE DECOUPLER GDP DIAGNOSTICS ---\n"
)

print(
  absolute_decoupler_diagnostics,
  n = Inf,
  width = Inf
)


cat(
  "\n--- ABSOLUTE DECOUPLING GLM ---\n"
)

print(
  glm_absolute_tidy,
  n = Inf,
  width = Inf
)


cat(
  "\n--- ODDS RATIO PER GDP DOUBLING ---\n"
)

print(
  OR_doubling,
  n = Inf,
  width = Inf
)


cat(
  "\n--- LINEAR VS QUADRATIC GLM ---\n"
)

print(
  glm_model_comparison,
  n = Inf,
  width = Inf
)


cat(
  "\n--- LINEAR VS QUADRATIC GLM LRT ---\n"
)

print(
  lrt_linear_vs_quadratic,
  n = Inf,
  width = Inf
)


cat(
  "\n--- REFERENCE GDP PREDICTIONS ---\n"
)

print(
  reference_predictions,
  n = Inf,
  width = Inf
)


cat(
  "\n--- MANUSCRIPT-READY STATISTICAL ANCHORS ---\n"
)

print(
  manuscript_anchors_readable,
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
  "- Transition classes are loaded directly from Script 03 and are not recalculated here.\n"
)

cat(
  "- Initial and final cropland N surplus correspond to exact annual 1992 and 2022 values.\n"
)

cat(
  "- GDP_median represents development context across 1992-2022, not the GDP level at which decoupling occurred.\n"
)

cat(
  "- Positive N_surplus_reduction values indicate absolute reductions in cropland N surplus.\n"
)

cat(
  "- Spearman correlations are the reported association statistics for panels b and c.\n"
)

cat(
  "- Linear fits against log10 median GDP are visual aids and supplementary model summaries.\n"
)

cat(
  "- The logistic GLM is cross-sectional at the country-trajectory level and is not interpreted causally.\n"
)

cat(
  "- The quadratic GLM is a diagnostic sensitivity check, not the primary model.\n"
)

cat(
  "- A non-significant GDP coefficient must not be described as evidence of no relationship; report the estimate, uncertainty and P value.\n"
)

cat(
  "- Improvement in a country's N surplus does not imply convergence toward universally low final N surplus.\n"
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
# END OF 05_Figure4_absolute_decoupling_development_context.R
# ============================================================