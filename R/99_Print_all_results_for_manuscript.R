# ============================================================
# 99_Print_all_results_for_manuscript.R
#
# Divergent nitrogen transition pathways during
# global agricultural development
#
# PURPOSE
# -------
# Collect and print all key numerical results from the definitive
# analysis pipeline:
#
#   01_Figure1_GAM_relationships.R
#   02_Figure2_global_trajectories.R
#   03_Transition_classification.R
#   04_Figure3_trajectory_configurations.R
#   05_Figure4_absolute_decoupling_development_context.R
#
# IMPORTANT
# ---------
# This script:
#
# - DOES NOT refit any statistical model.
# - DOES NOT recreate any classification.
# - DOES NOT modify any analytical object.
# - Reads the definitive CSV outputs already produced by
#   Scripts 01-05.
#
# It prints all important results to the R console and also saves
# the complete output as:
#
#   results/99_Manuscript_results_summary/
#   ALL_RESULTS_FOR_MANUSCRIPT.txt
#
# This file can then be copied directly into the Results-writing
# workflow for manuscript auditing.
#
# ============================================================


# ============================================================
# 1. SETUP
# ============================================================

rm(list = ls())

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  width = 250
)


# ============================================================
# 2. PACKAGE
# ============================================================

if (
  !requireNamespace(
    "tidyverse",
    quietly = TRUE
  )
) {
  
  stop(
    "Please install tidyverse before running this script."
  )
}


suppressPackageStartupMessages({
  
  library(tidyverse)
  
})


# ============================================================
# 3. PATHS
# ============================================================

base_dir <- "."


results_dir <- file.path(
  base_dir,
  "results"
)


res_01 <- file.path(
  results_dir,
  "01_Figure1_GAM_relationships"
)


res_02 <- file.path(
  results_dir,
  "02_Figure2_global_trajectories"
)


res_03 <- file.path(
  results_dir,
  "03_Transition_classification"
)


res_04 <- file.path(
  results_dir,
  "04_Figure3_trajectory_configurations"
)


res_05 <- file.path(
  results_dir,
  "05_Figure4_absolute_decoupling_development_context"
)


out_dir <- file.path(
  results_dir,
  "99_Manuscript_results_summary"
)


dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


output_txt <- file.path(
  out_dir,
  "ALL_RESULTS_FOR_MANUSCRIPT.txt"
)


# ============================================================
# 4. HELPER FUNCTIONS
# ============================================================


# ------------------------------------------------------------
# Read a CSV and fail clearly if it is missing
# ------------------------------------------------------------

read_result <- function(
    directory,
    filename
) {
  
  path <- file.path(
    directory,
    filename
  )
  
  
  if (
    !file.exists(
      path
    )
  ) {
    
    stop(
      "\nRequired result file not found:\n",
      path,
      "\n\nRun the corresponding analysis script first."
    )
  }
  
  
  readr::read_csv(
    path,
    show_col_types = FALSE
  )
}


# ------------------------------------------------------------
# Read optional CSV
# ------------------------------------------------------------

read_optional_result <- function(
    directory,
    filename
) {
  
  path <- file.path(
    directory,
    filename
  )
  
  
  if (
    !file.exists(
      path
    )
  ) {
    
    return(
      NULL
    )
  }
  
  
  readr::read_csv(
    path,
    show_col_types = FALSE
  )
}


# ------------------------------------------------------------
# Print section header
# ------------------------------------------------------------

section_header <- function(
    title
) {
  
  cat(
    "\n\n"
  )
  
  cat(
    paste(
      rep(
        "#",
        78
      ),
      collapse = ""
    ),
    "\n"
  )
  
  cat(
    title,
    "\n"
  )
  
  cat(
    paste(
      rep(
        "#",
        78
      ),
      collapse = ""
    ),
    "\n"
  )
}


# ------------------------------------------------------------
# Print subsection header
# ------------------------------------------------------------

subsection_header <- function(
    title
) {
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    title,
    "\n"
  )
  
  cat(
    "============================================================\n"
  )
}


# ------------------------------------------------------------
# Print tibble fully
# ------------------------------------------------------------

print_full <- function(
    x
) {
  
  print(
    tibble::as_tibble(
      x
    ),
    n = Inf,
    width = Inf
  )
}


# ============================================================
# 5. READ FIGURE 1 RESULTS
# ============================================================

F1_indicator_qc <- read_result(
  res_01,
  "00_QC_indicator_reconstruction.csv"
)


F1_dataset_qc <- read_result(
  res_01,
  "00_dataset_QC.csv"
)


F1_sample_sizes <- read_result(
  res_01,
  "01_main_model_sample_summary.csv"
)


F1_gam_stats <- read_result(
  res_01,
  "02_main_GAM_statistics.csv"
)


F1_k_check <- read_result(
  file.path(
    res_01,
    "diagnostics"
  ),
  "01_main_GAM_k_dimension_checks.csv"
)


F1_lag1 <- read_result(
  file.path(
    res_01,
    "diagnostics"
  ),
  "02_main_GAM_within_country_lag1_summary.csv"
)


F1_gam_vs_linear <- read_result(
  res_01,
  "03_GAM_vs_linear_AIC_ML.csv"
)


F1_derivative_intervals <- read_result(
  res_01,
  "06_derivative_significance_intervals.csv"
)


F1_GDP_terminal_anchor <- read_result(
  res_01,
  "07_Model_A_GDP_terminal_decline_anchor.csv"
)


F1_GDP_anchor_sensitivity <- read_result(
  res_01,
  "08_Model_A_GDP_anchor_runlength_sensitivity.csv"
)


F1_Ninput_max_slope <- read_result(
  res_01,
  "09_Model_B_steepest_fitted_slope.csv"
)


F1_country_sensitivity_stats <- read_result(
  file.path(
    res_01,
    "supplementary"
  ),
  "01_country_exclusion_GAM_statistics.csv"
)


F1_country_sensitivity_comparison <- read_result(
  file.path(
    res_01,
    "supplementary"
  ),
  "02_country_exclusion_GAM_vs_linear.csv"
)


F1_temporal_sensitivity_stats <- read_result(
  file.path(
    res_01,
    "supplementary"
  ),
  "05_temporal_sensitivity_GAM_statistics.csv"
)


F1_temporal_sensitivity_comparison <- read_result(
  file.path(
    res_01,
    "supplementary"
  ),
  "06_temporal_sensitivity_GAM_vs_linear.csv"
)


F1_temporal_support <- read_result(
  file.path(
    res_01,
    "supplementary"
  ),
  "09_temporal_predictor_support.csv"
)


F1_temporal_common_support <- read_result(
  file.path(
    res_01,
    "supplementary"
  ),
  "10_temporal_common_predictor_support.csv"
)


F1_manuscript_anchors <- read_result(
  res_01,
  "manuscript_anchors.csv"
)


# ============================================================
# 6. READ FIGURE 2 RESULTS
# ============================================================

F2_dataset_qc <- read_result(
  res_02,
  "01_Figure2_QC.csv"
)


F2_metric_qc <- read_result(
  res_02,
  "02_metric_reconstruction_QC.csv"
)


F2_rolling_qc <- read_result(
  res_02,
  "04_rolling_mean_QC.csv"
)


F2_highlight_summary <- read_result(
  res_02,
  "05_highlighted_country_trajectory_summary.csv"
)


F2_ranges <- read_result(
  res_02,
  "06_Figure2_variable_ranges.csv"
)


F2_manuscript_anchors <- read_result(
  res_02,
  "manuscript_anchors.csv"
)


# ============================================================
# 7. READ TRANSITION CLASSIFICATION RESULTS
# ============================================================

T_metric_qc <- read_result(
  res_03,
  "02_mass_metric_reconstruction_QC.csv"
)


T_endpoint_completeness <- read_result(
  res_03,
  "03_endpoint_variable_completeness_QC.csv"
)


T_denominator_qc_summary <- read_result(
  res_03,
  "06_relative_change_denominator_QC_summary.csv"
)


T_main_counts <- read_result(
  res_03,
  "07_transition_category_counts_5pct.csv"
)


T_validated_counts <- read_result(
  res_03,
  "09_validated_main_count_check.csv"
)


T_threshold_counts <- read_result(
  res_03,
  "12_threshold_sensitivity_category_counts.csv"
)


T_threshold_counts_wide <- read_result(
  res_03,
  "13_threshold_sensitivity_category_counts_wide.csv"
)


T_stability <- read_result(
  res_03,
  "15_threshold_sensitivity_overall_stability_summary.csv"
)


T_agreement_5pct <- read_result(
  res_03,
  "16_threshold_agreement_with_main_5pct.csv"
)


T_key_sensitivity <- read_result(
  res_03,
  "18_key_category_threshold_sensitivity.csv"
)


T_manuscript_anchors <- read_result(
  res_03,
  "manuscript_anchors.csv"
)


# ============================================================
# 8. READ FIGURE 3 RESULTS
# ============================================================

F3_dataset_qc <- read_result(
  res_04,
  "00_dataset_QC.csv"
)


F3_metric_qc <- read_result(
  res_04,
  "01_mass_metric_QC.csv"
)


F3_feature_coverage <- read_result(
  res_04,
  "04_feature_coverage_QC.csv"
)


F3_pca_variance <- read_result(
  res_04,
  "07_PCA_variance_explained.csv"
)


F3_cluster_diagnostics <- read_result(
  res_04,
  "08_cluster_diagnostics_k2_k6.csv"
)


F3_configuration_diagnostics <- read_result(
  res_04,
  "10_configuration_label_diagnostics.csv"
)


F3_configuration_counts <- read_result(
  res_04,
  "12_configuration_counts.csv"
)


F3_trajectory_summary <- read_result(
  res_04,
  "13_configuration_median_trajectories.csv"
)


F3_cross_tab <- read_optional_result(
  res_04,
  "15_transition_class_vs_configuration_long.csv"
)


F3_cross_tab_rowprop <- read_optional_result(
  res_04,
  "15_transition_class_vs_configuration_row_proportions.csv"
)


F3_scaling_agreement <- read_result(
  res_04,
  "16_scaling_method_agreement_summary.csv"
)


F3_scaling_country_comparison <- read_result(
  res_04,
  "16_scaling_country_configuration_comparison.csv"
)


F3_scaling_profiles <- read_result(
  res_04,
  "16_scaling_configuration_summaries.csv"
)


F3_scaling_PCA <- read_result(
  res_04,
  "16_scaling_PCA_variance.csv"
)


F3_scaling_counts <- read_result(
  res_04,
  "17_scaling_configuration_counts.csv"
)


F3_scaling_changed <- read_result(
  res_04,
  "18_countries_changing_configuration_across_scaling.csv"
)


F3_manuscript_anchors <- read_result(
  res_04,
  "manuscript_anchors.csv"
)


# ------------------------------------------------------------
# Reconstruct the table we inspected manually:
# main configuration -> scaling configuration
# ------------------------------------------------------------

F3_scaling_transition_summary <-
  F3_scaling_country_comparison %>%
  
  group_by(
    Method,
    Configuration_main,
    Configuration_scaling
  ) %>%
  
  summarise(
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  
  arrange(
    Method,
    desc(n)
  )


# ============================================================
# 9. READ FIGURE 4 RESULTS
# ============================================================

F4_dataset_qc <- read_result(
  res_05,
  "00_dataset_QC.csv"
)


F4_classification_check <- read_result(
  res_05,
  "05_classification_and_coverage_check.csv"
)


F4_transition_counts <- read_result(
  res_05,
  "06_transition_category_counts.csv"
)


F4_transition_summary <- read_result(
  res_05,
  "07_transition_category_summary.csv"
)


F4_GLM <- read_result(
  res_05,
  "08_GLM_absolute_decoupling_coefficients.csv"
)


F4_GLM_fit <- read_result(
  res_05,
  "08_GLM_absolute_decoupling_fit.csv"
)


F4_OR_doubling <- read_result(
  res_05,
  "09_GLM_OR_per_GDP_doubling.csv"
)


F4_linear_quadratic <- read_result(
  res_05,
  "10_GLM_linear_vs_quadratic_fit.csv"
)


F4_linear_quadratic_LRT <- read_result(
  res_05,
  "10_GLM_linear_vs_quadratic_LRT.csv"
)


F4_reference_predictions <- read_result(
  res_05,
  "11_probability_reference_GDP.csv"
)


F4_decoupler_diagnostics <- read_result(
  res_05,
  "13_absolute_decoupler_GDP_diagnostics.csv"
)


F4_decoupler_summary <- read_result(
  res_05,
  "14_absolute_decoupler_summary.csv"
)


F4_manuscript_anchors <- read_result(
  res_05,
  "15_manuscript_ready_statistical_anchors.csv"
)


F4_manuscript_anchors_readable <- read_result(
  res_05,
  "16_manuscript_ready_statistical_anchors_readable.csv"
)


# ============================================================
# 10. CREATE COMPLETE PRINTING FUNCTION
# ============================================================

print_all_results <- function() {
  
  
  # ==========================================================
  # FIGURE 1
  # ==========================================================
  
  section_header(
    "FIGURE 1 | GLOBAL NONLINEAR RELATIONSHIPS"
  )
  
  
  subsection_header(
    "INDICATOR RECONSTRUCTION QC"
  )
  
  print_full(
    F1_indicator_qc
  )
  
  
  subsection_header(
    "DATASET QC"
  )
  
  print_full(
    F1_dataset_qc
  )
  
  
  subsection_header(
    "MODEL SAMPLE SIZES"
  )
  
  print_full(
    F1_sample_sizes
  )
  
  
  subsection_header(
    "MAIN GAM STATISTICS"
  )
  
  print_full(
    F1_gam_stats
  )
  
  
  subsection_header(
    "GAM vs LINEAR: ML AIC COMPARISON"
  )
  
  print_full(
    F1_gam_vs_linear
  )
  
  
  subsection_header(
    "BASIS-DIMENSION CHECKS"
  )
  
  print_full(
    F1_k_check
  )
  
  
  subsection_header(
    "WITHIN-COUNTRY LAG-1 RESIDUAL CORRELATIONS"
  )
  
  print_full(
    F1_lag1
  )
  
  
  subsection_header(
    "DERIVATIVE SIGNIFICANCE INTERVALS"
  )
  
  print_full(
    F1_derivative_intervals
  )
  
  
  subsection_header(
    "MODEL A: GDP TERMINAL DECLINE"
  )
  
  print_full(
    F1_GDP_terminal_anchor
  )
  
  
  subsection_header(
    "MODEL A: RUN-LENGTH SENSITIVITY"
  )
  
  print_full(
    F1_GDP_anchor_sensitivity
  )
  
  
  subsection_header(
    "MODEL B: MAXIMUM FIRST DERIVATIVE"
  )
  
  print_full(
    F1_Ninput_max_slope
  )
  
  
  subsection_header(
    "COUNTRY-EXCLUSION SENSITIVITY: GAM STATISTICS"
  )
  
  print_full(
    F1_country_sensitivity_stats
  )
  
  
  subsection_header(
    "COUNTRY-EXCLUSION SENSITIVITY: GAM vs LINEAR"
  )
  
  print_full(
    F1_country_sensitivity_comparison
  )
  
  
  subsection_header(
    "TEMPORAL SENSITIVITY: GAM STATISTICS"
  )
  
  print_full(
    F1_temporal_sensitivity_stats
  )
  
  
  subsection_header(
    "TEMPORAL SENSITIVITY: GAM vs LINEAR"
  )
  
  print_full(
    F1_temporal_sensitivity_comparison
  )
  
  
  subsection_header(
    "TEMPORAL SENSITIVITY: PREDICTOR SUPPORT"
  )
  
  print_full(
    F1_temporal_support
  )
  
  
  subsection_header(
    "TEMPORAL SENSITIVITY: COMMON PREDICTOR SUPPORT"
  )
  
  print_full(
    F1_temporal_common_support
  )
  
  
  subsection_header(
    "FIGURE 1 MANUSCRIPT ANCHORS"
  )
  
  print_full(
    F1_manuscript_anchors
  )
  
  
  # ==========================================================
  # FIGURE 2
  # ==========================================================
  
  section_header(
    "FIGURE 2 | GLOBAL NATIONAL TRAJECTORIES"
  )
  
  
  subsection_header(
    "DATASET QC"
  )
  
  print_full(
    F2_dataset_qc
  )
  
  
  subsection_header(
    "MASS-HARMONIZED METRIC QC"
  )
  
  print_full(
    F2_metric_qc
  )
  
  
  subsection_header(
    "ROLLING-MEAN QC"
  )
  
  print_full(
    F2_rolling_qc
  )
  
  
  subsection_header(
    "VARIABLE RANGES"
  )
  
  print_full(
    F2_ranges
  )
  
  
  subsection_header(
    "HIGHLIGHTED COUNTRY TRAJECTORY SUMMARY"
  )
  
  print_full(
    F2_highlight_summary
  )
  
  
  subsection_header(
    "FIGURE 2 MANUSCRIPT ANCHORS"
  )
  
  print_full(
    F2_manuscript_anchors
  )
  
  
  # ==========================================================
  # TRANSITION CLASSIFICATION
  # ==========================================================
  
  section_header(
    "FIRST-TO-LAST TRANSITION CLASSIFICATION"
  )
  
  
  subsection_header(
    "MASS-HARMONIZED METRIC QC"
  )
  
  print_full(
    T_metric_qc
  )
  
  
  subsection_header(
    "ENDPOINT COMPLETENESS"
  )
  
  print_full(
    T_endpoint_completeness
  )
  
  
  subsection_header(
    "RELATIVE-CHANGE DENOMINATOR QC"
  )
  
  print_full(
    T_denominator_qc_summary
  )
  
  
  subsection_header(
    "MAIN 5% TRANSITION CATEGORIES"
  )
  
  print_full(
    T_main_counts
  )
  
  
  subsection_header(
    "VALIDATED COUNT CHECK"
  )
  
  print_full(
    T_validated_counts
  )
  
  
  subsection_header(
    "THRESHOLD SENSITIVITY COUNTS: 0-10%"
  )
  
  print_full(
    T_threshold_counts_wide
  )
  
  
  subsection_header(
    "AGREEMENT WITH MAIN 5% CLASSIFICATION"
  )
  
  print_full(
    T_agreement_5pct
  )
  
  
  subsection_header(
    "OVERALL COUNTRY-LEVEL STABILITY"
  )
  
  print_full(
    T_stability
  )
  
  
  subsection_header(
    "KEY CATEGORY SENSITIVITY"
  )
  
  print_full(
    T_key_sensitivity
  )
  
  
  subsection_header(
    "TRANSITION CLASSIFICATION MANUSCRIPT ANCHORS"
  )
  
  print_full(
    T_manuscript_anchors
  )
  
  
  # ==========================================================
  # FIGURE 3
  # ==========================================================
  
  section_header(
    "FIGURE 3 | TRAJECTORY-BASED N CONFIGURATIONS"
  )
  
  
  subsection_header(
    "DATASET QC"
  )
  
  print_full(
    F3_dataset_qc
  )
  
  
  subsection_header(
    "MASS-HARMONIZED METRIC QC"
  )
  
  print_full(
    F3_metric_qc
  )
  
  
  subsection_header(
    "FEATURE COVERAGE"
  )
  
  print_full(
    F3_feature_coverage
  )
  
  
  subsection_header(
    "PCA VARIANCE: FIRST 6 COMPONENTS"
  )
  
  print_full(
    F3_pca_variance %>%
      slice_head(
        n = 6
      )
  )
  
  
  subsection_header(
    "CLUSTER NUMBER DIAGNOSTICS"
  )
  
  print_full(
    F3_cluster_diagnostics
  )
  
  
  subsection_header(
    "CONFIGURATION LABEL DIAGNOSTICS"
  )
  
  print_full(
    F3_configuration_diagnostics
  )
  
  
  subsection_header(
    "FINAL CONFIGURATION COUNTS"
  )
  
  print_full(
    F3_configuration_counts
  )
  
  
  if (
    !is.null(
      F3_cross_tab
    )
  ) {
    
    subsection_header(
      "TRANSITION CLASS vs TRAJECTORY CONFIGURATION"
    )
    
    print_full(
      F3_cross_tab
    )
  }
  
  
  if (
    !is.null(
      F3_cross_tab_rowprop
    )
  ) {
    
    subsection_header(
      "TRANSITION CLASS vs CONFIGURATION: ROW PROPORTIONS"
    )
    
    print_full(
      F3_cross_tab_rowprop
    )
  }
  
  
  subsection_header(
    "SCALING METHOD SENSITIVITY"
  )
  
  print_full(
    F3_scaling_agreement
  )
  
  
  subsection_header(
    "CONFIGURATION COUNTS ACROSS SCALING METHODS"
  )
  
  print_full(
    F3_scaling_counts
  )
  
  
  subsection_header(
    "COUNTRY CONFIGURATION TRANSITIONS ACROSS SCALING METHODS"
  )
  
  print_full(
    F3_scaling_transition_summary
  )
  
  
  subsection_header(
    "CONFIGURATION PROFILES ACROSS SCALING METHODS"
  )
  
  print_full(
    F3_scaling_profiles
  )
  
  
  subsection_header(
    "PCA VARIANCE ACROSS SCALING METHODS"
  )
  
  print_full(
    F3_scaling_PCA
  )
  
  
  subsection_header(
    "COUNTRIES CHANGING CONFIGURATION ACROSS SCALING METHODS"
  )
  
  print_full(
    F3_scaling_changed
  )
  
  
  subsection_header(
    "FIGURE 3 MANUSCRIPT ANCHORS"
  )
  
  print_full(
    F3_manuscript_anchors
  )
  
  
  # ==========================================================
  # FIGURE 4
  # ==========================================================
  
  section_header(
    "FIGURE 4 | DEVELOPMENT CONTEXT AND ABSOLUTE DECOUPLING"
  )
  
  
  subsection_header(
    "DATASET QC"
  )
  
  print_full(
    F4_dataset_qc
  )
  
  
  subsection_header(
    "CLASSIFICATION AND COVERAGE CHECK"
  )
  
  print_full(
    F4_classification_check
  )
  
  
  subsection_header(
    "TRANSITION CATEGORY COUNTS"
  )
  
  print_full(
    F4_transition_counts
  )
  
  
  subsection_header(
    "GDP / N-PRESSURE SUMMARY BY TRANSITION CATEGORY"
  )
  
  print_full(
    F4_transition_summary
  )
  
  
  subsection_header(
    "ABSOLUTE DECOUPLER SUMMARY"
  )
  
  print_full(
    F4_decoupler_summary
  )
  
  
  subsection_header(
    "ABSOLUTE DECOUPLER GDP DIAGNOSTICS"
  )
  
  print_full(
    F4_decoupler_diagnostics
  )
  
  
  subsection_header(
    "ABSOLUTE DECOUPLING GLM"
  )
  
  print_full(
    F4_GLM
  )
  
  
  subsection_header(
    "ABSOLUTE DECOUPLING GLM FIT"
  )
  
  print_full(
    F4_GLM_fit
  )
  
  
  subsection_header(
    "ODDS RATIO PER GDP DOUBLING"
  )
  
  print_full(
    F4_OR_doubling
  )
  
  
  subsection_header(
    "LINEAR vs QUADRATIC GLM"
  )
  
  print_full(
    F4_linear_quadratic
  )
  
  
  subsection_header(
    "LINEAR vs QUADRATIC GLM LRT"
  )
  
  print_full(
    F4_linear_quadratic_LRT
  )
  
  
  subsection_header(
    "REFERENCE GDP PREDICTIONS"
  )
  
  print_full(
    F4_reference_predictions
  )
  
  
  subsection_header(
    "FIGURE 4 MANUSCRIPT-READY STATISTICAL ANCHORS"
  )
  
  print_full(
    F4_manuscript_anchors
  )
  
  
  subsection_header(
    "FIGURE 4 HUMAN-READABLE MANUSCRIPT ANCHORS"
  )
  
  print_full(
    F4_manuscript_anchors_readable
  )
  
  
  # ==========================================================
  # END
  # ==========================================================
  
  section_header(
    "END OF DEFINITIVE MANUSCRIPT RESULTS"
  )
  
  
  cat(
    "\nAll reported values above were read from the saved outputs ",
    "of Scripts 01-05.\n",
    sep = ""
  )
  
  
  cat(
    "\nThe discarded urbanization / animal-protein Supplementary ",
    "Figure is intentionally NOT included.\n",
    sep = ""
  )
}


# ============================================================
# 11. PRINT TO CONSOLE
# ============================================================

print_all_results()


# ============================================================
# 12. ALSO SAVE EVERYTHING TO A SINGLE TXT FILE
# ============================================================

sink(
  output_txt,
  split = FALSE
)


print_all_results()


sink()


# ============================================================
# 13. FINAL MESSAGE
# ============================================================

cat(
  "\n\n============================================================\n"
)

cat(
  "COMPLETE RESULTS REPORT CREATED\n"
)

cat(
  "============================================================\n"
)


cat(
  "\nSaved to:\n",
  output_txt,
  "\n",
  sep = ""
)


cat(
  "\nYou can now open ALL_RESULTS_FOR_MANUSCRIPT.txt and copy ",
  "the complete contents into the Results-writing chat.\n",
  sep = ""
)


# ============================================================
# END OF 99_Print_all_results_for_manuscript.R
# ============================================================