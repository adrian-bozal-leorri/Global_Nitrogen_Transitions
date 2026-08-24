# ============================================================
# 04_Figure3_trajectory_configurations.R
#
# Divergent nitrogen transition pathways during
# global agricultural development
#
# FIGURE 3
# Trajectory-based nitrogen configurations
#
# MAIN ANALYSIS
# -------------
# Recurrent multivariate trajectory configurations based on
# country-level trajectory features calculated across 1992-2022.
#
# CORE VARIABLES
# --------------
# 1. N surplus per unit protein supply
#    -> definitive mass-harmonized metric
#
# 2. Cropland N surplus
#
# 3. N input intensity
#
# 4. Total protein supply
#
# TRAJECTORY FEATURES
# -------------------
# For each core variable:
#
#   final:
#     mean during 2018-2022
#
#   delta:
#     final mean minus initial mean
#     initial mean = 1992-1996
#
#   slope:
#     linear slope across 1992-2022
#
#   acceleration:
#     late slope (2008-2022)
#     minus
#     early slope (1992-2006)
#
# Plus:
#
#   delta_N_surplus / delta_Total_protein_supply
#
# Total features = 17
#
# MAIN SCALING
# ------------
# Robust scaling:
#
#   (x - median) / IQR
#
# followed by constraining scaled values to [-5, +5].
#
# PCA is used for VISUALIZATION.
#
# K-means clustering is performed on the complete scaled feature
# matrix, NOT on PC1-PC2 alone.
#
# MAIN CONFIGURATIONS
# -------------------
# - Lower-pressure configuration
# - Declining-intensity configuration
# - Intensifying configuration
#
# IMPORTANT
# ---------
# These configurations are partially overlapping regions of
# multivariate trajectory space. They are not discrete natural
# states and are distinct from the first-to-last transition
# classes generated in Script 03.
#
# SUPPLEMENTARY ANALYSES INCLUDED HERE
# ------------------------------------
# 1. k = 2-6 elbow and silhouette diagnostics.
#
# 2. Scaling-method sensitivity:
#      - Robust + constrained
#      - Z-score
#      - Min-max
#      - Signed log + z-score
#
#    Agreement quantified using:
#      - Adjusted Rand Index
#      - % countries retaining the same interpretive configuration
#
# 3. Cross-tabulation with first-to-last transition classes.
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
  "patchwork",
  "scales",
  "ggrepel",
  "cluster",
  "mclust",
  "countrycode",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata"
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
  library(patchwork)
  library(scales)
  library(ggrepel)
  library(cluster)
  library(mclust)
  library(countrycode)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  
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
  "04_Figure3_trajectory_configurations"
)


out_plot_dir <- file.path(
  plots_dir,
  "04_Figure3_trajectory_configurations"
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

initial_start <- 1992
initial_end <- 1996

final_start <- 2018
final_end <- 2022

early_slope_start <- 1992
early_slope_end <- 2006

late_slope_start <- 2008
late_slope_end <- 2022

k_final <- 3

k_values <- 2:6


# ============================================================
# 5. CONFIGURATION TERMINOLOGY
# ============================================================

configuration_order <- c(
  
  "Lower-pressure configuration",
  
  "Declining-intensity configuration",
  
  "Intensifying configuration"
)


configuration_palette <- c(
  
  "Lower-pressure configuration" =
    "#D55E00",
  
  "Declining-intensity configuration" =
    "#0072B2",
  
  "Intensifying configuration" =
    "#009E73"
)


# ============================================================
# 6. HIGHLIGHTED COUNTRIES
# ============================================================

highlight_codes <- c(
  "CHN",
  "IND",
  "USA",
  "BRA",
  "NLD",
  "ZAF",
  "KOR",
  "NGA",
  "ARG"
)


# ============================================================
# 7. HELPER FUNCTIONS
# ============================================================

safe_mean <- function(x) {
  
  x_ok <- x[
    is.finite(x)
  ]
  
  if (length(x_ok) == 0) {
    return(NA_real_)
  }
  
  mean(x_ok)
}


safe_sd <- function(x) {
  
  x_ok <- x[
    is.finite(x)
  ]
  
  if (length(x_ok) < 2) {
    return(NA_real_)
  }
  
  sd(x_ok)
}


safe_slope <- function(
    y,
    year
) {
  
  ok <-
    is.finite(y) &
    is.finite(year)
  
  if (sum(ok) < 8) {
    return(NA_real_)
  }
  
  unname(
    coef(
      lm(
        y[ok] ~ year[ok]
      )
    )[2]
  )
}


robust_scale <- function(x) {
  
  med <- median(
    x,
    na.rm = TRUE
  )
  
  iqr <- IQR(
    x,
    na.rm = TRUE
  )
  
  if (
    !is.finite(iqr) ||
    iqr == 0
  ) {
    
    return(
      rep(
        NA_real_,
        length(x)
      )
    )
  }
  
  (x - med) / iqr
}


z_scale <- function(x) {
  
  mu <- mean(
    x,
    na.rm = TRUE
  )
  
  sigma <- sd(
    x,
    na.rm = TRUE
  )
  
  if (
    !is.finite(sigma) ||
    sigma == 0
  ) {
    
    return(
      rep(
        NA_real_,
        length(x)
      )
    )
  }
  
  (x - mu) / sigma
}


minmax_scale <- function(x) {
  
  xmin <- min(
    x,
    na.rm = TRUE
  )
  
  xmax <- max(
    x,
    na.rm = TRUE
  )
  
  range_x <-
    xmax -
    xmin
  
  if (
    !is.finite(range_x) ||
    range_x == 0
  ) {
    
    return(
      rep(
        NA_real_,
        length(x)
      )
    )
  }
  
  (x - xmin) /
    range_x
}


signed_log10 <- function(x) {
  
  sign(x) *
    log10(
      abs(x) + 1
    )
}


constrain_scaled <- function(
    x,
    lower = -5,
    upper = 5
) {
  
  pmin(
    pmax(
      x,
      lower
    ),
    upper
  )
}


country_plot_name <- function(
    Country,
    ISO3
) {
  
  case_when(
    
    Country ==
      "Netherlands (Kingdom of the)" ~
      "Netherlands",
    
    Country ==
      "United States of America" ~
      "USA",
    
    Country ==
      "China, mainland" ~
      "China",
    
    ISO3 == "KOR" ~
      "South Korea",
    
    ISO3 == "NGA" ~
      "Nigeria",
    
    ISO3 == "ARG" ~
      "Argentina",
    
    TRUE ~
      Country
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
      
      plot.margin =
        margin(
          6,
          8,
          6,
          8
        )
    )
}


# ============================================================
# 8. LOAD DATA
# ============================================================

if (!file.exists(input_file)) {
  
  stop(
    "Input file not found:\n",
    input_file
  )
}


df_raw <- readr::read_csv(
  input_file,
  show_col_types = FALSE
)


# ============================================================
# 9. REQUIRED VARIABLES
# ============================================================

required_vars <- c(
  
  "ISO3",
  
  "Country",
  
  "Year",
  
  "Population_total",
  
  "Cropland_area",
  
  "Total_N_input",
  
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
    "Required variables missing: ",
    paste(
      missing_vars,
      collapse = ", "
    )
  )
}


# ============================================================
# 10. PREPARE DEFINITIVE VARIABLES
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
      as.numeric(Population_total),
    
    Cropland_area =
      as.numeric(Cropland_area),
    
    Total_N_input =
      as.numeric(Total_N_input),
    
    Total_protein_supply =
      as.numeric(Total_protein_supply),
    
    N_surplus =
      as.numeric(N_surplus),
    
    N_surplus_per_protein =
      as.numeric(N_surplus_per_protein)
  ) %>%
  
  rename(
    
    N_surplus_per_protein_legacy =
      N_surplus_per_protein
  ) %>%
  
  mutate(
    
    # --------------------------------------------------------
    # N-input intensity
    # --------------------------------------------------------
    
    N_input_per_ha =
      if_else(
        
        is.finite(Total_N_input) &
          is.finite(Cropland_area) &
          Cropland_area > 0,
        
        Total_N_input *
          1000 /
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
    # Definitive mass-harmonized metric
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
# 11. STRUCTURAL QC
# ============================================================

duplicate_check <- df %>%
  
  dplyr::count(
    ISO3,
    Year,
    name = "n"
  ) %>%
  
  dplyr::filter(
    n > 1
  )


if (nrow(duplicate_check) > 0) {
  
  stop(
    "Duplicated ISO3-Year observations detected."
  )
}


dataset_qc <- tibble(
  
  metric = c(
    "Rows",
    "Countries",
    "First year",
    "Last year"
  ),
  
  value = c(
    nrow(df),
    n_distinct(df$ISO3),
    min(df$Year),
    max(df$Year)
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
# 12. MASS-HARMONIZED METRIC QC
# ============================================================

metric_qc <- df %>%
  
  mutate(
    
    identity_value =
      N_surplus_per_protein_legacy *
      (Cropland_area / Population_total) *
      1000 /
      365
  ) %>%
  
  summarise(
    
    max_absolute_difference =
      max(
        abs(
          N_surplus_per_protein_mass -
            identity_value
        ),
        na.rm = TRUE
      )
  )


write_csv(
  metric_qc,
  file.path(
    out_res_dir,
    "01_mass_metric_QC.csv"
  )
)


# ============================================================
# 13. CORE TRAJECTORY VARIABLES
# ============================================================

vars_core <- c(
  
  "N_surplus_per_protein_mass",
  
  "N_surplus",
  
  "N_input_per_ha",
  
  "Total_protein_supply"
)


# ============================================================
# 14. BUILD COUNTRY-LEVEL TRAJECTORY FEATURES
# ============================================================

country_features_long <- df %>%
  
  select(
    ISO3,
    Country,
    Year,
    all_of(vars_core)
  ) %>%
  
  pivot_longer(
    
    cols =
      all_of(vars_core),
    
    names_to =
      "Variable",
    
    values_to =
      "Value"
  ) %>%
  
  group_by(
    ISO3,
    Country,
    Variable
  ) %>%
  
  summarise(
    
    n_years =
      sum(
        is.finite(Value)
      ),
    
    
    initial =
      safe_mean(
        Value[
          Year >= initial_start &
            Year <= initial_end
        ]
      ),
    
    
    final =
      safe_mean(
        Value[
          Year >= final_start &
            Year <= final_end
        ]
      ),
    
    
    delta =
      final -
      initial,
    
    
    slope =
      safe_slope(
        Value,
        Year
      ),
    
    
    variability =
      safe_sd(
        Value
      ),
    
    
    early_slope =
      safe_slope(
        
        Value[
          Year >= early_slope_start &
            Year <= early_slope_end
        ],
        
        Year[
          Year >= early_slope_start &
            Year <= early_slope_end
        ]
      ),
    
    
    late_slope =
      safe_slope(
        
        Value[
          Year >= late_slope_start &
            Year <= late_slope_end
        ],
        
        Year[
          Year >= late_slope_start &
            Year <= late_slope_end
        ]
      ),
    
    
    acceleration =
      late_slope -
      early_slope,
    
    
    .groups =
      "drop"
  )


write_csv(
  country_features_long,
  file.path(
    out_res_dir,
    "02_country_trajectory_features_long.csv"
  )
)


# ============================================================
# 15. CLUSTERING FEATURE MATRIX
# ============================================================

features_for_clustering <- country_features_long %>%
  
  select(
    
    ISO3,
    
    Country,
    
    Variable,
    
    final,
    
    delta,
    
    slope,
    
    acceleration
  ) %>%
  
  pivot_wider(
    
    names_from =
      Variable,
    
    values_from =
      c(
        final,
        delta,
        slope,
        acceleration
      ),
    
    names_glue =
      "{.value}_{Variable}"
  ) %>%
  
  mutate(
    
    delta_surplus_per_delta_protein =
      delta_N_surplus /
      if_else(
        
        is.finite(
          delta_Total_protein_supply
        ) &
          abs(
            delta_Total_protein_supply
          ) >= 1e-6,
        
        delta_Total_protein_supply,
        
        NA_real_
      )
  )


write_csv(
  features_for_clustering,
  file.path(
    out_res_dir,
    "03_country_trajectory_features_raw.csv"
  )
)


# ============================================================
# 16. COMPLETE-CASE FEATURE QC
# ============================================================

feature_cols <- features_for_clustering %>%
  
  select(
    -ISO3,
    -Country
  ) %>%
  
  names()


features_complete <- features_for_clustering %>%
  
  filter(
    if_all(
      all_of(feature_cols),
      is.finite
    )
  )


countries_excluded_features <- features_for_clustering %>%
  
  anti_join(
    
    features_complete %>%
      select(
        ISO3
      ),
    
    by =
      "ISO3"
  )


feature_coverage_qc <- tibble(
  
  countries_dataset =
    n_distinct(
      df$ISO3
    ),
  
  countries_complete_features =
    nrow(
      features_complete
    ),
  
  countries_excluded =
    nrow(
      countries_excluded_features
    )
)


write_csv(
  features_complete,
  file.path(
    out_res_dir,
    "04_country_trajectory_features_complete.csv"
  )
)


write_csv(
  countries_excluded_features,
  file.path(
    out_res_dir,
    "04_countries_excluded_due_to_incomplete_features.csv"
  )
)


write_csv(
  feature_coverage_qc,
  file.path(
    out_res_dir,
    "04_feature_coverage_QC.csv"
  )
)


# ============================================================
# 17. CORRELATION DIAGNOSTICS
# ============================================================

cor_mat <- features_complete %>%
  
  select(
    all_of(feature_cols)
  ) %>%
  
  cor(
    use =
      "pairwise.complete.obs",
    
    method =
      "spearman"
  )


write_csv(
  
  as.data.frame(
    cor_mat
  ) %>%
    
    rownames_to_column(
      "Feature"
    ),
  
  file.path(
    out_res_dir,
    "05_feature_spearman_correlation_matrix.csv"
  )
)


cor_pairs <- as.data.frame(
  as.table(
    cor_mat
  )
) %>%
  
  rename(
    
    feature_1 =
      Var1,
    
    feature_2 =
      Var2,
    
    rho =
      Freq
  ) %>%
  
  filter(
    feature_1 !=
      feature_2
  ) %>%
  
  mutate(
    abs_rho =
      abs(
        rho
      )
  ) %>%
  
  arrange(
    desc(
      abs_rho
    )
  )


write_csv(
  cor_pairs,
  file.path(
    out_res_dir,
    "05_feature_spearman_correlation_pairs.csv"
  )
)


# ============================================================
# 18. MAIN ROBUST SCALING
# ============================================================

features_scaled <- features_complete %>%
  
  mutate(
    
    across(
      
      all_of(
        feature_cols
      ),
      
      robust_scale,
      
      .names =
        "scaled_{.col}"
    )
  )


scaled_cols <- names(
  features_scaled
)[
  str_detect(
    names(
      features_scaled
    ),
    "^scaled_"
  )
]


features_scaled <- features_scaled %>%
  
  mutate(
    
    across(
      
      all_of(
        scaled_cols
      ),
      
      constrain_scaled,
      
      .names =
        "used_{.col}"
    )
  )


used_cols <- names(
  features_scaled
)[
  str_detect(
    names(
      features_scaled
    ),
    "^used_scaled_"
  )
]


X <- features_scaled %>%
  
  select(
    all_of(
      used_cols
    )
  ) %>%
  
  as.matrix()


rownames(X) <-
  features_scaled$ISO3


if (any(!is.finite(X))) {
  
  stop(
    "Non-finite values remain in the main clustering matrix."
  )
}


write_csv(
  features_scaled,
  file.path(
    out_res_dir,
    "06_country_features_robust_scaled_constrained.csv"
  )
)


saveRDS(
  X,
  file.path(
    out_res_dir,
    "06_main_clustering_matrix.rds"
  )
)


# ============================================================
# 19. MAIN PCA
# ============================================================

pca <- prcomp(
  
  X,
  
  center =
    FALSE,
  
  scale. =
    FALSE
)


pca_variance <- tibble(
  
  PC =
    paste0(
      "PC",
      seq_along(
        pca$sdev
      )
    ),
  
  variance_explained =
    pca$sdev^2 /
    sum(
      pca$sdev^2
    ),
  
  cumulative_variance =
    cumsum(
      variance_explained
    )
)


pca_scores <- as_tibble(
  
  pca$x[
    ,
    1:min(
      6,
      ncol(
        pca$x
      )
    ),
    drop =
      FALSE
  ]
  
) %>%
  
  bind_cols(
    
    features_scaled %>%
      select(
        ISO3,
        Country
      )
  )


pca_loadings <- as.data.frame(
  pca$rotation
) %>%
  
  rownames_to_column(
    "Feature"
  ) %>%
  
  as_tibble()


write_csv(
  pca_variance,
  file.path(
    out_res_dir,
    "07_PCA_variance_explained.csv"
  )
)


write_csv(
  pca_scores,
  file.path(
    out_res_dir,
    "07_PCA_scores.csv"
  )
)


write_csv(
  pca_loadings,
  file.path(
    out_res_dir,
    "07_PCA_loadings.csv"
  )
)


saveRDS(
  pca,
  file.path(
    out_res_dir,
    "07_PCA_model.rds"
  )
)


# ============================================================
# 20. k-MEANS DIAGNOSTICS
# ============================================================

set.seed(123)


cluster_diagnostics <- purrr::map_dfr(
  
  k_values,
  
  function(k) {
    
    km <- kmeans(
      
      X,
      
      centers =
        k,
      
      nstart =
        100
    )
    
    
    sil <- cluster::silhouette(
      
      km$cluster,
      
      dist(X)
    )
    
    
    tibble(
      
      k =
        k,
      
      total_withinss =
        km$tot.withinss,
      
      betweenSS_fraction =
        km$betweenss /
        km$totss,
      
      mean_silhouette =
        mean(
          sil[, 3]
        )
    )
  }
)


write_csv(
  cluster_diagnostics,
  file.path(
    out_res_dir,
    "08_cluster_diagnostics_k2_k6.csv"
  )
)


# ============================================================
# 21. SUPPLEMENTARY: k DIAGNOSTICS
# ============================================================

p_elbow <- ggplot(
  
  cluster_diagnostics,
  
  aes(
    x =
      k,
    
    y =
      total_withinss
  )
  
) +
  
  geom_line(
    linewidth =
      0.8
  ) +
  
  geom_point(
    size =
      2
  ) +
  
  scale_x_continuous(
    breaks =
      k_values
  ) +
  
  labs(
    
    title =
      "a  Elbow diagnostic",
    
    x =
      "Number of configurations",
    
    y =
      "Total within-cluster sum of squares"
  ) +
  
  theme_nature()


p_silhouette <- ggplot(
  
  cluster_diagnostics,
  
  aes(
    x =
      k,
    
    y =
      mean_silhouette
  )
  
) +
  
  geom_line(
    linewidth =
      0.8
  ) +
  
  geom_point(
    size =
      2
  ) +
  
  scale_x_continuous(
    breaks =
      k_values
  ) +
  
  labs(
    
    title =
      "b  Silhouette diagnostic",
    
    x =
      "Number of configurations",
    
    y =
      "Mean silhouette width"
  ) +
  
  theme_nature()


Supplementary_k_diagnostics <- (
  p_elbow |
    p_silhouette
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Supplementary_cluster_number_diagnostics.png"
  ),
  
  Supplementary_k_diagnostics,
  
  width =
    9,
  
  height =
    4,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Supplementary_cluster_number_diagnostics.pdf"
  ),
  
  Supplementary_k_diagnostics,
  
  width =
    9,
  
  height =
    4
)


# ============================================================
# 22. FINAL k-MEANS
# ============================================================

set.seed(123)


km_final <- kmeans(
  
  X,
  
  centers =
    k_final,
  
  nstart =
    500
)


configuration_df <- features_scaled %>%
  
  mutate(
    
    Configuration_raw =
      factor(
        km_final$cluster
      )
  ) %>%
  
  left_join(
    
    pca_scores,
    
    by =
      c(
        "ISO3",
        "Country"
      )
  )


# ============================================================
# 23. RAW CONFIGURATION SUMMARY
# ============================================================

configuration_summary <- configuration_df %>%
  
  group_by(
    Configuration_raw
  ) %>%
  
  summarise(
    
    n_countries =
      n(),
    
    
    mean_final_N_surplus_per_protein =
      mean(
        final_N_surplus_per_protein_mass,
        na.rm = TRUE
      ),
    
    median_final_N_surplus_per_protein =
      median(
        final_N_surplus_per_protein_mass,
        na.rm = TRUE
      ),
    
    mean_delta_N_surplus_per_protein =
      mean(
        delta_N_surplus_per_protein_mass,
        na.rm = TRUE
      ),
    
    median_delta_N_surplus_per_protein =
      median(
        delta_N_surplus_per_protein_mass,
        na.rm = TRUE
      ),
    
    
    mean_final_N_surplus =
      mean(
        final_N_surplus,
        na.rm = TRUE
      ),
    
    median_final_N_surplus =
      median(
        final_N_surplus,
        na.rm = TRUE
      ),
    
    mean_delta_N_surplus =
      mean(
        delta_N_surplus,
        na.rm = TRUE
      ),
    
    median_delta_N_surplus =
      median(
        delta_N_surplus,
        na.rm = TRUE
      ),
    
    
    mean_final_N_input_per_ha =
      mean(
        final_N_input_per_ha,
        na.rm = TRUE
      ),
    
    median_final_N_input_per_ha =
      median(
        final_N_input_per_ha,
        na.rm = TRUE
      ),
    
    mean_delta_N_input_per_ha =
      mean(
        delta_N_input_per_ha,
        na.rm = TRUE
      ),
    
    median_delta_N_input_per_ha =
      median(
        delta_N_input_per_ha,
        na.rm = TRUE
      ),
    
    
    mean_final_protein =
      mean(
        final_Total_protein_supply,
        na.rm = TRUE
      ),
    
    median_final_protein =
      median(
        final_Total_protein_supply,
        na.rm = TRUE
      ),
    
    mean_delta_protein =
      mean(
        delta_Total_protein_supply,
        na.rm = TRUE
      ),
    
    median_delta_protein =
      median(
        delta_Total_protein_supply,
        na.rm = TRUE
      ),
    
    
    .groups =
      "drop"
  )


write_csv(
  configuration_summary,
  file.path(
    out_res_dir,
    "09_raw_configuration_summary.csv"
  )
)


# ============================================================
# 24. MAIN CONFIGURATION LABELS
# ============================================================
#
# Comparative labels:
#
# Lower-pressure:
#   comparatively low final N surplus AND final N input.
#
# Intensifying:
#   increasing N input AND increasing N surplus.
#
# Declining-intensity:
#   remaining cluster, characterized in the present analysis
#   by declining N pressure / N intensity.
#
# ============================================================

median_cluster_final_surplus <- median(
  
  configuration_summary$mean_final_N_surplus,
  
  na.rm =
    TRUE
)


median_cluster_final_input <- median(
  
  configuration_summary$mean_final_N_input_per_ha,
  
  na.rm =
    TRUE
)


configuration_labels <- configuration_summary %>%
  
  mutate(
    
    Configuration =
      case_when(
        
        
        mean_final_N_surplus <
          median_cluster_final_surplus &
          mean_final_N_input_per_ha <
          median_cluster_final_input ~
          
          "Lower-pressure configuration",
        
        
        mean_delta_N_input_per_ha > 0 &
          mean_delta_N_surplus > 0 ~
          
          "Intensifying configuration",
        
        
        TRUE ~
          
          "Declining-intensity configuration"
      )
  ) %>%
  
  select(
    Configuration_raw,
    Configuration
  )


if (
  nrow(configuration_labels) !=
  k_final ||
  any(
    is.na(
      configuration_labels$Configuration
    )
  )
) {
  
  stop(
    "Main configuration labelling failed."
  )
}


if (
  n_distinct(
    configuration_labels$Configuration
  ) !=
  k_final
) {
  
  stop(
    paste0(
      "The three raw clusters did not receive three unique ",
      "interpretive configuration labels. Inspect summaries."
    )
  )
}


configuration_df <- configuration_df %>%
  
  left_join(
    
    configuration_labels,
    
    by =
      "Configuration_raw"
  ) %>%
  
  mutate(
    
    Configuration =
      factor(
        
        Configuration,
        
        levels =
          configuration_order
      ),
    
    
    Country_plot =
      country_plot_name(
        Country,
        ISO3
      )
  )


configuration_label_diagnostics <- configuration_summary %>%
  
  left_join(
    
    configuration_labels,
    
    by =
      "Configuration_raw"
  ) %>%
  
  arrange(
    factor(
      Configuration,
      levels =
        configuration_order
    )
  )


write_csv(
  configuration_label_diagnostics,
  file.path(
    out_res_dir,
    "10_configuration_label_diagnostics.csv"
  )
)


write_csv(
  configuration_df,
  file.path(
    out_res_dir,
    "11_country_trajectory_configurations.csv"
  )
)


# ============================================================
# 25. CONFIGURATION COUNTS
# ============================================================

configuration_counts <- configuration_df %>%
  
  dplyr::count(
    
    Configuration,
    
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
  configuration_counts,
  file.path(
    out_res_dir,
    "12_configuration_counts.csv"
  )
)


# ============================================================
# 26. HIGHLIGHTED COUNTRIES
# ============================================================

highlight_points <- configuration_df %>%
  
  filter(
    ISO3 %in%
      highlight_codes
  )


# ============================================================
# 27. MAIN FIGURE PANEL A
#     TRAJECTORY-BASED N CONFIGURATIONS
# ============================================================

p_pca <- ggplot(
  
  configuration_df,
  
  aes(
    x =
      PC1,
    
    y =
      PC2
  )
  
) +
  
  geom_point(
    
    aes(
      color =
        Configuration
    ),
    
    size =
      2.2,
    
    alpha =
      0.75
  ) +
  
  geom_point(
    
    data =
      highlight_points,
    
    aes(
      fill =
        Configuration
    ),
    
    shape =
      21,
    
    size =
      3.2,
    
    stroke =
      0.7,
    
    color =
      "black",
    
    show.legend =
      FALSE
  ) +
  
  geom_text_repel(
    
    data =
      highlight_points,
    
    aes(
      label =
        Country_plot
    ),
    
    color =
      "black",
    
    size =
      3,
    
    fontface =
      "bold",
    
    show.legend =
      FALSE,
    
    max.overlaps =
      Inf,
    
    box.padding =
      0.4,
    
    point.padding =
      0.35,
    
    segment.color =
      "black",
    
    segment.size =
      0.25,
    
    seed =
      123
  ) +
  
  scale_color_manual(
    
    values =
      configuration_palette,
    
    drop =
      FALSE
  ) +
  
  scale_fill_manual(
    
    values =
      configuration_palette,
    
    drop =
      FALSE
  ) +
  
  labs(
    
    title =
      "a  Trajectory-based N configurations",
    
    x =
      paste0(
        "PC1 (",
        percent(
          pca_variance$variance_explained[1],
          accuracy =
            0.1
        ),
        ")"
      ),
    
    y =
      paste0(
        "PC2 (",
        percent(
          pca_variance$variance_explained[2],
          accuracy =
            0.1
        ),
        ")"
      ),
    
    color =
      NULL
  ) +
  
  theme_nature()


# ============================================================
# 28. LINK CONFIGURATIONS TO ANNUAL DATA
# ============================================================

df_configuration <- df %>%
  
  semi_join(
    
    configuration_df,
    
    by =
      "ISO3"
  ) %>%
  
  left_join(
    
    configuration_df %>%
      select(
        ISO3,
        Configuration
      ),
    
    by =
      "ISO3"
  )


# ============================================================
# 29. MEDIAN TRAJECTORIES
# ============================================================

trajectory_summary <- df_configuration %>%
  
  group_by(
    Configuration,
    Year
  ) %>%
  
  summarise(
    
    N_surplus_per_protein_mass =
      median(
        N_surplus_per_protein_mass,
        na.rm = TRUE
      ),
    
    N_surplus =
      median(
        N_surplus,
        na.rm = TRUE
      ),
    
    N_input_per_ha =
      median(
        N_input_per_ha,
        na.rm = TRUE
      ),
    
    Total_protein_supply =
      median(
        Total_protein_supply,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )


write_csv(
  trajectory_summary,
  file.path(
    out_res_dir,
    "13_configuration_median_trajectories.csv"
  )
)


# ============================================================
# 30. MAIN FIGURE PANEL B
#     N SURPLUS PER PROTEIN TRAJECTORIES
# ============================================================

p_traj_nsp <- ggplot(
  
  trajectory_summary,
  
  aes(
    x =
      Year,
    
    y =
      N_surplus_per_protein_mass,
    
    color =
      Configuration
  )
  
) +
  
  geom_line(
    linewidth =
      1
  ) +
  
  scale_color_manual(
    
    values =
      configuration_palette,
    
    drop =
      FALSE
  ) +
  
  labs(
    
    title =
      "b  Median N surplus per protein trajectories",
    
    x =
      "Year",
    
    y =
      expression(
        "N surplus per unit protein supply (kg N kg protein"^-1*")"
      ),
    
    color =
      NULL
  ) +
  
  theme_nature()


# ============================================================
# 31. MAIN FIGURE PANEL C
#     N SURPLUS TRAJECTORIES
# ============================================================

p_traj_surplus <- ggplot(
  
  trajectory_summary,
  
  aes(
    x =
      Year,
    
    y =
      N_surplus,
    
    color =
      Configuration
  )
  
) +
  
  geom_line(
    linewidth =
      1
  ) +
  
  scale_color_manual(
    
    values =
      configuration_palette,
    
    drop =
      FALSE
  ) +
  
  labs(
    
    title =
      "c  Median N surplus trajectories",
    
    x =
      "Year",
    
    y =
      expression(
        "Cropland N surplus (kg N ha"^-1*" yr"^-1*")"
      ),
    
    color =
      NULL
  ) +
  
  theme_nature()


# ============================================================
# 32. WORLD MAP
# ============================================================

world <- rnaturalearth::ne_countries(
  
  scale =
    "medium",
  
  returnclass =
    "sf"
) %>%
  
  mutate(
    
    ISO3 =
      iso_a3,
    
    ISO3 =
      case_when(
        
        ISO3 ==
          "-99" ~
          
          countrycode(
            admin,
            "country.name",
            "iso3c"
          ),
        
        TRUE ~
          ISO3
      )
  )


world_configuration <- world %>%
  
  left_join(
    
    configuration_df %>%
      select(
        ISO3,
        Configuration
      ),
    
    by =
      "ISO3"
  )


# ============================================================
# 33. MAP MATCHING QC
# ============================================================

countries_present_but_not_classified <- df %>%
  
  distinct(
    ISO3,
    Country
  ) %>%
  
  anti_join(
    
    configuration_df %>%
      distinct(
        ISO3
      ),
    
    by =
      "ISO3"
  )


countries_classified_but_not_mapped <- configuration_df %>%
  
  distinct(
    ISO3,
    Country,
    Configuration
  ) %>%
  
  anti_join(
    
    world %>%
      st_drop_geometry() %>%
      distinct(
        ISO3
      ),
    
    by =
      "ISO3"
  )


write_csv(
  countries_present_but_not_classified,
  file.path(
    out_res_dir,
    "14_countries_present_but_not_classified.csv"
  )
)


write_csv(
  countries_classified_but_not_mapped,
  file.path(
    out_res_dir,
    "14_countries_classified_but_not_mapped.csv"
  )
)


# ============================================================
# 34. MAIN FIGURE MAP
#
# Deliberately has NO panel title.
#
# It accompanies panel A spatially but is not treated as a
# separately lettered analytical panel.
# ============================================================

p_map <- ggplot(
  world_configuration
) +
  
  geom_sf(
    
    aes(
      fill =
        Configuration
    ),
    
    color =
      "white",
    
    linewidth =
      0.15,
    
    show.legend =
      FALSE
  ) +
  
  scale_fill_manual(
    
    values =
      configuration_palette,
    
    na.value =
      "grey80",
    
    drop =
      FALSE
  ) +
  
  theme_void() +
  
  theme(
    
    plot.margin =
      margin(
        6,
        8,
        6,
        8
      )
  )


# ============================================================
# 35. BUILD MAIN FIGURE 3
#
# TOP:
#   a PCA trajectory configuration | spatial map without title
#
# BOTTOM:
#   b N surplus/protein trajectory | c N surplus trajectory
# ============================================================

Figure3 <- (
  
  p_pca |
    p_map
  
) /
  
  (
    
    p_traj_nsp |
      p_traj_surplus
    
  ) +
  
  plot_layout(
    
    guides =
      "collect",
    
    heights =
      c(
        1.05,
        1
      )
  ) &
  
  theme(
    
    legend.position =
      "bottom",
    
    legend.title =
      element_blank(),
    
    legend.box =
      "horizontal"
  )


print(
  Figure3
)


# ============================================================
# 36. SAVE MAIN FIGURE 3
# ============================================================

ggsave(
  
  file.path(
    out_plot_dir,
    "Figure3_trajectory_configurations.png"
  ),
  
  Figure3,
  
  width =
    12,
  
  height =
    9,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Figure3_trajectory_configurations.pdf"
  ),
  
  Figure3,
  
  width =
    12,
  
  height =
    9
)


# ============================================================
# 37. SAVE MAIN PANELS
# ============================================================

ggsave(
  
  file.path(
    out_plot_dir,
    "Figure3a_trajectory_based_N_configurations.png"
  ),
  
  p_pca,
  
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
    "Figure3_spatial_configuration_map.png"
  ),
  
  p_map,
  
  width =
    7,
  
  height =
    4.5,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Figure3b_Nsurplus_per_protein_trajectories.png"
  ),
  
  p_traj_nsp,
  
  width =
    6,
  
  height =
    4.5,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Figure3c_Nsurplus_trajectories.png"
  ),
  
  p_traj_surplus,
  
  width =
    6,
  
  height =
    4.5,
  
  dpi =
    600
)


# ============================================================
# 38. CROSS-TAB WITH FIRST-LAST TRANSITION CLASSIFICATION
# ============================================================

if (file.exists(classification_file)) {
  
  transition_classification <-
    readRDS(
      classification_file
    ) %>%
    
    mutate(
      
      transition_class =
        as.character(
          transition_class
        )
    )
  
  
  missing_in_configuration <- transition_classification %>%
    
    anti_join(
      
      configuration_df %>%
        select(
          ISO3
        ),
      
      by =
        "ISO3"
    )
  
  
  missing_in_classification <- configuration_df %>%
    
    anti_join(
      
      transition_classification %>%
        select(
          ISO3
        ),
      
      by =
        "ISO3"
    )
  
  
  cross_tab_joined <- transition_classification %>%
    
    select(
      ISO3,
      Country,
      transition_class
    ) %>%
    
    inner_join(
      
      configuration_df %>%
        select(
          ISO3,
          Configuration
        ),
      
      by =
        "ISO3"
    )
  
  
  cross_tab_long <- cross_tab_joined %>%
    
    dplyr::count(
      
      transition_class,
      
      Configuration,
      
      name =
        "n"
    )
  
  
  cross_tab_rowprop <- cross_tab_long %>%
    
    group_by(
      transition_class
    ) %>%
    
    mutate(
      
      row_total =
        sum(
          n
        ),
      
      proportion_within_transition_class =
        n /
        row_total
    ) %>%
    
    ungroup()
  
  
  cross_tab_colprop <- cross_tab_long %>%
    
    group_by(
      Configuration
    ) %>%
    
    mutate(
      
      column_total =
        sum(
          n
        ),
      
      proportion_within_configuration =
        n /
        column_total
    ) %>%
    
    ungroup()
  
  
  cross_tab_wide <- cross_tab_long %>%
    
    pivot_wider(
      
      names_from =
        Configuration,
      
      values_from =
        n,
      
      values_fill =
        0
    )
  
  
  write_csv(
    missing_in_configuration,
    file.path(
      out_res_dir,
      "15_transition_countries_missing_from_configuration.csv"
    )
  )
  
  
  write_csv(
    missing_in_classification,
    file.path(
      out_res_dir,
      "15_configuration_countries_missing_from_transition_classification.csv"
    )
  )
  
  
  write_csv(
    cross_tab_joined,
    file.path(
      out_res_dir,
      "15_transition_class_vs_configuration_joined.csv"
    )
  )
  
  
  write_csv(
    cross_tab_long,
    file.path(
      out_res_dir,
      "15_transition_class_vs_configuration_long.csv"
    )
  )
  
  
  write_csv(
    cross_tab_rowprop,
    file.path(
      out_res_dir,
      "15_transition_class_vs_configuration_row_proportions.csv"
    )
  )
  
  
  write_csv(
    cross_tab_colprop,
    file.path(
      out_res_dir,
      "15_transition_class_vs_configuration_column_proportions.csv"
    )
  )
  
  
  write_csv(
    cross_tab_wide,
    file.path(
      out_res_dir,
      "15_transition_class_vs_configuration_wide.csv"
    )
  )
  
} else {
  
  warning(
    "Script 03 classification RDS not found. Cross-tab skipped."
  )
}


# ============================================================
# 39. SUPPLEMENTARY SCALING SENSITIVITY
# ============================================================
#
# The exact same country-level raw feature table is used.
#
# Only the scaling transformation changes.
# ============================================================


# ============================================================
# 40. FUNCTION TO LABEL CONFIGURATIONS UNDER EACH SCALING
# ============================================================

label_scaling_configurations <- function(
    configuration_data
) {
  
  summary_df <- configuration_data %>%
    
    group_by(
      Configuration_raw
    ) %>%
    
    summarise(
      
      n_countries =
        n(),
      
      
      mean_final_N_surplus_per_protein =
        mean(
          final_N_surplus_per_protein_mass,
          na.rm = TRUE
        ),
      
      
      mean_delta_N_surplus_per_protein =
        mean(
          delta_N_surplus_per_protein_mass,
          na.rm = TRUE
        ),
      
      
      mean_final_N_surplus =
        mean(
          final_N_surplus,
          na.rm = TRUE
        ),
      
      
      mean_delta_N_surplus =
        mean(
          delta_N_surplus,
          na.rm = TRUE
        ),
      
      
      mean_final_N_input_per_ha =
        mean(
          final_N_input_per_ha,
          na.rm = TRUE
        ),
      
      
      mean_delta_N_input_per_ha =
        mean(
          delta_N_input_per_ha,
          na.rm = TRUE
        ),
      
      
      mean_final_protein =
        mean(
          final_Total_protein_supply,
          na.rm = TRUE
        ),
      
      
      mean_delta_protein =
        mean(
          delta_Total_protein_supply,
          na.rm = TRUE
        ),
      
      
      .groups =
        "drop"
    )
  
  
  median_final_surplus <- median(
    
    summary_df$mean_final_N_surplus,
    
    na.rm =
      TRUE
  )
  
  
  median_final_input <- median(
    
    summary_df$mean_final_N_input_per_ha,
    
    na.rm =
      TRUE
  )
  
  
  labels_df <- summary_df %>%
    
    mutate(
      
      Configuration =
        case_when(
          
          
          mean_final_N_surplus <
            median_final_surplus &
            mean_final_N_input_per_ha <
            median_final_input ~
            
            "Lower-pressure configuration",
          
          
          mean_delta_N_input_per_ha > 0 &
            mean_delta_N_surplus > 0 ~
            
            "Intensifying configuration",
          
          
          TRUE ~
            
            "Declining-intensity configuration"
        )
    ) %>%
    
    select(
      Configuration_raw,
      Configuration
    )
  
  
  list(
    
    summary =
      summary_df,
    
    labels =
      labels_df
  )
}


# ============================================================
# 41. FUNCTION TO RUN ONE SCALING METHOD
# ============================================================

run_scaling_analysis <- function(
    features,
    feature_cols,
    method_name,
    scaling_type
) {
  
  message(
    "Running scaling sensitivity: ",
    method_name
  )
  
  
  scaled_data <- features
  
  
  # ----------------------------------------------------------
  # ROBUST + CONSTRAINED
  # ----------------------------------------------------------
  
  if (
    scaling_type ==
    "robust_constrained"
  ) {
    
    scaled_data <- scaled_data %>%
      
      mutate(
        
        across(
          
          all_of(
            feature_cols
          ),
          
          robust_scale,
          
          .names =
            "tmp_{.col}"
        )
      ) %>%
      
      mutate(
        
        across(
          
          starts_with(
            "tmp_"
          ),
          
          constrain_scaled,
          
          .names =
            "used_{.col}"
        )
      )
    
    
    method_cols <- names(
      scaled_data
    )[
      str_detect(
        names(
          scaled_data
        ),
        "^used_tmp_"
      )
    ]
    
    
    # ----------------------------------------------------------
    # Z-SCORE
    # ----------------------------------------------------------
    
  } else if (
    scaling_type ==
    "zscore"
  ) {
    
    scaled_data <- scaled_data %>%
      
      mutate(
        
        across(
          
          all_of(
            feature_cols
          ),
          
          z_scale,
          
          .names =
            "used_{.col}"
        )
      )
    
    
    method_cols <- names(
      scaled_data
    )[
      str_detect(
        names(
          scaled_data
        ),
        "^used_"
      )
    ]
    
    
    # ----------------------------------------------------------
    # MIN-MAX
    # ----------------------------------------------------------
    
  } else if (
    scaling_type ==
    "minmax"
  ) {
    
    scaled_data <- scaled_data %>%
      
      mutate(
        
        across(
          
          all_of(
            feature_cols
          ),
          
          minmax_scale,
          
          .names =
            "used_{.col}"
        )
      )
    
    
    method_cols <- names(
      scaled_data
    )[
      str_detect(
        names(
          scaled_data
        ),
        "^used_"
      )
    ]
    
    
    # ----------------------------------------------------------
    # SIGNED LOG + Z-SCORE
    # ----------------------------------------------------------
    
  } else if (
    scaling_type ==
    "signedlog_zscore"
  ) {
    
    scaled_data <- scaled_data %>%
      
      mutate(
        
        across(
          
          all_of(
            feature_cols
          ),
          
          signed_log10,
          
          .names =
            "log_{.col}"
        )
      )
    
    
    log_cols <- names(
      scaled_data
    )[
      str_detect(
        names(
          scaled_data
        ),
        "^log_"
      )
    ]
    
    
    scaled_data <- scaled_data %>%
      
      mutate(
        
        across(
          
          all_of(
            log_cols
          ),
          
          z_scale,
          
          .names =
            "used_{.col}"
        )
      )
    
    
    method_cols <- names(
      scaled_data
    )[
      str_detect(
        names(
          scaled_data
        ),
        "^used_log_"
      )
    ]
    
    
  } else {
    
    stop(
      "Unknown scaling method: ",
      scaling_type
    )
  }
  
  
  # ----------------------------------------------------------
  # MATRIX
  # ----------------------------------------------------------
  
  X_method <- scaled_data %>%
    
    select(
      all_of(
        method_cols
      )
    ) %>%
    
    as.matrix()
  
  
  rownames(
    X_method
  ) <-
    scaled_data$ISO3
  
  
  if (
    any(
      !is.finite(
        X_method
      )
    )
  ) {
    
    stop(
      "Non-finite values detected under scaling method: ",
      method_name
    )
  }
  
  
  # ----------------------------------------------------------
  # PCA
  # ----------------------------------------------------------
  
  pca_method <- prcomp(
    
    X_method,
    
    center =
      FALSE,
    
    scale. =
      FALSE
  )
  
  
  pca_variance_method <- tibble(
    
    Method =
      method_name,
    
    PC =
      paste0(
        "PC",
        seq_along(
          pca_method$sdev
        )
      ),
    
    variance_explained =
      pca_method$sdev^2 /
      sum(
        pca_method$sdev^2
      ),
    
    cumulative_variance =
      cumsum(
        variance_explained
      )
  )
  
  
  pca_scores_method <- as_tibble(
    
    pca_method$x[
      ,
      1:4,
      drop =
        FALSE
    ]
    
  ) %>%
    
    bind_cols(
      
      scaled_data %>%
        select(
          ISO3,
          Country
        )
    )
  
  
  # ----------------------------------------------------------
  # k-MEANS
  # ----------------------------------------------------------
  
  set.seed(123)
  
  
  km_method <- kmeans(
    
    X_method,
    
    centers =
      k_final,
    
    nstart =
      500
  )
  
  
  configuration_method <- scaled_data %>%
    
    mutate(
      
      Configuration_raw =
        factor(
          km_method$cluster
        )
    ) %>%
    
    left_join(
      
      pca_scores_method,
      
      by =
        c(
          "ISO3",
          "Country"
        )
    )
  
  
  # ----------------------------------------------------------
  # INTERPRETATIVE LABELS
  # ----------------------------------------------------------
  
  labelling <- label_scaling_configurations(
    configuration_method
  )
  
  
  configuration_method <- configuration_method %>%
    
    left_join(
      
      labelling$labels,
      
      by =
        "Configuration_raw"
    ) %>%
    
    mutate(
      
      Method =
        method_name,
      
      
      Configuration =
        factor(
          
          Configuration,
          
          levels =
            configuration_order
        ),
      
      
      Country_plot =
        country_plot_name(
          Country,
          ISO3
        )
    )
  
  
  summary_method <- labelling$summary %>%
    
    left_join(
      
      labelling$labels,
      
      by =
        "Configuration_raw"
    ) %>%
    
    mutate(
      Method =
        method_name
    )
  
  
  # ----------------------------------------------------------
  # COMPARE WITH MAIN CONFIGURATION
  # ----------------------------------------------------------
  
  main_reference <- configuration_df %>%
    
    select(
      
      ISO3,
      
      Configuration_main =
        Configuration
    ) %>%
    
    mutate(
      
      Configuration_main =
        as.character(
          Configuration_main
        )
    )
  
  
  comparison <- configuration_method %>%
    
    transmute(
      
      ISO3,
      
      Country,
      
      Method,
      
      Configuration_scaling =
        as.character(
          Configuration
        )
    ) %>%
    
    left_join(
      
      main_reference,
      
      by =
        "ISO3"
    ) %>%
    
    mutate(
      
      same_configuration =
        Configuration_scaling ==
        Configuration_main
    )
  
  
  ari <- mclust::adjustedRandIndex(
    
    comparison$Configuration_main,
    
    comparison$Configuration_scaling
  )
  
  
  summary_agreement <- tibble(
    
    Method =
      method_name,
    
    n_compared =
      nrow(
        comparison
      ),
    
    adjusted_rand_index =
      ari,
    
    n_same_configuration =
      sum(
        comparison$same_configuration,
        na.rm =
          TRUE
      ),
    
    pct_same_configuration =
      100 *
      mean(
        comparison$same_configuration,
        na.rm =
          TRUE
      ),
    
    PC1_variance =
      pca_variance_method$variance_explained[1],
    
    PC2_variance =
      pca_variance_method$variance_explained[2],
    
    PC1_PC2_cumulative =
      sum(
        pca_variance_method$variance_explained[
          1:2
        ]
      )
  )
  
  
  list(
    
    method =
      method_name,
    
    matrix =
      X_method,
    
    pca =
      pca_method,
    
    pca_variance =
      pca_variance_method,
    
    configuration_df =
      configuration_method,
    
    configuration_summary =
      summary_method,
    
    comparison =
      comparison,
    
    agreement =
      summary_agreement
  )
}


# ============================================================
# 42. DEFINE SCALING METHODS
# ============================================================

scaling_methods <- tribble(
  
  ~method_name,
  ~scaling_type,
  
  "Robust + constrained",
  "robust_constrained",
  
  "Z-score",
  "zscore",
  
  "Min-max",
  "minmax",
  
  "Signed log + z-score",
  "signedlog_zscore"
)


# ============================================================
# 43. RUN SCALING SENSITIVITY
# ============================================================

scaling_results <- purrr::pmap(
  
  scaling_methods,
  
  ~ run_scaling_analysis(
    
    features =
      features_complete,
    
    feature_cols =
      feature_cols,
    
    method_name =
      ..1,
    
    scaling_type =
      ..2
  )
)


names(
  scaling_results
) <-
  scaling_methods$method_name


# ============================================================
# 44. CONSOLIDATE SCALING RESULTS
# ============================================================

scaling_agreement <- map_dfr(
  scaling_results,
  "agreement"
)


scaling_comparisons <- map_dfr(
  scaling_results,
  "comparison"
)


scaling_configurations <- map_dfr(
  scaling_results,
  "configuration_df"
)


scaling_configuration_summaries <- map_dfr(
  scaling_results,
  "configuration_summary"
)


scaling_pca_variance <- map_dfr(
  scaling_results,
  "pca_variance"
)


write_csv(
  scaling_agreement,
  file.path(
    out_res_dir,
    "16_scaling_method_agreement_summary.csv"
  )
)


write_csv(
  scaling_comparisons,
  file.path(
    out_res_dir,
    "16_scaling_country_configuration_comparison.csv"
  )
)


write_csv(
  scaling_configurations,
  file.path(
    out_res_dir,
    "16_scaling_country_configurations.csv"
  )
)


write_csv(
  scaling_configuration_summaries,
  file.path(
    out_res_dir,
    "16_scaling_configuration_summaries.csv"
  )
)


write_csv(
  scaling_pca_variance,
  file.path(
    out_res_dir,
    "16_scaling_PCA_variance.csv"
  )
)


# ============================================================
# 45. MAIN-SCALING REPRODUCTION CHECK
# ============================================================

main_scaling_check <- scaling_agreement %>%
  
  filter(
    Method ==
      "Robust + constrained"
  )


if (
  nrow(
    main_scaling_check
  ) != 1 ||
  abs(
    main_scaling_check$adjusted_rand_index -
    1
  ) > 1e-12 ||
  main_scaling_check$pct_same_configuration <
  99.999
) {
  
  stop(
    paste0(
      "CRITICAL: Robust + constrained sensitivity analysis ",
      "does not exactly reproduce the main configuration."
    )
  )
}


# ============================================================
# 46. CONFIGURATION COUNTS ACROSS SCALING METHODS
# ============================================================

scaling_configuration_counts <- scaling_configurations %>%
  
  dplyr::count(
    
    Method,
    
    Configuration,
    
    name =
      "n_countries"
  ) %>%
  
  group_by(
    Method
  ) %>%
  
  mutate(
    
    percentage =
      100 *
      n_countries /
      sum(
        n_countries
      )
  ) %>%
  
  ungroup()


write_csv(
  scaling_configuration_counts,
  file.path(
    out_res_dir,
    "17_scaling_configuration_counts.csv"
  )
)


# ============================================================
# 47. COUNTRIES CHANGING CONFIGURATION
# ============================================================

countries_changing_scaling <- scaling_comparisons %>%
  
  filter(
    !same_configuration
  ) %>%
  
  arrange(
    Method,
    Configuration_main,
    Configuration_scaling,
    Country
  )


write_csv(
  countries_changing_scaling,
  file.path(
    out_res_dir,
    "18_countries_changing_configuration_across_scaling.csv"
  )
)


# ============================================================
# 48. SUPPLEMENTARY SCALING PANEL A
#     ADJUSTED RAND INDEX
# ============================================================

method_order <- c(
  
  "Robust + constrained",
  
  "Z-score",
  
  "Signed log + z-score",
  
  "Min-max"
)


scaling_plot_summary <- scaling_agreement %>%
  
  mutate(
    
    Method =
      factor(
        
        Method,
        
        levels =
          rev(
            method_order
          )
      )
  )


p_scaling_ari <- ggplot(
  
  scaling_plot_summary,
  
  aes(
    x =
      Method,
    
    y =
      adjusted_rand_index
  )
  
) +
  
  geom_col(
    width =
      0.68
  ) +
  
  geom_text(
    
    aes(
      label =
        sprintf(
          "%.2f",
          adjusted_rand_index
        )
    ),
    
    hjust =
      -0.15,
    
    size =
      3.2
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    
    limits =
      c(
        0,
        1.15
      ),
    
    breaks =
      c(
        0,
        0.25,
        0.50,
        0.75,
        1
      )
  ) +
  
  labs(
    
    title =
      "a  Clustering agreement with main analysis",
    
    x =
      NULL,
    
    y =
      "Adjusted Rand Index"
  ) +
  
  theme_nature(
    base_size =
      9
  )


# ============================================================
# 49. SUPPLEMENTARY SCALING PANEL B
#     SAME CONFIGURATION
# ============================================================

p_scaling_same <- ggplot(
  
  scaling_plot_summary,
  
  aes(
    x =
      Method,
    
    y =
      pct_same_configuration
  )
  
) +
  
  geom_col(
    width =
      0.68
  ) +
  
  geom_text(
    
    aes(
      label =
        paste0(
          sprintf(
            "%.1f",
            pct_same_configuration
          ),
          "%"
        )
    ),
    
    hjust =
      -0.15,
    
    size =
      3.2
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    
    limits =
      c(
        0,
        108
      ),
    
    breaks =
      c(
        0,
        25,
        50,
        75,
        100
      )
  ) +
  
  labs(
    
    title =
      "b  Countries retaining the same configuration",
    
    x =
      NULL,
    
    y =
      "Countries with same configuration (%)"
  ) +
  
  theme_nature(
    base_size =
      9
  )


# ============================================================
# 50. SUPPLEMENTARY PCA PLOTTING FUNCTION
# ============================================================

plot_scaling_pca <- function(
    method_name
) {
  
  dat <- scaling_configurations %>%
    
    filter(
      Method ==
        method_name
    )
  
  
  var_dat <- scaling_pca_variance %>%
    
    filter(
      Method ==
        method_name
    )
  
  
  PC1_var <- var_dat$variance_explained[
    var_dat$PC ==
      "PC1"
  ]
  
  
  PC2_var <- var_dat$variance_explained[
    var_dat$PC ==
      "PC2"
  ]
  
  
  highlighted <- dat %>%
    
    filter(
      ISO3 %in%
        highlight_codes
    )
  
  
  ggplot(
    
    dat,
    
    aes(
      x =
        PC1,
      
      y =
        PC2
    )
    
  ) +
    
    geom_point(
      
      aes(
        color =
          Configuration
      ),
      
      size =
        1.7,
      
      alpha =
        0.72
    ) +
    
    geom_point(
      
      data =
        highlighted,
      
      aes(
        fill =
          Configuration
      ),
      
      shape =
        21,
      
      size =
        2.8,
      
      stroke =
        0.6,
      
      color =
        "black",
      
      show.legend =
        FALSE
    ) +
    
    geom_text_repel(
      
      data =
        highlighted,
      
      aes(
        label =
          Country_plot
      ),
      
      color =
        "black",
      
      size =
        2.35,
      
      fontface =
        "bold",
      
      show.legend =
        FALSE,
      
      max.overlaps =
        Inf,
      
      box.padding =
        0.35,
      
      point.padding =
        0.30,
      
      segment.color =
        "black",
      
      segment.size =
        0.20,
      
      seed =
        123
    ) +
    
    scale_color_manual(
      
      values =
        configuration_palette,
      
      drop =
        FALSE
    ) +
    
    scale_fill_manual(
      
      values =
        configuration_palette,
      
      drop =
        FALSE
    ) +
    
    labs(
      
      title =
        method_name,
      
      x =
        paste0(
          "PC1 (",
          percent(
            PC1_var,
            accuracy =
              1
          ),
          ")"
        ),
      
      y =
        paste0(
          "PC2 (",
          percent(
            PC2_var,
            accuracy =
              1
          ),
          ")"
        ),
      
      color =
        NULL
    ) +
    
    theme_nature(
      base_size =
        8
    )
}


# ============================================================
# 51. SUPPLEMENTARY PCA PANELS
# ============================================================

p_scaling_robust <- plot_scaling_pca(
  "Robust + constrained"
)


p_scaling_z <- plot_scaling_pca(
  "Z-score"
)


p_scaling_minmax <- plot_scaling_pca(
  "Min-max"
)


p_scaling_signedlog <- plot_scaling_pca(
  "Signed log + z-score"
)


# ============================================================
# 52. BUILD SUPPLEMENTARY SCALING FIGURE
# ============================================================

Supplementary_scaling <- (
  
  p_scaling_ari |
    p_scaling_same
  
) /
  
  (
    
    p_scaling_robust |
      p_scaling_z
    
  ) /
  
  (
    
    p_scaling_minmax |
      p_scaling_signedlog
    
  ) +
  
  plot_layout(
    guides =
      "collect"
  ) &
  
  theme(
    
    legend.position =
      "bottom",
    
    legend.title =
      element_blank()
  )


print(
  Supplementary_scaling
)


# ============================================================
# 53. SAVE SUPPLEMENTARY SCALING FIGURE
# ============================================================

ggsave(
  
  file.path(
    out_plot_dir,
    "Supplementary_scaling_method_sensitivity.png"
  ),
  
  Supplementary_scaling,
  
  width =
    12,
  
  height =
    12.5,
  
  dpi =
    600
)


ggsave(
  
  file.path(
    out_plot_dir,
    "Supplementary_scaling_method_sensitivity.pdf"
  ),
  
  Supplementary_scaling,
  
  width =
    12,
  
  height =
    12.5
)


# ============================================================
# 54. MANUSCRIPT ANCHORS
# ============================================================

manuscript_anchors <- bind_rows(
  
  tibble(
    
    result = c(
      
      "Countries in configuration analysis",
      
      "Trajectory features",
      
      "Final number of configurations",
      
      "PC1 variance explained",
      
      "PC2 variance explained",
      
      "PC1 plus PC2 cumulative variance",
      
      "Mean silhouette at k3"
    ),
    
    value = c(
      
      nrow(
        features_complete
      ),
      
      length(
        feature_cols
      ),
      
      k_final,
      
      100 *
        pca_variance$variance_explained[1],
      
      100 *
        pca_variance$variance_explained[2],
      
      100 *
        sum(
          pca_variance$variance_explained[
            1:2
          ]
        ),
      
      cluster_diagnostics$mean_silhouette[
        cluster_diagnostics$k ==
          3
      ]
    ),
    
    units = c(
      
      "countries",
      
      "features",
      
      "configurations",
      
      "%",
      
      "%",
      
      "%",
      
      "silhouette width"
    )
  ),
  
  
  configuration_counts %>%
    
    transmute(
      
      result =
        paste0(
          as.character(
            Configuration
          ),
          "_n"
        ),
      
      value =
        n_countries,
      
      units =
        "countries"
    ),
  
  
  configuration_counts %>%
    
    transmute(
      
      result =
        paste0(
          as.character(
            Configuration
          ),
          "_percentage"
        ),
      
      value =
        percentage,
      
      units =
        "%"
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
# 55. SAVE CONSOLIDATED RDS
# ============================================================
#
# This object contains everything required to rebuild Figure 3
# and its directly associated supplementary figures later without
# rerunning the full analysis.
# ============================================================

figure3_objects <- list(
  
  raw_data =
    df,
  
  country_features_long =
    country_features_long,
  
  features_raw =
    features_for_clustering,
  
  features_complete =
    features_complete,
  
  main_features_scaled =
    features_scaled,
  
  main_clustering_matrix =
    X,
  
  main_pca =
    pca,
  
  pca_variance =
    pca_variance,
  
  pca_scores =
    pca_scores,
  
  pca_loadings =
    pca_loadings,
  
  cluster_diagnostics =
    cluster_diagnostics,
  
  kmeans_final =
    km_final,
  
  configuration_df =
    configuration_df,
  
  configuration_summary =
    configuration_summary,
  
  configuration_label_diagnostics =
    configuration_label_diagnostics,
  
  configuration_counts =
    configuration_counts,
  
  trajectory_summary =
    trajectory_summary,
  
  configuration_palette =
    configuration_palette,
  
  scaling_results =
    scaling_results,
  
  scaling_agreement =
    scaling_agreement,
  
  scaling_comparisons =
    scaling_comparisons,
  
  scaling_configurations =
    scaling_configurations,
  
  scaling_configuration_summaries =
    scaling_configuration_summaries,
  
  scaling_pca_variance =
    scaling_pca_variance,
  
  p_pca =
    p_pca,
  
  p_map =
    p_map,
  
  p_traj_nsp =
    p_traj_nsp,
  
  p_traj_surplus =
    p_traj_surplus,
  
  Figure3 =
    Figure3,
  
  Supplementary_k_diagnostics =
    Supplementary_k_diagnostics,
  
  Supplementary_scaling =
    Supplementary_scaling
)


if (exists("cross_tab_joined")) {
  
  figure3_objects$cross_tab_joined <-
    cross_tab_joined
  
  figure3_objects$cross_tab_long <-
    cross_tab_long
  
  figure3_objects$cross_tab_rowprop <-
    cross_tab_rowprop
  
  figure3_objects$cross_tab_colprop <-
    cross_tab_colprop
}


saveRDS(
  
  figure3_objects,
  
  file.path(
    out_res_dir,
    "Figure3_objects.rds"
  )
)


# ============================================================
# 56. SAVE COUNTRY CONFIGURATION ALONE
# ============================================================

saveRDS(
  
  configuration_df,
  
  file.path(
    out_res_dir,
    "country_trajectory_configurations.rds"
  )
)


# ============================================================
# 57. REPRODUCIBILITY
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
# 58. FINAL CONSOLE SUMMARY
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "FIGURE 3 - TRAJECTORY-BASED N CONFIGURATIONS\n"
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
  "\n--- MASS-HARMONIZED METRIC QC ---\n"
)

print(
  metric_qc,
  n = Inf,
  width = Inf
)


cat(
  "\n--- FEATURE COVERAGE ---\n"
)

print(
  feature_coverage_qc,
  n = Inf,
  width = Inf
)


cat(
  "\n--- PCA VARIANCE ---\n"
)

print(
  
  pca_variance %>%
    slice_head(
      n =
        6
    ),
  
  n = Inf,
  width = Inf
)


cat(
  "\n--- CLUSTER NUMBER DIAGNOSTICS ---\n"
)

print(
  cluster_diagnostics,
  n = Inf,
  width = Inf
)


cat(
  "\n--- CONFIGURATION LABEL DIAGNOSTICS ---\n"
)

print(
  configuration_label_diagnostics,
  n = Inf,
  width = Inf
)


cat(
  "\n--- FINAL CONFIGURATION COUNTS ---\n"
)

print(
  configuration_counts,
  n = Inf,
  width = Inf
)


cat(
  "\n--- SCALING METHOD SENSITIVITY ---\n"
)

print(
  scaling_agreement,
  n = Inf,
  width = Inf
)


cat(
  "\n--- CONFIGURATION COUNTS ACROSS SCALING METHODS ---\n"
)

print(
  scaling_configuration_counts,
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
  "- PCA is used for visualization; k-means uses the full 17-feature matrix.\n"
)

cat(
  "- Main analysis uses robust scaling followed by constraining scaled values to [-5,+5].\n"
)

cat(
  "- k = 3 is evaluated against k = 2-6 using elbow and silhouette diagnostics.\n"
)

cat(
  "- Lower-pressure, declining-intensity and intensifying configurations are comparative trajectory labels.\n"
)

cat(
  "- They are not discrete natural states or threshold-defined transition classes.\n"
)

cat(
  "- First-to-last transition classes from Script 03 are a distinct analytical construct.\n"
)

cat(
  "- Scaling sensitivity tests robustness; it is not used to choose the main scaling method post hoc.\n"
)

cat(
  "- Panel A uses the corrected mass-harmonized N-surplus/protein metric exclusively.\n"
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
# END OF 04_Figure3_trajectory_configurations.R
# ============================================================