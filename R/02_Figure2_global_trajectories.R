# ============================================================
# 02_Figure2_global_trajectories.R
#
# Divergent nitrogen transition pathways during
# global agricultural development
#
# FIGURE 2
# Global nitrogen-transition trajectories
#
# PURPOSE
# -------
# Visualize longitudinal national trajectories across three
# transition spaces:
#
#   A) GDP per capita vs N surplus per unit of protein supply
#   B) N input intensity vs cropland N surplus
#   C) Protein supply vs cropland N surplus
#
# VISUAL STRATEGY
# ---------------
# - Grey hexbins:
#     raw annual country-year observations.
#
# - Grey national trajectories:
#     centred 3-year rolling means.
#
# - Highlighted national trajectories:
#     centred 3-year rolling means.
#
# - Small coloured points:
#     annual positions along highlighted rolling trajectories.
#
# - Large coloured point:
#     final available rolling-mean position (2022).
#
# - Labels:
#     final position of highlighted countries.
#
# IMPORTANT
# ---------
# Highlighted countries are illustrative examples selected only
# to aid visualization and interpretation. They receive no
# differential weighting or treatment in any quantitative
# analysis.
#
# PANEL A
# -------
# Uses the DEFINITIVE mass-harmonized indicator:
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
# This is NOT a consumption-based nitrogen footprint.
#
# ROLLING MEANS
# -------------
# Centred 3-year rolling means are used only to facilitate
# visualization of national trajectories.
#
# For boundary years (1992 and 2022), .complete = FALSE means
# that the rolling mean is based on the available adjacent years.
#
# These smoothed endpoints must NOT be confused with the annual
# 1992 and 2022 endpoints used later for transition
# classification.
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
  "slider",
  "ggrepel",
  "patchwork",
  "scales",
  "hexbin"
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
  library(slider)
  library(ggrepel)
  library(patchwork)
  library(scales)
  library(hexbin)
  
})


# ============================================================
# 3. PATHS
# ============================================================

base_dir <- "."

data_dir <- file.path(
  base_dir,
  "data"
)

plots_dir <- file.path(
  base_dir,
  "plots"
)

results_dir <- file.path(
  base_dir,
  "results"
)

input_file <- file.path(
  data_dir,
  "Data_Final_31.csv"
)

out_plot_dir <- file.path(
  plots_dir,
  "02_Figure2_global_trajectories"
)

out_data_dir <- file.path(
  results_dir,
  "02_Figure2_global_trajectories"
)

dir.create(
  out_plot_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  out_data_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 4. LOAD DATA
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
# 5. REQUIRED INPUT VARIABLES
# ============================================================

required_vars <- c(
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
# 6. PREPARE CORE VARIABLES
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
  ) %>%
  
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
    # Definitive mass-harmonized indicator
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
# 7. STRUCTURAL QC
# ============================================================

duplicate_country_years <- df %>%
  count(
    ISO3,
    Year
  ) %>%
  filter(
    n > 1
  )

if (nrow(duplicate_country_years) > 0) {
  
  write_csv(
    duplicate_country_years,
    file.path(
      out_data_dir,
      "QC_duplicate_country_years.csv"
    )
  )
  
  stop(
    "Duplicated ISO3-Year combinations detected."
  )
}


figure2_qc <- tibble(
  
  metric = c(
    "Rows",
    "Countries",
    "Minimum year",
    "Maximum year",
    "Missing GDP",
    "Missing N input intensity",
    "Missing total protein supply",
    "Missing cropland N surplus",
    "Missing definitive N surplus per protein",
    "Negative cropland N surplus observations"
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
        df$N_input_per_ha
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
        df$N_surplus_per_protein_mass
      )
    ),
    
    sum(
      df$N_surplus < 0,
      na.rm = TRUE
    )
  )
)

write_csv(
  figure2_qc,
  file.path(
    out_data_dir,
    "01_Figure2_QC.csv"
  )
)


# ============================================================
# 8. CHECK MASS-HARMONIZED INDICATOR
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
      na.rm = TRUE
    )
)


write_csv(
  metric_qc,
  file.path(
    out_data_dir,
    "02_metric_reconstruction_QC.csv"
  )
)


# ============================================================
# 9. HIGHLIGHTED COUNTRIES
# ============================================================
#
# Same illustrative countries as in the previous Figure 2.
#
# They are used only to aid visualization.
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


country_labels <- c(
  
  CHN = "China",
  IND = "India",
  USA = "USA",
  BRA = "Brazil",
  NLD = "Netherlands",
  ZAF = "South Africa",
  KOR = "South Korea",
  NGA = "Nigeria",
  ARG = "Argentina"
)


country_order <- c(
  
  "China",
  "India",
  "USA",
  "Brazil",
  "Netherlands",
  "South Africa",
  "South Korea",
  "Nigeria",
  "Argentina"
)


# ============================================================
# 10. HIGHLIGHT VARIABLES
# ============================================================

df <- df %>%
  
  mutate(
    
    Country_plot =
      if_else(
        
        ISO3 %in%
          highlight_codes,
        
        recode(
          ISO3,
          !!!country_labels
        ),
        
        Country
      ),
    
    Highlight =
      if_else(
        
        ISO3 %in%
          highlight_codes,
        
        Country_plot,
        
        "Other countries"
      ),
    
    Highlight =
      factor(
        
        Highlight,
        
        levels =
          c(
            country_order,
            "Other countries"
          )
      )
  )


# ============================================================
# 11. CHECK HIGHLIGHTED COUNTRIES
# ============================================================

highlight_country_check <- tibble(
  
  ISO3 =
    highlight_codes,
  
  expected_name =
    unname(
      country_labels[
        highlight_codes
      ]
    )
) %>%
  
  left_join(
    
    df %>%
      distinct(
        ISO3,
        Country
      ),
    
    by =
      "ISO3"
  )


if (
  any(
    is.na(
      highlight_country_check$Country
    )
  )
) {
  
  stop(
    "One or more highlighted countries are absent from the dataset."
  )
}


write_csv(
  highlight_country_check,
  file.path(
    out_data_dir,
    "03_highlighted_country_check.csv"
  )
)


# ============================================================
# 12. THREE-YEAR CENTRED ROLLING MEAN
# ============================================================
#
# Raw annual values are retained for the hexbin background.
#
# Rolling means are used only for national trajectories.
#
# .before = 1
# .after  = 1
# .complete = FALSE
#
# Therefore:
#
# - internal years generally use 3 observations;
# - 1992 uses 1992-1993;
# - 2022 uses 2021-2022.
# ============================================================

rolling_mean_3yr <- function(x) {
  
  slider::slide_dbl(
    
    x,
    
    ~ mean(
      .x,
      na.rm = TRUE
    ),
    
    .before = 1,
    
    .after = 1,
    
    .complete = FALSE
  )
}


smooth_vars <- c(
  
  "GDP_pc_constant2015USD",
  
  "N_surplus_per_protein_mass",
  
  "N_input_per_ha",
  
  "N_surplus",
  
  "Total_protein_supply"
)


df_roll <- df %>%
  
  arrange(
    ISO3,
    Year
  ) %>%
  
  group_by(
    ISO3
  ) %>%
  
  mutate(
    
    across(
      
      all_of(
        smooth_vars
      ),
      
      rolling_mean_3yr,
      
      .names =
        "{.col}_roll3"
    )
  ) %>%
  
  ungroup()


# ============================================================
# 13. CHECK ROLLING-MEAN COVERAGE
# ============================================================

rolling_qc <- df_roll %>%
  
  summarise(
    
    n_rows =
      n(),
    
    countries =
      n_distinct(
        ISO3
      ),
    
    missing_GDP_roll3 =
      sum(
        !is.finite(
          GDP_pc_constant2015USD_roll3
        )
      ),
    
    missing_N_surplus_intensity_roll3 =
      sum(
        !is.finite(
          N_surplus_per_protein_mass_roll3
        )
      ),
    
    missing_N_input_roll3 =
      sum(
        !is.finite(
          N_input_per_ha_roll3
        )
      ),
    
    missing_N_surplus_roll3 =
      sum(
        !is.finite(
          N_surplus_roll3
        )
      ),
    
    missing_protein_roll3 =
      sum(
        !is.finite(
          Total_protein_supply_roll3
        )
      )
  )


write_csv(
  rolling_qc,
  file.path(
    out_data_dir,
    "04_rolling_mean_QC.csv"
  )
)


# ============================================================
# 14. SAVE ROLLING DATASET
# ============================================================

write_csv(
  df_roll,
  file.path(
    out_data_dir,
    "Figure2_global_dataset_roll3.csv"
  )
)


saveRDS(
  df_roll,
  file.path(
    out_data_dir,
    "Figure2_global_dataset_roll3.rds"
  )
)


# ============================================================
# 15. PLOT THEME
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
      
      legend.position =
        "bottom",
      
      legend.title =
        element_blank(),
      
      legend.text =
        element_text(
          size =
            base_size - 1
        ),
      
      plot.title =
        element_text(
          face =
            "bold",
          size =
            base_size + 1
        ),
      
      plot.caption =
        element_text(
          size =
            base_size - 2,
          color =
            "grey35"
        ),
      
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
# 16. COUNTRY PALETTE
# ============================================================

country_palette <- c(
  
  "China" =
    "#D55E00",
  
  "India" =
    "#E69F00",
  
  "USA" =
    "#0072B2",
  
  "Brazil" =
    "#009E73",
  
  "Netherlands" =
    "#CC79A7",
  
  "South Africa" =
    "#56B4E9",
  
  "South Korea" =
    "#7B3294",
  
  "Nigeria" =
    "#000000",
  
  "Argentina" =
    "#A6761D"
)


# ============================================================
# 17. GENERIC LANDSCAPE PLOTTING FUNCTION
# ============================================================

plot_landscape_panel <- function(
    data,
    x_raw,
    y_raw,
    x_roll,
    y_roll,
    xlab,
    ylab,
    panel_title,
    log_x = FALSE,
    x_limits = NULL,
    y_limits = NULL
) {
  
  # ----------------------------------------------------------
  # Complete cases for each panel
  # ----------------------------------------------------------
  
  data_plot <- data %>%
    
    filter(
      
      is.finite(
        .data[[
          x_raw
        ]]
      ),
      
      is.finite(
        .data[[
          y_raw
        ]]
      ),
      
      is.finite(
        .data[[
          x_roll
        ]]
      ),
      
      is.finite(
        .data[[
          y_roll
        ]]
      )
    )
  
  
  # ----------------------------------------------------------
  # Non-highlighted countries
  # ----------------------------------------------------------
  
  data_other <- data_plot %>%
    
    filter(
      !(ISO3 %in%
          highlight_codes)
    )
  
  
  # ----------------------------------------------------------
  # Highlighted countries
  # ----------------------------------------------------------
  
  data_highlight <- data_plot %>%
    
    filter(
      ISO3 %in%
        highlight_codes
    ) %>%
    
    mutate(
      
      Country_plot =
        factor(
          Country_plot,
          levels =
            country_order
        )
    )
  
  
  # ----------------------------------------------------------
  # Final trajectory positions
  # ----------------------------------------------------------
  
  end_data <- data_highlight %>%
    
    group_by(
      ISO3,
      Country_plot
    ) %>%
    
    arrange(
      Year,
      .by_group = TRUE
    ) %>%
    
    summarise(
      
      Year =
        last(
          Year
        ),
      
      x =
        last(
          .data[[
            x_roll
          ]]
        ),
      
      y =
        last(
          .data[[
            y_roll
          ]]
        ),
      
      .groups =
        "drop"
    )
  
  
  label_data <- end_data %>%
    
    mutate(
      label =
        as.character(
          Country_plot
        )
    )
  
  
  # ----------------------------------------------------------
  # Plot
  # ----------------------------------------------------------
  
  p <- ggplot() +
    
    # --------------------------------------------------------
  # Raw global country-year occupancy
  # --------------------------------------------------------
  
  geom_hex(
    
    data =
      data_plot,
    
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
    
    bins =
      55,
    
    alpha =
      0.62
  ) +
    
    scale_fill_gradient(
      
      low =
        "grey96",
      
      high =
        "grey38",
      
      trans =
        "sqrt",
      
      guide =
        "none"
    ) +
    
    
    # --------------------------------------------------------
  # Background rolling national trajectories
  # --------------------------------------------------------
  
  geom_path(
    
    data =
      data_other,
    
    aes(
      x =
        .data[[
          x_roll
        ]],
      y =
        .data[[
          y_roll
        ]],
      group =
        ISO3
    ),
    
    color =
      "grey62",
    
    alpha =
      0.12,
    
    linewidth =
      0.20
  ) +
    
    
    # --------------------------------------------------------
  # Highlighted rolling trajectories
  # --------------------------------------------------------
  
  geom_path(
    
    data =
      data_highlight,
    
    aes(
      x =
        .data[[
          x_roll
        ]],
      y =
        .data[[
          y_roll
        ]],
      color =
        Country_plot,
      group =
        Country_plot
    ),
    
    linewidth =
      1.05,
    
    alpha =
      0.96,
    
    lineend =
      "round"
  ) +
    
    
    # --------------------------------------------------------
  # Annual positions along highlighted trajectories
  # --------------------------------------------------------
  
  geom_point(
    
    data =
      data_highlight,
    
    aes(
      x =
        .data[[
          x_roll
        ]],
      y =
        .data[[
          y_roll
        ]],
      color =
        Country_plot
    ),
    
    size =
      0.95,
    
    alpha =
      0.55
  ) +
    
    
    # --------------------------------------------------------
  # Final rolling-mean position
  # --------------------------------------------------------
  
  geom_point(
    
    data =
      end_data,
    
    aes(
      x =
        x,
      y =
        y,
      color =
        Country_plot
    ),
    
    size =
      3.1,
    
    alpha =
      1,
    
    show.legend =
      FALSE
  ) +
    
    
    # --------------------------------------------------------
  # Final country labels
  # --------------------------------------------------------
  
  geom_label_repel(
    
    data =
      label_data,
    
    aes(
      x =
        x,
      y =
        y,
      label =
        label,
      color =
        Country_plot
    ),
    
    size =
      2.7,
    
    fontface =
      "bold",
    
    fill =
      alpha(
        "white",
        0.78
      ),
    
    label.size =
      NA,
    
    label.padding =
      unit(
        0.12,
        "lines"
      ),
    
    box.padding =
      0.55,
    
    point.padding =
      0.35,
    
    force =
      5,
    
    force_pull =
      0.25,
    
    min.segment.length =
      0,
    
    segment.color =
      "grey40",
    
    segment.size =
      0.22,
    
    show.legend =
      FALSE,
    
    max.overlaps =
      Inf,
    
    seed =
      123
  ) +
    
    
    # --------------------------------------------------------
  # Colour palette
  # --------------------------------------------------------
  
  scale_color_manual(
    
    values =
      country_palette,
    
    drop =
      FALSE
  ) +
    
    
    # --------------------------------------------------------
  # Labels
  # --------------------------------------------------------
  
  labs(
    
    title =
      panel_title,
    
    x =
      xlab,
    
    y =
      ylab
  ) +
    
    
    # --------------------------------------------------------
  # Theme
  # --------------------------------------------------------
  
  theme_nature() +
    
    
    # --------------------------------------------------------
  # Visual limits
  #
  # coord_cartesian() does NOT remove observations from the
  # underlying data; it only controls the displayed window.
  # --------------------------------------------------------
  
  coord_cartesian(
    
    xlim =
      x_limits,
    
    ylim =
      y_limits,
    
    clip =
      "on"
  )
  
  
  # ----------------------------------------------------------
  # Optional log10 x-axis
  # ----------------------------------------------------------
  
  if (log_x) {
    
    p <- p +
      
      scale_x_log10(
        
        labels =
          label_number(
            big.mark = ","
          )
      )
  }
  
  
  return(
    p
  )
}


# ============================================================
# 18. PANEL A
#     Economic development and N surplus intensity
# ============================================================

pA <- plot_landscape_panel(
  
  data =
    df_roll,
  
  x_raw =
    "GDP_pc_constant2015USD",
  
  y_raw =
    "N_surplus_per_protein_mass",
  
  x_roll =
    "GDP_pc_constant2015USD_roll3",
  
  y_roll =
    "N_surplus_per_protein_mass_roll3",
  
  xlab =
    "GDP per capita, constant 2015 US$",
  
  ylab =
    expression(
      "N surplus per unit protein supply (kg N kg protein"^-1*")"
    ),
  
  panel_title =
    "a  Economic development and N surplus intensity",
  
  log_x =
    TRUE,
  
  x_limits =
    NULL,
  
  y_limits =
    NULL
)


# ============================================================
# 19. PANEL B
#     Agricultural intensification and N surplus
# ============================================================
#
# The previous figure used a continuous x-axis extending to
# approximately 850 kg N ha-1 yr-1.
#
# We retain that approach rather than using a broken axis.
# ============================================================

pB <- plot_landscape_panel(
  
  data =
    df_roll,
  
  x_raw =
    "N_input_per_ha",
  
  y_raw =
    "N_surplus",
  
  x_roll =
    "N_input_per_ha_roll3",
  
  y_roll =
    "N_surplus_roll3",
  
  xlab =
    expression(
      "N input intensity (kg N ha"^-1*" yr"^-1*")"
    ),
  
  ylab =
    expression(
      "Cropland N surplus (kg N ha"^-1*" yr"^-1*")"
    ),
  
  panel_title =
    "b  Agricultural intensification and N surplus",
  
  log_x =
    FALSE,
  
  x_limits =
    c(
      0,
      850
    ),
  
  y_limits =
    c(
      -100,
      700
    )
)


# ============================================================
# 20. PANEL C
#     Protein supply and N surplus
# ============================================================

pC <- plot_landscape_panel(
  
  data =
    df_roll,
  
  x_raw =
    "Total_protein_supply",
  
  y_raw =
    "N_surplus",
  
  x_roll =
    "Total_protein_supply_roll3",
  
  y_roll =
    "N_surplus_roll3",
  
  xlab =
    expression(
      "Protein supply (g capita"^-1*" day"^-1*")"
    ),
  
  ylab =
    expression(
      "Cropland N surplus (kg N ha"^-1*" yr"^-1*")"
    ),
  
  panel_title =
    "c  Protein supply and N surplus",
  
  log_x =
    FALSE,
  
  x_limits =
    c(
      35,
      140
    ),
  
  y_limits =
    c(
      -100,
      700
    )
)


# ============================================================
# 21. COMBINE FIGURE
# ============================================================

Figure2 <- (
  
  pA /
    
    pB /
    
    pC
  
) +
  
  plot_layout(
    guides =
      "collect"
  ) &
  
  theme(
    legend.position =
      "bottom"
  )


print(
  Figure2
)


# ============================================================
# 22. SAVE MAIN FIGURE
# ============================================================

ggsave(
  
  filename =
    file.path(
      out_plot_dir,
      "Figure2_global_trajectories.png"
    ),
  
  plot =
    Figure2,
  
  width =
    8.5,
  
  height =
    11.5,
  
  dpi =
    600
)


ggsave(
  
  filename =
    file.path(
      out_plot_dir,
      "Figure2_global_trajectories.pdf"
    ),
  
  plot =
    Figure2,
  
  width =
    8.5,
  
  height =
    11.5
)


# ============================================================
# 23. SAVE PANELS SEPARATELY
# ============================================================

ggsave(
  
  file.path(
    out_plot_dir,
    "Figure2A_GDP_vs_N_surplus_intensity.png"
  ),
  
  pA,
  
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
    "Figure2B_N_input_vs_N_surplus.png"
  ),
  
  pB,
  
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
    "Figure2C_protein_vs_N_surplus.png"
  ),
  
  pC,
  
  width =
    8,
  
  height =
    5.5,
  
  dpi =
    600
)


# ============================================================
# 24. HIGHLIGHTED TRAJECTORY SUMMARY
# ============================================================
#
# IMPORTANT:
# These are descriptive summaries of the smoothed trajectories.
# They are NOT used for first-to-last transition classification.
# ============================================================

highlight_summary <- df_roll %>%
  
  filter(
    ISO3 %in%
      highlight_codes
  ) %>%
  
  mutate(
    Country_plot =
      factor(
        Country_plot,
        levels =
          country_order
      )
  ) %>%
  
  arrange(
    ISO3,
    Year
  ) %>%
  
  group_by(
    ISO3,
    Country_plot
  ) %>%
  
  summarise(
    
    first_year =
      min(
        Year
      ),
    
    last_year =
      max(
        Year
      ),
    
    n_years =
      n_distinct(
        Year
      ),
    
    
    # --------------------------------------------------------
    # GDP
    # --------------------------------------------------------
    
    GDP_roll3_initial =
      first(
        GDP_pc_constant2015USD_roll3
      ),
    
    GDP_roll3_final =
      last(
        GDP_pc_constant2015USD_roll3
      ),
    
    
    # --------------------------------------------------------
    # N surplus intensity
    # --------------------------------------------------------
    
    N_surplus_intensity_roll3_initial =
      first(
        N_surplus_per_protein_mass_roll3
      ),
    
    N_surplus_intensity_roll3_final =
      last(
        N_surplus_per_protein_mass_roll3
      ),
    
    
    # --------------------------------------------------------
    # N input intensity
    # --------------------------------------------------------
    
    N_input_roll3_initial =
      first(
        N_input_per_ha_roll3
      ),
    
    N_input_roll3_final =
      last(
        N_input_per_ha_roll3
      ),
    
    
    # --------------------------------------------------------
    # Cropland N surplus
    # --------------------------------------------------------
    
    N_surplus_roll3_initial =
      first(
        N_surplus_roll3
      ),
    
    N_surplus_roll3_final =
      last(
        N_surplus_roll3
      ),
    
    
    # --------------------------------------------------------
    # Protein supply
    # --------------------------------------------------------
    
    protein_roll3_initial =
      first(
        Total_protein_supply_roll3
      ),
    
    protein_roll3_final =
      last(
        Total_protein_supply_roll3
      ),
    
    .groups =
      "drop"
  )


write_csv(
  highlight_summary,
  file.path(
    out_data_dir,
    "05_highlighted_country_trajectory_summary.csv"
  )
)


# ============================================================
# 25. VARIABLE RANGES
# ============================================================

figure2_ranges <- tibble(
  
  variable = c(
    
    "GDP_pc_constant2015USD",
    
    "N_surplus_per_protein_mass",
    
    "N_input_per_ha",
    
    "N_surplus",
    
    "Total_protein_supply"
  ),
  
  minimum = c(
    
    min(
      df$GDP_pc_constant2015USD,
      na.rm = TRUE
    ),
    
    min(
      df$N_surplus_per_protein_mass,
      na.rm = TRUE
    ),
    
    min(
      df$N_input_per_ha,
      na.rm = TRUE
    ),
    
    min(
      df$N_surplus,
      na.rm = TRUE
    ),
    
    min(
      df$Total_protein_supply,
      na.rm = TRUE
    )
  ),
  
  maximum = c(
    
    max(
      df$GDP_pc_constant2015USD,
      na.rm = TRUE
    ),
    
    max(
      df$N_surplus_per_protein_mass,
      na.rm = TRUE
    ),
    
    max(
      df$N_input_per_ha,
      na.rm = TRUE
    ),
    
    max(
      df$N_surplus,
      na.rm = TRUE
    ),
    
    max(
      df$Total_protein_supply,
      na.rm = TRUE
    )
  )
)


write_csv(
  figure2_ranges,
  file.path(
    out_data_dir,
    "06_Figure2_variable_ranges.csv"
  )
)


# ============================================================
# 26. MANUSCRIPT ANCHORS
# ============================================================

manuscript_anchors <- tibble(
  
  result = c(
    
    "Countries represented",
    
    "Country-year observations",
    
    "First year",
    
    "Last year",
    
    "Illustrative countries highlighted",
    
    "Rolling window",
    
    "Negative N-surplus observations retained"
  ),
  
  value = c(
    
    n_distinct(
      df$ISO3
    ),
    
    nrow(
      df
    ),
    
    min(
      df$Year
    ),
    
    max(
      df$Year
    ),
    
    length(
      highlight_codes
    ),
    
    3,
    
    sum(
      df$N_surplus < 0,
      na.rm = TRUE
    )
  ),
  
  units = c(
    
    "countries",
    
    "country-years",
    
    "year",
    
    "year",
    
    "countries",
    
    "years",
    
    "country-years"
  )
)


write_csv(
  manuscript_anchors,
  file.path(
    out_data_dir,
    "manuscript_anchors.csv"
  )
)


# ============================================================
# 27. SAVE IMPORTANT OBJECTS
# ============================================================

figure2_objects <- list(
  
  raw_data =
    df,
  
  rolling_data =
    df_roll,
  
  highlighted_countries =
    highlight_codes,
  
  highlighted_summary =
    highlight_summary,
  
  country_palette =
    country_palette,
  
  pA =
    pA,
  
  pB =
    pB,
  
  pC =
    pC
)


saveRDS(
  figure2_objects,
  file.path(
    out_data_dir,
    "Figure2_objects.rds"
  )
)


# ============================================================
# 28. REPRODUCIBILITY
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
    out_data_dir,
    "package_versions.csv"
  )
)


capture.output(
  
  sessionInfo(),
  
  file =
    file.path(
      out_data_dir,
      "sessionInfo.txt"
    )
)


# ============================================================
# 29. CONSOLE SUMMARY
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "FIGURE 2 - GLOBAL NITROGEN TRANSITION TRAJECTORIES\n"
)

cat(
  "============================================================\n"
)


cat(
  "\n--- DATASET QC ---\n"
)

print(
  figure2_qc,
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
  "\n--- ROLLING-MEAN QC ---\n"
)

print(
  rolling_qc,
  n = Inf,
  width = Inf
)


cat(
  "\n--- HIGHLIGHTED COUNTRIES ---\n"
)

print(
  highlight_country_check,
  n = Inf,
  width = Inf
)


cat(
  "\n--- VARIABLE RANGES ---\n"
)

print(
  figure2_ranges,
  n = Inf,
  width = Inf
)


cat(
  "\n--- HIGHLIGHTED TRAJECTORY SUMMARY ---\n"
)

print(
  highlight_summary,
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
  "- Grey hexbins show raw annual country-year observations.\n"
)

cat(
  "- Grey and coloured trajectories use centred 3-year rolling means.\n"
)

cat(
  "- Rolling means are used only for visualization.\n"
)

cat(
  "- Smoothed trajectory endpoints are NOT the annual endpoints used for transition classification.\n"
)

cat(
  "- Panel A uses N_surplus_per_protein_mass exclusively.\n"
)

cat(
  "- The legacy N_surplus_per_protein variable is never plotted.\n"
)

cat(
  "- Negative cropland N-surplus observations are retained.\n"
)

cat(
  "- Highlighted countries are illustrative only and receive no analytical weighting.\n"
)

cat(
  "- Figure 2 describes movement through transition space; it does not define deterministic transition stages.\n"
)


cat(
  "\nResults saved to:\n",
  out_data_dir,
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
# END OF 02_Figure2_global_trajectories.R
# ============================================================