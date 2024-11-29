# ------------------------------------------------------------------------------
### Workspace ----
# Clean Workspace
rm(list = ls())

# Set or change Working Directory
path <- getwd()
setwd(path)

# Installation and Loading of Required Packages

# If not installed, install package pacman
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# (Install if not yet and) Load packages using pacman
pacman::p_load(
  # Data manipulation and wrangling
  tidyverse, janitor, haven,
  
  # Spatial data processing
  sf, raster, geosphere, nngeo, stplanr, stars, units, ggspatial, stplanr, 
  
  # Statistical modeling and econometrics
  fixest, statar, rdrobust, rdlocrand, rddensity, binsreg, fastDummies,
  
  # Data visualization and plotting
  ggnewscale, ggpubr, ggsignif, wesanderson, latex2exp, ggthemes, viridis, 
  cowplot, broom, scales, ggthemes,
  
  # Reporting and tables
  kableExtra,
  
  # Miscellaneous utilities
  beepr, progress
)

# Same for GitHub packages
pacman::p_load_gh(
  "dmbwebb/qval", # sharpened q-values
  "yutannihilation/ggsflabel" # labels for sf maps
)

# Notes:
# 1. 'pacman' package is used to simplify the loading and installation of packages.
# 2. 'qval' and 'ggsflabel' are installed from GitHub.
# 3. The packages are grouped by their primary functionality for better readability.
# 4. Duplicate package entries have been removed to avoid unnecessary loading.
# 5. If any package is not installed, pacman will install it before loading.

# For an easier visualization in Mac
X11(type = "cairo")

select <- dplyr::select # mask dplyr select function

# Set ggplot theme
theme_clean <- 
  ggthemes::theme_clean() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA), # Set plot background to white
    panel.background = element_rect(fill = "white", color = NA), # Set panel background to white
    legend.background = element_rect(fill = "white", color = NA), # Set legend background to white
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
  )
theme_set(theme_clean)

#
# ------------------------------------------------------------------------------
# Functions ----
source("scripts/IoR_functions.R")

# ------------------------------------------------------------------------------
# Color palettes ----
color_palette <- wes_palette("Darjeeling1", 6, type = "continuous")
color_palette
rev(color_palette)

color_ind <- "#5BBCD6"
color_ind_c <- "#D98F2A"
tau <- "#32806E"
tau_c <- "#91A737"
color_mix <- "#FF0000"
color_mix_c <-  "#F49C00" 

palette1 <- c(color_ind, color_mix)
palette2 <- c(color_ind, color_ind_c, color_mix_c, color_mix)

# ------------------------------------------------------------------------------
### Data ----
source("scripts/IoR_data_construction.R")

# ------------------------------------------------------------------------------
### Main Figures and results ----
# Fuzzy cutoff and donut ----
varas500 <- 420 # 500*.84
varas600 <- 500 # 600*.84


cutoff <- 460 # (varas500 + varas600)/2
donut <- 45

#
# RDD data ----

catastro_rd <- 
  catastro |> 
  filter(dist_n_pi <= 1000) |> 
  mutate(
    t2b1 = case_when(
      dist_n_pi <= cutoff & solo_indigenas == 1 ~ "Indigenous - Treated",
      dist_n_pi > cutoff & solo_indigenas == 1 ~ "Indigenous - Control",
      dist_n_pi <= cutoff & solo_indigenas == 0 ~ "Mixed - Treated",
      dist_n_pi > cutoff & solo_indigenas == 0 ~ "Mixed - Control",
    ),    
  )


# Figure 1: Pueblos + CDMX Historical extension ----
source("scripts/scripts_results/IoR_Fig1.R")

# NOTES: Script also includes Figure A.1

# Figure 2: Identification illustration – Pueblo locations and fuzzy boundaries ----
source("scripts/scripts_results/IoR_Fig2.R")

# NOTES: Script also includes Figure C.2
# Need to run source("scripts/scripts_results/IoR_Fig1.R") to get baseline map

# Figure 3: Land values – mean and gradient ----
source("scripts/scripts_results/IoR_Fig3.R")

# NOTES: This script produces plots in mean differences between treated and control 
# land plots, for both types of pueblos and looking at the heterogeneous effects

# Figure 3 corresponds to the non-parametric regressions comparing Pueblos vs contro.
# saved as "nonparam_reg.png"

# The remaining figures are not included in the paper, 
# but contain the estimates reported there

# Figure 4: Donut RDD plot and estimates ----
source("scripts/scripts_results/IoR_Fig4.R")

# NOTES: Main RDD plot and estimate
# See Table C1 and corresponding script source("scripts/scripts_results/IoR_TabC1.R")
# for point estimates and further details

# Figure 5: Heterogeneity – Donut RDD plots and estimates ----
source("scripts/scripts_results/IoR_Fig5.R")

# NOTES: RDD plot and estimate for typo of pueblo: Indigenous and Mixed
# See Table C1 and corresponding script source("scripts/scripts_results/IoR_TabC1.R")
# for point estimates and further details

# Figure 6: Historical mechanisms – Public street lights ----
source("scripts/scripts_results/IoR_Fig6.R")

# Table 1: Historical amenities: Descriptive evidence ----
source("scripts/scripts_results/IoR_Tab1.R")

# Figure 7: Housing overcrowding at block level for Indigenous pueblos – Donut RDD plots and estimates ----
source("scripts/scripts_results/IoR_Fig7.R")

# Table 2: Contemporary public goods: School supply ----
source("scripts/scripts_results/IoR_Tab2.R")

# ------------------------------------------------------------------------------
### Appendix Figures and results ----
## Appendix B: Identification ----
# Figure B.1: Durable investments in pueblos: Churches ----
source("scripts/scripts_results/IoR_FigB1.R")

# Figure B.2: Pueblo size – Share of potential intersections of pueblo catchment areas ----
source("scripts/scripts_results/IoR_FigB2.R")

# Figure B.3: Cattaneo et al. (2018) density test ----
source("scripts/scripts_results/IoR_FigB3.R")

# Table B.1: Balance in location fundamentals at 250×250 meter cell level ----
source("scripts/scripts_results/IoR_TabB1.R")

## Appendix C: Robustness and falsification tests ----
# Table C.1: Main results – Donut RDD estimates ----
source("scripts/scripts_results/IoR_TabC1.R")

# NOTES: The script also includes the Effect magnitude estimates

# Figure C.1: Robustness – Fake cutoffs ----
source("scripts/scripts_results/IoR_FigC1.R")

# Figure C.2: RDD illustration – Pueblo locations and fuzzy boundaries ----

# NOTES: Run script source("scripts/scripts_results/IoR_Fig2.R")

# Figure C.3: Robustness – Donut size ----
source("scripts/scripts_results/IoR_FigC3.R")

# Figure C.4: Robustness – Local randomization RDD varying window or bandwidth size ----
source("scripts/scripts_results/IoR_FigC4.R")

# Figure C.5: Robustness – Leave-one-out estimates ----
source("scripts/scripts_results/IoR_FigC5.R")

# Figure C.6: Robustness – Bayesian clustered bootstrap (Rubin, 1981) ----
source("scripts/scripts_results/IoR_FigC6.R")

# Figure C.7: Falsification or placebo check ----
source("scripts/scripts_results/IoR_FigC7.R")

# Table C.2: Donut RDD – Balance in plot characteristics ----
source("scripts/scripts_results/IoR_TabC2.R")

# Table C.3: Robustness – RDD estimates with controls for zoning ----
source("scripts/scripts_results/IoR_TabC3.R")

# Table C.4: RDD parametric estimates and Conley (1999) standard errors ----
source("scripts/scripts_results/IoR_TabC4.R")

# Table C.5: Heterogeneity: Distance to colonial CBD and type of pueblo ----
source("scripts/scripts_results/IoR_TabC5.R")

## Appendix D: Mechanisms ----
# Figure D.1: Historical mechanisms – Roads (with asphalt) ----
source("scripts/scripts_results/IoR_FigD1.R")

# Figure D.2: Historical mechanisms – Tramways ----
source("scripts/scripts_results/IoR_FigD2.R")

# Figure D.3: Historical mechanisms – Post offices ----
source("scripts/scripts_results/IoR_FigD3.R")

# Figure D.4: Historical mechanisms – Mexico City grid and cell classification ----

# See first part of script source("scripts/scripts_results/IoR_Tab1.R")
# specifically, plot named historical_mechanisms_grid500m_illustration.png

# Table D.1: Demographic and socioeconomic sorting at block level ----
source("scripts/scripts_results/IoR_TabD1.R")

# Table D.2: Contemporary amenities: Street-level amenities ----
source("scripts/scripts_results/IoR_TabD2.R")

# Table D.3: Contemporary amenities: Establishments ----
source("scripts/scripts_results/IoR_TabD3.R")

# Table D.4: Contemporary amenities: Crime ----
source("scripts/scripts_results/IoR_TabD4.R")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------