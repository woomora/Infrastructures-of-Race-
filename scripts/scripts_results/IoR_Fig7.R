# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/Fig7"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

## Overcrowding ----
blocks_rd <- 
  tibble(cdmx_blocks) |>
  # Filter only blocks with population > 0 and
  filter(pobtot != 0) |>
  filter(pobtot > 0) |> 
  # Due to privacy rules, blocks with one or two inhabited dwellings
  # don't have socio-economic information
  filter(tvivhab > 2) |> 
  dplyr::select(
    cvegeo, cve_mun, area, 
    dist_n_pi, t2b1, solo_indigenas, dist_cbd, pueblo, block_lon, block_lat,
    pobtot, pobfem, rel_h_m, pob0_14, pob15_64, pob65_mas, p12ym_solt, prom_hnv, pnacoe, presoe15, pcatolica,
    phog_ind, pob_afro, pcon_disc, pcon_limi, graproes, pea, pdesocup, psinder, phogjef_f, 
    pro_ocup_c, vivtot,
    tvivhab, vph_pisodt, vph_c_elec, vph_aguafv, vph_tinaco, vph_cister, vph_excsa,
    vph_drenaj, vph_refri, vph_lavad, vph_hmicro, vph_autom, vph_moto, vph_bici, 
    vph_radio, vph_tv, vph_pc, vph_telef, vph_cel, vph_inter, vph_stvp, vph_spmvpi, vph_cvj
  ) |> 
  mutate(
    pdesocup = pdesocup/pea,
    pob_density = pobtot/area,
  ) |> 
  mutate_at(
    vars(
      pobfem, pob0_14, pob15_64, pob65_mas, pnacoe, presoe15, phog_ind, pob_afro, 
      pcon_disc, pcon_limi, pea, psinder, phogjef_f, pcatolica
    ),
    ~ ./pobtot
  ) |>
  mutate_at(
    vars(vph_pisodt:vph_cvj),
    ~ ./tvivhab
  )  |> 
  mutate(
    pobweights = pobtot,
  ) |> 
  # # Donut
  filter(
    dist_n_pi < cutoff - donut | dist_n_pi > cutoff + donut
  ) |>
  mutate(
    dist_n_pi = case_when(
      dist_n_pi < cutoff ~ dist_n_pi + donut,
      TRUE ~ dist_n_pi - donut
    )
  ) |>
  # Edit treatment variables
  mutate(
    treated = if_else(dist_n_pi <= cutoff, 1, 0),
    ind = if_else(solo_indigenas == 1, 1, 0),
  ) 

# Indigenous treated vs. Indigenous control
blocks_rd_ind <- 
  blocks_rd |> 
  filter(str_detect(t2b1, "Indigenous") == T)

# Mixed treated vs. Mixed control
blocks_rd_mix <- 
  blocks_rd |> 
  filter(str_detect(t2b1, "Mixed") == T)

# RDD Estimate ----
rd1_ind <- 
  rdrobust(
    scale(blocks_rd_ind$pro_ocup_c), -blocks_rd_ind$dist_n_pi, c = -cutoff,
    covs =
      cbind(
        factor(blocks_rd_ind$pueblo),
        blocks_rd_ind$block_lon,
        blocks_rd_ind$block_lat
      )
  )

summary(rd1_ind)

rd2_ind <- 
  rdrobust(
    scale(blocks_rd_ind$pro_ocup_c), -blocks_rd_ind$dist_n_pi, c = -cutoff, p = 2,
    covs =
      cbind(
        factor(blocks_rd_ind$pueblo),
        blocks_rd_ind$block_lon,
        blocks_rd_ind$block_lat
      )
  )

summary(rd2_ind)

# RDD plot ---- 

rdplot_data <- 
  blocks_rd_ind |> 
  filter(dist_n_pi >= cutoff - 130) |> 
  filter(dist_n_pi <= cutoff + 130)

rdd_plot <- 
  rdplot(
    (rdplot_data$pro_ocup_c), rdplot_data$dist_n_pi, c = cutoff,
    nbins = 10, p = 2,
  )

rdd_plot2 <- 
  rdplot(
    (rdplot_data$pro_ocup_c), rdplot_data$dist_n_pi, c = cutoff,
    nbins = 30, p = 2,
  )

plot1 <- rdd_plot
c = cutoff

data_points1 <- 
  tibble(
    x = plot1$vars_bins[,"rdplot_mean_bin"],
    y = plot1$vars_bins[,"rdplot_mean_y"],
  ) |> 
  mutate(
    treat = if_else(x <= c, "Treated", "Control")
  )

data_poly1 <- 
  tibble(
    y = plot2$vars_poly[,"rdplot_y"],
    x = plot2$vars_poly[,"rdplot_x"],
  ) |> 
  mutate(
    treat = case_when(
      x < c ~ "Treated", 
      x > c ~ "Control",
      TRUE ~ NA_character_
    )
  )

y_end1 <- (data_poly1 |> filter(x == cutoff))$y[1]
y_end2 <- (data_poly1 |> filter(x == cutoff))$y[2]

plot2 <- rdd_plot2

data_points2 <- 
  tibble(
    x = plot2$vars_bins[,"rdplot_mean_bin"],
    y = plot2$vars_bins[,"rdplot_mean_y"],
  )  |> 
  mutate(
    treat = if_else(x <= c, "Treated", "Control")
  )

rd_est <- round(rd2_ind$coef[3], 4)
rd_se <- round(rd2_ind$se[3], 4)
rd_label <- expression(~tau == ~ 0.21 ~~ (0.13))

data_points1 |> 
  ggplot(aes(x, y)) + 
  geom_line(
    data = data_poly1,
  ) +
  geom_point(aes(color = treat), size = 1.5, na.rm = TRUE) +
  geom_point(
    data = data_points2,
    size = 1, na.rm = TRUE, shape = 1, color = "grey"
  ) +
  geom_vline(xintercept = c, size = 0.5) +
  annotate(
    "segment", 
    x = c, xend = c,
    y = y_end2,
    yend = y_end1, 
    colour = tau, linewidth = .9,
    arrow = arrow(type = "closed", length = unit(0.015, "npc"))
  ) +
  scale_color_manual(values = c(color_ind_c, color_ind)) +
  theme_clean +
  labs(
    x = "Dist. to fuzzy cutoff (m)",
    y = "People per room",
    title = ""
  ) +
  scale_x_continuous(
    breaks = seq(cutoff-150, cutoff+150, 30),
    labels = seq(-150, 150, 30)
  ) +
  theme(
    legend.position = "none"
  ) +
  annotate(
    "text", x = cutoff - 20, y = (y_end1 + y_end2)/2,
    parse = TRUE,
    label = as.character(rd_label),
    color = tau, size = 3, fontface = 3
  ) +
  annotate(
    "text", x = cutoff - 80, y = Inf,
    label = "Within pueblo's catchment area",
    color = color_ind, size = 4, fontface = 3
  ) +
  annotate(
    "text", x = cutoff + 80, y = Inf,
    label = "Outside pueblo's catchment area",
    color = color_ind_c, size = 4, fontface = 3
  ) +
  coord_cartesian(clip = "off")

ggsave(
  str_c(output_dir, "/rdplot_overcrowding_ind.png"), 
  width = 7.5, height = 4.85
)

#