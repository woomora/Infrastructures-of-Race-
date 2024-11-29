# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/Fig5"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

## Indigenous vs Control ----

catastro_rd_ind <- 
  catastro_rd |> 
  tibble() |> 
  filter(str_detect(t2b1, "Indigenous") == T) 

#
# RDD estimate ---- 
rd_ind <- 
  rdrobust(
    (catastro_rd_ind$lvalor), -catastro_rd_ind$dist_n_pi, c = -cutoff,
    covs = cbind(
      factor(catastro_rd_ind$pueblo),
      catastro_rd_ind$predio_lon,
      catastro_rd_ind$predio_lat
    )
  )

summary(rd_ind)



rd2_ind <- 
  rdrobust(
    (catastro_rd_ind$lvalor), -catastro_rd_ind$dist_n_pi, c = -cutoff,
    p = 2,
    covs = cbind(
      factor(catastro_rd_ind$pueblo),
      catastro_rd_ind$predio_lon,
      catastro_rd_ind$predio_lat
    )
  )

summary(rd2_ind)


# RDD plot ---- 

rdplot_data <- 
  catastro_rd_ind |> 
  filter(dist_n_pi >= cutoff - 150) |> 
  filter(dist_n_pi <= cutoff + 150)

rdd_plot <- 
  rdplot(
    (rdplot_data$lvalor), rdplot_data$dist_n_pi, c = cutoff,
    nbins = 10, p = 2,
    covs =
      cbind(
        factor(rdplot_data$pueblo)
      )
  )

rdd_plot2 <- 
  rdplot(
    (rdplot_data$lvalor), rdplot_data$dist_n_pi, c = cutoff,
    nbins = 30, p = 2,
    covs =
      cbind(
        factor(rdplot_data$pueblo)
      )
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
    y = plot1$vars_poly[,"rdplot_y"],
    x = plot1$vars_poly[,"rdplot_x"],
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
rd_label <- expression(~tau == ~ -0.072 ~~ (0.019))

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
    y = expression("Land price (log, MXN per"~m^2~")"),
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
    "text", x = cutoff - 40, y = (y_end1 + y_end2)/2,
    parse = TRUE,
    label = as.character(rd_label),
    color = tau, size = 4, fontface = 3
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
  str_c(output_dir, "/rdplot_ind.png"),
  width = 7.5, height = 4.85
)


#
## Mixed vs Control ----

catastro_rd_mix <- 
  catastro_rd |> 
  tibble() |> 
  filter(str_detect(t2b1, "Mixed") == T) 

#
# RDD estimate ---- 
rd_mix <- 
  rdrobust(
    (catastro_rd_mix$lvalor), -catastro_rd_mix$dist_n_pi, c = -cutoff,
    covs = cbind(
      factor(catastro_rd_mix$pueblo),
      catastro_rd_mix$predio_lon,
      catastro_rd_mix$predio_lat
    )
  )

summary(rd_mix)

rd_mix2 <- 
  rdrobust(
    (catastro_rd_mix$lvalor), -catastro_rd_mix$dist_n_pi, c = -cutoff,
    p = 2,
    covs = cbind(
      factor(catastro_rd_mix$pueblo),
      catastro_rd_mix$predio_lon,
      catastro_rd_mix$predio_lat
    )
  )

summary(rd_mix2)

# RDD plot ---- 

rdplot_data <- 
  catastro_rd_mix |> 
  filter(dist_n_pi >= cutoff - 150) |> 
  filter(dist_n_pi <= cutoff + 150)

rdd_plot <- 
  rdplot(
    (rdplot_data$lvalor), rdplot_data$dist_n_pi, c = cutoff,
    nbins = 10, p = 2,
    covs = 
      cbind(
        factor(rdplot_data$pueblo)
      )
  )

rdd_plot2 <- 
  rdplot(
    (rdplot_data$lvalor), rdplot_data$dist_n_pi, c = cutoff,
    nbins = 30, p = 2,
    covs = 
      cbind(
        factor(rdplot_data$pueblo)
      )
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
    y = plot1$vars_poly[,"rdplot_y"],
    x = plot1$vars_poly[,"rdplot_x"],
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

rd_est <- round(rd_mix2$coef[3], 4)
rd_se <- round(rd_mix2$se[3], 4)
rd_label <- expression(~tau == ~ 0.008 ~~ (0.023))

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
  scale_color_manual(values = c(color_mix_c, color_mix)) +
  theme_clean +
  labs(
    x = "Dist. to fuzzy cutoff (m)",
    y = expression("Land price (log, MXN per"~m^2~")"),
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
    "text", x = cutoff + 35, y = 7.965,
    parse = TRUE,
    label = as.character(rd_label),
    color = tau, size = 4, fontface = 3
  ) +
  annotate(
    "text", x = cutoff - 80, y = Inf,
    label = "Within pueblo's catchment area",
    color = color_mix, size = 4, fontface = 3
  ) +
  annotate(
    "text", x = cutoff + 80, y = Inf,
    label = "Outside pueblo's catchment area",
    color = color_mix_c, size = 4, fontface = 3
  ) +
  coord_cartesian(clip = "off", ylim = c(7.9,8))

ggsave(
  str_c(output_dir, "/rdplot_mix.png"),
  width = 7.5, height = 4.85
)
