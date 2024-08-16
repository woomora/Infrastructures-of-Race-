## Treatment vs Control ----
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
  ) |> 
  mutate(
    lvalor = log(valor_unitario_suelo)
  ) |>
  # Filter observations outside donut
  filter(
    dist_n_pi <= cutoff - donut | dist_n_pi > cutoff + donut
  ) |> 
  # Recenter running variable
  mutate(
    dist_n_pi = case_when(
      dist_n_pi <= cutoff ~ dist_n_pi + donut,
      TRUE ~ dist_n_pi - donut
    )
  ) |>
  tibble()

#

# RDD estimate ---- 
rd_all <- 
  rdrobust(
    (catastro_rd$lvalor), -catastro_rd$dist_n_pi, c = -cutoff,
    covs = cbind(
      factor(catastro_rd$pueblo),
      catastro_rd$predio_lon,
      catastro_rd$predio_lat
    )
  )

summary(rd_all)

h1 <- rd_all$bws[1,1]
b1 <- rd_all$bws[2,1]

rd_all2 <- 
  rdrobust(
    (catastro_rd$lvalor), -catastro_rd$dist_n_pi, c = -cutoff,
    p = 2,
    covs = cbind(
      factor(catastro_rd$pueblo),
      catastro_rd$predio_lon,
      catastro_rd$predio_lat
    )
  )

summary(rd_all2)

h2 <- rd_all2$bws[1,1]
b2 <- rd_all2$bws[2,1]

# RDD plot ---- 

rdplot_data <- 
  catastro_rd |> 
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
      T ~ NA_character_
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

rd_est <- round(rd_all$coef[3], 4)
rd_se <- round(rd_all$se[3], 3)
rd_label <- expression(~tau == ~ -0.049 ~~ (0.014))

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
  scale_color_manual(values = c(tau_c, tau)) +
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
    color = tau, size = 4, fontface = 3
  ) +
  annotate(
    "text", x = cutoff + 80, y = Inf,
    label = "Outside pueblo's catchment area",
    color = tau_c, size = 4, fontface = 3
  ) +
  coord_cartesian(clip = "off")

ggsave(
  str_c(plots_path, "/rdplot.png"),
  width = 7.5, height = 4.85
)


#
