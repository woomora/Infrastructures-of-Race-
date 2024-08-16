### NOTES ----

# NOTES: This script produces plots in mean differences between treated and control 
# land plots, for both types of pueblos and looking at the heterogeneous effects

# Figure 3 corresponds to the non-parametric regressions comparing Pueblos vs contro.
# saved as "nonparam_reg.png"

# The remaining figures are not included in the paper, 
# but contain the estiamtes reported there

## Mean difference ----
# Pueblos vs control ----
data_plot <- 
  catastro_rd |> 
  mutate(
    treated = case_when(
      str_detect(t2b1, "Treated") ~ "Treated",
      TRUE ~ "Control"
    ),
    treated = fct_relevel(
      treated,
      "Treated",
      "Control",
    ),
    valor = exp(lvalor)
  ) 


feols(
  (valor) ~ treated, data_plot
)

feols(
  log(valor) ~ treated, data_plot
)

stat.test <- 
  compare_means(
    lvalor ~ treated, data = data_plot,
    method = "t.test"
  )

p <- 
  ggbarplot(
    data_plot, x = "treated", y = "valor",
    color = "treated", fill = "treated",
    add.params	= list(alpha = .5),
    add = "mean_se", lab.vjust = -1.6, 
    label = T, lab.nb.digits = 2,
    width = .33, lab.size = 2.5,
    position = position_dodge(0.4)
  ) +
  stat_pvalue_manual(
    stat.test, y.position = 3000, 
    tip.length = 0.001, size = 2.5,
    label = "p-value = {p.format}"
  )

p +
  labs(
    x = "",
    y = expression("Land price (MXN per"~m^2~")")
  ) +
  theme_clean +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12)
  ) +
  scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  scale_fill_manual(values = c(tau, tau_c)) +
  scale_color_manual(values = c(tau, tau_c)) +
  scale_alpha_manual(values = c(0.02, 0.02)) 

ggsave(
  str_c(plots_path, "/diff_means_all.png"),
  width = 7.5, height = 4.85
)


# Heterogeneity ----

data_plot <- 
  catastro_rd |> 
  mutate(
    valor = exp(lvalor),
    t2b1 = fct_relevel(
      t2b1, 
      "Indigenous - Treated",
      "Indigenous - Control",
      "Mixed - Control",
      "Mixed - Treated",
    )
  ) 

feols(
  (valor) ~ t2b1, data_plot |> filter(str_detect(t2b1, "Indigenous"))
)

feols(
  log(valor) ~ t2b1, data_plot |> filter(str_detect(t2b1, "Indigenous"))
)

feols(
  (valor) ~ t2b1, data_plot |> filter(str_detect(t2b1, "Mixed"))
)

feols(
  log(valor) ~ t2b1, data_plot |> filter(str_detect(t2b1, "Mixed"))
)


stat.test <- 
  compare_means(
    lvalor ~ t2b1, data = data_plot,
    method = "t.test"
  ) |> 
  mutate(
    interest = case_when(
      group1 == "Indigenous - Treated" & group2 == "Indigenous - Control" ~ 1,
      group1 == "Mixed - Control" & group2 == "Mixed - Treated" ~ 1,
      group1 == "Indigenous - Treated" & group2 == "Mixed - Treated" ~ 1,
      TRUE ~ 0
    )
  ) |> 
  filter(
    interest == 1
  )

p <- 
  ggbarplot(
    data_plot, x = "t2b1", y = "valor",
    color = "t2b1", fill = "t2b1",
    add = "mean_se", lab.vjust = -1.6, 
    label = T, lab.nb.digits = 2,
    width = .33, lab.size = 2.5,
    position = position_dodge(0.4)
  ) +
  stat_pvalue_manual(
    stat.test,
    y.position = c(2450, 4000, 3700),
    tip.length = 0.001, size = 2.5,
    label = "p-value = {p.adj}"
  )

p +
  labs(
    x = "",
    y = expression("Land price (MXN per"~m^2~")")
  ) +
  theme_clean +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12)
  ) +
  scale_y_continuous(breaks = seq(0, 4000, by = 500)) +
  scale_fill_manual(values = palette2) +
  scale_color_manual(values = palette2) 

ggsave(
  str_c(plots_path, "/diff_means_het.png"),
  width = 7.5, height = 4.85
)

#

## Non-parametric regressions ----
# Pueblos vs control ----
data_plot <- 
  catastro_rd |> 
  tibble() |> 
  mutate(
    treated = case_when(
      str_detect(t2b1, "Treated") ~ "Treated",
      TRUE ~ "Control"
    ),
    treated = fct_relevel(
      treated,
      "Treated",
      "Control",
    ),
    valor = exp(lvalor),
    segments = case_when(
      dist_n_pi <= 200 ~ "≤ 200 meters",
      dist_n_pi > 200 & dist_n_pi < 700 ~ "> 200 & < 700 meters",
      dist_n_pi >= 700 ~ "≥ 700 meters",
    ),
    segments = fct_relevel(
      segments, "≤ 200 meters", "> 200 & < 700 meters", "≥ 700 meters"
    )
  ) 

m1 <- 
  feols(
    valor ~ segments, 
    data_plot |> filter(segments != "> 200 & < 700 meters")
  )


est <- binsreg(
  data_plot$valor, data_plot$dist_n_pi, 
  # w = ~ predio_lon + predio_lat,
  data= data_plot, 
  nbins = 50
)


data_points <-  
  tibble(
    est$data.plot$`Group Full Sample`$data.dots
  ) |> 
  mutate(
    treated = case_when(
      x <= cutoff - donut ~ "Treated",
      x > cutoff + donut ~ "Control",
      T ~ "Fuzzy cuttof area"
    ),
    treated = fct_relevel(
      treated, 
      "Treated",
      "Control",
      "Fuzzy cuttof area"
    ),
    dist_n_pi = x,
    valor = fit,
  ) 


data_points |> 
  filter(!is.na(treated)) |> 
  ggplot(aes(dist_n_pi, valor, color = treated)) +
  annotate(
    "rect", 
    xmin = 420, xmax = 500, ymin = -Inf, ymax = Inf, 
    fill = "grey", alpha = 0.3
  ) +
  geom_segment(
    aes(x = 0, xend = 200, y = m1$coefficients[1], yend = m1$coefficients[1]),
    lwd = .75, linetype = "dashed", color = "black"
  ) +
  geom_segment(
    aes(x = 700, xend = 1000, y = m1$coefficients[1] +  m1$coefficients[2], yend = m1$coefficients[1] +  m1$coefficients[2]),
    lwd = .75, linetype = "dashed", color = "black"
  ) +
  geom_point(size = 1.5) +
  geom_smooth(
    data = data_plot |> filter(treated == "Treated"), 
    se = T, alpha = .05, lwd = .75
  ) +
  geom_smooth(
    data = data_plot |> filter(treated == "Control"), 
    se = T, alpha = .05, lwd = .75
  ) +
  # Fuzzy cutoffs
  geom_vline(xintercept = cutoff - donut, linetype = "dotted") +
  geom_vline(xintercept = cutoff, lwd = .5) +
  geom_vline(xintercept = cutoff + donut, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 3500, by = 100)) +
  scale_x_continuous(breaks = seq(0, 1500, by = 100)) +
  scale_color_manual(values = c(tau, tau_c, "grey")) +
  labs(x = "Dist. from the center of the nearest pueblo (m)", y = expression("Land price (MXN per"~m^2~")"), color = "") +
  theme(
    legend.position = "bottom",
    legend.key = element_rect(colour = NA, fill = NA),
    axis.text = element_text(size = 10)
  ) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  # Annotate fuzzy cutoff
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  annotate(
    "text", label = "Fuzzy cutoff", alpha = .5,
    x = cutoff,
    y = Inf, vjust = -1, size = 2.5
  ) 

ggsave(
  str_c(plots_path, "/nonparam_reg.png"),
  width = 7.5, height = 4.85
)

#
# Simple contrast table ----
m1 <- 
  feols(
    log(valor) ~ segments, 
    data_plot,
    cluster = ~ pueblo
  )

m2 <- 
  feols(
    log(valor) ~ segments + predio_lat + predio_lon, 
    data_plot,
    cluster = ~ pueblo
  )

# Heterogeneity ----
data_plot <- 
  catastro_rd |> 
  tibble() |> 
  mutate(
    group = case_when(
      solo_indigenas == 1 ~ "Indigenous",
      TRUE ~ "Mixed"
    ),
    valor = exp(lvalor),
    segments = case_when(
      dist_n_pi <= 200 ~ "≤ 200 meters",
      dist_n_pi > 200 & dist_n_pi < 700 ~ "> 200 & < 700 meters",
      dist_n_pi >= 700 ~ "≥ 700 meters",
    ),
    segments = fct_relevel(
      segments, "≤ 200 meters", "> 200 & < 700 meters", "≥ 700 meters"
    )
  )

est <- binsreg(
  data_plot$valor, data_plot$dist_n_pi, 
  # w = ~ predio_lon + predio_lat,
  by=data_plot$group,
  data=data_plot, 
  nbins = 50
)


data_points <-  
  tibble(
    est$data.plot$`Group Indigenous`$data.dots
  ) |> 
  bind_rows(
    tibble(
      est$data.plot$`Group Mixed`$data.dots
    )
  ) |> 
  mutate(
    t2b1 = case_when(
      x <= cutoff - donut & group == "Indigenous"  ~ "Indigenous - Treated",
      x > cutoff + donut  & group == "Indigenous" ~ "Indigenous - Control",
      x <= cutoff - donut & group == "Mixed"  ~ "Mixed - Treated",
      x > cutoff + donut  & group == "Mixed" ~ "Mixed - Control",
      T ~ "Fuzzy cuttof area"
    ),
    t2b1 = fct_relevel(
      t2b1, 
      "Indigenous - Treated",
      "Indigenous - Control",
      "Mixed - Control",
      "Mixed - Treated",
      "Fuzzy cuttof area"
    ),
    dist_n_pi = x,
    valor = fit,
  ) 


data_points |> 
  filter(!is.na(t2b1)) |> 
  ggplot(aes(dist_n_pi, valor, color = t2b1)) +
  annotate(
    "rect", 
    xmin = 420, xmax = 500, ymin = -Inf, ymax = Inf, 
    fill = "grey", alpha = 0.3
  ) +
  geom_point(size = 1.5)  +
  geom_smooth(
    data = data_plot |> filter(t2b1 == "Indigenous - Treated"), 
    se = T, alpha = .05
  ) +
  geom_smooth(
    data = data_plot |> filter(t2b1 == "Indigenous - Control"), 
    se = T, alpha = .05
  ) +
  geom_smooth(
    data = data_plot |> filter(t2b1 == "Mixed - Treated"), 
    se = T, alpha = .05
  ) +
  geom_smooth(
    data = data_plot |> filter(t2b1 == "Mixed - Control"), 
    se = T, alpha = .05
  ) +
  # Fuzzy cutoffs
  geom_vline(xintercept = cutoff - donut, linetype = "dotted") +
  geom_vline(xintercept = cutoff, lwd = .25) +
  geom_vline(xintercept = cutoff + donut, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 3500, by = 100)) +
  scale_x_continuous(breaks = seq(0, 1500, by = 100)) +
  scale_color_manual(values = c(palette2, "grey")) +
  labs(x = "Dist. from the center of the nearest pueblo (m)", y = expression("Land price (MXN per"~m^2~")"), color = "") +
  theme(
    legend.position = "bottom",
    legend.key = element_rect(colour = NA, fill = NA),
    axis.text.x = element_text(6)
  ) +
  guides(color=guide_legend(override.aes=list(fill=NA)))  +
  # Annotate fuzzy cutoff
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  annotate(
    "text", label = "Fuzzy cutoff", alpha = .5,
    x = cutoff,
    y = Inf, vjust = -1, size = 2.5
  ) +
  facet_wrap(~group, scales = "free_y") 

ggsave(
  str_c(plots_path, "/nonparam_reg_het.png")
)

#
