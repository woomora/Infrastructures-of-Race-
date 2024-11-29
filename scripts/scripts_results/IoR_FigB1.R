# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigB1"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Durable investments: Churches ----
data_plot <- 
  iglesias_cdmx_block |> 
  filter(dist_n_pi <= 1000) |> 
  tibble() |> 
  mutate(
    t2b1 = case_when(
      dist_n_pi <= cutoff & solo_indigenas == 1 ~ "Indigenous - Treated",
      dist_n_pi > cutoff & solo_indigenas == 1 ~ "Indigenous - Control",
      dist_n_pi <= cutoff & solo_indigenas == 0 ~ "Mixed - Treated",
      dist_n_pi > cutoff & solo_indigenas == 0 ~ "Mixed - Control",
    ),
    pueblo_type = case_when(
      solo_indigenas == 1 ~ "Indigenous",
      TRUE ~ "Mixed"
    ),
    dist_n_pi_n = ntile(dist_n_pi, 10),
    dist_n_pi_q = ntile(dist_n_pi, 5),
  )

m1 <-
  feols(
    catolica ~ i(dist_n_pi_n) | n_pi_index,
    data_plot,
    cluster = ~ n_pi_index
  )

mean(fixest::fixef(m1)$n_pi_index, na.rm = T)

data_plot |> 
  filter(dist_n_pi_n == 1) |> 
  summarise(
    dist_n_pi = mean(dist_n_pi)
  )

m1 <-
  feols(
    catolica ~ i(dist_n_pi_q) | n_pi_index,
    data_plot,
    cluster = ~ n_pi_index
  )

mean(fixest::fixef(m1)$n_pi_index)

data_plot |> 
  filter(dist_n_pi_q == 1) |> 
  summarise(
    dist_n_pi = mean(dist_n_pi)
  )

m1 <- 
  feols(
    catolica ~ log(dist_n_pi) | n_pi_index,
    data_plot,
    cluster = ~ n_pi_index
  )

mean(fixest::fixef(m1)$n_pi_index)

feols(
  catolica ~ t2b1,
  data_plot,
  cluster = ~ n_pi_index
)

est <- binsreg(
  (data_plot$catolica), (data_plot$dist_n_pi), 
  # w = ~ block_lon + block_lat + log(dist_cbd),
  by=data_plot$pueblo_type,
  data=data_plot, 
  nbins = 15, polyreg = 2
)

data_points <- 
  tibble(
    est$data.plot$`Group Indigenous`$data.dots
  ) |> 
  bind_rows(
    tibble(
      est$data.plot$`Group Mixed`$data.dots
    )
  )

data_poly <- 
  tibble(
    est$data.plot$`Group Indigenous`$data.poly
  ) |> 
  bind_rows(
    tibble(
      est$data.plot$`Group Mixed`$data.poly
    )
  )

data_points |> 
  ggplot(aes(x, fit, color = group, shape = group)) +
  geom_point(size = 2) +
  geom_line(
    data = data_poly
  ) +
  scale_y_continuous(breaks = seq(0, .5, by = .01)) +
  scale_x_continuous(breaks = seq(0, 1500, by = 250)) +
  scale_color_manual(values = c(color_ind, color_mix)) +
  labs(x = "Dist. from the center of the nearest pueblo (m)", y = "P(Catholic church = 1)", color = "", shape = "") +
  theme(
    legend.position = "bottom",
  )

ggsave(
  str_c(
    output_dir, "/identification_durable_inv_churches.png"
  ),
  width = 7.5, height = 4.85
)

#