# LRRD ----

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

lrrd_table <- tibble()

for (i in seq(5, 100, 5)) {
  
  print(i)
  
  lrrd <- 
    rdlocrand::rdrandinf(
      catastro_rd$lvalor, -catastro_rd$dist_n_pi,
      cutoff = -cutoff,
      wl = -cutoff - i, wr = -cutoff + i, interfci = .05
    )
  
  lrrd_table <- 
    lrrd_table |> 
    bind_rows(
      table_rdlr(lrrd) |> 
        mutate(
          window = i
        )
    )
  
}

lrrd_table |> 
  mutate_at(
    vars(coef, lci, uci), as.double
  ) |> 
  bind_rows(
    tibble(
      coef = rd_all$Estimate[2],
      lci = rd_all$ci[3,1],
      uci = rd_all$ci[3,2],
      window = h1,
      type = "Main RDD specification"
    )
  ) |> 
  mutate(
    type = replace_na(type, "Alt. Local Randomization RDD")
  ) |> 
  ggplot(aes(window, coef, color = type, shape = type)) +
  geom_hline(yintercept = 0) +
  geom_linerange(
    aes(ymin = lci, ymax = uci)
  ) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(-.14, .08, by = .02)) +
  scale_x_continuous(breaks = seq(0, 120, by = 5)) +
  labs(
    x = "Window or bandwith size (m)",
    y = "RDD Estimate (log-points)",
    color = "", shape = ""
  ) +
  scale_color_manual(values = c(tau_c, tau))

ggsave(
  str_c(plots_path, "/rd_est_lrrd_windows.png"),
  width = 7.5, height = 4.85
)

#