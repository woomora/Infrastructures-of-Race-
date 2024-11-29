# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigC3"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Donuts ----
# Define the file path for the output CSV file
est_donuts_path <- str_c(output_dir, "/est_donuts.csv")

# Run the first part of the script only if the file does not exist
if (!file.exists(est_donuts_path)) {
  
  # Donuts ----
  catastro_rd <-
    catastro |>
    tibble() |>
    filter(dist_n_pi <= 1000) |>
    mutate(
      t2b1 = case_when(
        dist_n_pi <= cutoff & solo_indigenas == 1 ~ "Indigenous - Treated",
        dist_n_pi > cutoff & solo_indigenas == 1 ~ "Indigenous - Control",
        dist_n_pi <= cutoff & solo_indigenas == 0 ~ "Mixed - Treated",
        dist_n_pi > cutoff & solo_indigenas == 0 ~ "Mixed - Control",
      ),
    )
  
  donuts <- seq(0, 90, 5)
  
  level <- 100 - 5/length(donuts)
  
  # Progress bar
  pb <- progress_bar$new(total = length(donuts))
  pb$tick(0)
  Sys.sleep(1)
  
  est_donuts <- tibble()
  
  for (d in donuts) {
    
    tryCatch({
      
      pb$tick()
      
      catastro_rd_donut <-
        catastro_rd |>
        filter(
          dist_n_pi <= cutoff - d | dist_n_pi > cutoff + d
        ) |>
        mutate(
          dist_n_pi = case_when(
            dist_n_pi <= cutoff ~ dist_n_pi + d,
            TRUE ~ dist_n_pi - d
          )
        ) |>
        tibble()
      
      rd1_donut <-
        rdrobust(
          (catastro_rd_donut$lvalor), -catastro_rd_donut$dist_n_pi,
          c = -cutoff,
          p = 1, h = h1, b = b1, # level = level,
          covs = cbind(
            factor(catastro_rd_donut$pueblo),
            catastro_rd_donut$predio_lon,
            catastro_rd_donut$predio_lat
          )
        )
      
      # Results
      est_donuts <-
        est_donuts |>
        bind_rows(
          tibble(
            cutoff = cutoff,
            donut = d,
            rdd_est = rd1_donut$Estimate[1,2],
            rdd_se = rd1_donut$Estimate[1,4],
            rdd_lci = rd1_donut$ci[3,1],
            rdd_uci = rd1_donut$ci[3,2],
            rdd_tval = rd1_donut$Estimate[1,2]/rd1_donut$Estimate[1,4],
          )
        )
      
      Sys.sleep(1 / length(donuts))
      
    }, error=function(e){})
    
  }
  
  est_donuts |>
    write_csv(est_donuts_path)
}

# Continue with the remaining part of the script, which loads and processes the file
est_donuts <- 
  read_csv(est_donuts_path) |> 
  mutate(
    main_spec = if_else(
      donut == 45, "Main specification", "Alternative specification"
    )
  )

est_donuts |> 
  ggplot(aes(donut, rdd_est)) +
  geom_hline(yintercept = 0) +
  geom_linerange(
    aes(ymin = rdd_lci, ymax = rdd_uci, color = main_spec)
  ) +
  geom_point(aes(color = main_spec, shape = main_spec), size = 3) +
  scale_y_continuous(breaks = seq(-.12, .06, by = .02)) +
  scale_x_continuous(breaks = seq(0, 90, by = 5)) +
  labs(
    x = "Donut size (m)",
    y = "RDD Estimate (log-points)",
    color = "", shape = ""
  ) +
  scale_color_manual(values = c(tau_c, tau))

ggsave(
  str_c(output_dir, "/rd_est_donuts.png"),
  width = 7.5, height = 4.85
)
