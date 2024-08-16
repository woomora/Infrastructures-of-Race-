# Fake cutoffs ----
# Define the file path for the output CSV file
fake_cutoffs_path <- str_c(results_path, "/fake_cutoffs.csv")

# Run the first part of the script only if the file does not exist
if (!file.exists(fake_cutoffs_path)) {
  
  # Fake cutoffs ----
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
  
  cutoffs <- seq(200, 900, 10)
  
  # Progress bar
  pb <- progress_bar$new(total = length(cutoffs))
  pb$tick(0)
  Sys.sleep(1)
  
  fake_cutoffs <- tibble()
  
  for (c in cutoffs) {
    
    tryCatch({
      
      pb$tick()
      
      catastro_rd_fcutoff <-
        catastro_rd |>
        filter(
          dist_n_pi <= c - donut | dist_n_pi > c + donut
        ) |>
        mutate(
          dist_n_pi = case_when(
            dist_n_pi <= c ~ dist_n_pi + donut,
            TRUE ~ dist_n_pi - donut
          )
        ) |>
        tibble()
      
      # RDD - ALL
      rdd <-
        rdrobust(
          (catastro_rd_fcutoff$lvalor), -catastro_rd_fcutoff$dist_n_pi, c = -c,
          h = h1, b = b1,
          covs = cbind(
            factor(catastro_rd_fcutoff$pueblo),
            catastro_rd_fcutoff$predio_lon,
            catastro_rd_fcutoff$predio_lat
          )
        )
      
      # Results
      fake_cutoffs <-
        fake_cutoffs |>
        bind_rows(
          tibble(
            cutoff = c,
            rdd_est = rdd$Estimate[1,2],
            rdd_lci = rdd$ci[3,1],
            rdd_uci = rdd$ci[3,2],
            rdd_pval = rdd$pv[3],
            rdd_tval = rdd$Estimate[1,2]/rdd$Estimate[1,4]
          )
        )
      
      Sys.sleep(1 / length(cutoffs))
      
    }, error=function(e){})
    
  }
  
  fake_cutoffs |>
    write_csv(fake_cutoffs_path)
  
}

# Continue with the remaining part of the script, which loads and processes the file
fake_cutoffs <- 
  read_csv(fake_cutoffs_path) |> 
  mutate(
    qval = q_val(rdd_pval),
    main_spec_qval = case_when(
      cutoff == 460 ~ "Main specification",
      cutoff != 460 & qval <= .05 ~ "Alternative specification & q-val ≤ 0.05",
      cutoff != 460 & qval > .05 ~ "Alternative specification & q-val > 0.05"
    )
  )


fake_cutoffs |> 
  ggplot(aes(cutoff, rdd_est)) +
  annotate(
    "rect", 
    xmin = 420, xmax = 500, ymin = -Inf, ymax = Inf, 
    fill = "grey", alpha = 0.3
  ) +
  geom_hline(yintercept = 0) +
  # Fuzzy cutoffs
  geom_vline(xintercept = cutoff - donut, linetype = "dotted") +
  geom_vline(xintercept = cutoff, alpha = .5, linetype = "dashed") +
  geom_vline(xintercept = cutoff + donut, linetype = "dotted") +
  geom_linerange(
    aes(ymin = rdd_lci, ymax = rdd_uci, color = main_spec_qval)
  ) +
  geom_point(aes(color = main_spec_qval, shape = main_spec_qval), size = 2) +
  scale_y_continuous(breaks = seq(-.16, .12, by = .02)) +
  scale_x_continuous(breaks = seq(0, 1000, by = 30)) +
  labs(
    x = "Cutoff: Dist. from the center of the nearest pueblo (m)",
    y = "RDD Estimate (log-points)",
    color = "", shape = ""
  ) +
  scale_color_manual(values = c("grey", tau_c, tau)) +
  theme(
    axis.text = element_text(size = 6),
    legend.text = element_text(size = 8)
  ) +
  # Annotate fuzzy cutoff
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  annotate(
    "text", label = "Fuzzy cutoff", alpha = .5,
    x = cutoff,
    y = Inf, vjust = -1, size = 2.5
  )  

ggsave(
  str_c(plots_path, "/rd_est_cutoffs.png"), width = 7.5, height = 4.85
)
