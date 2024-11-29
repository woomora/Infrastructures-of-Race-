# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigC5"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Leave-one-out Pueblo ----

# Define the file path for the output CSV file
loo_pi_path <- str_c(output_dir, "/robustness_loo.csv")

# Run the first part of the script only if the file does not exist
if (!file.exists(loo_pi_path)) {
  
  # LOO Pueblo ----
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
    )  |>
    filter(
      dist_n_pi <= cutoff - donut | dist_n_pi > cutoff + donut
    ) |>
    mutate(
      dist_n_pi = case_when(
        dist_n_pi <= cutoff ~ dist_n_pi + donut,
        TRUE ~ dist_n_pi - donut
      )
    ) |>
    tibble()
  
  
  # Sample: Pueblos
  pueblos <-
    catastro_rd |>
    tibble() |>
    select(pueblo) |>
    group_by(pueblo) |>
    summarise() |>
    ungroup()
  
  pueblos <- pueblos$pueblo
  
  # Progress bar
  pb <- progress_bar$new(total = length(pueblos))
  pb$tick(0)
  Sys.sleep(1)
  
  loo_pi <- tibble()
  
  for (i in pueblos) {
    
    pb$tick()
    
    # Drop a given Pueblo
    foo1_sample <-
      catastro_rd |>
      filter(pueblo != i)
    
    rdd_loo <-
      rdrobust(
        (foo1_sample$lvalor), -foo1_sample$dist_n_pi, c = -cutoff,
        h = h1, b = b1,
        covs = cbind(
          factor(foo1_sample$pueblo),
          foo1_sample$predio_lon,
          foo1_sample$predio_lat
        ),
      )
    
    loo_pi <-
      loo_pi |>
      bind_rows(
        tibble(
          rdd = rdd_loo$coef[3,],
          rdd_tval = rdd_loo$z[3,],
          pueblo_loo = i
        )
      )
    
    Sys.sleep(1 / length(pueblos))
    
  }
  
  loo_pi |>
    write_csv(loo_pi_path)
  
}

# Continue with the remaining part of the script, which loads and processes the file
loo_pi <- 
  read_csv(loo_pi_path)

# RDD LOO
est_mean <- round(mean(loo_pi$rdd), 3)
est_sd <- round(sd(loo_pi$rdd), 3)
est_tval <- round(est_mean/est_sd, 3)
est_pval <- round(2*pt(q=abs(est_mean/est_sd), df=999, lower.tail=F), 3)

loo_pi |> 
  ggplot(aes(x = rdd)) +
  geom_vline(
    xintercept = 0
  ) +
  geom_histogram(
    aes(y=..count../sum(..count..)),
    bins = 40, alpha = .75
  ) +
  geom_vline(xintercept = est_mean, color = tau, lwd = 1) +
  geom_vline(xintercept = 0)  +
  scale_x_continuous(breaks = seq(-.2, .2, .01)) +
  scale_y_continuous(breaks = seq(0, .5, .05)) +
  labs(
    y = "Density",
    x = "RDD estimate: Leave-one-out (Pueblo) estimates",
    fill = ""
  ) +
  theme_clean  +
  # RDD type
  annotate(
    "text", x = -.04, y = .16, color = tau,
    label = "Treatment vs. Control"
  ) +
  # Mean
  annotate(
    "text", x = -.04, y = .15, color = tau,
    label = paste0("LOO Mean: ", est_mean)
  ) +
  # SD
  annotate(
    "text", x = -.04, y = .14, color = tau,
    label = paste0("LOO se: ", est_sd)
  ) +
  # Pvalue
  annotate(
    "text", x = -.04, y = .13, color = tau,
    label = paste0("LOO p-value: ", round(est_pval, 3))
  )

ggsave(
  str_c(output_dir, "/robustness_loo.png"),  width = 7.5, height = 4.85
)
#