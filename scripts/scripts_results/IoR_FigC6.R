# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigC6"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Bootstrap (clustered) ----

# Define the file path for the output CSV file
bootstrap_results_path <- str_c(output_dir, "/bootstrapped_results.csv")

nboots <- 1000

# Run the first part of the script only if the file does not exist
if (!file.exists(bootstrap_results_path)) {
  
  # Bootstrap (clustered) ----
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
  
  # Generating a n1 by n_clusters matrix
  # where each row is an n_clusters dimensional Dirichlet draw.
  rd_clusters <-
    catastro_rd |>
    tibble() |>
    group_by(pueblo) |>
    summarise() |>
    ungroup()
  
  set.seed(1234)
  weights_pueblo <- matrix(rexp(nrow(rd_clusters) * nboots, 1), ncol = nrow(rd_clusters), byrow = TRUE)
  weights_pueblo <- weights_pueblo / rowSums(weights_pueblo)
  
  # Progress bar
  pb <- progress_bar$new(total = nboots)
  pb$tick(0)
  Sys.sleep(1)
  
  bootstrap_results <- tibble()
  
  for (i in 1:nboots) {
    
    tryCatch({
      
      pb$tick()
      
      # RDD - Indigenous
      foo1 <-
        catastro_rd |>
        left_join(
          rd_clusters |>
            mutate(weights = weights_pueblo[i,]),
          by = "pueblo"
        )
      
      rdd <-
        rdrobust(
          (foo1$lvalor), -foo1$dist_n_pi, c = -cutoff, h = h1, b = b1,
          covs = cbind(
            factor(foo1$pueblo),
            foo1$predio_lon,
            foo1$predio_lat
          ),
          weights = foo1$weights,
        )
      
      # Results
      bootstrap_results <-
        bootstrap_results |>
        bind_rows(
          tibble(
            rdd = rdd$Estimate[1,2],
          )
        )
      
    }, error=function(e){})
    
    Sys.sleep(1 / nboots)
    
  }
  
  bootstrap_results |>
    write_csv(bootstrap_results_path)
  
}

# Continue with the remaining part of the script, which loads and processes the file
bootstrap_results <- read_csv(bootstrap_results_path)

# Summarize and plot the results
bootstrap_results |> 
  summarise_all(mean)

bootstrap_results |> 
  summarise_all(sd)

# Calculate mean, standard deviation, and p-value for the RDD estimates
est_mean <- round(mean(bootstrap_results$rdd), 3)
est_sd <- round(sd(bootstrap_results$rdd), 3)
est_pval <- round(2*pt(q=abs(est_mean/est_sd), df=999, lower.tail=F), 3)

# Plot the bootstrapped RDD estimates
bootstrap_results |>
  ggplot(aes(rdd)) +
  geom_histogram(
    aes(y=..count../sum(..count..)),
    bins = 50,
    alpha = .75
  ) +
  geom_vline(xintercept = 0)  +
  geom_vline(xintercept = mean(bootstrap_results$rdd), color = tau, lwd = 1.5)  +
  scale_x_continuous(breaks = seq(-.25, .25, .05)) +
  scale_y_continuous(breaks = seq(0, .15, .01)) +
  labs(
    y = "Density",
    x = "Bootstrapped RDD estimates",
    fill = ""
  ) +
  theme_clean +
  # RDD type
  annotate(
    "text", x = -.1, y = .05, color = tau,
    label = "Treatment vs. Control"
  ) +
  # Mean
  annotate(
    "text", x = -.1, y = .0475, color = tau,
    label = paste0("Bootstrapped mean: ", est_mean)
  ) +
  # SD
  annotate(
    "text", x = -.1, y = .045, color = tau,
    label = paste0("Bootstrapped se: ", est_sd)
  ) +
  # Pvalue
  annotate(
    "text", x = -.1, y = .0425, color = tau,
    label = paste0("Bootstrapped p-value: ", round(est_pval, 3))
  )

# Save the plot
ggsave(
  str_c(output_dir, "/robustness_bootstrap.png"),  width = 7.5, height = 4.85
)
