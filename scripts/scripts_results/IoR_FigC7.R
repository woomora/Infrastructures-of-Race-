# Falsification or placebo check: Local Randomization ----
# Define the file path for the output CSV file
locrand_path <- str_c(results_path, "/locrand.csv")

# Run the first part of the script only if the file does not exist
if (!file.exists(locrand_path)) {
  
  # Falsification or placebo check: Local Randomization ----
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
  
  nboots <- 1000
  
  # Progress bar
  pb <- progress_bar$new(total = nboots)
  pb$tick(0)
  Sys.sleep(1)
  
  locrand <- tibble()
  
  for (i in 1:nboots) {
    
    tryCatch({
      
      pb$tick()
      
      set.seed(i)
      foo1 <-
        catastro_rd |>
        mutate(
          lrz = sample(dist_n_pi)
        )
      
      rdd <-
        rdrobust(
          (foo1$lvalor), -foo1$lrz, c = -cutoff, h = h1, b = b1,
          covs = cbind(
            factor(foo1$pueblo),
            foo1$predio_lon,
            foo1$predio_lat
          )
        )
      
      # Results
      locrand <-
        locrand |>
        bind_rows(
          tibble(
            rdd = rdd$Estimate[1,2],
          )
        )
      
      Sys.sleep(1 / nboots)
      
    }, error=function(e){})
    
  }
  
  locrand |>
    write_csv(locrand_path)
  
}

# Continue with the remaining part of the script, which loads and processes the file
locrand <- read_csv(locrand_path)

nboots <- 1000

# Calculate the p-value
est_pval <- 
  (locrand |>
     select(rdd) |> 
     filter(rdd <= rd_all$Estimate[1,2]) |> 
     count() |> 
     mutate(
       n = n/nboots
     ))$n

# Plot the randomization inference estimates
locrand |>
  ggplot(aes(rdd)) +
  geom_histogram(
    aes(y=..count../sum(..count..)),
    bins = 50,
    alpha = .75
  ) +
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = rd_all$Estimate[1,2], color = tau, lwd = 1.5) +
  labs(
    y = "Density",
    x = "Randomization inference estimates",
    fill = ""
  ) +
  theme_clean +
  # RDD type
  annotate(
    "text", x = -0.035, y = .0625, size = 4, color = tau,
    label = "Treatment vs. Control", 
  ) +
  # Estimate
  annotate(
    "text", x = -0.035, y = .06, size = 4, color = tau,
    label = paste0("RDD Estimate: ", round(rd_all$Estimate[1,2], 3))
  ) +
  # Pvalue
  annotate(
    "text", x = -0.035, y = .0575, size = 4, color = tau,
    label = paste0("RI p-value: ", round(est_pval, 3))
  ) 

# Save the plot
ggsave(
  str_c(plots_path, "/robustness_randinf.png"),  width = 7.5, height = 4.85
)
#