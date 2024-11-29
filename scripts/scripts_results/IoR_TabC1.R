# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/TabC1"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

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

## Table RDD Estimates ----
rd_table <- 
  map_df(
    list(rd_all, rd_all2, rd_ind, rd2_ind, rd_mix, rd_mix2),
    table_rd
  )

rd_table <- 
  rd_table |> 
  mutate(
    type = c(rep("All", 2), rep("Indigenous", 2) , rep("Mixed", 2))
  ) |> 
  group_by(poly) |> 
  mutate(
    pval_adj = q_val(pval)
  ) |> 
  ungroup() |> 
  select(type, poly, coef, se, lci, uci, pval, pval_adj, h, b, N)

rd_table |> 
  data.frame() |>
  kableExtra::kable(
    format = "latex", 
    escape = FALSE,
    align = "ccccccccccc",
    table.envir = "threeparttable",
    booktabs = TRUE,
    linesep = '',
    digits = 3
  )

# Compare estimates to Yamagishi and Sato (2023) ----
beta_buraku <- exp(-0.0989) - 1

t_all = (rd_all$Estimate[2] - beta_buraku)/rd_all$se[3]

2*pt(-abs(t_all),df=(rd_all$N_h[1] + rd_all$N_h[2] -1))

t_ind = (rd_ind$Estimate[2] - beta_buraku)/rd_ind$se[3]

2*pt(-abs(t_ind),df=(rd_ind$N_h[1] + rd_ind$N_h[2] -1))
