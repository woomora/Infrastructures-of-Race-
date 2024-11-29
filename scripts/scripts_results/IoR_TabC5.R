# Heterogeneity ----
## Distance to CBD + Indigenous/Mixed pueblo ----
# Close to CBD ----

catastro_rd_cbd_close <- 
  catastro_rd |> 
  tibble() |> 
  filter(dist_zocalo <= median(dist_zocalo)) 

catastro_rd_cbd_close |> 
  group_by(pueblo, solo_indigenas) |> 
  summarise() |> 
  ungroup() |> 
  group_by(solo_indigenas) |> 
  count() |> 
  ungroup()

#
# Indigenous vs Control
catastro_rd_cbd_close_ind <- 
  catastro_rd_cbd_close |> 
  tibble() |> 
  filter(str_detect(t2b1, "Indigenous") == T) 

rd_cbd_close_ind <- 
  rdrobust(
    (catastro_rd_cbd_close_ind$lvalor), -catastro_rd_cbd_close_ind$dist_n_pi, c = -cutoff,
    covs = cbind(
      factor(catastro_rd_cbd_close_ind$pueblo),
      catastro_rd_cbd_close_ind$predio_lon,
      catastro_rd_cbd_close_ind$predio_lat
    )
  )

summary(rd_cbd_close_ind)

# Mixed vs Control
catastro_rd_cbd_close_mix <- 
  catastro_rd_cbd_close |> 
  tibble() |> 
  filter(str_detect(t2b1, "Mixed") == T) 

rd_cbd_close_mix <- 
  rdrobust(
    (catastro_rd_cbd_close_mix$lvalor), -catastro_rd_cbd_close_mix$dist_n_pi, c = -cutoff,
    covs = cbind(
      factor(catastro_rd_cbd_close_mix$pueblo),
      catastro_rd_cbd_close_mix$predio_lon,
      catastro_rd_cbd_close_mix$predio_lat
    )
  )

summary(rd_cbd_close_mix)

# Far from CBD ----

catastro_rd_cbd_far <- 
  catastro_rd |> 
  tibble() |> 
  filter(dist_zocalo > median(dist_zocalo)) 

catastro_rd_cbd_far |> 
  group_by(pueblo, solo_indigenas) |> 
  summarise() |> 
  ungroup() |> 
  group_by(solo_indigenas) |> 
  count() |> 
  ungroup()

# RDD estimate 

# Indigenous vs Control
catastro_rd_cbd_far_ind <- 
  catastro_rd_cbd_far |> 
  tibble() |> 
  filter(str_detect(t2b1, "Indigenous") == T) 

rd_cbd_far_ind <- 
  rdrobust(
    (catastro_rd_cbd_far_ind$lvalor), -catastro_rd_cbd_far_ind$dist_n_pi, c = -cutoff,
    covs = cbind(
      factor(catastro_rd_cbd_far_ind$pueblo),
      catastro_rd_cbd_far_ind$predio_lon,
      catastro_rd_cbd_far_ind$predio_lat
    )
  )

summary(rd_cbd_far_ind)

# Mixed vs Control
catastro_rd_cbd_far_mix <- 
  catastro_rd_cbd_far |> 
  tibble() |> 
  filter(str_detect(t2b1, "Mixed") == T) 

rd_cbd_far_mix <- 
  rdrobust(
    (catastro_rd_cbd_far_mix$lvalor), -catastro_rd_cbd_far_mix$dist_n_pi, c = -cutoff,
    covs = cbind(
      factor(catastro_rd_cbd_far_mix$pueblo),
      catastro_rd_cbd_far_mix$predio_lon,
      catastro_rd_cbd_far_mix$predio_lat
    )
  )

summary(rd_cbd_far_mix)

## Table RDD Estimates ----
rd_table <- 
  map_df(
    list(rd_cbd_close_ind, rd_cbd_close_mix, rd_cbd_far_ind, rd_cbd_far_mix),
    table_rd
  )

rd_table <- 
  rd_table |> 
  mutate(
    dist_cbd = c(rep("Close", 2) , rep("Far", 2)),
    type = rep(c("Indigenous", "Mixed"), 2)
  ) |> 
  mutate(
    no_pueblos = c(5, 19, 38, 9)
  ) |> 
  mutate(
    pval_adj = q_val(pval)
  ) |> 
  select(dist_cbd, type, coef, se, lci, uci, pval, pval_adj, h, b, N, no_pueblos)

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
