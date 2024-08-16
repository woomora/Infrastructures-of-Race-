## RDD Estimates controlling for zoning ----
# Treatment vs Control ----
rd_all_c <- 
  rdrobust(
    (catastro_rd$lvalor), -catastro_rd$dist_n_pi, c = -cutoff,
    covs = cbind(
      factor(catastro_rd$pueblo),
      catastro_rd$predio_lon,
      catastro_rd$predio_lat,
      factor(catastro_rd$uso_construccion)
    )
  )

summary(rd_all_c)

rd_all_c2 <- 
  rdrobust(
    (catastro_rd$lvalor), -catastro_rd$dist_n_pi, c = -cutoff,
    p = 2,
    covs = cbind(
      factor(catastro_rd$pueblo),
      catastro_rd$predio_lon,
      catastro_rd$predio_lat,
      factor(catastro_rd$uso_construccion)
    )
  )

summary(rd_all_c2)


# Indigenous vs Control ----

rd_ind_c <- 
  rdrobust(
    (catastro_rd_ind$lvalor), -catastro_rd_ind$dist_n_pi, c = -cutoff,
    covs = cbind(
      factor(catastro_rd_ind$pueblo),
      catastro_rd_ind$predio_lon,
      catastro_rd_ind$predio_lat,
      factor(catastro_rd_ind$uso_construccion)
    )
  )

summary(rd_ind_c)



rd2_ind_c <- 
  rdrobust(
    (catastro_rd_ind$lvalor), -catastro_rd_ind$dist_n_pi, c = -cutoff,
    p = 2, 
    covs = cbind(
      factor(catastro_rd_ind$pueblo),
      catastro_rd_ind$predio_lon,
      catastro_rd_ind$predio_lat,
      factor(catastro_rd_ind$uso_construccion)
    )
  )

summary(rd2_ind_c)


# Mixed vs Control ----

rd_mix_c <- 
  rdrobust(
    (catastro_rd_mix$lvalor), -catastro_rd_mix$dist_n_pi, c = -cutoff,
    covs = cbind(
      factor(catastro_rd_mix$pueblo),
      catastro_rd_mix$predio_lon,
      catastro_rd_mix$predio_lat,
      factor(catastro_rd_mix$uso_construccion)
    )
  )

summary(rd_mix_c)

rd_mix_c2 <- 
  rdrobust(
    (catastro_rd_mix$lvalor), -catastro_rd_mix$dist_n_pi, c = -cutoff,
    p = 2, 
    covs = cbind(
      factor(catastro_rd_mix$pueblo),
      catastro_rd_mix$predio_lon,
      catastro_rd_mix$predio_lat,
      factor(catastro_rd_mix$uso_construccion)
    )
  )

summary(rd_mix_c2)

# Table ----
rd_table <- 
  map_df(
    list(rd_all_c, rd_all_c2, rd_ind_c, rd2_ind_c, rd_mix_c, rd_mix_c2),
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
