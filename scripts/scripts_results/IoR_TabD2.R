# Table D.2: Contemporary amenities: Street-level amenities ----
inv_rd <-
  inv_rd |>
  filter(dist_n_pi <= 1000)

inv_rd_ind <-
  inv_rd |>
  filter(str_detect(t2b1, "Indigenous") == T)

inv_rd_mix <-
  inv_rd |>
  filter(str_detect(t2b1, "Mixed") == T)

names(inv_rd)
amenities <-
  c(
    "pavimento", "rampa_silla", "paso_peatonal", "banqueta",
    "guarnicion", "ciclovia", "ciclocarril", "alumbrado", "letrero_calle",
    "tel_publico", "arboles", "semaforo", "semaforo_auditivo", "parada_transporte",
    "estacion_bici", "alcantarilla", "transporte_colectivo", "restriccion_peatones",
    "restriccion_autos", "d_puesto_semifijo", "d_comercio_abulante"
  )

# Indigenous vs. Control
balance_amenities_ind_c <- tibble()

for (i in c(amenities)) {
  
  print(i)
  
  y <- scale(unlist(inv_rd_ind[,i]))
  x <- -inv_rd_ind$dist_n_pi
  c <- -cutoff
  covs <- 
    cbind(
      factor(inv_rd_ind$pueblo),
      inv_rd_ind$block_lon,
      inv_rd_ind$block_lat
    )
  
  RDD1 <- rdrobust(y, x, covs = covs, c = c)
  
  balance_amenities_ind_c <- 
    balance_amenities_ind_c  |> 
    bind_rows(
      table_rd(RDD1) |> 
        mutate(
          variable = i
        )
    ) 
  
}


# Mixed vs. control
balance_amenities_mix_c <- tibble()

for (i in c(amenities)) {
  
  print(i)
  
  y <- scale(unlist(inv_rd_mix[,i]))
  x <- -inv_rd_mix$dist_n_pi
  c <- -cutoff
  covs <- 
    cbind(
      factor(inv_rd_mix$pueblo),
      inv_rd_mix$block_lon,
      inv_rd_mix$block_lat
    )
  
  RDD1 <- rdrobust(y, x, covs = covs, c = c)
  
  balance_amenities_mix_c <- 
    balance_amenities_mix_c |>  
    bind_rows(
      table_rd(RDD1) |> 
        mutate(
          variable = i
        )
    ) 
  
}



balance <- 
  balance_amenities_ind_c |> 
  select(variable, coef, pval) |> 
  mutate(
    type = "Indigenous"
  ) |> 
  bind_rows(
    balance_amenities_mix_c |> 
      select(variable, coef, pval) |> 
      mutate(
        type = "Mixed"
      )
  ) |> 
  group_by(type) |> 
  mutate(
    pval_adj = q_val(pval)
  ) |> 
  ungroup()


# Indigenous poly 1
cbind(
  balance |> 
    filter(type == "Indigenous") |> 
    select(-type, -pval)
) |> 
  # Mixed poly 1
  cbind(
    balance |> 
      filter(type == "Mixed") |> 
      select(-variable, -type, -pval)
  ) |> 
  # Table
  data.frame() |>
  kableExtra::kable(
    format = "latex", 
    escape = FALSE,
    align = "lccccc",
    table.envir = "threeparttable",
    booktabs = TRUE,
    linesep = '',
    digits = 3
  )

balance |> 
  filter(type == "Indigenous") |> 
  select(-type) |> 
  filter(pval_adj <= .1)

balance |> 
  filter(type == "Mixed") |> 
  select(-type) |> 
  filter(pval_adj <= .1)

#