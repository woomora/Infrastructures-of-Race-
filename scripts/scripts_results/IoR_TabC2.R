# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/TabC2"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Balance in Zoning ----

property_char <-
  c(
    "anio_construccion", "superficie_construccion", "niveles",
    "uso_suelo_hab", "uso_suelo_hab_com", "uso_suelo_verdes", "uso_suelo_centrob",
    "uso_suelo_equip", "uso_suelo_industrial", "uso_suelo_no_zoning"
  )

balance_property_ind <- tibble()
balance_property_mix <- tibble()

for (i in property_char) {
  
  tryCatch({
    
    print(i)
    
    c <- -cutoff
    
    # Indigenous
    y <- scale(unlist(catastro_rd_ind[,i]))
    x <- -catastro_rd_ind$dist_n_pi
    covs <-
      cbind(
        factor(catastro_rd_ind$pueblo),
        catastro_rd_ind$predio_lon,
        catastro_rd_ind$predio_lat
      )
    
    RDD <- rdrobust(y, x, covs = covs, c = c)
    
    balance_property_ind <-
      balance_property_ind |>
      bind_rows(
        table_rd(RDD) |>
          mutate(
            variable = i
          )
      ) 
    
    # Mixed
    y <- scale(unlist(catastro_rd_mix[,i]))
    x <- -catastro_rd_mix$dist_n_pi
    covs <-
      cbind(
        factor(catastro_rd_mix$pueblo),
        catastro_rd_mix$predio_lon,
        catastro_rd_mix$predio_lat
      )
    
    RDD <- rdrobust(y, x, covs = covs, c = c)
    
    balance_property_mix <-
      balance_property_mix |>
      bind_rows(
        table_rd(RDD) |>
          mutate(
            variable = i
          )
      ) 
    
  }, error=function(e){})
  
}


balance <- 
  balance_property_ind |> 
  mutate(
    type = "Indigenous"
  ) |> 
  bind_rows(
    balance_property_mix |> 
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
rbind(
  balance |> 
    filter(type == "Indigenous") |> 
    select(variable, coef, se, lci, uci, pval, pval_adj, h, b, N)
) |> 
  # Mixed poly 1
  rbind(
    balance |> 
      filter(type == "Mixed") |> 
      select(variable, coef, se, lci, uci, pval, pval_adj, h, b, N)
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
