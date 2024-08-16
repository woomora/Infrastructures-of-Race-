# Table D.3: Contemporary amenities: Establishments ----
denue_rd <- 
  denue_rd |> 
  tibble() |> 
  filter(dist_n_pi <= 1500)

denue_rd_ind <-
  denue_rd |>
  filter(str_detect(t2b1, "Indigenous") == T)

denue_rd_mix <-
  denue_rd |>
  filter(str_detect(t2b1, "Mixed") == T)

firm_char <- 
  c(names(denue_rd_ind)[52:71][-19], 
    "microemp", "pequenaemp", "medianaemp", "grandeemp", "fijo"
  )

# Indigenous vs. Control
balance_firm_char_ind_c <- tibble()

for (i in c(firm_char)) {
  
  print(i)
  
  y <- scale(unlist(denue_rd_ind[,i]))
  x <- -denue_rd_ind$dist_n_pi
  c <- -cutoff
  covs <- 
    cbind(
      factor(denue_rd_ind$pueblo),
      denue_rd_ind$block_lon,
      denue_rd_ind$block_lat
    )
  
  RDD1 <- rdrobust(y, x, covs = covs, c = c)
  
  balance_firm_char_ind_c <- 
    balance_firm_char_ind_c  |> 
    bind_rows(
      table_rd(RDD1) |> 
        mutate(
          variable = i
        )
    ) 
  
}


# Mixed vs. control
balance_firm_char_mix_c <- tibble()

for (i in c(firm_char)) {
  
  print(i)
  
  y <- scale(unlist(denue_rd_mix[,i]))
  x <- -denue_rd_mix$dist_n_pi
  c <- -cutoff
  covs <- 
    cbind(
      factor(denue_rd_mix$pueblo),
      denue_rd_mix$block_lon,
      denue_rd_mix$block_lat
    )
  
  RDD1 <- rdrobust(y, x, covs = covs, c = c)
  
  balance_firm_char_mix_c <- 
    balance_firm_char_mix_c |>  
    bind_rows(
      table_rd(RDD1) |> 
        mutate(
          variable = i
        )
    ) 
  
}



balance <- 
  balance_firm_char_ind_c |> 
  select(variable, coef, pval) |> 
  mutate(
    type = "Indigenous"
  ) |> 
  bind_rows(
    balance_firm_char_mix_c |> 
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


balance <- 
  balance |> 
  mutate(
    variable = str_remove_all(variable, "type_"),
    variable = str_replace_all(variable, "_", " "),
  )


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