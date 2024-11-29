# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/Tab2"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Table 2: Contemporary public goods: School supply ----
cemabe_rd <- 
  cemabe |> 
  filter(dist_n_pi <= 2500) |> 
  left_join(
    tibble(pindios_cdmx) |> 
      select(n_pi_index, pueblo)
  )

cemabe_rd_ind <- 
  cemabe_rd|> 
  filter(str_detect(t2b1, "Indigenous") == T) 

cemabe_rd_mix <- 
  cemabe_rd|> 
  filter(str_detect(t2b1, "Mixed") == T) 

names(cemabe_rd_ind)
school_quality <- 
  c("preeschool", "primary", "secondary", "public", "number_students_census", "student_teacher_ratio_1")

# Indigenous vs. Control
balance_schools_ind_c <- tibble()

for (i in c(school_quality)) {
  
  print(i)
  
  y <- scale(unlist(cemabe_rd_ind[,i]))
  x <- -cemabe_rd_ind$dist_n_pi
  c <- -cutoff
  covs <- 
    cbind(
      factor(cemabe_rd_ind$pueblo),
      factor(cemabe_rd_ind$cve_mun),
      cemabe_rd_ind$block_lon,
      cemabe_rd_ind$block_lat
    )
  
  RDD1 <- rdrobust(y, x, covs = covs, c = c, masspoints = T)
  
  balance_schools_ind_c <- 
    balance_schools_ind_c  |> 
    bind_rows(
      table_rd(RDD1) |> 
        mutate(
          variable = i
        )
    ) 
  
}


# Mixed vs. control
balance_schools_mix_c <- tibble()

for (i in c(school_quality)) {
  
  print(i)
  
  y <- scale(unlist(cemabe_rd_mix[,i]))
  x <- -cemabe_rd_mix$dist_n_pi
  c <- -cutoff
  covs <- 
    cbind(
      factor(cemabe_rd_mix$pueblo),
      factor(cemabe_rd_mix$cve_mun),
      cemabe_rd_mix$block_lon,
      cemabe_rd_mix$block_lat
    )
  
  RDD1 <- rdrobust(y, x, covs = covs, c = c, masspoints = T)
  
  balance_schools_mix_c <- 
    balance_schools_mix_c |>  
    bind_rows(
      table_rd(RDD1) |> 
        mutate(
          variable = i
        )
    ) 
  
}



balance <- 
  balance_schools_ind_c |> 
  select(variable, coef, pval) |> 
  mutate(
    type = "Indigenous"
  ) |> 
  bind_rows(
    balance_schools_mix_c |> 
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