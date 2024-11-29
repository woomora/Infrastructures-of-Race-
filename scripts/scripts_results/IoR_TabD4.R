# Table D.4: Contemporary amenities: Crime ----
crime_rd <-
  crime |>
  mutate(
    weekday = weekdays(date_event)
  ) |> 
  filter(dist_n_pi <= 1000) |> 
  select(geometry, month_event, weekday, robbery:homicide) |> 
  st_join(
    cdmx_blocks_rd |> 
      select(geometry, cvegeo, cve_mun, dist_n_pi, t2b1, pueblo, block_lon, block_lat, pobtot) |> 
      st_sf()
  ) |> 
  tibble() |> 
  group_by(cvegeo, cve_mun, month_event, weekday, dist_n_pi, t2b1, pueblo, block_lon, block_lat, pobtot) |> 
  summarise(
    crimes = n(),
    robbery = sum(robbery),
    minor_crimes = sum(minor_crimes),
    major_crimes = sum(major_crimes),
    homicide = sum(homicide)
  ) |> 
  ungroup()

crime_rd_ind <-
  crime_rd |>
  filter(str_detect(t2b1, "Indigenous") == T)

crime_rd_mix <-
  crime_rd |>
  filter(str_detect(t2b1, "Mixed") == T)

crimes <- 
  c("crimes", "robbery", "minor_crimes", "major_crimes", "homicide")

# Indigenous vs. Control
balance_crimes_ind_c <- tibble()

for (i in c(crimes)) {
  
  print(i)
  
  y <- scale(unlist(crime_rd_ind[,i]))
  x <- -crime_rd_ind$dist_n_pi
  c <- -cutoff
  covs <- 
    cbind(
      factor(crime_rd_ind$pueblo),
      crime_rd_ind$block_lon,
      crime_rd_ind$block_lat,
      factor(crime_rd_ind$month_event),
      factor(crime_rd_ind$weekday),
      log(1 + crime_rd_ind$pobtot)
    )
  
  RDD1 <- rdrobust(y, x, covs = covs, c = c, masspoints = T)
  
  balance_crimes_ind_c <- 
    balance_crimes_ind_c  |> 
    bind_rows(
      table_rd(RDD1) |> 
        mutate(
          variable = i
        )
    ) 
  
}


# Mixed vs. control
balance_crimes_mix_c <- tibble()

for (i in c(crimes)) {
  
  print(i)
  
  y <- scale(unlist(crime_rd_mix[,i]))
  x <- -crime_rd_mix$dist_n_pi
  c <- -cutoff
  covs <- 
    cbind(
      factor(crime_rd_mix$pueblo),
      crime_rd_mix$block_lon,
      crime_rd_mix$block_lat,
      factor(crime_rd_mix$month_event),
      factor(crime_rd_mix$weekday),
      log(1 + crime_rd_mix$pobtot)
    )
  
  RDD1 <- rdrobust(y, x, covs = covs, c = c, masspoints = T)
  
  balance_crimes_mix_c <- 
    balance_crimes_mix_c |>  
    bind_rows(
      table_rd(RDD1) |> 
        mutate(
          variable = i
        )
    ) 
  
}


balance <- 
  balance_crimes_ind_c |> 
  select(variable, coef, pval) |> 
  mutate(
    type = "Indigenous"
  ) |> 
  bind_rows(
    balance_crimes_mix_c |> 
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