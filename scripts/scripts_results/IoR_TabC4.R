# Parametric estimates + Conley standard errors ----
foo <- 
  catastro_rd |> 
  mutate(
    dist_recent = dist_n_pi - cutoff,
    treat = if_else(str_detect(t2b1, "Treated"), 1, 0)
  ) |> 
  filter(dist_recent >= -h1) |> 
  filter(dist_recent <= h1)

rd_p1 <- 
  feols(
    lvalor ~ treat * dist_recent + predio_lon + predio_lat,
    data = foo,
    vcov = vcov_conley(lon = ~ predio_lon, lat = ~ predio_lat, cutoff = h1*2/1000),
    fsplit = ~ solo_indigenas
  )

parametric_estimates <- tibble()
for (m in 1:length(rd_p1)) {
  
  parametric_estimates <- 
    parametric_estimates |> 
    bind_rows(
      broom::tidy(rd_p1[[m]]) |> 
        filter(term == "treat") |> 
        mutate(
          lci = estimate - 1.96*std.error,
          uci = estimate + 1.96*std.error,
          group = case_when(
            m == 1 ~ "All pueblos",
            m == 2 ~ "Mixed",
            m == 3 ~ "Indigenous",
          ),
          polynomial = 1,
          h = h1,
          N = nobs(rd_p1[[m]])
        ) |> 
        select(group, polynomial, estimate, std.error, lci, uci, p.value, h, N)
    )
  
}



foo2 <- 
  catastro_rd |> 
  mutate(
    dist_recent = dist_n_pi - cutoff,
    treat = if_else(str_detect(t2b1, "Treated"), 1, 0)
  ) |> 
  filter(dist_recent >= -h2) |> 
  filter(dist_recent <= h2)

rd_p2 <- 
  feols(
    lvalor ~ treat * (dist_recent + I(dist_recent^2)) + predio_lon + predio_lat,
    data = foo2,
    vcov = vcov_conley(lon = ~ predio_lon, lat = ~ predio_lat, cutoff = h2*2/1000),
    fsplit = ~ solo_indigenas
  )

for (m in 1:length(rd_p1)) {
  
  parametric_estimates <- 
    parametric_estimates |> 
    bind_rows(
      broom::tidy(rd_p2[[m]]) |> 
        filter(term == "treat") |> 
        mutate(
          lci = estimate - 1.96*std.error,
          uci = estimate + 1.96*std.error,
          group = case_when(
            m == 1 ~ "All pueblos",
            m == 2 ~ "Mixed",
            m == 3 ~ "Indigenous",
          ),
          polynomial = 2,
          h = h2,
          N = nobs(rd_p2[[m]])
        ) |> 
        select(group, polynomial, estimate, std.error, lci, uci, p.value, h, N)
    )
  
}

parametric_estimates <- 
  parametric_estimates |> 
  arrange(group, polynomial) |> 
  group_by(polynomial) |> 
  mutate(
    pval_adj = q_val(p.value)
  ) |> 
  ungroup() |> 
  select(group, polynomial, estimate, std.error, lci, uci, p.value, pval_adj, h, N) |> 
  mutate_if(
    is.double, ~ round(., 3)
  )

parametric_estimates |> 
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
