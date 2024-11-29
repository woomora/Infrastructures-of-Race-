# Table D.1: Demographic and socioeconomic sorting at block level ----
# Balance for CDMX blocks

blocks_rd <- 
  tibble(cdmx_blocks) |>
  # Filter only blocks with population > 0 and
  filter(pobtot != 0) |>
  filter(pobtot > 0) |> 
  # Due to privacy rules, blocks with one or two inhabited dwellings
  # don't have socio-economic information
  filter(tvivhab > 2) |> 
  select(
    cvegeo, cve_mun, area, 
    dist_n_pi, t2b1, solo_indigenas, dist_cbd, pueblo, block_lon, block_lat,
    pobtot, pobfem, rel_h_m, pob0_14, pob15_64, pob65_mas, p12ym_solt, prom_hnv, pnacoe, presoe15, pcatolica,
    phog_ind, pob_afro, pcon_disc, pcon_limi, graproes, pea, pdesocup, psinder, phogjef_f, 
    pro_ocup_c, vivtot,
    tvivhab, vph_pisodt, vph_c_elec, vph_aguafv, vph_tinaco, vph_cister, vph_excsa,
    vph_drenaj, vph_refri, vph_lavad, vph_hmicro, vph_autom, vph_moto, vph_bici, 
    vph_radio, vph_tv, vph_pc, vph_telef, vph_cel, vph_inter, vph_stvp, vph_spmvpi, vph_cvj
  ) |> 
  mutate(
    pdesocup = pdesocup/pea,
    pob_density = pobtot/area,
  ) |> 
  mutate_at(
    vars(
      pobfem, pob0_14, pob15_64, pob65_mas, pnacoe, presoe15, phog_ind, pob_afro, 
      pcon_disc, pcon_limi, pea, psinder, phogjef_f, pcatolica
    ),
    ~ ./pobtot
  ) |>
  mutate_at(
    vars(vph_pisodt:vph_cvj),
    ~ ./tvivhab
  )  |> 
  mutate(
    pobweights = pobtot,
  ) 

# Scale Variables of interest at the population level
socio_demo <-
  c(
    "pob_density", "area", "pobtot", "vivtot", "pobfem", "rel_h_m", "pob0_14", "pob15_64", "pob65_mas",
    "prom_hnv", "pnacoe", "presoe15", "pcatolica", "phog_ind", "pob_afro", "pcon_disc",
    "p12ym_solt", "pea", "pdesocup", "psinder", "phogjef_f",
    "graproes", "pro_ocup_c", "vph_autom"
  )

blocks_rd <- 
  blocks_rd |> 
  mutate(
    pobweights = pobtot,
  ) |> 
  mutate_at(
    socio_demo, ~ scale(.)[,1]
  ) |> 
  # # Donut
  filter(
    dist_n_pi < cutoff - donut | dist_n_pi > cutoff + donut
  ) |>
  mutate(
    dist_n_pi = case_when(
      dist_n_pi < cutoff ~ dist_n_pi + donut,
      TRUE ~ dist_n_pi - donut
    )
  ) |>
  # Edit treatment variables
  mutate(
    treated = if_else(dist_n_pi <= cutoff, 1, 0),
    ind = if_else(solo_indigenas == 1, 1, 0),
  ) 

# Indigenous treated vs. Indigenous control
blocks_rd_ind <- 
  blocks_rd |> 
  filter(str_detect(t2b1, "Indigenous") == T)

# Mixed treated vs. Mixed control
blocks_rd_mix <- 
  blocks_rd |> 
  filter(str_detect(t2b1, "Mixed") == T)



# Indigenous vs. Control
balance_blocks_ind_c <- tibble()

for (i in c(socio_demo)) {
  
  print(i)
  
  y <- (unlist(blocks_rd_ind[,i]))
  x <- -blocks_rd_ind$dist_n_pi
  c <- -cutoff
  covs <- 
    cbind(
      factor(blocks_rd_ind$pueblo),
      blocks_rd_ind$block_lon,
      blocks_rd_ind$block_lat
    )
  
  RDD1 <- rdrobust(y, x, covs = covs, c = c)
  
  balance_blocks_ind_c <- 
    balance_blocks_ind_c  |> 
    bind_rows(
      table_rd(RDD1) |> 
        mutate(
          variable = i
        )
    ) 
  
}


# Mixed vs. control
balance_blocks_mix_c <- tibble()

for (i in c(socio_demo)) {
  
  print(i)
  
  y <- (unlist(blocks_rd_mix[,i]))
  x <- -blocks_rd_mix$dist_n_pi
  c <- -cutoff
  covs <- 
    cbind(
      factor(blocks_rd_mix$pueblo),
      blocks_rd_mix$block_lon,
      blocks_rd_mix$block_lat
    )
  
  RDD1 <- rdrobust(y, x, covs = covs, c = c)
  
  balance_blocks_mix_c <- 
    balance_blocks_mix_c |>  
    bind_rows(
      table_rd(RDD1) |> 
        mutate(
          variable = i
        )
    ) 
  
}


balance <- 
  balance_blocks_ind_c |> 
  select(variable, coef, pval) |> 
  mutate(
    type = "Indigenous"
  ) |> 
  bind_rows(
    balance_blocks_mix_c |> 
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