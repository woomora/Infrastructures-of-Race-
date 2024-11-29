# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/TabB1"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Balance in locational fundamentals ----
# Define the file path for the output CSV file
loc_fund_balance_path <- str_c(output_dir, "/balance_locfund.csv")

# Run the processing only if the file does not exist
if (!file.exists(loc_fund_balance_path)) {
  
  loc_fund <- c("altitude", "slope", "temperature", "precipitation", "humidity")
  rasters <- list(altitude, slope, temperature, precipitation, humidity, termal_var)
  
  loc_fund_balance <- tibble()
  
  for (i in 1:length(loc_fund)) {
    
    print(loc_fund[i])
    
    foo2 <-
      rasterToPoints(rasters[i][[1]]) |>
      data.frame() |>
      tibble()
    
    colnames(foo2)[3] = "values"
    
    foo2 <-
      foo2 |>
      mutate(lon = x, lat = y) |>
      st_as_sf(coords = c("x", "y")) |>
      st_set_crs(st_crs(cdmx)) |>
      mutate(
        id = 1:nrow(foo2)
      )
    
    # Graphs
    p <-
      ggplot(foo2) +
      geom_tile(aes(x = lon, y = lat, fill = values)) +
      geom_sf(data = cdmx |> st_sf(), alpha = .1, color = "white", lwd = .05) +
      viridis::scale_fill_viridis(direction = 1) +
      theme_minimal() +
      labs(x = "", y = "", fill = str_to_title(loc_fund[i]), color = "Pueblo", shape = "Pueblo") +
      ggspatial::annotation_scale()
    
    p +
      # Pueblos
      geom_point(
        data = pindios_cdmx |>
          mutate(
            type = if_else(solo_indigenas == 1, "Indigenous", "Mixed")
          ),
        aes(longitud, latitud, shape = type, color = type),
        size = 2
      )  +
      scale_color_manual(values = c(color_ind, color_mix))
    
    ggsave(
      str_c(
        output_dir, "/balance_", loc_fund[i], ".png"
      ),
      dpi = 300,
      height = 7.84,
      width = 7.84
    )
    
    # Formal testing
    foo2 <-
      foo2 |>
      mutate(
        n_pi_index = st_nearest_feature(geometry, pindios_cdmx),
      ) |>
      glimpse()
    
    foo2 <-
      foo2 |>
      left_join(
        pindios_cdmx |>
          select(-cve_mun) |>
          tibble() |>
          rename(
            pueblo_lon = longitud,
            pueblo_lat = latitud,
            pueblo_geom = geometry
          )
      )
    
    foo2 <-
      foo2 |>
      mutate(
        dist_n_pi = st_distance(geometry, pueblo_geom, by_element = T),
        dist_n_pi = units::drop_units(dist_n_pi),
        t2b1 = case_when(
          dist_n_pi <= 500 & solo_indigenas == 1 ~ "Indigenous - Treated",
          dist_n_pi > 500 & solo_indigenas == 1 ~ "Indigenous - Control",
          dist_n_pi <= 500 & solo_indigenas == 0 ~ "Mixed - Treated",
          dist_n_pi > 500 & solo_indigenas == 0 ~ "Mixed - Control",
        )
      ) |>
      glimpse()
    
    foo2_rd <-
      foo2 |>
      filter(dist_n_pi <= 1000)
    
    foo2_rd_ind <-
      foo2_rd |>
      filter(str_detect(t2b1, "Indigenous") == T)
    
    foo2_rd_mix <-
      foo2_rd |>
      filter(str_detect(t2b1, "Mixed") == T)
    
    # Indigenous
    y <- scale(foo2_rd_ind$values)
    x <- -foo2_rd_ind$dist_n_pi
    covs <-
      cbind(
        factor(foo2_rd_ind$pueblo),
        foo2_rd_ind$lon,
        foo2_rd_ind$lat
      )
    
    RDD <- rdrobust(y, x, covs = covs, c = -cutoff) 
    RDD2 <- rdrobust(y, x, covs = covs, c = -cutoff, p = 2)
    
    loc_fund_balance <-
      loc_fund_balance |>
      bind_rows(
        table_rd(RDD) |>
          mutate(
            variable = loc_fund[i],
            type = "Indigenous"
          )
      ) |>
      bind_rows(
        table_rd(RDD2) |>
          mutate(
            variable = loc_fund[i],
            type = "Indigenous"
          )
      )
    
    # Mixed
    y <- scale(foo2_rd_mix$values)
    x <- -foo2_rd_mix$dist_n_pi
    covs <-
      cbind(
        factor(foo2_rd_mix$pueblo),
        foo2_rd_mix$lon,
        foo2_rd_mix$lat
      )
    
    RDD <- rdrobust(y, x, covs = covs, c = -cutoff)
    RDD2 <- rdrobust(y, x, covs = covs, c = -cutoff, p = 2)
    
    loc_fund_balance <-
      loc_fund_balance |>
      bind_rows(
        table_rd(RDD) |>
          mutate(
            variable = loc_fund[i],
            type = "Mixed"
          )
      ) |>
      bind_rows(
        table_rd(RDD2) |>
          mutate(
            variable = loc_fund[i],
            type = "Mixed"
          )
      )
    
  }
  
  loc_fund_balance |>
    write_csv(loc_fund_balance_path)
  
}

# Continue with the remaining part of the script, which loads and processes the file
loc_fund_balance <- read_csv(loc_fund_balance_path)

loc_fund_balance <-
  loc_fund_balance |>
  group_by(poly, type) |>
  mutate(
    pval_adj = q_val(pval)
  ) |>
  ungroup()

loc_fund_balance |> 
  filter(poly == 1) |> 
  filter(type == "Indigenous") |> 
  select(-type, -poly) |> 
  select(variable, coef, se, lci, uci, pval, pval_adj, everything()) |> 
  rbind(
    loc_fund_balance |> 
      filter(poly == 1) |> 
      filter(type == "Mixed") |> 
      select(-type, -poly) |> 
      select(variable, coef, se, lci, uci, pval, pval_adj, everything()) 
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
