# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/Fig2"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Figure 2: Identification illustration ----

# NOTE: Need baseline map from Figure 1
# Run source("scripts/scripts_results/IoR_Fig1.R")

m460_arc <- 0.004139669
mm500_arc_arc <- 0.00449964
mm1000_arc_arc <- 0.00899928

catastro_rd <- 
  catastro |> 
  filter(dist_n_pi <= 1000) |> 
  mutate(
    t2b1 = case_when(
      dist_n_pi <= cutoff & solo_indigenas == 1 ~ "Indigenous - Treated",
      dist_n_pi > cutoff & solo_indigenas == 1 ~ "Indigenous - Control",
      dist_n_pi <= cutoff & solo_indigenas == 0 ~ "Mixed - Treated",
      dist_n_pi > cutoff & solo_indigenas == 0 ~ "Mixed - Control",
    ),    
  ) |> 
  mutate(
    lvalor = log(valor_unitario_suelo)
  ) |>
  # Filter observations outside donut
  filter(
    dist_n_pi <= cutoff - donut | dist_n_pi > cutoff + donut
  ) |> 
  # Recenter running variable
  mutate(
    dist_n_pi = case_when(
      dist_n_pi <= cutoff ~ dist_n_pi + donut,
      TRUE ~ dist_n_pi - donut
    )
  ) |>
  tibble()

sf_use_s2(F)
p2 <- 
  p +
  # Mexico city properties - RDD
  geom_sf(
    data = catastro_rd |> 
      filter(dist_n_pi <= 1000) |> 
      mutate(
        t2b1 = fct_relevel(
          t2b1, 
          "Indigenous - Treated",
          "Indigenous - Control",
          "Mixed - Control",
          "Mixed - Treated",
        )
      ) |> 
      st_sf(),
    aes(fill = t2b1),
    lwd = .05
  ) +
  scale_fill_manual(values = palette2) +
  # Pueblos
  geom_point(
    data = pindios_cdmx |> 
      mutate(
        type = if_else(solo_indigenas == 1, "Indigenous", "Mixed")
      ),
    aes(longitud, latitud, shape = type),
    size = 2
  ) +
  # Pueblos m460_arc buffer ~ m500_arc varas
  geom_sf(
    data = pindios_cdmx |> 
      mutate(
        buffer = st_buffer(geometry, 0.004139669)
      ) |> 
      tibble() |> 
      st_sf(
        sf_column_name = "buffer"
      )
    ,
    alpha = 0, color = "black", lwd = .2
  )  +
  # Pueblos m500_arc buffer ~ 600 varas
  geom_sf(
    data = pindios_cdmx |> 
      mutate(
        buffer = st_buffer(geometry, 0.00449964)
      ) |> 
      tibble() |> 
      st_sf(
        sf_column_name = "buffer"
      )
    ,
    alpha = 0, color = "black", lwd = .2
  )  +
  # Pueblos m1000_arc buffer
  geom_sf(
    data = pindios_cdmx |>  
      mutate(
        buffer = st_buffer(geometry, 0.00899928)
      ) |> 
      tibble() |> 
      st_sf(
        sf_column_name = "buffer"
      )
    ,
    alpha = 0
  ) +
  # Labels
  geom_sf_label(
    data = pindios_cdmx,
    aes(label = n_pi_index), size = 2,
    nudge_x = -0.006, nudge_y = -0.006
  ) +
  labs(fill = "") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  ) 

p2 +
  # Zoom
  coord_sf(
    xlim = c(-99.275, -99.05),
    ylim = c(19.30, 19.525)
  ) +
  theme(
    legend.text = element_text(size = 7)
  ) +
  # CBD Label
  ggsflabel::geom_sf_label_repel(
    data = cbd |> mutate(label = "Colonial CBD"),
    aes(label = label), size = 2,
    nudge_x = +0.0275, nudge_y = +0.009
  ) 

ggsave(
  str_c(
    output_dir, "/rdd_identification_illustration_1.png"
  ),
  width = 8, height = 7
)


p2 +
  # Zoom
  coord_sf(
    xlim = c(-99.20, -99.125),
    ylim = c(19.325, 19.40)
  ) +
  theme(
    legend.text = element_text(size = 7)
  )

ggsave(
  str_c(
    output_dir, "/rdd_identification_illustration_2.png"
  ),
  width = 8, height = 7
)

#

# Figure C.2: RDD illustration – Pueblo locations and fuzzy boundaries as squares ----
# Add squares to show robustness of identification
p2 +
  # Pueblos m460_arc buffer ~ m500_arc varas
  geom_sf(
    data = pindios_cdmx |> 
      mutate(
        buffer = st_buffer(geometry, 0.004139669, endCapStyle = "SQUARE")
      ) |> 
      tibble() |> 
      st_sf(
        sf_column_name = "buffer"
      )
    ,
    alpha = 0, color = "black", lwd = .2
  )  +
  # Pueblos m500_arc buffer ~ 600 varas
  geom_sf(
    data = pindios_cdmx |> 
      mutate(
        buffer = st_buffer(geometry, 0.00449964, endCapStyle = "SQUARE")
      ) |> 
      tibble() |> 
      st_sf(
        sf_column_name = "buffer"
      )
    ,
    alpha = 0, color = "black", lwd = .2
  )  +
  # Zoom
  coord_sf(
    xlim = c(-99.20, -99.125),
    ylim = c(19.325, 19.40)
  ) +
  theme(
    legend.text = element_text(size = 7)
  ) 

ggsave(
  str_c(
    output_dir, "/rdd_identification_illustration_3.png"
  ),
  width = 8, height = 7
)

#