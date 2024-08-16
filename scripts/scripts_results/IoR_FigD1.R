# Figure D.1: Historical mechanisms – Roads (with asphalt) ----
roads <- 
  st_read("data/source/historical_maps/caminos_shapefiles/Caminos.shp") |> 
  st_transform(default_crs) |> 
  st_cast("LINESTRING") 


# Petrolized trail
petrolized_trail <- 
  roads |> 
  filter(Type == "Petrolizados") 

sf_use_s2(F)
convex_hull_roads <- st_convex_hull(st_union(cdmx)) 
convex_hull_roads <- st_bbox(convex_hull_roads)

p <- 
  ggplot() +
  # Municipalities
  geom_sf(
    data = cdmx |> st_sf(),
    lwd = .15, alpha = .01, fill = NA, color = "grey"
  ) +
  # Historical amenities: tramways
  geom_sf(
    data = petrolized_trail, color = "purple", size = 2.5, lwd = .5
  ) +
  # Mexico city historical boundaries
  geom_sf(
    data = cdmx_historical_1830, 
    aes(fill = traza_1830, color = traza_1830), alpha = .3, lwd = .05
  ) +
  viridis::scale_fill_viridis(discrete = T, direction = 1, option = "C") +
  viridis::scale_color_viridis(discrete = T, direction = 1, option = "C") +
  labs(color = "Urban expansion", fill = "Urban expansion") +
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  # CBD (Zócalo)
  geom_sf(
    data = cbd, color = "black", shape = 9, size = 2
  ) +
  theme_minimal() 

sf_use_s2(T)
p +
  # Pueblos
  geom_point(
    data = pindios_cdmx |> 
      mutate(
        type = if_else(solo_indigenas == 1, "Indigenous", "Mixed")
      ),
    aes(longitud, latitud, color = type, shape = type),
    size = 2
  ) +
  # Pueblos 420 buffer ~ 500 varas
  geom_sf(
    data = pindios_cdmx |> 
      mutate(
        buffer = st_buffer(geometry, 420),
        treat = if_else(solo_indigenas == 1, "Indigenous", "Mixed"),
        treat = fct_relevel(
          treat, 
          "Indigenous",
          "Mixed",
        )
      ) |> 
      tibble() |> 
      st_sf(
        sf_column_name = "buffer"
      )
    , aes(color = treat),
    alpha = 0, lwd = .3
  )  +
  # Pueblos 500 buffer ~ 600 varas
  geom_sf(
    data = pindios_cdmx |> 
      mutate(
        buffer = st_buffer(geometry, 500),
        treat = if_else(solo_indigenas == 1, "Indigenous", "Mixed"),
        treat = fct_relevel(
          treat, 
          "Indigenous",
          "Mixed",
        )
      ) |> 
      tibble() |> 
      st_sf(
        sf_column_name = "buffer"
      )
    , aes(color = treat),
    alpha = 0, lwd = .3
  ) +
  scale_color_manual(
    values = c(color_ind, color_mix)
  ) +
  # Pueblos Labels
  geom_sf_text(
    data = pindios_cdmx,
    aes(label = n_pi_index), size = 2,
    nudge_x = -0.0015, nudge_y = -0.0015
  )  +
  # CBD Label
  ggsflabel::geom_sf_label_repel(
    data = cbd |> mutate(label = "Colonial CBD"),
    aes(label = label), size = 3,
    nudge_x = +0.0275, nudge_y = +0.009
  ) +
  coord_sf(
    xlim = c(convex_hull_roads[1], convex_hull_roads[3]),
    ylim = c(convex_hull_roads[2], convex_hull_roads[4]),
  ) +
  labs(color = "", shape = "", x = "", y = "") +
  theme(
    legend.position = "bottom", axis.text = element_text(size = 6)
  ) +
  ggspatial::annotation_scale()


ggsave(
  str_c(
    plots_path, "/historical_mechanisms_roads_asphalt.png"
  ),
  width = 6.66, height = 8
)
