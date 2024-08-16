# Lightning ----

# Filter 1830 Colonial traza
cdmx_historical_1830 <- 
  cdmx_historical_bound |> 
  filter(year == 1830) |> 
  mutate(
    traza_1830 
  )

# Read amenity shapefile
alumbrado <- 
  st_read("data/source/historical_maps/alumbrado_publico_shapefiles/alumbrado_publico.shp") |> 
  st_transform(default_crs) |> 
  select(-Id) |> 
  mutate(
    type = "Street lighting",
    year = 1932
  ) 

# Ensure `s2` is disabled for spatial operations that require planar geometry
sf_use_s2(FALSE)

# Create the convex hull and bounding box for the amenity
convex_hull_amenity <- st_convex_hull(st_union(alumbrado)) 
convex_hull_amenity_bb <- st_bbox(convex_hull_amenity)
convex_hull_amenity_bb_sfc <- st_as_sfc(convex_hull_amenity_bb)

# Base map
base_map <- 
  ggplot() +
  # Municipalities
  geom_sf(
    data = cdmx |> st_sf(),
    lwd = .15, alpha = .01, fill = NA, color = "grey"
  ) +
  # Historical amenities: alumbrado
  geom_sf(
    data = alumbrado, color = "yellow", size = 2.5, lwd = .5
  ) +
  # Mexico city historical boundaries
  geom_sf(
    data = cdmx_historical_1830, 
    aes(fill = year, color = year), alpha = .3, lwd = .05
  ) +
  # Urban expansion colors
  viridis::scale_fill_viridis(discrete = TRUE, direction = 1, option = "C") +
  viridis::scale_color_viridis(discrete = TRUE, direction = 1, option = "C") +
  theme_minimal() +
  labs(color = "Urban expansion", fill = "Urban expansion") +
  # Set new scale
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  # Pueblos
  geom_point(
    data = pindios_cdmx |> 
      mutate(
        type = if_else(solo_indigenas == 1, "Indigenous", "Mixed")
      ),
    aes(longitud, latitud, color = type, shape = type),
    size = 1.5
  ) +
  # Pueblos' colors
  scale_color_manual(
    values = c(color_ind, color_mix)
  ) +
  # Labs
  labs(color = "", shape = "", x = "", y = "") +
  # No legend
  theme(
    legend.position = "bottom", axis.text = element_text(size = 6),
    plot.background = element_rect(fill = "white", color = NA), # Set plot background to white
    panel.background = element_rect(fill = "white", color = NA), # Set panel background to white
    legend.background = element_rect(fill = "white", color = NA), # Set legend background to white
  ) +
  ggspatial::annotation_scale()

# Inset map
inset_map <-
  base_map +
  # Zoom bounding box
  geom_sf(data = convex_hull_amenity_bb_sfc, fill = NA, color = "black", size = 1.5) +
  theme(
    legend.position = "none"
  )

# Main map (zoomed-in area)
sf_use_s2(TRUE)  # Enable s2 processing if needed
main_map <- 
  base_map +
  # CBD (Zócalo)
  geom_sf(
    data = cbd, color = "white", shape = 9, size = 2
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
          "Mixed"
        )
      ) |> 
      tibble() |> 
      st_sf(
        sf_column_name = "buffer"
      ),
    aes(color = treat),
    alpha = 0, lwd = .3
  ) +
  # Pueblos 500 buffer ~ 600 varas
  geom_sf(
    data = pindios_cdmx |> 
      mutate(
        buffer = st_buffer(geometry, 500),
        treat = if_else(solo_indigenas == 1, "Indigenous", "Mixed"),
        treat = fct_relevel(
          treat, 
          "Indigenous",
          "Mixed"
        )
      ) |> 
      tibble() |> 
      st_sf(
        sf_column_name = "buffer"
      ),
    aes(color = treat),
    alpha = 0, lwd = .3
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
    xlim = c(convex_hull_amenity_bb[1], convex_hull_amenity_bb[3]),
    ylim = c(convex_hull_amenity_bb[2], convex_hull_amenity_bb[4]),
  )

# Combine inset map and main map side by side
final_plot <- 
  plot_grid(
    main_map, inset_map, 
    ncol = 2, rel_widths = c(1, .35), # rel_heights = c(1, .5),
    align = "hv",   # Align horizontally and vertically
    axis = "tb"    # Add axis lines on top and bottom
  )

# Display the combined plot
final_plot

# Save plot
ggsave(
  str_c(
    plots_path, "/historical_mechanisms_lightning.png"
  ),
  width = 12.4, height = 7.15,
  bg = "white"  # Set the background to white when saving
)

#