### Historical data for persistence mechanisms ----
# Convert CDMX to raster ----
# Create a raster of aprox. 500 x 500 meters grid cells
raster_template <- raster(extent(cdmx), 
                          res = c(0.00449, 0.00449), 
                          crs = crs(cdmx))

# Rasterize the polygon to create a raster layer
cdmx_grid <- 
  rasterize(cdmx, raster_template, field = 1) |> 
  # Convert the raster object to a stars object
  st_as_stars() |> 
  # Convert to sf
  st_as_sf() |> 
  select(-layer)

cdmx_grid <- 
  cdmx_grid |> 
  mutate(
    grid_id = 1:nrow(cdmx_grid),
    centroid = st_centroid(geometry)
  ) |> 
  select(grid_id, everything())

# Join 1830 Traza CDMX
cdmx_historical_1830 <- 
  cdmx_historical_bound |> 
  filter(year == 1830) |> 
  rename(
    traza_1830 = year
  ) 

cdmx_grid <- 
  cdmx_grid |> 
  st_join(
    cdmx_historical_1830
  ) |> 
  mutate(
    traza_1830 = if_else(is.na(traza_1830), 0, 1)
  )

# Join 1970 CDMX sprawl
cdmx_historical_1970 <- 
  cdmx_historical_bound |> 
  filter(year == 1970) |> 
  rename(
    urbsprawl_1970 = year
  ) 

sf_use_s2(F)
cdmx_grid <- 
  cdmx_grid |> 
  st_join(
    cdmx_historical_1970
  ) |> 
  mutate(
    urbsprawl_1970 = if_else(is.na(urbsprawl_1970), 0, 1)
  )

# Distance to nearest pueblo for each grid
cdmx_grid <-
  cdmx_grid |>
  mutate(
    n_pi_index = st_nearest_feature(centroid, pindios_cdmx),
    area = units::set_units(st_area(geometry), km^2)
  ) |>
  glimpse()

# Join info of nearest Pueblo for each unit
cdmx_grid <-
  cdmx_grid |>
  left_join(
    pindios_cdmx |>
      select(pueblo, n_pi_index, solo_indigenas, geometry) |> 
      tibble() |>
      rename(
        pueblo_geom = geometry
      )
  )

# Calculate distance to nearest pueblo
sf_use_s2(T)
cdmx_grid <-
  cdmx_grid |>
  mutate(
    dist_n_pi = st_distance(centroid, pueblo_geom, by_element = T),
    dist_n_pi = units::drop_units(dist_n_pi),
    treatment = case_when(
      traza_1830 == 1 ~ "Colonial Traza",
      urbsprawl_1970 == 1 ~ "1970's Urban extension",
      T ~ "Other"
    ),
    treatment = case_when(
      dist_n_pi <= 500 & solo_indigenas == 1 ~ "Indigenous",
      dist_n_pi <= 500 & solo_indigenas == 0 ~ "Mixed",
      T ~ treatment
    ),
    treatment = case_when(
      n_pi_index %in% c(1,2) & dist_n_pi <= 500 ~ "Colonial Traza",
      T ~ treatment
    ),
    treatment = fct_relevel(
      treatment, 
      "Other", 
      "Indigenous", 
      "Mixed",
      "Colonial Traza",
      "1970's Urban extension"
    )
  ) |>
  select(-dist_n_pi, -pueblo_geom, -pueblo, -solo_indigenas) |> 
  glimpse()

# Figure D.4: Historical mechanisms – Mexico City grid and cell classification 
# Illustration of CDMX grid 500x500 meters
cdmx_grid |> 
  ggplot(aes(fill = treatment)) +
  geom_sf(lwd = .1) +
  theme_minimal() +
  labs(x = "", y = "", fill= "Treatment") +
  scale_fill_manual(
    values = c("#dadada", "#5BBCD6", "#FF0000", "#46327e", "#fde725")
  )

# Save illustration
ggsave(
  str_c(
    plots_path, "/historical_mechanisms_grid500m_illustration.png"
  ),
  width = 8, height = 7
)

#
# Main function to process shapefiles ----
process_shapefile <- function(shapefile_path, cdmx_grid, treatment_type, year, filter_type = NULL, segment_length = 500) {
  
  # Print the type of treatment
  print(treatment_type)
  
  # Read the shapefile and transform the CRS
  shapefile_data <- st_read(shapefile_path) |> 
    st_transform(st_crs(cdmx_grid))  # Ensure the CRS matches the grid
  
  # Determine the geometry type
  geom_type <- unique(st_geometry_type(shapefile_data))
  
  # Handle POINT or MULTIPOINT geometries
  if (geom_type %in% c("POINT", "MULTIPOINT")) {
    
    # Apply any filter if provided
    if (!is.null(filter_type)) {
      shapefile_data <- shapefile_data |> filter(Type %in% filter_type)
    }
    
    # Spatial join with CDMX grid using st_within
    foo <- shapefile_data |> 
      st_join(cdmx_grid, join = st_within) |> 
      tibble() |> 
      group_by(grid_id) |> 
      count() |> 
      ungroup()
    
  } else if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
    # Handle POLYGON or MULTIPOLYGON geometries by converting to centroids
    
    shapefile_data <- shapefile_data |> mutate(geometry = st_centroid(geometry))
    
    # Apply any filter if provided
    if (!is.null(filter_type)) {
      shapefile_data <- shapefile_data |> filter(Type %in% filter_type)
    }
    
    # Spatial join with CDMX grid using st_within
    foo <- shapefile_data |> 
      st_join(cdmx_grid, join = st_within) |> 
      tibble() |> 
      group_by(grid_id) |> 
      count() |> 
      ungroup()
    
  } else if (geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
    # Handle LINESTRING or MULTILINESTRING geometries
    
    # Apply any filter if provided
    if (!is.null(filter_type)) {
      shapefile_data <- shapefile_data |> filter(Type %in% filter_type)
    }
    
    # Transform to a suitable planar CRS for segmentation
    segmented_lines <- 
      shapefile_data |> st_cast("LINESTRING") |> 
      # Segment the line into smaller pieces
      stplanr::line_segment(segment_length = segment_length)
    
    # Spatial join with CDMX grid
    foo <- segmented_lines |> 
      st_join(cdmx_grid) |> 
      mutate(type = treatment_type, year = year) |> 
      tibble() |> 
      group_by(grid_id) |> 
      count() |> 
      ungroup()
    
  }
  
  # Summarize number of units, area, and units per area by treatment status
  foo2 <- cdmx_grid |> 
    left_join(foo, by = "grid_id") |> 
    tibble() |> 
    group_by(treatment) |> 
    summarise(
      units = sum(n, na.rm = TRUE),
      area = sum(area, na.rm = TRUE)
    ) |> 
    ungroup() |> 
    mutate(
      units_per_km2 = units / area,
      type = treatment_type, 
      year = year
    )
  
  return(foo2)
}

# List of shapefiles with their corresponding type and year ----
shapefiles_info <- list(
  # Point-based shapefiles
  list(path = "data/source/historical_maps/alumbrado_publico_shapefiles/alumbrado_publico.shp", type = "Street lighting", year = 1932),
  list(path = "data/source/historical_maps/mercados_publicos_1930s/mercados.shp", type = "Public markets", year = 1932),
  list(path = "data/source/historical_maps/edificios_shapefiles/edificios.shp", type = "Buildings", year = 1932),
  list(path = "data/source/historical_maps/post_offices_shapefiles/post_offices_1960_3.shp", type = "Post offices", year = 1962),
  list(path = "data/source/historical_maps/teatros_cines_shapefiles/Teatros.shp", type = "Theaters and cinemas", year = 1962),
  list(path = "data/source/historical_maps/hoteles_shapefiles/Hoteles.shp", type = "Hotels", year = 1962),
  list(path = "data/source/historical_maps/bancos_shapefiles/Banamex_1963.shp", type = "Banks", year = 1963),
  
  # Line-based shapefiles
  list(
    path = "data/source/historical_maps/steam_horse_railways_shapefiles/Railways.shp", 
    type = "Steam railway tracks", 
    year = 1885, 
    filter_type = c("Steam_Railway", "Steam Railway"),
    segment_length = 10
  ),
  list(
    path = "data/source/historical_maps/tranvias/tranvias.shp", 
    type = "Tramway tracks", 
    year = 1962, 
    segment_length = 10
  ),
  list(
    path = "data/source/historical_maps/caminos_shapefiles/Caminos.shp", 
    type = "Horseshoe trail", 
    year = 1947, 
    filter_type = "Camino_de_Herradura",
    segment_length = 10
  ),
  list(
    path = "data/source/historical_maps/caminos_shapefiles/Caminos.shp", 
    type = "Roads (petrolized)", 
    year = 1947, 
    filter_type = "Petrolizados",
    segment_length = 10
  )
)


# Process each shapefile and append to the main table ----
# Initialize amenities_table
amenities_table <- tibble()

for (shapefile_info in shapefiles_info) {
  foo2 <- process_shapefile(shapefile_info$path, cdmx_grid, shapefile_info$type, shapefile_info$year)
  amenities_table <- bind_rows(amenities_table, foo2)
}

# Table with final results ----
foo <- 
  amenities_table |> 
  mutate(
    type = str_c(type, " (", year, ")"),
  ) |> 
  group_by(type) |> 
  mutate(
    units_per_km2_max = max(units::drop_units(units_per_km2), na.rm = T),
  ) |> 
  ungroup()

# Transform the tibble to a wide format
foo_wide <- 
  foo |> 
  mutate(
    units_per_km2 = units::drop_units(units_per_km2)
  ) |> 
  filter(treatment != "Other") |> 
  select(type, treatment, units_per_km2) |> 
  pivot_wider(names_from = treatment, values_from = units_per_km2)

# Generate the LaTeX table without additional formatting
latex_table <- foo_wide |> 
  kable(
    format = "latex", booktabs = TRUE, 
    caption = "Historical mechanisms – Amenities density by type of area",
    digits = 2
  )

# Print the LaTeX table
cat(latex_table)

# Add area
foo <- 
  amenities_table |> 
  mutate(
    type = str_c(type, " (", year, ")"),
  ) |> 
  group_by(treatment) |> 
  summarise(
    area = mean(units::drop_units(area), na.rm = T),
  ) |> 
  ungroup()

# Transform the tibble to a wide format
foo_wide <- 
  foo |> 
  filter(treatment != "Other") |> 
  select(treatment, area) |> 
  pivot_wider(names_from = treatment, values_from = area)

# Generate the LaTeX table without additional formatting
latex_table <- foo_wide |> 
  kable(
    format = "latex", booktabs = TRUE, 
    caption = "Historical mechanisms – Amenities density by type of area",
    digits = 2
  )

# Print the LaTeX table
cat(latex_table)

# Plot results ----
# Plot 
foo <- 
  amenities_table |> 
  mutate(
    type = str_c(type, " (", year, ")"),
  ) |> 
  group_by(type) |> 
  mutate(
    units_per_km2_max = max(units::drop_units(units_per_km2), na.rm = T),
  ) |> 
  ungroup()

foo <- 
foo |> 
  mutate(
    density_column = case_when(
      units_per_km2_max <= 2 ~ "1",
      units_per_km2_max > 2 & units_per_km2_max < 10 ~ "2",
      TRUE ~ "3"
    )
  ) 

foo |> 
  ggplot(aes(units_per_km2, reorder(type, -year), fill = treatment)) +
  geom_point(
    aes(color = treatment, shape = treatment), size = 3.5
  ) +
  labs(x = "Unit density", y = "", fill = "", color = "", shape = "") +
  facet_wrap(
    ~ density_column, scales = "free", labeller = label_both,
    nrow = 3, ncol = 1
  ) +
  scale_color_manual(
    values = c("#dadada", "#5BBCD6", "#FF0000", "#46327e", "#fde725")
  ) +
  scale_shape_manual(
    values = c(0, 16, 17, 9, 0)
  ) +
  theme(
    strip.text.x = element_blank(),  # Hide facet strip labels
    strip.background = element_blank()  # Remove the facet strip background
  )

ggsave(
  str_c(
    plots_path, "/historical_amenities_density_plot.png"
  ),
  width = 12, height = 8.5
)
