# ------------------------------------------------------------------------------
# Fuzzy cutoff and donut ----
# 1 vara aprox. 0.84 meters
varas500 <- 420 # 500*.84
varas600 <- 500 # 600*.84

cutoff <- 460 # (varas500 + varas600)/2
donut <- 45

#
# 2015 Municipalities shp ----
# Load the shapefile containing the municipalities' boundaries and attributes
mun_shp <- 
  st_read("data/source/shapefiles/municipalities.shp") |> # Read the shapefile into an sf object
  rename(
    id_metro_area = id_mtr_ # Rename the 'id_mtr_' column to 'id_metro_area' for clarity
  )

# Filter the shapefile to retain only the records corresponding to Mexico City (CDMX)
cdmx <- mun_shp |> filter(cve_ent == "09") # 'cve_ent' with value "09" corresponds to Mexico City

# Extract the Coordinate Reference System (CRS) from the CDMX shapefile
default_crs <- st_crs(cdmx) # Save the CRS of the CDMX shapefile to the variable 'default_crs'

# Clean up the environment by removing the 'mun_shp' object, as it's no longer needed
rm(mun_shp) # Remove the 'mun_shp' object to free up memory

# CDB: Zócalo ----
# The following code defines the central business district (CBD) location as the Zócalo in Mexico City.
# See: https://maps.app.goo.gl/brrSCG5oPGhqiLGX6
# Create a simple feature point representing the Zócalo's coordinates
p <- st_point(c(-99.13322312217048, 19.43266591475427))
# The coordinates are in the format (longitude, latitude).

# Convert the point into a simple feature geometry collection (sfc) and then into a simple feature (sf) object
cbd <- 
  st_sfc(p) |>  # Create a simple feature geometry collection (sfc) from the point 'p'
  st_sf(        # Convert the simple feature geometry collection into an sf object
    crs = st_crs(default_crs) # Assign the coordinate reference system (CRS) from 'default_crs'
  )

# Pueblos de Indios (PI) by Tanck de Estrada ----
# This section processes the data on "Pueblos de Indios" (Indigenous towns) from the work of Tanck de Estrada.

# Define the path for the processed shapefile
processed_shapefile_path <- "data/derived/pueblos_cdmx/pueblos_cdmx.rds"

# Check if the processed shapefile already exists
if (file.exists(processed_shapefile_path)) {
  # Load the processed Pueblos de Indios data for CDMX
  pindios_cdmx <- read_rds(processed_shapefile_path)
  message("Processed Pueblos de Indios data loaded successfully.")
  
} else {
  # If the processed shapefile doesn't exist, process the raw data
  
  # Source Data ----
  pindios <- 
    read_dta("data/source/tanck/tanck_clean.dta") |> # Load the dataset from a Stata file
    janitor::clean_names() |> # Clean the column names to snake_case format
    mutate(
      longitud = -longitud, # Convert longitude values to negative (if required to align with Western Hemisphere)
      lon = longitud,       # Create a new column 'lon' for longitude
      lat = latitud,        # Create a new column 'lat' for latitude
      pueblo = str_to_sentence(pueblo),             # Convert the 'pueblo' column to sentence case
      santo = str_to_sentence(santo),               # Convert the 'santo' column to sentence case
      intendencia = str_to_sentence(intendencia),   # Convert the 'intendencia' column to sentence case
      subdelegacion = str_to_sentence(subdelegacion) # Convert the 'subdelegacion' column to sentence case
    ) |>
    filter(latitud < 40) # Remove an observation with an incorrectly captured latitude (filtering out extreme values)
  
  # Filter Pueblos within CDMX ----
  cdmx_bbox <- st_bbox(cdmx) # Get the bounding box of the CDMX polygon, which represents the spatial extent of Mexico City
  
  pindios_cdmx <- 
    pindios |> 
    filter(lon >= cdmx_bbox[1] & lon <= cdmx_bbox[3]) |> # Filter pueblos within the longitude range of CDMX
    filter(lat >= cdmx_bbox[2] & lat <= cdmx_bbox[4]) |> # Filter pueblos within the latitude range of CDMX
    mutate(
      # Convert 'indios' column to numeric after cleaning it
      indios = str_remove_all(indios, " indios"), # Remove the word 'indios' from the column
      indios = str_remove_all(indios, ","),       # Remove commas from the column
      indios = as.integer(indios)                 # Convert the cleaned 'indios' column to an integer
    )
  
  # Convert to SF (Simple Features) ----
  pindios_cdmx <- 
    pindios_cdmx |> 
    st_as_sf(coords = c("lon", "lat")) |>       # Convert the dataframe to a simple features object using 'lon' and 'lat' as coordinates
    st_set_crs(st_crs(default_crs))             # Set the coordinate reference system (CRS) to match the default CRS of CDMX
  
  # Filter again ----
  pindios_cdmx <- 
    pindios_cdmx |> 
    st_join(
      cdmx |> select(cve_mun, geometry) # Spatially join the CDMX geometry to the pueblos based on location
    ) |> 
    filter(!is.na(cve_mun)) # Keep only those pueblos that fall within a municipality in CDMX (i.e., where the join was successful)
  
  # Identify Pueblos with respect to distance to Zocalo ----
  pindios_cdmx <- 
    pindios_cdmx |> 
    mutate(
      # Calculate distance to Zocalo in meters
      dist_zocalo = units::drop_units(st_distance(geometry, cbd)[,1]), # Compute the distance from each pueblo to the Zócalo and drop units to keep it as numeric
    ) |> 
    arrange(dist_zocalo) |> # Arrange pueblos by their distance to the Zócalo
    mutate(
      n_pi_index = 1:nrow(pindios_cdmx) # Create an index column for pueblos based on their order after sorting by distance
    ) |> 
    glimpse() # Display the structure of the resulting data frame to the console for a quick inspection
  
  # Save the processed data to a shapefile ----
  pindios_cdmx |>
    write_rds(
      processed_shapefile_path, # Specify the output file path
    )
  message("Processed shapefile saved successfully.")
}

# CDMX Historical extension ----
list_files <- 
  list.files(
    path = "data/source/cdmx_historical_boundaries",
    recursive = TRUE,
    pattern = "\\.shp$",
    full.names = TRUE
  )

cdmx_historical_bound <- 
  st_read(list_files[1]) |> 
  st_transform(st_crs(default_crs)) |> 
  janitor::clean_names() |> 
  select(year, geometry)

for (f in list_files[-1]) {
  
  cdmx_historical_bound <- 
    cdmx_historical_bound |> 
    rbind(
      st_read(f) |> 
        st_transform(st_crs(default_crs)) |> 
        janitor::clean_names() |> 
        select(year, geometry) 
    )
}

cdmx_historical_bound

# ------------------------------------------------------------------------------
### Blocks ----
# 2019 Blocks shp ----
# This section processes the shapefile for city blocks in Mexico City (CDMX) for the year 2019.

# Define the path for the processed shapefile
processed_shapefile_path <- "data/derived/cdmx_blocks.shp"

# Check if the processed shapefile already exists
if (file.exists(processed_shapefile_path)) {
  # Load the pre-filtered CDMX blocks shapefile
  cdmx_blocks_shp <- st_read(processed_shapefile_path)
  message("Processed CDMX blocks shapefile loaded successfully.")
  
} else {
  # If the processed shapefile doesn't exist, load and process the original shapefile
  
  # Load the blocks shapefile for all regions
  blocks_shp <-
    st_read("data/source/shapefiles/mzmge19gw/mzmge19gw.shp") |> # Load the blocks shapefile
    janitor::clean_names() # Clean column names to snake_case
  
  # Filter blocks within CDMX and select relevant columns
  cdmx_blocks_shp <- 
    blocks_shp |> 
    filter(cve_ent == "09") |> # Filter only the blocks in CDMX using 'cve_ent' code "09"
    select(cvegeo:tipomza, area, perimeter, geometry) # Select relevant columns for further analysis
  
  # Save the filtered CDMX blocks shapefile for future use
  cdmx_blocks_shp |> 
    st_write(processed_shapefile_path, append = F) # Write the filtered CDMX blocks to a new shapefile
  message("Processed CDMX blocks shapefile saved successfully.")
}

# Add centroid and coordinates to the CDMX blocks data
cdmx_blocks_shp <- 
  cdmx_blocks_shp |> 
  mutate(
    block_centroid = st_centroid(geometry),        # Calculate the centroid for each block geometry
    block_lon = st_coordinates(block_centroid)[,1], # Extract the longitude of the centroid
    block_lat = st_coordinates(block_centroid)[,2], # Extract the latitude of the centroid
  )

#
# Load and clean the 2020 census data ----
# This section processes the 2020 census block-level data for CDMX.

# Define the path for the processed RDS file
processed_data_path <- "data/derived/cdmx_blocks.rds"

# Check if the processed RDS file already exists
if (file.exists(processed_data_path)) {
  # Load the processed CDMX blocks data from the RDS file
  cdmx_blocks <- read_rds(processed_data_path)
  message("Processed CDMX blocks data loaded successfully.")
  
} else {
  # If the processed data doesn't exist, load and process the raw census data
  
  # Load and clean the raw 2020 census data
  cdmx_blocks <-
    read_csv("data/source/censo/conjunto_de_datos/conjunto_de_datos_ageb_urbana_09_cpv2020.csv") |> # Load the 2020 census data for CDMX
    janitor::clean_names() |> # Clean column names to snake_case format
    # Filter out aggregated data, retaining only block-level observations
    filter(mun != "000") |> # Exclude aggregated data at the municipal level
    filter(loc != "0000") |> # Exclude aggregated data at the locality level
    filter(ageb != "0000") |> # Exclude aggregated data at the AGEB (basic geostatistical area) level
    filter(mza != "000") |> # Ensure only block-level observations are kept
    # Filter blocks with a population greater than 0 (optional)
    # filter(pobtot > 0) |>
    # Filter out blocks with fewer than 3 inhabited dwellings (due to privacy rules, these blocks lack socio-economic data)
    # filter(tvivhab > 2) |>
    # Convert all relevant variables to double type for consistency
    mutate_at(
      vars(pobfem:vph_sintic), # Convert socio-economic variables to double
      ~ as.double(.)
    ) |>
    # Rename geographic identifiers to more descriptive names and create a unique block ID
    rename(
      cve_ent = entidad, # State code
      cve_mun = mun,     # Municipality code
      cve_loc = loc,     # Locality code
      cve_ageb = ageb,   # AGEB code
      cve_mza = mza      # Block code
    ) |>
    mutate(
      cvegeo = str_c(cve_ent, cve_mun, cve_loc, cve_ageb, cve_mza) # Create a unique identifier for each block
    )
  
  # Join spatial feature by block ----
  # Join the census data with the spatial data of blocks in CDMX
  cdmx_blocks <- 
    cdmx_blocks |>
    left_join(
      cdmx_blocks_shp # Join with the shapefile data for CDMX blocks
    )
  
  # Distance to nearest Pueblo ----
  # Calculate the distance to the nearest Pueblo for each block and add relevant information
  cdmx_blocks <- 
    cdmx_blocks |>
    mutate(
      n_pi_index = st_nearest_feature(block_centroid, pindios_cdmx), # Identify the nearest Pueblo for each block
    ) |>
    glimpse() # Quick inspection of the resulting data frame
  
  # Join info of nearest Pueblo for each unit ----
  # This step merges the information of the nearest Pueblo with each block
  cdmx_blocks <- 
    cdmx_blocks |> 
    left_join(
      pindios_cdmx |> 
        tibble() |> 
        rename(
          pueblo_lon = longitud,   # Rename longitude column for Pueblo
          pueblo_lat = latitud,    # Rename latitude column for Pueblo
          pueblo_geom = geometry   # Rename geometry column for Pueblo
        )
    )
  
  # Calculate distance ----
  # Calculate the distance from each block to the nearest Pueblo and categorize the blocks
  cdmx_blocks <- 
    cdmx_blocks |> 
    mutate(
      dist_n_pi = st_distance(block_centroid, pueblo_geom, by_element = T), # Calculate distance to the nearest Pueblo
      dist_n_pi = units::drop_units(dist_n_pi), # Drop units to keep distance as numeric
      t2b1 = case_when(
        dist_n_pi <= 500 & solo_indigenas == 1 ~ "Indigenous - Treated",   # Block is within 500m and predominantly Indigenous
        dist_n_pi > 500 & solo_indigenas == 1 ~ "Indigenous - Control",    # Block is farther than 500m and predominantly Indigenous
        dist_n_pi <= 500 & solo_indigenas == 0 ~ "Mixed - Treated",        # Block is within 500m and mixed population
        dist_n_pi > 500 & solo_indigenas == 0 ~ "Mixed - Control",         # Block is farther than 500m and mixed population
      )
    ) |>
    glimpse() # Quick inspection of the categorized data
  
  # Distance to CBD ----
  # Calculate the distance from each block to the Central Business District (CBD, i.e., Zócalo)
  cdmx_blocks <- 
    cdmx_blocks |> 
    mutate(
      dist_cbd = st_distance(block_centroid, cbd)[,1], # Calculate distance to the Zócalo (CBD)
      dist_cbd = units::drop_units(dist_cbd), # Drop units to keep distance as numeric
    ) |>
    glimpse() # Quick inspection of the resulting data
  
  # Write RDS ----
  # Save the processed data to an RDS file for future use
  cdmx_blocks |> 
    write_rds(processed_data_path) # Save the data to a file
  message("Processed CDMX blocks data saved successfully.")
}
#
# Filter blocks within 1000 meters of a Pueblo ----
# Create a filtered dataset of blocks that are within 1000 meters of a Pueblo.
cdmx_blocks_rd <- cdmx_blocks |> 
  filter(dist_n_pi <= 1000) # Keep only blocks that are within 1000 meters of the nearest Pueblo

# ------------------------------------------------------------------------------
### Catastro ----
# Catastro Data Processing ----
# This section processes the catastro shapefile and related data for CDMX.

# Define the path for the processed RDS file
processed_data_path <- "data/derived/catastro_rd.rds"

# Check if the processed RDS file already exists
if (file.exists(processed_data_path)) {
  # Load the processed catastro data from the RDS file
  catastro <- read_rds(processed_data_path)
  message("Processed catastro data loaded successfully.")
  
} else {
  # If the processed data doesn't exist, load and process the raw catastro data
  
  # Load the Catastro shapefile ----
  catastro_shp <- read_sf("data/source/catastro/catastro.shp")
  
  # Load and process properties characteristics data ----
  catastro_data <-
    read_csv("data/source/catastro/catastro.csv") |>
    mutate(
      lvalor = log(valor_unitario_suelo), # Log-transform the land unit value
      uso_suelo_hab = case_when(
        uso_construccion == "Habitacional" ~ 1,
        TRUE ~ 0
      ),
      uso_suelo_hab_com = case_when(
        uso_construccion == "Habitacional y comercial" ~ 1,
        uso_construccion == "Comercial" ~ 1,
        TRUE ~ 0
      ),
      uso_suelo_otros = case_when(
        uso_construccion %in% c("Áreas Verdes", "Centro de Barrio", "Equipamiento", 
                                "Estacionamiento", "Industrial", "Sin Zonificación", 
                                "Usos Existentes") ~ 1,
        TRUE ~ 0
      ),
      niveles = case_when(
        clave_rango_nivel == "01" ~ 0,
        clave_rango_nivel == "02" ~ 2,
        clave_rango_nivel == "05" ~ 5,
        clave_rango_nivel == "10" ~ 10,
        clave_rango_nivel == "15" ~ 15,
        clave_rango_nivel == "20" ~ 20,
        TRUE ~ 20
      )
    )
  
  # Disable the use of spherical geometry (s2) to handle planar geometries
  sf_use_s2(FALSE)
  
  # Join the catastro shapefile with the properties data and calculate centroids ----
  catastro_rd <-
    catastro_shp |>
    left_join(catastro_data) |>
    filter(superficie_terreno > 0) |>
    mutate(
      predio_ctrd = st_centroid(geometry), # Calculate the centroid of each property
      predio_lon = st_coordinates(predio_ctrd)[,1], # Extract the longitude of the centroid
      predio_lat = st_coordinates(predio_ctrd)[,2]  # Extract the latitude of the centroid
    )
  
  # Distance to nearest Pueblo and CBD ----
  # Calculate the nearest Pueblo and CBD for each property and categorize them
  catastro_rd <-
    catastro_rd |>
    mutate(
      n_pi_index = st_nearest_feature(predio_ctrd, pindios_cdmx), # Identify the nearest Pueblo for each property
    ) |>
    left_join(
      pindios_cdmx |>
        tibble() |>
        rename(
          pueblo_lon = longitud,   # Rename longitude column for Pueblo
          pueblo_lat = latitud,    # Rename latitude column for Pueblo
          pueblo_geom = geometry   # Rename geometry column for Pueblo
        )
    ) |>
    mutate(
      # Distance to the nearest Pueblo
      dist_n_pi = st_distance(predio_ctrd, pueblo_geom, by_element = TRUE),
      dist_n_pi = units::drop_units(dist_n_pi),
      t2b1 = case_when(
        dist_n_pi <= 500 & solo_indigenas == 1 ~ "Indigenous - Treated",
        dist_n_pi > 500 & solo_indigenas == 1 ~ "Indigenous - Control",
        dist_n_pi <= 500 & solo_indigenas == 0 ~ "Mixed - Treated",
        dist_n_pi > 500 & solo_indigenas == 0 ~ "Mixed - Control",
      ),
      # Distance to the Central Business District (CBD)
      dist_cbd = st_distance(predio_ctrd, cbd)[,1],
      dist_cbd = units::drop_units(dist_cbd),
    ) |>
    glimpse()
  
  # Save the processed data to an RDS file ----
  catastro_rd |> 
    write_rds(processed_data_path)
  message("Processed catastro data saved successfully.")
  
  # Assign the processed data to the variable for further use
  catastro <- catastro_rd
}

# Further processing on the loaded or processed data ----
catastro <- catastro |>
  tibble() |>
  mutate(
    t2b1 = case_when(
      dist_n_pi <= cutoff & solo_indigenas == 1 ~ "Indigenous - Treated",
      dist_n_pi > cutoff & solo_indigenas == 1 ~ "Indigenous - Control",
      dist_n_pi <= cutoff & solo_indigenas == 0 ~ "Mixed - Treated",
      dist_n_pi > cutoff & solo_indigenas == 0 ~ "Mixed - Control",
    ),
    uso_suelo_verdes = case_when(
      uso_construccion == "Áreas Verdes" ~ 1,
      TRUE ~ 0
    ),
    uso_suelo_centrob = case_when(
      uso_construccion == "Centro de Barrio" ~ 1,
      TRUE ~ 0
    ),
    uso_suelo_equip = case_when(
      uso_construccion == "Equipamiento" ~ 1,
      TRUE ~ 0
    ),
    uso_suelo_estacion = case_when(
      uso_construccion == "Estacionamiento" ~ 1,
      TRUE ~ 0
    ),
    uso_suelo_industrial = case_when(
      uso_construccion == "Industrial" ~ 1,
      TRUE ~ 0
    ),
    uso_suelo_no_zoning = case_when(
      uso_construccion == "Sin Zonificación" ~ 1,
      TRUE ~ 0
    ),
    uso_suelo_usos_exist = case_when(
      uso_construccion == "Usos Existentes" ~ 1,
      TRUE ~ 0
    ),
  )

# Additional filtering or operations based on processed data ----
catastro_rd <- catastro |> filter(dist_n_pi <= 1000)

# ------------------------------------------------------------------------------
### Amenities ----
# Inventario Nacional de Vivienda (INV) ----
# This section processes the Inventario Nacional de Vivienda (INV) data for CDMX.

# Define the path for the processed RDS file
processed_data_path <- "data/derived/inv_rd.rds"

# Check if the processed RDS file already exists
if (file.exists(processed_data_path)) {
  # Load the processed INV data from the RDS file
  inv_rd <- read_rds(processed_data_path)
  message("Processed INV data loaded successfully.")
  
} else {
  # If the processed data doesn't exist, load and process the raw INV data
  
  # Load and process the raw Inventario Nacional de Vivienda (INV) data ----
  inv <- 
    read_rds("data/source/inv/cdmx_inventario_blocks_clean.rds") |> 
    tibble() |>
    mutate_at(
      vars(rampa_silla:d_comercio_abulante),
      funs(
        case_when(
          . == "1" ~ 1,
          . == "3" ~ 0,
          TRUE ~ NA_real_
        )
      )
    ) |>
    mutate(
      id = str_c(cvegeo, cve_segmento, cve_frente), # Create a unique ID for each entry
      pavimento = if_else(recubrimiento == "1", 1, 0) # Determine if the pavement is present
    )
  
  # Calculate centroids and extract coordinates ----
  inv_rd <- 
    inv |> 
    mutate(
      centroid = st_centroid(geometry), # Calculate the centroid of each geometry
      vial_lon = st_coordinates(centroid)[,1], # Extract the longitude of the centroid
      vial_lat = st_coordinates(centroid)[,2], # Extract the latitude of the centroid
    ) |>
    glimpse() # Quick inspection of the resulting data frame
  
  # Distance to nearest Pueblo ----
  # Calculate the nearest Pueblo for each unit
  inv_rd <- 
    inv_rd |> 
    mutate(
      n_pi_index = st_nearest_feature(centroid, pindios_cdmx), # Identify the nearest Pueblo for each unit
    ) |> 
    glimpse() # Quick inspection of the data
  
  # Join with Pueblo data and calculate distances ----
  inv_rd <- 
    inv_rd |> 
    left_join(
      pindios_cdmx |> 
        select(-cve_mun) |> # Remove 'cve_mun' column to avoid conflicts during join
        tibble() |> 
        rename(
          pueblo_lon = longitud, # Rename longitude column for Pueblo
          pueblo_lat = latitud,  # Rename latitude column for Pueblo
          pueblo_geom = geometry # Rename geometry column for Pueblo
        )
    ) |> 
    mutate(
      dist_n_pi = st_distance(centroid, pueblo_geom, by_element = TRUE), # Calculate the distance to the nearest Pueblo
      dist_n_pi = units::drop_units(dist_n_pi), # Drop units to keep distance as numeric
      t2b1 = case_when(
        dist_n_pi <= 500 & solo_indigenas == 1 ~ "Indigenous - Treated",   # Categorize based on distance and Indigenous status
        dist_n_pi > 500 & solo_indigenas == 1 ~ "Indigenous - Control",
        dist_n_pi <= 500 & solo_indigenas == 0 ~ "Mixed - Treated",
        dist_n_pi > 500 & solo_indigenas == 0 ~ "Mixed - Control",
      )
    ) |>
    glimpse() # Quick inspection of the categorized data
  
  # Save the processed data to an RDS file ----
  inv_rd |> 
    write_rds(processed_data_path)
  message("Processed INV data saved successfully.")
}

# ------------------------------------------------------------------------------
### Crime ----
# Carpetas de Investigación ADIP Processing ----
# This section processes the crime records data for CDMX.

# Define the path for the processed RDS file
processed_data_path <- "data/derived/crime_rd.rds"

# Check if the processed RDS file already exists
if (file.exists(processed_data_path)) {
  # Load the processed crime data from the RDS file
  crime <- read_rds(processed_data_path)
  message("Processed crime data loaded successfully.")
  
} else {
  # If the processed data doesn't exist, load and process the raw crime data
  
  # Load and process the raw crime data ----
  crime <-
    # Robbery
    read_rds("data/source/crime/robbery_records.rds") |>
    mutate(
      robbery = 1
    ) |>
    # Minor crimes
    bind_rows(
      read_rds("data/source/crime/minor_crime_records.rds") |>
        mutate(
          minor_crimes = 1
        )
    ) |>
    # Major crimes
    bind_rows(
      read_rds("data/source/crime/major_crime_records.rds") |>
        mutate(
          major_crimes = 1
        )
    ) |>
    # Homicides
    bind_rows(
      read_rds("data/source/crime/homicide_records.rds") |>
        mutate(
          homicide = 1
        )
    ) |>
    mutate(
      x = lon,
      y = lat
    ) |>
    st_as_sf(
      coords = c("x", "y"),
      crs = default_crs
    ) |>
    mutate_at(
      vars(robbery, minor_crimes, major_crimes, homicide),
      list(~replace_na(., 0)) # Replace NA with 0 for crime indicators
    ) |>
    mutate(
      day_event = weekdays(date_event) # Extract the day of the week from the date of the event
    ) |>
    glimpse() # Quick inspection of the resulting data frame
  
  # Filter crimes for the year 2019 ----
  crimes_rd <- crime |> filter(year == 2019)
  
  # Distance to nearest Pueblo ----
  # Calculate the nearest Pueblo for each crime event
  crimes_rd <- 
    crimes_rd |> 
    mutate(
      n_pi_index = st_nearest_feature(geometry, pindios_cdmx), # Identify the nearest Pueblo for each crime event
    ) |> 
    glimpse() # Quick inspection of the data
  
  # Join with Pueblo data and calculate distances ----
  crimes_rd <- 
    crimes_rd |> 
    left_join(
      pindios_cdmx |> 
        select(-cve_mun) |> # Remove 'cve_mun' column to avoid conflicts during join
        tibble() |> 
        rename(
          pueblo_lon = longitud, # Rename longitude column for Pueblo
          pueblo_lat = latitud,  # Rename latitude column for Pueblo
          pueblo_geom = geometry # Rename geometry column for Pueblo
        )
    ) |> 
    mutate(
      dist_n_pi = st_distance(geometry, pueblo_geom, by_element = TRUE), # Calculate the distance to the nearest Pueblo
      dist_n_pi = units::drop_units(dist_n_pi), # Drop units to keep distance as numeric
      t2b1 = case_when(
        dist_n_pi <= 500 & solo_indigenas == 1 ~ "Indigenous - Treated",   # Categorize based on distance and Indigenous status
        dist_n_pi > 500 & solo_indigenas == 1 ~ "Indigenous - Control",
        dist_n_pi <= 500 & solo_indigenas == 0 ~ "Mixed - Treated",
        dist_n_pi > 500 & solo_indigenas == 0 ~ "Mixed - Control",
      )
    ) |>
    glimpse() # Quick inspection of the categorized data
  
  # Save the processed data to an RDS file ----
  crimes_rd |> 
    write_rds(processed_data_path)
  message("Processed crime data saved successfully.")
  
  # Assign the processed data to the variable for further use
  crime <- crimes_rd
}


# ------------------------------------------------------------------------------
## Firms ----
# DENUE Data Processing ----
# This section processes the DENUE (National Directory of Economic Units) data for CDMX.

# Define the path for the processed RDS file
processed_data_path <- "data/derived/denue_rd.rds"

# Check if the processed RDS file already exists
if (file.exists(processed_data_path)) {
  # Load the processed DENUE data from the RDS file
  denue_rd <- read_rds(processed_data_path)
  message("Processed DENUE data loaded successfully.")
  
} else {
  # If the processed data doesn't exist, load and process the raw DENUE data
  
  # Load and process the raw DENUE data ----
  denue <-
    read_csv("data/source/denue/denue_09_csv/conjunto_de_datos/denue_inegi_09_.csv") |>
    mutate(
      scian2digit = str_sub(codigo_act, end = 2), # Extract the first 2 digits of the SCIAN code
      scian3digit = str_sub(codigo_act, end = 3), # Extract the first 3 digits of the SCIAN code
      type = case_when(
        scian2digit == "11" ~ "Agriculture",
        scian2digit == "22" ~ "Energy",
        scian2digit == "23" ~ "Construction",
        scian2digit %in% c("31", "32", "33") ~ "Manufacturing",
        scian2digit == "43" ~ "Wholesale",
        scian2digit == "46" ~ "Retail",
        scian2digit %in% c("48", "49") ~ "Transport",
        scian2digit == "51" ~ "Media",
        scian2digit == "52" ~ "Financial_services",
        scian2digit == "53" ~ "Real_state",
        scian2digit == "54" ~ "Professional_services",
        scian2digit == "55" ~ "Corporate",
        scian3digit == "561" ~ "Business_support_services",
        scian3digit == "562" ~ "Waste_management",
        scian2digit == "61" ~ "Educational_services",
        scian2digit == "61" ~ "Health_services",
        scian2digit == "72" ~ "Hotels_restaurants_bars",
        scian3digit == "811" ~ "Maintenance_services",
        scian3digit == "812" ~ "Personal_services",
        scian3digit == "813" ~ "Associations_organizations",
        scian2digit == "93" ~ "Government",
      ),
      microemp = case_when(
        per_ocu == "0 a 5 personas" ~ 1,
        per_ocu == "6 a 10 personas" ~ 1,
        TRUE ~ 0
      ),
      pequenaemp = case_when(
        per_ocu == "11 a 30 personas" ~ 1,
        per_ocu == "31 a 50 personas" ~ 1,
        TRUE ~ 0
      ),
      medianaemp = case_when(
        per_ocu == "31 a 50 personas" & type %in% c("Retail", "Wholesale") ~ 1,
        per_ocu == "51 a 100 personas" ~ 1,
        TRUE ~ 0
      ),
      grandeemp = case_when(
        per_ocu == "101 a 250 personas" ~ 1,
        per_ocu == "251 y más personas" ~ 1,
        TRUE ~ 0
      ),
      firm_size = case_when(
        microemp == 1 ~ "Micro (1-10 employees)",
        pequenaemp == 1 ~ "Small (11-51 employees)",
        medianaemp == 1 ~ "Median (51-100 employees)",
        grandeemp == 1 ~ "Big (+100 employees)",
      ),
      firm_size = fct_relevel(
        firm_size,
        "Micro (1-10 employees)",
        "Small (11-51 employees)",
        "Median (51-100 employees)",
        "Big (+100 employees)"
      ),
      fijo = case_when(
        tipoUniEco == "Fijo" ~ 1,
        TRUE ~ 0
      ),
      # Generate Block ID
      cve_ent = case_when(
        str_count(cve_ent) == 1 ~ str_c("0", cve_ent),
        TRUE ~ as.character(cve_ent)
      ),
      cve_mun = case_when(
        str_count(cve_mun) == 1 ~ str_c("00", cve_mun),
        str_count(cve_mun) == 2 ~ str_c("0", cve_mun),
        TRUE ~ as.character(cve_mun)
      ),
      cve_loc = case_when(
        str_count(cve_loc) == 1 ~ str_c("000", cve_loc),
        str_count(cve_loc) == 2 ~ str_c("00", cve_loc),
        str_count(cve_loc) == 3 ~ str_c("0", cve_loc),
        TRUE ~ as.character(cve_loc)
      ),
      ageb = case_when(
        str_count(ageb) == 2 ~ str_c("00", ageb),
        str_count(ageb) == 3 ~ str_c("0", ageb),
        TRUE ~ as.character(ageb)
      ),
      manzana = case_when(
        str_count(manzana) == 1 ~ str_c("00", manzana),
        str_count(manzana) == 2 ~ str_c("0", manzana),
        TRUE ~ as.character(manzana)
      ),
      cvegeo = str_c(cve_ent, cve_mun, cve_loc, ageb, manzana) # Concatenate to create Block ID
    ) |>
    fastDummies::dummy_cols(select_columns = "type") |>
    glimpse() # Quick inspection of the resulting data frame
  
  denue_rd <- denue
  
  # Convert to simple features (sf) ----
  denue_rd <-
    denue_rd |>
    mutate(
      x = longitud,
      y = latitud
    ) |>
    st_as_sf(
      coords = c("x", "y"),
      crs = default_crs
    )
  
  # Distance to nearest Pueblo ----
  # Calculate the nearest Pueblo for each unit
  denue_rd <- 
    denue_rd |> 
    mutate(
      n_pi_index = st_nearest_feature(geometry, pindios_cdmx), # Identify the nearest Pueblo for each unit
    ) |> 
    glimpse() # Quick inspection of the data
  
  # Join nearest Pueblo ----
  denue_rd <- 
    denue_rd |> 
    left_join(
      pindios_cdmx |> 
        select(-cve_mun) |> # Remove 'cve_mun' column to avoid conflicts during join
        tibble() |> 
        rename(
          pueblo_lon = longitud, # Rename longitude column for Pueblo
          pueblo_lat = latitud,  # Rename latitude column for Pueblo
          pueblo_geom = geometry # Rename geometry column for Pueblo
        )
    )
  
  # Calculate distance to nearest Pueblo and filter ----
  denue_rd <- 
    denue_rd |> 
    mutate(
      dist_n_pi = st_distance(geometry, pueblo_geom, by_element = TRUE), # Calculate the distance to the nearest Pueblo
      dist_n_pi = units::drop_units(dist_n_pi), # Drop units to keep distance as numeric
      t2b1 = case_when(
        dist_n_pi <= 500 & solo_indigenas == 1 ~ "Indigenous - Treated",   # Categorize based on distance and Indigenous status
        dist_n_pi > 500 & solo_indigenas == 1 ~ "Indigenous - Control",
        dist_n_pi <= 500 & solo_indigenas == 0 ~ "Mixed - Treated",
        dist_n_pi > 500 & solo_indigenas == 0 ~ "Mixed - Control",
      )
    ) |>
    filter(dist_n_pi <= 1000) |> # Keep only units within 1000 meters of the nearest Pueblo
    glimpse() # Quick inspection of the filtered data
  
  # Save the processed data to an RDS file ----
  denue_rd |> 
    write_rds(processed_data_path)
  message("Processed DENUE data saved successfully.")
  
  # Assign the processed data to the variable for further use
  denue_rd <- denue_rd
}


# ------------------------------------------------------------------------------
### School quality ----
# Read rds ----
cemabe <- 
  read_rds("data/source/cemabe/school_census.rds") |> 
  filter(cve_ent == "09") |> 
  left_join(
    cdmx_blocks |> 
      select(cvegeo, geometry, block_lon, block_lat, dist_n_pi, n_pi_index, solo_indigenas, t2b1)
  ) |> 
  mutate(
    public = if_else(public_private_string == "publico", 1, 0),
    secondary = if_else(school_level_string == "secundaria", 1, 0),
    primary = if_else(school_level_string == "primaria", 1, 0),
    preeschool = if_else(school_level_string == "preescolar", 1, 0),
    other = if_else(school_level_string %in% c("centros atencion multiple", "educacion especial"), 1, 0),
  )

cemabe_rd <- 
  cemabe |> 
  filter(dist_n_pi <= 2500)


# ------------------------------------------------------------------------------
### Durable investments: Churches ----
# Churches by INE Processing ----
# This section processes the churches data from the INE for CDMX.

# Define the path for the processed RDS file
processed_data_path <- "data/derived/iglesias_cdmx_block.rds"

# Check if the processed RDS file already exists
if (file.exists(processed_data_path)) {
  # Load the processed churches data from the RDS file
  iglesias_cdmx_block <- read_rds(processed_data_path)
  message("Processed churches data loaded successfully.")
  
} else {
  # If the processed data doesn't exist, load and process the raw churches data
  
  # Load and process the raw churches data ----
  iglesias <-
    st_read("data/source/ine2010_iglesias/ine2010_iglesias_gw.shp") |>
    janitor::clean_names() |>
    rename(iglesia_nombre = nombre) |>
    mutate(
      iglesia = 1, # Mark all entries as churches
      lon = st_coordinates(geometry)[,1], # Extract longitude from geometry
      lat = st_coordinates(geometry)[,2], # Extract latitude from geometry
      catolica = case_when(
        str_detect(iglesia_nombre, "BAUTISTA") ~ 0,
        str_detect(iglesia_nombre, "METODISTA") ~ 0,
        str_detect(iglesia_nombre, "CRISTIANA") ~ 0,
        str_detect(iglesia_nombre, "EVANGELICA") ~ 0,
        str_detect(iglesia_nombre, "PROTESTANTE") ~ 0,
        TRUE ~ 1
      ),
      parroquia = case_when(
        str_detect(iglesia_nombre, "PARROQUIA") ~ 1,
        TRUE ~ 0
      ),
      capilla = case_when(
        str_detect(iglesia_nombre, "CAPILLA") ~ 1,
        TRUE ~ 0
      ),
    )
  
  # Define bounding box for CDMX ----
  cdmx_bbox <- st_bbox(st_sf(cdmx_blocks))
  
  # Filter churches within CDMX based on bounding box ----
  iglesias_cdmx <-
    iglesias |>
    filter(lon >= cdmx_bbox[1] & lon <= cdmx_bbox[3]) |>
    filter(lat >= cdmx_bbox[2] & lat <= cdmx_bbox[4])
  
  # Spatially join with CDMX geometry to ensure the churches are within CDMX ----
  iglesias_cdmx <-
    iglesias_cdmx |>
    st_join(
      cdmx |> select(geometry) |> mutate(within_cdmx = 1)
    ) |>
    filter(!is.na(within_cdmx))
  
  # Assign an identification number to each church ----
  iglesias_cdmx <-
    iglesias_cdmx |>
    mutate(
      id = 1:nrow(iglesias_cdmx)
    )
  
  # Clean up the environment by removing the raw churches data
  rm(iglesias)
  
  # Churches by block ----
  # Join the churches data with the block data and handle NA values
  iglesias_cdmx_block <-
    cdmx_blocks |>
    st_sf() |>
    st_join(
      iglesias_cdmx
    ) |>
    mutate_at(
      vars(iglesia, catolica, parroquia, capilla),
      ~ replace_na(., 0) # Replace NA values with 0 for church-related indicators
    )
  
  # Save the processed data to an RDS file ----
  iglesias_cdmx_block |> 
    write_rds(processed_data_path)
  message("Processed churches data saved successfully.")
  
  # Assign the processed data to the variable for further use
  iglesias_cdmx_block <- iglesias_cdmx_block
}

# ------------------------------------------------------------------------------
### Roads ----
# Vialidades Data Processing ----
# This section processes the vialidades (roads and streets) data for CDMX.

# Define the path for the processed RDS file
processed_data_path <- "data/derived/vialidades_100m.rds"

# Check if the processed RDS file already exists
if (file.exists(processed_data_path)) {
  # Load the processed vialidades data from the RDS file
  vial_metros <- read_rds(processed_data_path) |> tibble()
  message("Processed vialidades data loaded successfully.")
  
} else {
  # If the processed data doesn't exist, load and process the raw vialidades data
  
  # Load and process the raw vialidades data ----
  # Load all vialidades
  vial <-
    st_read("data/source/lineas_ejes_de_vialidad/lineas_ejes_de_vialidad.shp") |>
    janitor::clean_names() |>
    st_transform(st_crs(3857)) # Transform to Web Mercator (EPSG:3857)
  
  # Load and process main vialidades ----
  vial_princ <-
    st_read("data/source/lineas_ejes_de_vialidad/vialidades_primarias_cdmx.shp") |>
    janitor::clean_names() |>
    st_transform(st_crs(3857)) |>
    mutate(
      vial_prin = 1 # Mark main vialidades
    ) |>
    select(vial_prin) # Keep only the main vialidades identifier
  
  # Join all vialidades with main vialidades ----
  vial <-
    vial |>
    st_join(vial_princ) |>
    distinct(cvegeo, cve_ent, cve_mun, cve_loc, cvevial, cveseg, .keep_all = TRUE) |>
    mutate(
      vial_prin = replace_na(vial_prin, 0) # Replace NA values in vial_prin with 0
    )
  
  # Clean up the environment by removing the main vialidades data
  rm(vial_princ)
  
  # Transform lines to 10-meter evenly spaced points ----
  vial_metros <-
    vial |>
    mutate(
      geometry = st_line_sample(geometry, density = units::as_units(10, "m")) # Sample points every 10 meters
    ) |>
    st_transform(st_crs(default_crs)) |> # Transform to the default CRS
    st_cast("POINT", group_or_split = TRUE) |>
    tibble() # Convert to tibble
  
  # Calculate distance to nearest Pueblo ----
  vial_metros <-
    vial_metros |>
    mutate(
      n_pi_index = st_nearest_feature(geometry, pindios_cdmx), # Identify the nearest Pueblo for each point
    ) |>
    left_join(
      pindios_cdmx |>
        tibble() |>
        rename(pueblo_geom = geometry) |>
        select(pueblo_geom, n_pi_index), # Join with nearest Pueblo geometry
      by = "n_pi_index"
    ) |>
    mutate(
      dist_n_pi = st_distance(geometry, pueblo_geom, by_element = TRUE), # Calculate the distance to the nearest Pueblo
      dist_n_pi = units::drop_units(dist_n_pi) # Drop units to keep distance as numeric
    )
  
  # Categorize vialidades and save the processed data ----
  vial_metros |>
    mutate(
      peatonal = case_when(
        sentido == "Peatonal" ~ 1,
        TRUE ~ 0
      ),
      calle = case_when(
        peatonal == 0 & tipovial %in% c("Andador", "Calle", "Callejón", "Calzada", "Cerrada", "Corredor", "Pasaje", "Peatonal", "Privada", "Prolongación") ~ 1,
        TRUE ~ 0
      ),
      avenida = case_when(
        peatonal == 0 & calle == 0 ~ 1,
        TRUE ~ 0
      )
    ) |>
    write_rds(processed_data_path)
  message("Processed vialidades data saved successfully.")
  
  # Assign the processed data to the variable for further use
  vial_metros <- vial_metros
}

# ------------------------------------------------------------------------------
### Rasters ----
# Altitude ----
altitude <-
  raster("data/source/rasters/cem/CiudadMexico_r15m.tif") |> 
  aggregate(fact = 10)

plot(altitude)

# Slope ----
slope <- terrain(altitude)

plot(slope)

# Temperature  ----

temperature <- 
  raster("data/source/rasters/Clima 1902-2011/temperatura_anual_1902-2011.tif") 

temperature <- 
  temperature |> 
  crop(cdmx |> 
         st_transform(st_crs(temperature))
  ) |> 
  projectRaster(crs = crs(cdmx))

plot(temperature)

# Precipitation  ----
precipitation <- 
  raster("data/source/rasters/Clima 1902-2011/precipitacion_anual_1902-2011.tif") 

precipitation <- 
  precipitation |> 
  crop(cdmx |> 
         st_transform(st_crs(precipitation))
  ) |> 
  projectRaster(crs = crs(cdmx))

plot(precipitation)


# Humidity ----

humidity <- 
  raster("data/source/rasters/Clima 1902-2011/indice_de_lang_1902-2011.tif") 

humidity <- 
  humidity |> 
  crop(cdmx |> 
         st_transform(st_crs(humidity))
  ) |> 
  projectRaster(crs = crs(cdmx))

plot(humidity)

# Thermal variation ----

termal_var <- 
  raster("data/source/rasters/Clima 1902-2011/oscilacion_termica_1902-2011.tif") 

termal_var <- 
  termal_var |> 
  crop(cdmx |> 
         st_transform(st_crs(termal_var))
  ) |> 
  projectRaster(crs = crs(cdmx))

plot(termal_var)


# Type of weather ----

clima <- 
  raster("data/source/rasters/Clima 1902-2011/clima_1902-2011.tif") 

clima <- 
  clima |> 
  crop(cdmx |> 
         st_transform(st_crs(clima))
  ) |> 
  projectRaster(crs = crs(cdmx))

plot(clima)

# Sismic zoning ----
# http://www.atlasnacionalderiesgos.gob.mx/apps/Sismo19sCDMX/

zonas_sism <-  
  st_read("data/source/rasters/zonas_sismicas/Zonificación Sísmica CDMX (GDF).kml") |>
  janitor::clean_names() |> 
  select(name, geometry) |> 
  rename(zona_sismica = name) |> 
  mutate(
    value = case_when(
      zona_sismica == "Zona III d" ~ 3.4,
      zona_sismica == "Zona III c" ~ 3.3,
      zona_sismica == "Zona III b" ~ 3.2,
      zona_sismica == "Zona III a" ~ 3.2,
      zona_sismica == "Zona II" ~ 2,
      zona_sismica == "Zona I" ~ 1,
      TRUE ~ NA_real_
    ),
    zona_sismica = str_replace_all(zona_sismica, " ", "_")
  ) |> 
  fastDummies::dummy_cols(
    select_columns = "zona_sismica"
  ) |> 
  st_sf()

zonas_sism_raster <- 
  zonas_sism |> 
  dplyr::select(value, geometry) |> 
  stars::st_rasterize()

plot(zonas_sism_raster)
