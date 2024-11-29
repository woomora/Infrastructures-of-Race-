# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/Fig1"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Figure 1: Pueblos + CDMX Historical extension ----

# X11(type = "cairo")

# Base map
sf_use_s2(F)
p <- 
  ggplot() +
  theme_minimal() +
  # Municipalities
  geom_sf(
    data = cdmx |> st_sf(),
    lwd = .5, alpha = .01, fill = NA
  ) +
  # Mexico city blocks
  geom_sf(
    data = cdmx_blocks |> st_sf(),
    lwd = .025, alpha = .5
  ) +
  # CBD (Zócalo)
  geom_sf(
    data = cbd, color = "black", shape = 9, size = 2
  ) +
  labs(color = "", shape = "", x = "", y = "") +
  theme(
    legend.position = "bottom"
  ) +
  ggspatial::annotation_scale()

p1 <- 
  p +
  # Mexico city historical boundaries
  geom_sf(
    data = cdmx_historical_bound |> arrange(rev(year)), 
    aes(fill = year, color = year), alpha = .3, lwd = .05
  ) +
  viridis::scale_fill_viridis(discrete = T, direction = 1, option = "C") +
  viridis::scale_color_viridis(discrete = T, direction = 1, option = "C") +
  theme_minimal() +
  labs(x = "", y = "", fill = "Urban expansion", color = "Urban expansion") +
  ggspatial::annotation_scale() +
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  # Pueblos
  geom_point(
    data = pindios_cdmx |> 
      mutate(
        type = if_else(solo_indigenas == 1, "Indigenous", "Mixed")
      ),
    aes(longitud, latitud, shape = type, color = type),
    size = 2, 
  ) +
  scale_color_manual(
    values = c(color_ind, color_mix)
  ) +
  labs(color = "Pueblo", shape = "Pueblo", x = "", y = "") +
  # # Pueblos Labels
  # geom_sf_text(
  #   data = pindios_cdmx,
  #   aes(label = n_pi_index), size = 2,
  #   nudge_x = -0.006, nudge_y = -0.006
  # ) +
  # CBD (Zócalo)
  geom_sf(
    data = cbd, color = "white", shape = 9, size = 2
  ) +
  # CBD Label
  ggsflabel::geom_sf_text_repel(
    data = cbd |> mutate(label = "Colonial CBD"),
    aes(label = label), size = 2.5,
    nudge_x = .15, nudge_y = +0.055
  ) +
  theme(
    legend.position = "right",
    plot.margin = unit(rep(-0.15, 4), "cm"),
    plot.background = element_rect(fill = "white", color = NA), # Set plot background to white
    panel.background = element_rect(fill = "white", color = NA), # Set panel background to white
    legend.background = element_rect(fill = "white", color = NA), # Set legend background to white
  )  +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )

p1

ggsave(
  str_c(
    output_dir, "/map_pueblos_urban_extension_1.png"
  ),
  width = 7.5, height = 7.5
)

# Figure A.1: Pueblos + CDMX Historical extension + Pueblos' labels ----
p1 <- 
  p +
  # Mexico city historical boundaries
  geom_sf(
    data = cdmx_historical_bound |> arrange(rev(year)), 
    aes(fill = year, color = year), alpha = .3, lwd = .05
  ) +
  viridis::scale_fill_viridis(discrete = T, direction = 1, option = "C") +
  viridis::scale_color_viridis(discrete = T, direction = 1, option = "C") +
  theme_minimal() +
  labs(x = "", y = "", fill = "Urban expansion", color = "Urban expansion") +
  ggspatial::annotation_scale() +
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  # Pueblos
  geom_point(
    data = pindios_cdmx |> 
      mutate(
        type = if_else(solo_indigenas == 1, "Indigenous", "Mixed")
      ),
    aes(longitud, latitud, shape = type, color = type),
    size = 2, 
  ) +
  scale_color_manual(
    values = c(color_ind, color_mix)
  ) +
  labs(color = "Pueblo", shape = "Pueblo", x = "", y = "") +
  # Pueblos Labels
  geom_sf_text(
    data = pindios_cdmx,
    aes(label = n_pi_index), size = 2,
    nudge_x = -0.006, nudge_y = -0.006
  ) +
  # CBD (Zócalo)
  geom_sf(
    data = cbd, color = "white", shape = 9, size = 2
  ) +
  # CBD Label
  ggsflabel::geom_sf_text_repel(
    data = cbd |> mutate(label = "Colonial CBD"),
    aes(label = label), size = 2.5,
    nudge_x = .15, nudge_y = +0.055
  ) +
  theme(
    legend.position = "left",
    plot.margin = unit(rep(-0.15, 4), "cm")
  )  +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )

foo <- 
  pindios_cdmx |> 
  mutate(
    legend_entry = str_c(n_pi_index, ". ", str_to_title(pueblo)),
    x_tile = as.character(ntile(n = 1)),
    y_tile = 1:nrow(pindios_cdmx)
    # y_tile = head(rep(c(1:((nrow(pindios_cdmx) + 1)/2)), 2), -1)
  ) 

p2 <- 
  ggplot() +
  geom_text(
    data = foo,
    aes(label=legend_entry, x=x_tile, y=-y_tile), size = 2, hjust = 0
  ) +
  theme_void() +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm")
    
  )

cowplot::plot_grid(
  NULL, p1, NULL, p2,
  rel_widths = c(-.05, 1, -.5, .66),
  align = "hv", nrow = 1
)

ggsave(
  str_c(
    output_dir, "/map_pueblos_urban_extension_2.png"
  ),
  width = 10, height = 6.15
)

#