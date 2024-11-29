# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigB2"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Pueblos' catchment area intersections ----
buffers <- c(seq(0, 1000, 10))

pindios_buffers <- tibble()

for (b in buffers) {
  
  print(b)
  
  sf_use_s2(T)
  foo <- 
    pindios_cdmx |> 
    mutate(
      buffer = st_buffer(geometry, b)
    ) 
  
  matrix <- 
    st_intersects(foo$buffer, foo$buffer, sparse = F)
  
  
  pindios_buffers <- 
    pindios_buffers |> 
    bind_rows(
      tibble(
        intersects = rowSums(matrix)
      ) |> 
        mutate(
          intersects = intersects - 1,
          intersects = if_else(intersects > 0, 1, 0)
        ) |> 
        summarise(
          intersects = sum(intersects)/2
        ) |> 
        mutate(
          buffer = b
        )
    )
  
}

# Note that the total number of possible intersections
# is the combination between 2 pueblos for each of the 71 in the sample
# C(n, r) = n!/(r! * (n-r)!)
# Thus: C(71, 2) = 71! / (2! * 69!) = 71 * 70 * 69! / 2 * 69! = 71*35 = 2485

pindios_buffers <- 
  pindios_buffers |> 
  mutate(
    p_intersects = intersects/2485
  ) 

pindios_buffers |> 
  filter(buffer >= 420) |> 
  filter(buffer <= 500)

pindios_buffers |> 
  ggplot(aes(buffer, p_intersects)) +
  annotate(
    "rect", 
    xmin = 420, xmax = 500, ymin = -Inf, ymax = Inf, 
    fill = "grey", alpha = 0.3
  ) +
  geom_col(col="white", fill = tau, alpha=0.5, lwd = .5) +
  labs(
    x = "Buffer size",
    y = "Prob. of buffer intersections"
  ) +
  # scale_y_continuous(breaks = seq(0, 25, by = 2)) +
  scale_x_continuous(breaks = seq(0, 1500, by = 100)) +
  # Fuzzy cutoffs
  geom_vline(xintercept = cutoff - donut, linetype = "dotted") +
  geom_vline(xintercept = cutoff, lwd = .5, alpha = .5) +
  geom_vline(xintercept = cutoff + donut, linetype = "dotted") +
  # Annotate fuzzy cutoff
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  annotate(
    "text", label = "Fuzzy cutoff", alpha = .5,
    x = cutoff,
    y = Inf, vjust = -1, size = 2.5
  ) 

ggsave(
  str_c(
    output_dir, "/identification_pueblos_buffer_intersection.png"
  ),
  width = 7.5, height = 4.85
)

#