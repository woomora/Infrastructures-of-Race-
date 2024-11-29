# ------------------------------------------------------------------------------
# Ensure the output directory exists
# ------------------------------------------------------------------------------
output_dir <- "results/FigB3"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

## Treatment vs Control ----
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

#
# Density test ----

rdd <- rddensity(X = catastro_rd$dist_n_pi, c = cutoff)
summary(rdd)
rdd_plot <- rdplotdensity(rdd,  catastro_rd$dist_n_pi)

rdplot <- rdd_plot

rdplotLeft <- as.data.frame(rdplot$Estl$Estimate)
rdplotLeft$cil <- rdplotLeft$f_q - qnorm(0.975) * rdplotLeft$se_q
rdplotLeft$ciu <- rdplotLeft$f_q + qnorm(0.975) * rdplotLeft$se_q
# CI: above the cutoff
rdplotRight <- as.data.frame(rdplot$Estr$Estimate)
rdplotRight$cil <- rdplotRight$f_q - qnorm(0.975) * rdplotRight$se_q
rdplotRight$ciu <- rdplotRight$f_q + qnorm(0.975) * rdplotRight$se_q
# histogram
NLeft <- sum(catastro_rd$dist_n_pi >=min(rdplotLeft$grid) & catastro_rd$dist_n_pi<cutoff)
histNLeft <- ceiling(min(sqrt(NLeft), 10 * log(NLeft)/log(10)))

NRight <- sum(catastro_rd$dist_n_pi<=max(rdplotRight$grid) & catastro_rd$dist_n_pi>=cutoff)
histNRight <- ceiling(min(sqrt(NRight), 10 * log(NRight)/log(10)))

histBreaks <- c(seq(min(rdplotLeft$grid), cutoff, length.out = histNRight+1), seq(cutoff, max(rdplotRight$grid), length.out = histNRight+1)[2:(histNRight+1)])
histScale <- mean(catastro_rd$dist_n_pi>=min(rdplotLeft$grid) & catastro_rd$dist_n_pi<=max(rdplotRight$grid))

### Call ggplot()
ggplot() + theme_bw() + 
  geom_histogram(
    data=as.data.frame(catastro_rd$dist_n_pi), 
    aes(x=catastro_rd$dist_n_pi, y=..density..*histScale), 
    breaks=histBreaks, col="white", fill = tau, alpha=0.2
  ) +
  geom_ribbon(data=rdplotLeft , aes(x=grid, ymin=cil, ymax=ciu), lwd=0.5, fill="grey", alpha = .5) + 
  geom_ribbon(data=rdplotRight, aes(x=grid, ymin=cil, ymax=ciu), lwd=0.5, fill="grey", alpha = .5) + 
  geom_line(data=rdplotLeft , aes(x=grid, y=f_p), col="black") +
  geom_line(data=rdplotRight, aes(x=grid, y=f_p), col="black") + 
  geom_vline(xintercept = cutoff) +
  theme_clean +
  labs(
    x = "Dist. to fuzzy cutoff (m)",
    y = "Density"
  ) +
  scale_x_continuous(
    breaks = seq(cutoff-350, cutoff+350, 25),
    labels = seq(-350, 350, 25)
  ) 

ggsave(
  str_c(output_dir, "/density_test.png"),
  width = 7.5, height = 4.85
)

#