library(glyph)
library(ggplot2)

final_dat <- orbit(seed_probs = c(1, 0, 0, 0))

ggplot() +
  geom_polygon(data = final_dat[["seed"]], aes(x = x, y = y, group = id), fill = "white") +
  geom_path(data = final_dat[["seed_outlines"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["seed_outlines"]]$linetype, color = "white") +
  geom_path(data = final_dat[["pareto1"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["pareto1"]]$linetype, color = "white") +
  geom_path(data = final_dat[["pareto2"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["pareto2"]]$linetype, color = "white") +
  geom_path(data = final_dat[["orbits"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["orbits"]]$linetype, color = "white") +
  geom_polygon(data = final_dat[["planets"]], aes(x = x, y = y, group = id), fill = "white") +
  geom_path(data = final_dat[["planet_orbits"]], aes (x = x, y = y, group = id, size = linesize), color = "white", size = 0.13) +
  scale_size_identity() +
  scale_color_identity() +
  theme_void() +
  coord_equal() +
  theme(panel.background = element_rect(fill = "transparent"), plot.background = element_rect(color = NA, fill = "transparent"))

ggsave("output/glyph32.png", bg = "transparent")


final_dat <- summon(sec_shape_probs = c(1, 0), third_shape_probs = c(1, 0))

ggplot() +
  geom_polygon(data = final_dat[["seed"]], aes(x = x, y = y, group = id), fill = "white") +
  geom_path(data = final_dat[["seed_outlines"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["seed_outlines"]]$linetype, color = "white") +
  geom_path(data = final_dat[["orbits"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["orbits"]]$linetype, color = "white") +
  geom_path(data = final_dat[["inscribed"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["inscribed"]]$linetype, color = "white") +
  geom_path(data = final_dat[["sec"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["sec"]]$linetype, color = "white") +
  geom_path(data = final_dat[["third"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["third"]]$linetype, color = "white") +
  geom_point(data = final_dat[["inscribed_planets"]], aes(x = x, y = y, size = size, color = color)) +
  scale_size_identity() +
  scale_color_identity() +
  theme_void() +
  coord_equal() +
  theme(panel.background = element_rect(fill = "transparent"), plot.background = element_rect(color = NA, fill = "transparent"))

ggsave("output/glyph7.png", bg = "transparent")


final_dat <- orbit_glitch(glitch_type = "spike",
                          glitch_params =
                            list(num_glitches = 150, glitch_r_min = 0.9, glitch_r_max = 2.8,
                                 min_spikes = 20, max_spikes = 80, min_spikes2 = 30,
                                 max_spikes2 = 60, min_spike_jitter = -0.1, max_spike_jitter = 0.1))

ggplot() +
  geom_polygon(data = final_dat[["seed"]], aes(x = x, y = y, group = id), fill = "white") +
  geom_path(data = final_dat[["seed_outlines"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["seed_outlines"]]$linetype, color = "white") +
  geom_path(data = final_dat[["pareto1"]], aes(x = x, y = y, group = id, size = linewidth), linetype = final_dat[["pareto1"]]$linetype, color = "white") +
  geom_path(data = final_dat[["pareto2"]], aes(x = x, y = y, group = id, size = linewidth), linetype = final_dat[["pareto2"]]$linetype, color = "white") +
  geom_path(data = final_dat[["orbits"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["orbits"]]$linetype, color = "white") +
  scale_size_identity() +
  scale_color_identity() +
  theme_void() +
  coord_equal() +
  theme(panel.background = element_rect(fill = "transparent"), plot.background = element_rect(color = NA, fill = "transparent"))

ggsave("output/glyph25.png", bg = "transparent")
