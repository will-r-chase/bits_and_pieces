library(tidyverse)
library(poissoned)
library(deldir)
library(mgcv)
library(polyclip)
library(rlist)
library(rlang)
library(magrittr)


texturize <- function(polys, pts) {
  polys %>%
    select(x, y) %>%
    split(., polys$id) %>%
    map(., ~in.out(as.matrix(.x), as.matrix(pts))) %>%
    map(., ~cbind(.x, pts)) %>%
    map(., ~rename(.x, "inout" = ".x")) %>%
    map(., ~filter(.x, inout == TRUE)) %>%
    bind_rows(.id = "id") %>%
    select(-inout)
}

cleanup <- function(x) x[ !names(x) %in% c("pt", "ptNum", "area", "id")]
cleanup2 <- function(x) x[ !names(x) %in% c("x", "y", "ptNum", "area", "id", "bp")]

medium_voronois <- function(num_points = 50, x_max = 300, y_max = 800, max_size = 8000, min_size = 3000) {
  rand_pts <- data.frame(x = runif(num_points, 0, x_max), y = runif(num_points, 0, y_max))
  tess <- deldir(rand_pts)
  vor_list <- tile.list(tess) 
  
  vor_list_small <-
    vor_list %>%
    keep( ~ .x$area < max_size) %>%
    keep( ~ .x$area > min_size)
  
  vor_list_small %>%
    map( ~cleanup(.x)) %>%
    bind_rows(.id = "id") %>%
    select(id, x, y) %>%
    mutate(id = as.numeric(id))
}
rand_voronois <- function(num_points = 50, x_max = 300, y_max = 800, num_shapes = 10) {
  rand_pts <- data.frame(x = runif(num_points, 0, x_max), y = runif(num_points, 0, y_max))
  tess <- deldir(rand_pts)
  vor_list <- tile.list(tess) 
  
  vor_list_sample <- list.sample(vor_list, num_shapes)
  
  vor_list_sample %>%
    map( ~cleanup(.x)) %>%
    bind_rows(.id = "id") %>%
    select(id, x, y) %>%
    mutate(id = as.numeric(id))
}

voronize <- function(data) {
  tess <- deldir(as.data.frame(data))
  vor_list <- tile.list(tess) 
  
  vor_list %>%
    map( ~cleanup(.x)) %>%
    bind_rows(.id = "id") %>%
    select(id, x, y) %>%
    mutate(id = as.numeric(id))
}

get_seeds <- function(data) {
  tess <- deldir(as.data.frame(data))
  vor_list <- tile.list(tess) 
  
  vor_list %>%
    map( ~as.data.frame(t(unlist(cleanup2(.x))))) %>%
    bind_rows(.id = "id") %>%
    select(id, pt.x, pt.y) %>%
    mutate(id = as.numeric(id))
}

shapify <- function(data, delta, jointype, miterlim = 2) {
  x_new <- split(data$x, data$id)
  y_new <- split(data$y, data$id)
  polygons <- Map(list, x = x_new, y = y_new)
  
  polygons2 <- lapply(polygons, polyoffset, delta = delta,
                      jointype = jointype, miterlim = miterlim)
  
  polygons2 %>%
    map(~as.data.frame(.x)) %>%
    bind_rows(.id = "id")
}

pals <- list(
  pal1 = colorRampPalette(colors = c("#daeed8", "#A4C990")),
  pal2 = colorRampPalette(colors = c("#FAEBD7", "#FF7373")),
  pal3 = colorRampPalette(colors = c("#B0E0E6", "#323232")),
  pal6 = colorRampPalette(colors = c("#efe1ff", "#bd3037"))
)

colorize <- function(data) {
  dir <- sample(exprs(x, y, desc(x), desc(y)))[[1]]
  pal <- sample(pals)[[1]]
  
  data %>% 
    arrange(!!dir) %>%
    mutate(color = pal(nrow(data)))
}


##make some gradient colored pebbles
salt <- poisson_disc(ncols = 800, nrows = 2000, cell_size = 0.5, verbose = TRUE)
salt$x <- salt$x - 50
salt$y <- salt$y - 60

shapes <- medium_voronois(max_size = 11000)

shapes2 <- shapify(shapes, delta = -30, jointype = "miter", miterlim = 1000)
shapes3 <- shapify(shapes2, delta = 20, jointype = "round")

ggplot() +
  geom_polygon(data = shapes3, aes(x = x, y = y, group = id))

textures <- texturize(shapes3, salt)

tex_list <- 
  split(textures, textures$id)

tex_colored <- 
  tex_list %>%
  map(., ~colorize(.x)) %>%
  bind_rows()

ggplot() +
  #geom_polygon(data = shapes3, aes(x = x, y = y, group = id)) +
  geom_point(data = tex_colored, aes(x = x, y = y, color = color), alpha = 0.8, size = 2) +
  scale_color_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = "#EEE0E5"))

ggsave("gradient_rocks2.png", height = 11, width = 6)


#make some random layered pebbles
layer1 <- rand_voronois() %>%
  shapify(delta = -25, jointype = "miter", miterlim = 1000) %>%
  shapify(delta = 15, jointype = "round")
layer2 <- rand_voronois() %>%
  shapify(delta = -25, jointype = "miter", miterlim = 1000) %>%
  shapify(delta = 15, jointype = "round")
layer3 <- rand_voronois() %>%
  shapify(delta = -25, jointype = "miter", miterlim = 1000) %>%
  shapify(delta = 15, jointype = "round")

ggplot() +
  geom_polygon(data = layer1, aes(x = x, y = y, group = id), fill = "green", alpha = 0.5) +
  geom_polygon(data = layer2, aes(x = x, y = y, group = id), fill = "goldenrod", alpha = 0.5) +
  geom_polygon(data = layer3, aes(x = x, y = y, group = id), fill = "grey", alpha = 0.5) +
  theme_void()

##try a new layout
bad_circle <- function(num_pts = 10, min_r, max_r, jitter) {
  tibble(angle = seq(0, 2*pi, length.out = num_pts), r = sample(seq(min_r, max_r, length.out = 100), num_pts, replace = TRUE)) %>%
    mutate(x_jitter = sample(seq(-jitter, jitter, length.out = 100), num_pts, replace = TRUE), 
           y_jitter = sample(seq(-jitter, jitter, length.out = 100), num_pts, replace = TRUE),
           x = r*cos(angle) + x_jitter, 
           y = r*sin(angle) + y_jitter) %>%
    select(x, y) 
}

ggplot() +
  geom_point(data = bad_circle, aes(x = x, y = y)) +
  geom_path(data = bad_circle, aes(x = x, y = y, group = 1))

circle1 <- bad_circle(10, 50, 100, 100)
circle2 <- bad_circle(10, 200, 300, 100)
circle3 <- bad_circle(10, 400, 500, 100)

all_circles <- rbind(circle1, circle2, circle3)

circular_layer <- voronize(all_circles) %>%
  shapify(delta = -30, jointype = "miter", miterlim = 1000) %>%
  shapify(delta = 20, jointype = "round")

seeds <- get_seeds(all_circles)

probs <- c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)

pebbles1 <- 
  circular_layer %>%
  filter(id %in% sample(1:30, 9, prob = probs))

ggplot() +
  geom_polygon(data = pebbles1, aes(x = x, y = y, group = id)) +
  geom_text(data = seeds, aes(x = pt.x, y = pt.y, label = id), color = "white")

probs1 <- c(0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03)

circle_pebbles <- function(num_pts = 10, min_r1 = 50, max_r1 = 100, jitter_1 = 100, min_r2 = 200, max_r2 = 300, jitter_2 = 100,
                           min_r3 = 400, max_r3 = 500, jitter_3 = 100, expand = -30, round = 20, num_keepers = 8, probs = NULL) {
  circle1 <- bad_circle(num_pts, min_r1, max_r1, jitter_1)
  circle2 <- bad_circle(num_pts, min_r2, max_r2, jitter_2)
  circle3 <- bad_circle(num_pts, min_r3, max_r3, jitter_3)
  
  all_circles <- rbind(circle1, circle2, circle3)
  
  circular_layer <- voronize(all_circles) %>%
    shapify(delta = expand, jointype = "miter", miterlim = 1000) %>%
    shapify(delta = round, jointype = "round")
  
  keepers <- sample(1:30, num_keepers, prob = probs)
  
  seeds <- get_seeds(all_circles) %>%
    filter(id %in% keepers)
  
  pebbles <- 
    circular_layer %>%
    filter(id %in% keepers)
  
  list(seeds = seeds, pebbles = pebbles)
}

circ_layer1 <- circle_pebbles(probs = probs1, num_keepers = 3)
circ_layer2 <- circle_pebbles(probs = probs1, num_keepers = 3)
circ_layer3 <- circle_pebbles(probs = probs1, num_keepers = 3)
circ_layer4 <- circle_pebbles(probs = probs1, num_keepers = 2)
circ_layer5 <- circle_pebbles(probs = probs1, num_keepers = 2)

ggplot() +
  geom_segment(data = circ_layer1[["seeds"]], aes(x = pt.x, y = pt.y, xend = 0, yend = 0), color = "black", linetype = "dotted", alpha = 0.8) +
  geom_segment(data = circ_layer2[["seeds"]], aes(x = pt.x, y = pt.y, xend = 0, yend = 0), color = "black", linetype = "dotted", alpha = 0.8) +
  geom_segment(data = circ_layer3[["seeds"]], aes(x = pt.x, y = pt.y, xend = 0, yend = 0), color = "black", linetype = "dotted", alpha = 0.8) +
  geom_segment(data = circ_layer4[["seeds"]], aes(x = pt.x, y = pt.y, xend = 0, yend = 0), color = "black", linetype = "dotted", alpha = 0.8) +
  geom_segment(data = circ_layer5[["seeds"]], aes(x = pt.x, y = pt.y, xend = 0, yend = 0), color = "black", linetype = "dotted", alpha = 0.8) +
  geom_polygon(data = circ_layer1[["pebbles"]], aes(x = x, y = y, group = id), fill = "#10628E", alpha = 0.85) +
  geom_polygon(data = circ_layer2[["pebbles"]], aes(x = x, y = y, group = id), fill = "#D42A20", alpha = 0.85) +
  geom_polygon(data = circ_layer3[["pebbles"]], aes(x = x, y = y, group = id), fill = "#FAC12C", alpha = 0.85) +
  geom_polygon(data = circ_layer4[["pebbles"]], aes(x = x, y = y, group = id), fill = "black", alpha = 0.85) +
  geom_polygon(data = circ_layer5[["pebbles"]], aes(x = x, y = y, group = id), fill = "white", alpha = 0.85) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#FCF3D9"))
  #geom_text(data = circ_layer1[["seeds"]], aes(x = pt.x, y = pt.y, label = id), color = "white")

ggsave("byrne_pebbles_8.png", height = 8, width = 8)

