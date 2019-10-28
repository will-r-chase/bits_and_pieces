library(tidyverse)
library(poissoned)
library(deldir)
library(mgcv)
library(polyclip)
library(rlist)
library(rlang)

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

medium_voronois <- function(num_points = 50, x_max = 300, y_max = 800, max_size = 8000, min_size = 3000) {
  rand_pts <- data.frame(x = runif(num_points, 0, x_max), y = runif(num_points, 0, y_max))
  tess <- deldir(rand_pts)
  vor_list <- tile.list(tess) 
  
  vor_list_small <-
    vor_list %>%
    keep( ~ .x$area < max_size) %>%
    keep( ~ .x$area > min_size)
  
  cleanup <- function(x) x[ !names(x) %in% c("pt", "ptNum", "area", "id")]
  
  vor_list_small %>%
    map( ~cleanup(.x)) %>%
    bind_rows(.id = "id") %>%
    select(id, x, y) %>%
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

pal1 <- colorRampPalette(colors = c("#daeed8", "#A4C990"))
pal2 <- colorRampPalette(colors = c("#FAEBD7", "#FF7373"))
pal3 <- colorRampPalette(colors = c("#B0E0E6", "#323232"))
pal6 <- colorRampPalette(colors = c("#efe1ff", "#bd3037"))

colorize <- function(data) {
  dir <- sample(c("x", "y", "-x", "-y"), 1)
  dir_parsed <- parse_expr(dir)
  pal <- sample(c("pal1", "pal2", "pal3", "pal6"), 1)
  pal_parsed <- parse_expr(pal)
  pal_expr <- expr(pal_parsed)
  len <- nrow(data)
  
  data %>% 
    arrange(!!dir_parsed) %>%
    mutate(color = eval(expr((!!pal)(len))))
}

salt <- poisson_disc(ncols = 800, nrows = 2000, cell_size = 0.5, verbose = TRUE)
salt$x <- salt$x - 50
salt$y <- salt$y - 60

shapes <- medium_voronois(max_size = 11000)

shapes2 <- shapify(shapes, delta = -30, jointype = "miter", miterlim = 1000)
shapes3 <- shapify(shapes2, delta = 20, jointype = "round")

textures <- texturize(shapes3, salt)

tex_list <- 
  split(textures, textures$id)

tex_colored <- 
  tex_list %>%
  map(., ~colorize(.x)) %>%
  bind_rows()

ggplot() +
  geom_point(data = tex_colored, aes(x = x, y = y, color = color), alpha = 0.8, size = 2) +
  scale_color_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = "#EEE0E5"))

