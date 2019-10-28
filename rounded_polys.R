library(tidyverse)
library(poissoned)
library(deldir)
library(mgcv)
library(polyclip)
library(rlist)

rand_pts <- data.frame(x = runif(50, 0, 300), y = runif(50, 0, 800))
tess <- deldir(rand_pts)
vor_list <- tile.list(tess) 

vor_list_small <-
  vor_list %>%
  keep( ~ .x$area < 8000) %>%
  keep( ~ .x$area > 3000)

cleanup <- function(x) x[ !names(x) %in% c("pt", "ptNum", "area", "id")]

vor_df <- 
  vor_list %>%
  map( ~cleanup(.x)) %>%
  bind_rows(.id = "id") %>%
  select(id, x, y) %>%
  mutate(id = as.numeric(id))

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
rand_voronois <- function(num_points = 50, x_max = 300, y_max = 800, num_shapes = 10) {
  rand_pts <- data.frame(x = runif(num_points, 0, x_max), y = runif(num_points, 0, y_max))
  tess <- deldir(rand_pts)
  vor_list <- tile.list(tess) 
  
  vor_list_sample <- list.sample(vor_list, num_shapes)
  
  cleanup <- function(x) x[ !names(x) %in% c("pt", "ptNum", "area", "id")]
  
  vor_list_sample %>%
    map( ~cleanup(.x)) %>%
    bind_rows(.id = "id") %>%
    select(id, x, y) %>%
    mutate(id = as.numeric(id))
}
# x_new <- split(vor_df$x, vor_df$id)
# y_new <- split(vor_df$y, vor_df$id)
# vor_polygons <- Map(list, x = x_new, y = y_new)
# 
# polygons2 <- lapply(vor_polygons, polyoffset, delta = -30,
#                     jointype = 'miter', miterlim = 1000)
# polygons3 <- lapply(polygons2, polyoffset, delta = 20,
#                     jointype = 'round')
# 
# expand_df <- 
#   polygons3 %>%
#   map(~as.data.frame(.x)) %>%
#   bind_rows(.id = "id")

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

expand_df <- shapify(vor_df, delta = -20, jointype = "miter", miterlim = 1000)
expand_df <- shapify(expand_df, delta = 10, jointype = "square")

ggplot(expand_df, aes(x = x, y = y)) +
  geom_polygon(aes(group = id))

rand_df <- rand_voronois()
ggplot(rand_df, aes(x = x, y = y)) +
  geom_polygon(aes(group = id))

salt <- poisson_disc(ncols = 200, nrows = 500, cell_size = 2, verbose = TRUE)
salt$x <- salt$x - 50
salt$y <- salt$y - 60

salt2 <- poisson_disc(ncols = 200, nrows = 500, cell_size = 2, verbose = TRUE)
salt2$x <- salt2$x - 50
salt2$y <- salt2$y - 60

salt3 <- poisson_disc(ncols = 200, nrows = 500, cell_size = 2, verbose = TRUE)
salt3$x <- salt3$x - 50
salt3$y <- salt3$y - 60

salt4 <- poisson_disc(ncols = 100, nrows = 250, cell_size = 4, verbose = TRUE)
salt4$x <- salt4$x - 50
salt4$y <- salt4$y - 60

poly_list <- 
  rand_df %>%
  select(x, y) %>%
  split(., rand_df$id)

textures <- 
  poly_list %>%
  map(., ~in.out(as.matrix(.x), as.matrix(salt))) %>%
  map(., ~cbind(.x, salt)) %>%
  map(., ~rename(.x, "inout" = ".x")) %>%
  map(., ~filter(.x, inout == TRUE)) %>%
  bind_rows(.id = "id") %>%
  select(-inout)


boundary <- 
  split(expand_df, expand_df$id) %>%
  map( ~add_row(.x, id = NA, x = NA, y = NA)) %>%
  bind_rows() %>%
  select(x, y)

salt_inout <- in.out(as.matrix(boundary), as.matrix(salt))
salt_inside <- cbind(salt, salt_inout) %>%
  filter(salt_inout == TRUE)
salt2_inout <- in.out(as.matrix(boundary), as.matrix(salt2))
salt2_inside <- cbind(salt2, salt2_inout) %>%
  filter(salt2_inout == TRUE)
salt3_inout <- in.out(as.matrix(boundary), as.matrix(salt3))
salt3_inside <- cbind(salt3, salt3_inout) %>%
  filter(salt3_inout == TRUE)
salt4_inout <- in.out(as.matrix(boundary), as.matrix(salt4))
salt4_inside <- cbind(salt4, salt4_inout) %>%
  filter(salt4_inout == TRUE)

ggplot() +
  #geom_path(data = expand_df, aes(x = x, y = y, group = id)) +
  geom_point(data = salt_inside, aes(x = x, y = y), size = 5, alpha = 0.7, shape = "/", color = "white") +
  geom_point(data = salt3_inside, aes(x = x, y = y), size = 5, alpha = 0.7, shape = "/", color = "#B20937") +
  geom_point(data = salt2_inside, aes(x = x, y = y), size = 5, alpha = 0.7, shape = "/", color = "#8B9DC3") +
  geom_point(data = salt4_inside, aes(x = x, y = y), size = 5, alpha = 0.7, shape = "/", color = "#761409") +
  theme_void() +
  theme(plot.background = element_rect(fill = "antiquewhite"))

ggsave("popart_rocks3.png", height = 12, width = 7)




