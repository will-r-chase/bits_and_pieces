library(tidyverse)
library(poissondisc)
library(gganimate)
library(deldir)
library(sp)
library(mgcv)
library(ggforce)

#sunrise
pts <- poisson_disc(300, 800, min_dist = 2, init = c(150, 750), k = 50)

pts2 <- 
  pts %>%
  mutate(row = row_number()) 

ggplot(pts2) +
  geom_point(aes(x = x, y = y, color = row, group = row), size = 4, alpha = 0.9) +
  scale_color_gradientn(colors = c("#F37374", "#F48181", "#F58D8D","#FF9999", "#FFA3A3","#FFA699", "#FFB399", "#FFB399","#FFC099", "#FFC099","#FFCC99", "#FFCC99"), guide = "none") +
  theme_void() 

ggsave("sunrise.png", width = 5, height = 10)

anim <- 
  ggplot(pts2) +
  geom_point(aes(x = x, y = y, color = row, group = row), size = 4, alpha = 0.9) +
  scale_color_gradientn(colors = c("#F37374", "#F48181", "#F58D8D","#FF9999", "#FFA3A3","#FFA699", "#FFB399", "#FFB399","#FFC099", "#FFC099","#FFCC99", "#FFCC99"), guide = "none") +
  theme_void()  +
  scale_y_reverse() +
  transition_reveal(along = row) +
  ease_aes("cubic-in") +
  enter_grow() +
  enter_fade(alpha = 0.9)

animate(anim, nframes = 100, fps = 20)
anim_save("sunrise.gif")

##tessellation
rand_pts <- data.frame(x = runif(50, 0, 300), y = runif(50, 0, 800))
tess <- deldir(rand_pts)
triang <- triang.list(tess)

triang_area <- function(data) {
  x <- data$x
  y <- data$y
  mat <- matrix(data = c(1,1,1,x[1],x[2],x[3],y[1],y[2],y[3]), nrow = 3, ncol = 3, byrow = TRUE)
  area <- 0.5*det(mat)
  return(area)
}

#add area to each triangle and do some reshuffling
triang %>%
  map( ~mutate(.x, area = triang_area(.x))) %>%
  bind_rows(.id = "id") %>%
  select(id, x, y, area) -> triang_df

big_triang <- 
  triang_df %>%
  group_by(id) %>%
  filter(area > 3000) %>%
  ungroup()

vor_seeds <- poisson_disc(300, 800, 12)

vor <- deldir(vor_seeds)
vor_list <- tile.list(vor)

cleanup <- function(x) x[ !names(x) %in% c("pt", "ptNum", "area", "id")]

vor_df <- 
  vor_list %>%
  map( ~cleanup(.x)) %>%
  bind_rows(.id = "id") %>%
  select(id, x, y)

gradient_fill <- poisson_disc(300, 800, 2, k = 10)
salt <- poisson_disc(300, 800, 2, k = 10)
pepper <- poisson_disc(300, 800, 2, k = 10)
salt_close <- poisson_disc(300, 800, 1, k = 10)
pepper_close <- poisson_disc(300, 800, 1, k = 10)

saveRDS(salt, "salt.rds")
saveRDS(salt_close, "salt_close.rds")
saveRDS(pepper, "pepper.rds")
saveRDS(pepper_close, "pepper_close.rds")

boundary <- 
  split(big_triang, big_triang$id) %>%
  map( ~add_row(.x, id = NA, x = NA, y = NA, area = NA)) %>%
  bind_rows() %>%
  select(x, y)

salt_inout <- in.out(as.matrix(boundary), as.matrix(salt))
salt_inside <- cbind(salt, salt_inout) %>%
  filter(salt_inout == TRUE)

pepper_inout <- in.out(as.matrix(boundary), as.matrix(pepper))
pepper_inside <- cbind(pepper, pepper_inout) %>%
  filter(pepper_inout == TRUE)

salt_close_inout <- in.out(as.matrix(boundary), as.matrix(salt_close))
salt_close_inside <- cbind(salt_close, salt_close_inout) %>%
  filter(salt_close_inout == TRUE)

pepper_close_inout <- in.out(as.matrix(boundary), as.matrix(pepper_close))
pepper_close_outside <- cbind(pepper_close, pepper_close_inout) %>%
  filter(pepper_close_inout == FALSE)

big_tri_list <- 
  split(big_triang, big_triang$id) %>%
  map( ~cleanup(.x)) 

fill_inside_list <- list()

for(i in 1:length(big_tri_list)) {
  temp_inout <- in.out(as.matrix(big_tri_list[[i]]), as.matrix(gradient_fill))
  fill_inside_list[[i]] <- cbind(gradient_fill, temp_inout) %>%
    filter(temp_inout == TRUE)
}

colors <- c("#E1BABA", "#FFDFE2", "#AAD8A8", "#8B9DC3", "#5C8492", "#B20937", "#E9FA77", "#D7EAAE", "#667788", 
            "#761409", "#FFDD4D", "#aebab7", "#a3a3a3")

big_triang <- 
  big_triang %>%
  group_by(id) %>%
  mutate(color = sample(colors, 1, replace = TRUE))

ggplot() +
  #geom_polygon(data = vor_df, aes(x = x, y = y, group = id), color = "black", fill = "NA", size = 0.3) +
  #geom_point(data = vor_seeds, aes(x = x, y = y), size = 0.3) +
  geom_polygon(data = big_triang, aes(x = x, y  = y, group = id, fill = color), color = "white", size = 1) +
  geom_point(data = salt_close_inside, aes(x = x, y = y), size = 0.001, color = "white", alpha = 0.15) +
  geom_point(data = pepper_inside, aes(x = x, y = y), size = 0.1, color = "black", alpha = 1, shape = "*") +
  geom_point(data = pepper_close_outside, aes(x = x, y = y), size = 0.05, color = "black", alpha = 1, shape = "*") +
  scale_fill_identity() +
  lims(x = c(0, 300), y = c(0, 800)) +
  theme_void()

ggsave("tri_test3.png", width = 5, height = 10)

salt <- readRDS("salt.rds")
salt_close <- readRDS("salt_close.rds")
pepper <- readRDS("pepper.RDS")
pepper_close <- readRDS("pepper_close.rds")

rand_pts <- data.frame(x = runif(50, 0, 300), y = runif(50, 0, 800))
tess <- deldir(rand_pts)
vor_list <- tile.list(tess) 

# vor_list_small <- 
#   vor_list %>%
#   keep( ~ .x$area < 8000) %>%
#   keep( ~ .x$area > 3000) 
# 
# plot(vor_list_small)

cleanup <- function(x) x[ !names(x) %in% c("pt", "ptNum", "area", "id")]

vor_df <- 
  vor_list %>%
  map( ~cleanup(.x)) %>%
  bind_rows(.id = "id") %>%
  select(id, x, y) %>%
  mutate(id = as.numeric(id))
  
centroids <- tile.centroids(vor_list) %>% mutate(id = 1:50) %>%rename(cen_x = x, cen_y = y)

plot <- 
  ggplot() +
  geom_shape(data = vor_df, aes(x = x, y  = y, group = id), color = "black", fill = "transparent", radius = unit(3, "mm"), expand = unit(-1, "mm"), size = 1) +
  #geom_text(data = centroids, aes(x = cen_x, y = cen_y, label = id)) +
  #geom_point(data = salt_inside, aes(x = x, y = y), size = 3, color = "black", alpha = 0.5, shape = "/") +
  #geom_point(data = pepper_inside, aes(x = x, y = y), size = 0.1, color = "black", alpha = 1, shape = "*") +
  #geom_point(data = pepper_close_outside, aes(x = x, y = y), size = 0.05, color = "black", alpha = 1, shape = "*") +
  scale_fill_identity() +
  lims(x = c(0, 300), y = c(0, 800)) +
  theme_void()

gg <- ggplot_build(plot)
layer <- layer_data(plot)
poly_8 <- layer %>% filter(group == 8)
ggplot(poly_8) +
  geom_polygon(aes(x = x, y = y, group = group))

boundary <- 
  split(vor_df, vor_df$id) %>%
  map( ~add_row(.x, id = NA, x = NA, y = NA)) %>%
  bind_rows() %>%
  select(x, y)

salt_inout <- in.out(as.matrix(boundary), as.matrix(salt))
salt_inside <- cbind(salt, salt_inout) %>%
  filter(salt_inout == TRUE)

pepper_inout <- in.out(as.matrix(boundary), as.matrix(pepper))
pepper_inside <- cbind(pepper, pepper_inout) %>%
  filter(pepper_inout == TRUE)

salt_close_inout <- in.out(as.matrix(boundary), as.matrix(salt_close))
salt_close_inside <- cbind(salt_close, salt_close_inout) %>%
  filter(salt_close_inout == TRUE)

pepper_close_inout <- in.out(as.matrix(boundary), as.matrix(pepper_close))
pepper_close_outside <- cbind(pepper_close, pepper_close_inout) %>%
  filter(pepper_close_inout == FALSE)

#randomly sample from voronoi polygons
ggplot() +
  geom_shape(data = vor_df2, aes(x = x, y  = y, group = id), color = "black", fill = "transparent", radius = unit(3, "mm"), expand = unit(-1, "mm"), size = 1) +
  geom_text(data = centroids, aes(x = cen_x, y = cen_y, label = id)) +
  #geom_point(data = salt_inside, aes(x = x, y = y), size = 3, color = "black", alpha = 0.5, shape = "/") +
  #geom_point(data = pepper_inside, aes(x = x, y = y), size = 0.1, color = "black", alpha = 1, shape = "*") +
  #geom_point(data = pepper_close_outside, aes(x = x, y = y), size = 0.05, color = "black", alpha = 1, shape = "*") +
  scale_fill_identity() +
  lims(x = c(0, 300), y = c(0, 800)) +
  theme_void()






