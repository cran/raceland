## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5,
  fig.width = 6
)

## ---- eval=FALSE--------------------------------------------------------------
#  # install required packages
#  pkgs = c(
#    "raceland",
#    "comat",
#    "rgdal",
#    "raster",
#    "sf",
#    "dplyr",
#    "RColorBrewer"
#  )
#  to_install = !pkgs %in% installed.packages()
#  if(any(to_install)) {
#    install.packages(pkgs[to_install])
#  }

## ---- warning=FALSE, message=FALSE, include=FALSE-----------------------------
library(raceland)
library(raster)
library(sf)
library(dplyr)

## -----------------------------------------------------------------------------
# reading input data
list_raster = list.files(system.file("rast_data", package = "raceland"),
                         full.names = TRUE)
race_raster = stack(list_raster)

# constructing racial landscape
real_raster = create_realizations(x = race_raster, n = 100)

# calculating local subpopulation densities
dens_raster = create_densities(real_raster, race_raster, window_size = 10)

## ---- echo=FALSE, out.width = '100%'------------------------------------------
knitr::include_graphics("fig3size_shift.png")

## ---- fig5, fig.align = "center", out.width = '40%'---------------------------
race_colors = c("#F16667", "#6EBE44", "#7E69AF", "#C77213", "#F8DF1D")
grid_sf = create_grid(real_raster, size = 20)
plot_realization(real_raster[[1]], race_raster, hex = race_colors)
plot(st_geometry(grid_sf), add = TRUE, lwd = 2)

## -----------------------------------------------------------------------------
metr_df_20 = calculate_metrics(x = real_raster, w = dens_raster, 
                               neighbourhood = 4, fun = "mean", 
                               size = 20, threshold = 0.5)
metr_df_20[metr_df_20$realization == 1, ]

## -----------------------------------------------------------------------------
smr = metr_df_20 %>%
  group_by(row, col) %>%
  summarize(
    ent_mean = mean(ent, na.rm = TRUE),
    ent_sd = sd(ent, na.rm = TRUE),
    mutinf_mean = mean(mutinf, na.rm = TRUE),
    mutinf_sd = sd(mutinf, na.rm = TRUE)
  )
smr

## -----------------------------------------------------------------------------
smr %>% 
  ungroup() %>% 
  select(-row, -col) %>% 
  summarise_all(mean)

## ---- echo=FALSE, out.width = '40%'-------------------------------------------
knitr::include_graphics("fig4_bivariate.png")

## -----------------------------------------------------------------------------
# n is a number of categories in racial landscape
bivariate_classification = function(entropy, mutual_information, n) {
  
  # calculate bivariate classification
  nent = log2(n)
  ent_cat = cut(entropy, breaks = c(0, 0.66, 1.33, nent), labels = c(1, 2, 3), 
                include.lowest = TRUE, right = TRUE)
  ent_cat = as.integer(as.character(ent_cat))
  
  mut_cat = cut(mutual_information, breaks = c(0, 0.33, 0.66, 1), labels = c(10, 20, 30), 
                include.lowest = TRUE, right = TRUE)
  mut_cat = as.integer(as.character(mut_cat))
  
  bivar_cls = mut_cat + ent_cat
  bivar_cls = as.factor(bivar_cls)
  
  return(bivar_cls)
}

## -----------------------------------------------------------------------------
smr$bivar_cls = bivariate_classification(entropy = smr$ent_mean, 
                                         mutual_information = smr$mutinf_mean,
                                         n = nlayers(race_raster))

## -----------------------------------------------------------------------------
# join IT-metric to the grid
attr_grid = dplyr::left_join(grid_sf, smr, by = c("row", "col"))

## -----------------------------------------------------------------------------
# calculate breaks parameter for plotting entropy and mutual information
# the values of entropy and mutual information are divided into equal breaks
ent_breaks = c(seq(0, 2, by = 0.25), log2(nlayers(race_raster)))
mut_breaks = seq(0, 1, by = 0.1)

## ---- warning=FALSE, message=FALSE, fig.align = "center"----------------------
plot(attr_grid["ent_mean"], breaks = ent_breaks, key.pos = 1, 
     pal = rev(RColorBrewer::brewer.pal(length(ent_breaks) - 1, name = "RdBu")),
     # pal = grDevices::hcl.colors(length(ent_breaks) - 1, palette = "Blue-Red"),
     bty = "n", main = "Racial diversity (Entropy)")

## ---- warning=FALSE, message=FALSE, fig.align = "center"----------------------
plot(attr_grid["mutinf_mean"], breaks = mut_breaks, key.pos = 1, 
     pal = rev(RColorBrewer::brewer.pal(length(mut_breaks) - 1, name = "RdBu")),
     # pal = grDevices::hcl.colors(length(mut_breaks) - 1, palette = "Blue-Red"),
     bty = "n", main = "Racial segregation (Mutual information)")

## ---- fig.align = "center"----------------------------------------------------
biv_colors = c("11" = "#e8e8e8", "12" = "#e4acac", "13" = "#c85a5a", "21" = "#b0d5df",
               "22" = "#ad9ea5", "23" = "#985356", "31" = "#64acbe", "32"= "#627f8c", 
               "33" = "#574249")
bcat = biv_colors[names(biv_colors)%in%unique(attr_grid$bivar_cls)]
plot(attr_grid["bivar_cls"], pal = bcat, main = "Racial diversity and residential segregation")

## -----------------------------------------------------------------------------
# calculate metrics for overlapping windows
metr_df_10 = calculate_metrics(x = real_raster, w = dens_raster, 
                               neighbourhood = 4, fun = "mean", 
                               size = 20, shift = 10, threshold = 0.5)

smr10 = metr_df_10 %>%
  group_by(row, col) %>%
  summarize(
    ent_mean = mean(ent, na.rm = TRUE),
    ent_sd = sd(ent, na.rm = TRUE),
    mutinf_mean = mean(mutinf, na.rm = TRUE),
    mutinf_sd = sd(mutinf, na.rm = TRUE)
  )

smr10 %>% 
  ungroup() %>% 
  select(-row, -col) %>% 
  summarise_all(mean)

# calculate bivariate classification
smr10$bivar_cls = bivariate_classification(
  entropy = smr10$ent_mean,
  mutual_information = smr10$mutinf_mean,
  n = nlayers(race_raster)
)

## -----------------------------------------------------------------------------
# create spatial grid object
grid_sf10 = create_grid(real_raster, size = 20, shift = 10)

# join IT-metrics to the grid
attr_grid10 = dplyr::left_join(grid_sf10, smr10, by = c("row", "col"))

## ---- warning=FALSE, message=FALSE, fig.align = "center"----------------------
plot(attr_grid10["ent_mean"], breaks = ent_breaks, key.pos = 1, 
     pal = rev(RColorBrewer::brewer.pal(length(ent_breaks) - 1, name = "RdBu")),
     # pal = grDevices::hcl.colors(length(ent_breaks) - 1, palette = "Blue-Red"),
     bty = "n", main = "Racial diversity (Entropy)")

## ---- warning=FALSE, message=FALSE, fig.align = "center"----------------------
plot(attr_grid10["mutinf_mean"], breaks = mut_breaks, key.pos = 1,
     pal = rev(RColorBrewer::brewer.pal(length(mut_breaks) - 1, name = "RdBu")),
     # pal = grDevices::hcl.colors(length(mut_breaks) - 1, palette = "Blue-Red"),
     bty = "n", main = "Racial segregation (Mutual information)")

## ---- fig.align = "center"----------------------------------------------------
# `biv_color`s defines a bivariate palette, 
# `bcat` selects only colors for categories available for analyzed areas
biv_colors = c("11" = "#e8e8e8", "12" = "#e4acac", "13" = "#c85a5a", "21" = "#b0d5df", 
               "22" = "#ad9ea5", "23" = "#985356", "31" = "#64acbe","32" = "#627f8c",
               "33" = "#574249")
bcat = biv_colors[names(biv_colors)%in%unique(attr_grid10$bivar_cls)]
plot(attr_grid10["bivar_cls"], pal = bcat, main = "Racial diversity and residential segregation")

