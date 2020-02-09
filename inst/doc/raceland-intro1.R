## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5,
  fig.width = 6
)

## ---- echo=FALSE, out.width=800-----------------------------------------------
knitr::include_graphics("fig0framework.png")

## ---- eval=FALSE--------------------------------------------------------------
#  # install required packages
#  pkgs = c(
#    "raceland",
#    "comat",
#    "rgdal",
#    "raster",
#    "sf",
#    "dplyr"
#  )
#  to_install = !pkgs %in% installed.packages()
#  if(any(to_install)) {
#    install.packages(pkgs[to_install])
#  }

## ---- warning=FALSE, message=FALSE, include=FALSE-----------------------------
# attach required packages
library(raceland)
library(raster)
library(sf)
library(dplyr)

## -----------------------------------------------------------------------------
list_raster = list.files(system.file("rast_data", package = "raceland"),
                         full.names = TRUE)

## -----------------------------------------------------------------------------
race_raster = stack(list_raster)
race_raster

## ----fig1, fig.align = "center", out.width = '80%'----------------------------
plot(race_raster)

## -----------------------------------------------------------------------------
pf_to_data = system.file("vect_data/block_data.gpkg", package = "raceland")

## ---- warning=FALSE, message=FALSE--------------------------------------------
vect_data = st_read(pf_to_data)

## -----------------------------------------------------------------------------
names(vect_data)

## ---- warning=FALSE, message=FALSE--------------------------------------------
race_raster_from_vect = zones_to_raster(v = vect_data,
                                        resolution = 30, 
                                        variables = c("ASIAN", "BLACK", "HISPANIC", "OTHER", "WHITE"))

## -----------------------------------------------------------------------------
# generate 100 realizations based on race_raster object
real_raster = create_realizations(x = race_raster, n = 100)

## ---- fig2, fig.align = "center", out.width = '100%'--------------------------
# plot five first realizations
plot(real_raster[[1:5]], col = c("#F16667", "#6EBE44", "#7E69AF", "#C77213", "#F8DF1D"))

## ---- fig3, fig.align = "center", out.width = '40%'---------------------------
# In race_colors first color corresponds to asian, second to black,
# third to hispanics, fourth to other and fifth to white)
race_colors = c("#F16667", "#6EBE44", "#7E69AF", "#C77213", "#F8DF1D")
plot_realization(x = real_raster[[1]], y = race_raster, hex = race_colors)

## ---- echo=FALSE, out.width = '100%'------------------------------------------
knitr::include_graphics("fig1adjacencies.png")

## ---- echo=FALSE, out.width = '100%'------------------------------------------
knitr::include_graphics("fig2matrix.png")

## -----------------------------------------------------------------------------
dens_raster = create_densities(real_raster, race_raster, window_size = 10)

## -----------------------------------------------------------------------------
exposure_mat = comat::get_wecoma(x = as.matrix(real_raster[[1]]), 
                                 w = as.matrix(dens_raster[[1]]))
colnames(exposure_mat) = c("ASIAN", "BLACK", "HISPANIC", "OTHER", "WHITE")
rownames(exposure_mat) = c("ASIAN", "BLACK", "HISPANIC", "OTHER", "WHITE")
round(exposure_mat, 2)

## -----------------------------------------------------------------------------
metr_df = calculate_metrics(x = real_raster, w = dens_raster, 
                            neighbourhood = 4, fun = "mean", 
                            size = NULL, threshold = 1)

## -----------------------------------------------------------------------------
head(metr_df)

## -----------------------------------------------------------------------------
summary(metr_df[, c("ent", "mutinf")])

## -----------------------------------------------------------------------------
metr_df %>% 
  summarise(mean_ent = mean(ent, na.rm = TRUE),
  sd_ent = sd(ent, na.rm = TRUE),
  mean_mutinf = mean(mutinf),
  sd_mutinf = sd(mutinf))

