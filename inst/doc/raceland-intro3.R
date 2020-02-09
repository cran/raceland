## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5,
  fig.width = 6
)

## ---- echo = FALSE------------------------------------------------------------
df = data.frame(size = c("60", "120", "240", "480", "ALL"),
                shift = c("30", "60", "120", "240", "ALL"),
                n = c(2245,584,145,37,1),
                ent = c(1.0468, 1.1331, 1.2951, 1.4403, 1.8393),
                ent_sd = c(0.0166, 0.0087, 0.0043, 0.0023, 0.0006),
                mutinf = c(0.0422, 0.0759, 0.1432, 0.2229, 0.4732),
                mutinf_sd = c(0.0051, 0.0036, 0.0027, 0.0019, 0.0009))
knitr::kable(df, caption = "Table 1: Entropy and mutual information for different spatial scales")

## ---- echo=FALSE, out.width = '40%', fig.cap="Figure 1: Racial landscape"-----
knitr::include_graphics("racial_landscape.png")

## ---- echo=FALSE, out.width = '100%', fig.cap="Figure 2: Racial diversity and segregation at different spatial scales (an example for the scale of 1.8 km)"----
knitr::include_graphics("div_seg.png")

## ---- eval=FALSE--------------------------------------------------------------
#  # R script calculates IT-derived metrices for different spatial scales.
#  
#  # INSTALL REQUIRED PACKAGES
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
#  
#  # REQUIRED R-PACKAGES
#  library(raceland)
#  library(raster)
#  library(sf)
#  library(dplyr)
#  library(RColorBrewer)
#  
#  # SET WORKING DIRECTORY
#  ## setwd("")
#  
#  ################################## USER DEFINED PARAMETERS #######################################
#  # Please define following parameters before running a script.
#  
#  # Path to race directory with downloaded data.
#  pf_to_data = "il_cook/race"
#  
#  # sfx indicates which dataset will be used. There are 3 options:
#  ## sfx="1990myc" - race specific grids for 1990 year.
#  ## sfx="2000myc" - race specific grids for 2000 year.
#  ## sfx="2010myc" - race specific grids for 2010 year.
#  
#  sfx = "2010myc"
#  
#  # Number of realizations (racial landscape) to generate.
#  # It is recommended to generate at least 30 realizations.
#  nrealization = 100
#  
#  # list with size and shift parameters: list(c(size, shift), c(size, shift),...).
#  # In this case calculation will be performed for 4 different spatia scale:
#  # 1. c(60,30) - size = 60 local ladnscape from 60x60 cells window will be calculated,
#  #    it corresponds to the spatial scale of 1.8km (60 cells x 30m).
#  # 2. c(120,60) - corresponds to the scale of 3.6 km.
#  # 3. c(240,120) - corresponds to the scale of 7.2 km.
#  # 4. c(480, 240) - corresponds to the scale of 14.4 km.
#  list_size_shift = list(c(60,30), c(120,60), c(240,120), c(480, 240))
#  
#  ####################################################################################################
#  
#  # FUNCTION TO CALCULATE RACIAL SEGREGATION/DIVERSITY CLASSIFICATION
#  bivariate_classification = function(entropy, mutual_information, n) {
#  
#    # max entropy is calculated as log2 from the number of race categories
#    nent = log2(n)
#  
#    # divide entropy values into 3 categories
#    ent_cat = cut(entropy, breaks = c(0, 0.66, 1.33, nent), labels = c(1, 2, 3),
#                  include.lowest = TRUE, right = TRUE)
#    ent_cat = as.integer(as.character(ent_cat))
#  
#    # divide mutual information values into 3 categories
#    mut_cat = cut(mutual_information, breaks = c(0, 0.33, 0.66, 1), labels = c(10, 20, 30),
#                  include.lowest = TRUE, right = TRUE)
#    mut_cat = as.integer(as.character(mut_cat))
#  
#    # combine categories of entropy (measure of racial diversity)
#    # and mutual information (measure of racial segregation)
#    bivar_cls = mut_cat + ent_cat
#    bivar_cls = as.factor(bivar_cls)
#  
#    return(bivar_cls)
#  }
#  
#  ####################################################################################################
#  
#  # COLORS USED FOR VISUALIZATION
#  ## They corresponds to 5 racial categories: ASIAN, BLACK, HISPANIC, OTHER, WHITE
#  race_colors = c("#F16667", "#6EBE44", "#7E69AF", "#C77213", "#F8DF1D")
#  
#  ## Bivariate palette to display the racial segregation/diversity classification
#  bivariate_class_colors = c("11" = "#e8e8e8", "12" = "#e4acac", "13" = "#c85a5a",
#                             "21" = "#b0d5df", "22" = "#ad9ea5", "23" = "#985356",
#                             "31" = "#64acbe", "32" = "#627f8c", "33" = "#574249")
#  
#  ####################################################################################################
#  
#  ################################## CREATE RESULTS DIRECTORY WITH SUBDIRECTORIES ####################
#  # results directory will be created as subdirectory in working directory
#  pf = getwd()
#  dir.create(file.path(pf, "results"), showWarnings = FALSE)
#  dir.create(file.path(pf, "results", "out_data"), showWarnings = FALSE)
#  dir.create(file.path(pf, "results", "out_metrics"), showWarnings = FALSE)
#  dir.create(file.path(pf, "results", "final"), showWarnings = FALSE)
#  dir.create(file.path(pf, "results", "shp"), showWarnings = FALSE)
#  
#  ###################################################################################################
#  
#  ################################## PREPROCESS RACE-SPECIFIC DATA ##################################
#  # Read data from GeoTIFFs to a RasterStack object
#  list_raster = list.files(pf_to_data, pattern = sfx, full.names = TRUE)
#  race_raster = stack(list_raster)
#  
#  # Rename raster layers
#  rnames = sapply(strsplit(names(race_raster), "_"), tail, 1)
#  rnames = substr(rnames, 1, nchar(rnames) - nchar(sfx))
#  
#  new_names = dplyr::recode(
#    rnames,
#    "hispanic" = "hispanic",
#    "nham" = "am",
#    "nhas" = "asian",
#    "nhb" = "black",
#    "nhother" = "other",
#    "nhpi" = "pi",
#    "nhw" = "white"
#  )
#  names(race_raster) = new_names
#  
#  # Reorder layers in RasterStack
#  race_raster = subset(race_raster, c("asian", "am", "black", "hispanic", "other", "pi", "white"))
#  
#  # Combine race-specific categories (ASIAN=ASIAN+PI, OTHER=OTHER+AM).
#  # Please notice that pi category does not exist for 1990myc dataset.
#  if (sfx == "1990myc") {
#    race_raster[["other"]] = race_raster[["other"]] + race_raster[["am"]]
#    race_raster = dropLayer(race_raster, c("am"))
#  } else {
#    race_raster[["other"]] = race_raster[["other"]] + race_raster[["am"]]
#    race_raster[["asian"]] = race_raster[["asian"]] + race_raster[["pi"]]
#    race_raster = dropLayer(race_raster, c("am", "pi"))
#  }
#  
#  # race raster object contains 5 layers:
#  # asian, black, hispanic, other, white with subpolulation densities
#  race_raster
#  
#  # save race_raster to a rds file
#  saveRDS(race_raster, file.path(pf, "results", "out_data", "race_raster.rds"))
#  
#  ###################################################################################################
#  
#  ################################## CONSTRUCTING RACIAL LANDSCAPE ##################################
#  real_raster = create_realizations(race_raster, n = nrealization)
#  
#  # save real_raster object
#  saveRDS(real_raster, file.path(pf, "results", "out_data", "real_rast.rds"))
#  
#  # plot racial landscape
#  png(file.path("results", "final", "racial_landscape.png"))
#  plot_realization(x = real_raster[[1]], y = race_raster, hex = race_colors)
#  dev.off()
#  ###################################################################################################
#  
#  ################################## CALCULATE RACE-SPECIFIC DENSTITIES #############################
#  dens_raster = create_densities(real_raster, race_raster, window_size = 10)
#  
#  # save dens_raster object
#  saveRDS(dens_raster, file.path(pf, "results", "out_data", "dens_rast.rds"))
#  ###################################################################################################
#  
#  ################################## CALCULATE METRICS FOR DIFFERENT SPATIAL SCALES ################
#  
#  complete_smr_df = data.frame()
#  for (i in list_size_shift) { #start size loop
#    size = i[1]
#    shift = i[2]
#  
#    #######################CALCULATE METRICS FOR SPECIFIED SIZE/SHIFT PARAMETER######################
#    metr_df = calculate_metrics(real_raster, dens_raster,
#                                neighbourhood = 4, fun = "mean",
#                                size = size, shift = shift, threshold = 0.5)
#  
#    # Summarize metrics
#    ## Number of motifels at given spatial scale
#    sel = metr_df[!is.na(metr_df$ent), ]
#    nmotif = mean(table(sel$realization))
#  
#    # Metrics summary
#    smr = metr_df %>%
#      group_by(row, col) %>%
#      summarize(
#        ent_mean = mean(ent, na.rm = TRUE),
#        ent_sd = sd(ent, na.rm = TRUE),
#        mutinf_mean = mean(mutinf, na.rm = TRUE),
#        mutinf_sd = sd(mutinf, na.rm = TRUE)
#      )
#  
#    smr = as.data.frame(smr)
#  
#    # Calculate an ensemble average for entropy and mutual information
#    complete_smr = c(
#      size = size,
#      shift = shift,
#      n = nmotif,
#      ent = mean(smr$ent_mean, na.rm = TRUE),
#      ent_sd = mean(smr$ent_sd, na.rm = TRUE),
#      mutinf = mean(smr$mutinf_mean, na.rm = TRUE),
#      mutinf_sd = mean(smr$mutinf_sd, na.rm = TRUE)
#    )
#    complete_smr_df = rbind(complete_smr_df, complete_smr)
#  
#    # Calculate racial segregation/diversity classification
#    smr$bivar_cls = bivariate_classification(entropy = smr$ent_mean,
#                                             mutual_information = smr$mutinf_mean,
#                                             n = nlayers(race_raster))
#  
#    # Save the metrics to csv
#    write.csv(metr_df,
#              file.path("results", "out_metrics",
#                        paste("metr_df_", size, "_", shift, ".csv", sep = "")),
#              row.names = FALSE)
#    write.csv(smr,
#              file.path("results", "out_metrics",
#                        paste("smr_metr_df_", size, "_", shift, ".csv", sep = "")),
#              row.names = FALSE)
#  
#  
#    #######################CREATE SPATIAL OBJECT WITG METRICES######################
#  
#    # Create spatial object
#    grid_sf = create_grid(real_raster, size = size, shift = shift)
#  
#    # Join metrics to the grid
#    attr_grid = dplyr::left_join(grid_sf, smr, by = c("row", "col"))
#    sel_grid = attr_grid[!is.na(attr_grid$ent_mean),]
#  
#    # Save the grid as shapefile
#    st_write(attr_grid,
#             file.path("results", "shp",
#                       paste("metr_stat_", size, "_", shift, ".shp", sep = "")))
#  
#    #######################VISUALIZATION#########################################
#  
#    # Divide entropy values into 10 class
#    ent_breaks = c(seq(0, 2, by = 0.25), log2(nlayers(race_raster)))
#  
#    # Divide mutual information into 10 class
#    mut_breaks = seq(0, 1, by = 0.1)
#  
#    # Spatial scale in km
#    scale_km = (shift * res(race_raster)[1]) / 1000
#  
#    # Mapping racial diversity (save plot to .png)
#    png(file.path("results", "final", paste("diversity_", size, "_", shift, ".png", sep = "")))
#    plot(sel_grid["ent_mean"], breaks = ent_breaks, key.pos = 1,
#         pal = rev(brewer.pal(length(ent_breaks) - 1, name = "RdBu")), bty = "n",
#         main = paste("Racial diversity (Entropy) at the scale of ",
#                      scale_km, " km", sep = ""))
#    dev.off()
#  
#    # Mapping racial segregation (save plot to .png)
#    png(file.path("results", "final", paste("segregation_", size, "_", shift, ".png", sep = "")))
#    plot(sel_grid["mutinf_mean"], breaks = mut_breaks, key.pos = 1,
#         pal = rev(brewer.pal(length(mut_breaks) - 1, name = "RdBu")), bty = "n",
#         main = paste("Racial segregation (Mutual information) at the scale of ",
#                      scale_km, " km", sep = ""))
#    dev.off()
#  
#    # Mapping racial segregation/diversity (save plot to .png)
#    png(file.path("results", "final", paste("bivar_", size, "_", shift, ".png", sep = "")))
#    bcat = bivariate_class_colors[names(bivariate_class_colors)%in%unique(sel_grid$bivar_cls)]
#    plot(sel_grid["bivar_cls"],
#         pal = bcat,
#         main = paste("Racial diversity/segregation classification at the scale of ",
#                       scale_km, " km", sep = ""))
#    dev.off()
#  } #stop size loop
#  
#  
#  ###################################################################################################
#  
#  ################################## CALCULATE METRICS FOR THE WHOLE RACIAL LANDSCAPE ###############
#  
#  metr = calculate_metrics(real_raster, dens_raster,
#                           neighbourhood = 4, fun = "mean",
#                           size = NULL, threshold = 1)
#  complete_smr_all = c(
#    size = "ALL",
#    shift = "ALL",
#    n = 1,
#    ent = mean(metr$ent, na.rm = TRUE),
#    ent_sd = sd(metr$ent, na.rm = TRUE),
#    mutinf = mean(metr$mutinf, na.rm = TRUE),
#    mutinf_sd = sd(metr$mutinf, na.rm = TRUE)
#  )
#  complete_smr_df = rbind(complete_smr_df, complete_smr_all)
#  colnames(complete_smr_df) = c("size", "shift", "n", "ent", "ent_sd", "mutinf", "mutinf_sd")
#  
#  # Write table with metrices for different spatial scales and for the whole area.
#  write.csv(complete_smr_df,
#            file.path("results", "final", "complete_smr.csv"),
#            row.names = FALSE)
#  
#  ###################################################################################################
#  

