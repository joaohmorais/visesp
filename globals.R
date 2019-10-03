sp_cities <- readRDS("shapefile/simpl_sp.rds")
sp_cities <- sp_cities[,c(3, 4)]
sp_rras <- readRDS("shapefile/simpl_rras_cod.rds")
sp_rras <- sp_rras[,c(2, 1)]
sp_drs <- readRDS("shapefile/simpl_drs.rds")
sp_reg_saude <- readRDS("shapefile/simpl_reg_saude.rds")

positron_no_labels <- "//{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png"

s_tabnet_df_retrieval <- safely(tabnet_df_retrieval)
s_make_tabnet_obj <- safely(make_tabnet_obj)