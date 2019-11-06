library(purrr)
library(rtabnetsp)

#sp_cities <- readRDS("shapefile/simpl_sp.rds")
sp_cities_md <- readRDS("shapefile/ibge_md.rds")
#sp_cities_lg <- readRDS("shapefile/ibge_lg.rds")
#ibge <- readRDS("shapefile/ibge.rds")
sp_rras <- readRDS("shapefile/simpl_rras.rds")
sp_drs <- readRDS("shapefile/simpl_drs.rds")
sp_reg_saude <- readRDS("shapefile/simpl_reg_saude.rds")

regionalizacao <- read.csv("csv/regionalizacao.csv")
drs_list <- unique(regionalizacao[,c(4, 5)])
drs_list <- drs_list[order(drs_list$cod_drs),]
rras_list <- unique(regionalizacao[,c(6, 7)])
rras_list <- rras_list[order(rras_list$cod_rras),]
reg_saude_list <- unique(regionalizacao[,c(8, 9)])
reg_saude_list <- reg_saude_list[order(reg_saude_list$cod_reg_saude),]

positron_no_labels <- "//{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png"

s_tabnet_df_retrieval <- safely(tabnet_df_retrieval)
s_make_tabnet_obj <- safely(make_tabnet_obj)
s_bins <- safely(binr::bins)

#color schemes
scheme_1 <- NULL
scheme_1$primary <- c("#FFB000", "#FFCF63", "#FFC239", "#C58800", "#9B6B00")
scheme_1$sec_1 <- c("#4013AF", "#7759C3", "#5B37B4", "#310D87", "#25086A")
scheme_1$sec_2 <- c("#04859D", "#49A4B6", "#278FA3", "#036779", "#02515F")

scheme_2 <- NULL
scheme_2$primary <- c("#AA8739", "#FFE5AA", "#D4B46A", "#805F15", "#553B00")
scheme_2$sec_1 <- c("#432F75", "#8A7BAF", "#635192", "#291657", "#15063A")
scheme_2$sec_2 <- c("#255E69", "#6A959D", "#437983", "#0F444F", "#012C34")


gradient_sec_2 <- c("#a6f1ff", "#7ac9d7", "#51a2b2", "#297c8a", "#005767")

gradient_primary <- rev(c("#FFB000", "#ffb612", "#ffba20", "#ffbf31", "#ffc441", "#ffc951", "#ffcf63"))
gradient_primary_2 <- c("#FFEABA", "#ffc648", "#FFB000", "#C58800")

num_intervals_bins <- function(vector, nBins) {
  intervals <- cut_number(vector, nBins)
  levels(intervals) <- gsub("[", "(", levels(intervals), fixed=TRUE)
  levels(intervals) <- gsub("]", ")", levels(intervals), fixed=TRUE)
  start <- unlist(gregexpr("(", levels(intervals), fixed = TRUE))
  end <- unlist(gregexpr(")", levels(intervals), fixed = TRUE))
  comma <- unlist(gregexpr(",", levels(intervals), fixed = TRUE))
  
  lower <- round(as.numeric(substr(levels(intervals), start + 1, comma - 1)), 2)
  upper <- round(as.numeric(substr(levels(intervals), comma + 1, end-1)), 2)
  levels(intervals) <- paste0("De ", lower, " até ", upper)
  return(intervals)
}

num_intervals_breaks <- function(vector, breaks) {
  intervals <- cut(vector, breaks)
  levels(intervals) <- gsub("[", "(", levels(intervals), fixed=TRUE)
  levels(intervals) <- gsub("]", ")", levels(intervals), fixed=TRUE)
  levels(intervals) <- gsub("-Inf", min(vector, na.rm = TRUE), levels(intervals))
  levels(intervals) <- gsub("Inf", max(vector, na.rm = TRUE), levels(intervals))
  start <- unlist(gregexpr("(", levels(intervals), fixed = TRUE))
  end <- unlist(gregexpr(")", levels(intervals), fixed = TRUE))
  comma <- unlist(gregexpr(",", levels(intervals), fixed = TRUE))
  
  lower <- round(as.numeric(substr(levels(intervals), start + 1, comma - 1)), 2)
  upper <- round(as.numeric(substr(levels(intervals), comma + 1, end-1)), 2)
  levels(intervals) <- paste0("De ", lower, " até ", upper)
  return(intervals)
}