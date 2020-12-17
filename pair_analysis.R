library(EGRET)
library(EGRETci)
library(tidyverse)

path_to_pairs_out <- "C:/Users/ldecicco/DOI/GS-UMid MMSD - Trends/Data_Modeled/pairs_no_rt_cen"
path_to_group <- "pairs_no_rt/pairs_out_new"

path_to_elists <- "C:/Users/ldecicco/DOI/GS-UMid MMSD - Trends/Data_Inputs/eLists"

eList_list <- readRDS(file.path(path_to_elists, "eList_no_dups.rds"))

eList_names <- names(eList_list)
eList_names <- eList_names[!grepl("FC_MPN", eList_names)]
eList_names <- eList_names[!grepl("LC-04", eList_names)]

pairs_all <- data.frame()
pairs_boot_all <- data.frame()

for(id in eList_names){
  eList <- eList_list[[id]]
  
  if(max(eList$Sample$waterYear, na.rm = TRUE) > 2015){
    # pairResults <- runPairs(eList,minNumUncen = 10,
    #                         year1 = 2004, year2 = 2016,
    #                         windowSide = 0)
    # 
    pairResults <- readRDS(file.path(path_to_group, paste0(id,".rds")))
    pairBoots <- readRDS(file.path(path_to_group,paste0(id,"_boot.rds")))

    # percentChange <- as.list(attr(pairResults, "Other")[["PercentChangeConc"]]) %>%
    #   as_tibble() %>%
    #   mutate(parameter = "Conc") %>%
    #   bind_rows(as.list(attr(pairResults, "Other")[["PercentChangeFlux"]]) %>%
    #               as_tibble() %>%
    #               mutate(parameter = "Flux"))
    # 
    # pairResults <- pairResults %>% 
    #   mutate(#parameter = row.names(.),
    #          id = !!id,
    #          wall = attr(pairResults, "Other")[["wall"]],
    #          ) %>% 
    #   as_tibble()
    
    pairs_all <- bind_rows(pairs_all, pairResults)
    pairs_boot_all <- bind_rows(pairs_boot_all, pairBoots$bootOut %>% 
                                  mutate(id = !!id))
  }
}

data.table::fwrite(pairs_all, file = file.path(path_to_pairs_out, "pairs.csv"))
data.table::fwrite(pairs_boot_all, file = file.path(path_to_pairs_out, "pairs_boot.csv"))
