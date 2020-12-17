library(EGRET)
library(EGRETci)
library(tidyverse)

path_to_group_out <- "C:/Users/ldecicco/DOI/GS-UMid MMSD - Trends/Data_Modeled/groups_no_rt_cen"
path_to_elists <- "C:/Users/ldecicco/DOI/GS-UMid MMSD - Trends/Data_Inputs/eLists"

path_to_group <- "group_no_rt/groups_no_dups"


eList_list <- readRDS(file.path(path_to_elists, "eList_no_dups.rds"))

eList_names <- names(eList_list)

group_files <- list.files(path_to_group)

group_2_ids <- group_files[grep("group_2_", group_files)]
group_2_ids <- group_2_ids[grep("_boot_", group_2_ids)]
group_2_ids <- gsub("group_2_boot_", "", group_2_ids)
group_2_ids <- gsub(".rds", "", group_2_ids)

group_out_all <- data.frame()
group_out_all_boot <- data.frame()

for(id in group_2_ids){
  eList <- eList_list[[id]]
  
  eList$Sample <- eList$Sample[eList$Sample$Q > 0, ]
  eList$Daily <- eList$Daily[eList$Daily$Q > 0, ]
  
  
  group_2_boot_out <- readRDS(file.path(path_to_group, paste0("group_2_boot_",id,".rds")))
  group_2_out <- readRDS(file.path(path_to_group, paste0("group_2_",id,".rds")))
  group_info <- attr(group_2_out, "groupInfo")
  
  group_2_out_extra <- group_2_out %>% 
    mutate(parameter = row.names(.),
           id = !!id,
           group1firstYear = group_info[["group1firstYear"]],
           group1lastYear = group_info[["group1lastYear"]], 
           group2firstYear = group_info[["group2firstYear"]],
           group2lastYear = group_info[["group2lastYear"]],
           wall = attr(group_2_out, "Other")[["wall"]])
  
  group_boot_extra <- group_2_boot_out[["bootOut"]] %>%
    mutate(id = !!id,
           group1firstYear = group_info[["group1firstYear"]],
           group1lastYear = group_info[["group1lastYear"]],
           group2firstYear = group_info[["group2firstYear"]],
           group2lastYear = group_info[["group2lastYear"]],
           wall = attr(group_2_out, "Other")[["wall"]])
    
  group_out_all <- bind_rows(group_out_all, group_2_out_extra)
  group_out_all_boot <- bind_rows(group_out_all_boot, group_boot_extra)
}

group_1_ids <- group_files[grep("group_1_", group_files)]
group_1_ids <- group_1_ids[grep("_boot_", group_1_ids)]
group_1_ids <- gsub("group_1_boot_", "", group_1_ids)
group_1_ids <- gsub(".rds", "", group_1_ids)

for(id in group_1_ids){
  eList <- eList_list[[id]]
  
  eList$Sample <- eList$Sample[eList$Sample$Q > 0, ]
  eList$Daily <- eList$Daily[eList$Daily$Q > 0, ]
  
  
  group_1_boot_out <- readRDS(file.path(path_to_group, paste0("group_1_boot_",id,".rds")))
  group_1_out <- readRDS(file.path(path_to_group, paste0("group_1_",id,".rds")))
  group_info <- attr(group_1_out, "groupInfo")
  
  group_1_out_extra <- group_1_out %>% 
    mutate(parameter = row.names(.),
           id = !!id,
           group1firstYear = group_info[["group1firstYear"]],
           group1lastYear = group_info[["group1lastYear"]], 
           group2firstYear = group_info[["group2firstYear"]],
           group2lastYear = group_info[["group2lastYear"]],
           wall = attr(group_1_out, "Other")[["wall"]])
  
  group_boot_extra <- group_1_boot_out[["bootOut"]] %>%
    mutate(id = !!id,
           group1firstYear = group_info[["group1firstYear"]],
           group1lastYear = group_info[["group1lastYear"]],
           group2firstYear = group_info[["group2firstYear"]],
           group2lastYear = group_info[["group2lastYear"]],
           wall = attr(group_1_out, "Other")[["wall"]])
  
  group_out_all <- bind_rows(group_out_all, group_1_out_extra)
  group_out_all_boot <- bind_rows(group_out_all_boot, group_boot_extra)
}

data.table::fwrite(group_out_all, file = file.path(path_to_group_out, "group_out_all.csv"))
data.table::fwrite(group_out_all_boot, file = file.path(path_to_group_out, "group_Trends_all.csv"))

eList <- eList_list[[id]]
plot(eList)
group_1_boot_out <- readRDS(file.path(path_to_group, paste0("group_1_boot_",id,".rds")))
group_2_boot_out <- readRDS(file.path(path_to_group, paste0("group_2_boot_",id,".rds")))
group_1_out <- readRDS(file.path(path_to_group, paste0("group_1_",id,".rds")))
group_2_out <- readRDS(file.path(path_to_group, paste0("group_2_",id,".rds")))

attr(group_1_out, "groupInfo")
attr(group_1_out, "dateInfo")
attr(group_1_out, "SampleBlocks")
attr(group_1_out, "Other")

plotHistogramTrend(eList, 
                   group_1_boot_out, flux = FALSE)
plotHistogramTrend(eList, 
                   group_1_boot_out, flux = TRUE)

plotHistogramTrend(eList, 
                   group_2_boot_out, flux = FALSE)
plotHistogramTrend(eList, 
                   group_2_boot_out, flux = TRUE)
