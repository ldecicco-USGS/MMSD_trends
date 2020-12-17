library(EGRET)
library(EGRETci)
library(tidyverse)

path_to_group <- "C:/Users/ldecicco/DOI/GS-UMid MMSD - Trends/Data_Modeled/group_out"
path_to_elists <- "C:/Users/ldecicco/DOI/GS-UMid MMSD - Trends/Data_Inputs/eLists"

eList_list <- readRDS(file.path(path_to_elists, "eLists_out.rds"))

eList_names <- names(eList_list)

group_files <- list.files(path_to_group)
done_groups_boots_1 <- gsub(".rds","", gsub("group_1_boot_","",group_files[grepl("group_1_boot_", group_files)]))
done_groups_boots_2 <- gsub(".rds","", gsub("group_2_boot_","",group_files[grepl("group_2_boot_", group_files)]))

done_groups <- done_groups_boots_2[which(done_groups_boots_2 %in% done_groups_boots_1)]

# Can make a loop here, for now, 1 at a time:
id <- done_groups[1]

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
