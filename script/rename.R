process_filename = function(file_dir){
  library(readxl)
  library(dplyr)
  library(tidyr)
  data_dir = file.path(file_dir,'separate')
  metadata_dir = list.files(path = data_dir,pattern = '\\.xlsx$',full.names = T)
  # 检查是否找到 xlsx 文件
  if (length(metadata_dir) != 1) {
    stop("❌ xlsx文件数不为1，请检查。")
  } else {
    print(metadata_dir)
    metadata <- readxl::read_xlsx(metadata_dir)
    cat("✔ 已成功读入 metadata 文件\n")
  }
  metadata <- metadata %>%
    select(-1) %>%        # 删除第一列
    select(where(~ !any(is.na(.))))   # 删除所有包含 NA 的列
  colnames(metadata) = c(1,2,3,4)
  
  file_list = list.files(path = data_dir,pattern = "\\.fcs$", full.names = FALSE)
  df <- tibble(
    filename = file_list,
    pos = sapply(strsplit(file_list, "-"), `[`, 2)
  ) %>%
    separate(pos, into = c("col", "row"), sep = "_", remove = FALSE)
  
  df$participant <- mapply(function(r, c) metadata[r, c][[1]], 
                           df$row, df$col)
  
  df$newname <- paste0(df$participant, "_", sub("^export_", "", df$filename))
  
  # 假设 df 中包含 filename（原名） 和 newname（新名）
  for(i in seq_len(nrow(df))){
    old <- df$filename[i]
    old_path = file.path(data_dir,old)
    new <- df$newname[i]
    new_path = file.path(data_dir,new)
    if (file.exists(old_path)) {
      file.rename(old_path, new_path)
    } else {
      message("⚠️ 文件不存在：", old_path)
    }
  }
}

setwd('~/VZV/lvzhu/20260318/')

folder = "~/VZV/lvzhu/20260318/"
process_filename(folder)

remove_prefix_fcs = function(dir){
  
  files = list.files(dir, pattern = "\\.fcs$", full.names = FALSE)
  
  for(f in files){
    
    parts = strsplit(f, "_")[[1]]
    
    # 如果没有 "_" 就跳过
    if(length(parts) <= 1){
      message("⚠️ 跳过（无前缀）：", f)
      next
    }
    
    new_name = paste(parts[-1], collapse = "_")
    
    old_path = file.path(dir, f)
    new_path = file.path(dir, new_name)
    
    # 防止覆盖
    if(file.exists(new_path)){
      message("❌ 已存在，跳过：", new_name)
      next
    }
    
    file.rename(old_path, new_path)
    
    cat("✔ ", f, " -> ", new_name, "\n")
  }
  
  cat("🎉 完成\n")
}
file_dir = '~/VZV/lvzhu/20260318/separate'
remove_prefix_fcs(file_dir)
