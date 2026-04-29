library(CytoML)
library(flowCore)
library(flowWorkspace)
library(ggcyto)
library(dplyr)

# 导出wsp中所有fcs的特定gating的数据
export_gate_fcs <- function(gs,
                            target_gate,
                            output_base = ".",
                            use_raw = FALSE,
                            prefix = NULL,
                            verbose = TRUE) {
  
  library(flowCore)
  
  # ===== 目录名处理 =====
  gate_name_clean <- basename(target_gate)
  gate_name_clean <- gsub("[^[:alnum:]_]+", "_", gate_name_clean)
  
  output_dir <- file.path(output_base, gate_name_clean)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  samples <- sampleNames(gs)
  
  if (verbose) {
    cat("Exporting gate:", target_gate, "\n")
    cat("Output dir:", output_dir, "\n")
  }
  
  # ===== 主循环 =====
  for (i in seq_along(samples)) {
    
    s <- samples[i]
    tryCatch({
      
      # 1️⃣ 取数据
      fr <- gs_pop_get_data(
        gs[[s]],
        target_gate,
        inverse.transform = use_raw
      )[[1]]
      
      # 2️⃣ 从 FCS header 获取原始文件名（关键）
      key <- keyword(fr)
      
      orig_name <- if (!is.null(key$FILENAME)) {
        key$FILENAME
      } else if (!is.null(key$`$FIL`)) {
        key$`$FIL`
      } else {
        s  # fallback
      }
      orig_name <- basename(orig_name)
      # 3️⃣ 清理文件名（防止非法字符）
      orig_name <- gsub("[/:*?\"<>| ]+", "_", orig_name)
      
      # 4️⃣ 加 prefix（如果需要）
      fname <- if (!is.null(prefix)) {
        paste0(prefix, "_", orig_name)
      } else {
        orig_name
      }
      
      # 5️⃣ 保证 .fcs 后缀
      if (!grepl("\\.fcs$", fname, ignore.case = TRUE)) {
        fname <- paste0(fname, ".fcs")
      }
      
      # 6️⃣ 输出路径
      out_file <- file.path(output_dir, fname)
      
      # 7️⃣ 写文件
      write.FCS(fr, filename = out_file)
      
      if (verbose) {
        cat("✔ Saved:", fname, "\n")
      }
      
    }, error = function(e) {
      cat("❌ Skip sample:", s, "|", e$message, "\n")
    })
  }
  
  if (verbose) cat("Done!\n")
}


gates <- c("/B", "/B/Plasmablast","/nonTB/CD56-HLADR+/Classical_Mono",
           "/nonTB/CD56-HLADR+/Intermediate_Mono","/nonTB/CD56-HLADR+/Non-classical_Mono",
           "/nonTB/CD56-HLADR+/DC","/nonTB/CD56-HLADR+/DC/pDCs","/nonTB/CD56-HLADR+/DC/cDCs",
           "/nonTB/NK","/nonTB/NK/NKbright","/nonTB/NK/NKdim",
           "/Toltal T","/Toltal T/gdT","/Toltal T/NKT",
           "/Toltal T/T","/Toltal T/T/CD4T","/Toltal T/T/CD4T/CD4TCM",                
           "/Toltal T/T/CD4T/CD4TEMRA","/Toltal T/T/CD4T/CD4TEM",                
           "/Toltal T/T/CD4T/CD4TN","/Toltal T/T/CD8T",       
           "/Toltal T/T/CD8T/CD8TCM","/Toltal T/T/CD8T/CD8TEMRA",
           "/Toltal T/T/CD8T/CD8TEM","/Toltal T/T/CD8T/CD8TN"
          )


base_dir <- "/public/users/xueyupeng/VZV/lvzhu/data/"

# ===== 递归找所有 wsp =====
wsp_files <- list.files(
  base_dir,
  pattern = "\\.wsp$",
  recursive = TRUE,
  full.names = TRUE
)

# 👉 只保留 separate 目录
wsp_files <- wsp_files[grepl("/separate/", wsp_files)]

# 👉 去掉包含“副本”的文件
wsp_files <- wsp_files[!grepl("副本", basename(wsp_files))]

wsp_files

# ===== 2. 逐个 WSP 处理 =====
for (wsp_file in wsp_files) {
  
  cat("\n====================\n")
  cat("Processing:", wsp_file, "\n")
  
  tryCatch({
    
    fcs_path <- dirname(wsp_file)
    
    ws <- open_flowjo_xml(wsp_file)
    
    gs <- flowjo_to_gatingset(
      ws = ws,
      path = fcs_path,
      name = "All Samples",
      truncate_max_range = FALSE
    )
    
    for (g in gates) {
      
      export_gate_fcs(
        gs,
        target_gate = g,
        output_base = "/public/users/xueyupeng/VZV/lvzhu/subtype",
        use_raw = FALSE
      )
    }
    
    cat("✔ Done:", wsp_file, "\n")
    
  }, error = function(e) {
    cat("❌ Failed:", wsp_file, "|", e$message, "\n")
  })
}





for (wsp_file in wsp_files) {
  
  cat("\n====================\n")
  cat("Processing:", wsp_file, "\n")
  
  tryCatch({
    
    fcs_path <- dirname(wsp_file)
    
    ws <- open_flowjo_xml(wsp_file)
    
    gs <- flowjo_to_gatingset(
      ws = ws,
      path = fcs_path,
      name = "All Samples",
      truncate_max_range = FALSE
    )
    
    for (g in gates) {
      
      export_gate_fcs(
        gs,
        target_gate = g,
        output_base = "/public/users/xueyupeng/VZV/lvzhu/subtype_raw",
        use_raw = T
      )
    }
    
    cat("✔ Done:", wsp_file, "\n")
    
  }, error = function(e) {
    cat("❌ Failed:", wsp_file, "|", e$message, "\n")
  })
}






all_stats <- list()

for (wsp_file in wsp_files) {
  
  cat("\n====================\n")
  cat("Processing:", wsp_file, "\n")
  
  tryCatch({
    
    fcs_path <- dirname(wsp_file)
    
    ws <- open_flowjo_xml(wsp_file)
    
    gs <- flowjo_to_gatingset(
      ws = ws,
      path = fcs_path,
      name = "All Samples",
      truncate_max_range = FALSE
    )
    
    # ===== 1️⃣ count =====
    stats <- gs_pop_get_count_fast(gs)
    
    # ===== 2️⃣ 基本信息 =====
    pdata <- pData(gs)
    
    samples <- sampleNames(gs)
    
    # ===== 2️⃣ 提取 FILENAME（核心）=====
    file_map <- data.frame(
      name = samples,
      FILENAME = NA_character_,
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(samples)) {
      
      s <- samples[i]
      
      fr <- gs_pop_get_data(gs[[s]], "root")[[1]]
      key <- keyword(fr)
      
      orig_name <- if (!is.null(key$FILENAME)) {
        key$FILENAME
      } else if (!is.null(key$`$FIL`)) {
        key$`$FIL`
      } else {
        s
      }
      
      # 👉 只保留文件名（去路径）
      orig_name <- basename(orig_name)
      
      file_map$FILENAME[i] <- orig_name
    }
    
    # ===== 3️⃣ 合并 FILENAME =====
    stats <- left_join(stats, file_map, by = "name")
    
    stats <- stats %>%
      mutate(root_count = as.numeric(sub(".*_", "", name)))
    
    # ===== 6️⃣ 比例计算 =====
    stats$percent_of_parent <- stats$Count / stats$ParentCount
    stats$percent_of_root   <- stats$Count / stats$root_count
    
    # ===== 8️⃣ 加来源（可选）=====
    stats$wsp <- basename(wsp_file)
    
    all_stats[[wsp_file]] <- stats
    
    cat("✔ Done:", wsp_file, "\n")
    
  }, error = function(e) {
    cat("❌ Failed:", wsp_file, "|", e$message, "\n")
  })
}

# ===== 合并所有 =====
final_stats <- bind_rows(all_stats)



library(dplyr)
library(stringr)

final_stats <- final_stats %>%
  mutate(
    # 1. 按 "-" 拆
    part1 = str_split(FILENAME, "-", simplify = TRUE)[,1],
    part2 = str_split(FILENAME, "-", simplify = TRUE)[,2],
    part3 = str_split(FILENAME, "-", simplify = TRUE)[,3],
    
    # 2. 拆 part2: V0_20260129
    Timepoint = str_split(part2, "_", simplify = TRUE)[,1],
    Exp_id    = str_split(part2, "_", simplify = TRUE)[,2],
    
    # 3. 写字段
    Participant   = part1,
    Exp_position  = part3
  ) %>%
  select(-part1, -part2, -part3)

final_stats <- final_stats %>%
  mutate(
    Timepoint_V = recode(
      Timepoint,
      "V0" = "V1Day0", "V6" = "V1Day0",
      "V1" = "V1Day1", "V7" = "V1Day1",
      "V3" = "V2Day0", "V8" = "V2Day0",
      "V4" = "V2Day1", "V9" = "V2Day1",
      .default = NA_character_
    )
  )

# ===== 保存 =====
write.csv(
  final_stats,
  "/public/users/xueyupeng/VZV/lvzhu/mid_data/gating_stats.csv",
  row.names = FALSE
)

