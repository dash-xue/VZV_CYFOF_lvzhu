library(CytoML)
library(flowCore)
library(flowWorkspace)
library(ggcyto)
wsp_file <- "/public/users/xueyupeng/VZV/lvzhu/data/20260129/separate/20260129_separate.wsp"
fcs_path <- "/public/users/xueyupeng//VZV/lvzhu/data/20260129/separate/"

ws <- open_flowjo_xml(wsp_file)
gs <- flowjo_to_gatingset(
  ws = ws,
  path = fcs_path,
  name = "All Samples",
  truncate_max_range = FALSE
)

fr <- gs_pop_get_data(gs[[1]], "root")[[1]]

param <- pData(parameters(fr))

# 标记和通道
markers <- param$desc
channels <- param$name

# 过滤掉非生物marker
keep <- !grepl("Time|Event|DNA|Residual|Offset|Center", markers)

expr <- exprs(fr)[, channels[keep]]
colnames(expr) <- markers[keep]

pdf("/public/users/xueyupeng/VZV/lvzhu/sample1_marker_distribution.pdf", width = 12, height = 8)

chs <- colnames(expr)

for (i in seq(1, length(chs), by = 16)) {
  
  par(mfrow = c(4, 4), mar = c(2,2,2,1))
  
  subset_ch <- chs[i:min(i+15, length(chs))]
  
  for (ch in subset_ch) {
    hist(expr[, ch],
         main = ch,
         xlab = "",
         breaks = 100,
         col = "grey")
  }
}

dev.off()


ggcyto(gs[[1]],
       aes(x = `CD3_113In`, y = `CD19_174Yb`),
       subset = "root") +
  geom_hex(bins = 128) +
  geom_gate() +
  geom_stats() +
  theme_bw()

stats <- gs_pop_get_count_fast(gs)
head(stats)

gs_get_pop_paths(gs)

# 导出wsp中所有fcs的特定gating的数据
export_gate_fcs <- function(gs,
                            target_gate,
                            output_base = ".",
                            use_raw = FALSE,
                            prefix = NULL,
                            verbose = TRUE) {
  
  # 👉 取最后一层作为目录名
  gate_name_clean <- basename(target_gate)
  
  # 👉 进一步清理非法字符（空格、逗号等）
  gate_name_clean <- gsub("[^[:alnum:]_]+", "_", gate_name_clean)
  
  # 输出目录
  output_dir <- file.path(output_base, gate_name_clean)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  samples <- sampleNames(gs)
  
  if (verbose) {
    cat("Exporting gate:", target_gate, "\n")
    cat("Output dir:", output_dir, "\n")
  }
  
  for (s in samples) {
    
    if (verbose) cat("Processing:", s, "\n")
    
    tryCatch({
      
      fr <- gs_pop_get_data(
        gs[[s]],
        target_gate,
        inverse.transform = use_raw
      )[[1]]
      
      fname <- if (!is.null(prefix)) {
        paste0(prefix, "_", s, ".fcs")
      } else {
        paste0(s, ".fcs")
      }
      
      out_file <- file.path(output_dir, fname)
      
      write.FCS(fr, filename = out_file)
      
    }, error = function(e) {
      cat("❌ Skip:", s, "|", e$message, "\n")
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

for (g in gates) {
  export_gate_fcs(gs, g,
                  output_base = "/public/users/xueyupeng/VZV/lvzhu/subtype",
                  use_raw = FALSE)
}

for (g in gates) {
  export_gate_fcs(gs, g,
                  output_base = "/public/users/xueyupeng/VZV/lvzhu/subtype_raw",
                  use_raw = TRUE)
}

