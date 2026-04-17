import os

N_THREADS = "32"

os.environ["OMP_NUM_THREADS"] = N_THREADS
os.environ["MKL_NUM_THREADS"] = N_THREADS
os.environ["OPENBLAS_NUM_THREADS"] = N_THREADS
os.environ["NUMBA_NUM_THREADS"] = N_THREADS
os.environ["VECLIB_MAXIMUM_THREADS"] = N_THREADS
os.environ["NUMEXPR_NUM_THREADS"] = N_THREADS

import pytometry as pm
import anndata as ad

import scanpy as sc
sc.settings.n_jobs = 32
sc.settings.verbosity = 3

import numpy as np
import pandas as pd
import seaborn as sb
from sklearn.neighbors import KernelDensity
from scipy.spatial.distance import cdist
import scanpy.external as sce
import datetime
import matplotlib.pyplot as pl
from matplotlib import rcParams
from matplotlib import colors
from fcsparser import parse

data_path = "/public/users/xueyupeng/VZV/lvzhu/"

fcs_files = []

for root, dirs, files in os.walk(data_path):
    # 只在路径中包含 separate 的目录中找 fcs
    if os.path.basename(root) == "separate":
        for file in files:
            if file.endswith(".fcs"):
                fcs_files.append(os.path.join(root, file))

print(len(fcs_files))
fcs_files[:5]

records = []
for f in fcs_files:
    try:
        meta, data = parse(f, reformat_meta=True)
        cell_count = data.shape[0]
        records.append({
            "file": f,
            "cells": cell_count
        })
    except Exception as e:
        print(f"无法读取文件 {f}: {e}")
print("all files read")
# ===== 保存结果 =====
record_dir = os.path.join(data_path, "record")
os.makedirs(record_dir, exist_ok=True)
out_file = os.path.join(record_dir, "cell_counts.csv")
pd.DataFrame(records).to_csv(out_file, index=False)
print(f"Saved to: {out_file}")


adatas = []

for file_path in fcs_files:
    # ===== 取纯文件名 =====
    file_name = os.path.basename(file_path)
    # A004-V1_20260129-1_2-3.fcs
    prefix = file_name.split("_")[0]        # A004-V1
    ID = prefix.split("-")[0]               # A004
    Visit = prefix.split("-")[1]            # V1
    Sample = f"{ID}_{Visit}"
    # ===== 读取FCS =====
    adata = pm.io.read_fcs(file_path)
    # ===== 写metadata =====
    adata.obs["ID"] = ID
    adata.obs["Visit"] = Visit
    adata.obs["Sample"] = Sample
    # ===== CyTOF signal split =====
    pm.pp.split_signal(
        adata,
        var_key="channel",
        data_type="cytof"
    )
    adatas.append(adata)
print("Total adatas:", len(adatas))

np.random.seed(42)
down_adatas = []
for adata in adatas:
    n_cells = adata.n_obs
    if n_cells > 50000:
        idx = np.random.choice(n_cells, 50000, replace=False)
        adata_sub = adata[idx].copy()
    else:
        adata_sub = adata.copy()
    down_adatas.append(adata_sub)


records = []
for adata in down_adatas:
    sample_name = adata.obs["Sample"].iloc[0]
    n_cells = adata.n_obs
    records.append({
        "Sample": sample_name,
        "Cells": n_cells
    })
df_cells = pd.DataFrame(records)
print(df_cells)

# 合并所有 sample
adata_all = ad.concat(
    down_adatas,
    join="outer",        # 保留所有marker
    label="batch",       # 自动生成batch列
    keys=[a.obs["Sample"].iloc[0] for a in down_adatas]
)

print(adata_all)


# 保存原始矩阵
adata_all.layers["raw"] = adata_all.X.copy()
# arcsinh 转换
cofactor = 5
adata_all.X = np.arcsinh(adata_all.X / cofactor)

print("arcsinh finished")

print(adata_all.shape)

print("RAW range:",
      adata_all.layers["raw"].min(),
      adata_all.layers["raw"].max())

print("Arcsinh range:",
      adata_all.X.min(),
      adata_all.X.max())

markers = ['CD8_112Cd','CD3_113In',  'CD4_115In', 'CD57_139La', 'CD56_141Pr', 
           'IgD_142Nd', 'IFN-a_144Nd', 'CXCL9_146Nd', 'CD86_147Sm','H3K27ac_148Nd', 
           'IL-1b_149Sm', '4E-BP1_150Nd', 'pS6_151Eu','CCR2_152Sm', 'TNF-a_153Eu', 
           'IL-4_154Sm', 'CD14_155Gd', 'CASP-8_156Gd','TCR-gd_158Gd', 'CD38_159Tb', 
           'CD27_160Gd', 'CD123_161Dy','Ki-67_162Dy', 'CXCL10_163Dy', 'Arginase-1_164Dy', 
           'IL-6_165Ho','MCP-1_166Er', 'CD69_167Er', 'STC1_168Er', 'GZMB_169Tm', 
           'CD45RA_170Er','HLA-DR_171Yb', 'IFN-g_172Yb', 'CD11c_173Yb', 'CD19_174Yb',
       'pSTAT1_175Lu', 'pSTAT3_176Yb',  'CD16_209Bi']

adata_all.write("/public/users/xueyupeng/VZV/lvzhu/record/adata_all_down_50k.h5ad")

mask = adata_all.var_names.isin(markers)

sc.tl.pca(
    adata_all,
    n_comps=30,
    mask_var=mask,
    svd_solver="randomized"
)
print(sum(mask))
print(adata_all.var_names[mask])

sc.pl.pca_overview(adata_all, color = "Visit")

sc.pp.neighbors(
    adata_all,
    n_neighbors=15,
    n_pcs=10,
    method="pynndescent"
)
print("Neighbors finished")

sc.tl.umap(adata_all)
print("UMAP finished")

sc.tl.leiden(
    adata_all,
    resolution=0.5,
    flavor="igraph",
    n_iterations=2,
    directed=False
)
print("Leiden finished")

adata_all.write("/public/users/xueyupeng/VZV/lvzhu/record/adata_all_umap_50k.h5ad")

sc.pl.umap(
    adata_all,
    color="leiden",
    legend_loc="on data",
    frameon=False,
    size=5,
    title="UMAP - Leiden clusters"
)

sc.pl.umap(
    adata_all,
    color="Visit",
    frameon=False,
    size=5,
    title="UMAP - Sample"
)

sc.pl.umap(
    adata_all,
    color=markers,
    ncols=2,
    cmap="viridis",
    frameon=False,
    size=5
)

sc.settings.figdir = "/public/users/xueyupeng/VZV/lvzhu/figures"
os.makedirs("/public/users/xueyupeng/VZV/lvzhu/figures", exist_ok=True)

sc.pl.umap(
    adata_all,
    color="leiden",
    legend_loc="on data",
    frameon=False,
    show=False,
    save="_leiden.png"
)

for m in markers:
    
    sc.pl.umap(
        adata_all,
        color=m,
        cmap="viridis",
        frameon=False,
        show=False,
        save=f"_UMAP_{m}.png"
    )

for m in markers:
    
    sc.pl.violin(
        adata_all,
        keys=m,
        groupby="leiden",
        rotation=90,
        stripplot=False,
        show=False,
        save=f"_violin_{m}.png"
    )


dp = sc.pl.dotplot(
    adata_all,
    var_names=markers,
    groupby="leiden",
    standard_scale="var",
    cmap="RdBu_r",
    return_fig=True
)

dp.savefig("/public/users/xueyupeng/VZV/lvzhu/figures/dotplot_markers.png", dpi=300)

