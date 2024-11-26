# Infrastructures of Race? Colonial Indigenous Segregation and Contemporary Land Values

This is the replication package for *Infrastructures of Race? Colonial Indigenous Segregation and Contemporary Land Values* by [Baldomero-Quintana, Woo-Mora, and de la Rosa-Ramos (2024)](https://www.sciencedirect.com/science/article/abs/pii/S0166046224000966).

<div align="center">
  <img src="https://github.com/woomora/Infrastructures_of_Race/blob/main/plots/nonparam_reg.png" width="95%">
</div>

## Instructions

To replicate the analysis in R:

1. Make sure you have at least R version 4.4.1 (see sessionInfo below). You can download the latest R version [here](https://cloud.r-project.org/bin/macosx/).
2. For better and faster map visualization, use `X11(type = "cairo")`. Make sure you have XQuartz installed ([available here](https://www.xquartz.org/)), as it provides X11 support on macOS.
3. Download the data files from [this link](https://www.dropbox.com/scl/fo/if8z28io6dtr6uz4ce3a6/AGTQNamHK1vuLIw_JcEs6Vo?rlkey=h1h3jj426dx83l828ci85a41s&dl=0). Add them to the master file. 
4. Open `IoR_master.R` and run the script to reproduce each figure and table.


**Notes:**  

Ensure all necessary packages are installed using `pacman::pload`. 

```r
# If not installed, install package pacman
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
```

Session Information:
```r
sessionInfo()
# R version 4.4.1 (2024-06-14)
# Platform: aarch64-apple-darwin20
# Running under: macOS Sonoma 14.2.1
#
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
#
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
```

---

## Citation

Please cite accordingly if using any of the data:

```bibtex
@article{BWdlR2024,
  title = {Infrastructures of race? Colonial indigenous segregation and contemporary land values},
  author = {Luis {Baldomero-Quintana} and L. Guillermo {Woo-Mora} and Enrique {De la Rosa-Ramos}},
  journal = {Regional Science and Urban Economics},
  year = {2024},
  volume = {In Press},
  doi = {10.1016/j.regsciurbecon.2024.104065},
  url = {https://doi.org/10.1016/j.regsciurbecon.2024.104065}
}
```

If using *Pueblos de Indios* data, please cite [Tanck de Estrada (2005)](https://repositorio.colmex.mx/concern/books/0v838347v?locale=es):

```bibtex
@book{TanckdeEstrada2005,
  title={Atlas Ilustrado De Los Pueblos De Indios: Nueva España, 1800},
  author={Dorothy {Tanck de Estrada}},
  year={2005},
  publisher={El Colegio de México},
  address={Mexico City},
  url={https://repositorio.colmex.mx/concern/books/0v838347v?locale=es}
}
```

---

If you have any issues, please reach out to guillermo.woo-mora [at] psemail.eu. See also academic website for latest public email.
