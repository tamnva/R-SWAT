---
title: 'RSWAT: An R package for the Soil and Water Assessment Tool models'
tags:
- R
- SWAT
- parameter calibration
- sensitivity and uncertainty analysis
date: "2 February 2024"
output:
  html_document:
    df_print: paged
authors:
- name: Tam V. Nguyen
  orcid: 0000-0001-9111-4393
  equal-contrib: yes
  affiliation: '1'
bibliography: paper.bib
affiliations:
- name: Tam V. Nguyen, Helmholtz Centre for Environmental Research - UFZ,Germany
  index: 1
---

# Summary

Hydro-ecological model is an important tool for a quantitative understanding of hydro-ecological processes and their interactions. Parameter calibration, sensitivity, and uncertainty analyses play a crucial role in hydro-ecological modeling [@song2015; @abbaspour2022]. A model-specific tool is often required for parameter calibration, sensitivity, and uncertainty analyses as each model usually has its own input and output file format/structure. This study introduces the RSWAT package, which resulted from the continuous development of the R-SWAT shiny application [@nguyen2022] for the Soil and Water Assessment Tool (SWAT, @arnold1998) community. RSWAT has more functionalities compared to R-SWAT, allowing users to work with SWAT and different SWAT versions with or without using the graphical user interface (GUI).

# Statement of need

The SWAT model is one of the most widely used and tested hydro-ecological models [@gassman2007; @aloui2023]. SWAT has been constantly developed since its development due to an improved understanding of hydro-ecological processes [e.g., @ilampooranan2019] and to solve new challenges. On the other hand, SWAT also enhances our understanding of reality and contributes to water resources management at both local and regional scales [@gassman2007]. While most of the changes in SWAT are minors, there are major changes [@bieger2017]. Many technical challenges arise when working with models that are constantly changing. For example, model parameter calibration, sensitivity, and uncertainty analyses are not an easy task as (1) SWAT has numerous input files and parameters, and (2) there is a lack of a standard tool that can cope with these changes.

Various tools have been developed to support SWAT modeling, for example, [ArcSWAT](https://swat.tamu.edu/software/arcswat/), [QSWAT](https://swat.tamu.edu/software/qswat/), R-SWAT [@nguyen2022], SWAT-CUP [@abbaspour2015], SWATrunR [@christoph2019], and IPEAT+ [@Yen2019]. Among those, R-SWAT is the only R-based application with the GUI that can work with various versions of SWAT (e.g., [SWAT-Carbon](https://sites.google.com/view/swat-carbon)) without modifying the R-SWAT code. R-SWAT has various built-in and add-on methods for automatic parameter calibration and/or parameter sensitivity and uncertainty analyses from the R community. However, as a shiny application (not an R package), installing R-SWAT is troublesome since R-SWAT depends on many R packages. In addition, R-SWAT has no clear separation of functions for the GUI and stand-alone functions for working without the GUI. No documentation of the R-SWAT functions also makes it difficult to use without the GUI. R-SWAT cannot be employed for SWAT+ [@bieger2017], a completely revised structure of SWAT. R-SWAT does not support interactive manual calibration of SWAT and SWAT+ in a similar way to airGRteaching [@delaigue2023], which is especially useful for teaching. Therefore, major changes in R-SWAT are needed to overcome the aforementioned limitations. 


# Features
RSWAT is an R [@r2021] package developed based on the original version of the R-SWAT shiny application. Therefore, RSWAT inherits the following functionalities of the R-SWAT: 
  
  + Automatic parameter calibration, sensitivity, and uncertainty analyses for SWAT with the GUI using various approaches.
  + Visualization of the simulated and observed variables. 
  
Spatial visualization of the model results was not included in RSWAT as this option is not often used by users and there exist different tools for this (e.g., PAVLIB4SWAT, @lin2023). Additional features of RSWAT compared to R-SWAT are:

  + Automatic parameter calibration, sensitivity, and uncertainty analyses for SWAT and SWAT+ with and without using the GUI. 
  + Manual interactive calibration of SWAT and SWAT+.
  
# Mention
  R-SWAT/RSWAT is being adopted for SWAT/SWAT+ modeling. Currently, there are more than 160 users in the [R-SWAT Google group](https://groups.google.com/g/R-SWAT) with more than 120 discussion topics. R-SWAT/RSWAT was used in some recent publications (@wang2023 @wu2024 
@li2024, @karki2023, @ougahi2024, @myers2023). With additional features, RSWAT is expected to be used not only in research but also in teaching.

# References
