---
title: 'RSWAT: An extension of the R-SWAT app for SWAT modelling
  and its modified versions'
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
bibliography: joss_manuscript.bib
affiliations:
- name: Tam V. Nguyen, Helmholtz Centre for Environmental Research - UFZ,Germany
  index: 1
---

# Summary

write some text here

# Statement of need

The Soil and Water Assessment Tool, SWAT [@arnold1998], is one of the most widely used and tested hydro-ecological models (@gassman2007; @aloui2023). SWAT has been constantly developed since its development due to our improved understanding of hydro-ecological processes (e.g., @nguyen2018; @ilampooranan2019) and to solve new challenges. On the other hand, SWAT also enhances our understanding of reality and contributes to water resources management at both local and regional scales (@gassman2007). While most of the changes in SWAT are minors, there are major changes [@bieger2017]. Many technical challenges arise when working with models that are constantly undergoing changes. For example, model parameter calibration, sensitivity and uncertainty analyses are not easy as (1) SWAT has numerous inputs files and parameters and (2) there is a lack of a standard tool that can cope with these changes.

Various tools have developed to support SWAT parameter calibration and/or parameter sensitivity and uncertainty analyses, for example, ArcSWAT, QSWAT, R-SWAT, SWAT-CUP, SWATrunR, SWAT+ Toolbox. Among those, R-SWAT is the only R-based tool with a graphical user interface. In addition, R-SWAT can be cope with minor changes of SWAT without modifying it code. However, SWAT+ is completely restructured version of SWAT and R-SWAT was not designed to work with SWAT+. Due the increasing used of SWAT+ and R-SWAT, there is a growing demand to modify R-SWAT for SWAT+. In addition, developing an interactive manual calibration of both SWAT and SWAT+ in R-SWAT is required, especially for 




`RSWAT` is an R package designed for parameter calib




For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"


# Acknowledgements

We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
Oh, and support from Kathryn Johnston during the genesis of this project.

# References
