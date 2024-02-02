---
title: 'RSWAT: An extenstion of R-SWAT for parameter parameter calibration with SWAT
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
- name: Tam V. Nguyen, Helmholtz Centre for Environmental Research – UFZ,Germany
  index: 1
---

# Summary

write some text here

# Statement of need

The Soil and Water Assessment Tool (SWAT) is one of the most widely used and tested hydro-ecological models. SWAT has been constantly modified for different purposes, such as to adapt to certain hydrological conditions or to extend its functionalities. Most of the modifications are minor, additional input/output files but no changes in the model input/output file structures (e.g., ), there is also major changes in SWAT structures such as the 




`RSWAT` is an R package designed for parameter calib


Astropy-affiliated Python package for galactic dynamics. Python
enables wrapping low-level languages (e.g., C) for speed without losing

flexibility or ease-of-use in the user-interface. The API for `Gala` was
designed to provide a class-based and user-friendly interface to fast (C or
Cython-optimized) implementations of common operations such as gravitational
potential and force evaluation, orbit integration, dynamical transformations,
and chaos indicators for nonlinear dynamics. `Gala` also relies heavily on and
interfaces well with the implementations of physical units and astronomical
coordinate systems in the `Astropy` package [@astropy] (`astropy.units` and
`astropy.coordinates`).

`Gala` was designed to be used by both astronomical researchers and by
students in courses on gravitational dynamics or astronomy. It has already been
used in a number of scientific publications [@Pearson:2017] and has also been
used in graduate courses on Galactic dynamics to, e.g., provide interactive
visualizations of textbook material [@Binney:2008]. The combination of speed,
design, and support for Astropy functionality in `Gala` will enable exciting
scientific explorations of forthcoming data releases from the *Gaia* mission
[@gaia] by students and experts alike.

# Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$

Double dollars make self-standing equations:

$$\Theta(x) = \left\{\begin{array}{l}
0\textrm{ if } x < 0\cr
1\textrm{ else}
\end{array}\right.$$

You can also use plain \LaTeX for equations
\begin{equation}\label{eq:fourier}
\hat f(\omega) = \int_{-\infty}^{\infty} f(x) e^{i\omega x} dx
\end{equation}
and refer to \autoref{eq:fourier} from text.

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"


# Acknowledgements

We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
Oh, and support from Kathryn Johnston during the genesis of this project.

# References
