# Composite eDISH Model

This repository contains a shiny app code that generates and displays the composite eDISH plot, subject level plot of liver tests overtime, and eDISH migration table featured in our paper with title: [**_`Composite plot for visualizing aminotransferase and bilirubin changes in clinical trials of subjects with abnormal baseline values`_**](https://doi.org/10.1007/s40264-024-01425-5) which was puplished in Drug Safety[^1].

[^1]: Tesfaldet, B., Patel, T., Chen, M. et al. Composite Plot for Visualizing Aminotransferase and Bilirubin Changes in Clinical Trials of Subjects with Abnormal Baseline Values. Drug Saf 47, 699–710 (2024). 

The composite eDISH plot is a visualization of peak on-treatment changes in liver test results both as multiples of the upper limit of normal (xULN) and multiples of the subjects' baseline (xBLN) values in clinical trials of subjects with abnormal baseline values. A simple click on a point in the plot enables generating overtime plot of all liver tests of a corresponding subject for a detailed observation. The eDISH migration table is a summary table of changes in eDISH quadrants from pretreatment (baseline) to peak on-treatment liver test results. See figures below for sampled of eDISH plot and migration table.

<p align="center">
  <kbd><img src="https://github.com/FDA/Composite-eDish-Plot/blob/main/Output/Composite%20eDISH%20Plot.png"></kbd>  
</p>

<p align="center">
  <kbd><img src="https://github.com/FDA/Composite-eDish-Plot/blob/main/Output/LinePlot.png"></kbd>
</p>


<p align="center">
<kbd><img src="https://github.com/FDA/Composite-eDish-Plot/blob/main/Output/eDISH%20Migration%20Table.jpeg"></kbd>
</p>

## Authors

The codes and "readme" file were written by Bereket Tesfaldet.

## R Code Requirements

This Shiny app was developed with R version 4.3.2 and uses the following packages:

+	shiny (version 1.8.0)
+	shinyBS (version 0.61.1)
+	shinythemes (version 1.2.0)
+	plotly (version 4.10.4)
+	ggplot2 (version 3.4.4)
+	dplyr (version 1.1.4)
+	tidyr (version 1.3.0)
+	datawizard (version 0.9.1)
+	gt (version 0.10.1)
+	janitor (version 2.2.0)
+	patchwork (version 1.2.0)

## Input Data Requirements

The following two datasets in ADaM CDISC standard are required to run the application:
+	adsl - A subject level dataset (~ADSL) with at least the following two variables: USUBJID and ARM.
+	adlb - A laboratory dataset (~ADLB) with at least the following six variables: USUBJID, PARAMCD, AVISITN, ADY, AVAL, ANRHI, and BASE.

> [!IMPORTANT]
> **_In this release, AVISITN = 0 is assumed to be the baseline visit while all visits with AVISITN > 0 are assumed to be treatment visits._** 

Hypothetical adsl and adlb datasets are included in the repository to demonstrate the application.

To cite the code: [![DOI](https://zenodo.org/badge/777308644.svg)](https://zenodo.org/doi/10.5281/zenodo.10892050)

## Disclaimer
Wherever applied, the contents in this Software and its accompanied documentations reflect the views of the authors and should not be construed to represent FDAâ€™s views or policies. The use of this code in no way implies endorsement by the FDA. Besides, FDA makes no representations that the use of the Software will not infringe any patent or proprietary rights of third parties.


## Disclaimer
Wherever applied, the contents in this Software and its accompanied documentations reflect the views of the authors and should not be construed to represent FDA’s views or policies. The use of this code in no way implies endorsement by the FDA. Besides, FDA makes no representations that the use of the Software will not infringe any patent or proprietary rights of third parties.

