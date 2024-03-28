# Composite eDISH Model

This repository contains a shiny app code that generates and displays the composite eDISH plot and eDISH migration table featured in our papper with title: **_`Composite plot for visualizing aminotransferase and bilirubin changes in clinical trials of subjects with abnormal baseline values`_** which is accepted for published in the Journal of Drug Safety.

The composite eDISH plot is a visualization of peak on-treatment changes in liver test results both as multiples of the upper limit of normal (xULN) and multiples of the subjects’ baseline (xBLN) values in clinical trials of subjects with abnormal baseline values. The eDISH migration table is a summary table of changes in eDISH quadrants from pretreatment (baseline) to peak on-treatment liver test results. See figures below for sampled of eDISH plot and migration table.

<p align="center">
  <img src="https://github.com/FDA/Composite-eDish-Plot/blob/Outcome/Composite%20eDISH%20Plot.png">
</p>

<p align="center">
<img src="https://github.com/FDA/Composite-eDish-Plot/blob/Outcome/eDISH%20Migration%20Table.jpeg">
</p>

## Authors

The codes and "readme" file were written by Bereket Tesfaldet.

## R Code Requirements

This Shiny app was developed with R version 4.3.2 and uses the following packages:

+	shiny (version 1.8.0)
+	shinythemes (version 1.2.0)
+	ggplot2 (version 3.4.4)
+	dplyr (version 1.1.4)
+	tidyr (version 1.3.0)
+	datawizard (version 0.9.1)
+	gt (version 0.10.1)
+	janitor (version 2.2.0)

## Input Data Requirements

The following two datasets in ADaM CDISC standard are required to run the application:
+	adsl - A subject level dataset (~ADSL) with at least the following two variables: USUBJID and ARM.
+	adlb - A laboratory dataset (~ADLB) with at least the following six variables: USUBJID, PARAMCD, AVISITN, AVAL, ANRHI, and BASE.

> [!IMPORTANT]
> **_In this release, AVISITN = 0 is assumed to be the baseline visit while all visits with AVISITN > 0 are assumed to be treatment visits._** 

Hypothetical adsl and adlb datasets are included in the repository to demonstrate the application.

## Disclaimer
Wherever applied, the contents in this Software and its accompanied documentations reflect the views of the authors and should not be construed to represent FDA’s views or policies. The use of this code in no way implies endorsement by the FDA. Besides, FDA makes no representations that the use of the Software will not infringe any patent or proprietary rights of third parties.
