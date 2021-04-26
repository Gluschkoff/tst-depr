#### R-script for the publication Testosterone and specific symptoms of depression: Evidence from NHANES 2011–2016, https://doi.org/10.1016/j.cpnec.2021.100044.

Modelling was performed in R Statistical Software (version 3.6.1) using the svyglm function [22]. The complete code is freely available on GitHub (https://github.com/Gluschkoff/tst-depr). Statistical significance for all tests was set at p ​< ​.05, and all tests were two-tailed. Given the exploratory nature of the study, nonsignificant associations with p values ​< ​.10 are also noted.

Linear regression models were fitted to examine the sex-specific associations of total testosterone with depression sum score and logistic regression for associations with specific symptoms of depression. We first examined the associations of low total testosterone. For this analysis, total testosterone levels were dichotomized into low and normal categories (cut-off <300 ​ng/dL for men and <15 ​ng/dL for women) [18,19,23]. To accommodate for more complex, potential non-linear associations, we next used restricted (also known as natural) cubic splines to model the distribution of total testosterone (see Ref. [24].) A spline is constructed of piecewise polynomials which pass through a set of data values (i.e., knots). The spline included two boundary knots (the lowest and highest values of testosterone) and two internal knots that were placed at the 33rd and 66th percentiles of total testosterone (316.83 and 456.73 for men, 14.75 and 24.30 for women). Statistical significance of the overall effect of total testosterone was tested with a Wald test.

After fitting unadjusted models, we adjusted for the effects of age, BMI, alcohol use, smoking, physical activity (men and women), and pregnancy status (women). As a sensitivity analysis, we repeated all the analysis in the subsets of men and women who screened positive for at least mild depression (PHQ9 sum score ​> ​4). We also assessed the sensitivity of our results to an alternative coding strategy where we used log-transformed testosterone values instead of raw concentrations.

Following NHANES analytic and reporting guidelines, all analyses were conducted using survey procedures to account for the complex survey design (including oversampling), survey nonresponse, and post-stratification. 