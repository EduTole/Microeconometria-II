
///Econometric Mehthods 2 - STATA Workshop # 2  
************************************************************************* 
***********MEAN & QUANTILE DECOMPOSITION TECHNIQUES IN ECONOMICS*********
*************************************************************************

******************************************************************************
*****Question 1 - Summarize the Raw Data & Test for any Gender Differences********
******************************************************************************
cd "C:\Users\et396\OneDrive\Dropbox\Docencia\UNAC\Evaluation\S4\Aplication"
use BD4.dta,clear

sum lnwage educ exper expersq tenure married divorced single male
sum lnwage educ exper expersq tenure married divorced single if male==1
sum lnwage educ exper expersq tenure married divorced single if male==0

ttest lnwage, by(female)
ttest educ, by(female)
ttest exper, by(female)
ttest tenure, by(female)

median lnwage, by(female)
median educ, by (female)
median exper, by(female)
median tenure, by(female)



tab status female,chi2
tab single female,chi2
tab married female,chi2
tab divorce female,chi2



*****************************************************************************
*Question 2 - Mean Regressions by Gender Group & Oaxaca-Blinder (OB)decompositions
*****************************************************************************
findit oaxaca8
////Click on the first link and then click to install.

reg lnwage educ exper expersq tenure married divorced if male==1, robust
estimates store male
reg lnwage educ exper expersq tenure married divorced if male==0, robust
estimates store female


///This assumes a male wage structure in the absence of unequal treatment
oaxaca8 male female, weight(1)
///This assumes a female wage structure in the absence of unequal treatment 
oaxaca8 male female, weight(0)


**********************************************************************************************
*** Question 2: Mean Oaxaca-Blinder decomposition (aggregate and detailed sub-scomponent) ****
**********************************************************************************************

***Download the STATA Oaxaca programme
ssc install oaxaca, replace
///This assumes a male wage structure in the absemce of unequal treatment
oaxaca lnwage educ exper expersq tenure married divorced, by(female) weight(1) vce(robust) nodetail
///This assumes a female wage structure in the absemce of unequal treatment
oaxaca lnwage educ exper expersq tenure married divorced, by(female) weight(0) vce(robust) nodetail



************************************************************************
***** Question 3 *** Summarise the Log Wage Data by Quantiles*********** 
************************************************************************

****Compute summary statistics for selected quantiles
centile lnwage if male==1, centile (10 25 50 75 90) 
centile lnwage if male==0, centile (10 25 50 75 90) 

****Obtaining (very approximate) satandard errors for quantile values

****The male sample
qreg lnwage if male==1, quantile (0.1) 
qreg lnwage if male==1, quantile (0.25) 
qreg lnwage if male==1, quantile (0.50) 
qreg lnwage if male==1, quantile (0.75) 
qreg lnwage if male==1, quantile (0.90) 

****The female sample
qreg lnwage if male==0, quantile (0.1) 
qreg lnwage if male==0, quantile (0.25) 
qreg lnwage if male==0, quantile (0.50) 
qreg lnwage if male==0, quantile (0.75) 
qreg lnwage if male==0, quantile (0.90) 

***Test for gender differences at selected quantiles

qreg lnwage male, quantile (0.1) 
qreg lnwage male, quantile (0.25) 
qreg lnwage male, quantile (0.50) 
qreg lnwage male, quantile (0.75) 
qreg lnwage male, quantile (0.90) 


************************************************************************************ 
 *** Question 4 *********Firpo et al (2009) RIF-OLS decomposition *****************
************************************************************************************

*** compute RIF for the 10th, 25th, 50th, 75th and 90th quantiles 
foreach qt of numlist 10 25 50 75 90 {
gen rif_`qt'=.
}

*** for the female sample
pctile eval2=lnwage if male==0 , nq(100) 
kdensity lnwage if male==0, at(eval2) gen(eval_nw dens_nw) gau width(0.0981)  nograph 

foreach qt of numlist 10 25 50 75 90 {
 local qc = `qt'/100
 replace rif_`qt'=eval_nw[`qt']+`qc'/dens_nw[`qt'] if lnwage>=eval_nw[`qt'] & male==0
 replace rif_`qt'=eval_nw[`qt']-(1-`qc')/dens_nw[`qt'] if lnwage<eval_nw[`qt']& male==0
}

*** for male sample
pctile eval1=lnwage if male==1, nq(100) 
kdensity lnwage if male==1, at(eval1) gen(eval_w dens_w) gau width(0.0867) nograph 

foreach qt of numlist 10 25 50 75 90 {
 local qc = `qt'/100
 replace rif_`qt'=eval_w[`qt']+`qc'/dens_w[`qt'] if lnwage>=eval_w[`qt'] & male==1
 replace rif_`qt'=eval_w[`qt']-(1-`qc')/dens_w[`qt'] if lnwage<eval_w[`qt']& male==1
}

*** Implement the Oaxaca decomposition using rif_`qt' as dependent variable

foreach qt of numlist 10 25 50 75 90 {
dis `qt'

sum rif_`qt' if male==1
gen qlwm`qt'=r(mean)
sum rif_`qt' if male==0
gen qlwf`qt'=r(mean)
gen difq`qt'= qlwm`qt'-qlwf`qt'
display difq`qt'

oaxaca rif_`qt' educ exper expersq tenure married divorced, by(female) weight(1) robust nodetail



}


 