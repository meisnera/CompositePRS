# CompositePRS

Code and data for composite PRS analysis (Meisner et al., AJHG, 2020)

### allsnp_weights.csv: list of SNPs and weights for disease and mortality risk factor PRS analyzed in Meisner et al. (AJHG, 2020)

* This dataset includes the following variables:
	+ Trait
	+ Chromosome
	+ Position
	+ RS ID
	+ Effect Allele
	+ Effect Allele Frequency
	+ Beta: estimated association between the SNP and the trait (e.g., regression coefficient); reported association is for the effect allele
	+ Beta_weight_F: re-weighted beta for calculating composite PRS described in Meisner et al. _for women_ 
	+ Beta_weight_M: re-weighted beta for calculating composite PRS described in Meisner et al. _for men_

### cPRS_primaryanalysis.R: R code for construction and evaluation of the composite PRS for the main analysis presented in Table 2 of Meisner et al. (AJHG, 2020)
