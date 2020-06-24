# CompositePRS

Code and data for composite PRS analysis (Meisner et al., AJHG, 2020+)

### SNPinfo.csv: list of SNPs and weights for disease and mortality risk factor PRS analyzed in Meisner et al. (AJHG, 2020+)

* This dataset includes the following variables:
	+ Trait
	+ Chromosome
	+ Position
	+ RS ID
	+ Effect Allele
	+ Effect Allele Frequency
	+ Beta: estimated association between the SNP and the trait (e.g., regression coefficient); reported association is for the effect allele

### cPRS_primaryanalysis.R: R code for construction and evaluation of the composite PRS for the main analysis presented in Table 2 of Meisner et al. (AJHG, 2020+)
