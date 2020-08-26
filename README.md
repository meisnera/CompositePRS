# CompositePRS

Code and data for composite PRS analysis (Meisner et al., AJHG, 2020)

### allsnp_weights.csv: list of SNPs and weights for disease and mortality risk factor PRS and sex-specific composite PRS analyzed in Meisner et al. (AJHG, 2020)

* This dataset includes the following variables:
	+ Trait
	+ Chromosome
	+ Position
	+ RS ID
	+ Effect Allele
	+ Effect Allele Frequency
	+ Beta: estimated association between the SNP and the trait (e.g., regression coefficient); reported association is for the effect allele (used to construct the trait-specific PRS)
	+ Beta_weight_F: re-weighted Beta for each variant to construct the composite PRS described in Meisner et al. _for women_ 
	+ Beta_weight_M: re-weighted Beta for each variant to construct the composite PRS described in Meisner et al. _for men_
* Important notes:
	1. For women, the Beta_weight_F = NA when Trait = "prostate_cancer". These prostate cancer SNPs should not be included in the construct of the composite PRS for women. The same is true for Beta_weight_M and Trait = "breast_cancer" for men. 
	2. Two of the SNPs in the heart disease PRS were discovered via a recessive model: rs12976411 and rs11830157. This influences how the heart disease PRS, and thus the composite PRS, are constructed. 
		1. For rs12976411, this means that the log odds ratio (Beta) for TT vs. (AT and AA), where "T" has an allele frequency of ~0.04, is 0.4004776. So, rather than having a genotype of 0/1/2 for AA/AT/TT, the genotype (for constructing the PRS) is 0/0/1.
		2. For rs11830157, this means that the log odds ratio (Beta) for GG vs. (GT and TT) is 0.1133287. In this case, the genotype (for constructing the PRS) is 0/0/1 for TT/GT/GG.

### cPRS_primaryanalysis.R: R code for construction and evaluation of the composite PRS for the main analysis presented in Table 2 of Meisner et al. (AJHG, 2020)
