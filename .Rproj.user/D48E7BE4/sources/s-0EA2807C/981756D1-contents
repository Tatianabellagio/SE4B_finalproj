library(devtools)

create_package("~/Documents/soft_e_forbio/final_project_")
# 1.
#install.packages("MCMC.qpcr")
library(MCMC.qpcr)

# 2. read in data
# changed stringsAsFactors = TRUE; in R 4.x the default was changed to FALSE -GP
# this script was written before R 4.x was released - GP
data <- read.csv(file="Nowicki_C_SPP_Males_data_ESM.csv", stringsAsFactors = TRUE)
data$count <- as.numeric(data$count) # force counts to be numeric
head(data) # allows you to view your data to ensure it was imported correctly
levels(data$sex) # shows you all levels of the sex factor, to sanity check the data set looks correct

# 3. sub-set data into different brain regions and store them in separate data frames, in order to analyze each brain region separately
levels(data$region_mammal) #shows names of brain region levels
Hipp=subset(data, region_mammal=="Hipp") #makes a dataframe for Hipp
blAMY=subset(data, region_mammal=="blAMY")
LS=subset(data, region_mammal=="LS")
meAMY.BNST=subset(data, region_mammal=="meAMY.BNST")
NAcc=subset(data, region_mammal=="NAcc")
POA=subset(data, region_mammal=="POA")
Str.CP=subset(data, region_mammal=="Str.CP")
VTA=subset(data, region_mammal=="VTA")



# 4. model fitting
# had to change variable to factor -GP
meAMY.BNST$species = relevel(factor(meAMY.BNST$species), ref='C.bar') #sets C.bar as "reference" for species factor
meAMY.BNST_naive_model=mcmc.qpcr(data=meAMY.BNST, fixed="species", random =
                                   "individual", nitt=510000,thin=500,burnin=10000, pr=TRUE)
summary(meAMY.BNST_naive_model) # this shows results from 1-way fixed effects
meAMY.BNST_HPDsumm_naive_model=HPDsummary(meAMY.BNST_naive_model,meAMY.BNST, relative=T) #eyeballametrically determine whether there are global effects.

# 5. decision time
## if no global effects are present in naive model, try sharpening credible inervals by running an "informed" model instead.
## if global effects ARE present in naive model, then fit a "soft normalization" model (pg. 30 Matz tutorial, step 10)
### testing whether informed model sharpens credible intervals more than naive model:

# I think they started capitalizing meAMY.BNST, will briefly change to lowercase to test -GP

MeAMY.BNST_naive_model = mcmc.qpcr(data=meAMY.BNST, fixed="species", random =
                                     "individual", controls=c("r18S"), include=0, nitt=510000,thin=500,burnin=10000) #runs naive model
MeAMY.BNST_informed_model = mcmc.qpcr(data=meAMY.BNST, fixed="species", random =
                                        "individual", controls=c("r18S"), m.fix=1.2, nitt=510000,thin=500,burnin=10000) #runs informed model
HPDplot(model=MeAMY.BNST_naive_model, factors="speciesC.lun", main="C.lun", hpdtype="l") #plots credible interval of naive model
HPDpoints(model=MeAMY.BNST_informed_model, factors="speciesC.lun", hpdtype="l", col="coral") #plots credible interval difference between naive and informed

# mdae meAMY.BNST lowercase here too -GP

# 6. In the best fit model, conduct pair-wise comparisons of p-values
MeAMY.BNST_spp_informed_pwp = HPDsummary(model=MeAMY.BNST_informed_model, data=meAMY.BNST) # shows absolute abundances
MeAMY.BNST_spp_informed_pwp$summary # retrieves bundles of data that can be used for more plotting (means, sds, CIs)
MeAMY.BNST_spp_informed_pwp$geneWise # calculates pairwise difference between treatments and their statistical significances; upper triangle is log fold changes, lower triangle is the corresponding p-values

#capitalized meAMY.BNST_informed_model here -GP
# 7. plot results

spp_order<-c("C.vag", "C.lun", "C.bar", "C.trif", "C.rainf", "C.pleb")

plot_meAMY.BNST_informed=HPDsummary(MeAMY.BNST_informed_model,meAMY.BNST,xgroup="species", x.order=spp_order) # trellis plot of all genes in same panel
trellisByGene(plot_meAMY.BNST_informed, xFactor="species", groupFactor =
                "species")+xlab("species") # trellis plots of genes in separate panels
#end
