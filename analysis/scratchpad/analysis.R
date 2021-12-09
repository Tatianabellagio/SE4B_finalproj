# this should be install autmaticalley?
library(devtools)
# cargar el paquete
load_all()
data = data('butter_neuro')
### this might me needed
colClasses=c("count"="numeric")
## if i add this this package should be installed automaticalley right?
use_package("MCMC.qpcr")

#we need to know which particular things inside tidyverse we are using
use_package("tidyverse")

library(tidyverse)
library(ggsignif)
library(grid)
library(gridExtra)
# read in data


# 3. sub-set data into different brain regions and store them in separate data
# frames, in order to analyze each brain region separately
#levels(data$region_mammal) #shows names of brain region levels

subsets = split(data, data$region_mammal)
list2env(subsets, envir = .GlobalEnv)

# 4. model fitting
# had to change variable to factor -GP
meAMY.BNST$species = relevel(factor(meAMY.BNST$species), ref='C.bar') #sets C.bar as "reference" for species factor


#meAMY.BNST_naive_model=mcmc.qpcr(data=meAMY.BNST, fixed="species", random =
#                                   "individual", nitt=510000,thin=500,burnin=10000, pr=TRUE)
#summary(meAMY.BNST_naive_model) # this shows results from 1-way fixed effects
#meAMY.BNST_HPDsumm_naive_model=HPDsummary(meAMY.BNST_naive_model,meAMY.BNST, relative=T)
#eyeballametrically determine whether there are global effects.

# 5. decision time
## if no global effects are present in naive model, try sharpening credible inervals by running an "informed" model instead.
## if global effects ARE present in naive model, then fit a "soft normalization" model (pg. 30 Matz tutorial, step 10)
### testing whether informed model sharpens credible intervals more than naive model:

# I think they started capitalizing meAMY.BNST, will briefly change to lowercase to test -GP

#MeAMY.BNST_naive_model = mcmc.qpcr(data=meAMY.BNST, fixed="species", random =
#                                     "individual", controls=c("r18S"), include=0, nitt=510000,thin=500,burnin=10000) #runs naive model
MeAMY.BNST_informed_model = mcmc.qpcr(data=meAMY.BNST, fixed="species", random =
                                        "individual", controls=c("r18S"), m.fix=1.2, nitt=510000,thin=500,burnin=10000) #runs informed model
#HPDplot(model=MeAMY.BNST_naive_model, factors="speciesC.lun", main="C.lun", hpdtype="l") #plots credible interval of naive model
#HPDpoints(model=MeAMY.BNST_informed_model, factors="speciesC.lun", hpdtype="l", col="coral") #plots credible interval difference between naive and informed

# mdae meAMY.BNST lowercase here too -GP

# 6. In the best fit model, conduct pair-wise comparisons of p-values
MeAMY.BNST_spp_informed_pwp = HPDsummary(model=MeAMY.BNST_informed_model, data=meAMY.BNST) # shows absolute abundances
#MeAMY.BNST_spp_informed_pwp$summary # retrieves bundles of data that can be used for more plotting (means, sds, CIs)
#MeAMY.BNST_spp_informed_pwp$geneWise # calculates pairwise difference between treatments and their statistical significances; upper triangle is log fold changes, lower triangle is the corresponding p-values

#capitalized meAMY.BNST_informed_model here -GP

# 7. plot results
spp_order<-c("C.vag", "C.lun", "C.bar", "C.trif", "C.rainf", "C.pleb")
plot_meAMY.BNST_informed=HPDsummary(MeAMY.BNST_informed_model,meAMY.BNST,xgroup="species", x.order=spp_order) # trellis plot of all genes in same panel
trellisByGene(plot_meAMY.BNST_informed, xFactor="species", groupFactor =
                "species")+xlab("species") # trellis plots of genes in separate panels
#end

###############################################################
# finalizing plot to look as close to plot in pub as possible #
###############################################################


# ITR == mammalian OTR; isotocin receptor vs. oxytocin receptor - this is not stated in the manuscript -GP
# slight deviations between plots if you run them multiple times (from random sampling) but same overall trend - GP

#extract ITR data only
itr_plot = HPDsummary(MeAMY.BNST_informed_model,meAMY.BNST,xgroup="species", x.order=spp_order,genes="ITR")

#change ITR to OTR for all entries and shorten abbreviations to reflect manuscript figure
itr_plot$ggPlot$data = itr_plot$ggPlot$data %>%
  mutate(species = case_when(species == "C.vag" ~ "C.vag.",
                             species == "C.lun" ~ "C.lun.",
                             species == "C.bar" ~ "C.bar.",
                             species == "C.trif" ~ "C.tri.",
                             species == "C.rainf" ~ "C.rai.")) %>%
  mutate(gene = "OTR", species = factor(species, levels = c("C.vag.","C.lun.","C.bar.","C.tri.","C.rai.")))

# change wisker width
itr_plot$ggPlot[["layers"]][[1]][["geom_params"]][["width"]] = 0.1

# remove geom_line from plot
itr_plot$ggPlot[["layers"]] = itr_plot$ggPlot[["layers"]][-2]

# create separate df for labels
abc_labels <- itr_plot$ggPlot$data %>%
  arrange(species) %>%
  mutate(labels = c("a", "ab", "ab", "bc", "c"))

# modify plot elements to reflect figure in manuscript
p = itr_plot$ggPlot +
  labs(title="Vs", y=expression(paste("mRNA (log"[2], " abundance)"))) +
  # scale_x_discrete(labels = as.character(xlab_vec)) +
  scale_color_manual(values=c("#bc74bc")) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_rect(color = "black",size=0.8),
        plot.margin = margin(t=2,1,4,1, "lines"),
        plot.title = element_text(hjust = 0.5),
        legend.position=c(0.5,1.1),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        text=element_text(family="serif"),
        axis.text.x.bottom=element_text(face = "italic",size = 12),
        axis.text.y.left = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x.bottom = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(x = species, y = lower - 15, label = labels),
            abc_labels,
            family = "serif",
            color = "black") +
  geom_signif(aes(species, mean),
              comparisons = list(c("C.vag.","C.bar."), c("C.tri.","C.rai.")),
              inherit.aes = FALSE,
              color = "black",
              annotation = "",
              size = 0.75)
p

# make elements that go under the x-axis
p2 = p +
  annotation_custom(textGrob("(6)", gp=gpar(fontfamily="serif")), ymin = -1015, ymax = 50, xmin = 1, xmax = 1) +
  annotation_custom(textGrob("(5)", gp=gpar(fontfamily="serif")), ymin = -1015, ymax = 50, xmin = 2, xmax = 2) +
  annotation_custom(textGrob("(7)", gp=gpar(fontfamily="serif")), ymin = -1015, ymax = 50, xmin = 3, xmax = 3) +
  annotation_custom(textGrob("(2)", gp=gpar(fontfamily="serif")), ymin = -1015, ymax = 50, xmin = 4, xmax = 4) +
  annotation_custom(textGrob("(2)", gp=gpar(fontfamily="serif")), ymin = -1015, ymax = 50, xmin = 5, xmax = 5) +
  annotation_custom(textGrob("pb", gp=gpar(fontfamily="serif")), ymin = -1080, ymax = 50, xmin = 2, xmax = 2) +
  annotation_custom(textGrob("sol", gp=gpar(fontfamily="serif")), ymin = -1080, ymax = 50, xmin = 4.5, xmax = 4.5) +
  annotation_custom(linesGrob(gp=gpar(lwd=2)), xmin = 0.9, xmax = 3.1, ymin = -500, ymax = -500) +
  annotation_custom(linesGrob(gp=gpar(lwd=2)), xmin = 3.9, xmax = 5.1, ymin = -500, ymax = -500)
gg_table <- ggplot_gtable(ggplot_build(p2))
gg_table$layout$clip[gg_table$layout$name=="panel"] <- "off"
grid.draw(gg_table)




