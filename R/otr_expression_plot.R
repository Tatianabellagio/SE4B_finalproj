#' plot otr expression
#'
#' @param model
#' @param data
#'
#' @return
#' @export
#'
#' @examples
otr_expression_plot = function(model,data){

  ## libraries needed
  #library(dplyr)
  #library(ggsignif)
  #library(grid)
  #library(gridExtra)

  spp_order<-c("C.vag", "C.lun", "C.bar", "C.trif", "C.rainf", "C.pleb")
  itr_plot = HPDsummary(model,data,xgroup="species", x.order=spp_order,genes="ITR")

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
}
