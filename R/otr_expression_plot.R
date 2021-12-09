#' plot otr expression
#' It is showing downregulation of OTR in the supracocmissural part of the ventral telencephalo in solitary species
#' @param model
#' @param data
#'
#' @return
#' @export
#'
#' @examples
otr_expression_plot = function(model,data){

  # ITR == mammalian OTR; isotocin receptor vs. oxytocin receptor - this is not stated in the manuscript -GP
  # slight deviations between plots if you run them multiple times (from random sampling) but same overall trend - GP
  spp_order<-c("C.vag", "C.lun", "C.bar", "C.trif", "C.rainf", "C.pleb")
  #extract ITR data only
  itr_plot = MCMC.qpcr::HPDsummary(model,data,xgroup="species", x.order=spp_order,genes="ITR")

  #change ITR to OTR for all entries and shorten abbreviations to reflect manuscript figure
  itr_plot$ggPlot$data = itr_plot$ggPlot$data %>%
    dplyr::mutate(species = dplyr::case_when(species == "C.vag" ~ "C.vag.",
                                             species == "C.lun" ~ "C.lun.",
                                             species == "C.bar" ~ "C.bar.",
                                             species == "C.trif" ~ "C.tri.",
                                             species == "C.rainf" ~ "C.rai.")) %>%
    dplyr::mutate(gene = "OTR", species = factor(species, levels = c("C.vag.","C.lun.","C.bar.","C.tri.","C.rai.")))

  # change wisker width
  itr_plot$ggPlot[["layers"]][[1]][["geom_params"]][["width"]] = 0.1

  # remove geom_line from plot
  itr_plot$ggPlot[["layers"]] = itr_plot$ggPlot[["layers"]][-2]

  # create separate df for labels
  abc_labels <- itr_plot$ggPlot$data %>%
    dplyr::arrange(species) %>%
    dplyr::mutate(labels = c("a", "ab", "ab", "bc", "c"))

  # modify plot elements to reflect figure in manuscript
  p = itr_plot$ggPlot +
    ggplot2::labs(title="Vs", y=expression(paste("mRNA (log"[2], " abundance)"))) +
    # scale_x_discrete(labels = as.character(xlab_vec)) +
    ggplot2::scale_y_continuous(limits=c(-500,50)) +
    ggplot2::scale_color_manual(values=c("#bc74bc")) +
    ggplot2::theme(panel.grid.major=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(color = "black",size=0.8),
                   plot.margin = ggplot2::margin(t=2.1,1,4,1, "lines"),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position=c(0.5,1.1),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size=12),
                   text=ggplot2::element_text(family="serif"),
                   axis.text.x.bottom=ggplot2::element_text(face = "italic",size = 12),
                   axis.text.y.left = ggplot2::element_text(size = 12),
                   axis.title.y = ggplot2::element_text(size = 12),
                   axis.title.x.bottom = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::geom_text(aes(x = species, y = lower - 15, label = labels),
                       abc_labels,
                       family = "serif",
                       color = "black") +
    ggsignif::geom_signif(ggplot2::aes(species, mean),
                          comparisons = list(c(1, 3),
                                             c(4, 5),
                                             c(2, 4.5)),
                          y_position = c(0, 0, 15),
                          inherit.aes = FALSE,
                          color = "black",
                          annotation = c("", "", "*"),
                          size = 0.75)

  # make elements that go under the x-axis
  p2 = p +
    ggplot2::annotation_custom(grid::textGrob("(6)", gp=grid::gpar(fontfamily="serif")), ymin = -1225, ymax = 50, xmin = 1, xmax = 1) +
    ggplot2::annotation_custom(grid::textGrob("(5)", gp=grid::gpar(fontfamily="serif")), ymin = -1225, ymax = 50, xmin = 2, xmax = 2) +
    ggplot2::annotation_custom(grid::textGrob("(7)", gp=grid::gpar(fontfamily="serif")), ymin = -1225, ymax = 50, xmin = 3, xmax = 3) +
    ggplot2::annotation_custom(grid::textGrob("(2)", gp=grid::gpar(fontfamily="serif")), ymin = -1225, ymax = 50, xmin = 4, xmax = 4) +
    ggplot2::annotation_custom(grid::textGrob("(2)", gp=grid::gpar(fontfamily="serif")), ymin = -1225, ymax = 50, xmin = 5, xmax = 5) +
    ggplot2::annotation_custom(grid::textGrob("pb", gp=grid::gpar(fontfamily="serif")), ymin = -1350, ymax = 50, xmin = 2, xmax = 2) +
    ggplot2::annotation_custom(grid::textGrob("sol", gp=grid::gpar(fontfamily="serif")), ymin = -1350, ymax = 50, xmin = 4.5, xmax = 4.5) +
    ggplot2::annotation_custom(grid::linesGrob(gp=grid::gpar(lwd=2)), xmin = 0.9, xmax = 3.1, ymin = -625, ymax = -625) +
    ggplot2::annotation_custom(grid::linesGrob(gp=grid::gpar(lwd=2)), xmin = 3.9, xmax = 5.1, ymin = -625, ymax = -625)
  gg_table <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p2))
  gg_table$layout$clip[gg_table$layout$name=="panel"] <- "off"
  grid::grid.draw(gg_table)

}


