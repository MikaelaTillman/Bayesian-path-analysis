#Fig S1
library(gridExtra)
library(ggpubr)


S2<- ggarrange( (spring_time + rremove("xlab")),  (fall_time + rremove("xlab")),
                 (success_time + rremove("xlab")), (winter_dens_time + rremove("xlab")), (summer_dens_time + rremove("xlab")), labels = c("A", "B", "C", "D", "E")) 
S2 <- annotate_figure(S2, bottom = text_grob(size = 16, "Year"))
ggsave(plot = S2, device = "pdf", filename = "S2.pdf",  width = 8.5, height = 7.22)
ggsave(plot = S2,  device = "png", filename = "S2.png", width = 8.5, height = 7.22)

#Fig S3
S3 <- ggarrange(spring_prev_fall_sim, 
                 spring_winter_dens_sim, 
                 spring_max_snow_sim, 
                 nrow = 1, labels = c("A", "B", "C")) 
ggsave(plot = S3, device = "pdf", filename = "S3.pdf")
ggsave(plot = S3, device = "png", filename = "S3.png", width = 8.99, height = 3.61)

#Fig S4
S4 <- ggarrange(succ_winter_dens_sim, 
                 succ_snow_sim,
                 succ_spring_sim,
                 succ_spr_sim, 
                 succ_max_hundred_sim, labels = c("A", "B", "C", "D", "E"))
ggsave(plot = S4, device = "pdf", filename = "S4.pdf")
ggsave(plot = S4, device = "png", filename = "S4.png", width = 8.99, height = 7.22)

#Fig S5
S5 <- ggarrange(fall_prev_fall_sim,
                 fall_summer_dens_sim,
                 fall_snow_sim,
                 fall_spring_sim,
                 fall_spr_sim,
                 fall_max_sim, labels = c("A", "B", "C", "D", "E", "F"))
ggsave(plot = S5, device = "pdf", filename = "S5.pdf")
ggsave(plot = S5, device = "png", filename = "S5.png", width = 8.99, height = 7.22)

#Fig S6
S6 <- ggarrange((spring_success_plot  + rremove("xlab")), (fall_success_plot +  rremove("xlab")), 
                   (winter_success_plot + rremove("xlab")), (summer_success_plot + rremove("xlab")),  common.legend = TRUE,
                 labels = c("A", "B", "C", "D"))
S6 <- annotate_figure(S6,bottom = text_grob(size = 16, bquote(Population~density~winter[t]*~(ind./km^2))))
ggsave(plot = S6, device = "pdf", filename = "S6.pdf", width = 9.99, height = 7.22)
ggsave(plot = S6, device = "png", filename = "S6.png", width = 8.99, height = 7.22)

#Fig S7
S7 <- ggarrange((winter_time + rremove("xlab")), (summer_time + rremove("xlab")),
                  (max_snow_m_time + rremove("xlab")), (spr_time + rremove("xlab")),
                   (max_hundred_time + rremove("xlab")), labels = c("A", "B", "C", "D", "E", "F"))
S7 <- annotate_figure(S7, bottom = text_grob(size = 16, bquote("Year")))
ggsave(plot = S7, device = "pdf", filename = "S7.pdf",  width = 9, height = 7.22)
ggsave(plot = S7, device = "png", filename = "S7.png",  width = 9, height = 7.22)

#Fig S8
S8 <- grid.arrange(est_spring, est_spring_abs,
                          est_success, est_success_abs,
                          est_fall, est_fall_abs,
                          nrow = 3)
S8 <- annotate_figure(S8, bottom = text_grob(size = 16, bquote("Effect size")))
ggsave(plot = S8, device = "pdf", filename = "S8.pdf", width = 10, height = 7.22)
ggsave(plot = S8, device = "png", filename = "S8.png", width = 10, height = 7.22)

#Fig S9
S9 <- ggarrange(spring_prev_fall_partial_plot,
                                 spring_winter_dens_partial_plot,
                                 spring_snow_partial_plot, nrow = 1, labels = c("A", "B", "C"))
ggsave(plot = S9, device = "pdf", filename = "S9.pdf", width = 12, height = 3.5)
ggsave(plot = S9, device = "png", filename = "S9.png", width = 12, height = 3.5)

#Fig S10
S10 <- ggarrange(succ_winter_dens_plot, succ_snow_plot, 
                                  succ_spring_plot, succ_spr_plot,
                                  succ_max_plot,  labels = c("A", "B", "C", "D", "E"))
ggsave(plot = S10, device = "pdf", filename = "S10.pdf",  width = 12, height = 7)
ggsave(plot = S10, device = "png", filename = "S10.png", width = 12, height = 7)

#Fig S11
S11 <- ggarrange(indirect_plot_success, indirect_plot_success_abs, nrow = 2,  labels = c("A", "B"))
ggsave(plot = S11, device = "pdf", filename = "S11.pdf", width = 10, height = 4)
ggsave(plot = S11, device = "png", filename = "S11.png", width = 10, height = 4)

#Fig S12 
S12 <- ggarrange(fall_prev_fall_partial_plot,
                               fall_summer_dens_partial_plot,
                               fall_max_snow_m_partial_plot,
                               fall_spring_partial_plot,
                               fall_spr_partial_plot,
                               fall_max_hundred_partial_plot, labels = c("A", "B", "C", "D", "E", "F"))
ggsave(plot = S12, device = "pdf", filename = "S12.pdf", width = 12, height = 7)
ggsave(plot = S12, device = "png", filename = "S12.png", width = 12, height = 7)

#Fig S13
S13 <- ggarrange(indirect_plot_fall, indirect_plot_fall_abs, nrow = 2, labels = c("A", "B"))
ggsave(plot = S13, device = "pdf", filename = "S13.pdf", width = 10, height = 10)
ggsave(plot = S13, device = "png", filename = "S13.png", width = 10, height = 10)


data %>% 
  ggplot(aes(summer_dens, winter_dens)) + geom_point()
