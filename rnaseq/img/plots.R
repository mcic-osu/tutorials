## Volcano
p <- EnhancedVolcano(
  toptable = res_rc24,      
  title = "relictum vs. control at 24 hpi",
  x = "log2FoldChange",     
  y = "padj",             
  lab = rownames(res_rc24), 
  labSize = 0,               # Now we will show the gene labels
  pCutoff = 0.05,          # Modify the p-value cut-off
  subtitle = NULL,           # Remove the subtitle
  caption = NULL,            # Remove the caption
  xlim = c(-10, 10),
  ylim = c(0, 25),
  col = c("grey50", "grey50", "grey50", "red")
)
p +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))
ggsave("lectures/img_rnaseq/volcano.png", width = 7, height = 6)

## Gene plot
top25_DE <- row.names(res_rc24[order(res_rc24$padj)[1:25], ])

focal_gene_counts <- plotCounts(
  dds,
  gene = top25_DE[1],
  intgroup = c("time", "treatment"),
  returnData = TRUE
)

focal_gene_counts |>
  filter(time == "24hpi", treatment %in% c("control", "relictum")) |> 
ggplot(aes(x = treatment, y = count, fill = treatment)) +
  facet_wrap(vars(time)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_point(size = 4, shape = 21,
             position = position_jitter(w = 0.1, h = 0)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none")
ggsave("lectures/img_rnaseq/geneplot.png", width = 7, height = 6)
