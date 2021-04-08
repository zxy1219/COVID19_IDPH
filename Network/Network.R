###### Draw network graphs in R

library(tidyverse)
library(tidygraph)
library(ggraph)
library(Gmisc)
library(gridExtra)
library(pROC)
library(gtable)

#Read data for network
links_w <- read.table("/COVID19_IDPH/Network/Network_whole.txt",header=T,as.is = T,sep = ";")

#Age <20
nodes <- read.table("/COVID19_IDPH/Network/Network_age1_Nodes.txt",header=T,as.is = T,sep = ";")
links_w2 <- subset(links_w, links_w$Short_name_row %in% nodes$Short_name)
links_w3 <- subset(links_w2, links_w2$Short_name_col %in% nodes$Short_name)

links <- links_w3[,c("Short_name_row","Short_name_col","abs_cor","cor_d")]

#Remove nodes without links
link_name <- links %>% 
  select(Short_name_row, Short_name_col) %>% 
  t %>% c %>% unique

nodes2 <- subset(nodes,nodes$Short_name %in% link_name )

#Save the unconnected nodes as annotation table
nodes_discon <- subset(nodes,! nodes$Short_name %in% link_name )
nodes_discon$Pvalue <- round(abs(log10(nodes_discon$Age_adjust)),0)
nodes_discon$OR <- round(nodes_discon$Age_s,2)  

colnames(nodes_discon)[colnames(nodes_discon)=="Short_name"] <- "Variable"

myvars <- c("Variable","OR","Pvalue")
nodes_age20 <- nodes_discon[myvars]

colnames(nodes_age20)[colnames(nodes_age20)=="Pvalue"] <-"-Log10(P)"
colnames(nodes_age20)[colnames(nodes_age20)=="OR"] <-"IR"

nodes2$log_p <- 0-log10(nodes2$Age_adjust)

routes_tidy1 <- tbl_graph(nodes = nodes2, edges = links, directed = TRUE)

#Age <20 graph
tsize <- ttheme_default(base_size = 7, base_colour = "black")

g1 <- ggraph(routes_tidy1, layout = 'kk') + 
  geom_edge_link(aes(colour  = factor(cor_d)), alpha = 1) + 
  geom_node_point(aes(size  = log_p,colour=Age_s),pch=16) + scale_size(range = c(2,20), name="Poisson regression:\n-Log10(P)")+
  scale_color_gradient2(name="Poisson regression:\nIR (1 SD)",midpoint=1, low="green", mid="white", high="red", space ="Lab" )+
  geom_node_text(aes(label = Short_name), repel = TRUE,check_overlap =T) +
  scale_edge_colour_discrete(name="Correlation Between:\nACS Variables",
                             breaks=c("n","p"),
                             labels=c("Negative","Positive"))+
  guides(colour = guide_colorbar(order = 1),
         size = guide_legend(order = 2,override.aes = list(size = c(1,2,4,6,8))))+
  theme(legend.position = "right" )+
  ggtitle("A) Age <20") +
  theme_graph()+
  annotation_custom(tableGrob(nodes_age20,theme = tsize), xmin=4, xmax=0, ymin=0, ymax=0)

jpeg("/COVID19_IDPH/Network/Age1_network.jpg", width = 1100, height =700,res=110)
g1
dev.off()

setEPS()
postscript("/COVID19_IDPH/Network/Age1_network.eps",width =10, height =7,fonts = "sans")
g1
dev.off()

