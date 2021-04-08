#Heatmap
library(pheatmap)
library(RColorBrewer)
library(viridis)

#Read the correlation results
Corr_all <- read.csv("/COVID19_IDPH/Correlation_result.csv",header=T,as.is = T)
#Change the order of columns
Corr_all <- Corr_all[c("ACS.PE.variable","Age1_corr","Age2_corr","Age3_corr","Age4_corr",
                       "Age5_corr","Age6_corr","Age7_corr","Age8_corr","group","Short_name")]

jpeg("/COVID19_IDPH/Heatmap_corr.jpeg", width = 3000, height =5000,res=600)

group <- Corr_all$group
rnames<-Corr_all$Short_name
cnames<- colnames(Corr_all)[2:9]
Corr_all <- Corr_all[,2:9]
Corr_matrix <- data.matrix(Corr_all)
rownames(Corr_matrix) <- rnames
colnames(Corr_matrix) <- cnames

#Set color gradient
my_palette <- colorRampPalette(colors = c("darkblue", "red"))
breaks <- seq(from=min(range(Corr_matrix)), to=max(range(Corr_matrix)), length.out=100)
midpoint <- which.min(abs(breaks - 0))
rampCol1 <- colorRampPalette(c("darkblue", "grey"))(midpoint)
rampCol2 <- colorRampPalette(c("grey", "red"))(100-(midpoint+1))
rampCols <- c(rampCol1,rampCol2)

pheatmap(
  mat             = Corr_matrix,
  color           = rampCols,
  border_color    = NA,
  drop_levels     = TRUE,
  fontsize        = 1.2,
  labels_row      = rnames,
  show_rownames   = T,
  treeheight_row  = 0, treeheight_col = 0,
  cellwidth = 30, cellheight = 1,
  cluster_cols=F, cluster_rows=F
)
dev.off()

