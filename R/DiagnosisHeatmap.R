#' Visualize the Diagnosis Heatmap
#'
#' @param No empty
#' @return
#'
#' @export 
#'
#' @examples DiagnosisHeatmap()
DiagnosisHeatmap <- function() {
library(pheatmap)
library(gtable)
library(grid)
library(data.table)

# test = matrix(rnorm(30), 2, 15)
# test[1:10, seq(1, 15, 2)] = test[1:10, seq(1, 15, 2)] + 3
# test[11:20, seq(2, 15, 2)] = test[11:20, seq(2, 15, 2)] + 2
# test[15:20, seq(2, 15, 2)] = test[15:20, seq(2, 15, 2)] + 4
# colnames(test) = paste("Test", 1:15, sep = "")
# rownames(test) = paste("Gene", 1:20, sep = "")
# head(test)

# data<-read.table("gene_exp_significant-3G.xls", row.names=1, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
# data<-fread(system.file("extdata", "gene_exp_significant-3G.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=FALSE,data.table=FALSE)
data<-read.table(system.file("extdata", "gene_exp_significant-3G.xls", package = "DiagnosisHeatmap"), row.names=1, header = TRUE, stringsAsFactors = FALSE, sep = "\t")

# data <- as.matrix(data)
# dex <- data
# sd2 <- apply(dex, 1, sum)
# dex <- dex[sd2 != 0,]
# sd2 <- apply(dex, 1, sd)
# dex <- dex[sd2 != 0,]
# mns <- apply(dex, 1, mean)
# dex[,1] <- dex[,1] - mns
# dex[,2] <- dex[,2] - mns
# data<-dex


# data<-data[order(data$Pvalue,decreasing=FALSE),]
# data<-data[,-1*((length(colnames(data))-4):length(colnames(data)))]

# data<-head(data,n=30)

if(length(rownames(data))>60){
    x=40
}else{
    x=length(rownames(data))
}
t=data
n=nrow(t)
a=sample(n,x,replace=F)
tt=t[a,]
write.table(tt,"sample.peakCenter.heatmap.xls",sep="\t",quote = F,row.names=T,col.names=T)
data<-tt


# color 参数自定义颜色
# pheatmap(data, scale='row', color = colorRampPalette(c("navy", "white", "firebrick3"))(50))

# 带有附加信息
# 构建列注释信息 - 由后到前进行显示
# annotation_col = data.frame(
#   OncocyticTumor=c("No","Yes"),
#   # Diagnosis = colnames(data), 
#   TranscriptomeCluster = factor(rep(c("con3h", "oe3h", "oe6h"), each=4))
# )
# rownames(annotation_col) = c("con3h_1", "con3h_2", "con3h_3", "con3h_4", "oe3h_1", "oe3h_2", "oe3h_3", "oe3h_4", "oe6h_1", "oe6h_2", "oe6h_3", "oe6h_4")
# head(annotation_col)
##       CellType Time
## Test1      CT1    1
## Test2      CT2    2
## Test3      CT1    3
## Test4      CT2    4
## Test5      CT1    5
## Test6      CT2    1
# library(data.table)

# 分组的注释信息总表
# annotation_col<-fread("Clusters.xls",header=TRUE,stringsAsFactors=F,data.table=F)
annotation_col<-fread(system.file("extdata", "Clusters.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=FALSE,data.table=FALSE)

rownames(annotation_col)<-annotation_col[,1]
annotation_col[,1]<-NULL
annotation_col

# 各分组颜色标注的小分组颜色信息
# TranscriptomeCluster
# TranscriptomeCluster<-fread("TranscriptomeCluster.xls",header=TRUE,stringsAsFactors=F,data.table=F)
TranscriptomeCluster<-fread(system.file("extdata", "TranscriptomeCluster.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=FALSE,data.table=FALSE)
names<-colnames(TranscriptomeCluster)
TranscriptomeCluster<-as.character(TranscriptomeCluster)
names(TranscriptomeCluster)<-names
TranscriptomeCluster

# Diagnosis
# Diagnosis<-fread("Diagnosis.xls",header=TRUE,stringsAsFactors=F,data.table=F)
Diagnosis<-fread(system.file("extdata", "Diagnosis.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=FALSE,data.table=FALSE)
names<-colnames(Diagnosis)
Diagnosis<-as.character(Diagnosis)
names(Diagnosis)<-names
Diagnosis

# OncocyticTumor
# OncocyticTumor<-fread("OncocyticTumor.xls",header=TRUE,stringsAsFactors=F,data.table=F)
OncocyticTumor<-fread(system.file("extdata", "OncocyticTumor.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=FALSE,data.table=FALSE)
names<-colnames(OncocyticTumor)
OncocyticTumor<-as.character(OncocyticTumor)
names(OncocyticTumor)<-names
OncocyticTumor

# WellsScore
# WellsScore<-fread("WellsScore.xls",header=TRUE,stringsAsFactors=F,data.table=F)
WellsScore<-fread(system.file("extdata", "WellsScore.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=FALSE,data.table=FALSE)
names<-colnames(WellsScore)
WellsScore<-as.character(WellsScore)
names(WellsScore)<-names
WellsScore

# ENSATStage
# ENSATStage<-fread("ENSATStage.xls",header=TRUE,stringsAsFactors=F,data.table=F)
ENSATStage<-fread(system.file("extdata", "ENSATStage.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=FALSE,data.table=FALSE)
names<-colnames(ENSATStage)
ENSATStage<-as.character(ENSATStage)
names(ENSATStage)<-names
ENSATStage

# KI67Index
# KI67Index<-fread("KI67Index.xls",header=TRUE,stringsAsFactors=F,data.table=F)
KI67Index<-fread(system.file("extdata", "KI67Index.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=FALSE,data.table=FALSE)
names<-colnames(KI67Index)
KI67Index<-as.character(KI67Index)
names(KI67Index)<-names
KI67Index

# Death
# Death<-fread("Death.xls",header=TRUE,stringsAsFactors=F,data.table=F)
Death<-fread(system.file("extdata", "Death.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=FALSE,data.table=FALSE)
names<-colnames(Death)
Death<-as.character(Death)
names(Death)<-names
Death

# Prognosis
# Prognosis<-fread("Prognosis.xls",header=TRUE,stringsAsFactors=F,data.table=F)
Prognosis<-fread(system.file("extdata", "Prognosis.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=FALSE,data.table=FALSE)
names<-colnames(Prognosis)
Prognosis<-as.character(Prognosis)
names(Prognosis)<-names
Prognosis

ann_colors = list(
  # ImmuneScore = c("#4071b2", "#fafdc7", "#d62e25"),
  # AdrenalScore = c("#4071b2", "#fafdc7", "#d62e25"),
  # Proliferation = c("#4071b2", "#fafdc7", "#d62e25"),
  # NMFRank3 = c("#4071b2", "#fafdc7", "#d62e25"),
  # NMFRank2 = c("#4071b2", "#fafdc7", "#d62e25"),
  # NMFRank1 = c("#4071b2", "#fafdc7", "#d62e25"),
  Prognosis=Prognosis,
  Death=Death,
  KI67Index=KI67Index,
  ENSATStage=ENSATStage,
  WellsScore=WellsScore,
  # OncocyticTumor = c(No="white", Yes="#fea400"),
  OncocyticTumor = OncocyticTumor,
  Diagnosis = Diagnosis,
  TranscriptomeCluster = TranscriptomeCluster
)

# annotation_col参数添加列注释信息
# pheatmap(data, scale='row', annotation_col = annotation_col, annotation_colors = ann_colors, main = "HeatMap")
heat<-pheatmap(
  data, 
	# clustering_distance_cols="minkowski", 
	# clustering_distance_rows = "euclidean",
  annotation_col = annotation_col, 
  annotation_colors = ann_colors,
	cutree_col=3
  )

# pdf("heatmap.pdf",height=16,width=10)
# print(heat)
# dev.off()

# heat$gtable
#   z     cells                 name                         grob
# 1 1 (2-2,3-3)             col_tree polyline[GRID.polyline.1038]
# 2 2 (4-4,1-1)             row_tree polyline[GRID.polyline.1039]
# 3 3 (4-4,3-3)               matrix       gTree[GRID.gTree.1041]
# 4 4 (5-5,3-3)            col_names         text[GRID.text.1042]
# 5 5 (4-4,4-4)            row_names         text[GRID.text.1043]
# 6 6 (3-3,3-3)       col_annotation         rect[GRID.rect.1044]
# 7 7 (3-3,4-4) col_annotation_names         text[GRID.text.1045]
# 8 8 (3-5,6-6)    annotation_legend       gTree[GRID.gTree.1071]
# 9 9 (3-5,5-5)               legend       gTree[GRID.gTree.1074]

# # 横向聚类树
# heat$gtable$grobs[[1]]$gp <- gpar(alpha = 0)
# # 纵向聚类树
# heat$gtable$grobs[[2]]$gp <- gpar(alpha = 0)
# # 聚类平面
# heat$gtable$grobs[[3]]$gp <- gpar(alpha = 0)
# # 样本名称
# heat$gtable$grobs[[4]]$gp <- gpar(alpha = 0)
# # 基因名称
# heat$gtable$grobs[[5]]$gp <- gpar(alpha = 0)
# # 所有的注释的 barplot
# heat$gtable$grobs[[6]]$gp <- gpar(alpha = 0)
# # 所有的注释的 名称
# heat$gtable$grobs[[7]]$gp <- gpar(alpha = 0)
# # 所有的注释的 图例
# heat$gtable$grobs[[8]]$gp <- gpar(alpha = 0)
# # 聚类的 图例
# heat$gtable$grobs[[9]]$gp <- gpar(alpha = 0)

# 样本排序
# heat$gtable$grobs[[4]]$label



# SomeScores
SomeScores<-fread(system.file("extdata", "SomeScores.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=F,data.table=F)

rownames(SomeScores)<-SomeScores[,1]
SomeScores[,1]<-NULL
SomeScores

pheat_1<-pheatmap(
  SomeScores[,heat$gtable$grobs[[4]]$label], 
  cluster_rows = FALSE, 
  cluster_cols = FALSE,
  gaps_col = c(4, 8)
  )

# pheat_1$gtable
# TableGrob (5 x 6) "layout": 4 grobs
#   z     cells      name                   grob
# 1 1 (4-4,3-3)    matrix gTree[GRID.gTree.1188]
# 2 2 (5-5,3-3) col_names   text[GRID.text.1189]
# 3 3 (4-4,4-4) row_names   text[GRID.text.1190]
# 4 4 (3-5,5-5)    legend gTree[GRID.gTree.1193]
pheat_1$gtable$grobs[[2]]$gp <- gpar(alpha = 0)
pheat_1$gtable$grobs[[4]]$gp <- gpar(alpha = 0)


# NMF
NMF<-fread(system.file("extdata", "NMF.xls", package = "DiagnosisHeatmap"),header=TRUE,stringsAsFactors=F,data.table=F)

rownames(NMF)<-NMF[,1]
NMF[,1]<-NULL
NMF

pheat_2<-pheatmap(
  NMF[,heat$gtable$grobs[[4]]$label], 
  cluster_rows = FALSE, 
   cluster_cols = FALSE, 
  gaps_col = c(4, 8)
  )

pheat_2$gtable
# TableGrob (5 x 6) "layout": 4 grobs
#   z     cells      name                   grob
# 1 1 (4-4,3-3)    matrix gTree[GRID.gTree.1188]
# 2 2 (5-5,3-3) col_names   text[GRID.text.1189]
# 3 3 (4-4,4-4) row_names   text[GRID.text.1190]
# 4 4 (3-5,5-5)    legend gTree[GRID.gTree.1193]
pheat_2$gtable$grobs[[2]]$gp <- gpar(alpha = 0)
pheat_2$gtable$grobs[[4]]$gp <- gpar(alpha = 0)

# pheat<-grid.arrange(pheat_1$gtable, pheat_2$gtable, heat$gtable)
# pheat$gtable

pheat <- gtable(
  unit(c(0.01, 0.05, 0.55, 0.06, 0.06, 0.06, 0.2, 0.01), "npc"), 
  unit(c(0.01, 0.05, 0.12, 0.01, 0.043, 0.01, 0.04, 0.01,  0.6, 0.01), "npc")
  )

# 横向聚类树
pheat <- gtable_add_grob(pheat, heat$gtable$grobs[[1]], 2, 3)
# 纵向聚类树
pheat <- gtable_add_grob(pheat, heat$gtable$grobs[[2]], 9, 2)

# 所有的注释的 名称
pheat <- gtable_add_grob(pheat, heat$gtable$grobs[[7]], 3, 4, r=6)
pheat <- gtable_add_grob(pheat, pheat_1$gtable$grobs[[3]], 5, 4, r=6)
pheat <- gtable_add_grob(pheat, pheat_2$gtable$grobs[[3]], 7, 4, r=6)
# 聚类树的基因名
pheat <- gtable_add_grob(pheat, heat$gtable$grobs[[5]], 9, 4, r=6)
# 所有的注释的 图例
pheat <- gtable_add_grob(pheat, heat$gtable$grobs[[8]], 3, 7, b=9)
# 聚类的 图例
pheat <- gtable_add_grob(pheat, heat$gtable$grobs[[9]], 9, 6)

pheat <- gtable_add_grob(pheat, heat$gtable$grobs[[6]], 3, 3)
pheat <- gtable_add_grob(pheat, pheat_1$gtable$grobs[[1]], 5, 3)

# SomeScores 聚类平面
pheat <- gtable_add_grob(pheat, pheat_2$gtable$grobs[[1]], 7, 3)
# NMF 聚类平面
pheat <- gtable_add_grob(pheat, heat$gtable$grobs[[3]], 9, 3)
# pheat <- gtable_add_grob(pheat, heat$gtable$grobs[[8]], 2, 5, l=5)
# pheat <- gtable_add_grob(pheat, pheat_1$gtable$grobs[[1]], 2, 3)
# pheat <- gtable_add_grob(pheat, pheat_2$gtable$grobs[[1]], 3, 3)
# pheat <- gtable_add_grob(pheat, heat$gtable$grobs[[3]], 4, 3)
# plot(pheat)

# gtable_show_layout(pheat)

pdf("heatmap.pdf",width=9,height=11.2)
# grid.newpage()
plot(pheat)
dev.off()
}

