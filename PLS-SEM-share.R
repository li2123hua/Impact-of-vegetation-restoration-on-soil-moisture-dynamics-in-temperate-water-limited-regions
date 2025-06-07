
library(sp)
library(raster)
library(broom)
#library(rgdal)
library(tibble)
library(dplyr)
library(tidyr)
library(tidyverse)
library(devtools)
library(usethis)
#install_github("gastonstat/plspm")
library(plspm)
data <- read.csv("F:/论文撰写/论文1/作图代码及最终出图-20240809/P-T-SM时间序列绘图五个区域平均.csv")


win <- subset(data,  class == "WS(m/s)")[,3]
tem <- subset(data, class == "T(K)")[,3]
ssd <- subset(data, class == "SSD(h)")[,3]
rhu <- subset(data,  class == "RH(%)")[,3]
pre <- subset(data,  class == "P(mm)")[,3]
evp <- subset(data,   class == "E(mm)")[,3]
prs <- subset(data, class == "PRS(kpa)")[,3]
ndvi <- subset(data, class == "NDVI")[,3]
sm <- subset(data, class == "SM(m^3/m^3)")[,3]
rhuaddndvi <- mapply(`*`,as.matrix(rhu),as.matrix(ndvi))
preaddndvi <- mapply(`*`,as.matrix(pre),as.matrix(ndvi))
prsaddndvi <- mapply(`*`,as.matrix(prs),as.matrix(ndvi))


  dat <- data.frame(win,tem,ssd,rhu,pre,evp,prs,ndvi,sm, rhuaddndvi,preaddndvi,prsaddndvi)
  dat<-data.frame(apply(dat,2,function(x){
      x[is.na(x)] = mean(x,na.rm = T);x}))
     names(dat)<-c("win","tem","ssd","rhu","pre","evp","prs","ndvi","sm"，"rhuaddndvi","preaddndvi","prsaddndvi")
    pht = c(0,0,0,0,0)
    edw = c(0,0,0,0,0)
    ndviaddclim = c(0,0,0,0,0)
    ndvi = c(1,1,0,0,0)
    sm   = c(1,1,1,1,0)
    path_matrix = rbind(pht,edw,ndviaddclim,ndvi,sm)
    innerplot(path_matrix)
    colnames(path_matrix) = rownames(path_matrix)
    blocks = list(c(5,4,2,7),c(6,3,1),11:15,8,9)
    modes = c("A","A","A","A","A")
    pls = plspm(dat, path_matrix, blocks = blocks, modes = modes)
    print(pls$inner_model)

pth_sm <- pls$path_coefs[5,1]
edw_sm <- pls$path_coefs[5,2]
ndviaddclim_sm <- pls$path_coefs[5,3]
ndvi_sm <- pls$path_coefs[5,4]
res_sm <- 1-(pth_sm+edw_sm+ndvi_sm+ndviaddclim_sm)
 R2 <- pls$inner_summary[5,2]


# Obtain the weights of NDVI in the latent variable model
ndvi_weights <- pls_result$outer_model[pls_result$outer_model$block == "ndvi", ]
print(ndvi_weights)
library(ggplot2)
ndvi_scores <- pls_result$scores[, "ndvi"]
sm_scores <- pls_result$scores[, "sm"]


ndvi_effect <- data.frame(
  NDVI = dat$ndvi,
  NDVI_Score = ndvi_scores,
  SM_Score = sm_scores
)


ggplot(ndvi_effect, aes(x = NDVI, y = SM_Score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  labs(
    title = "Partial Dependency Plot: NDVI vs. SM (Latent Scores)",
    x = "NDVI (Original Values)",
    y = "Soil Moisture (Latent Variable Score)"
  ) +
  theme_base()+
  theme(plot.background = element_rect(fill = "white",color = NA))+
  theme(text = element_text(family = "serif", size = 10))+
  theme(
    legend.position = "right",
    legend.key.height = unit(2, "cm"),   
   legend.key.width = unit(0.4, "cm")   
  )+
  theme(
    axis.ticks.length = unit(0.1, "cm"), 
    axis.ticks = element_line(size =0.2) 
  )+
  theme(panel.border = element_rect(color = "darkgrey", linewidth = 0.1))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  geom_vline(xintercept = 0.27, linetype = "dashed", color = "black", linewidth = 0.5)









