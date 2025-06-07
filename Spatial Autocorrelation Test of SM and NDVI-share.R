



# 加载必要的包
library(raster)    # 用于处理栅格数据
library(gstat)     # 用于地统计分析，包括半变异函数
library(sp)        # 提供空间数据类
library(ggplot2)   # 用于可视化

# 1. 加载NDVI和SM变化趋势的栅格数据
ndvi_trend <- raster("F:/论文撰写/论文1/NDVI平均值/new/ndvimean1.tif")  # 替换为你的NDVI趋势文件路径
sm_trend <- raster("F:/论文撰写/论文1/a出图/tifzong/clip不分区/smaverage.tif_矢量边界.shp.tif")      # 替换为你的SM趋势文件路径

# 2. 数据预处理
# 确保两个栅格的分辨率和范围一致
if (!compareRaster(ndvi_trend, sm_trend, stopiffalse = FALSE)) {
  # 如果不一致，进行重采样
  sm_trend <- resample(sm_trend, ndvi_trend, method = "bilinear")
}

# 3. 将栅格转换为点数据（半变异函数需要点数据）
# 随机采样以减少计算量（对于大栅格）
set.seed(123)  # 确保可重复性
sample_points <- sampleRandom(ndvi_trend, size = 100, sp = TRUE)  # 从NDVI栅格采样1000个点

# 提取NDVI和SM值到采样点
sample_points$NDVI <- terra::extract(ndvi_trend, sample_points)
sample_points$SM <- terra::extract(sm_trend, sample_points)

# 移除NA值
sample_points <- sample_points[!is.na(sample_points$NDVI) & !is.na(sample_points$SM), ]

# 4. 计算NDVI的半变异函数
ndvi_variogram <- variogram(NDVI ~ 1, data = sample_points, cutoff = 1000, width = 500)
# cutoff: 最大距离范围（根据你的数据空间范围调整）
# width: 距离区间宽度

# 5. 计算SM的半变异函数
sm_variogram <- variogram(SM ~ 1, data = sample_points, cutoff = 1000, width = 500)

# 6. 可视化半变异函数
ggplot(ndvi_variogram, aes(x = dist, y = gamma)) +
  geom_point() +
  geom_line() +
  labs(title = "NDVI趋势的半变异函数", 
       x = "距离", 
       y = "半方差") +
  theme_bw()

ggplot(sm_variogram, aes(x = dist, y = gamma)) +
  geom_point() +
  geom_line() +
  labs(title = "SM趋势的半变异函数", 
       x = "距离", 
       y = "半方差") +
  theme_bw()

# 7. 拟合理论模型（可选）
# 例如拟合球形模型
ndvi_fit <- fit.variogram(ndvi_variogram, vgm("Sph"))
sm_fit <- fit.variogram(sm_variogram, vgm("Sph"))

# 查看模型参数
print(ndvi_fit)
print(sm_fit)

# 绘制拟合结果
plot(ndvi_variogram, model = ndvi_fit, main = "NDVI趋势的半变异函数及拟合模型")
plot(sm_variogram, model = sm_fit, main = "SM趋势的半变异函数及拟合模型")

