

# 定义 Mann-Kendall 函数
Mann_Kendall <- function(timeserial) {
  Mann_Kendall_sub <- function(timeserial) {
    r <- c()
    s <- c()
    U <- c()
    
    for (i in 2:length(timeserial)) {
      r[i] <- 0
      for (j in 1:i) {
        if (timeserial[i] > timeserial[j]) {
          r[i] <- r[i] + 1
        }
      }
      s[i] <- 0
      for (ii in 2:i) {
        s[i] <- s[i] + r[ii]
      }
      U[i] <- (s[i] - (i * (i - 1) / 4)) / sqrt(i * (i - 1) * (2 * i + 5) / 72)
    }
    r[1] <- 0
    s[1] <- 0
    U[1] <- 0
    
    LST <- list(r = r, s = s, U = U)
    return(LST)
  }
  
  timeserial_rev <- rev(timeserial)
  y1 <- Mann_Kendall_sub(timeserial)
  y2 <- Mann_Kendall_sub(timeserial_rev)
  y2$U <- -(rev(y2$U))
  
  LST <- list(UF = y1, UB = y2)
  return(LST)
}

# 读取数据
Q <- read.csv("F:/论文撰写/论文1/论文投稿文件/论文返修/R语言代码/R_change_point_detection/zdx_data.csv")

# 提取变量名
variables <- colnames(Q)[-1]  # 第一列是年份，其余列为变量

# 设置图形布局
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))  # 2行2列，调整边距

# 循环对每个变量进行 Mann-Kendall 检验并绘图
for (var in variables) {
  d <- Mann_Kendall(Q[[var]])  # 对当前变量进行 Mann-Kendall 检验
  yUF <- as.data.frame(d$UF[3])$U
  yUB <- as.data.frame(d$UB[3])$U
  
  # 绘制图形
  plot(x = c(1:length(Q$year)), y = yUF, type = "l", 
       ylim = c(min(yUF, yUB, -1.96), max(yUF, yUB, 1.96)), 
       lwd = 1, lty = 5, ylab = var, cex = 0.5, xaxt = "n", 
       mgp = c(1, 0.1, 0), tck = -0.02, xlab ="")
  points(x = c(1:length(Q$year)), y = yUB, type = "l", lty = 4, col = 6, lwd = 1.5)
  abline(h = 1.96, lty = 4, lwd = 0.5)  # 95% 置信区间上限
  abline(h = -1.96, lty = 4, lwd = 0.5) # 95% 置信区间下限
  abline(h = 0, col = "gray", lwd = 0.5)
  
  # 设置 x 轴刻度标签为年份
  year_interval <- 6  # 设置间隔
  selected_indices <- seq(1, length(Q$year), by = year_interval)  # 计算需要显示的年份位置
  selected_years <- Q$year[selected_indices]  # 筛选对应的年份
  
  axis(1, at = selected_indices, labels = selected_years, las = 1, cex.axis = 0.8)
}






#######################出图例
# 循环对每个变量进行 Mann-Kendall 检验并绘图
for (var in variables) {
  d <- Mann_Kendall(Q[[var]])  # 对当前变量进行 Mann-Kendall 检验
  yUF <- as.data.frame(d$UF[3])$U
  yUB <- as.data.frame(d$UB[3])$U
  
  # 绘制图形
  plot(x = c(1:length(Q$year)), y = yUF, type = "l", 
       ylim = c(min(yUF, yUB, -1.96), max(yUF, yUB, 1.96)), 
       lwd = 1, lty = 5, ylab = var, cex = 0.5, xaxt = "n", 
       mgp = c(1, 0.1, 0), tck = -0.02, xlab ="")
  points(x = c(1:length(Q$year)), y = yUB, type = "l", lty = 4, col = 6, lwd = 1.5)
  abline(h = 1.96, lty = 4, lwd = 0.5)  # 95% 置信区间上限
  abline(h = -1.96, lty = 4, lwd = 0.5) # 95% 置信区间下限
  abline(h = 0, col = "gray", lwd = 0.5)
  
  # 设置 x 轴刻度标签为年份
  year_interval <- 6  # 设置间隔
  selected_indices <- seq(1, length(Q$year), by = year_interval)  # 计算需要显示的年份位置
  selected_years <- Q$year[selected_indices]  # 筛选对应的年份
  
  axis(1, at = selected_indices, labels = selected_years, las = 1, cex.axis = 0.8)
  
  # 添加图例
  legend("bottomright",                           # 图例位置
         legend = c("UF Statistic", "UB Statistic", "95% Confidence Interval", "y = 0 "), # 图例文字
         col = c("black", 6, "black", "gray"),            # 颜色
         lty = c(5, 4, 4, 1),                             # 线型
         lwd = c(1, 1.5, 0.5, 0.5),                       # 线宽
         bty = "n",                                       # 无边框
         cex = 0.8)                                       # 文字大小
}
