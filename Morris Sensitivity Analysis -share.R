library(plspm)

# Define the PLS-SEM model function
model_func <- function(X) {
  # Load and preprocess data (adjust path as needed)
  data <- read.csv("F:/论文撰写/论文1/作图代码及最终出图-20240809/P-T-SM时间序列绘图五个区域平均.csv")
  
  # Extract variables
  win <- subset(data, class == "WS(m/s)")[, 3]
  tem <- subset(data, class == "T(K)")[, 3]
  ssd <- subset(data, class == "SSD(h)")[, 3]
  rhu <- subset(data, class == "RH(%)")[, 3]
  pre <- subset(data, class == "P(mm)")[, 3]
  evp <- subset(data, class == "E(mm)")[, 3]
  prs <- subset(data, class == "PRS(kpa)")[, 3]
  ndvi <- subset(data, class == "NDVI")[, 3]
  sm <- subset(data, class == "SM(m^3/m^3)")[, 3]
  
  # Interaction terms
  rhuaddndvi <- mapply(`*`, as.matrix(rhu), as.matrix(ndvi))
  preaddndvi <- mapply(`*`, as.matrix(pre), as.matrix(ndvi))
  prsaddndvi <- mapply(`*`, as.matrix(prs), as.matrix(ndvi))
  
  # Create dataframe and handle NAs
  dat <- data.frame(win, tem, ssd, rhu, pre, evp, prs, ndvi, sm, rhuaddndvi, preaddndvi, prsaddndvi)
  dat <- data.frame(apply(dat, 2, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
  names(dat) <- c("win", "tem", "ssd", "rhu", "pre", "evp", "prs", "ndvi", "sm", "rhuaddndvi", "preaddndvi", "prsaddndvi")
  
  # Define path matrix (adjust based on your hypotheses)
  pht <- c(0, 0, 0, 0, 0)
  edw <- c(0, 0, 0, 0, 0)
  ndviaddclim <- c(0, 0, 0, 0, 0)
  ndvi <- c(1, 1, 0, 0, 0)
  sm <- c(1, 1, 1, 1, 0)
  path_matrix <- rbind(pht, edw, ndviaddclim, ndvi, sm)
  colnames(path_matrix) <- rownames(path_matrix)
  
  # Define blocks and modes
  blocks <- list(c(5, 4, 2, 7), c(6, 3, 1), 11:15, 8, 9)  # Adjust indices to match your variables
  modes <- rep("A", 5)  # All reflective modes
  
  # Fit PLS-SEM model
  pls_model <- plspm(dat, path_matrix, blocks = blocks, modes = modes)
  
  # Predict using new data (X must match column names of `dat`)
  if (!missing(X)) {
    X_df <- as.data.frame(X)
    colnames(X_df) <- names(dat)[1:(ncol(dat) - 1)]  # Ensure alignment with predictors
    predicted_sm <- predict(pls_model, newdata = X_df)
    return(predicted_sm)
  } else {
    return(pls_model)  # Return full model if no new data provided
  }
}

# Example usage:
# X_input <- matrix(rnorm(100), ncol = 11)  # Mock input (adjust dimensions)
# predictions <- model_func(X_input)


# 定义变量的合理取值范围（根据实际数据调整！）
binf <- apply(dat, 2, min)  # 最小值作为下界
bsup <- apply(dat, 2, max)  # 最大值作为上界

# 执行Morris分析
morris_results <- morris(
  model = model_func,
  factors = colnames(dat)[-9],  # 排除因变量sm
  r = 50,                       # 增加轨迹数以提高精度
  design = list(type = "oat", levels = 10),  # 更多水平
  binf = binf[-9],              # 排除sm的下界
  bsup = bsup[-9]               # 排除sm的上界
)

# 结果可视化
print(morris_results)
plot(morris_results, main = "Morris敏感性分析")



