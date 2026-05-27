#' 快速箱线图（带散点和配色）
#'
#' 用 ggplot2 画一张开箱即用的分组箱线图，
#' 自动添加散点、配色和标题。
#'
#' @param data 数据框
#' @param x 分组变量名（字符串）
#' @param y 数值变量名（字符串）
#' @param title 图表标题，默认自动生成
#' @param palette 配色向量，默认使用内置配色
#'
#' @return ggplot2 对象
#' @export
#'
#' @examples
#' quick_boxplot(iris, x = "Species", y = "Sepal.Length")
#'
quick_boxplot <- function(data, x, y, title = NULL, palette = NULL) {

  if (is.null(title)) {
    title <- paste(y, "by", x)
  }

  if (is.null(palette)) {
    palette <- c("#4ECDC4", "#FF6B6B", "#45B7D1", "#F39C12",
                 "#9B59B6", "#1ABC9C", "#E74C3C", "#3498DB")
  }

  n_groups <- length(unique(data[[x]]))

  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data[[x]], y = .data[[y]], fill = .data[[x]]
  )) +
    ggplot2::geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.3, size = 1.2) +
    ggplot2::scale_fill_manual(values = palette[1:n_groups]) +
    ggplot2::labs(title = title, x = NULL, y = y) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    )

  p
}