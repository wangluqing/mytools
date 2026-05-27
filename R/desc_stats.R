#' 一键生成描述统计表
#'
#' 对数据框中的数值变量计算均值(SD)，分类变量计算n(%)，
#' 按指定分组变量分组汇总。
#'
#' @param data 数据框
#' @param group_var 分组变量名（字符串），如 "treatment"
#' @param digits 小数位数，默认 1
#'
#' @return 一个 tibble，包含各变量的分组统计结果
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars$am <- factor(mtcars$am, labels = c("Auto", "Manual"))
#' desc_table(mtcars, group_var = "am")
#'
desc_table <- function(data, group_var = NULL, digits = 1) {

  # 识别数值列和分类列
  num_vars <- names(data)[sapply(data, is.numeric)]
  cat_vars <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]

  # 去掉分组变量
  if (!is.null(group_var)) {
    num_vars <- setdiff(num_vars, group_var)
    cat_vars <- setdiff(cat_vars, group_var)
  }

  results <- list()

  # 数值变量统计
  for (var in num_vars) {
    if (is.null(group_var)) {
      vals <- stats::na.omit(data[[var]])
      results[[var]] <- dplyr::tibble(
        variable = var,
        group = "Overall",
        statistic = paste0(
          round(mean(vals), digits), " (",
          round(stats::sd(vals), digits), ")"
        ),
        type = "mean (SD)"
      )
    } else {
      res <- data |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::summarise(
          statistic = paste0(
            round(mean(.data[[var]], na.rm = TRUE), digits), " (",
            round(stats::sd(.data[[var]], na.rm = TRUE), digits), ")"
          ),
          .groups = "drop"
        ) |>
        dplyr::mutate(variable = var, type = "mean (SD)") |>
        dplyr::rename(group = 1)
      results[[var]] <- res
    }
  }

  # 分类变量统计
  for (var in cat_vars) {
    if (is.null(group_var)) {
      tab <- table(data[[var]])
      pct <- round(prop.table(tab) * 100, digits)
      results[[var]] <- dplyr::tibble(
        variable = paste0(var, " - ", names(tab)),
        group = "Overall",
        statistic = paste0(tab, " (", pct, "%)"),
        type = "n (%)"
      )
    } else {
      res <- data |>
        dplyr::count(.data[[group_var]], .data[[var]]) |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::mutate(
          pct = round(n / sum(n) * 100, digits),
          statistic = paste0(n, " (", pct, "%)"),
          variable = paste0(var, " - ", .data[[var]])
        ) |>
        dplyr::ungroup() |>
        dplyr::select(variable, group = 1, statistic) |>
        dplyr::mutate(type = "n (%)")
      results[[var]] <- res
    }
  }

  dplyr::bind_rows(results)
}