# Create a function to detect similar formular (x + y and y + x)
is_same <- function(x, y){
  all_vect <- list()
  
  if (length(x) != length(y)) {
    error_mess <- paste("Impossible to compare size of", length(x), "for x and", length(y), "for y")
    stop(error_mess)
  }
  
  for (i in 1:length(x)) {
    
    spl_x <- strsplit(x[i], ""); spl_y <- strsplit(y[i], "")
    spl_x <- spl_x[[1]]; spl_y <- spl_y[[1]]
    if (length(spl_x) != length(spl_y)) {
      warning("Number of character in x and y differs")
    }
    suppressWarnings({
      is_same <- sort(spl_x) == sort(spl_y)
    })
    
    all_vect[[i]] <- all(is_same)
  }
  return(unlist(all_vect))
}



make_table <- function(models){
  
  # this function extracts the model data for a single model (row)
  extract_model_data <- function(model){
    c(summary(model)$ds$key,
      model$ddf$ds$aux$ddfobj$scale$formula,
      model$ddf$criterion,
      ddf.gof(model$ddf, qq=FALSE)$dsgof$CvM$p,
      summary(model)$ds$average.p,
      summary(model)$ds$average.p.se
    )
  }
  
  # applying that to all the models then putting it into a data.frame
  res <- as.data.frame(t(as.data.frame(lapply(models, extract_model_data))),
                       stringsAsFactors=FALSE)
  
  # making sure the correct columns are numeric
  res[,3] <- as.numeric(res[,3])
  res[,4] <- as.numeric(res[,4])
  res[,5] <- as.numeric(res[,5])
  res[,6] <- as.numeric(res[,6])
  
  # giving the columns names
  colnames(res) <- c("Key function", "Formula", "AIC", "Cramer-von Mises p-value",
                     "P_a", "se(P_a)")
  
  # creating a new column for the AIC difference to the best model
  res[["DeltaAIC"]] <- res$AIC - min(res$AIC, na.rm=TRUE)
  # ordering the model by AIC score
  res <- res[order(res$AIC),]
  
  # returning the data.frame
  return(res)
}

# Create a function to return p-value significant symbol
pval_sign <- function(x){
  if (x >= 0 & x < 0.001) {
    sgn <- "***"
  }else if(x >= 0.001 & x < 0.01){
    sgn <- "**"
  }else if (x >= 0.01 & x < 0.05) {
    sgn <- "*"
  }else{
    sgn <- "ns"
  }
  return(sgn)
}

name <- function(variables) {
  x.rank <- rank(x)
  mean.ranks <- tapply(x.rank, g, mean, na.rm = TRUE)
  grp.sizes <- tapply(x, g, length)
}


dunntest <- function (data, formula, p.adjust.method = "holm", detailed = FALSE) 
{
  args <- as.list(environment()) %>% .add_item(method = "dunn_test")
  if (is_grouped_df(data)) {
    results <- data %>% doo(dunn_test_, formula, p.adjust.method)
  }
  else {
    results <- dunn_test_(data, formula, p.adjust.method)
  }
  if (!detailed) {
    results <- results %>% select(-.data$method, -.data$estimate, 
                                  -.data$estimate1, -.data$estimate2)
  }
  results %>% rstatix:::set_attrs(args = args) %>% rstatix:::add_class(c("rstatix_test", 
                                                     "dunn_test"))
}

dunn_test_ <- function(data, formula, p.adjust.method = "holm") 
  {
  outcome <- rstatix:::get_formula_left_hand_side(formula)
  group <- rstatix:::get_formula_right_hand_side(formula)
  number.of.groups <- rstatix:::guess_number_of_groups(data, group)
  if (number.of.groups == 1) {
    stop("all observations are in the same group")
  }
  data <- data %>% select(!!!syms(c(outcome, group))) %>% rstatix:::get_complete_cases() %>% 
    rstatix:::.as_factor(group)
  x <- data %>% pull(!!outcome)
  g <- data %>% pull(!!group)
  group.size <- data %>% rstatix:::get_group_size(group)
  if (!all(is.finite(g))) 
    stop("all group levels must be finite")
  x.rank <- rank(x)
  mean.ranks <- tapply(x.rank, g, mean, na.rm = TRUE)
  ci_l <- tapply(x.rank, g, find_ci, side = "left")
  ci_r <- tapply(x.rank, g, find_ci, side = "right")

  grp.sizes <- tapply(x, g, length)
  n <- length(x)
  C <- rstatix:::get_ties(x.rank, n)
  
  compare.meanrank <- function(i, j) {
    mean.ranks[i] - mean.ranks[j]
  }
  compare_ci_l <- function(i, j) {
    ci_l[i] - ci_l[j]
  }
  compare_ci_r <- function(i, j) {
    ci_r[i] - ci_r[j]
  }
  
  compare.stats <- function(i, j) {
    dif <- mean.ranks[i] - mean.ranks[j]
    A <- n * (n + 1)/12
    B <- (1/grp.sizes[i] + 1/grp.sizes[j])
    zval <- dif/sqrt((A - C) * B)
    zval
  }
  
  compare.levels <- function(i, j) {
    dif <- mean.ranks[i] - mean.ranks[j]
    A <- n * (n + 1)/12
    B <- (1/grp.sizes[i] + 1/grp.sizes[j])
    zval <- dif/sqrt((A - C) * B)
    pval <- 2 * stats::pnorm(abs(zval), lower.tail = FALSE)
    pval
  }
  
  ESTIMATE <- stats::pairwise.table(compare.meanrank, levels(g), 
                                    p.adjust.method = "none") %>% 
    rstatix:::tidy_squared_matrix("diff")
  PSTAT <- stats::pairwise.table(compare.stats, levels(g), 
                                 p.adjust.method = "none") %>% 
    rstatix:::tidy_squared_matrix("statistic")
  CI_L <- stats::pairwise.table(compare_ci_l, levels(g), p.adjust.method = "none") %>% 
    rstatix:::tidy_squared_matrix("ci_lower")
  CI_R <- stats::pairwise.table(compare_ci_r, levels(g), p.adjust.method = "none") %>% 
    rstatix:::tidy_squared_matrix("ci_upper")
  PVAL <- stats::pairwise.table(compare.levels, levels(g), p.adjust.method = "none") %>% 
    rstatix:::tidy_squared_matrix("p")


  # Tidy all matrices
  PVAL <- PVAL %>% 
    mutate(method = "Dunn Test", .y. = outcome) %>% 
    rstatix:::adjust_pvalue(method = p.adjust.method) %>% 
    rstatix:::add_significance("p.adj") %>% 
    tibble::add_column(statistic = PSTAT$statistic, .before = "p") %>% 
    tibble::add_column(estimate = ESTIMATE$diff, .before = "group1") %>% 
    tibble::add_column(ci_upper = CI_R$ci_upper, .after = "estimate") %>% 
    tibble::add_column(ci_lower = CI_L$ci_lower, .after = "estimate") %>% 
    select(.data$.y., .data$group1, .data$group2, .data$estimate, everything())
  print(PVAL)

  
  n1 <- group.size[PVAL$group1]
  n2 <- group.size[PVAL$group2]
  mean.ranks1 <- mean.ranks[PVAL$group1]
  mean.ranks2 <- mean.ranks[PVAL$group2]
  
  PVAL %>% tibble::add_column(n1 = n1, n2 = n2, .after = "group2") %>% 
    tibble::add_column(estimate1 = mean.ranks1, estimate2 = mean.ranks2, 
               .after = "estimate")
}
.add_item <- rstatix:::.add_item

# https://bookdown.org/logan_kelly/r_practice/p09.html

find_ci <- function(x, alpha = .05, side = 'all') {
  #Step 1: Calculate the mean
  sample_mean <- mean(x, na.rm = TRUE)
  
  #Step 2: Calculate the standard error of the mean
  sample_sd <- sd(x, na.rm = TRUE)
  sample_lenght <- length(x[!is.na(x)])
  se <- sample_sd/sqrt(sample_lenght)

  #Step 3: Find the t-score that corresponds to the confidence level
  degrees_freedom = sample_lenght - 1
  t_score = qt(p = alpha/2, df = degrees_freedom,lower.tail = FALSE)
  
  #Step 4. Calculate the margin of error and construct the confidence interval
  margin_error <- t_score * se
  lower_bound <- sample_mean - margin_error
  upper_bound <- sample_mean + margin_error
  
  ci <- switch (side,
    'all' = c(lower_bound, upper_bound),
    'left' = lower_bound,
    'right' = upper_bound
  )
  return(ci)
}
