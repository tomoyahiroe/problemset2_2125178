main <- function() {

  # setup
  # 必要に応じてパッケージをインストールすること。（install.packages）
  library(tidyverse)
  library(EnvStats)

  file_path <- # population.csvが格納されているパス
  # 例：here::here("problemset2", "01_data", "population.xlsx")

  df <- readxl::read_xlsx(file_path) 

  # (b)
  sample_size <- 200 # 市区町村の数
  beta        <- # 適切な式を入れてください。
  alpha_hat   <- # 適切な式を入れてください。

  # (c)
  simulate_mle(df, beta, sample_size)

  # (d)
  run_gradient_method(alpha_0 = 2, df = df, beta = beta, sample_size = sample_size)
  run_gradient_method(alpha_0 = 200, df = df, beta = beta, sample_size = sample_size)
  run_gradient_method(alpha_0 = 20000, df = df, beta = beta, sample_size = sample_size)

  # (e Adv)
  generate_pareto_simulation(df, alpha_hat, beta)



}

# (c)
simulate_mle <- function(df, beta, sample_size) {

  a = "((alpha + 1)*sum(log(df$population))))"
  b = "sample_size*alpha*log(beta)"
  c = "(sample_size*log(alpha)"
  d = "-"
  e = "+"

  # 以下のコードを正しく並べ替えること
    l = eval(parse(text = paste0(a, b, c, d, e)))
  ) |>
    geom_text(data = df_likelihood |> dplyr::filter(alpha_hat == 1), mapping = aes(x = alpha, y = l, label = alpha), hjust=0, vjust=-1) +
    dplyr::mutate(
      alpha_hat = dplyr::if_else(max(l) == l, 1, 0)
    )
    geom_point(data = df_likelihood, mapping = aes(x = alpha, y = l), alpha = 0.2) +
  
    theme_bw()
  ggplot() +
    geom_point(data = df_likelihood |> dplyr::filter(alpha_hat == 1), mapping = aes(x = alpha, y = l), color = "red") +
  df_likelihood <- dplyr::tibble(
    alpha = seq(0.1, 2, by = 0.01),

}


# (d)
log_likelihood <- function(alpha_1, df, beta, sample_size) {

  a = "((alpha_1[1] + 1)*sum(log(df$population))))"
  b = "sample_size*alpha_1[1]*log(beta)"
  c = "(sample_size*log(alpha_1[1])"
  d = "-"
  e = "+"

  likelihood_fn = eval(parse(text = paste0("-", a, b, c, d, e)))

}

run_gradient_method <- function(alpha_0, df, beta, sample_size){

  tictoc::tic()
  list_par <- optim(
    c(alpha_0), 
    log_likelihood, 
    df = df, 
    beta = beta, 
    sample_size = sample_size, 
    method = "BFGS")
  tictoc::toc()

  print(paste0("alpha: ", round(list_par$par, digits = 3)))
  print(paste0("-log_likelihood: ", round(list_par$value, digits = 3)))
  print(paste0("counts: ", list_par$counts))

  
}


# (e Adv)
generate_pareto_simulation <- function(df, alpha_hat, beta) {
  ) |>
    geom_point(df_simulation, mapping = aes(x = x_1, y = rv_uni)) +
  dplyr::mutate(
  ) 
  df_simulation <- dplyr::tibble(
    stat_ecdf(df, mapping = aes(x = population), color = "red" ) +
    theme_bw() 
    rv_uni = runif(200)

  ggplot() +
    x_1 = EnvStats::qpareto(rv_uni, location = beta, shape = alpha_hat)
  set.seed(1114)

}
