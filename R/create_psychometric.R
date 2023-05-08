create_psychometric <- function(.x, .value, 
                                .xmin = -200, 
                                .xmax = 200) {
  tibble({{.x}} := seq(.xmin, .xmax, length.out = 100)) |> 
    mutate(prop = glm_logistic_fun({{.x}}, .value))
}