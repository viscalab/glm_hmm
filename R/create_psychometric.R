create_psychometric <- function(.value) {
  tibble(signed_contrast = seq(-1, 1, .01)) |> 
    mutate(prop = glm_logistic_fun(signed_contrast, .value))
}