plotting_psycho_one_participant <- function(.fit) {
  
  posterior_wins <- .fit$posterior_probs |> 
    group_by(trial) %>%
    filter(p == max(p)) %>%
    select(-p) 
  
  averages <- .fit$model_matrix_with_session %>% 
    mutate(trial = row_number()) %>%
    left_join(posterior_wins, by = c("session", "trial")) %>%
    mutate(dot_diff_bin = cut_interval(dot_diff, 10)) |> 
    group_by(dot_diff_bin, state) %>%
    summarise(dot_diff = mean(dot_diff), prob = mean(response), n = n(), .groups = "keep") 
  
  psycho <- .fit$recovered_weights |> 
    group_by(state) |> 
    reframe(create_psychometric(dot_diff, value))
  
  ggplot() + 
    geom_point(data = averages, 
               aes(dot_diff, prob, color = state)) + 
    geom_line(data = psycho,
              aes(x = dot_diff, y = prop, color = state)) 
}