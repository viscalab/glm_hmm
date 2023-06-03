plotting_trials_one_participant_DL <- function(.fit) {
  
  data_trials <- .fit$model_matrix_with_session %>% 
    mutate(trial = row_number())
  
  ggplot() +
    geom_line(data = .fit$posterior_probs, 
              aes(x = trial, y = p, color = state)) +
    geom_point(data = data_trials, 
               aes(x = trial, y = response), shape = "|")
}