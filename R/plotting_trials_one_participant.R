plotting_trials_one_participant <- function(.fit) {
  
  internal_matrix_doble <- data.frame(.fit$model_matrix_with_session[rep(seq_len(nrow(.fit$model_matrix_with_session)), each = length(unique(.fit$posterior_probs$state))), ])|>
    cbind(.fit$posterior_probs) 
  
  duplicated_cols <- duplicated(names(internal_matrix_doble))
  
  internal_matrix_doble <- internal_matrix_doble[!duplicated_cols | names(internal_matrix_doble) %in% duplicated_cols[!duplicated_cols]]
  
  internal_matrix_single <- internal_matrix_doble |> select(trial, response) |> distinct(trial, .keep_all = TRUE)
  
  
  ggplot() +
    geom_line(data = internal_matrix_doble, aes(x = trial, y = p, color = state)) +
    geom_point(data = internal_matrix_single, aes(x = trial, y = response), shape = "|") +
    geom_text(data = internal_matrix_doble, aes(text = subject))
}