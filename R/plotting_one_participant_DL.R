plotting_one_participant_DL <- function(.fit, .label = "") {
  plot_grid(plotting_psycho_one_participant(.fit), 
            plotting_trials_one_participant_DL(.fit),
            labels = .label,
            label_x = .15, label_y = .95, 
            rel_widths = c(.5, .5))
}