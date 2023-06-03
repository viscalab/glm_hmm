plotting_one_participant <- function(.fit) {
  plot_grid(plotting_psycho_one_participant(.fit), 
            plotting_trials_one_participant(.fit), 
            rel_widths = c(.5, .5), 
            labels = "AUTO")
}