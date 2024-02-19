library(GGally)
library(ggpubr)
library(scales)

mif_traces <- function(traces_df, unknown_pars, traces_col) {
  
  traces_df |> 
    filter(name %in% c("loglik", unknown_pars)) |> 
    mutate(name = ifelse(name == "I_0", "I[0]", name)) |> 
    ggplot(aes(x = as.numeric(iteration), y = value, group = .L1)) +
    geom_line(color = traces_col, alpha = 0.25) +
    theme_pubr() +
    scale_y_continuous(labels = comma) +
    labs(y = "Value", x = "Iteration") +
    facet_wrap(~name,scales = "free_y", labeller = label_parsed)
}

raw_likelihood <- function(all_df, var_x, point_colour) {
  
  var_x <- as.character(var_x)
  lab_x <- var_x
  lab_x <- ifelse(lab_x == "I_0", "I[0]", lab_x)
  
  all_df %>%
    filter(type == "result") |> 
    ggplot(aes(x = !!ensym(var_x), y = loglik))+
    geom_point(colour = point_colour)+
    theme_pubr() +
    geom_line(stat = "smooth", method = "loess", formula = y ~ x, alpha = 0.25,
              colour = point_colour, span = 0.75, linewidth = 1) +
    labs(
      x = parse(text = lab_x))
}

plot_guesses <- function(guesses_df, point_size = 1, point_colour) {
  
  aes_points <- list(continuous = wrap("points", alpha = 0.5, size = point_size,
                                       colour = point_colour))
  
  guesses_df <- rename(guesses_df, `I[0]` = `I_0`)
  
  ggpairs(guesses_df, 
          upper = aes_points,
          lower = aes_points,
          diag  = list(continuous =  'blankDiag'),
          labeller = label_parsed) +
    theme_pubr() +
    theme(axis.text = element_text(size = 6))
}