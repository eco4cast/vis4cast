

forecast_ggobj <- function(df, ncol = NULL, show.legend = TRUE) {
  
  df |> dplyr::collect() |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(datetime, observation)) +
    ggiraph::geom_ribbon_interactive(ggplot2::aes(x = datetime, ymin = quantile02.5, ymax = quantile97.5,
                                fill = model_id, data_id = model_id, tooltip = model_id),
                            alpha = 0.2, show.legend=FALSE) +
    ggiraph::geom_line_interactive(ggplot2::aes(datetime, mean, col = model_id,
                              tooltip = model_id, data_id = model_id), show.legend=show.legend) +
    ggplot2::facet_wrap(~site_id, scales = "free", ncol=ncol) +
    ggplot2::guides(x =  ggplot2::guide_axis(angle = 45)) +
    ggplot2::theme_bw()
}


forecast_plots <- function(df,
                           ncol = NULL, 
                           show.legend = FALSE, 
                           width_svg = 8,
                           height_svg = 4) {
  
  if(nrow(df)==0) return(NULL)
  
  ggobj <- forecast_ggobj(df, ncol, show.legend)
  ggiraph::girafe(ggobj = ggobj,
         width_svg = width_svg,
         height_svg = height_svg,
         options = list(
           ggiraph::opts_hover_inv(css = "opacity:0.20;"),
           ggiraph::opts_hover(css = "stroke-width:2;"),
           ggiraph::opts_zoom(max = 4)
         ))
  
}



by_model_id <- function(df, show.legend = FALSE) {
  leaderboard <-
    df |>
    dplyr::group_by(model_id) |>
    dplyr::summarise(crps = mean(crps, na.rm=TRUE),
              logs = mean(logs, na.rm=TRUE),
              .groups = "drop") |>
    dplyr::collect() |>
    dplyr::mutate(model_id = forcats::fct_rev(forcats::fct_reorder(model_id, crps)))
  
  leaderboard |>
    tidyr::pivot_longer(cols = c(crps, logs), names_to="metric", values_to="score") |>
    
    ggplot2::ggplot(ggplot2::aes(x = model_id, y= score,  fill=model_id)) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = model_id, data_id = model_id),
                         show.legend = FALSE) +
    # ggplot2::scale_y_log10() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~metric, scales='free') +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank()) # don't show model_id twice
  
}




by_reference_datetime <- function(df, show.legend = FALSE) {
  leaderboard <-
    df |>
    dplyr::group_by(model_id, reference_datetime) |>
    dplyr::summarise(crps = mean(crps, na.rm=TRUE),
              logs = mean(logs, na.rm=TRUE),
              .groups = "drop") |>
    dplyr::mutate(reference_datetime = lubridate::as_datetime(reference_datetime)) |>
    dplyr::collect() |>
    dplyr::mutate(model_id = forcats::fct_rev(forcats::fct_reorder(model_id, crps)))
  
  leaderboard |>
    tidyr::pivot_longer(cols = c(crps, logs), names_to="metric", values_to="score") |>
    
    ggplot2::ggplot(ggplot2::aes(x = reference_datetime, y= score,  col=model_id)) +
    ggiraph::geom_point_interactive(ggplot2::aes(tooltip = model_id, data_id = model_id),
                           show.legend = FALSE) +
    ggplot2::scale_y_log10() +
    ggplot2::facet_wrap(~metric, scales='free') +
    ggplot2::guides(x =  ggplot2::guide_axis(angle = 45)) +
    ggplot2::theme_bw()
}



by_horizon <- function(df, show.legend=FALSE) {
  
  leaderboard2 <- df |>
    dplyr::group_by(model_id, horizon) |>
    dplyr::summarise(crps = mean(crps, na.rm=TRUE),
              logs = mean(logs, na.rm=TRUE),
              .groups = "drop") |>
    dplyr::collect() |>
    dplyr::mutate(model_id = forcats::fct_rev(forcats::fct_reorder(model_id, crps)))  # sort by score
  
  leaderboard2 |>
    tidyr::pivot_longer(cols = c(crps, logs), names_to="metric", values_to="score") |>
    ggplot2::ggplot(ggplot2::aes(x = horizon, y= score,  col=model_id)) +
    ggiraph::geom_point_interactive(ggplot2::aes(tooltip = model_id, data_id = model_id),
                           show.legend = show.legend) +
    ggplot2::facet_wrap(~metric, scales='free') +
    ggplot2::scale_y_log10() +
    ggplot2::theme_bw()
}


horizon_filter <- function(df, horizon_cutoff=35, horizon_units="days") {
  df |>
    dplyr::mutate(horizon =
             difftime(
               lubridate::as_datetime(datetime),
               lubridate::as_datetime(reference_datetime),
               units = horizon_units)
    ) |>
    dplyr::filter(horizon <= horizon_cutoff, horizon > 0)
}

leaderboard_plots <- function(df,
                              var,
                              horizon_cutoff = 35,
                              horizon_units = "days",
                              show.legend=TRUE,
                              width_svg = 8,
                              height_svg = 6
                              ) {
  
  df <- df |> dplyr::filter(variable == var)
  if(nrow(df)==0) return(NULL)
  
  df <- horizon_filter(df, horizon_cutoff, horizon_units)
  board1 <- by_model_id(df, show.legend = FALSE)
  board2 <- by_reference_datetime(df, show.legend = FALSE) + ggplot2::theme_bw()
  board3 <- by_horizon(df, show.legend = FALSE) + ggplot2::theme_bw()
  
  requireNamespace("patchwork", quietly = TRUE)
  patchwork::get_dim(board1) # dummy
  ggob <- board1 / board2 / board3 # patchwork stack
  
  ggiraph::girafe(
    ggobj = ggob,
    width_svg = width_svg,
    height_svg = height_svg,
    options = list(
      ggiraph::opts_hover_inv(css = "opacity:0.20;"),
      ggiraph::opts_hover(css = "stroke-width:2;"),
      ggiraph::opts_zoom(max = 4)
    )
  )
  
}

globalVariables(c("datetime", "observation", "model_id", "quantile02.5",
                  "quantile97.5", "crps", "logs", "horizon", "variable", 
                  "score", "reference_datetime"),
                package="vis4cast")