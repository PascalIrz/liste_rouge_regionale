#' Plot FBI time series by group
#' 
#' Adapted from aspe::gg_temp_ipr() available at: https://github.com/PascalIrz/aspe
#'
#' @param df 
#' @param var_y 
#' @param var_group 
#' @param station_sel 
#' @param sup_500m 
#' @param nb_col 
#' @param max_y 
#' @param inv_y 
#' @param year_as_factor 
#' @param title 
#' @param y_title 
#' @param df_classes 
#' @param interactive 
#' @param width 
#' @param height 
#' @param options 
#'
#' @return
#' @export
#'
#' @examples
gg_temp_fbi <-
  function(df,
        #   var_facet = NA,
           var_x,
           var_y,
           var_group,
           sup_500m = FALSE,
           nb_col = 6,
           max_y = NULL,
           inv_y = TRUE,
           year_as_factor = FALSE,
           title = "",
           y_title = "",
           df_classes,
           interactive = FALSE,
           width = 6,
           height = 5,
           options = list())

  {
    # mise en forme des étiquettes inspirée de https://stackoverflow.com/a/57086284
    int_breaks <- function(x, n = 5) {
      if (length(unique(x)) > 1) {
        pretty(x, n)[round(pretty(x, n), 1) %% 1 == 0]
      } else {
        round(unique(x)) + c(-1, 0, 1)
      }
    }
    
    int_limits <- function(x) {
      if (length(unique(x)) > 1) {
        range(x)
      } else {
        range(int_breaks(x))
      }
    }
    
    # sélection des données
    var_x <- enquo(var_x)
    var_y <- enquo(var_y)
    var_group <- enquo(var_group)

    if (is.null(max_y))
    {
      max_y <- df %>% #max(df$ipr)
        pull(!!var_y) %>%
        max(na.rm = T)
    }
    
    # année en facteur ?
    if (year_as_factor)
    {
      df <- df %>%
        mutate(!!var_x := as.factor(!!var_x))
    }
    
    plot_ipr_station <- ggplot(data = df) +
      ggiraph::geom_rect_interactive(
        data = df_classes,
        mapping = ggplot2::aes(
          ymin = cli_borne_inf,
          ymax = cli_borne_sup,
          fill = cli_libelle
        ),
        xmin = -Inf,
        xmax = Inf,
        alpha = 0.3
      ) +
      scale_fill_manual(
        values = fbi_classes$classe_couleur,
        labels = c("Very good", "Good", "Mediocre", "Bad", "Very bad"),
        name = "Quality"
      ) +
      scale_color_manual(
        values = c("green3", "brown"),
        labels = c("REFNET", "REPNET"),
        name = "Network",
        guide = guide_legend(reverse = TRUE)
      ) +
      scale_x_continuous(breaks = int_breaks,
                         limits = int_limits) +
      ggplot2::scale_y_continuous(trans = "reverse",
                                  expand = ggplot2::expansion(mult = c(0.05, 0.01))) +
      # notes IPR
      geom_line(aes(
        x = !!var_x,
        y = !!var_y,
        col = !!var_group
      ),
      show.legend = F) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          x = !!var_x,
          y = !!var_y,
         # tooltip = hover,
          col = !!var_group
        ),
        size = 2.5,
        # pch = 21,
        fill = "grey70"
      ) +
      labs(
        y = "Fish-based Index",
        x = "Year",
        title = title
      ) +
      guides(fill = guide_legend(
        title = "Quality",
        title.position = "top",
        nrow = 5,
        # byrow = TRUE,
        override.aes = list(
          color = df_classes$classe_couleur,
          fill = df_classes$classe_couleur,
          shape = 15,
          alpha = 0.6
        )
      )) +
      theme(
        strip.text.x = element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "lightgrey", size = .25),
        panel.background = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          size = 13
        ),
        axis.text.y = element_text(size = 13),
        axis.title = element_text(size = 13)
      )
    # orientation de l'axe des IPR selon l'argument inv_y
    if (inv_y) {
      plot_ipr_station <- plot_ipr_station +
        coord_cartesian(ylim = c(max_y, 0))
    } else {
      plot_ipr_station <- plot_ipr_station +
        coord_cartesian(ylim = c(0, max_y))
    }
      
    plot_ipr_station

  }
