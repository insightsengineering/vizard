# ===============================================================================
#' Render a SVG
#'
#' Render a given ggplot object or a call object as a SVG
#'
#' @param g A \code{ggplot} object or a \code{call} object
#' @param height Height of the SVG
#' @param width Width of the SVG
#' @param scaling scaling factor for the SVG
#' @param element_width width of the SVG element in any valid CSS unit
#' @param element_height height of the SVG element in any valid CSS unit
#' @param add_download_button if \code{TRUE} a download button will be added to the SVG
#' @param standalone if \code{TRUE} the SVG will be surrounded by an HTML document
#' @param web_fonts Fonts to be used for the SVG
#' @param bg Background color of the SVG
#' @param font_family Font family of the SVG
#' @param fix_rect if \code{TRUE} a fix rectangle elements will be applied
#'  (solves problems with overlayed white rectanges in certain circumstances)
#' @param ... Additional arguments passed to the SVG device
#'
#' @return A browsable SVG
#' @export
render_svg <- function(g = NULL,
                       height = 8,
                       width = 8,
                       scaling = 1,
                       element_width = "100%",
                       element_height = "100%",
                       add_download_button = T,
                       standalone = F,
                       web_fonts = "https://fonts.googleapis.com/css2?family=Lato:wght@100;300;400;700;900&display=swap",
                       bg = "transparent",
                       font_family = "Lato, sans-serif",
                       fix_rect = T,
                       ...) {
  if (!inherits(g, "ggplot") && !inherits(g, "call")) {
    rlang::abort(glue::glue("g must be either a `ggplot` object or a `call` object\nCurrently g is a {class(g)}"))
  }
  stopifnot(is.numeric(height))
  stopifnot(is.numeric(width))
  stopifnot(is.numeric(scaling))
  stopifnot(is.character(element_width))
  stopifnot(is.character(element_height))
  n_dev <- length(dev.list())
  tryCatch(
    {
      # Open SVG device
      svg <- svglite::svgstring(
        height = height,
        width = width,
        scaling = scaling,
        web_fonts = web_fonts,
        standalone = standalone,
        bg = bg,
        ...
      )
      if (inherits(g, "call")) {
        g <- rlang::eval_tidy(g)
      }
      print(g)
      # Close SVG device
      dev.off()
      svg <- svg()
      svg <- as.character(svg)
      # Adjust SVG parameterss
      if (!is.null(font_family)) svg <- vizard:::fix_font(svg, param = "font-family", value = font_family)
      if (!is.null(element_width)) svg <- vizard:::regex_replace_element_parameter(svg, param = "width", value = element_width)
      if (!is.null(element_height)) svg <- vizard:::regex_replace_element_parameter(svg, param = "height", value = element_height)
      if (fix_rect) svg <- stringr::str_replace(svg, stringr::fixed("/>"), "></rect>")
      # Add Download button
      # TODO: import nessecary functions from other packages
      # if (add_download_button) {
      #   svg <- nightowl::add_download_button(svg)
      # }
      svg <- htmltools::HTML(svg)
      svg <- htmltools::browsable(svg)
      return(svg)
    },
    error = function(e) {
      stop(e)
    },
    finally = {
      while (length(dev.list()) > n_dev) {
        dev.off()
      }
    }
  )
}

# Fix font family in HTML/SVG
fix_font <- function(html, param = "font-family", value = "Lato, sans-serif", old_value = "[^;]*") {
  str <- as.character(html)
  pattern <- glue::glue("{param}: {old_value}")
  replace <- glue::glue("{param}: {value}")
  new_str <- stringr::str_replace_all(str, pattern, replace)
  return(htmltools::HTML(new_str))
}

# Regex replace element parameters
regex_replace_element_parameter <- function(html, param, value, as_character = FALSE) {
  str <- as.character(html)
  pattern <- glue::glue("{param}[ =]+'\\S+'")
  replace <- glue::glue("{param}='{value}'")
  new_str <- stringr::str_replace(str, pattern, replace)
  if (as_character) {
    return(new_str)
  } else {
    return(shiny::HTML(new_str))
  }
}
