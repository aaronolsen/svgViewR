#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
svgviewr <- function(
  svg = NULL,
  animation_reverse = FALSE,
  animation_duration = 1,
  animation_repeat = -1,
  animation_count = 0,
  margin = list(),
  background_color = NULL,
  start_rotate = FALSE,
  show_control_panel = TRUE,
  width = NULL, height = NULL, elementId = NULL
) {

  stopifnot(inherits(svg,c("character","connection")))
  
  if(inherits(svg,"connection")) {
    svg <- paste0(
      readLines(svg),
      collapse="\n"
    )
  }
  
  # forward options using x
  x = list(
    svg = htmltools::HTML(svg),
    animation_reverse = animation_reverse,
    animation_duration = animation_duration,
    animation_repeat = animation_repeat,
    animation_count = animation_count,
    margin = margin,
    background_color = background_color,
    start_rotate = start_rotate,
    show_control_panel = show_control_panel
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'svgviewr',
    x,
    width = width,
    height = height,
    package = 'svgViewR',
    elementId = elementId
  )
}

# custom html function for svgviewr
svgviewr_html <- function(id, style, class, ...){
  control_panel <- paste0(
    c(
      readLines(system.file("extdata/html/control_panel1.html", package="svgViewR")),
      readLines(system.file("extdata/html/control_panel2.html", package="svgViewR"))
    ),
    collapse="\n"
  )
  htmltools::tagList(
    htmltools::tags$div(
      id = id, class = class, style = style, style="position:relative;", ...,
      htmltools::tags$a(class="keydown", type="checkbox", onkeydown="javascript:;"),
      htmltools::HTML(control_panel),
      htmltools::tag("svg",list(style="width: 100%; height: 100%; position: absolute; top: 0px; left: 0px; z-index: -1; background-color: white;",class="world"))
    )
  )
}

#' Shiny bindings for svgviewr
#'
#' Output and render functions for using svgviewr within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a svgviewr
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name svgviewr-shiny
#'
#' @export
svgviewrOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'svgviewr', width, height, package = 'svgViewR')
}

#' @rdname svgviewr-shiny
#' @export
renderSvgviewr <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, svgviewrOutput, env, quoted = TRUE)
}
