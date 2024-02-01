
#' Normal vs the Student
#'
#' Plot both the Normal density and the Student density using ggplot. The plot also shows shaded areas under each density corresponding to a user-specified interval.
#'
#' @param mean mean of the Normal distribution
#' @param sd Standard Deviation of the Normal distribution
#' @param df Degree of freedom of the Student's t distribution
#' @param a lower bound of the interval of the shaded area
#' @param b upper bound of the interval of the shaded area
#' @param output which density to plot? "norm", "t", or "overlay" (both). This latter is the default.
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' PlotDens()
#'
#' @import stats
#' @import ggplot2
#'
PlotDens <- function(mean = 0, sd = 1, df = 1, a = -1, b = 1, output="overlay") {

  theme_set(
    theme_minimal() +
      theme(legend.position = "top") +
      theme(legend.text = element_text(size = 12)) +
      theme(legend.key.width = unit(2, "cm"))
  )

  if(output=="norm") {
    p<-ggplot() +
      stat_function(
        fun = dnorm,
        args = list(
          mean = mean,
          sd = sd
        ),
        aes(color = paste("Normal ", "(mean = ", mean, ", sd= ", sd, ")\nP(a < X < b) = ", round(pnorm(b, mean = mean, sd = sd) - pnorm(a, mean = mean, sd = sd), 3), sep = "")),
        linewidth = 1
      ) +
      stat_function(
        fun = dnorm,
        args = list(
          mean = mean,
          sd = sd
        ),
        geom = "area",
        fill = "red", alpha = 0.2,
        xlim = c(a, b)
      ) +
      xlim(-5, 5) +
      labs(color = NULL, y = "density", x = "") +
      geom_segment(data = NULL, aes(x = a, y = 0, xend = a, yend = dnorm(a, mean = mean, sd = sd)), linetype = "dashed", linewidth = 0.5) +
      geom_segment(data = NULL, aes(x = b, y = 0, xend = b, yend = dnorm(b, mean = mean, sd = sd)), linetype = "dashed", linewidth = 0.5) +
      annotate(
        geom = "text", x = a, y = dnorm(a, mean = mean, sd = sd)/2, label = "a",
        fontface = "plain", angle = 90, vjust = -1
      ) +
      annotate(
        geom = "text", x = b, y = dnorm(b, mean = mean, sd = sd)/2, label = "b",
        fontface = "plain", angle = 90, vjust = -1
      ) +
      guides(color = guide_legend(override.aes = list(size = 2)))
  }

if(output=="t") {
    p<-ggplot() +
      stat_function(
        fun = dt,
        args = list(
          df = df
        ),
        aes(color = paste("Student's t-distributio ", "(df = ", df, ")\nP(a < X < b) = ", round(pt(b, df = df) - pt(a, df = df), 3), sep = "")),
        linewidth = 1
      ) +
      stat_function(
        fun = dt,
        args = list(
          df = df
        ),
        geom = "area",
        fill = "blue", alpha = 0.1,
        xlim = c(a, b)
      ) +
      xlim(-5, 5) +
      labs(color = NULL, y = "density", x = "") +
      geom_segment(data = NULL, aes(x = a, y = 0, xend = a, yend = dt(a, df = df)), linetype = "dashed", linewidth = 0.5) +
      geom_segment(data = NULL, aes(x = b, y = 0, xend = b, yend = dt(b, df = df)), linetype = "dashed", linewidth = 0.5) +
      annotate(
        geom = "text", x = a, y = dt(a, df = df) / 2, label = "a",
        fontface = "plain", angle = 90, vjust = -1
      ) +
      annotate(
        geom = "text", x = b, y = dt(b, df = df) / 2, label = "b",
        fontface = "plain", angle = 90, vjust = -1
      ) +
      guides(color = guide_legend(override.aes = list(size = 2)))
  }


if(output=="overlay") {
  p<-ggplot() +
    stat_function(
      fun = dnorm,
      args = list(
        mean = mean,
        sd = sd
      ),
      aes(color = paste("Normal ", "(mean = ", mean, ", sd= ", sd, ")\nP(a < X < b) = ", round(pnorm(b, mean = mean, sd = sd) - pnorm(a, mean = mean, sd = sd), 3), sep = "")),
      linewidth = 1
    ) +
    stat_function(
      fun = dnorm,
      args = list(
        mean = mean,
        sd = sd
      ),
      geom = "area",
      fill = "red", alpha = 0.2,
      xlim = c(a, b)
    ) +
    stat_function(
      fun = dt,
      args = list(
        df = df
      ),
      aes(color = paste("Student's t-distributio ", "(df = ", df, ")\nP(a < X < b) = ", round(pt(b, df = df) - pt(a, df = df), 3), sep = "")),
      linewidth = 1
    ) +
    stat_function(
      fun = dt,
      args = list(
        df = df
      ),
      geom = "area",
      fill = "blue", alpha = 0.1,
      xlim = c(a, b)
    ) +
    xlim(-5, 5) +
    labs(color = NULL, y = "density", x = "") +
    geom_segment(data = NULL, aes(x = a, y = 0, xend = a, yend = max(dt(a, df = df), dnorm(a, mean = mean, sd = sd))), linetype = "dashed", linewidth = 0.5) +
    geom_segment(data = NULL, aes(x = b, y = 0, xend = b, yend = max(dt(b, df = df), dnorm(b, mean = mean, sd = sd))), linetype = "dashed", linewidth = 0.5) +
    annotate(
      geom = "text", x = a, y = max(dt(a, df = df), dnorm(a, mean = mean, sd = sd)) / 2, label = "a",
      fontface = "plain", angle = 90, vjust = -1
    ) +
    annotate(
      geom = "text", x = b, y = max(dt(b, df = df), dnorm(b, mean = mean, sd = sd)) / 2, label = "b",
      fontface = "plain", angle = 90, vjust = -1
    ) +
    guides(color = guide_legend(override.aes = list(size = 2)))
}

return(p)

}



#' Normal vs the Student
#'
#' Plot both the Normal density and the Student density **interactively** using shiny.
#'
#' @return shiny app
#'
#' @export
#'
#' @examples
#' # runPlotDens()
#'
#' @import shiny
runPlotDens <- function() {

  ui <- fluidPage(


    # App title ----
    titlePanel("Normal vs Student"),
    br(),

    sidebarLayout(

    # Inputs ----
    sidebarPanel(width = 3,

      # Sidebar to control the mean ----
      sliderInput(
        inputId = "mean",
        label = "Mean - Normal",
        min = -5, max = 5, value = 0, step = 0.5
      ),

      # Sidebar to control the sd ----
      sliderInput(
        inputId = "sd",
        label = "Standard Deviation ('sd') - Normal",
        min = 0.1, max = 3, value = 1, step = 0.1
      ),
      hr(),

      # Sidebar to control df ----
      sliderInput(
        inputId = "df",
        label = "Degree of freedom ('df') - Student",
        min = 1, max = 50, value = 1, step = 1
      ),

      # Sidebar to control the a-b interval ----
      sliderInput(
        inputId = "range",
        label = "Interval (a,b):",
        min = -5, max = 5, value = c(a = -1, b = 1), step = 0.5,
        animate = animationOptions(interval = 800, loop = TRUE)
      ),

      # radioButton to control the output ----
     radioButtons("out", label = "Output",
                   choices = list("Overlay" = "overlay", "Normal" = "norm", "Student" = "t"), inline = TRUE)
    ),

    # Main panel for displaying outputs ----
    mainPanel(width = 9,

      # Output: Table summarizing the values entered ----
      plotOutput("plot", height = 580)
    )
  )
  )


  server <- function(input, output) {
    output$plot <- renderPlot({
      print(PlotDens(df = input$df, mean = input$mean, sd = input$sd, a = input$range[1], b = input$range[2], output = input$out)
      )
    })
  }

  runApp(shinyApp(ui, server))
}
