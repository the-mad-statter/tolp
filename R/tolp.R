#' Calibration Instructions Plot
calibration_instructions_plot <- function() {
  graphics::par(mar = rep(2, 4))

  plot(
    -1, -1,
    xaxs = "i", yaxs = "i",
    xlab = "", ylab = "",
    xlim = 0:1, ylim = 0:1,
    xpd = NA
  )
  set.seed(42)
  x <- stats::runif(100)
  y <- x + stats::rnorm(100, sd = 0.2)
  d <- data.frame(x = x, y = y)
  m <- stats::lm(y ~ x, d)
  conf <- stats::predict(m, interval = "conf")
  colnames(conf) <- paste0("conf_", colnames(conf))
  suppressWarnings({
    pred <- stats::predict(m, interval = "pred")
  })
  colnames(pred) <- paste0("pred_", colnames(pred))
  p <- cbind(d, conf, pred)
  p <- p[order(p$x), ]
  graphics::polygon(
    c(p$x, rev(p$x)),
    c(p[, "pred_lwr"], rev(p[, "pred_upr"])),
    col = grDevices::rgb(128, 128, 128, 50, maxColorValue = 225),
    border = NA
  )
  graphics::lines(p$x, p[, "pred_lwr"], lty = "dotted")
  graphics::lines(p$x, p[, "pred_upr"], lty = "dotted")
  graphics::polygon(
    c(p$x, rev(p$x)),
    c(p[, "conf_lwr"], rev(p[, "conf_upr"])),
    col = grDevices::rgb(128, 128, 128, 100, maxColorValue = 225),
    border = NA
  )
  graphics::lines(p$x, p[, "conf_lwr"], lty = "dashed")
  graphics::lines(p$x, p[, "conf_upr"], lty = "dashed")
  graphics::points(d$x, d$y)
  graphics::abline(m)

  x <- c(0.1, 0.9, 0, 0)
  y <- c(0, 0, 0.1, 0.9)
  graphics::points(
    x, y,
    cex = 4,
    col = grDevices::rgb(1, 0, 0, 1),
    pch = 19,
    xpd = NA
  )
  graphics::text(
    x = x, y = y,
    labels = c(
      expression(P[1]),
      expression(P[2]),
      expression(P[3]),
      expression(P[4])
    ),
    xpd = NA
  )
}

#' tolp
#'
#' Run a shiny app to tolp data points from a plot image.
#'
#' @return data.frame with columns g, x, and y containing tolped data points
#' @export
#'
#' @examples
#' \dontrun{
#' my_tolped_data <- tolp()
#' }
tolp <- function() {
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$script(tolp_resource("resources", "js", "dt_listeners.js")),
      shiny::tags$script(tolp_resource("resources", "js", "shiny_custom_message_handlers.js")),
      shiny::tags$style(tolp_resource("resources", "css", "stylesheet.css"))
    ),
    shiny::titlePanel("tolp"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::conditionalPanel(
          "false",
          shiny::selectInput("mode", "Mode", c("Import Image", "Calibrate", "Reverse Plot"))
        ),
        shiny::conditionalPanel(
          "input.mode == 'Import Image'",
          shiny::tags$h1("Import Image"),
          shiny::fileInput("image_upload_button", "Choose file to upload")
        ),
        shiny::conditionalPanel(
          "input.mode == 'Calibrate'",
          shiny::tags$h1("Calibrate"),
          shiny::tags$h5("Using the order shown in red circles in the example plot below, click four known points on the axes in your plot to the right."),
          shiny::plotOutput("calibration_instructions_plot", width = "225px", height = "225px"),
          shiny::tags$h5("Then enter the X-values of the two points clicked on the X-axis and the Y-values of the two points clicked on the Y-axis."),
          shiny::fluidRow(
            shiny::column(4, shiny::numericInput("calibration_x1", shiny::HTML(paste0("P", shiny::tags$sub(1), " (x-value)")), NA)),
            shiny::column(4, shiny::numericInput("calibration_x2", shiny::HTML(paste0("P", shiny::tags$sub(2), " (x-value)")), NA)),
            shiny::column(4, shiny::selectInput("calibration_x_is_logged", "X is Log Scale", c("No", "Yes")))
          ),
          shiny::fluidRow(
            shiny::column(4, shiny::numericInput("calibration_y1", shiny::HTML(paste0("P", shiny::tags$sub(3), " (y-value)")), NA)),
            shiny::column(4, shiny::numericInput("calibration_y2", shiny::HTML(paste0("P", shiny::tags$sub(4), " (y-value)")), NA)),
            shiny::column(4, shiny::selectInput("calibration_y_is_logged", "Y is Log Scale", c("No", "Yes")))
          ),
          shiny::actionButton("calibration_confirmation_button", "Confirm Calibration")
        ),
        shiny::conditionalPanel(
          "input.mode == 'Reverse Plot'",
          shiny::tags$h1("Reverse Plot"),
          shiny::tags$h5("Hover over a point and click to record or double click to remove."),
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::wellPanel(
                shiny::selectInput("group_names", "Current Group", 1),
                shiny::actionButton("add_group_button", "Add Group")
              ),
              DT::DTOutput("data"),
              shiny::wellPanel(
                shiny::fluidRow(
                  shiny::tags$div(class = "exit-button", title = "Optionally, export tolp data as .csv", shiny::downloadButton("export_csv_button", "Export .csv")),
                  shiny::tags$div(class = "exit-button", title = "Exit and return data to R", shiny::actionButton("exit_button", "Exit"))
                )
              )
            )
          )
        ),
      ),
      shiny::mainPanel(
        shiny::plotOutput("plot", click = "plot_click", dblclick = "plot_dblclick", height = "auto")
      )
    )
  )

  server <- function(input, output, session) {
    vals <- shiny::reactiveValues(
      tolped_data = data.frame(g = numeric(0), x = numeric(0), y = numeric(0)),
      calibration_data = data.frame(x = numeric(0), y = numeric(0)),
      calibration_coefs = list(x = c(NA, NA), y = c(NA, NA)),
      plot_aspect_ratio = 1,
      group_names = 1
    )

    shiny::observeEvent(input$add_group_button, {
      new_group <- max(vals$group_names) + 1
      vals$group_names <- c(vals$group_names, new_group)
      shiny::updateSelectInput(session, "group_names", "Group", vals$group_names, new_group)
    })

    image <- shiny::reactive({
      img <- input$image_upload_button
      if (is.null(img$datapath)) {
        return(NULL)
      }
      shiny::updateSelectInput(session, "mode", selected = "Calibrate")
      img <- magick::image_read(img$datapath)
      img_info <- magick::image_info(img)
      vals$plot_aspect_ratio <- img_info$width / img_info$height
      img
    })

    shiny::observeEvent(input$calibration_confirmation_button, {
      vals$calibration_coefs$x <- calibration_coefs(
        vals$calibration_data$x[1:2],
        c(input$calibration_x1, input$calibration_x2),
        input$calibration_x_is_logged == "Yes"
      )

      vals$calibration_coefs$y <- calibration_coefs(
        vals$calibration_data$y[3:4],
        c(input$calibration_y1, input$calibration_y2),
        input$calibration_y_is_logged == "Yes"
      )

      shiny::updateSelectInput(session, "mode", selected = "Reverse Plot")
    })

    shiny::observeEvent(input$plot_click, {
      if (input$mode == "Calibrate") {
        if (nrow(vals$calibration_data) < 4) {
          vals$calibration_data <- rbind(
            vals$calibration_data,
            data.frame(
              x = input$plot_click$x,
              y = input$plot_click$y
            )
          )
        }
      }

      if (input$mode == "Reverse Plot") {
        vals$tolped_data <- rbind(
          vals$tolped_data,
          data.frame(
            g = as.numeric(input$group_names),
            x = input$plot_click$x,
            y = input$plot_click$y
          )
        )
      }
    })

    shiny::observeEvent(input$plot_dblclick, {
      if (input$mode == "Calibrate") {
        n <- nrow(vals$calibration_data)
        if (0 < n & n < 5) {
          vals$calibration_data <- vals$calibration_data[-n, ]
        }
      }

      if (input$mode == "Reverse Plot") {
        distances <- vapply(
          seq_len(nrow(vals$tolped_data)),
          \(i) {
            x1 <- vals$tolped_data$x[i]
            x2 <- input$plot_dblclick$x
            y1 <- vals$tolped_data$y[i]
            y2 <- input$plot_dblclick$y
            sqrt((x2 - x1)^2 + (y2 - y1)^2)
          },
          numeric(1)
        )
        i <- which.min(distances)
        vals$tolped_data <- vals$tolped_data[-i, ]
      }
    })

    output$plot <- shiny::renderPlot(
      {
        input$plot_click
        input$plot_dblclick

        graphics::par(mai = c(0, 0, 0, 0))

        plot(
          x = -1, y = -1,
          xlim = c(0, 1), ylim = c(0, 1),
          type = "n",
          xaxs = "i", yaxs = "i",
          xlab = "", ylab = "",
          axes = FALSE
        )

        if (!is.null(image())) {
          lim <- graphics::par()
          graphics::rasterImage(image(), lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
          graphics::grid()
        }

        graphics::box()

        if (input$mode == "Calibrate") {
          with(vals$calibration_data, points(x, y, cex = 2, col = 1:4, pch = 1:4))
        }

        if (input$mode == "Reverse Plot") {
          with(vals$tolped_data, points(x, y, cex = 2, col = g, pch = g))
        }
      },
      height = function() {
        1 / vals$plot_aspect_ratio * session$clientData$output_plot_width
      }
    )

    page_length <- shiny::reactiveVal(10)
    shiny::observeEvent(input$page_length, {
      page_length(input$page_length)
    })

    output$data <- DT::renderDT({
      if (nrow(vals$tolped_data) > 0) {
        n <- nrow(vals$tolped_data)
        l <- shiny::isolate(page_length())
        display_start <- (ceiling(n / l) - 1) * l

        DT::datatable(
          data = data.frame(
            g = vals$tolped_data$g,
            x = tolp_scale(vals$tolped_data$x, vals$calibration_coefs$x, input$calibration_x_is_logged == "Yes"),
            y = tolp_scale(vals$tolped_data$y, vals$calibration_coefs$y, input$calibration_y_is_logged == "Yes")
          ),
          options = list(
            displayStart = display_start,
            pageLength = shiny::isolate(page_length()),
            search = list(regex = TRUE, caseInsensitive = FALSE)
          ),
          callback = DT::JS("add_length_listener(table);"),
          editable = TRUE
        ) |>
          DT::formatRound(2:3, 4)
      }
    })

    shiny::observeEvent(input$data_cell_edit, {
      row <- input$data_cell_edit$row
      col <- input$data_cell_edit$col
      val <- input$data_cell_edit$value

      if (is.numeric(val)) {
        if (col == 1) {
          vals$tolped_data$g[row] <- val
        } else if (col == 2) {
          vals$tolped_data$x[row] <- tolp_scale(val, vals$calibration_coefs$x, input$calibration_x_is_logged == "Yes", TRUE)
        } else if (col == 3) {
          vals$tolped_data$y[row] <- tolp_scale(val, vals$calibration_coefs$y, input$calibration_y_is_logged == "Yes", TRUE)
        }
      } else {
        vals$tolped_data <- vals$tolped_data[-row, ]
      }
    })

    shiny::observe({
      if (input$mode == "Reverse Plot") {
        tolp_data <<- data.frame(
          g = vals$tolped_data$g,
          x = tolp_scale(vals$tolped_data$x, vals$calibration_coefs$x, input$calibration_x_is_logged == "Yes"),
          y = tolp_scale(vals$tolped_data$y, vals$calibration_coefs$y, input$calibration_y_is_logged == "Yes")
        )
      }
    })

    output$export_csv_button <- shiny::downloadHandler(
      filename = function() {
        "tolp.csv"
      },
      content = function(file) {
        utils::write.csv(
          data.frame(
            g = vals$tolped_data$g,
            x = tolp_scale(vals$tolped_data$x, vals$calibration_coefs$x, input$calibration_x_is_logged == "Yes"),
            y = tolp_scale(vals$tolped_data$y, vals$calibration_coefs$y, input$calibration_y_is_logged == "Yes")
          ),
          file
        )
      }
    )

    shiny::observeEvent(input$exit_button, {
      session$sendCustomMessage(type = "closeWindow", list(message = "window.close();"))
      shiny::stopApp()
    })

    output$calibration_instructions_plot <- shiny::renderPlot({
      calibration_instructions_plot()
    })
  }

  tolp_data <- NA
  print(shiny::shinyApp(ui = ui, server = server))
  tolp_data
}
