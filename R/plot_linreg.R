#' Diagnostic plots for a linreg object
#'
#' Creates a Residuals vs Fitted plot and a Scale-Location plot.
#'
#' @param x An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return Invisibly returns \code{NULL} and displays two diagnostic plots.
#' @export

plot.linreg <- function(x, ...) {

    # Check that x is a linreg object
    if (!inherits(x, "linreg")) stop("x must be an object of class 'linreg'")

    # Create a data frame containing the values needed for the plots.
    # as.numeric() converts n x 1-matrices into regular numeric vectors that can be used naturally as columns in a data frame.
    plot_data <- data.frame(
      observation = seq_along(x$e_hat), # Create one observation number for each residual.
      fitted = as.numeric(x$y_hat),
      residual = as.numeric(x$e_hat))

    # Calculate the diagonal elements of the hat matrix
    hat_matrix <- x$X %*% solve(t(x$X) %*% x$X) %*% t(x$X)
    leverage <- diag(hat_matrix)

    # Calculate standardized residuals
    plot_data$standardized_residual <- plot_data$residual / (sqrt(x$sigma2_hat) * sqrt(1 - leverage))

    # Calculate the values used in the Scale-Location plot
    plot_data$scale_location <- sqrt(abs(plot_data$standardized_residual))

    # Identify the three observations with the largest absolute residuals (For example, c(99,118,119) in the example)
    number_of_labels <- min(3, nrow(plot_data))

    # Here, we sort the absolute value of residuals in a decreasing order and then choose the 3 (It can be less than 3 if plot_data has less then 3 rows) biggest one
    labelled_observations <- order(abs(plot_data$residual),decreasing = TRUE)[seq_len(number_of_labels)]

    # Store the row positions of the observations with the largest Scale-Location values
    labelled_scale_observations <- order(plot_data$scale_location,decreasing = TRUE)[seq_len(number_of_labels)]

    # Create the Residuals vs Fitted plot
    residual_plot <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = fitted, y = residual)
    ) +
      ggplot2::geom_point(shape = 1) + # 1 hollow circle for each observation

      # Calculate the median residual for each fitted value and connect the medians with a red line
      ggplot2::stat_summary(

        # The x and y aesthetics do not need to be specified again because
        # this layer inherits them from the main ggplot() call.
        # group = 1 (group identifier) places all summary points in one group, allowing geom = "line" to connect them.
        ggplot2::aes(group = 1),
        fun = median,
        geom = "line",
        colour = "red",
        linewidth = 0.7
      ) +

      # Add observation numbers to the selected observations
      ggplot2::geom_text(
        data = plot_data[labelled_observations, ],
        ggplot2::aes(label = observation), # Use the observation column as the text displayed beside each point.
        vjust = -0.5 # text moved farther upward
        ) +
      ggplot2::labs(
        title = "Residuals vs Fitted",
        x = "Fitted values",
        y = "Residuals"
      ) +
      ggplot2::theme_minimal()

    # Create the Scale-Location plot
    scale_location_plot <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = fitted, y = scale_location)
    ) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::stat_summary(
        ggplot2::aes(group = 1),
        fun = median,
        geom = "line",
        colour = "red",
        linewidth = 0.7
      ) +
      ggplot2::geom_text(
        data = plot_data[labelled_scale_observations, ],
        ggplot2::aes(label = observation),
        vjust = -0.5,
        check_overlap = TRUE
      ) +
      ggplot2::labs(
        title = "Scale-Location",
        x = "Fitted values",
        y = expression(sqrt("|Standardized residuals|"))
      ) +
      ggplot2::theme_minimal()

    # Display both plots
    print(residual_plot)
    print(scale_location_plot)

    # Explicitly return no visible value
    invisible(NULL)
    }
