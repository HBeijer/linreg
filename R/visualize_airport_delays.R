#' Visualise mean arrival delays by airport
#'
#' Plots mean arrival delays at destination airports for flights
#' departing New York City in 2013.
#'
#' @details
#' Uses the flights and airports datasets from nycflights13.
#' Flights with missing arrival delays are excluded before averaging.
#' Negative delays represent early arrivals and are included.
#' Airports without matching longitude and latitude are excluded.
#'
#' @return A ggplot object with one point per retained destination airport.
#'   Position shows longitude and latitude; colour shows mean arrival
#'   delay in minutes.
#'
#' @examples
#' visualize_airport_delays()
#'
#' @importFrom rlang .data
#' @export
visualize_airport_delays <- function() {

  # Remove flights whose arrival delay is unknown.
  # Keep negative delays: these flights arrived early.
  # .data$arr_delay refers to the arr_delay column of this dataset.
  flights <- dplyr::filter(nycflights13::flights,!is.na(.data$arr_delay))

  # Put flights with the same destination into the same group.
  # dest contains airport codes such as "LAX" and "ORD".
  grouped_flights <- dplyr::group_by(flights, .data$dest)

  # Replace each group of flights with one row containing its mean delay.
  # .groups = "drop" removes the grouping after summarising.
  airport_delays <- dplyr::summarise(grouped_flights, mean_delay = mean(.data$arr_delay), .groups = "drop")

  # Keep only the airport information needed for the plot.
  # faa is the airport code; lon and lat are its coordinates.
  airport_locations <- dplyr::select(nycflights13::airports, "faa", "name", "lon", "lat")

  # Add coordinates to each destination's mean delay.
  # Match dest in airport_delays with faa in airport_locations.
  # Averaging before joining avoids adding coordinates to every flight.
  plot_data <- dplyr::left_join(airport_delays, airport_locations, by = c("dest" = "faa"))

  # A destination without matching airport information gets NA coordinates.
  # Remove these rows because we cannot place them on the plot.
  plot_data <- dplyr::filter(plot_data, !is.na(.data$lon), !is.na(.data$lat))

  # Each row now represents one airport.
  # Map its location to position and its mean delay to colour.
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$lon,
      y = .data$lat,
      colour = .data$mean_delay
    )
  ) +
    ggplot2::geom_point(size = 3, alpha = 0.85) +

    # Blue means early, grey means on time, and red means late.
    ggplot2::scale_colour_gradient2(
      low = "steelblue",
      mid = "grey80",
      high = "firebrick",
      midpoint = 0,
      name = "Mean arrival\ndelay (min)"
    ) +

    # Adjust the axis proportions for plotting longitude and latitude.
    ggplot2::coord_quickmap() +

    ggplot2::labs(
      title = "Mean arrival delay by destination airport",
      subtitle = "Flights departing New York City in 2013",
      x = "Longitude",
      y = "Latitude",
      caption = "Missing delays and airport coordinates are excluded."
    ) +
    ggplot2::theme_minimal()

  # Return the plot so it can also be saved or customised :)
  return(plot)
}
