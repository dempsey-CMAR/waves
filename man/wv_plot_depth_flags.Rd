% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wv_plot_depth_flags.R
\name{wv_plot_depth_flags}
\alias{wv_plot_depth_flags}
\title{Plot sensor_depth_below_surface_m coloured by roc flag.}
\usage{
wv_plot_depth_flags(dat, plotly_friendly = FALSE)
}
\arguments{
\item{dat}{Data frame of wave data in long format, including
\code{depth_flag} column, as exported from \code{adcp_flag_data()}.}

\item{plotly_friendly}{Logical argument. If \code{TRUE}, the legend will be
plotted when \code{plotly::ggplotly} is called on \code{p}. Default is
\code{FALSE}, which makes the legend look better in a static figure.}
}
\value{
ggplot object. Figure shows sensor_depth_below_surface_m over time,
  coloured by the \code{depth_flag} column.
}
\description{
Plot sensor_depth_below_surface_m coloured by roc flag.
}
