#' Generate a pk_model_network object for subsequent layout and plotting
#'
#' @inheritParams pk_model_network_states
#' @inheritParams pk_model_network_edges
#' @family Network setup
#' @export
pk_model_network <- function(state, state_type = NA_character_, from, to, edge_type = "mass") {
  checkmate::assert_choice(from, choices = state)
  checkmate::assert_choice(to, choices = state)
  as_pk_model_network(
    states = pk_model_network_states(state = state, state_type = state_type),
    edges = pk_model_network_edges(from = from, to = to, edge_type = edge_type)
  )
}

#' @describeIn pk_model_network Generate a pk_model_network object from two
#'   data.frames
#' @param states The model states (typically the compartments in the model)
#' @param edges The connectivity of the model
#' @export
as_pk_model_network <- function(states, edges) {
  checkmate::assert_data_frame(states)
  checkmate::assert_data_frame(edges)
  checkmate::assert_true(all(c("name", "type") %in% names(states)))
  checkmate::assert_true(all(c("from", "to", "type") %in% names(edges)))
  ret <- list(states = states, edges = edges)
  class(ret) <- "pk_model_network"
  ret
}

#' Define all of the states in the model
#'
#' @param state The model state names as a character vector
#' @param state_type The type of state in the model (currently unused)
#' @return A data.frame with the state \code{name} and the \code{type} as two
#'   columns
#' @family Network setup
#' @examples
#' model_network_states(c("depot", "central", "elimination"), type = "pk")
#' @export
pk_model_network_states <- function(state, state_type) {
  checkmate::assert_character(state, min.len = 1, any.missing = FALSE)
  checkmate::assert_character(state_type, min.len = 1)
  data.frame(
    name = state,
    type = state_type
  )
}

#' Define all of the states in the model
#'
#' @param state The model state names as a character vector
#' @param edge_type The type of edge in the model. Typically, \code{edge_type}
#'   will be \code{"mass"} for mass transfer and \code{"effect"} for an effect
#'   from one compartment to another without a mass transfer
#' @return A data.frame with the network edges as \code{from} and \code{to} and
#'   the \code{type} of edge as three columns
#' @family Network setup
#' @examples
#' # For an oral, one-compartment PK model
#' model_network_edges(
#'   from=c("depot", "central"),
#'   to = c("central", "elimination")
#' )
#' @export
pk_model_network_edges <- function(from, to, edge_type) {
  checkmate::assert_character(from, min.len = 1, any.missing = FALSE)
  checkmate::assert_character(to, len = length(from), any.missing = FALSE)
  checkmate::assert_character(type)
  data.frame(
    from = from,
    to = to,
    type = type
  )
}
