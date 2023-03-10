#' Generate a PK model layout
#'
#' @param network The network states and edges as generated by
#'   \code{pk_model_network}
#' @param method The layout method to use
#' @param ... Passed to the layout method functions
#' @return The network with the states having \code{x} and \code{y} columns
#'   added
#' @family Network layout
#' @export
pk_model_layout <- function(network, method="force_directed_biased", ...) {
  checkmate::assert_class(network, classes = "pk_model_network")
  class(network) <- c(paste("pk_model_network", method, sep = "_"), class(network))
  UseMethod("pk_model_layout")
}

#' @describeIn pk_model_layout Layout a force-directed, biased network
#' @param pin The state to pin at the x,y location of 0,0
#' @param pull A list of states to pull away from the pinned state with the
#'   direction to pull.
#' @param distance The default, approximate distance between two connected nodes
#'   (defining the Coulomb and Hook forces)
#' @param max_step The maximum step size for the relaxation of the distances
#'   between states
#' @param tol When all step distances are less than \code{tol}, the relaxation
#'   of the locations will stop
pk_model_layout.pk_model_network_force_directed_biased <- function(network, method, ..., pin, pull, distance = 1, max_step = 0.2, tol = 0.01) {
  checkmate::assert_choice(pin, network$states$name, null.ok = FALSE)
  checkmate::assert_list(pull)
  checkmate::assert_names(names(pull), type = "named", subset.of = network$states$name)
  checkmate::assert_number(distance, lower = 0, finite = TRUE, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_number(max_step, lower = 0, finite = TRUE, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_number(tol, lower = 0, finite = TRUE, na.ok = FALSE, null.ok = FALSE)
  stopifnot(length(pull) == 1) # TODO: Allow longer "pull" values

}

model_network_stretch_initialize <- function(states, edges, pin, direction = c(x = 0, y = 1)) {
  browser()
  checkmate::assert_character(pin, any.missing = FALSE)
  checkmate::assert_choice(pin, choices = states$name)

  magnitude <- sqrt(sum(direction^2))
  ortho_direction <-
    as.vector(
      direction %*% matrix(data = c(0, 1, -1, 0), nrow = 2)
    )

  current_state <- pin
  other_states <- setdiff(states$name, pin)

  locations <- data.frame(name = pin, x = 0, y = 0)
  while (length(other_states) > 0) {
    connected_states <-
      unique(c(
        edges$from[edges$to %in% current_state],
        edges$to[edges$from %in% current_state]
      ))

  }

}

model_find_shortest_path <- function(edges, from, to) {
  all_choices <- model_find_shortest_path_helper(edges = edges, from = from, to = to)
  browser()
}

model_find_shortest_path_helper <- function(edges, from, to, current_path = NULL) {
  if (from == to) {
    return(list(c(current_path, to)))
  } else if (from %in% current_path) {
    # loop
    return(list())
  } else if (!(from %in% edges$from)) {
    # no path from the current "from" node
    return(list())
  } else if (!(to %in% edges$to)) {
    # no path to the current "to" node
    return(list())
  }
  ret <- list()
  for (idx_from in which(edges$from %in% from)) {
    current_ret <-
      model_find_shortest_path_helper(
        edges = edges[-idx_from, ],
        from = edges$to[idx_from],
        to = to,
        current_path = c(current_path, edges$from[idx_from])
      )
    if (length(current_ret) > 0) {
      ret <- append(ret, current_ret)
    }
  }
  ret
}

#' Layout repulsion with electrostatic repulsion equation (Coulomb's law)
#'
#' @inheritParams layout_pk_attraction
#' @param k_electro Coulomb constant
layout_pk_repulsion <- function(states, k_electro=1) {
  browser()
  distance <- stats::dist(cbind(states$x, states$y), method = "euclidean")
}

#' Layout attraction with Hook's law
#'
#' @param states A data.frame with the states in the model.  The data.frame must
#'   have a column called \code{names}, \code{x}, and \code{y} with the state
#'   names and positions.
#' @param k_hook Spring constant for Hook's law
#' @param dist_spring Zero deformation distance for the spring
layout_pk_attraction <- function(states, edges, k_hook=1, dist_spring = 1) {
  browser()
  distance <- stats::dist(cbind(states$x, states$y), method = "euclidean")
}

