#' Wrapping function for PEMs with competing risks using stats::glm
#' 
#' This function serves as a wrapper for the glm function (stats) to be 
#' applicable to competing risks. Competing risks are separately modelled 
#' in their own glm. The results can be interpreted synoptically, 
#' using the corresponding methods (summary etc.) or separately using the glm
#' methods for each entry alone.
#' The input is entered as for a single risk PEM.

#' 
#' @param formula an object of class formula or a string convertible to it.
#' The model formula corresponding to a glm (from the stats package).
#' @param family the family of the glm to be modelled. Only poisson is
#'  reasonable. Hence, any other input will not be accepted.
#' @param data A data.frame of class ped_cr that features time-to-event data
#' and convariates. (see https://adibender.github.io/pammtools/)
#' @param offset The offset for each observation. Contained in data.
#' @param ... additional arguments passed to the glm function.
#' @return a list of glms - one entry for a single competing risk.
#' @import checkmate
#' @importFrom stats glm
#' @export
#' @author Philipp Kopper
pem_cr <- function(formula, family = poisson, ped, offset, ...) {
  #check_input(formula, ped, offset)
  res <- vector(mode = "list", length = length(ped))
  for (i in 1:length(res)) {
    res[[i]] <- glm(formula = formula, family = family, 
                    data = ped[[i]], offset = offset, ...)
  }
  names(res) <- attr(ped, "risks")
  which_type <- c("sh", "cs")[c("sh", "cs") %in% class(ped)]
  class(res) <- c("pem_cr", which_type)
  attr(res, "risks") <- attr(ped, "risks")
  #for methods
  return(res)
}