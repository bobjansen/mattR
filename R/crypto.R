#' getRandomString
#'
#' Return a base64 encoded random number of the given lenght. The length must be
#' a multiple of 4.
#'
#' The source of randomness is \code{openssl::rand_byes}.
#'
#' @param length The length of the random output.
#'
#' @import httpuv openssl
#' @return A base64 encoded number with \code{length} digits.
#' @export
#'
#' @examples
#' getRandomString(16)
getRandomString <- function(length) {
  if (length %% 4L != 0L) {
    stop("Random strings should be a multiply of 4.")
  }

  httpuv::rawToBase64(openssl::rand_bytes(length / 4L * 3L))
}

