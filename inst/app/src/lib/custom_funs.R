#' @examples
#' nums <- c(422:465)
#' breaks <- c(4, 6, 11, 16, 21, 26)
#' scores <- c(0:6)
#'
#' obj <- e$numals_env[["CR"]][["Australia"]]
#' ## members <- ls(obj)
#' nums <- c(94106, 94200, 94300, 94400)
#' members <- paste0("num", nums)
#' sapply(members, function(x) obj[[x]]) %>% sum()
#'
#' attach(obj)
#' user_fun(breaks, scores)
#' user_fun(breaks=c(4, 6, 11, 16, 21, 26), scores=c(0:6))
#' detach(obj)
#' num94106 <- 1
#' user_fun(breaks, scores, num94106, num94200, num94300, num94400)

user_fun <- function(breaks, scores, ...) {
    ## return -1 if any value is missing
    ## if(any(num94106==-1, num94200==-1, num94300==-1, num94400==-1)) return(-1)
    ## my_sum <- sum(num94106, num94200, num94300, num94400)
    if (any(sapply(list(...), function(x) x==-1))) return(-1)
    my_sum <- sum(...)

    res <-
        data.frame(lower = c(0, breaks),
                   upper = c(breaks, Inf),
                   score = scores,
                   value = my_sum
                   ) %>%
        dplyr::mutate(between = ifelse(value > lower & value <= upper,
                                       TRUE, FALSE)) %>%
        dplyr::filter(between == TRUE) %>%
        dplyr::pull(score)

    return(res)
}
