library(fs)
library(dplyr)
library(magrittr)
library(stringr)
library(fs)
library(purrr)
library(lubridate)
library(optparse)
library(tibble)
library(rlang)
library(tidyr)
library(lubridate)

#' @export
create_analysis <- function(id,
                            reducer,
                            combiner = function(...) NULL,
                            summarizer,
                            reader = function(...) NULL,
                            writer = function(...) NULL) {
    ## for few elements, not hashing should yield better performance
    analysis <- new.env(hash = FALSE, parent = emptyenv())

    analysis$id <- id
    analysis$reducer <- structure(reducer, class = "dynalyzer.reducer")
    analysis$combiner <- structure(combiner, class = "dynalyzer.combiner")
    analysis$summarizer <- structure(summarizer, class = "dynalyzer.summarizer")
    analysis$reader <- reader
    analysis$writer <- writer

    structure(analysis, class = "dynalyzer.analysis")
}

#' @export
group_analyses <- function(...) {
    analyses <- list(...)
    names(analyses) <- map_chr(analyses, get_id)
    structure(analyses, class = "dynalyzer.analysis.group")
}


#' @export
get_id <- function(object, ...) UseMethod("get_id")

#' @export
get_reducer <- function(object, ...) UseMethod("get_reducer")

#' @export
get_combiner <- function(object, ...) UseMethod("get_combiner")

#' @export
get_summarizer <- function(object, ...) UseMethod("get_summarizer")

#' @export
get_reader <- function(object, ...) UseMethod("get_reader")

#' @export
get_writer <- function(object, ...) UseMethod("get_writer")

#' @export
get_id.dynalyzer.analysis <- function(analysis) analysis$id

#' @export
get_reducer.dynalyzer.analysis <- function(analysis) analysis$reducer

#' @export
get_combiner.dynalyzer.analysis <- function(analysis) analysis$combiner

#' @export
get_summarizer.dynalyzer.analysis <- function(analysis) analysis$summarizer

#' @export
get_reader.dynalyzer.analysis <- function(analysis) analysis$reader

#' @export
get_writer.dynalyzer.analysis <- function(analysis) analysis$writer


#' @export
set_id <- function(object, ...) UseMethod("set_id")

#' @export
set_reducer <- function(object, ...) UseMethod("set_reducer")

#' @export
set_combiner <- function(object, ...) UseMethod("set_combiner")

#' @export
set_summarizer <- function(object, ...) UseMethod("set_summarizer")

#' @export
set_reader <- function(object, ...) UseMethod("set_reader")

#' @export
set_writer <- function(object, ...) UseMethod("set_writer")

#' @export
set_id.dynalyzer.analysis <- function(analysis, id) {
    analysis$id <- id
    analysis
}

#' @export
set_reducer.dynalyzer.analysis <- function(analysis, reducer) {
    analysis$reducer <- reducer
    analysis
}

#' @export
set_combiner.dynalyzer.analysis <- function(analysis, combiner) {
    analysis$combiner <- combiner
    analysis
}

#' @export
set_summarizer.dynalyzer.analysis <- function(analysis, summarizer) {
    analysis$summarizer <- summarizer
    analysis
}

#' @export
set_reader.dynalyzer.analysis <- function(analysis, reader) {
    analysis$reader <- reader
    analysis
}

#' @export
set_writer.dynalyzer.analysis <- function(analysis, writer) {
    analysis$writer <- writer
    analysis
}

#' @export
create_reducer_output <- function(...) {
    structure(list(...), class = "dynalyzer.reducer.output")
}

#' @export
create_combiner_output <- function(...) {
    structure(list(...), class = "dynalyzer.combiner.output")
}

#' @export
create_summarizer_output <- function(...) {
    structure(list(...), class = "dynalyzer.summarizer.output")
}
