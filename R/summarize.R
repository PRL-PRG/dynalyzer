#' @export
summarize <- function(object, ...) UseMethod("summarize")

#' @export
summarize.dynalyzer.analysis.group <- function(analysis_group, settings) {

    dir_create(settings$output_dirpath)

    ## TODO - move this out or refactor it to make this function smaller and readable
    scan_input_dirpath <- function(settings) {

        info("=> Scanning for combined data files in ", settings$input_dirpath, "\n")

        combined_data_table <-
            settings$input_dirpath %>%
            dir_ls(type = "file") %>%
            {tibble(filepath = .)}

        info("=> Found ", nrow(combined_data_table), " data files\n")

        combined_data_table
    }

    info("=> Starting summarization\n")

    combined_data_table <- scan_input_dirpath(settings)

    summarizer <-
        analysis_group %>%
        purrr::detect(function(analysis) get_id(analysis) == settings$analysis) %>%
        get_summarizer()

    combined_data_filepaths <- combined_data_table$filepath

    summarized_data_filepaths <-
        combined_data_filepaths %>%
        path_ext_remove() %>%
        path_ext_remove()

    analyses <- new.env(parent = emptyenv(), hash = TRUE)

    map(summarized_data_filepaths,
        function(summarized_data_filepath) {
            delayedAssign(path_file(summarized_data_filepath),
                          read_data_table(summarized_data_filepath,
                                          binary = settings$binary,
                                          compression_level = settings$compression_level),
                          assign.env = analyses)
        })

    analyses %>%
        summarizer() %>%
        imap(
            function(df, name) {
                output_filepath <- path(settings$output_dirpath, name)

                info("=> Writing ", output_filepath, "\n")

                promisedyntracer::write_data_table(df, output_filepath,
                                                   truncate = TRUE,
                                                   binary = settings$binary,
                                                   compression_level = settings$compression_level)
                path(output_filepath,
                     ext = promisedyntracer::data_table_extension(settings$binary,
                                                                  settings$compression_level))
            }
        )

    info("=> Finished summarization\n\n")

    tibble(filepath = unlist(summarized_data_filepaths))
}

#' @export
create_summarize_settings <- function(input_dirpath,
                                      output_dirpath,
                                      analysis,
                                      table_names,
                                      binary,
                                      compression_level) {

    structure(list(input_dirpath = input_dirpath,
                   output_dirpath = output_dirpath,
                   analysis = analysis,
                   table_names = table_names,
                   binary = binary,
                   compression_level = compression_level),
              class = "dynalyzer.settings.summarize")
}

#' @export
parse_summarize_settings <- function(args = commandArgs(trailingOnly = TRUE)) {

    usage <- "%prog combined-output-dirpath summarized-output-dirpath"
    description <- paste(
        "combined-output-dirpath    directory containing combined data files",
        "summarized-output-dirpath  directory to which summarized data will be exported",
        "analysis                   name of analysis to run",
        sep = "\n")

    option_list <- list(
        make_option(c("--binary"),
                    action = "store_true",
                    default = FALSE,
                    help = "read data in binary format",
                    metavar = "binary"),

        make_option(c("--compression-level"),
                    action = "store",
                    type = "integer",
                    default = 0,
                    help = "compression level",
                    metavar = "compression_level")
    )

    option_parser <- OptionParser(usage = usage,
                                  description = description,
                                  add_help_option = TRUE,
                                  option_list = option_list)

    arguments <- parse_args2(option_parser, args = args)

    analysis_map <- list(
        object_type = list("object_count_distribution_by_type"),

        arguments = list("promise_distribution_by_source",
                         "argument_distribution_by_type",
                         "promise_argument_distribution_by_category",
                         "promise_distribution_by_sharing")
    )

    create_summarize_settings(input_dirpath = arguments$args[1],
                              output_dirpath = arguments$args[2],
                              analysis = arguments$args[3],
                              table_names = analysis_map[[arguments$args[3]]],
                              binary = arguments$options$binary,
                              compression_level = arguments$options$compression_level)

}
