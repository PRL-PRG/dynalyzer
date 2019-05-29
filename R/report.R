
#' @export
report <- function(object, ...) UseMethod("report")


#' @export
report.dynalyzer.report.template <- function(report_template, settings) {

    dir_create(path_dir(settings$report_output_filepath))

    ## force path computation to ensure that they are resolved
    ## with respect to the the current working directory.
    ## if passed as relative paths to render function, they are
    ## resolved with respect to the directory that contains
    ## the report.
    input <- path_abs(get_filepath(report_template))
    output <- path_abs(settings$report_output_filepath)
    summarized_data_dirpath <- path_abs(settings$summarized_data_dirpath)
    visualized_data_dirpath <- path_abs(settings$visualized_data_dirpath)
    latex_filepath <- path_abs(settings$latex_filepath)

    rmarkdown::render(input = input,
                      output_file = output,
                      runtime = "auto",
                      params = list(summarized_data_dirpath = summarized_data_dirpath,
                                    visualized_data_dirpath = visualized_data_dirpath,
                                    latex_filepath = latex_filepath,
                                    binary = settings$binary,
                                    compression_level = settings$compression_level))
}


#' @export
create_report_settings <- function(report_output_filepath,
                                   summarized_data_dirpath,
                                   visualized_data_dirpath,
                                   latex_filepath,
                                   binary,
                                   compression_level) {
    structure(list(report_output_filepath = report_output_filepath,
                   summarized_data_dirpath = summarized_data_dirpath,
                   visualized_data_dirpath = visualized_data_dirpath,
                   latex_filepath = latex_filepath,
                   binary = binary,
                   compression_level = compression_level),
              class = "dynalyzer.settings.report")
}


#' @export
parse_report_settings <- function(args = commandArgs(trailingOnly = TRUE)) {

    usage <- "%prog report-template-filepath report-output-filepath summarized-data-dirpath visualized-data-dirpath"

    description <- paste(
        "report-output-filepath     path to the output report file (html/pdf)",
        "summarized-data-dirpath    directory containing summarized data files",
        "visualized-data-dirpath    directory containing visualized data",
        "latex-filepath             file containing latex macros and tables",
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

    arguments <- parse_args2(option_parser, args)

    create_report_settings(report_output_filepath = arguments$args[1],
                           summarized_data_dirpath = arguments$args[2],
                           visualized_data_dirpath = arguments$args[3],
                           latex_filepath = arguments$args[4],
                           binary = arguments$options$binary,
                           compression_level = arguments$options$compression_level)
}

