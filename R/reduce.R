

#' @export
reduce <- function(object, ...) UseMethod("reduce")

#' @export
reduce.dynalyzer.analysis.group <- function(analysis_group, settings) {

    reducer <-
        analysis_group %>%
        purrr::detect(function(analysis) { get_id(analysis) == settings$analysis }) %>%
        get_reducer()

    raw_analysis_filename_glob <- str_c("*",
                                        data_table_extension(settings$binary,
                                                             settings$compression_level))

    read_raw_analysis_data <- function(input_dirpath,
                                       begin_input_filepath,
                                       finish_input_filepath) {

        analyses <- new.env(parent = emptyenv(), hash = TRUE)

        input_dirpath %>%
            dir_ls(type = "file", recursive = FALSE, glob = raw_analysis_filename_glob) %>%
            map(function(raw_data_filepath) {
                filename <- path_ext_remove(path_ext_remove(path_file(raw_data_filepath)))
                delayedAssign(filename,
                              read_data_table(path(settings$input_dirpath, filename),
                                              binary = settings$binary,
                                              compression_level = settings$compression_level),
                              assign.env = analyses)
            })

        analyses$BEGIN <- read_lines(begin_input_filepath)

        analyses$FINISH <- read_lines(finish_input_filepath)

        analyses
    }

    write_raw_analysis_data <- function(analyses, output_dirpath) {
        analyses %>%
            imap(function(table, table_name) {
                filepath <- path(output_dirpath, table_name)
                write_data_table(table,
                                 filepath,
                                 truncate = TRUE,
                                 binary = settings$binary,
                                 compression_level = settings$compression_level)
                filepath
            })
    }

    info("=> Reducing ", settings$input_dirpath, "\n")

    begin_input_filepath <- path(settings$input_dirpath, "BEGIN")
    finish_input_filepath <- path(settings$input_dirpath, "FINISH")
    noerror_input_filepath <- path(settings$input_dirpath, "NOERROR")

    begin_output_filepath <- path(settings$output_dirpath, "BEGIN")
    finish_output_filepath <- path(settings$output_dirpath, "FINISH")
    error_output_filepath <- path(settings$output_dirpath, "ERROR")
    noerror_output_filepath <- path(settings$output_dirpath, "NOERROR")

    dir_create(settings$output_dirpath)

    file_delete(dir_ls(settings$output_dirpath,
                       recursive = FALSE,
                       type = "file"))

    write_file("", begin_output_filepath)

    tryCatch({

        script_type <- path_file(path_dir(settings$input_dirpath))

        info("=> Script type is ", script_type, "\n")

        valid <- (all(file_exists(c(finish_input_filepath, noerror_input_filepath))) &&
                  script_type %in% settings$script_type)

        if (valid) {

            output_filepaths <-
                read_raw_analysis_data(settings$input_dirpath,
                                       begin_input_filepath,
                                       finish_input_filepath) %>%
                reducer() %>%
                write_raw_analysis_data(settings$output_dirpath)

            write_file("", noerror_output_filepath)

            info("=> Reduced ", settings$input_dirpath, "\n")

            output_filepaths
        }
        else {

            write_file("", error_output_filepath)

            info("=> Invalid ", settings$input_dirpath, "\n")

            ## empty filepaths returned for consistency
            list("")
        }

    },

    error = function(e) {
        write_file("", error_output_filepath)
        info("=> Error reducing ", settings$input_dirpath, "\n")
        stop(e)
    },

    finally = {
        write_file("", finish_output_filepath)
    })
}


#' @export
create_reduce_settings <- function(input_dirpath,
                                    output_dirpath,
                                    analysis,
                                    script_type,
                                    binary,
                                    compression_level) {
    structure(list(input_dirpath = input_dirpath,
                   output_dirpath = output_dirpath,
                   analysis = analysis,
                   script_type = script_type,
                   binary = binary,
                   compression_level = compression_level),
              class = c("dynalyzer.settings.reduce"))
}


#' @export
parse_reduce_settings <- function(args = commandArgs(trailingOnly = TRUE)) {

    usage <- "%prog raw-analysis-dirpath reduced-analysis-dirpath analysis %options"

    description <- paste(
        "raw-analysis-dirpath       directory containing raw data files",
        "reduced-analysis-dirpath   directory to which reduced data will be exported",
        "analysis                   name of analysis to run",
        sep = "\n")


    option_list <- list(
        make_option(c("--vignettes"),
                    action = "store_true",
                    default = FALSE,
                    help = "reduce raw data from vignettes",
                    metavar = "vignettes"),

        make_option(c("--examples"),
                    action = "store_true",
                    default = FALSE,
                    help = "reduce raw data from examples",
                    metavar = "examples"),

        make_option(c("--tests"),
                    action = "store_true",
                    default = FALSE,
                    help = "reduce raw data from tests",
                    metavar = "tests"),

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

    script_type <- c()

    if(arguments$options$vignettes) script_type <- c(script_type, "doc")
    if(arguments$options$examples) script_type <- c(script_type, "examples")
    if(arguments$options$tests) script_type <- c(script_type, "tests")

    if(length(script_type) == 0) {
        stop("script type not specified (--vignettes, --examples, --tests)")
    }

    create_reduce_settings(input_dirpath = arguments$args[1],
                           output_dirpath = arguments$args[2],
                           analysis = arguments$args[3],
                           script_type = script_type,
                           binary = arguments$options$binary,
                           compression_level = as.integer(arguments$options$compression_level))
}

