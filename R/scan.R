#' @export
scan <- function(object, ...) UseMethod("scan")

#' @export
scan.dynalyzer.analysis.group <- function(analysis_group, settings) {

    info("=> Scanning '", settings$input_dirpath, "'.\n")

    reduced_analyses <-
        settings$input_dirpath %>%
        dir_ls(type = "directory")

    ## we use this to ensure that for any program,
    ## these many FINISH files exist in the reduced
    ## data directory
    analyses_count <- length(reduced_analyses)

    ## TODO - figure out how to remove dplyr package name from the summarize call
    all_scripts <-
        settings$input_dirpath %>%
        ## analyses
        dir_ls(type = "directory") %>%
        ## package names
        dir_ls(type = "directory") %>%
        ## script types
        dir_ls(type = "directory") %>%
        ## script names
        dir_ls(type = "directory") %>%
        {tibble(script_dirpath = .)} %>%
        mutate(path_components = path_split(script_dirpath)) %>%
        mutate(path_length = lengths(path_components)) %>%
        mutate(script_name = map2_chr(path_components, path_length, `[`),
               script_type = map2_chr(path_components, path_length - 1, `[`),
               package = map2_chr(path_components, path_length - 2, `[`),
               analysis = map2_chr(path_components, path_length - 3, `[`),
                finish_filepath = path(script_dirpath, "FINISH"),
               noerror_filepath = path(script_dirpath, "NOERROR")) %>%
        mutate(finish_and_noerror = file_exists(finish_filepath) & file_exists(noerror_filepath)) %>%
        group_by(package, script_type, script_name) %>%
        print() %>%
        dplyr::summarize(analyses = str_c("(", str_c(analysis, collapse = " "), ")"),
                         finish_and_noerror = all(finish_and_noerror),
                         all_analyses = length(analysis) == analyses_count) %>%
        ungroup() %>%
        mutate(valid_script_type = script_type %in% settings$script_types) %>%
        mutate(valid = valid_script_type & finish_and_noerror & all_analyses)

    info("Found ", sum(all_scripts$valid), " valid programs.\n")

    info("Found ", sum(!all_scripts$valid), " invalid programs.\n")

    info("Writing all scripts to '",  settings$all_scripts_filepath, "'.\n")

    all_scripts %>%
        write_csv(path(settings$all_scripts_filepath))

    info("Writing valid scripts to '",  settings$valid_scripts_filepath, "'.\n")

    all_scripts %>%
        filter(valid) %>%
        mutate(script_dirpath = path(package, script_type, script_name)) %>%
        select(script_dirpath) %>%
        write_csv(path(settings$valid_scripts_filepath), col_names = FALSE)

    info("Writing invalid scripts to '",  settings$invalid_scripts_filepath, "'.\n")

    all_scripts %>%
        filter(!valid) %>%
        write_csv(path(settings$invalid_scripts_filepath))

    info("=> Finish scanning '", settings$input_dirpath, "'.\n")

    all_scripts
}

#' @export
create_scan_settings <- function(input_dirpath,
                                 all_scripts_filepath,
                                 valid_scripts_filepath,
                                 invalid_scripts_filepath,
                                 script_types) {
    structure(list(input_dirpath = input_dirpath,
                   all_scripts_filepath = all_scripts_filepath,
                   valid_scripts_filepath = valid_scripts_filepath,
                   invalid_scripts_filepath = invalid_scripts_filepath,
                   script_types = script_types),
              class = "dynalyzer.settings.scan")
}

#' @export
parse_scan_settings <- function(args = commandArgs(trailingOnly = TRUE)) {

    usage <- "%prog reduced-output-dirpath combined-output-dirpath analysis combine-count"
    description <- paste(
        "reduced-output-dirpath    directory containing reduced data files (scanned recursively)",
        "all-scripts-filepath      file to which information about all scripts will be exported",
        "valid-scripts-filepath    file to which information about valid scripts will be exported",
        "invalid-scripts-filepath  file to which information about invalid scripts will be exported",
        sep = "\n")


    option_list <- list(
        make_option(c("--vignettes"),
                    action="store_true",
                    default=FALSE,
                    help="combine reduced data from vignettes",
                    metavar="vignettes"),

        make_option(c("--examples"),
                    action="store_true",
                    default=FALSE,
                    help="combine reduced data from examples",
                    metavar="examples"),

        make_option(c("--tests"),
                    action="store_true",
                    default=FALSE,
                    help="combine reduced data from tests",
                    metavar="tests")
    )


    option_parser <- OptionParser(usage = usage,
                                  description = description,
                                  add_help_option = TRUE,
                                  option_list = option_list)

    arguments <- parse_args2(option_parser, args = args)

    script_types <- c()

    if(arguments$options$vignettes) script_types <- c(script_types, "doc")
    if(arguments$options$examples) script_types <- c(script_types, "examples")
    if(arguments$options$tests) script_types <- c(script_types, "tests")

    if(length(script_types) == 0) {
        stop("script type not specified (--vignettes, --examples, --tests)")
    }

    create_scan_settings(input_dirpath = arguments$args[1],
                         all_scripts_filepath = arguments$args[2],
                         valid_scripts_filepath = arguments$args[3],
                         invalid_scripts_filepath = arguments$args[4],
                         script_types = script_types)
}
