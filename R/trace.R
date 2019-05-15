
#' @export
build_vignettes <- function(settings) {

    vignettes <- pkgVignettes(settings$package, source=TRUE)

    ## if there are vignettes and there are no vignette sources,
    ## then compile the vignettes to sources. This compilation
    ## will result in .R files in the doc directory of package
    ## and will be picked up by the next step of the program
    if (length(vignettes$docs) != 0 &&
        length(vignettes$sources) == 0) {

        checkVignettes(settings$package,
                       find.package(settings$package)[1],
                       tangle = TRUE,
                       weave = FALSE,
                       workdir = "src")
    }
}


#' @export
copy_vignettes <- function(settings) {

    build_vignettes(settings)

    destination_paths <- path(settings$corpus_dirpath,
                              settings$package,
                              c("doc", "data"))

    vignettes <- vignette(package = settings$package)$results

    if(nrow(vignettes) == 0) {
        dir_create(destination_paths)
    }

    else {
        source_paths <- path(vignettes[1, "LibPath"],
                             vignettes[1, "Package"],
                             c("doc", "data"))
        if(dir_exists(source_paths[1]))
            dir_copy(source_paths[1], destination_paths[1])
        if(dir_exists(source_paths[2]))
            dir_copy(source_paths[2], destination_paths[2])
    }

    "doc"
}


#' @export
copy_tests <- function(settings) {
    destination_path <- path(settings$corpus_dirpath, settings$package, "tests")

    source_path <- path(find.package(settings$package), "tests")

    if(!dir_exists(source_path)) {
        dir_create(destination_path)
    } else {
        dir_copy(source_path, destination_path)
    }

    "tests"
}


#' @export
copy_examples <- function(settings) {
    destination_path <- path(settings$corpus_dirpath,
                             settings$package,
                             "examples")

    dir_create(destination_path)

    db <- tryCatch({
        Rd_db(settings$package)
    }, error=function(e) {
        print(e)
        list()
    })

    iwalk(db, function(rd_data, rd_name) {

        example_filepath <-
            destination_path %>%
            path(path_file(rd_name)) %>%
            path_ext_set("R")

        Rd2ex(rd_data, example_filepath, defines=NULL)

        if(file_exists(example_filepath)) {

            new_content <-
                str_c(str_glue("library({settings$package})"),
                      "",
                      read_file(example_filepath),
                      sep = "\n")

            write_file(new_content,
                       example_filepath)
        }
    })

    "examples"
}


#' @export
wrap_scripts <- function(settings, wrap_script, script_dirname) {


    path(settings$corpus_dirpath, settings$package, script_dirname) %>%
        dir_ls(type = "file", glob = "*.R") %>%
        path_file() %>%
        map_dfr(
            function(script_filename) {
                wrap_script(settings, script_dirname, script_filename)
            }
        )
}


#' @export
run_script <- function(settings, script_filepath) {

    cat("Executing ", script_filepath, "\n")

    processx::run(command = settings$r_dyntrace,
                  args = str_c("--file=", script_filepath),
                  timeout = settings$tracing_timeout,
                  cleanup_tree = TRUE)

    script_filepath
}


#' @export
run_scripts <- function(settings, script_filepaths) {
    script_filepaths %>%
        map(function(script_filepath) {
            run_script(settings, script_filepath)
        })
}


#' @export
create_trace_settings <- function(package,
                                  tracing_timeout,
                                  r_dyntrace,
                                  corpus_dirpath,
                                  raw_analysis_dirpath,
                                  verbose,
                                  truncate,
                                  binary,
                                  compression_level) {

    structure(list(package = package,
                   tracing_timeout = tracing_timeout,
                   r_dyntrace = r_dyntrace,
                   corpus_dirpath = corpus_dirpath,
                   raw_analysis_dirpath = raw_analysis_dirpath,
                   verbose = verbose,
                   truncate = truncate,
                   binary = binary,
                   compression_level = compression_level),
              class = "dynalyzer.settings.trace")
}

#' @export
parse_trace_settings <- function(args = commandArgs(trailingOnly = TRUE)) {

    option_list <- list(

        make_option(c("--tracing-timeout"),
                    action = "store",
                    type = "integer",
                    default = 60 * 60,
                    help="Timeout for tracing a script",
                    metavar="tracing-timeout"),

        make_option(c("--r-dyntrace"),
                    action="store",
                    type="character",
                    help="",
                    metavar="r-dyntrace"),

        make_option(c("--corpus-dirpath"),
                    action="store",
                    type="character",
                    help="",
                    metavar="corpus-dirpath"),

        make_option(c("--raw-analysis-dirpath"),
                    action="store",
                    type="character",
                    help="Output directory for raw tracer analysis (*.tdf)",
                    metavar="raw-analysis-dirpath"),

        make_option(c("--verbose"),
                    action="store_true",
                    default=FALSE,
                    help="Flag to enable verbose mode.",
                    metavar="verbose"),

        make_option(c("--truncate"),
                    action="store_true",
                    default=FALSE,
                    help="Flag to enable overwriting of trace files",
                    metavar="truncate"),

        make_option(c("--binary"),
                    action="store_true",
                    default = FALSE,
                    help="Output data format",
                    metavar="binary"),

        make_option(c("--compression-level"),
                    action="store",
                    type="integer",
                    default=1,
                    help="Compression level for ZSTD streaming compression",
                    metavar="compression-level")
    )

    args <- parse_args(OptionParser(option_list = option_list),
                       positional_arguments = TRUE,
                       args = args)

    create_trace_settings(package = args$args[1],
                          tracing_timeout = args$options$`tracing-timeout`,
                          r_dyntrace = path(getwd(), path_tidy(args$options$`r-dyntrace`)),
                          corpus_dirpath = path(getwd(), path_tidy(args$options$`corpus-dirpath`)),
                          raw_analysis_dirpath = path(getwd(), path_tidy(args$options$`raw-analysis-dirpath`)),
                          verbose = args$options$verbose,
                          truncate = args$options$truncate,
                          binary = args$options$binary,
                          compression_level = args$options$`compression-level`)
}
