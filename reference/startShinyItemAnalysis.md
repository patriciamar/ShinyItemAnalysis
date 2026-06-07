# Start ShinyItemAnalysis application

An interactive shiny application to run test and item analysis. By
default, the function runs the application as a background process
("Jobs" tab in the "RStudio" IDE). User is then free to use the `R`
Console for other work and to try the sample R code examples. You can
still run the app the usual way in the console by specifying
`background = FALSE`.

## Usage

``` r
startShinyItemAnalysis(background = TRUE, ...)

run_app(background = TRUE, ...)
```

## Arguments

- background:

  *logical*, should the application be run as a background process (in
  the 'RStudio')?

- ...:

  Arguments passed on to
  [`utils::install.packages`](https://rdrr.io/r/utils/install.packages.html)

  `lib`

  :   character vector giving the library directories where to install
      the packages. Recycled as needed. If missing, defaults to the
      first element of
      [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).

  `repos`

  :   character vector, the base URL(s) of the repositories to use,
      e.g., the URL of a CRAN mirror such as
      `"https://cloud.r-project.org"`. For more details on supported URL
      schemes see [`url`](https://rdrr.io/r/base/connections.html).

      Can be `NULL` to install from local files, directories or URLs:
      this will be inferred by extension from `pkgs` if of length one.

  `contriburl`

  :   URL(s) of the contrib sections of the repositories. Use this
      argument if your repository mirror is incomplete, e.g., because
      you mirrored only the `contrib` section, or only have binary
      packages. Overrides argument `repos`. Incompatible with
      `type = "both"`.

  `method`

  :   download method, see
      [`download.file`](https://rdrr.io/r/utils/download.file.html).

  `available`

  :   a matrix as returned by
      [`available.packages`](https://rdrr.io/r/utils/available.packages.html)
      listing packages available at the repositories, or `NULL` when the
      function makes an internal call to `available.packages`.
      Incompatible with `type = "both"`.

  `destdir`

  :   directory where downloaded packages are stored. If it is `NULL`
      (the default) a subdirectory `downloaded_packages` of the session
      temporary directory will be used (and the files will be deleted at
      the end of the session).

  `dependencies`

  :   logical indicating whether to also install uninstalled packages
      which these packages depend on/link to/import/suggest (and so on
      recursively). Not used if `repos = NULL`. Can also be a character
      vector, a subset of
      `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.

      Only supported if `lib` is of length one (or missing), so it is
      unambiguous where to install the dependent packages. If this is
      not the case it is ignored, with a warning.

      The default, `NA`, means `c("Depends", "Imports", "LinkingTo")`.

      `TRUE` means to use
      `c("Depends", "Imports", "LinkingTo", "Suggests")` for `pkgs` and
      `c("Depends", "Imports", "LinkingTo")` for added dependencies:
      this installs all the packages needed to run `pkgs`, their
      examples, tests and vignettes (if the package author specified
      them correctly).

      In all of these, `"LinkingTo"` is omitted for binary packages.

  `type`

  :   character, indicating the type of package to download and install.
      Will be `"source"` except on Windows and some macOS builds: see
      the section on ‘Binary packages’ for those.

  `configure.args`

  :   (Used only for source installs.) A character vector or a named
      list. If a character vector with no names is supplied, the
      elements are concatenated into a single string (separated by a
      space) and used as the value for the --configure-args flag in the
      call to `R CMD INSTALL`. If the character vector has names these
      are assumed to identify values for --configure-args for individual
      packages. This allows one to specify settings for an entire
      collection of packages which will be used if any of those packages
      are to be installed. (These settings can therefore be re-used and
      act as default settings.)

      A named list can be used also to the same effect, and that allows
      multi-element character strings for each package which are
      concatenated to a single string to be used as the value for
      --configure-args.

  `configure.vars`

  :   (Used only for source installs.) Analogous to `configure.args` for
      flag --configure-vars, which is used to set environment variables
      for the `configure` run.

  `clean`

  :   a logical value indicating whether to add the --clean flag to the
      call to `R CMD INSTALL`. This is sometimes used to perform
      additional operations at the end of the package installation in
      addition to removing intermediate files.

  `Ncpus`

  :   the number of parallel processes to use for a parallel install of
      more than one source package. Values greater than one are
      supported if the `make` command specified by
      `Sys.getenv("MAKE", "make")` accepts argument -k -j \<Ncpus\>.

  `verbose`

  :   a logical indicating if some “progress report” should be given.

  `INSTALL_opts`

  :   an optional character vector of additional option(s) to be passed
      to `R CMD INSTALL` for a source package install. E.g.,
      `c("--html", "--no-multiarch", "--no-test-load")` or, for macOS,
      `"--dsym"`.

      Can also be a named list of character vectors to be used as
      additional options, with names the respective package names.

  `quiet`

  :   logical: if true, reduce the amount of output. This is *not*
      passed to
      [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)
      in case that is called, on purpose.

  `keep_outputs`

  :   a logical: if true, keep the outputs from installing source
      packages in the current working directory, with the names of the
      output files the package names with `.out` appended (overwriting
      existing files, possibly from previous installation attempts).
      Alternatively, a character string giving the directory in which to
      save the outputs. Ignored when installing from local files.

## Value

No return value. Called for side effects.

## Author

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>

Adela Hladka  
Institute of Computer Science of the Czech Academy of Sciences  
<hladka@cs.cas.cz>

Jan Netik  
Institute of Computer Science of the Czech Academy of Sciences  
<netik@cs.cas.cz>

## Examples

``` r
if (FALSE) { # \dontrun{
startShinyItemAnalysis()
startShinyItemAnalysis(background = FALSE)
} # }
```
