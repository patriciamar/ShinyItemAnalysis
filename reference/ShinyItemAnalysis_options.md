# Options consulted by ShinyItemAnalysis

The package and interactive `{shiny}` app consult several options that
you can easily set via
[`options()`](https://rdrr.io/r/base/options.html). Moreover, there is
some behavior that can be changed through environment variables.

## Options

Options are set with `options(<option> = <value>)`.

- `sia.disable_modules`: You can completely disable SIA modules by
  setting this to `TRUE`.

- `sia.modules_repo`: This is the URL for a CRAN-like repository that
  the app uses to retrieve information about available module packages.

- `sia.offer_modules`: If set to `TRUE` (the default), calling
  [`run_app()`](startShinyItemAnalysis.md) will check for the available
  SIA modules on the official repository and offer to install those
  module packages that are not installed yet.

## Environment variables

You can set this variable system-wide or use `R` or project-wise
`.Renviron` file. For more details, please navigate to [the R
documentation](https://rdrr.io/r/base/Startup.html).

- `SIA_MODULES_DEBUG`: Setting this to `TRUE` provides a verbose
  description of SIA modules-related processes. Useful only for
  debugging purposes.

- `SIA_MODULES_FORCE_GUI_INSTALLATION`: When the app is running on
  shiny-server, interactive module installation within the app is not
  allowed by default. Setting this variable to `TRUE` will override this
  restriction and enable module installation in the app.
