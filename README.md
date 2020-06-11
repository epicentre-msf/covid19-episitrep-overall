
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Epi Sitrep World COVID-19

<!-- badges: start -->

<!-- badges: end -->

Epicentre’s code repository for COVID-19 Situation Reports

## Package management

Package versions are stored in the `renv.lock` file, automatically
created by the [`{renv}` package]().

After cloning this repository, you can build your isolated package
library by running:

``` r
renv::restore()
```

When a new package is required, after installation run
`renv::snapshot()` to add the package to the lockfile that will be
synced to bitbucket.

If you use Rstudio double-click on the file
`episitrep_world_covid-19.Rproj` to start.

If you use `R` directly, make sure you launch `R` at the root folder of
the project to allow `renv` to be activated properly.
