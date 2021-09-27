# Contributing to ***NeoNet***

This outlines how to propose a change to ***NeoNet***. The purpose of this GitHub repository is to gather contributions in order to improve the dataset, the RShiny app, in a FAIR Science perspective.

## Contribute to the **NeoNet dataset** and **NeoNet app**

* The current dataset is expected to the extended to the Middle and Southern European Atlantic watershed. See the [web tutorial](https://zoometh.github.io/neonet/). New unit test functions are needed to control the dataset before its processing

* You can fix typos, spelling mistakes, or grammatical errors in the app directly using the GitHub web interface.  

* During the on-the-fly calibration process, that can last a long time, a "loading" message should be plotted.

* We expect to integrate the Bayesian modeling of [PhaseCode](#mf.phasecode) during the calibration process ([**calib** panel](#panel.calib)) with [RChronoModel](https://cran.r-project.org/web/packages/RChronoModel/index.html) functions 

If you want to make a bigger change, or if you've found a bug, it's a good idea to first [file an issue](https://github.com/zoometh/neonet/blob/main/github/ISSUE_TEMPLATE.md), where possible, using  [reprex](https://www.tidyverse.org/help/#reprex).

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("zoometh/neonet", fork = TRUE)`.

*   Make sure the package passes R CMD check by running `devtools::check()`. If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of [`NEWS.md`](https://github.com/zoometh/iconr/blob/master/NEWS.md).


## Code of Conduct

Please note that the iconr project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
