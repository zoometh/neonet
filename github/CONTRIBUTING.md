# Contributing to ***NeoNet***

This outlines how to propose a change to ***NeoNet***. The purpose of this GitHub repository is to gather contributions in order to improve the quality of the RShiny application and develop new functionalities, in a FAIR Science perspective.

## Fixing typos

When relevant, you can fix typos, spelling mistakes, or grammatical errors in the app and the documentation (`.Rd` files) directly using the GitHub web interface.  

## Bigger changes

To develop the versatility of the package, we encourage contributions directly related to:

* **Bayesian modelling** The next planned development concerns the construction of a **chronological model with Bayesian statistics for the different 'PhaseCode' within a single site**. Specifically, this means to integrate [RChronoModel functions](https://cran.r-project.org/web/packages/RChronoModel/index.html) during the calibration process.  

* [Typology](https://zoometh.github.io/iconr/articles/next.html#typology-1): we aim to create a hierarchical vocabulary (ie, structured vocabulary), in the form of a directed graph, to describe both graphical units typology and technology, mostly for Prehistoric and Protohistoric iconography (like [this](https://raw.githubusercontent.com/zoometh/iconr/master/doc/img/typology_gu.png)). To get this thesaurus shared between different users, we need it in in different languages, easily editable, etc.  

* [Harris Matrix](https://zoometh.github.io/iconr/articles/next.html#harris-matrix-1): incor 0.1.0 has a [*diachronic* edge](https://zoometh.github.io/iconr/articles/index.html#contemporaneous-elements) which is the first step to model overlaping/superimposition/relative chronology of the iconographical content. We want to go further, for example to recreate dynamically a Harrix Matrix for each graphical item.

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it's needed. If you've found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("zoometh/iconr", fork = TRUE)`.

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
