# Generalized Additive Models in R

## A workshop on GAMs to support Ukraine

### April 13th, 2023, 1800&ndash;2000 CET

### Gavin Simpson

#### Department of Animal & Veterinary Science, Aarhus University, Denmark

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7825960.svg)](https://doi.org/10.5281/zenodo.7825960)

## About the workshop

Generalized Additive Models (GAMs) were introduced as an extension to linear and generalized linear models, where the relationships between the response and covariates are not specified up-front by the analyst but are learned from the data themselves. This learning is achieved by representing the effect of a covariate on the response as a smooth function, rather than following a fixed form (linear, quadratic, etc). GAMs are a large and flexible class of models that are widely used in applied research because of their flexibility and interpretability.

The workshop will explain what a GAM is and how penalized splines and automatic smoothness selection methods work, before focusing on the practical aspects of fitting GAMs to data using the [mgcv R package](https://cran.r-project.org/package=mgcv), and will be most useful to people who already have some familiarity with linear and generalized linear models.

## About the instructor

Gavin is a statistical ecologist and freshwater ecologist/palaeoecologist. He has a B.Sc. in Environmental Geography and a Ph.D. in Geography from University College London (UCL), UK. After submitting his Ph.D. thesis in 2001, Gavin worked as an environmental consultant and research scientist in the Department of Geography, UCL, before moving, in 2013, to a research position at the Institute of Environmental Change and Society, University of Regina, Canada. Gavin moved back to Europe in 2021 and is now Assistant Professor of Applied Statistics in the Department of Animal and Veterinary Sciences at Aarhus University, Denmark. Gavin's research broadly concerns how populations and ecosystems change over time and respond to disturbance, at time scales from minutes and hours, to centuries and millennia. Gavin has developed several R packages, including gratia, analogue, and cocorresp, he helps maintain the vegan package, and can often be found answering R- and GAM-related questions on StackOverflow and CrossValidated.

## Pre-workshop preparation

Please be sure to have at least version 4.1 &mdash; *and preferably version 4.2* &mdash; of R installed (the version of my gratia package we will be using depends on you having at least version 4.1 installed and some slides might contain code that requires version 4.2). Note that R and RStudio are two different things: it is not sufficient to just update RStudio, you also need to update R by installing new versions as they are released.

To check what version of R you have installed, you can run

```r
version
```

in R and look at the `version.string` entry (or the `major` and `minor` entries).

If the installed version of R is < 4.1.0, you'll need to update R by downloading and installing a newer version. To download R go to the [CRAN Download](https://cran.r-project.org/) page and follow the links to download R for your operating system:

* [Windows](https://cran.r-project.org/bin/windows/)
* [MacOS X](https://cran.r-project.org/bin/macosx/)
* [Linux](https://cran.r-project.org/bin/linux/)

We will make use of several R packages that you'll need to have installed. Prior to the start of the workshop, please run the following code to update your installed packages and then install the required packages:

```r
# update any installed R packages
update.packages(ask = FALSE, checkBuilt = TRUE)

# packages to install
pkgs <- c("mgcv",  "tidyverse", "readxl", "usethis")

# install those packages
install.packages(pkgs, Ncpus = 4) # set Ncpus to # of CPU cores you have
```

Finally, we will make use of the development version of the *gratia* package as it is not quite ready for CRAN. You can install this package using the binaries provided by the [rOpenSci](https://ropensci.org/) build service [R-Universe](https://r-universe.dev). To install from my R-Universe, you need to tell R to also install packages from my R-Universe package repo:

```r
# Download and install gratia
install.packages("gratia",
                 repos = c("https://gavinsimpson.r-universe.dev",
                           "https://cloud.r-project.org"))
```

# Workshop materials

## Course materials

To download the materials for the workshop and get working, open RStudio and run the `use_course()` code below

```r
usethis::use_course("https://bit.ly/ukraine-gam")
```

This will prompt you to agree to the workshop materials being downloaded to the `~/Desktop` folder (where `~/` refers to your home drive/directory). Agree to this by typing the appropriate number and pressing <kbd>Return</kbd>. The materials will then be downloaded as a zip file and unpacked for you.

When complete, you'll be asked if you want to delete the download `.zip` file. You can choose whichever option you prefer here.

Finally, once you've answered this last question, RStudio will load the workshop project in a new RStudio session. Once the new session has started you can close the other RStudio window (the one where you ran the `use_course()` code.)

### If you know what you are doing

If you aren't using RStudio or know what you are doing, you can download the zip file directly from <https://bit.ly/ukraine-gam>. You are responsible then for unpacking the files correctly and starting R in the correct folder.

## Slides

The rendered slide deck is available online from the link below:

* [Slide deck](https://gavinsimpson.github.io/ukraine-gam/slides/index.html)

You should also be able to view the slides locally in your browser by opening the `slides` folder and double clicking on `index.html`.
