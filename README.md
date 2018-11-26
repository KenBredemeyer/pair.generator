# pair.generator

## Overview
pair.generator produces sets of pairs for use in pairwise comparisons projects.  This package is designed for use in educational
assessment, where students' performances are judged against one another, two at a time.  The end result of this process is a scale
in which performances are located on an ability continuum.  pair.generator conveniently forms pairs and provides diagnostics for
the design stage.

## Installation
Install from GitHub by cloning the package, or install using devtools.
```
devtools:install_github("KenBredemeyer/pair.generator")
```

## Usage
Generate pairs with `pairs_generate()`.  This function returns a data.frame which includes the variables `left` and `right`, for 
presenting performances to judges on the left and right sides.  The package has some built in data sets, including `data_standard`,
for experimenting with the functions.

```
# set rng seed for reproducability
set.seed(1)

# generate pairs, with each performance included 10 times on average
pairs <- pairs_generate(data_standard, av_inclusions = 10, inclusion_tolerance = 1)

# plot the number of times each performance is included in the set of pairs
pairs_plot_inclusions(pairs)
```

