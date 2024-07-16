Thank you very much for the remarks associated with the original submission. 
We have implemented the required changes or responded to feedback when it was
impossible.

> You have examples for unexported functions. Please either omit these
> examples or export these functions.
> Examples for unexported function
>    rd_data_sampler() in:
>      rd_sampler.Rd
>      rd_sampler.Rd
>   task_graph_which_satisfied() in:
>      throttle.Rd
>   results() in:
>      rd_sampler.Rd
>      rd_sampler.Rd

The `task_graph_which_satisfied` had no examples however we removed examples for
the unexported `throttle` function. `rd_data_sampler` is a function from the 
dummy package that is shipped as test/examples data under inst/example_packages
and is not part of the package itself, it is only shipped with it for convenience. 
Similarly `results` function does not have any examples attached. Additionally 
the package is prepared for open-source collaboration and multiple contribution
and thus we believe examples for unexported functions help greatly other developers
to quickly familiarize with the code base and purpose of the function.

> Some code lines in examples are commented out. Please never do that.
> Ideally find toy examples that can be regularly executed and checked.
> Lengthy examples (> 5 sec), can be wrapped in \donttest{}.
> Examples in comments in:
>        throttle.Rd

Examples in the throttle.Rd got removed. Thank you for flagging them.

> You are using installed.packages() in your code. As mentioned in the
> notes of installed.packages() help page, this can be very slow.
> Therefore do not use installed.packages().
> -> R/check_process.R; R/install.R; R/utils.R

Please let us go through it case by case:

> R/check_process.R

Here the utils::installed.packages() is used to list all the packages installed
in the .Library. Because we don't know name of these packages, we cannot apply
suggestions from the installed.packages() manual page. utils::installed.packages()
is the only function that could be used here.

> R/install.R

utils::installed.packages() is not used in this file

> R/utils.R

utils::installed.packages() is used to derive which base packages are installed
in the version of R. As this list might be a subject to change in consequent R
release, we cannot be sure of their names. Thus to ensure packages is compatible
with any R version if assess that list programmaticaly. On top of that we are
interested in getting all installed base packages, not only those loaded by default
so deafultPackages option is not applicable here.


Additionally, due to the nature of the package which is to perform multiple,
sequential R CMD check runs, it is expected to run even for a few hours. 
Therefore slight performance problems related to utils::installed.packages() are
surely tolerable to any users of the package as they are negligible considering
expected running time for core functions in this package.