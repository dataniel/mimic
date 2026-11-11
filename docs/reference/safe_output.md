# Safe output for the researcher's script

Three functions for the researcher to use *in the script*, so the output
passes [`inspect()`](inspect.md) the first time: counts of persons with
small cells suppressed and the rest rounded, summaries of a numeric
variable without small or dominated groups, and a table of model
coefficients instead of the model object. They apply the same rules as
[`inspect()`](inspect.md).

## Why they exist

Output checkers send output back for the same few reasons again and
again. OpenSAFELY reports that about 30 % of rejected requests have
counts that are not rounded and 20 % have small counts that are not, or
not correctly, suppressed
(<https://docs.opensafely.org/output-checking/>). Suppression is easy to
get wrong: a suppressed cell next to a total is the total minus the
other cells. [`inspect()`](inspect.md) finds these mistakes, but only
after the script has run on the real data. These functions do it right
in the script, and [`trial_run()`](trial_run.md) shows the result on the
copy.

## Which rules apply

Without `config` the functions use the configuration that
[`trial_run()`](trial_run.md) and [`real_run()`](real_run.md) were
given: they place it in the script's session as the option
`mimic.config`. The script then follows the rules staff use, without
knowing them. Outside those functions the defaults of
[`mimic_config()`](mimic_config.md) apply.

Where researchers work on a researcher machine in the secure environment
and run their scripts themselves, staff can set the option for every
session in the machine's `Rprofile.site`:

    options(mimic.config = mimic::mimic_config(min_count = 5))

With [`describe_output()`](describe_output.md) the researcher also says
what each file shows, so the checker does not have to guess.
