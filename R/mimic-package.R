#' mimic: synthetic data and output control for small populations
#'
#' mimic ties together a remote execution workflow for register data on
#' small populations. A researcher never sees the real rows. They develop
#' their script on a synthetic copy, the script is run on the real data in a
#' secure environment, and only output that cannot reveal persons is
#' released.
#'
#' @section The workflow:
#' 1. [mimic_config()] sets the disclosure rules. Defaults are strict.
#' 2. [profile_data()] describes the real data. Protection is applied *before*
#'    anything is stored, so the description may leave the secure
#'    environment.
#' 3. [mimic()] builds a synthetic copy from the description. Level 1
#'    (structure only) is the default.
#' 4. [trial_run()] runs the researcher's script on the copy and checks its
#'    output with [inspect()] in advance. In the script, [safe_count()],
#'    [safe_summary()] and [safe_model()] make output that passes, and
#'    [describe_output()] says what each file shows.
#' 5. [real_run()] runs it on the real data and returns a safe error report
#'    if it fails.
#' 6. [inspect()] checks the output and gives a clear approve / needs
#'    changes verdict per file; [write_report()] writes it for the approver.
#' 7. [record_release()] keeps what was released in a register, so later
#'    output is checked against it.
#'
#' The rules behind the inspection are also available on their own as the
#' `check_*()` functions, see [checks] and [check_all()].
#'
#' @section What mimic protects against:
#' The package is built for an **honest researcher who makes mistakes**:
#' printing rows, saving identifiers, releasing small cells, saving a fitted
#' model that carries its data, suppressing a cell that the total gives
#' away, or releasing a table that can be subtracted from last month's.
#' Against these it is thorough, and when it cannot tell, it does not
#' approve.
#'
#' It does **not** protect against a researcher who deliberately smuggles
#' data out. A script can hide values in what it is allowed to release: in
#' the shape of a figure, in the last digit of a rounded number, in which
#' line it fails, or in a word chosen from its own code for the error
#' message. One bit per run is enough over many runs. No automatic check
#' can rule that out. What helps is outside the package:
#' * agreements and trust: access only for known researchers under contract,
#' * staff who read the script, not only the output,
#' * a limit on the number of runs and on what each run may release,
#' * file system permissions and no network in the secure environment.
#'   The write guard in [real_run()] is a safety net, not a sandbox.
#'
#' Two further limits: figures and Office files are always reviewed by
#' hand, and a level 4 copy from [mimic()] is built from the real data and
#' is output like any other.
#'
#' @section Principles:
#' * Data protection comes first. When in doubt the package chooses the safe
#'   option and warns.
#' * No real rows ever leave the secure environment: not in the copy, the
#'   description, error messages or logs.
#' * Synthesis methods are implemented in the package itself rather than
#'   borrowed, so they can be tuned for small populations over time.
#'
#' @keywords internal
"_PACKAGE"
