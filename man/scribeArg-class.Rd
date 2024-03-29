% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-args.R
\docType{class}
\name{scribeArg-class}
\alias{scribeArg-class}
\alias{scribeArg}
\title{{scribe} argument}
\description{
ReferenceClass object for managing arguments
}
\details{
The \link{scribeArg} class sets specifications and controls for how
command line arguments are to be parsed.  These are meant to be used in
conjunction with \link{scribeCommandArgs} and specifically with the \link{Rscript}
utility.  However, a use can define their own \link{scribeArg} separately.
}
\section{Fields}{

\describe{
\item{\code{aliases}}{\verb{[character]}\cr A vector to denote the argument's name}

\item{\code{action}}{\verb{[character]}\cr An action for resolving the argument (see
\code{default} for note on using another \link{scribeArg} object)}

\item{\code{default}}{\verb{[ANY]}\cr A default value.  This can be another \link{scribeArg}
object.  When that is the case, the default value and action are pass
through from the other \link{scribeArg} object.}

\item{\code{convert}}{\verb{[ANY]}\cr Passed to the \code{to} argument in \code{\link[=value_convert]{value_convert()}}}

\item{\code{n}}{\verb{[integer]}\cr The length of the values}

\item{\code{info}}{\verb{[character]}\cr Additional information about the argument when
printed}

\item{\code{options}}{\verb{[list]}\cr A named list of options (see \strong{Options})}

\item{\code{positional}}{\verb{[logical]}\cr Indicator if the argument is \emph{positional}
(i.e., not preceded by a \code{-} or \verb{--} command line argument)}

\item{\code{resolved}}{\verb{[logical]}\cr Has the object been resolved}

\item{\code{value}}{\verb{[ANY]}\cr The resolve value}

\item{\code{stop}}{\verb{[character]}\cr \code{"none"}, \code{"hard"}, or \code{"soft"}}

\item{\code{execute}}{\verb{[function]}\cr (For advanced use).  A \code{function} to be
evaluated along with the arg.  The function can have no parameters, a
single parameter for the \link{scribeArg} object, or accept the \link{scribeArg}
object as its first argument, and the \link{scribeCommandArgs} object as its
second.  Both objects will be passed by position}
}}

\section{Methods}{

\describe{
\item{\code{get_action()}}{Retrieve action}

\item{\code{get_aliases()}}{Retrieve aliases}

\item{\code{get_default()}}{Retrieve the default value}

\item{\code{get_help()}}{Retrieve help information as a \code{character} vector}

\item{\code{get_name(clean = TRUE)}}{Retrieve names

    \describe{
      \item{\code{clean}}{When \code{TRUE} removes \code{-}s from text}
    }}

\item{\code{get_value()}}{Retrieve the resolved value}

\item{\code{help()}}{Print out formatted help information}

\item{\code{initialize(
  aliases = "",
  action = arg_actions(),
  default = NULL,
  convert = scribe_convert(),
  n = NA_integer_,
  info = NA_character_,
  options = list(),
  stop = c("none", "hard", "soft"),
  execute = invisible
)}}{    Initialize the \link{scribeArg} object

    See \strong{fields} for parameter information.
    }

\item{\code{is_resolved()}}{Check if object has been resolved}
}}

\section{Options}{


Several available options
\describe{
\item{\code{action="list"}}{
\describe{
\item{\code{choices}}{
An explicit set of values that argument must be.  If the value parsed
is not one of these, an error will occur.
}
}
}
\item{\code{action="flag"}}{
\describe{
\item{\code{no}}{
When \code{TRUE} included appends \code{--no} to aliases to invert results\cr
\strong{Example:}\cr
With the argument \code{new_arg("--test", options = list(no = TRUE))},
passing command arguments \code{--test} would set this to \code{TRUE} and
\code{--no-test} explicitly set to \code{FALSE}.
}
}
}
}
}

\examples{
# new_arg() is recommended over direct use of scribeArg$new()

# arguments with `--` indicators
new_arg("--verbose", action = "flag")
new_arg(c("-f", "--force"), action = "flag")
new_arg("--values", action = "list")

# positional
new_arg("verbose", action = "flag")
new_arg("value", action = "list", n = 1)

# special `...` action which absorbs left-over arguments
new_arg("values", action = "dots", info = "list of values")
new_arg("...", info = "list of values") # defaults when alias is "..."
}
\seealso{
Other scribe: 
\code{\link{command_args}()},
\code{\link{new_arg}()},
\code{\link{scribeCommandArgs-class}}
}
\concept{scribe}
