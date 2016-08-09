# ## Introduction
# RDocco is an R-language port of [Docco](http://jashkenas.github.com/docco/),
# the quick-and-dirty, literate-programming-style, documentation generator. It
# produces human-readable HTML that displays your comments alongside your code.

# RDocco supports [Markdown](http://en.wikipedia.org/wiki/Markdown), a
# lightweight markup language, for formatting of in-source comments. For
# example,
#  + `**bold**` is **bold**
#  + `*emphasis*` is _emphasis_
#  + `~~strikethrough~~` is ~~strikethrough~~.
#  + `[a link](http://google.com)` is [a link](http://google.com).

# This script also automatically formats [Roxygen2]
# (https://github.com/yihui/roxygen2)-style R comments, so that this function
# header
# ```
# #' Multiply two numbers together
# #'
# #' This function calculates the product of two numbers.
# #' @param num_one the first factor
# #' @param num_two the second factor
# #' @return the product of the two numbers
# ```
# renders as:

#' Multiply two numbers together
#'
#' This function calculates the product of two numbers.
#' @param num_one the first factor
#' @param num_two the second factor
#' @return the product of the two numbers

# ---------------------

# This script was modeled after [Jocco] (http://lcw.github.com/jocco/), a Docco
# port for the [Julia] (http://julialang.org/) language. Jocco uses external
# tools for syntax highlighting and Markdown rendering, but we opt for
# existing R-packages to handle these tasks.

# Presently, RDocco supports [MathJax](http://mathjax.com/) rendering of
# \(\LaTeX\) math equations using the delimiters `\(...\)` (no spaces!) for
# inline math (e.g., \(a^2 + b^2 = c^2\)) equations and `\[ ... \]` for block
# equations: \[ \left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n
# a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right) \] Which is a desirable
# feature because R is a programming language for statistics!

# ## Actual Code Comments
# #### Load supporting packages.
# + [stringr](http://cran.r-project.org/web/packages/stringr/) does the heavy
#   lifting for string-processing.
# + [highlight](http://cran.r-project.org/web/packages/highlight/) performs
#   syntax highlighting on code chunks.
# + [markdown](http://cran.r-project.org/web/packages/markdown/) converts
#   Markdown-formatted comments into HTML.
library("stringr")
library("highlight")
library("markdown")
options(markdown.HTML.options = c('fragment_only', 'smartypants'))

# #### Initialize HTML templates
# The basic HTML structure of the output document is a 2--column table where
# each row consists of a comment-code pairing. These three string constants
# make an HTML sandwich. `.HEADER` and `.FOOTER` are the slices of bread and
# establish the start and end of the table. `.TABLE_ENTRY` provides the template
# for the sandwich fillings---i.e., the rows of the table.
.HEADER <- "<!DOCTYPE html>
<html><head>
  <title>%title%</title>
  <meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">
  <style type=\"text/css\">
    /* This is a style file based on the Julia port of
    * [Docco](http://jashkenas.github.com/docco/) called
    * [Jocco](http://lcw.github.com/jocco/).
    */

    /*----------------- Layout and Typography ------------------------*/
    body {
      font-family: 'Palatino Linotype', 'Book Antiqua', Palatino,
                    FreeSerif, serif;
      font-size: 15px;
      line-height: 22px;
      color: #252519;
      margin: 0;
      padding: 0;
    }

    a, a:visited               { color: #261a3b; }
    p, h1, h2, h3, h4, h5, h6  { margin: 0px 0 15px 0; }
    h1                         { margin-top: 40px; }
    ul                         { margin-top: -10px; }
    #container                 { position: relative; }

    #background {
      position: fixed;
      top: 0; left: 525px; right: 0; bottom: 0;
      background: #f5f5ff;
      border-left: 1px solid #e5e5ee;
      z-index: -1;
    }

    #jump_to, #jump_page {
      background: white;
      -webkit-box-shadow: 0 0 25px #777; -moz-box-shadow: 0 0 25px #777;
      -webkit-border-bottom-left-radius: 5px; -moz-border-radius-bottomleft: 5px;
      font: 10px Arial;
      text-transform: uppercase;
      cursor: pointer;
      text-align: right;
    }

    #jump_to, #jump_wrapper {
      position: fixed;
      right: 0; top: 0;
      padding: 5px 10px;
    }

    #jump_wrapper                 { display: none; padding: 0; }
    #jump_to:hover #jump_wrapper  { display: block; }
    #jump_page                    { padding: 5px 0 3px; margin: 0 0 25px 25px; }

    #jump_page .source {
      display: block;
      padding: 5px 10px;
      text-decoration: none;
      border-top: 1px solid #eee;
    }

    #jump_page .source:hover        { background: #f5f5ff; }
    #jump_page .source:first-child  { }

    table td { border: 0; outline: 0; }
    td.docs, th.docs {
      max-width: 450px;
      min-width: 450px;
      min-height: 5px;
      padding: 10px 25px 1px 50px;
      overflow-x: hidden;
      vertical-align: top;
      text-align: left;
    }
    .docs pre { margin: 15px 0 15px; padding-left: 15px; }
    .docs p tt, .docs p code {
      background: #f8f8ff;
      border: 1px solid #dedede;
      font-size: 12px;
      padding: 0 0.2em;
    }
    .pilwrap { position: relative; }
    .pilcrow {
      font: 12px Arial;
      text-decoration: none;
      color: #454545;
      position: absolute;
      top: 3px; left: -20px;
      padding: 1px 2px;
      opacity: 0;
      -webkit-transition: opacity 0.2s linear;
    }
    td.docs:hover .pilcrow { opacity: 1; }
    td.code, th.code {
        padding: 14px 15px 16px 25px;
        width: 100%;
        vertical-align: bottom;
        background: #f5f5ff;
        border-left: 1px solid #e5e5ee;
    }
    pre, tt, code {
      font-size: 12px; line-height: 18px;
      font-family: Menlo, Monaco, Consolas, \"Lucida Console\", monospace;
      margin: 0; padding: 0;
    }

    /*--------------------- Syntax Highlighting ---------------------*/
    pre .number             { color: #000; }
    pre .functioncall       { color: #954121; }
    pre .string             { color: #219161; }
    pre .keyword            { font-weight: bolder; color: #000; }
    pre .argument           { color: #19469D; }
    pre .comment            { color: #2E9957; }
    pre .roxygencomment     { color: #707AB3; }
    pre .formalargs         { color: #19469D; }
    pre .eqformalargs       { color: #000; }
    pre .assignement        { font-weight: bolder; color: #000; }
    pre .package            { color: #96B525; }
    pre .slot               { font-style: italic; }
    pre .symbol             { color: #000 ;}
    pre .prompt             { color: #333 ;}
  </style>
  <script type=\"text/javascript\"
    src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>
</head>
<body>
  <div id=\"container\">
    <div id=\"background\"></div>
    <table cellpadding=\"0\" cellspacing=\"0\">
      <thead><tr>
        <th class=\"docs\"><h1>%title%</h1></th>
        <th class=\"code\"></th>
      </tr></thead>
      <tbody>"
.TABLE_ENTRY <- "
        <tr id=\"section-%index%\">
          <td class=\"docs\">
            <div class=\"pilwrap\">
              <a class=\"pilcrow\" href=\"#section-%index%\">&#182;</a>
            </div>
            %docs_html%
          </td>
          <td class=\"code\">
            <div class=\"highlight\"><pre>%code_html%</pre></div>
          </td>
        </tr>"
.FOOTER <- "
      </tbody>
    </table>
  </div>
</body></html>"

#' Extract comments and code chunks from a .R file
#'
#' @param src a character string specifying the path to the .R file we want to
#'   process
#' @return a list of 3 character vectors containing the chunks of code, chunks
#'   of plain comments, and chunks of Roxygen-style comments.
.ParseSource <- function(src) {
  # Read in the lines of the input script and initalize values.
  lines <- readLines(src)
  code <- character()
  docs <- character()
  has_code <- FALSE
  code_text <- ""
  docs_text <- ""

  # _Regular expression for matching comments_
  rx_COMMENT <- perl("^\\s*(?:(#)+(')?\\s(.*?)\\s*$)")

  # Search for a comment match on each line of the input file. `has_code`
  # records whether the last line that was scanned contained code-like text. As
  # long as `has_code` doesn't change, we lump successive lines together into
  # `code_text` or `docs_text`. Those lumped-together get written onto `code` or
  # `docs` once `has_code` changes.
  for (line in lines) {
    # Extract lines that match the comment pattern.
    match <- str_extract_all(line, rx_COMMENT)
    match <- unlist(match)
    # If the line only matches an empty string, set it to an empty character
    # vector.
    if (length(match) > 0 && match == "") {
      match <- character()
    }
    # If there are no matches, then there is code-like text here. Store the line
    # as code text.
    if (length(match) == 0) {
      has_code <- TRUE
      code_text <- str_c(code_text, line, "\n")
    } else {
      # Otherwise, if there is a match to the comment pattern, _and_ the
      # previous line contained code, update the code texts and reset
      # initialization values.
      if (has_code) {
        code <- c(code, code_text)
        docs <- c(docs, docs_text)
        has_code <- FALSE
        code_text <- ""
        docs_text <- ""
      }
      # Update the documentation with the current line.
      doc_line <- match
      docs_text <- str_c(docs_text, doc_line, "\n")
    }
  }
  # Append the final code and documentation chunks to their text vectors. Trim
  # off any whitespace from the documentation strings, but keep it in the code
  # (for syntax indentation).
  code <- c(code, code_text)
  docs <- c(docs, docs_text)
  docs <- str_trim(docs)
  # Replace pairs of empty lines in the code with a single empty line.
  rx_TWO_RETURNS <- perl("\\n\\s*\\n")
  code <- str_replace_all(code, rx_TWO_RETURNS, "\n")
  # Roxygen comments are a special kind of comment that start with `#'`. These
  # get their own special formatting, so we isolate them from our set of doc
  # strings.
  rx_ROXYGEN <- perl("^(\\s*(#')+\\s*(.)*)+")
  rdocs <- docs
  rdoc_lines <- which(str_detect(docs, rx_ROXYGEN))
  docs[rdoc_lines] <- ""
  rdocs[-rdoc_lines] <- ""
  # Remove `#` characters from our documentation strings and return three equal-
  # length vectors containing the code lines, plain doc lines and the
  # Roxygen-style doc lines.
  rx_INITIAL <- perl("^\\s*#\\s+")
  rx_NEWLINE <- perl("\\s*\\n\\s*#\\s*")
  docs <- str_replace_all(docs, rx_INITIAL, "")
  docs <- str_replace_all(docs, rx_NEWLINE, "\n")
  parsed <- list(code = code, docs = docs, rdocs = rdocs)
  return (parsed)
}

#' Highlight R code
#'
#' We can only highlight syntactically valid R code, and most of the individual
#' chunks are not valid. If we put all the chunks back together, then we can
#' highlight the R code (assuming the input file contained valid code). In
#' order to break apart the highlighted code, we need to mark each place where
#' we concatenated chunks together and then break up the code at those markers
#' places. We also need to preserve newline characters in strings, so that
#' multi-line strings are not collapsed onto super-long single lines.
#'
#' @param code_array a character vector containing chunks of un-highlighted R
#'   code
#' @return a character vector of syntax-highlighted R code in HTML
.HighlightCode <- function (code_array) {
  # Preserve escaped newline characters by marking them.
  sep_part_1 <- "#%BREAK%"
  sep_part_2 <- "#%HERE%"
  sep <- str_c(sep_part_1, sep_part_2)
  code_array <- str_replace_all(code_array, "\\n", str_c(sep, "\n"))
  # Combine the chunks of R code together, marking where chunks are concatenated
  # together.
  code_sep <- "# CUT HERE\n"
  code_array <- str_c(code_array, collapse = code_sep)
  # Open a text connection on the code and capture the output of highlighting
  # the code.
  con <- textConnection(code_array)
  on.exit(close(con))
  code_array <- capture.output({
    highlight(con, renderer = renderer_html(document = FALSE,
                                            header = function() '',
                                            footer = function() ''),
              encoding = "unknown")
  })
  code_array <- str_c(code_array, collapse = "")
  # Split the code at the newline-preserving markers and the chunk-concatenating
  # markers.
  sep_in_comment <- str_c("<span class=\"comment\">", sep, "</span>")
  code_array <- str_replace_all(code_array, sep_in_comment, "<br/>")
  code_array <- str_replace_all(code_array, sep, "<br/>")
  code_sep_html <- "<span class=\"comment\"># CUT HERE</span>"
  code_array <- str_split(code_array, code_sep_html)
  code_array <- unlist(code_array)
  return(code_array)
}


#' Parse a Roxygen comment
#'
#' This function parses a Roxygen comment and extracts the first line of the
#' comment, any introductory text, and the names and values of any @@-tags in
#' the comment. The parsing function below was adapted from [the parsing
#' function in Roxygen2]
#' (https://github.com/yihui/roxygen2/blob/master/R/parse-preref.R).
#'
#' #### About Roxygen
#'
#' Documentation for R packages takes place in special `.Rd` files. This
#' organization is problematic because the doc is stored in a separate file from
#' the code it describes, so it takes extra work to keep documentation up to
#' date. The package [Roxygen2](https://github.com/yihui/roxygen2) rectifies
#' this problem by automatically generating those docs from specially formatted
#' _in-source_ comments. A Roxygen comment sits above the the function it
#' describes, is set off with special `#'` comment marks, and information about
#' the function is described with tags like `@@params` or `@@author`. (Read more
#' about R documentation in [this Wiki entry]
#' (https://github.com/hadley/devtools/wiki/docs-function) from the devtools
#' package.). Roxygen2 then transforms these comments into `.Rd` files.
#'
#' Roxygen-style comments seem like a pretty good practice for R coding, because
#' they succinctly describe pertinent information about functions in a
#' structured way. They are also supported by [RStudio] (http://rstudio.org/).
#'
#' @param lines chunks of text that may contain Roxygen-style comments
#' @return a list of Roxygen elements, or an empty list if the input text not
#'   contain any Roxygen comments
.ParseRoxygen <- function(lines) {
  # Pattern that distinguishes a Roxygen comment from a normal comment.
  rx_LINE_DELIMITER <- '\\s*#+\' ?'
  # Does the string contain no matter beside spaces?
  .is.null.string <- function(string) {
    str_length(str_trim(string)) == 0
  }
  # Grab lines starting with the Roxygen pattern and trim trailing (right-sided)
  # whitespace
  delimited_lines <- lines[str_detect(lines, rx_LINE_DELIMITER)]
  trimmed_lines <- str_trim(str_replace(delimited_lines, rx_LINE_DELIMITER, ""),
                            "right")
  # Return an empty list if the trimmed lines are empty. Otherwise merge the
  # lines together.
  if (length(trimmed_lines) == 0) return(list())
  joined_lines <- str_c(trimmed_lines, collapse = '\n')
  # Split the Roxygen comment into @-labels chunks
  elements <- strsplit(joined_lines, '(?<!@)@(?!@)', perl = TRUE)[[1]]
  elements <- str_replace_all(elements, fixed("@@"), "@")

  # Parse the first line of a Roxygen comment block and any text that follows
  .ParseIntro <- function(expression) {
    if (.is.null.string(expression)) return(NULL)
    intro <- unlist(str_trim(expression))
    intro <- unlist(str_split(intro, "\n(#' )\n"))
    parts <- list(first_line = intro[1], introduction = intro[-1])
    return(parts)
  }
  parsed_introduction <- .ParseIntro(elements[[1]])
  # Parse `@tag value` items
  .ParseElement <- function(element) {
    pieces <- str_split_fixed(element, "[[:space:]]+", 2)
    tag <- pieces[, 1]
    rest <- pieces[, 2]
    rest <- list(rest)
    names(rest)[1] <- tag
    return(rest)
  }
    parsed_elements <- unlist(lapply(elements[-1], .ParseElement),
                            recursive = FALSE)
  return(c(parsed_introduction, parsed_elements))
}

#' Insert Roxygen-style comments into Markdown templates
#'
#' RDocco applies Markdown formatting to the content of a Roxygen comment.
#' Specifically, the first line of a Roxygen block is formatted as an `<h3>`
#' element. The following Roxygen `@@`-tags are then formatted, if present:
#' `@@param` tags are combined and output as a table, `@@return` is output as a
#' table (assuming there is only one `@@return` tag), and `@@TODO` tags (which I
#' invented) are combined and output as an unordered list.
#'
#' **Note:** In order to write those @@-signs in this Roxygen comment, I had to
#' double up on them as an escape: `@@@@` renders as  "@@".
#'
#' @TODO Make Markdown lists work in the Roxygen comments
#' @TODO Generate template for `@@note` tags.
#' @TODO Generate template for `@@examples` tags.
#'
#' @param roxy_lines a `list` of Roxygen comments. The names of the list element
#'   tell us what kind of Roxygen comment each element is.
#' @return a Markdown-formatted string of the input Roxygen-style comments, or
#'   an empty string on empty input
.DressUpRoxy <- function(roxy_lines) {
  if (length(roxy_lines) == 0){
    return ("")
  }
  # _Support function for cleaning up Roxygen lines before formatting them_
  .FetchLines <- function(line) {
    line <- unlist(line, use.names = FALSE)
    # Blank out any `#'` that followed some amount of space. Then blank out any
    # `#'` that may be followed by a newline `\n`.
    line <- str_replace_all(line, "\\s*\\n*#'\\s+", " ")
    line <- str_replace_all(line, "\\s*\\n*#'\\n*", " ")
    line <- str_trim(line)
    return(line)
  }
  # Determine what kind of Roxygen comment each line is. Then start processing
  # lines by extracting and combining `@param` lines into a table.
  line_class <- unlist(attributes(roxy_lines), use.names = FALSE)
  param_lines <- which(str_detect(line_class, "param"))
  if(length(param_lines) > 0) {
    table_header <- "|Parameters |      |\n|------|------|"
    params <- .FetchLines(roxy_lines[param_lines])
    params <- str_replace(params, "^\\s*(\\S+)\\s+(.*)", "| `\\1` | \\2 |")
    params <- c(table_header, params)
    params <- str_c(params, collapse = "\n")
    params <- str_c(params, "\n")
    # Since we are combining elements into one `@param` table, we remove all
    # `@param` lines beyond the first.
    roxy_lines[param_lines[-1]] <- NULL
    roxy_lines$param <- params
  }
  # Extract `@return` element---there should be at most one---and insert it into
  # a table.
  line_class <- unlist(attributes(roxy_lines), use.names = FALSE)
  return_lines <- which(str_detect(line_class, "return"))
  if(length(return_lines) > 0) {
    table_header <- "| Returns |      |\n|------|------|"
    returns <- .FetchLines(roxy_lines[return_lines])
    returns <- str_c("| | ", returns, " |")
    returns <- c(table_header, returns)
    returns <- str_c(returns, collapse = "\n")
    returns <- str_c(returns, "\n")
    roxy_lines$"return" <- returns
  }
  # The first line of the Roxygen block gets formatted as an `<h3>`.
  line_class <- unlist(attributes(roxy_lines), use.names = FALSE)
  first_line <- which(str_detect(line_class, "first_line"))
  if(length(first_line) > 0) {
    roxy_lines$first_line <- .FetchLines(roxy_lines[first_line])
    roxy_lines$first_line <- str_c("### ", roxy_lines$first_line, "\n")
  }
  # Extract any `@TODO` elements and combine them into a list.
  line_class <- unlist(attributes(roxy_lines), use.names = FALSE)
  TODO_lines <- which(str_detect(line_class, "TODO"))
  if(length(TODO_lines) > 0) {
    list_header <- "**TODO**\n"
    TODOs <- .FetchLines(roxy_lines[TODO_lines])
    TODOs <- str_c("+ ", TODOs)
    TODOs <- c(list_header, TODOs)
    TODOs <- str_c(TODOs, collapse = "\n")
    TODOs <- str_c(TODOs, "\n")
    # Since we are combining elements into one `@TODO` list, we remove all
    # `@TODO` lines beyond the first.
    roxy_lines[TODO_lines[-1]] <- NULL
    roxy_lines$TODO <- TODOs
  }
  # All other Roxygen comments are considered paragraphs of an "introduction"
  # text, so these are combined together.
  line_class <- unlist(attributes(roxy_lines), use.names = FALSE)
  introduction <- which(str_detect(line_class, "introduction"))
  if(length(first_line) > 0) {
    intros <- .FetchLines(roxy_lines[introduction])
    intros <- str_c(intros, collapse = "\n\n")
    intros <- str_c(intros, "\n")
    roxy_lines$introduction <- intros
  }
  roxy_lines <- str_c(unlist(roxy_lines, use.names = FALSE), collapse = "\n")
  return(roxy_lines)
}

#' Convert Markdown-formatted strings into HTML
#'
#' @param doc_array a character vector containing Markdown-formatted strings
#' @return an HTML-formatted version of the input character vector
.MarkItDown <- function(doc_array) {
  cust_extensions <- c("no_intra_emphasis", "tables", "fenced_code", "autolink",
                       "strikethrough", "lax_spacing", "space_headers",
                       "latex_math")
  for (line_num in 1:length(doc_array)) {
    line <- doc_array[line_num]
    if(line != "") {
      line <- markdownToHTML(text = line, extensions = cust_extensions)
    }
    doc_array[line_num] <- line
  }
  return(doc_array)
}

#' Generate literate documentation from Markdown-formatted comments in an R
#' script
#'
#' This is the main formatting function. `Doccofy` loads the source R script,
#' parses the comments from the code, highlights the code, inserts the contents
#' of Roxygen comments into Markdown templates, combines the processed Roxygen
#' comments with regular comments, converts the Markdown-formatted test into
#' HTML, and then builds the final HTML output file.
#'
#' @param src the path to the R script
#' @return nothing is returned, but a `[src].html` file is generated in the
#'   source file's directory
Doccofy <- function(src) {
  # Extract title from filepath. Set the title in `.HEADER` HTML template.
  title <- unlist(str_split(src, "/"))
  title <- title[length(title)]
  header <- str_replace_all(.HEADER, "%title%", title)

  # Parse the input file into code chunks, plain-comment `#` chunks and
  # Roxygen-style `#'` comment chunks.
  parsed <- .ParseSource(src)
  code <- parsed$code
  docs <- parsed$docs
  rdocs <- parsed$rdocs

  # Format documentation text in each set of comments. Merge `#` and `#'`
  # chunks back to together.
  rdocs <- Map(.ParseRoxygen, rdocs)
  rdocs <- unlist(Map(.DressUpRoxy, rdocs))
  rdoc_lines <- which(!str_detect(rdocs, "^$"))
  docs[rdoc_lines] <- rdocs[rdoc_lines]
  docs <- .MarkItDown(docs)

  # Replace escaped backslashes with HTML entities. This lets us do \(\LaTeX\),
  # but we might (?) face problems rendering escape characters in our
  # documentation.
  docs <- str_replace_all(docs, "\\\\", "&#92;")

  # Highlight code. Remove unnecessary line-breaks at the ends of code chunks.
  # Preserve escape sequences in strings by doubling up on escape characters.
  code <- .HighlightCode(code)
  code <- str_replace_all(code, "<br/>$", "")
  code <- str_replace_all(code, "\\\\", "\\\\\\\\")

  # Plug the formatted documentation and highlighted code chunks together into
  # our `.TABLE_ENTRY` HTML template.
  formatted <- character(length(code))
  for (chunk_num in 1:length(code)) {
    table_row <- str_replace_all(.TABLE_ENTRY, "%index%", chunk_num)
    table_row <- str_replace_all(table_row, "%docs_html%", docs[chunk_num])
    table_row <- str_replace_all(table_row, "%code_html%", code[chunk_num])
    formatted[chunk_num] <- table_row
  }
  # Put our HTML sandwich together and output the final file.
  formatted <- str_c(formatted, collapse = "")
  final_html <- str_c(header, formatted, .FOOTER, collapse = "")
  write(final_html, str_c(src, ".html"))
}
