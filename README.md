
Creating a keyword index
------------------------

For a recent book chapter I wrote, I was asked to put together a list of keywords and the page numbers on which they appear. The code below is what I used to make this process easier.

#### This document covers how to

-   extract text from a PDF document
-   search using regular expressions for keywords
-   wrangle text into a custom formatted output
-   deliver the output via clipboard (ready to be pasted)

### Extract text from a pdf using {pdftools}

``` r
## install if not already
if (!requireNamespace("pdftools", quietly = TRUE)) install.packages("pdftools")

## extract text from pdf
x <- pdftools::pdf_text(
  "/Users/mwk/Downloads/Warner_1st pass_Ch 21_383-398.pdf"
)

## convert hyphenated line breaks into words
x <- gsub("\\-\\n", "", x)
```

### Define user functions for indexing task

``` r
##  user function to search pages for pat and return formatted text
find_keyword <- function(pat, p1) {
  p <- grep(pat, x, ignore.case = TRUE)
  if (length(p) == 0L) return(NULL)
  pat <- gsub("\\?", "", pat)
  p <- p + p1 - 1L
  p <- paste(p, collapse = ", ")
  p <- stringr::str_wrap(p, 20)
  spaces <- paste(rep("&nbsp;", 15), collapse = "")
  p <- paste0(pat, "\n", p)
  p <- gsub("\\n", paste0("\n", spaces), p)
  p
}

## vectoraize this function and then use tfse::pbcopy
find_keywords <- function(pat, p1) {
  pat <- sort(pat)
  x <- lapply(pat, find_keyword, p1)
  x <- unlist(x[lengths(x) > 0L])
  x <- paste(x, collapse = "\n")
  tfse::pbcopy(x)
  x
}
```

### Create keywords index

``` r
## FYI: my chapter started on page 383L
out <- find_keywords(
  c("social media", "twitter",
    "networks?", "partisans?",
    "tweets?", "sentiment",
    "nonpolitical"),
  p1 = 383L
)

## print output
cat(paste0("\n#### Index\n", gsub("\\n", " <br>", out)))
```

#### Index

networks <br>               385, 386, 387, 396, <br>               397, 398 <br>nonpolitical <br>               391, 393 <br>partisans <br>               384, 387, 388, 389, <br>               391, 394, 395, 396, <br>               398 <br>sentiment <br>               384, 387, 388, 393, <br>               394, 395, 396, 397, <br>               398 <br>social media <br>               383, 384, 385, 387, <br>               396, 397, 398 <br>tweets <br>               383, 385, 386, 387, <br>               388, 389, 390, 391, <br>               392, 393, 394, 395, <br>               397, 398 <br>twitter <br>               383, 384, 385, 386, <br>               387, 388, 389, 390, <br>               391, 392, 393, 394, <br>               395, 396, 397, 398
