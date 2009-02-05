require(utils)

RweaveOrg = function () {
  list(setup = RweaveOrgSetup, runcode = RweaveOrgRuncode, 
       writedoc = RweaveOrgWritedoc, finish = RweaveOrgFinish, 
       checkopts = RweaveOrgOptions)
}

RweaveOrgSetup <-
  function (file, syntax, output = NULL, quiet = FALSE, debug = FALSE, 
            echo = TRUE, eval = TRUE, keep.source = FALSE, split = FALSE, 
            stylepath, pdf = TRUE, eps = TRUE) {
    if (is.null(output)) {
      prefix.string <- basename(sub(syntax$extension, "", file))
      output <- paste(prefix.string, "org", sep = ".")
    }
    else prefix.string <- basename(sub("\\.org$", "", output))
    if (!quiet) 
      cat("Writing to file ", output, "\n", "Processing code chunks ...\n", 
          sep = "")
    output <- file(output, open = "w+")
    if (missing(stylepath)) {
      p <- as.vector(Sys.getenv("SWEAVE_STYLEPATH_DEFAULT"))
      stylepath <- if (length(p) >= 1 && nzchar(p[1])) 
        identical(p, "TRUE")
      else TRUE
    }
    if (stylepath) {
      styfile <- file.path(R.home("share"), "texmf", "Sweave")
      if (.Platform$OS.type == "windows") 
        styfile <- gsub("\\\\", "/", styfile)
      if (length(grep(" ", styfile))) 
        warning(gettextf("path to '%s' contains spaces,\n", 
                         styfile), gettext("this may cause problems when running LaTeX"), 
                domain = NA)
    }
    else styfile <- "Sweave"
    options <- list(prefix = TRUE, prefix.string = prefix.string, 
                    engine = "R", print = FALSE, eval = eval, fig = FALSE, 
                    pdf = pdf, eps = eps, width = 6, height = 6, term = TRUE, 
                    echo = echo, keep.source = keep.source, results = "verbatim", 
                    split = split, strip.white = "true", include = TRUE, 
                    pdf.version = "1.1", pdf.encoding = "default", concordance = FALSE, 
                    expand = TRUE)
    options <- RweaveOrgOptions(options)
    list(output = output, styfile = styfile, havesty = FALSE, 
         haveconcordance = FALSE, debug = debug, quiet = quiet, 
         syntax = syntax, options = options, chunkout = list(), 
         srclines = integer(0), srcfile = srcfile(file))
  }

makeRweaveOrgCodeRunner <- 
  function (evalFunc = RweaveEvalWithOpt) 
{
  RweaveOrgRuncode <- function(object, chunk, options) {
    if (!(options$engine %in% c("R", "S"))) {
      return(object)
    }
    if (!object$quiet) {
      cat(formatC(options$chunknr, width = 2), ":")
      if (options$echo) 
        cat(" echo")
      if (options$keep.source) 
        cat(" keep.source")
      if (options$eval) {
        if (options$print) 
          cat(" print")
        if (options$term) 
          cat(" term")
        cat("", options$results)
        if (options$fig) {
          if (options$eps) 
            cat(" eps")
          if (options$pdf) 
            cat(" pdf")
        }
      }
      if (!is.null(options$label)) 
        cat(" (label=", options$label, ")", sep = "")
      cat("\n")
    }
    chunkprefix <- RweaveChunkPrefix(options)
    if (options$split) {
      chunkout <- object$chunkout[chunkprefix][[1]]
      if (is.null(chunkout)) {
        chunkout <- file(paste(chunkprefix, "tex", sep = "."), 
                         "w")
        if (!is.null(options$label)) 
          object$chunkout[[chunkprefix]] <- chunkout
      }
    }
    else chunkout <- object$output
    saveopts <- options(keep.source = options$keep.source)
    on.exit(options(saveopts))
    SweaveHooks(options, run = TRUE)
    chunkexps <- try(parse(text = chunk), silent = TRUE)
    RweaveTryStop(chunkexps, options)
    openSinput <- FALSE
    openSchunk <- FALSE
    if (length(chunkexps) == 0) 
      return(object)
    srclines <- attr(chunk, "srclines")
    linesout <- integer(0)
    srcline <- srclines[1]
    srcrefs <- attr(chunkexps, "srcref")
    if (options$expand) 
      lastshown <- 0
    else lastshown <- srcline - 1
    thisline <- 0
    for (nce in 1:length(chunkexps)) {
      ce <- chunkexps[[nce]]
      if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
        if (options$expand) {
          srcfile <- attr(srcref, "srcfile")
          showfrom <- srcref[1]
          showto <- srcref[3]
        }
        else {
          srcfile <- object$srcfile
          showfrom <- srclines[srcref[1]]
          showto <- srclines[srcref[3]]
        }
        dce <- getSrcLines(srcfile, lastshown + 1, showto)
        leading <- showfrom - lastshown
        lastshown <- showto
        srcline <- srclines[srcref[3]]
        while (length(dce) && length(grep("^[[:blank:]]*$", 
                                          dce[1]))) {
          dce <- dce[-1]
          leading <- leading - 1
        }
      }
      else {
        dce <- deparse(ce, width.cutoff = 0.75 * getOption("width"))
        leading <- 1
      }
      if (object$debug) 
        cat("\nRnw> ", paste(dce, collapse = "\n+  "), 
            "\n")
      if (options$echo && length(dce)) {
        if (!openSinput) {
          if (!openSchunk) {
            cat("#+BEGIN_LaTeX\n", file = chunkout, append = TRUE)
            cat("\\begin{Schunk}\n", file = chunkout, append = TRUE)
            linesout[thisline + 1:2] <- srcline
            thisline <- thisline + 2
            openSchunk <- TRUE
          }
          cat("\\begin{Sinput}", file = chunkout, append = TRUE)
          openSinput <- TRUE
        }
        cat("\n", paste(getOption("prompt"), dce[1:leading], 
                        sep = "", collapse = "\n"), file = chunkout, 
            append = TRUE, sep = "")
        if (length(dce) > leading) 
          cat("\n", paste(getOption("continue"), dce[-(1:leading)], 
                          sep = "", collapse = "\n"), file = chunkout, 
              append = TRUE, sep = "")
        linesout[thisline + 1:length(dce)] <- srcline
        thisline <- thisline + length(dce)
      }
      tmpcon <- file()
      sink(file = tmpcon)
      err <- NULL
      if (options$eval) 
        err <- evalFunc(ce, options)
      cat("\n")
      sink()
      output <- readLines(tmpcon)
      close(tmpcon)
      if (length(output) == 1 & output[1] == "") 
        output <- NULL
      RweaveTryStop(err, options)
      if (object$debug) 
        cat(paste(output, collapse = "\n"))
      if (length(output) > 0 & (options$results != "hide")) {
        if (openSinput) {
          cat("\n\\end{Sinput}\n", file = chunkout, append = TRUE)
          linesout[thisline + 1:2] <- srcline
          thisline <- thisline + 2
          openSinput <- FALSE
        }
        if (options$results == "verbatim") {
          if (!openSchunk) {
            cat("#+BEGIN_LaTeX\n", file = chunkout, append = TRUE)
            cat("\\begin{Schunk}\n", file = chunkout, 
                append = TRUE)
            linesout[thisline + 1:2] <- srcline
            thisline <- thisline + 2
            openSchunk <- TRUE
          }
          cat("\\begin{Soutput}\n", file = chunkout, 
              append = TRUE)
          linesout[thisline + 1] <- srcline
          thisline <- thisline + 1
        }
        output <- paste(output, collapse = "\n")
        if (options$strip.white %in% c("all", "true")) {
          output <- sub("^[[:space:]]*\n", "", output)
          output <- sub("\n[[:space:]]*$", "", output)
          if (options$strip.white == "all") 
            output <- sub("\n[[:space:]]*\n", "\n", output)
        }
        cat(output, file = chunkout, append = TRUE)
        count <- sum(strsplit(output, NULL)[[1]] == "\n")
        if (count > 0) {
          linesout[thisline + 1:count] <- srcline
          thisline <- thisline + count
        }
        remove(output)
        if (options$results == "verbatim") {
          cat("\n\\end{Soutput}\n", file = chunkout, 
              append = TRUE)
          linesout[thisline + 1:2] <- srcline
          thisline <- thisline + 2
        }
      }
    }
    if (openSinput) {
      cat("\n\\end{Sinput}\n", file = chunkout, append = TRUE)
      linesout[thisline + 1:2] <- srcline
      thisline <- thisline + 2
    }
    if (openSchunk) {
      cat("\\end{Schunk}\n", file = chunkout, append = TRUE)
      cat("#+END_LaTeX\n", file = chunkout, append = TRUE)
      linesout[thisline + 1:2] <- srcline
      thisline <- thisline + 2
    }
    if (is.null(options$label) & options$split) 
      close(chunkout)
    if (options$split & options$include) {
      cat("#+LaTeX:  \\input{", chunkprefix, "}\n", sep = "", file = object$output, 
          append = TRUE)
      linesout[thisline + 1] <- srcline
      thisline <- thisline + 1
    }
    if (options$fig && options$eval) {
      if (options$eps) {
        grDevices::postscript(file = paste(chunkprefix, 
                                "eps", sep = "."), width = options$width, height = options$height, 
                              paper = "special", horizontal = FALSE)
        err <- try({
          SweaveHooks(options, run = TRUE)
          eval(chunkexps, envir = .GlobalEnv)
        })
        grDevices::dev.off()
        if (inherits(err, "try-error")) 
          stop(err)
      }
      if (options$pdf) {
        grDevices::pdf(file = paste(chunkprefix, "pdf", 
                         sep = "."), width = options$width, height = options$height, 
                       version = options$pdf.version, encoding = options$pdf.encoding)
        err <- try({
          SweaveHooks(options, run = TRUE)
          eval(chunkexps, envir = .GlobalEnv)
        })
        grDevices::dev.off()
        if (inherits(err, "try-error")) 
          stop(err)
      }
      if (options$include) {
        chunksuffix <- ifelse(options$eps, "eps", "pdf")
        cat("[[./", paste(chunkprefix, chunksuffix, sep = "."), "]]\n", 
            sep = "", file = object$output, append = TRUE)
        linesout[thisline + 1] <- srcline
        thisline <- thisline + 1
      }
    }
    object$linesout <- c(object$linesout, linesout)
    return(object)
  }
  RweaveOrgRuncode
}

RweaveOrgRuncode <- makeRweaveOrgCodeRunner()

RweaveOrgWritedoc <-
  function (object, chunk) {
    linesout <- attr(chunk, "srclines")
    ## This part of the function adds the appropriate \usepackage
    ## directive and begins the document.  Skip this for now, but
    ## eventually add in detection for #+LATEX_PREAMBLE
###     if (length(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk))) 
###       object$havesty <- TRUE
###     if (!object$havesty) {
###       begindoc <- "^[[:space:]]*\\\\begin\\{document\\}"
###       which <- grep(begindoc, chunk)
###       if (length(which)) {
###         chunk[which] <- sub(begindoc, paste("\\\\usepackage{", 
###                                             object$styfile, "}\n\\\\begin{document}", sep = ""), 
###                             chunk[which])
###         linesout <- linesout[c(1:which, which, seq(from = which + 
###                                                    1, length.out = length(linesout) - which))]
###         object$havesty <- TRUE
###       }
###     }
    while (length(pos <- grep(object$syntax$docexpr, chunk))) {
      cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1]])
      cmd <- substr(chunk[pos[1]], cmdloc, cmdloc + attr(cmdloc, 
                                                         "match.length") - 1)
      cmd <- sub(object$syntax$docexpr, "\\1", cmd)
      if (object$options$eval) {
        val <- as.character(eval(parse(text = cmd), envir = .GlobalEnv))
        if (length(val) == 0) 
          val <- ""
      }
      else val <- paste("\\\\verb{<<", cmd, ">>{", sep = "")
      chunk[pos[1]] <- sub(object$syntax$docexpr, val, chunk[pos[1]])
    }
    while (length(pos <- grep(object$syntax$docopt, chunk))) {
      opts <- sub(paste(".*", object$syntax$docopt, ".*", sep = ""), 
                  "\\1", chunk[pos[1]])
      object$options <- SweaveParseOptions(opts, object$options, 
                                           RweaveOrgOptions)
      if (isTRUE(object$options$concordance) && !object$haveconcordance) {
        savelabel <- object$options$label
        object$options$label <- "concordance"
        prefix <- RweaveChunkPrefix(object$options)
        object$options$label <- savelabel
        object$concordfile <- paste(prefix, "org", sep = ".")
        chunk[pos[1]] <- sub(object$syntax$docopt, paste("\\\\input{", 
                                                         prefix, "}", sep = ""), chunk[pos[1]])
        object$haveconcordance <- TRUE
      }
      else chunk[pos[1]] <- sub(object$syntax$docopt, "", chunk[pos[1]])
    }
    cat(chunk, sep = "\n", file = object$output, append = TRUE)
    object$linesout <- c(object$linesout, linesout)
    return(object)
  }

RweaveOrgFinish <-
  function (object, error = FALSE) {
    outputname <- summary(object$output)$description
    inputname <- object$srcfile$filename
    if (!object$quiet && !error) 
      cat("\n", gettextf("You can now run org-export-as-latex on '%s'", outputname), 
          "\n", sep = "")
    close(object$output)
    if (length(object$chunkout) > 0) 
      for (con in object$chunkout) close(con)
    if (object$haveconcordance) {
      linesout <- object$linesout
      vals <- rle(diff(linesout))
      vals <- c(linesout[1], as.numeric(rbind(vals$lengths, 
                                              vals$values)))
      concordance <- paste(strwrap(paste(vals, collapse = " ")), 
                           collapse = " %\n")
      special <- paste("\\special{concordance:", outputname, 
                       ":", inputname, ":%\n", concordance, "}\n", sep = "")
      cat(special, file = object$concordfile)
    }
    invisible(outputname)
  }  

RweaveOrgOptions <- RweaveLatexOptions
