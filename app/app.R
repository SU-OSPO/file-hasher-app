# app.R -- Shinylive app to rename arbitrary files by SHA-256 hash and emit
# accompanying CSVs.

library(shiny)
library(digest)
library(zip)

# ---- Memory safeguard -----------------------------------------------------
options(shiny.maxRequestSize = 150 * 1024^2)  # 150 MB

# ---- Helpers --------------------------------------------------------------

# SHA-256 hex digest of the raw bytes of a file.
# Using readBin() + serialize = FALSE so we hash the actual file bytes
# rather than R's serialised wrapper around them.
sha256_file <- function(path) {
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  digest::digest(bytes, algo = "sha256", serialize = FALSE)
}

# Given a full hex hash and the set of already-used stems, return a unique
# stem starting at `prefix_len` characters and extending by 2 on collision.
# Falls back to the full digest in the (vanishingly unlikely) worst case.
generate_hashed_stem <- function(full_hash, used_names, prefix_len = 8L) {
  len <- prefix_len
  repeat {
    candidate <- substr(full_hash, 1L, len)
    if (!(candidate %in% used_names)) return(candidate)
    len <- len + 2L
    if (len > nchar(full_hash)) {
      # Extremely unlikely fallback
      return(full_hash)
    }
  }
}

# ---- UI -------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("File hash renamer"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "files", "Upload files",
        multiple = TRUE
      ),
      helpText("Total upload capped at 150 MB."),
      numericInput(
        "prefix_len", "Initial hash prefix length",
        value = 8, min = 4, max = 64, step = 2
      ),
      actionButton("process", "Compute hashes", class = "btn-primary"),
      hr(),
      downloadButton("download_zip", "Download renamed files (.zip)"),
      br(), br(),
      downloadButton("download_csv", "Download mapping CSV"),
      br(), br(),
      downloadButton("download_ids_csv", "Download IDs CSV")
    ),
    mainPanel(
      h4("Preview"),
      helpText("Rows sorted alphabetically by id."),
      tableOutput("preview")
    )
  )
)

# ---- Server ---------------------------------------------------------------

server <- function(input, output, session) {

  # Holds the mapping data.frame once the user clicks "Compute hashes"
  mapping_rv <- reactiveVal(NULL)

  observeEvent(input$process, {
    req(input$files)
    files <- input$files  # cols: name, size, type, datapath

    # Sort by original filename so collision-driven stem extensions are
    # reproducible across runs of the same input set.
    files <- files[order(files$name), , drop = FALSE]

    n <- nrow(files)
    used_names <- character(0)
    rows <- vector("list", n)

    # First pass: compute the SHA-256 of each file and pick a unique stem.
    # Wrapped in withProgress so the user sees per-file progress on big batches.
    # Each file's original extension is preserved on its new name; files with
    # no extension stay extensionless.
    withProgress(message = "Hashing files", value = 0, {
      for (i in seq_len(n)) {
        full_hash <- sha256_file(files$datapath[i])
        stem <- generate_hashed_stem(
          full_hash, used_names, prefix_len = as.integer(input$prefix_len)
        )
        used_names <- c(used_names, stem)
        ext <- tools::file_ext(files$name[i])
        new_name <- if (nzchar(ext)) paste0(stem, ".", ext) else stem
        rows[[i]] <- data.frame(
          id           = stem,
          new_filename = new_name,
          old_filename = files$name[i],
          sha256_hex   = full_hash,
          datapath     = files$datapath[i],   # kept for ZIP staging; not exported
          stringsAsFactors = FALSE
        )
        incProgress(1 / n, detail = sprintf("file %d of %d", i, n))
      }
    })

    df <- do.call(rbind, rows)
    # Sort alphabetically by id. When all uploaded files share an extension
    # this is identical to sorting by new_filename (and therefore matches the
    # default file-browser order of the ZIP contents); with mixed extensions
    # it still gives a clean alphabetical id order in the IDs CSV.
    df <- df[order(df$id), , drop = FALSE]
    mapping_rv(df)
  })

  output$preview <- renderTable({
    df <- mapping_rv()
    req(df)
    # Drop the internal datapath column from the on-screen preview
    df[, c("new_filename", "old_filename", "sha256_hex")]
  })

  output$download_zip <- downloadHandler(
    filename = function() "renamed_files.zip",
    content  = function(file) {
      df <- mapping_rv()
      req(df)
      # Stage renamed copies in a temp directory, then zip the directory.
      # We copy rather than rename so the originals remain available if the
      # user wants to re-export with a different prefix length.
      stage <- tempfile("renamed_")
      dir.create(stage)
      on.exit(unlink(stage, recursive = TRUE), add = TRUE)

      n <- nrow(df)
      withProgress(message = "Building ZIP", value = 0, {
        # Staging accounts for n / (n + 1) of the progress bar; compression is
        # a single opaque step at the end (zip::zip() doesn't report progress).
        for (i in seq_len(n)) {
          file.copy(df$datapath[i], file.path(stage, df$new_filename[i]))
          incProgress(1 / (n + 1), detail = sprintf("staging %d of %d", i, n))
        }
        zip::zip(
          zipfile = file,
          files   = list.files(stage, full.names = TRUE)
        )
        incProgress(1 / (n + 1), detail = "compressing")
      })
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() "filename-map.csv",
    content  = function(file) {
      df <- mapping_rv()
      req(df)
      # CSV columns: new_filename first, then old name and full hash.
      write.csv(
        df[, c("new_filename", "old_filename", "sha256_hex")],
        file      = file,
        row.names = FALSE
      )
    }
  )

  output$download_ids_csv <- downloadHandler(
    filename = function() "ids.csv",
    content  = function(file) {
      df <- mapping_rv()
      req(df)
      # Single-column CSV of bare IDs (no extension), in the same alphabetical
      # order as the other outputs. mapping_rv() already stores the bare id, so
      # we pull that column directly.
      write.csv(
        data.frame(id = df$id),
        file      = file,
        row.names = FALSE
      )
    }
  )
}

shinyApp(ui, server)
