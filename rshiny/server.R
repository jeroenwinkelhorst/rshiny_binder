# server.R
shinyServer(function(input, output, session) {
  original_names <- reactiveVal(NULL)
  output_dir <- tempdir()
  dest_dir <- fs::path(output_dir, "Verzamelmap")
  Selectie <- fs::path(output_dir, "Selectie")

  observeEvent(input$convert, {
    req(input$videos)

    error_messages <- list()
    fs::dir_create(dest_dir, recurse = TRUE)
    fs::dir_create(Selectie, recurse = TRUE)

    original_names(tibble::tibble(a
      original_name = input$videos$name,
      datapath = input$videos$datapath
    ))

    if (fs::dir_exists(Selectie)) {
      files_to_remove <- fs::dir_ls(Selectie, recurse = FALSE, all = TRUE)
      if (length(files_to_remove)) fs::file_delete(files_to_remove)
    }

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Bezig met opknippen van video's", value = 0)

    total_videos <- length(input$videos$datapath)

    for (i in seq_len(total_videos)) {
      video_file <- input$videos$datapath[i]
      video_name <- stringr::str_remove_all(input$videos$name[i], pattern = "\\.mp4|\\.AVI|\\.MP4")
      video_map_dir <- fs::path(output_dir, "Output")
      fs::dir_create(video_map_dir, recurse = TRUE)
      video_output_dir <- fs::path(video_map_dir, video_name)
      fs::dir_create(video_output_dir, recurse = TRUE)

      tryCatch({
        av::av_video_images(video_file, destdir = video_output_dir, fps = input$fps/10)
        jpg_files <- fs::dir_ls(video_output_dir, glob = "*.jpg")
        if (length(jpg_files) == 0) {
          error_messages <- c(error_messages, paste("Geen frames gegenereerd voor video:", video_name))
        }
      }, error = function(e) {
        error_messages <- c(error_messages, paste("Fout bij verwerken van video:", video_name, "-", e$message))
      })

      progress$inc(1 / total_videos, detail = paste("Verwerken video", i, "van", total_videos))
      output$progress_bar <- renderUI({
        tags$div(
          class = "progress",
          tags$div(
            class = "progress-bar",
            role = "progressbar",
            style = paste("width:", round((i / total_videos) * 100, 2), "%;"),
            paste(round((i / total_videos) * 100, 2), "% voltooid")
          )
        )
      })
    }
    subdirs <- fs::dir_ls(fs::path(output_dir, "Output"), type = "directory")
    for (subdir in subdirs) {
      subdir_name <- fs::path_file(subdir)
      jpg_files <- fs::dir_ls(subdir, glob = "*.jpg")
      for (jpg_file in jpg_files) {
        new_name <- paste0(subdir_name, "_", fs::path_file(jpg_file))
        new_path <- fs::path(dest_dir, new_name)
        fs::file_move(jpg_file, new_path)
      }
    }

    if (length(error_messages) > 0) {
      output$status <- renderText({ paste(error_messages, collapse = "\n") })
    } else {
      output$status <- renderText({ paste("Video's zijn opgesplitst in afbeeldingen en opgeslagen in:", dest_dir) })
    }
  })

  observeEvent(input$analyze, {
    req(original_names())
    output$status <- renderText({ "De analyse van de video's kan even duren, pak gerust een kopje koffie ;)" })

    Input_foto <- dest_dir
    Output_folder <- fs::path(output_dir, "Output", "file.json")
    if (file.exists(Output_folder)) file.remove(Output_folder)

    reticulate::py_run_string(paste0("
import subprocess
subprocess.run(['python', '-m', 'speciesnet.scripts.run_model',
                '--folders', r'", Input_foto, "',
                '--predictions_json', r'", Output_folder, "'],
                stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
"))
    DATA <- jsonlite::fromJSON(Output_folder)

    X <-  as.data.frame(DATA$predictions) %>%
      dplyr::select(filepath, prediction) %>%
      tidyr::separate(prediction, sep = ';', into = c('code','groep','familie','klasse','soort2','soortEN','soort')) %>%
      dplyr::filter(groep == 'mammalia') %>%
      tidyr::separate(filepath, sep = '/', into = c('x1','x2','x3','x4','x5','x6','x7','x8','Naam'), remove = FALSE, fill = 'right') %>%
      tidyr::separate(Naam, sep = '_', into = c('Videonaam','y1','y2','y3'), fill = 'right') %>%
      dplyr::mutate(Videonaam = ifelse(y2 == 'image', paste0(Videonaam,'_',y1), Videonaam)) %>%
      dplyr::select(Videonaam, soort)

    unique_videos <- unique(X$Videonaam)
    extensions <- c(".mp4", ".AVI", ".jpg")

    for (video in unique_videos) {
      for (ext in extensions) {
        original_name <- original_names()$original_name[original_names()$original_name == paste0(video, ext)]
        source_path <- original_names()$datapath[original_names()$original_name == original_name]
        if (length(source_path) == 1 && file.exists(source_path)) {
          target_path <- fs::path(Selectie, paste0(video, ext))
          fs::file_copy(source_path, target_path, overwrite = TRUE)
        }
      }
    }

    files_to_zip <- list.files(Selectie, recursive = TRUE, full.names = TRUE)
    output$downloadData <- downloadHandler(
      filename = function() { 'videos.zip' },
      content = function(file) { zip::zipr(zipfile = file, files = files_to_zip) },
      contentType = 'application/zip'
    )
    output$status <- renderText({ 'Analyse voltooid. U kunt nu de resultaten downloaden.' })
    output$spinner <- renderUI(NULL)
  })

  observeEvent(input$analyze_photos, {
    req(input$photos)

    photo_dir <- tempdir()
    selectie_dir <- fs::path(photo_dir, "SelectieFotos")
    fs::dir_create(selectie_dir, recurse = TRUE)

    photo_paths <- input$photos$datapath
    photo_names <- input$photos$name
    for (i in seq_along(photo_paths)) {
      fs::file_copy(photo_paths[i], fs::path(selectie_dir, photo_names[i]), overwrite = TRUE)
    }

    session$sendCustomMessage("show_spinner", TRUE)
    output$photo_progress_bar <- renderUI({
      tags$div(
        class = "progress",
        tags$div(
          class = "progress-bar progress-bar-striped progress-bar-animated",
          role = "progressbar",
          style = "width: 100%;",
          "Analyse bezig... dit kan even duren"
        )
      )
    })
    output$photo_status <- renderText("Analyse gestart...")

    later::later(function() {
      Output_json <- fs::path(photo_dir, "photo_predictions.json")
      if (file.exists(Output_json)) file.remove(Output_json)

      reticulate::py_run_string(paste0("
import subprocess
subprocess.run(['python', '-m', 'speciesnet.scripts.run_model',
                '--folders', r'", selectie_dir, "',
                '--predictions_json', r'", Output_json, "'],
                stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
"))
      DATA <- jsonlite::fromJSON(Output_json)
      X <- as.data.frame(DATA$predictions) %>%
        dplyr::select(filepath, prediction) %>%
        tidyr::separate(prediction, sep = ';', into = c('code','groep','familie','klasse','soort2','soortEN','soort')) %>%
        dplyr::filter(groep == 'mammalia') %>%
        dplyr::mutate(bestandsnaam = basename(filepath))

      relevante_fotos <- fs::path(selectie_dir, X$bestandsnaam)
      output$downloadPhotos <- downloadHandler(
        filename = function() { 'relevante_fotos.zip' },
        content = function(file) { zip::zipr(zipfile = file, files = relevante_fotos) },
        contentType = "application/zip"
      )
      output$photo_status <- renderText({
        paste(length(relevante_fotos), "foto('s) met dieren gevonden en gebundeld.")
      })
      session$sendCustomMessage("hide_spinner", TRUE)
      output$photo_progress_bar <- renderUI(NULL)
    }, delay = 0.1)
  })
})
