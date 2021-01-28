# server side of the app

server <- shinyServer(function(input, output) {

  # responses_dates_summary --------------------------------------------------
  output$responses_dates_summary <- renderPlot({

    logs_dates_summary <- logs %>%
      filter(
        completed,
        version %in% input$dashboard_version
      ) %>%
      arrange(time_stamp) %>%
      count(time_stamp) %>%
      mutate(
        n = cumsum(n),
        n_label = max(n)
      )

    ggplot(logs_dates_summary, aes(x = time_stamp, y = n)) +
      geom_line(size = 1) +
      geom_label(
        aes(x = today(), y = n_label, label = n_label),
        show.legend = FALSE
      ) +
      labs(x = "Time stamp", y = "N", colour = "Version") +
      theme_minimal() +
      theme(
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_text(face = "bold")
      )
  })

  # responses_dates ----------------------------------------------------------
  output$responses_dates <- renderPlot({

    responses_dates <- logs %>%
      filter(
        completed,
        version %in% input$dashboard_version,
        between(
          age,
          input$dashboard_age[1],
          input$dashboard_age[2])
      ) %>%
      mutate(
        version = str_remove_all(
          version,
          pattern = "BL-Short-|BL-Lockdown-|BL-Long-"
        ),
        version = ifelse(
          version %in% c(1, 2),
          "Long",
          paste0("Short-", version))
      ) %>%
      arrange(version, time_stamp) %>%
      count(time_stamp, version) %>%
      group_by(version) %>%
      mutate(
        n = cumsum(n),
        n_label = max(n)
      )

    ggplot(responses_dates, aes(x = time_stamp, y = n, colour = version)) +
      geom_line(size = 1) +
      geom_label(
        aes(x = today(),
            y = n_label,
            label = n_label),
        show.legend = FALSE) +
      labs(x = "Time stamp", y = "N", colour = "Version") +
      scale_color_brewer(palette = "Dark2") +
      theme_minimal() +
      theme(
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_text(face = "bold")
      )
  })

  # responses_ages -----------------------------------------------------------
  output$responses_ages <- renderPlot({
    responses_ages <- logs %>%
      filter(
        completed,
        version %in% input$dashboard_version,
        between(
          age,
          input$dashboard_age[1],
          input$dashboard_age[2]
        )
      ) %>%
      mutate(
        age = round(age),
        version = str_remove_all(
          version,
          pattern = "BL-Short-|BL-Lockdown-|BL-Long-"
        ),
        version = ifelse(
          version %in% c(1, 2),
          "Long",
          paste0("Short-", version)
        )
      ) %>%
      group_by(version, age) %>%
      summarise(n = n(), .groups = "drop")

    ggplot(responses_ages, aes(x = age, y = n, fill = version)) +
      geom_col() +
      labs(x = "Age (months)", y = "N", fill = "Version") +
      scale_fill_brewer(palette = "Dark2") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_text(face = "bold")
      )
  })

  # responses_lps ------------------------------------------------------------
  output$responses_lps <- renderPlot({
    responses_lps <- logs %>%
      filter(
        completed,
        version %in% input$dashboard_version,
        between(
          age,
          input$dashboard_age[1],
          input$dashboard_age[2]
        ),
        lp != "Other"
      ) %>%
      mutate(
        age = round(age),
        doe2 = case_when(
          doe_spanish > doe_catalan ~ doe_catalan/100,
          doe_catalan > doe_spanish ~ doe_spanish/100,
          TRUE ~ 0.5
        ),
        doe2_cat = cut(
          doe2,
          breaks = seq(0, 0.5, by = 0.1),
          labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%")
        )
      ) %>%
      drop_na(doe2_cat) %>%
      group_by(age, doe2_cat) %>%
      summarise(n = n(), .groups = "drop")

    ggplot(responses_lps, aes(x = age, y = doe2_cat, fill = n)) +
      geom_tile() +
      labs(x = "Age (months)", y = "Exposure to L2", fill = "N") +
      scale_x_continuous(
        breaks = seq(
          round(min(responses$age, na.rm = TRUE)),
          round(max(responses$age, na.rm = TRUE)),
          by = 2
        )
      ) +
      scale_fill_distiller(palette = "Oranges") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "top"
      )
  })


  # logs ---------------------------------------------------------------------

  observeEvent(
    input$update, {
      withProgress(value = 0, {
        responses_path <- "data/responses.rds"
        if (file.exists(responses_path)) {
          file.remove(responses_path)
        }
        source("global.R")
      })
    })

  output$logs_all <- DT::renderDataTable({
    logs_all <- logs %>%
      select(id, id_exp, id_db, time, study, version, age, date_sent, time_stamp, progress)

    DT::datatable(
      logs_all,
      rownames = FALSE,
      width = "1000px",
      height = "4000px",
      style = "bootstrap",
      filter = "top",
      colnames = c("ID", "ID (Exp.)", "ID (DB)", "Time", "Study", "Version", "Age", "Date sent", "Time stamp", "Progress (%)"),
      options = list(
        pageLength = 8,
        autoWidth = TRUE
      )
    ) %>%
      DT::formatStyle(
        columns = "id",
        fontWeight = "bold"
      ) %>%
      DT::formatRound(
        columns = "age",
        digits = 0
      ) %>%
      DT::formatStyle(
        columns = "progress",
        backgroundColor = DT::styleEqual(c("100%"), c("#a5f0c7"))
      )
  })

  output$logs_successful <- DT::renderDataTable({
    logs_successful <- logs %>%
      filter(
        progress %in% paste0(95:100, "%"),
        version %!in% c("CBC", "DevLex"),
        code %!in% new_codes
      ) %>%
      select(id, code, date_sent, time_stamp)

    DT::datatable(
      logs_successful,
      rownames = FALSE,
      width = "1000px",
      height = "4000px",
      filter = "none",
      autoHideNavigation = TRUE,
      colnames = c("ID", "Code", "Date sent", "Time stamp"),
      options = list(
        dom = TRUE,
        pageLength = 8,
        autoWidth = TRUE
      )
    ) %>%
      DT::formatStyle(
        columns = "id",
        fontWeight = "bold"
      )
  })

  output$logs_download <- downloadHandler(
    filename = function() {
      paste0('logs-', Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(logs, file, row.names = FALSE)
    }
  )

  # vocabulary ---------------------------------------------------------------
  output$vocabulary <- DT::renderDataTable({
    vocabulary_table <- vocabulary %>%
      mutate(
        vocab_prop = label_percent(accuracy = 0.01)(vocab_prop),
        vocab_size = paste0(
          vocab_count, "/",
          vocab_n, " (",
          vocab_prop, ")")
      ) %>%
      select(-c(vocab_count, vocab_n, vocab_prop)) %>%
      pivot_wider(
        id_cols = c(id, time),
        names_from = c(language, vocab_type),
        values_from = vocab_size
      ) %>%
      clean_names() %>%
      left_join(
        select(logs, id, age, time, study, version, time_stamp, progress),
        by = c("id", "time")
      ) %>%
      relocate(
        id, age, time, study, version, time_stamp,
        catalan_understands, spanish_understands,
        catalan_produces, spanish_produces, progress
      ) %>%
      arrange(desc(time_stamp))

    DT::datatable(
      vocabulary_table,
      rownames = FALSE,
      width = "1000px",
      height = "4000px",
      style = "bootstrap",
      filter = "top",
      colnames = c("ID", "Age", "Time", "Study", "Version", "Timestamp", "CAT (comp.)", "SPA (comp.)", "CAT (prod.)", "SPA (prod.)", "Progress"),
      options = list(
        pageLength = 15,
        autoWidth = TRUE
      )
    ) %>%
      DT::formatRound(
        columns = "age",
        digits = 0
      ) %>%
      DT::formatStyle(
        columns = "id",
        fontWeight = "bold"
      ) %>%
      DT::formatStyle(
        columns = c("catalan_understands", "catalan_produces"),
        backgroundColor = "#e4e9f0"
      ) %>%
      DT::formatStyle(
        columns = "progress",
        backgroundColor = DT::styleEqual(c("100%"), c("#a5f0c7"))
      )

  })

  output$vocabulary_download <- downloadHandler(
    filename = function() {
      paste0("vocabulary-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(vocabulary, file, row.names = FALSE)
    }
  )

  # norms_plot ---------------------------------------------------------------
  output$norms_plot <- renderPlot({
    norms %>%
      filter(
        te %in% unique(norms[norms$item %in% input$norms_item,]$te),
        lp %in% input$norms_lp,
        item_dominance %in% input$norms_item_dominance
      ) %>%
      mutate(type = str_to_sentence(type)) %>%
      ggplot(aes(
        x = as.factor(age_bin),
        y = proportion,
        ymin = ci_lower,
        ymax = ci_upper,
        colour = interaction(lp, item_dominance, sep = " - "),
        fill = interaction(lp, item_dominance, sep = " - "),
        group = interaction(lp, item_dominance, sep = " - "))
      ) +
      facet_grid(language~type) +
      geom_point(
        size = 3,
        position = position_dodge(0.25),
        show.legend = FALSE
      ) +
      geom_hline(yintercept = 0.5, linetype = "dashed", colour = "black") +
      #geom_smooth(method = "loess", formula = "y ~ x", n = 3) +
      geom_errorbar(width = 0, size = 0.80, position = position_dodge(0.1)) +
      labs(
        x = "Age (months)",
        y = "Proportion",
        colour = "Group - Dominance",
        fill = "Group - Dominance",
        group = "Group - Dominance"
      ) +
      #guides(x = guide_axis(n.dodge = 2)) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_colour_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 13),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.position = "left"
      )

  })


  # norms_table --------------------------------------------------------------
  output$norms_table <- DT::renderDataTable({
    container <- withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 3, "Norms"),
          th(colspan = 5, "Catalan"),
          th(colspan = 5, "Spanish")
        ),
        tr(
          lapply(
            c("TE", "Age", "Group", rep(c("Item", "Comp. (L1)", "Comp. (L2)", "Prod. (L1)", "Prod. (L2)"), 2)),
            th
          )
        )
      )
    ))

    norms_table <- norms_processed %>%
      filter(
        te %in% unique(norms[norms$item %in% input$norms_item,]$te),
        lp %in% input$norms_lp
      ) %>%
      mutate(lp = case_when(
        lp=="Monolingual" ~ "ML",
        lp=="Bilingual" ~ "BL",
        lp=="Other" ~ "OT"
      ))

    DT::datatable(
      norms_table,
      rownames = FALSE,
      width = "1000px",
      height = "4000px",
      style = "bootstrap",
      filter = "none",
      container = container,
      options = list(
        pageLength = 15,
        autoWidth = TRUE
      )
    ) %>%
      DT::formatStyle(
        columns = c("age_bin", "lp"),
        fontWeight = "bold"
      ) %>%
      DT::formatStyle(
        columns = c(
          "label_understands_L1_Spanish",
          "value_understands_L1_Spanish", "value_understands_L2_Spanish",
          "value_produces_L1_Spanish", "value_produces_L2_Spanish"
        ),
        backgroundColor = "#e4e9f0"
      ) %>%
      DT::formatStyle(
        columns = c(
          "label_understands_L1_Catalan",
          "value_understands_L1_Catalan", "value_understands_L2_Catalan",
          "value_produces_L1_Catalan", "value_produces_L2_Catalan"
        ),
        backgroundColor = "white"
      ) %>%
      DT::formatStyle(
        columns = c(
          "te", "lp", "age_bin"
        ),
        backgroundColor = "orange",
        fontWeight = "bold"
      )
  })

  output$norms_download <- downloadHandler(
    filename = function() {
      paste0('norms-', Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(norms, file, row.names = FALSE)
    }
  )

  # pool ---------------------------------------------------------------------
  output$pool <- DT::renderDataTable({
    container <- withTags(table(
      class = "display",
      thead(
        tr(
          th(colspan = 3, ""),
          th(colspan = 4, "Catalan"),
          th(colspan = 4, "Spanish")
        ),
        tr(
          lapply(
            c("TE", "Category", "Class", rep(c("Item", "Label", "IPA", "Frequency"), 2)),
            th
          )
        )
      )
    ))

    pool_table <- pool %>%
      select(te, label, ipa, item, language, category, class, frequency_zipf) %>%
      mutate(ipa = paste0("/", ipa, "/")) %>%
      pivot_wider(
        id_cols = c(te, category, class),
        names_from = "language",
        values_from = c(item, label, ipa, frequency_zipf),
        values_fn = first_non_na
      ) %>%
      arrange(te) %>%
      clean_names() %>%
      relocate(
        te, category, class,
        item_catalan, label_catalan, ipa_catalan, frequency_zipf_catalan,
        item_spanish, label_spanish, ipa_spanish, frequency_zipf_spanish
      )

    DT::datatable(
      pool_table,
      rownames = FALSE,
      width = "1000px",
      height = "4000px",
      style = "bootstrap",
      filter = "top",
      container = container,
      options = list(
        pageLength = 15,
        autoWidth = TRUE
      )
    ) %>%
      DT::formatStyle(
        columns = "te",
        fontWeight = "bold"
      ) %>%
      DT::formatRound(
        columns = c("frequency_zipf_catalan", "frequency_zipf_spanish"),
        digits = 2
      ) %>%
      DT::formatStyle(
        columns = c("te", "category", "class"),
        backgroundColor = "orange"
      ) %>%
      DT::formatStyle(
        columns = "te",
        fontWeight = "bold"
      ) %>%
      DT::formatStyle(
        columns = c("item_catalan", "label_catalan", "ipa_catalan", "frequency_zipf_catalan"),
        backgroundColor = "white"
      ) %>%
      DT::formatStyle(
        columns = c("item_spanish", "label_spanish", "ipa_spanish", "frequency_zipf_spanish"),
        backgroundColor = "#e4e9f0"
      )
  })

  output$pool_download <- downloadHandler(
    filename = function() {
      paste0('pool-', Sys.Date(), ".csv")
    },
    content = function(file) {
      mutate(pool, version = sapply(version, toString)) %>%
        write.csv(., file, row.names = FALSE)
    }
  )


  # participants -------------------------------------------------------------
  output$participants_table <- DT::renderDataTable({
    participants %>%
      mutate(index = as.numeric(gsub("bilexicon_", "", id))) %>%
      arrange(desc(index)) %>%
      select(id, id_db, code, time, study, cdi, version, date_sent, call, comments) %>%
      DT::datatable(
        rownames = FALSE,
        width = "1000px",
        height = "4000px",
        style = "bootstrap",
        colnames = c("ID", "ID (DB)", "Code", "Time", "Study", "CDI", "Version", "Sent", "Status", "Comments"),
        filter = "top",
        options = list(
          pageLength = 30,
          autoWidth = FALSE
        )
      ) %>%
      DT::formatStyle(
        columns = "id",
        fontWeight = "bold"
      ) %>%
      DT::formatStyle(
        columns = "code",
        fontWeight = "bold"
      )
  })

  # participants_send --------------------------------------------------------
  observeEvent(
    input$send, {
      row_data <- data.frame(
        id = input$id,
        id_exp = input$id_exp,
        id_db = input$id_db,
        code = input$code,
        time = input$time,
        date_birth = input$date_birth,
        age_now = as.numeric(lubridate::today()-lubridate::as_date(input$date_birth))/30,
        study = input$study,
        cdi = input$cdi,
        version = input$version,
        date_test = lubridate::as_date(input$date_test),
        date_sent = lubridate::as_date(input$date_sent),
        call = input$call,
        email_ready = input$email_ready,
        email_sent = input$email_sent,
        reminded = input$reminded,
        completed = input$completed,
        link = ifelse(
          input$cdi %in% c("BL-Lockdown", "BL-Short"),
          paste0(
            "https://bllockdown.formr.org?bl_code=",
            input$code,
            "&version=",
            input$version),
          ""),
        comments = input$comments,
        last_edited = now())
      suppressMessages({
        sheet_append(
          ss = "164DMKLRO0Xju0gdfkCS3evAq9ihTgEgFiuJopmqt7mo",
          sheet = "Participants",
          data = row_data
        )
      })
      shinyalert(
        "Saved",
        "Participant added successfully",
        type = "success"
      )
    })

  # participants_email -------------------------------------------------------
  observeEvent(
    input$email, {
      output$email_subject <- renderText({
        "¡Colabora en nuestra investigación! - Babylab Universitat Pompeu Fabra"
      })
      output$email_body <- renderText({
        "Estimada familia,

        Os escribimos desde el BabyLab de la Universitat Pompeu Fabra para haceros llegar un cuestionario en el que os preguntaremos algunos datos sobre el entorno lingüístico de NAME y si conoce las palabras de una lista. Sigue el siguiente enlace para acceder al cuestionario:

        ENLACE

        El cuestionario dura aproximadamente 30 minutos. Podéis encontrar más información sobre los estudios que realizamos en la página del Centro para el Cerebro y la Cognición de la Universitat Pompeu Fabra.

        Sin la participación de las familias nuestra investigación no sería posible.

        ¡Gracias por vuestra colaboración!\n\nSaludos,

        Daniela y Gonzalo"
      })
      output$email_link <- renderText({
        paste0(
          "https://bllockdown.formr.org/?bl_code=",
          input$code,
          "&version=",
          input$version
        )
      })
    })

  # participants_close -------------------------------------------------------
  observeEvent(
    input$close, {
      stopApp()
    })

})
