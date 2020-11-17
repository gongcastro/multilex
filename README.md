# multilex: **Multi**lingual **lex**ical assessment using online surveys

Stablish reproducible workflows for assessing lexical development online using [formR](https://formr.org/). This package extends the functionalities of formr (see `formr` [repository](https://github.com/rubenarslan/formr)) to ease the standardisation of online vocabulary checklists used by developmental psychologists. This package covers two functions: the creation of surveys from Excel/CSV/TSV files, and the retrieval and preprocessing of data. A key motivation for this package is that all of these actions can be performed from the same R session, increasing the reproducibility of the workflow.

## Retrieve data

* `ml_participants` generates a dataframe with the information of all participants that have participated or are candidates to participate in any of the versions of MultiLex. It takes `google_email` (the email address you use in your Google Drive account, where the `Participants` database is stored) as argument. You will need to provide your Google credentials to allow it to access the `Participants` database in Google Drive.
* `ml_responses` generates a dataframe with participant's responses to each item, along with some session-specific metadata. It takes `participants` (the output of `ml_participants`), `formr_email` (the email address you use in your formr user), and `runs` (a character vector that can take zero, one, or multiple of the following values: "formr2", "formr-short", "formr-lockdown") as arguments. Only responses from the versions indicated in `runs` input will be updated. For the rest, data will be retrieved from the their last update.
* `ml_logs` generates a dataframe that contains participant-level information. Each row is a participant's response and each column is a variable. The same participant will always be identified with the same `id`. The variable `time` indexes how many times that participant has been sent the questionnaire, independently of whether a response was obtained from them. It `participants`, `responses` (the output of `bl_responses`), ` and `bilingual_threshold` (minimum degree of exposure to consider a participant as *monolingual*) as arguments.
* `ml_logs_summary` generates a dataframe with the sample size accross ages and lingusitic profiles.

An example:

```
participants <- ml_participants(google_email = "user@mail.edu") # retrieve participant data

# get participant responses
responses <- ml_responses(participants = participants,
                          formr_email = "user@mail.edu",
                          google_email = "user@mail.edu")

# generate logs
logs <- ml_logs(responses = responses,
                participants = participants,
                bilingual_threshold = 95)

# generate log summary
log_summary <- ml_logs_summary(logs = logs)
```
