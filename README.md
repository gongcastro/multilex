# multilex: **Multi**lingual **lex**ical assessment using online surveys

Stablish reproducible workflows for assessing lexical development online using [formR](https://formr.org/). This package extends the functionalities of formr (see `formr` [repository](https://github.com/rubenarslan/formr)) to ease the standardisation of online vocabulary checklists used by developmental psychologists. This package covers two functions: the creation of surveys from Excel/CSV/TSV files, and the retrieval and preprocessing of data. A key motivation for this package is that all of these actions can be performed from the same R session, increasing the reproducibility of the workflow.

## Retrieve data

* `ml_participants` generates a dataframe with the information of all participants that have participated or are candidates to participate in any of the versions of MultiLex. It takes `google_email` (the email address you use in your Google Drive account, where the `Participants` database is stored) as argument. You will need to provide your Google credentials to allow it to access the `Participants` database in Google Drive.
* `ml_responses` generates a dataframe with participant's responses to each item, along with some session-specific metadata. It takes `participants` (the output of `ml_participants`), `formr_email` (the email address you use in your formr user), and `runs` (a character vector that can take zero, one, or multiple of the following values: "formr2", "formr-short", "formr-lockdown") as arguments. Only responses from the versions indicated in `runs` input will be updated. For the rest, data will be retrieved from the their last update.
* `ml_app` launches a Shiny app in the browser to generate logs, add new participants and other features (still in progress).
* `ml_logs` generates a dataframe that contains participant-level information. Each row is a participant's response and each column is a variable. The same participant will always be identified with the same `id`. The variable `time` indexes how many times that participant has been sent the questionnaire, independently of whether a response was obtained from them. It `participants`, `responses` (the output of `ml_responses`), `bilingual_threshold` (minimum degree of exposure to consider a participant as *monolingual*), and `other_threshold` (minimum degree of exposure to consider a participant as *other*) as arguments.
* `ml_vocabulary` generates a dataframe with the vocabulary of each participant (keeping longitudinal data from the same participant in different raws). Comprehensive and productive vocabulary sizes are computed as raw counts (`vocab_count`) and as proportions (`vocab_prop`, calculated from the total of items filled by the participant in the response `vocab_n`).


An example:

```
participants <- ml_participants(google_email = "user@mail.edu") # retrieve participant data

ml_app(participants = participants)

# get participant responses
responses <- ml_responses(participants = participants,
                          formr_email = "user@mail.edu",
                          google_email = "user@mail.edu")

# generate logs
logs <- ml_logs(responses = responses,
                participants = participants,
                bilingual_threshold = 5,
                other_threshold = 10)


# generate vocabulary sizes
vocabulary <- ml_vocabulary(participants = participants,
                            responses = responses)

```
