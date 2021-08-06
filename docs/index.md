multilex: Multilingual lexical assessment using online surveys

Establish reproducible workflows for assessing lexical development online using formR. This package extends the functionalities of formr (see formr repository) to ease the standardisation of online vocabulary checklists used by developmental psychologists. This package covers two functions: the creation of surveys from Excel/CSV/TSV files, and the retrieval and preprocessing of data. A key motivation for this package is that all of these actions can be performed from the same R session, increasing the reproducibility of the workflow.

To install this package:

install.packages("remotes") # you may need to install this first
remotes::install_github("gongcastro/multilex")

Retrieve data

- ml_connect authenticates both the Google and formr accounts. It takes google_email (the email address you use in your Google Drive account, where the Participants database is stored) and formr_email (email used in the administrator formr account that runs the questionnaire/s) as arguments. You will need to provide your Google credentials to allow it to access the Participants database in Google Drive. You will also be prompted to write the formr password in the console. Please do not write your password in any script you will save later.
- ml_participants generates a data frame with the information of all participants that have participated or are candidates to participate in any of the versions of MultiLex.
- ml_responses generates a data frame with participant’s responses to each item, along with some session-specific metadata. It takes participants (the output of ml_participants) and runs (a character vector that can take zero, one, or multiple of the following values: “formr2”, “formr-short”, “formr-lockdown”) as arguments. Only responses from the versions indicated in runs input will be updated. For the rest, data will be retrieved from the their last update.
- ml_app launches a Shiny app in the browser to generate logs, add new participants and other features (still in progress).
- ml_logs generates a data frame that contains participant-level information. Each row is a participant’s response and each column is a variable. The same participant will always be identified with the same id. The variable time indexes how many times that participant has been sent the questionnaire, independently of whether a response was obtained from them. It takes participants, responses (the output of ml_responses), bilingual_threshold (minimum degree of exposure to consider a participant as monolingual), and other_threshold (minimum degree of exposure to consider a participant as other) as arguments.
- ml_vocabulary generates a data frame with the vocabulary of each participant (keeping longitudinal data from the same participant in different rows). Comprehensive and productive vocabulary sizes are computed as raw counts (vocab_count) and as proportions (vocab_prop, calculated from the total of items filled by the participant in the response vocab_n).
- ml_norms generates a data frame with the estimated proportion of children that understand and/or produce some items for a selected age range and participant profiles. It takes participants, responses, norms_language (language to compute vocabulary norms for: “catalan” and/or “spanish”), norms_type (vocabulary type to compute norms for: “understands”, “produces”), norms_age (age range of participants to compute norms for, as a numeric vector of length two (min-max)), norms_lp (language profile of participants to compute norms for: “Bilingual”, Monolingual“,”Other“), norms_sex (sex of participants to compute norms for:”Female“,”Male”, both included by default), and conf (confidence level of confidence intervals, defaults to 0.95) as arguments.

An example:

```r
ml_connect(google_email = "user@email.com") # authenticate to Google and formr accounts

p <- ml_participants() # retrieve participant data

# get responses
r <- ml_responses(participants = p)
            
# launch shiny app              
ml_app(participants = p, responses = r)

# generate logs
l <- ml_logs(responses = r,
             participants = p,
             bilingual_threshold = 5,
             other_threshold = 10)


# generate vocabulary sizes
v <- ml_vocabulary(participants = p,
                   responses = r)
                            
# generate norms for items
n <- ml_norms(participants = p,
              responses = r)
```
