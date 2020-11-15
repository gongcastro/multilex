# multilex: **Multi**lingual **lex**ical assessment using online surveys

Stablish reproducible workflows for assessing lexical development online using [formR](https://formr.org/). This package extends the functionalities of formr (see `formr` [repository](https://github.com/rubenarslan/formr)) to ease the standardisation of online vocabulary checklists used by developmental psychologists. This package covers two functions: the creation of surveys from Excel/CSV/TSV files, and the retrieval and preprocessing of data. A key motivation for this package is that all of these actions can be performed from the same R session, increasing the reproducibility of the workflow.

## Retrieve data
You can retrieve data from all questionnaire versions using the function `bilexicon_update()`, which takes your email (`email`, the email address you use in your formr user), and the questionnaire version you want to update (`runs`, a character vector that can take zero, one, or multiple of the following values: "formr2", "formr-short", "formr-lockdown"). Only the versions indicated in `runs` input will be updated. For the rest, data will be retrieved from the last update. This `bilexicon_update()` returns a list containint three data frames:

* `responses` contains all responses by all participants. Each row corresponds to a reponse to one item by one participant.
* `participant` contains demographic, linguistic and other relevant information form each participant. Each row corresponds to one questionnaire response by one participant (every time a participant completes any of the questionnaires, a new row is added to this data frame).
* `summary` contains a summary of the sample that has completed each of the questionnaires, split by demographic and linguistic profiles.

```
d <- bilexicon_update(email = "user@mail.com", runs = c("formr2", "formr-short"))
```
