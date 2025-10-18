recreate_context <- function() {
  lares::try_require("pdftools")
  # Load sources of information
  tut <- paste(pdftools::pdf_text("inst/www/tutorial.pdf"), collapse = " && ")
  load("data/glossary.RData")
  glo <- paste(unlist(lapply(glossary, function(x) apply(x, 1, function(row) paste(row, collapse = ": ")))), collapse = ";")
  context <- paste(c(tut, glo), collapse = " --- ")
  context <- trimws(gsub('    |      | •   |    | {2,}|\\{2,}|\\n', " ", context))
  nchar(context) # 104851
  saveRDS(context, "inst/www/context.RDS")
}

# Run to recreate context.RData
recreate_context()

######### ADD TO GLOSSARY
load("data/glossary.RData")
colnames(glossary$inputs)
glossary$inputs <- rbind(
  glossary$inputs,
  data.frame(
    Parameter = "Units of conversion",
    Description = "When modeling conversions (units), define what each unit of conversion is: prescriptions, units sold, etc.",
    Required = FALSE,
    Default = ""
  ))
save(glossary, file = "data/glossary.RData")
