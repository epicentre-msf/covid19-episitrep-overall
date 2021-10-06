

# Render analysis -------------------------------------

rmarkdown::render(
  input = file.path(path.Rmd, 'analysis_bgd.Rmd'), 
  output_file =  paste0(week_report, '_', 'analysis_bengladesh', '.html'),
  output_dir  = file.path(path.local.week, "bgd"))




# Create credentials ----------------------------------

# install.packages(c("blastula", "keyring"))

# Run once to generate credentials (puis supprimer du script)
# blastula::create_smtp_creds_key(
#   id = "",          # Name the Credential (un nom qui a du sens)
#   user = "",        # User E-mail Address
#   provider = "office365",       # Provider
#   use_ssl = TRUE                # Ensure SSL is used
# )

library(blastula)
library(keyring)

# Add yourself
if(Sys.info()[['user']] == "M-MOUSSET") {
  mail_from = "mathilde.mousset@epicentre.msf.org"
  mail_creds = creds_key("credential_mail_mousset")
}



# Compose and send mail -------------------------------



compose_email(body = glue::glue("Hello,

  Here are the analyses for Bengladesh,

  Sincerely,

  Mathilde")) %>%
  
  add_attachment(
    file = file.path(path.local.week, "bgd", 
                     paste0(week_report, '_', 'analysis_bengladesh', '.html'))) %>%
  smtp_send(
    from = mail_from,
    to = c("cxb-epidem@oca.msf.org",
           mail_from),
    subject = "analysis Bengladesh",
    credentials = mail_creds)

