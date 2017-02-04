ft.connect <- function(username, password) {
  url = "https://www.google.com/accounts/ClientLogin";
  params = list(Email = username, Passwd = password, accountType="GOOGLE", service= "fusiontables", source = "R_client_API")
  connection = postForm(uri = url, .params = params)
  if (length(grep("error", connection, ignore.case = TRUE))) {
    stop("The wrong username or password")
    return ("")
  }
  authn = strsplit(connection, "\nAuth=")[[c(1,2)]]
  auth = strsplit(authn, "\n")[[c(1,1)]]
  return (auth)
}