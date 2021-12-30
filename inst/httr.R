library(httr)
r <- GET("http://httpbin.org/get")
http_status(r)

r <- GET("https://www.nordnet.se/api/2")
http_status(r)
r
headers(r)
content(r)
str(content(r))

r <- GET("https://www.nordnet.se/api/2",
         query = list("Accept-Language" = "da")
)

r <- GET("https://www.nordnet.se/api/2/accounts")

r <- GET("https://www.nordnet.se/api/2/accounts/40976128/info")
http_status(r)

r <- GET("https://www.nordnet.se/api/2/instrument_search/query/optionlist/pairs",
         authenticate(nm, temp)
)

http_status(r)

# https://helmstedt.dk/2021/10/opdateret-program-til-at-hente-transaktioner-hos-nordnet/
# Actual login
# url = 'https://www.nordnet.dk/api/2/authentication/basic/login'
# request = session.post(url, data = {'username': user, 'password': password})

url  <- "https://www.nordnet.dk/api/2/authentication/basic/login"
user <- Sys.getenv("nord_user")
password <- Sys.getenv("nord_pass")

POST(url, body = list(username = user, password = password))


