usethis::git_vaccinate()
usethis::use_code_of_conduct()
usethis::use_gpl3_license("Pontificia Universidad Catolica de Chile")
usethis::use_lifecycle_badge("stable")

malleco <- read.table("data-raw/malleco.txt", header = T)
malleco <- ts(data = malleco$Value, start = c(min(malleco$Year), 1), end = c(max(malleco$Year), 1), frequency = 1)
usethis::use_data(malleco)
