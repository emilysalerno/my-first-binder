options(repos = c(CRAN = "https://cran.r-project.org"))
install.packages("tidyverse", dependencies = TRUE)
install.packages("rmarkdown", dependencies = TRUE)
install.packages("IRkernel", dependencies = TRUE)

IRkernel::installspec(user = FALSE)
