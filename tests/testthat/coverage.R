library(covr)
cov <- package_coverage()
print(cov)
zero_coverage(cov)
report(cov, file = file.path("/workdir/tests/coverage-report.html"), browse = FALSE)
codecov(coverage = cov, token = "ed4f7741-28fb-4fc7-a0d8-7deb95868f76")
