check_missing_packages <- function(install_missing_packages = FALSE){


files <- list.files(
        pattern = '[.](r|rmd)$', # TODO: Explain what kind of files do you wont? (nm)
        all.files = T,
        recursive = T,
        full.names = T,
        ignore.case =T)

# read in source code
code <- unlist(sapply(files, scan, what = 'character', quiet = TRUE))

# TODO: I know it is a bad habit, but what about "require" instead ob "library"? (nm)
# TODO: Make a regex out of it may fill our need better! (nm)
# retain only source code starting with library
code <- code[grepl('^library', code, ignore.case=T)]
code <- gsub('^library[(]', '', code)
code <- gsub('[)]', '', code)
code <- gsub('^library$', '', code)

# retain unique packages
uniq_packages <- unique(code)

# kick out "empty" package names
uniq_packages <- uniq_packages[!uniq_packages == '']

# order alphabetically
uniq_packages <- uniq_packages[order(uniq_packages)]

cat('Required packages: \n')
cat(paste0(uniq_packages, collapse= ', '),fill=T)
cat('\n\n\n')

# retrieve list of already installed packages
installed_packages <- installed.packages()[, 'Package']

# identify missing packages
to_be_installed <- setdiff(uniq_packages, installed_packages)

# TODO: The next part is messy! Why is the length(...)> part splitted?

if (length(to_be_installed)==length(uniq_packages)) cat('All packages need to be installed.\n')
if (length(to_be_installed)>0) {
  cat('Some needed packages already exist; some are missing.\n')
  cat("The following packages are missing:\n")
  cat(to_be_installed)
}
if (length(to_be_installed)==0) cat('All packages installed already!\n')

# install missing packages
if ((length(to_be_installed)>0) & install_missing_packages) install.packages(to_be_installed, repos = 'https://cloud.r-project.org')

cat('\nDone!\n\n')

}
