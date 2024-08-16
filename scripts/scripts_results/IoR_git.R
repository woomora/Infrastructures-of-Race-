# Load necessary packages
library(usethis)
library(git2r)

# Initialize Git in the current R project (if not done already)
usethis::use_git()

# Add your GitHub repository as a remote (if not already done)
remote_add(".", name = "origin", url = "https://github.com/woomora/Infrastructures_of_Race.git")

# Stage all files
add(".", "*")

# Commit the files
commit(".", message = "Initial commit: Add analysis files and scripts")

# Set up credentials using your PAT
credentials <- cred_token()

# Push the commit to GitHub
repo <- repository(".")
push(repo, credentials = credentials)
