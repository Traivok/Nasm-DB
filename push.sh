#!/bin/bash
git add $1
printf "Commit message: "
read commiter
git commit -m "$commiter"
git pull
git push