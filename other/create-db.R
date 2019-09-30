# create database

library(acs)

file_data <- "~/acs.csv"
file_db   <- "~/data/acs/acsdb2"

system.time(acs_db_write(file_data, file_db))
