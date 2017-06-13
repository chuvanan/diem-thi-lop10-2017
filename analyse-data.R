

## -----------------------------------------------------------------------------
## get data in

if (interactive()) {
    rm(list = ls())
    gc(reset = TRUE)
}

setwd("~/Documents/diem-thi-lop10-2017/")

dt.chuyen <- readxl::read_xls("diem-ts10-2017baochi-1497260000353.xls", sheet = 1)

dt.thuong <- readxl::read_xls("diem-ts10-2017baochi-1497260000353.xls", sheet = 2)

## sensible colnames

names(dt.thuong) <- c("Sbd", "Ho", "Ten", "Van", "Nn", "Toan")

dt.thuong <- subset(dt.thuong, Van >= 0 & Nn >= 0 & Toan >= 0)

dt.thuong <- dt.thuong[, c("Van", "Nn", "Toan")]

## -----------------------------------------------------------------------------
## explore data


## distribution of scores for each subject

dt.toan <- table(dt.thuong$Toan, dnn = "diem")
dt.toan <- as.data.frame(dt.toan, responseName = "n_toan", stringsAsFactors = FALSE)
dt.toan$diem <- as.double(dt.toan$diem)

dt.van <- table(dt.thuong$Van, dnn = "diem")
dt.van <- as.data.frame(dt.van, responseName = "n_van", stringsAsFactors = FALSE)
dt.van$diem <- as.double(dt.van$diem)

dt.nn <- table(dt.thuong$Nn, dnn = "diem")
dt.nn <- as.data.frame(dt.nn, responseName = "n_nn", stringsAsFactors = FALSE)
dt.nn$diem <- as.double(dt.nn$diem)


## fix the zero bug

dt.toan <- merge(dt.toan,
                 data.frame(diem = seq(0, 10, 0.25)),
                 by = "diem",
                 all = TRUE)
dt.toan$n_toan[is.na(dt.toan$n_toan)] <- 0


dt.van <- merge(dt.van,
                data.frame(diem = seq(0, 10, 0.25)),
                by = "diem",
                all = TRUE)
dt.van$n_van[is.na(dt.van$n_van)] <- 0

dt.nn <- merge(dt.nn,
               data.frame(diem = seq(0, 10, 0.25)),
               by = "diem",
               all = TRUE)
dt.nn$n_nn[is.na(dt.nn$n_nn)] <- 0

## compare distributions
