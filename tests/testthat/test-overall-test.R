library(smalltableobject)
library(testthat)

# /////////////////////////////////////////////////////////////////////
# Func: Create data for specific database, given a connection.
# /////////////////////////////////////////////////////////////////////
create_test_data <- function(conn, dbtype = "sqlite", n = 100, seed = 20190427){

  if(dbtype == "sqlite"){

    # Create connection object to sqlite.
    con_temp <- conn
    #DBI::dbListTables(con_temp)

    # Create a table with all pertinent datatypes
    #   -Date/time specific class is not an option for sqlite.
    #   -SQLIte has 4 "storage classes" and 1 column can mix different types(!).
    #   -More on data types : http://www.sqlitetutorial.net/sqlite-data-types/
    DBI::dbExecute(con_temp, "CREATE TABLE test1 (a int , b real , c text , d blob )")

    # Insert 100 random rows for each column.
    #   -No missing values.
    #   -Ensure both positive and negative numerics.
    #   -Ensure empty strings, but not NULLs/NA. Ensure åäö nordic characters in strings.
    #   -Ensure dates in the past, and the future. (not applicable here)
    DBI::dbExecute(con_temp, "INSERT INTO test1
                 VALUES(
                 9223372036854775807,
                 9.0,
                 'abcdefghijklmnopqrstuvxyzåäö 1234567890?!',
                 x'050015a0' ),
                  (
                 -9223372036854775805,
                 -9.9,
                 '',
                 x'0419ffff' )
                 ")
    # Set seed & table size.
    size_n <- n
    set.seed(seed = seed, kind = "default")

    # Generate random numbers to df, and ...
    randomrows <- data.frame(
      a = sample.int(n = 999999999999, size = size_n) * sample(
        x = c(-1, 1),
        size = size_n,
        replace = T
      ),
      b = runif(
        n = size_n,
        min = -99999999,
        max = 99999999
      ),
      c = replicate(n = size_n, expr = paste("'", paste(
        sample(
          x = c(LETTERS, letters),
          size = 10,
          replace = T
        ), collapse = ""
      ), "'", sep = "")  ),
      d = replicate(n = size_n, expr = paste(
        "x'", paste(sample(
          x = c(0:9, letters[1:6]),
          size = 10,
          replace = T
        ), collapse = ""), "'", sep = ""
      ))
    )

    # ... prep for SQL insert...
    inserts <-
      paste(apply(
        X = randomrows,
        MARGIN = 1,
        FUN = function(x)
          paste("(", paste(x, collapse = ","), ")")
      ), collapse = ",")

    # ... and insert into data base table.
    DBI::dbExecute(con_temp, paste("INSERT INTO test1
                                 VALUES", inserts))
    # Disconnect db connection.
    DBI::dbDisconnect(con_temp)



  }

  return(T)
}

# /////////////////////////////////////////////////////////////////////
# Func: Get values from database simpler
# /////////////////////////////////////////////////////////////////////

get_values_from_database <- function(con, where = list()){

  if(length(where) == 0) {out <- ""} else {
    out <- where %>%
      purrr::imap(.f = function(x, y){
        if(class(x) == "character"){
          paste(y, "IN", paste("(", paste(paste("'", x, "'", sep = ""), collapse = ","), ")"))
        } else {
          paste(y, "IN", paste("(", paste(x, collapse = ","), ")") )
        }
      }) %>% purrr::reduce(.f = function(x, y){paste(x, "AND", y)})
    out <- " WHERE " %+% out
  }


  #return()
  stmnt <- glue("SELECT * FROM test1 {out}")
  print(stmnt)
  res <- DBI::dbGetQuery(conn = con, statement = stmnt)
  DBI::dbDisconnect(con) # Kill connection.
  return(res)
}

# /////////////////////////////////////////////////////////////////////
# Func: Give me a quick random data STO-object
# /////////////////////////////////////////////////////////////////////

get_sto <- function(n = 100, seed = 20202020){
  tryCatch({
    # Create data and an object.
    tf <- tempfile()
    create_test_data(conn = DBI::dbConnect(drv = RSQLite::SQLite(), dbname = tf), n = n, seed = seed)
    sto2 <- NULL
    sto2 <- smalltableobject$new(dbtype = "sqlite", host = tf, tablename = "test1")
    return(sto2)
  }, error = function(e){
    stop("Couldnt create new object.")
  })#end trycatch
}


# /////////////////////////////////////////////////////////////////////
# Func: Compare 2 data frames
# /////////////////////////////////////////////////////////////////////
compare_dfs <- function(...){
  tryCatch({
    args <- list(...)
    if(length(args) < 2) stop("Not at least 2 dfs input to function compare_dfs.")
    #args %>% str()

    tst <- args %>% purrr::map(checkmate::test_data_frame) %>%
      purrr::reduce(all)
    if(!tst) return(FALSE)

    cmb <- args %>%
      purrr::map(.f = function(x){
        x %>%
          purrr::map(paste) %>%
          purrr::map(sort) %>%
          purrr::map(digest::digest) %>%
          purrr::reduce(c) %>%
          sort %>%
          paste(collapse = "") %>%
          digest::digest(.)
      }) %>%
      purrr::reduce(c) %>%
      expand.grid(., .) %>%
      purrr::map(paste)


    return(purrr::map2(.x = cmb$Var1, .y = cmb$Var2, .f=function(x, y)x==y) %>%
             purrr::reduce(all))
  }, error = function(e){return(FALSE)})
}


# /////////////////////////////////////////////////////////////////////
# Get original table to compare with.
# /////////////////////////////////////////////////////////////////////



# /////////////////////////////////////////////////////////////////////
# Tests
# /////////////////////////////////////////////////////////////////////

test_that("Create table object from nonexistent db should not be possible.", {
  expect_error({
    sto1 <- smalltableobject$new(dbtype = "sqlite",
                           host = tempfile(fileext = ".sqlite"),
                           tablename = "test")

  })
})


# Make re-usable object for the tests.
sto2 <- get_sto()

# Make a copy of original table to compare in tests.
con_temp <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = sto2$get_host)
original_table_df <- DBI::dbGetQuery(con_temp, "select * from test1")
DBI::dbDisconnect(con_temp)



test_that("Returned value is not null", {
  tmp <- sto2[1, 1]
  expect_true({!is.null(tmp)})
})
test_that("Returned value has 1 col", {
  tmp <- sto2[1, 1]
  expect_true({length(tmp) == 1})
})


tmp <- sto2[1, ]
test_that("sto2[1, ] has correct dimensions", {
  expect_true(!is.null(tmp))
  expect_true(length(length(tmp)) == 1)
  expect_true(length(tmp) == 4)
  expect_true(nrow(tmp) == 1)
  expect_true(class(tmp) == "data.frame")
})



test_that("sto2[, 1] has correct dimensions", {
  tmp <- sto2[, 1]
  expect_true(!is.null(tmp))
  expect_true(length(length(tmp)) == 1)
  expect_true(length(tmp) == 102)
  expect_true(class(tmp) != "data.frame")
})



test_that("Boolean vector subsetting works", {
  temp_booleanvec <- sample(x = c(TRUE, FALSE), size = nrow(sto2[,]), replace = T)
  tmp <- sto2[temp_booleanvec, ]
  expect_true(!is.null(tmp))
  expect_true(length(length(tmp)) == 1)
  expect_true(length(tmp) == 4)
  expect_true(nrow(tmp) == sum(temp_booleanvec))
  expect_true(class(tmp) == "data.frame")
})



test_that("Column subsetting with character 1", {
  tmp <- sto2[, "a"]
  expect_true(!is.null(tmp))
  expect_true(length(length(tmp)) == 1)
  expect_true(length(tmp) == 102)
})

test_that("Column subsetting with character 2", {
  tmp <- sto2[, "d"]
  expect_true(!is.null(tmp))
  expect_true(length(length(tmp)) == 1)
  expect_true(length(tmp) == 102)
})


test_that("Out of range row subsetting ie: sto[ 99999999, ]", {
  tmp <- sto2[99999999, ]
  expect_true(!is.null(tmp))
  expect_true(length(length(tmp)) == 1)
  expect_true(length(tmp) == 4)
  expect_true(nrow(tmp) == 1)
  # Not all classes (eg bit64::integer64) returns NA in this setting.
  # So we cannot really test for NA on all columns for "ghost"/out-of-range rows.
})



test_that("Subsetting negative rows ie: sto[ -1, ] ", {
  tmp <- sto2[-c(1:100), ]
  expect_true(!is.null(tmp))
  expect_true(length(length(tmp)) == 1)
  expect_true(length(tmp) == 4)
  expect_true(nrow(tmp) == 2)
})


test_that("Subsetting negative cols ie: sto[ , -1]", {
  tmp <- sto2[, -c(1:2)]
  expect_true(!is.null(tmp))
  expect_true(length(length(tmp)) == 1)
  expect_true(length(tmp) == 2)
  expect_true(nrow(tmp) == 102)
})

test_that("Subsetting negative rows and cols ie: sto[ -1, -1]", {
  tmp <- sto2[-c(1:100), -c(1:2)]
  expect_true(!is.null(tmp))
  expect_true(length(length(tmp)) == 1)
  expect_true(length(tmp) == 2)
  expect_true(nrow(tmp) == 2)
})

test_that("Subsetting total data frame ie: sto[ , ]", {
  tmp <- sto2[, ]
  expect_true(!is.null(tmp))
  expect_true(length(length(tmp)) == 1)
  expect_true(length(tmp) == 4)
  expect_true(nrow(tmp) == 102)
})

test_that("Subsetting outside data frame ie: sto[ , 'nonexistingcolumn']", {
  expect_error(sto2[, "non-existing-column"])
})


test_that("Subsetting outside data frame ie: sto[-999 , ]", {
  expect_equal(dim(sto2[-99999, ]), dim(sto2[]))
})

test_that("Subsetting with NA ie: sto[NA , NA]", {
  expect_error(sto2[NA, NA])
})

test_that("Class labels from original table is still intact after reading a bit.", {
  expect_setequal(paste(sapply(original_table_df, class)), paste(sapply(sto2[], class)))
})

test_that("", {
  expect_equal(2 * 2, 4)
})
test_that("", {
  expect_equal(2 * 2, 4)
})




sto2 <- NULL

test_that("we got here", {
  expect_equal(2 * 2, 4)
})



