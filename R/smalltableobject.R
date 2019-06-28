#' Access (small) remote sql-tables just like data frames
#'
#' Use object to query database table just like a data frame, using
#' ...[x, y]  notation.
#'
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return
#' Database table wrapped in an R6 object.
#' @format
#' \code{R6Class} object with S3 class 'smalltableobject'.
#' @usage
#' sto1 <- smalltableobject$new(...) # Named sql-table on db-server.
#' sto1[1:10, 1]
#' sto1[1, 1] <- 99 # Trigger a write to db.
#' sto1[, ] <- sto1[1:10, ] # NB: must use [,]
#'
#' @examples
#' tf <- tempfile()
#' cn <- RSQLite::dbConnect(RSQLite::SQLite(), tf)
#' DBI::dbWriteTable(cn, "mytab", cars)
#' DBI::dbDisconnect(cn)
#' sto1 <- smalltableobject$new(dbtype = "sqlite", host = tf, tablename = "mytab")
#' class(sto1)
#' sto1[1, 1]
#' @field get_host Name of host
#' @field get_dbtype Type of DB. Only "sqlite" allowed, currently.
#' @field get_db Database part of table schema.
#' @field get_tablename Table part of table schema.
#'
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full source code go to \href{https://github.com/dan070/smalltableobject}{Github repo} }
#'   \item{\code{subset_read}}{Don't call directly. Used with overloaded operator. Public by neccessity.}
#'   \item{\code{subset_write}}{Don't call directly. Used with overloaded operator. Public by neccessity.}
#'   \item{\code{print}}{Prints some helpful internal info about object. Also default method of object.}
#' }

smalltableobject <-
  R6::R6Class(
    classname = "smalltableobject",
    #~~~~~~~~~~~~~~~~~~~~~~~~
    # ~ Active ~
    #~~~~~~~~~~~~~~~~~~~~~~~~
    active = list(
      get_host = function(value) {
        if (missing(value)) {
          return(private$host)
        } else {
          stop("Cannot assign to field: host")
        }
      },
      get_dbtype = function(value) {
        if (missing(value)) {
          return(private$dbtype)
        } else {
          stop("Cannot assign to field: dbtype")
        }
      },

      get_db = function(value) {
        if (missing(value)) {
          return(private$db)
        } else {
          stop("Cannot assign to field: db")
        }
      },
      get_tablename = function(value) {
        if (missing(value)) {
          return(private$tablename)
        } else {
          stop("Cannot assign to field: tablename")
        }
      }
    )
    ,


    #~~~~~~~~~~~~~~~~~~~~~~~~
    # ~ Public ~
    #~~~~~~~~~~~~~~~~~~~~~~~~
    public = list(
      #~~~~~~~~~~~~~~~~~~~~~~~~
      # ~ Function: Subset Read ~
      # A rewrite of R:s special function [ and [<-
      # Used in tandem with S3-overloaded operator.
      # Handles selection/subsetting operations.
      #~~~~~~~~~~~~~~~~~~~~~~~~
      subset_read = function(x,
                             y,
                             Nargs, missingx, missingy) {
        if (Nargs == 1 && missingx && missingy) {
          return(private$table_df[])
        }
        if (Nargs == 1 && !missingx && missingy) {
          return(private$table_df[x])
        }
        if (Nargs == 2 && !missingx && missingy) {
          return(private$table_df[x, ])
        }
        if (Nargs == 2 && missingx && !missingy) {
          return(private$table_df[, y])
        }
        if (Nargs == 2 && !missingx && !missingy) {
          return(private$table_df[x, y])
        }
        if (Nargs == 2 && missingx && missingy) {
          return(private$table_df[, ])
        }
        stop("Subsetting operator failed to find correct syntax.")

      },

      #~~~~~~~~~~~~~~~~~~~~~~~~
      # ~ Function: Subset Write ~
      # A rewrite of R:s special function [ and [<-
      # Used in tandem with S3-overloaded operator.
      # Handles assignment & subsetting operations.
      #~~~~~~~~~~~~~~~~~~~~~~~~
      subset_write = function(x,
                              y,
                              value,
                              Nargs,
                              missingx,
                              missingy,
                              missingvalue) {

        if (missingvalue)
          stop("Value for assignment [<-] not supplied!")

        # Make copy and assign,
        # implicitly using Rs own error checking.
        tmp_table_df <-
          private$table_df # Make a copy of the table df.

        # Assign and let R's errors fire, if any.
        if (Nargs == 1 && missingx && missingy) {
          tmp_table_df <- value
        }
        if (Nargs == 1 && !missingx && missingy) {
          tmp_table_df[x] <- value
        }

        if (Nargs == 2 && missingx && missingy) {
          tmp_table_df <- value
        }

        if (Nargs == 2 && !missingx && missingy) {
          tmp_table_df[x, ] <- value
        }
        if (Nargs == 2 && missingx && !missingy) {
          tmp_table_df[, y] <- value
        }
        if (Nargs == 2 && !missingx && !missingy) {
          tmp_table_df[x, y] <- value
        }

        # Assert: classes on temp data frame have not changed.
        tmp_class_value <- sapply(X = tmp_table_df,
                                  FUN = class)
        print(tmp_class_value)
        tmp_class_df <- sapply(X = private$table_df,
                               FUN = class)
        print(tmp_class_df)

        checkmate::assert_set_equal(x = tmp_class_df,
                                    y = tmp_class_value,
                                    ordered = T)

        # Save local copy to data base.
        # Errors will be propagated through here.
        private$save_table_to_database(tmp_table_df)

        # Return
        return(self)

      },


      #~~~~~~~~~~~~~~~~~~~~~~~~
      # ~ Print ~
      #~~~~~~~~~~~~~~~~~~~~~~~~
      print = function(...) {
        cat("SmallTableObject\n")
        cat(paste("db type    : ", private$dbtype, "\n"))
        cat(paste("table name : ", private$tablename, "\n"))
        cat(paste("nrow       : ", nrow(private$table_df), "\n"))
        cat(paste("MD5        : ", private$table_hash, "\n"))

        invisible(self)
      },


      #~~~~~~~~~~~~~~~~~~~~~~~~
      # ~ Initialize ~
      #~~~~~~~~~~~~~~~~~~~~~~~~
      initialize = function(dbtype = NULL,
                            host = NULL,
                            db = NULL,
                            user = NULL,
                            pass = NULL,
                            tablename = NULL) {
        assertcollection <- checkmate::makeAssertCollection()
        checkmate::assertChoice(x = dbtype,
                                choices = private$allowed_dbtypes,
                                add = assertcollection)

        # SQLITE specific
        if (dbtype == "sqlite") {
          # Private field "dbtype" updated.
          private$dbtype <- dbtype

          # Check string input types.
          checkmate::assert_character(
            x = host,
            len = 1,
            min.chars = 1,
            add = assertcollection
          )
          checkmate::assert_character(
            x = tablename,
            len = 1,
            min.chars = 1,
            add = assertcollection
          )
          # Private field "tablename" updated.
          private$tablename <- tablename

          # Check host points to valid sqlite database file.
          if (!checkmate::testFileExists(x = host))
            assertcollection$push("Sqlite requires HOST argument to be local file.")
          checkmate::assertFileExists(x = host, add = assertcollection)

          # Private field "host" updated.
          private$host <- host

          # Check file connection is possible.
          tryCatch({
            rsqliteconnection <-
              RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = host)

            # Check table name indeed exist.
            if (!RSQLite::dbExistsTable(conn = rsqliteconnection,
                                        name = tablename)) {
              assertcollection$push("Table does not exist.")
            }
            # Free connection.
            DBI::dbDisconnect(rsqliteconnection)

            # Get table.
            private$connection <-
              RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = host)
            private$table_df <-
              RSQLite::dbReadTable(conn = private$connection, name = private$tablename)

          },
          error = function(e) {
            assertcollection$push(
              paste(
                "Could not connect to sqlite or get table. host = ",
                private$host,
                ", tablename = ",
                private$tablename

              )
            )
          },
          finally = {
            try(suppressWarnings(RSQLite::dbDisconnect(rsqliteconnection)), silent = T)
            # Halt on all the potential errors.
            checkmate::reportAssertions(collection = assertcollection)

          })

        }

        # Check: data types are simple.
        private$table_types <-
          unlist(lapply(private$table_df, class))
        check_class <-
          length(private$table_types) == length(private$table_df)
        if (check_class == F) {
          assertcollection$push("Some columns have more than 1 data type listed.")
        }

        # Save md5-hash for data frame.
        private$table_hash <-
          private$hash_data_frame(df_to_hash = private$table_df)


      }# initialize function ends here
      ,
      #~~~~~~~~~~~~~~~~~~~~~~~~
      # ~ Finalize ~
      #~~~~~~~~~~~~~~~~~~~~~~~~
      finalize = function() {
        print("Finalize function at gc on object.")
        if (private$dbtype == "sqlite") {
          # Clean up the connection.
          try(suppressWarnings(RSQLite::dbDisconnect(private$connection)), silent = T)
        }
      }# Finalize function ends here
    )# Public fields ends here
    ,
    #~~~~~~~~~~~~~~~~~~~~~~~~
    # ~ Private fields ~
    #~~~~~~~~~~~~~~~~~~~~~~~~

    private = list(
      dbtype = "",
      host = "",
      db = "",
      user = "",
      pass = "",
      tablename = "",
      connection = NA,
      ## TODO: remove "connection" field, and open connection when needed. Else errors like
      # Warning message:
      # call dbDisconnect() when finished working with a connection
      table_df = NA,
      table_types = NA,
      table_hash = NA,
      allowed_dbtypes = c("sqlite"),

      #~~~~~~~~~~~~~~~~~~~~~~~~
      # ~ Private func: hash_data_frame
      # Calc MD5 for each (sorted) column separately. Then MD5 those to 1 value.
      #~~~~~~~~~~~~~~~~~~~~~~~~
      hash_data_frame = function(df_to_hash = NA) {
        # Define local sort function that handles classes not implementing sort.
        sorts <- function(x) {
          if (class(x) == "blob")
            x <- as.integer(unlist(df_to_hash[, 4]))
          return(sort(x))
        }
        # Sort each column independently.
        df1 <- lapply(X = df_to_hash, FUN = sorts)
        # Digest MD5 on each column.
        df2 <-
          lapply(
            X = df1,
            FUN = function(x)
              digest::digest(object = x, algo = "md5")
          )
        # Digest the sorted md5 hashes to 1 final md5 value.
        df3 <-
          digest::digest(object = sort(unlist(df2)), algo = "md5")
        # Return 1 value.
        return(df3)
      }#func:hash_data_frame ends here
      ,
      #~~~~~~~~~~~~~~~~~~~~~~~~
      # ~ Private func: save_table_to_database
      # Write local data frame to data base table.
      #~~~~~~~~~~~~~~~~~~~~~~~~
      save_table_to_database = function(df_to_save) {
        if (private$dbtype == "sqlite") {
          print("save_table_to_database...")
          # Check data base connection working
          if (!dbIsValid(private$connection)) {
            private$connection <-
              dbConnect(drv = RSQLite::SQLite(), dbname = private$host)
            print("new private$connection made")
          }
          checkmate::assert_true(DBI::dbIsValid(private$connection))
          print("Check db connection.")


          # Ensure no data race on local table versus target table
          # ie. download table, compare hashes, else error.
          tmp_data_table <-
            dbReadTable(conn = private$connection, name = private$tablename) # Get DB-table.
          tmp_hash <-
            private$hash_data_frame(df_to_hash = tmp_data_table) # Hash DB-table.
          print(private$table_hash)
          print(tmp_hash)
          checkmate::assert_true(x = private$table_hash == tmp_hash, .var.name = "Hash compare.") # Compare DB-table to local MD5.
          print("Check hashes.")

          # ~~ Upsert the table ~~
          # Truncate the data base table.
          tmp <- dbSendQuery(
            conn = private$connection,
            statement = paste("DELETE FROM ", private$tablename)
          )
          dbClearResult(tmp)
          print("Delete done.")

          # Upsert whole private table to data base table.
          tmp <- dbWriteTable(
            conn = private$connection,
            name = private$tablename,
            value = df_to_save,
            overwrite = T
          )
          print("Upsert entire table done.")

          # Set the private table to the sent in table.
          private$table_df <- df_to_save
          print("Replace local table.")

          # Update private hash (of new table).
          private$table_hash <-
            private$hash_data_frame(private$table_df)
          print("Updated hash ")

          # Release connection
          dbDisconnect(private$connection)
        } # if private$dbtype == "sqlite" ends here.
      }# save_table_to_database ends here.
    )# Private fields ends here
  )# Class ends here
