#' Class providing object with methods for communication with lightning-viz server
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} abstracting a database table.
#' @format \code{\link{R6Class}} object.
#' @examples
#' tf <- tempfile()
#' cn <- RSQLite::dbConnect(RSQLite::SQLite(), tf)
#' DBI::dbWriteTable(cn, "mytab", cars)
#' DBI::dbDisconnect(cn)
#' sto1 <- smalltableobject$new(dbtype = "sqlite", host = tf, tablename = "mytab")
#' class(sto1)
#' sto1[1, 1]
#' @field
#' @field sessionid Stores id of your current session on the server.
#' @field url Stores url of the last visualization created by this object.
#' @field autoopen Checks if the server is automatically opening the visualizations.
#' @field notebook Checks if the server is in the jupyter notebook mode.
#'
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/lightning-viz/lightining-r/}
#'   \item{\code{new(serveraddress)}}{This method is used to create object of this class with \code{serveraddress} as address of the server object is connecting to.}
#'
#'   \item{\code{sethost(serveraddress)}}{This method changes server that you are contacting with to \code{serveraddress}.}
#'   \item{\code{createsession(sessionname = "")}}{This method creates new session on the server with optionally given name in \code{sessionname}.}
#'   \item{\code{usesession(sessionid)}}{This method changes currently used session on the server to the one with id given in \code{sessionid} parameter.}
#'   \item{\code{openviz(vizid = NA)}}{This method by default opens most recently created by this object visualization. If \code{vizid} parameter is given, it opens a visualization with given id instead.}
#'   \item{\code{enableautoopening()}}{This method enables auto opening of every visualisation that you create since that moment. Disabled by default.}
#'   \item{\code{disableautoopening()}}{This method disables auto opening of every visualisation that you create since that moment. Disabled by default.}
#'   \item{\code{line(series, index = NA, color = NA, label = NA, size = NA, xaxis = NA, yaxis = NA, logScaleX = "false", logScaleY = "false")}}{This method creates a line visualization for vector/matrix with each row representing a line, given in \code{series}.}
#'   \item{\code{scatter(x, y, color = NA, label = NA, size = NA, alpha = NA, xaxis = NA, yaxis = NA)}}{This method creates a scatterplot for points with coordinates given in vectors \code{x, y}.}
#'   \item{\code{linestacked(series, color = NA, label = NA, size = NA)}}{This method creates a plot of multiple lines given in matrix \code{series}, with an ability to hide and show every one of them.}
#'   \item{\code{force(matrix, color = NA, label = NA, size = NA)}}{This method creates a force plot for matrix given in \code{matrix}.}
#'   \item{\code{graph(x, y, matrix, color = NA, label = NA, size = NA)}}{This method creates a graph of points with coordinates given in \code{x, y} vectors, with connection given in \code{matrix} connectivity matrix.}
#'   \item{\code{map(regions, weights, colormap)}}{This method creates a world (or USA) map, marking regions given as a vector of abbreviations (3-char for countries, 2-char for states) in \code{regions} with weights given in \code{weights} vector and with \code{colormap} color (string from colorbrewer).}
#'   \item{\code{graphbundled(x, y, matrix, color = NA, label = NA, size = NA)}}{This method creates a bundled graph of points with coordinates given in \code{x, y} vectors, with connection given in \code{matrix} connectivity matrix. Lines on this graph are stacked a bit more than in the \code{graph} function.}
#'   \item{\code{matrix(matrix, colormap)}}{This method creates a visualization of matrix given in \code{matrix} parameter, with its contents used as weights for the colormap given in \code{colormap} (string from colorbrewer).}
#'   \item{\code{adjacency(matrix, label = NA)}}{This method creates a visualization for adjacency matrix given in \code{matrix} parameter.}
#'   \item{\code{scatterline(x, y, t, color = NA, label = NA, size = NA)}}{This method creates a scatterplot for coordinates in vectors \code{x, y} and assignes a line plot to every point on that plot. Each line is given as a row in \code{t} matrix.}
#'   \item{\code{scatter3(x, y, z, color = NA, label = NA, size = NA, alpha = NA)}}{This method creates a 3D scatterplot for coordinates given in vectors \code{x, y, z}.}
#'   \item{\code{image(imgpath)}}{This method uploads image from file \code{imgpath} to the server and creates a visualisation of it.}
#'   \item{\code{gallery(imgpathvector)}}{This method uploads images from vector of file paths \code{imgpathvector} to the server and creates a gallery of these images.}}

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
