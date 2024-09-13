#' function to check if files that are read in create empty objects
#' indicating something would be wrong with the file reading.
#' @noRd

check_data <- function(r_code) {

  # Capture the environment to evaluate the code safely
  env <- new.env()

  # List of common data reading functions
  reading_functions <- c("read.csv", "read.csv2", "read.table", "read.delim", "read.delim2",
                         "read_excel", "read.xls", "read.xlsx", "read_xls", "read_xlsx",
                         "fromJSON", "read_json", "xmlParse", "read_xml", "fread",
                         "read_csv", "read_tsv", "read_feather", "h5read", "read.dta",
                         "read_dta", "read.spss", "read_sav", "read_sas7bdat", "read_sas",
                         "readRDS", "read_arff", "read.dbf", "read.mtp", "read.ssd")

  # List of common file extensions
  file_extensions <- c('\\.txt', '\\.xls', '\\.xlsx', '\\.csv', '\\.tsv',
                       '\\.bed', '\\.bw', '\\.bigWig', '\\.fasta', '\\.fq',
                       '\\.gz')


  # Function to check if an object is empty
  is_empty <- function(obj) {
    if (is.data.frame(obj) || is.matrix(obj)) {
      return(nrow(obj) == 0)
    } else if (is.list(obj)) {
      return(length(obj) == 0)
    } else {
      return(FALSE)
    }
  }



  # check for those lines that have a reading function in them if
  # they create an 'empty' object
  # Parse the code string into an expression
  parsed_code <- tryCatch(
    {parse(text = r_code)},
    error= function(e){
      return(e)
    },
    warning = function(w){
      return (w)
    })

  # Deparse the parsed expression into individual elements
  code_elements <- sapply(parsed_code, deparse)


  for (line in code_elements){

    # if we find a line with file extension or a reading function
    # in it, we run those lines
    file_read<-any(sapply(reading_functions, grepl, line))
    file_ex<-any(sapply(file_extensions,grepl,line))

    if (file_read | file_ex){
      rs<-tryCatch(
        {
          eval(parse(text = line), envir = env)
        }, error = function(e){
          return(e)
        }, warning = function (w){
          return(w)
        }
      )
    }
  }

  # List to store results
  results <- list()


  # if there was a problem with reading in the file, return that error
  if (exists("rs") && (methods::is(rs,'warning') || methods::is(rs,'error'))){
    return (NULL)
  } else{ # if there is an empty object created due to file reading return a message
    for (var in ls(env)) {
      obj <- get(var, envir = env)
      rs<- is_empty(obj)
      if (rs){
        results[[var]] <- rs
      }
    }
    return(results)
  }
}
