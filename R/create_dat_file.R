#' Create .dat file for use in ADMB model
#'
#' `create_dat_file` takes a list of data `dat_list`,
#'
#' @param dat_list list with data that will be written to .dat file. Each element
#'                 must have mode = numeric.
#' @param dat_name list of character vectors. Must be same length as dat_list. Each
#'                 vector will be a comment that corresponds to the dat_list comment
#'                 at the same position. For example, dat_name\[\[1\]\] will be a comment
#'                 that will correspond to dat_list\[\[1\]\].
#' @param title    character vector. Title that will appear at top of .dat file as comment.
#'                 Default is "ADMB .dat file"
#' @param author   character vector. Name of author that will appear at top of .dat file.
#'                 Default is "Author".
#' @param d directory where .dat file will be placed. Default is the working directory.
#'          A directory that does not exist results in an error.
#' @param f file name for .dat file. Default is ADMB.dat. Only can contain letters,
#'          numbers, or _.
#'
#' @details
#'
#' @export



create_dat_file <- function(dat_list, dat_name, title = "ADMB .dat file", author = "Author", d = getwd(), f = "ADMB")
{

  if(class(dat_list) != "list")
    stop("dat_list must be a list")
  if(any(lapply(dat_list, FUN = mode) != "numeric"))
    stop("All elements of dat_list must be numeric")
  if(mode(title) != "character")
    stop("title must be character vector")
  if(mode(author) != "character")
    stop("author must be character vector")
  if(!dir.exists(d))
    stop("d must be a valid directory that already exists")
  if(any(grepl("[[:alnum:]]|_", strsplit(f, split = "")[[1]]) == FALSE))
    stop("The file name (f) must contain only letters, numbers, or _")


  file_name <- paste(d, "/", f, ".dat", sep = "")

  write(paste("#", "Input file for:", title,  sep = " "), file = file_name)
  write(paste("#", "Created by:", author, sep = " "), file = file_name, append = TRUE)
  write(paste("#", "Last modified:", date(), sep = " "), file = file_name, append = TRUE)
  write("", file = file_name, append = TRUE)



  n <- length(dat_list)

  for(i in 1:n)
  {
    write(paste("#", dat_name[[i]], sep = " "), file = file_name, append = TRUE)

    if(any(class(dat_list[[i]]) == "matrix"))
    {
      write.table(dat_list[[i]], file = file_name, append = TRUE, row.names = FALSE,
                  col.names = FALSE)
    } else
      {

        write(dat_list[[i]], file = file_name, append = TRUE, ncolumns = length(dat_list[[i]]))

      }

  }

  return(invisible(NULL))

}
