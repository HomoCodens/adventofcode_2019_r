day11 <- function(path) {
 tape <- readICCTape(path)

 ehpr <- function(initialCell = 0, output = NULL) {

   pos <- c(0, 0)
   dir <- c(0, 1)

   cells <- data.table(x = 0, y = 0, color = initialCell)
   state <- 0 # 0 - Awaiting color, 1 - Awaiting rotation

   function(input = NULL) {

     if(!is.null(input)) {
       # Being used as output (data in)

       if(!is.null(output)) {
         output(input)
       }

       if(state == 0) {
         # Paint a cell
         cells <<- cells[x == pos[1] & y == pos[2], color := input]

         state <<- 1

         # message(sprintf("Painting (%d %d) %d", pos[1], pos[2], input))
       } else {
         if(input == 0) {
           # Turn left
           # message("Turning left")
           dir <<- c(-dir[2], dir[1])
         } else {
           # Turn right
           # message("Turning right")
           dir <<- c(dir[2], -dir[1])
         }

         # Move
         pos <<- pos + dir

         # message(sprintf("Moving (%d, %d) to (%d, %d)", dir[1], dir[2], pos[1], pos[2]))


         if(cells[x == pos[1] & y == pos[2], .N] == 0) {
           cells <<- rbind(cells, data.table(x = pos[1], y = pos[2], color = 0))
         }

         state <<- 0
       }
     } else {
       # Being used as input (data out)
       # message(sprintf("Sending color %d at (%d %d)", cells[x == pos[1] & y == pos[2], color], pos[1], pos[2]))
       cells[x == pos[1] & y == pos[2], color]
     }
   }
 }

 ehprProg <- ehpr()

 ehprOut <- function(tape) {
   ells <- as.list(environment(ehprProg))$cells
   message(ells[, .N])
 }

 invisible(runIntCodeComputer(list(
     list(
         tape = tape,
         iccin = ehprProg,
         iccout = ehprProg,
         done = ehprOut))))

 ehprProg <- ehpr(1)

 ehprOut2 <- function(tape) {
     cells <- as.list(environment(ehprProg))$cells

     xr <- cells[, range(x)]
     xoff <- xr[1]

     yr <- cells[, range(y)]
     yoff <- yr[1]

     pxls <- matrix(" ", yr[2] - yoff + 1, xr[2] - xoff + 1)

     for(i in seq(cells[, .N])) {
         pxls[cells[i, y - yoff + 1], cells[i, x - xoff + 1]] <- cells[i, ifelse(color == 0, " ", "*")]
     }

     cat(paste(rev(apply(pxls, 1, paste, collapse = "")), collapse = "\n"))
 }

 invisible(runIntCodeComputer(list(
     list(
         tape = tape,
         iccin = ehprProg,
         iccout = ehprProg,
         done = ehprOut2))))

}
