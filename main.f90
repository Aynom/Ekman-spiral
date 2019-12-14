!##############################################################################################
!#\  main.f90
!#\ A main program that prompts the user to feed input and output file names. It also calls the  
!#\ init_instance, load_data, solve_problem and dump_data procedures of the ekman_spiral Module.
!##############################################################################################
PROGRAM main
  !#\ When required, use the public objects of the ekman_spiral module
  USE ekman_spiral
  !#\ A statement declaring explicit type definition of each data item used in this program
  IMPLICIT NONE
  !#\ Declare an instance variable of spiral type
  TYPE(spiral)                   :: Sp
  !#\ String variable to hold input file name  
  CHARACTER(LEN=80)              :: in_filename
  !#\ String variable to hold output file name  
  CHARACTER(LEN=80)              :: out_filename  
  !#\ Call the init procedure of class spiral to initialise the data sets
  CALL Sp%init
  !#\ Statement prompting the user to type input file name
  PRINT *,'Enter the namelist file name containing input data to the program'
  PRINT *,'NB. file names should be without space (as a single word)'
  !#\ Read and assign the string value to the in_filename string variable
  READ *, in_filename
  !#\ Statement prompting the user to type output file name
  PRINT *,'Enter the file name to which output data from the program is written'
  !#\ Read and assign the string value to the out_filename string variable
  READ *, out_filename
  !#\ Assign the input file name value to the input_filename variable in class spiral
  Sp%input_filename = in_filename
  !#\ Assign the output file name value to the output_filename variable in class spiral
  Sp%output_filename = out_filename
  !#\ Call the load procedure of class spiral to read values from an input file and load for further use
  CALL Sp%load
  !#\ Call the solve procedure of class spiral to determine values of the W complex at different
  !#\ water depth (z) values based on the ekman spiral concept
  CALL Sp%solve
  !#\ Call the dump procedure of class spiral to write contents of the W complex 
  !#\ and the z vectors to an output file  
  CALL Sp%dump
  !#\ Print a simulation complete message to the user
  PRINT *,'Simulation completed successfully. Open: ',out_filename
  PRINT *,'file from the working directory to view simulation results' 
END PROGRAM main