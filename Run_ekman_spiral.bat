REM Batch file to compile and run the ekman-spiral fortran program

REM Compile the ekman_spiral module
nagfor -c ekman_spiral.f90

REM Compile the main program
nagfor -o main main.f90 ekman_spiral.o

REM Clear screen
cls

REM execute the main program
main

pause

REM delete compiled object, executable and binary files 	

del main.exe	
del ekman_spiral.o
del ekman_spiral.mod
	

