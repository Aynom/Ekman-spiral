# Ekman-spiral
A Fortran-based solution to the Ekman Spiral phenomenon

I. A brief Overview of the ekman spiral concept

The phenomenon related to change in flow direction of an ocean current along a water depth due to the earth's rotation
is referred to as the Ekman spiral. The Ekman spiral phenomenon can be mathematically expressed using a second-order 
differential equation of complex values. This mathematical expression attempts to find how the flow direction and 
magnitude, W(z), change between the water depth z=-H and the surface z=0. As boundary conditions it is assumed that 
no motion exists at z=-H and that the velocity at water surface is known as W = v + ui, where u and v are two 
prescribed real constants.

II. Numerical solution to the problem using Fortran Programming

In this project, the equation describing the Ekman spiral phenomenon is approximated with the help of the finite 
difference method. Furthermore, a new variable (sigma) which is a function of z, f and k parameters coupled with the 
boundary conditions are used to formulate the approximation as a systm of linear equations.
 
The following general steps are followed in writting the Fortran program to solve the problem using a numerical method.

i.  calculate the change in depth using the total depth and number of intervals between sucessive points considered 
    for analysis
ii. determine depth below the water surface (z) at the N sucessive points

iii.calculate sigma that represents the imaginary compponent of the complex diagonal variable 'b' in martix 'A'

iv. set values for the vector diagonals a, b and c of Matrix 'A' and for the 'D' vector starting from the first 
    through the last index

v.  Modify the c-diagonal and values of vector 'D' (determine c' and d') using appropriate expressions

vi. Determine values of the wind velocity (W) vector in a back-ward order

vii.Determine the x-direction (real) and y-direction (imaginary) components of the complex W vector


III. Program files and their tasks

i.  main.f90 :

This is a main fortran program in the project and it is used to organize the overall activity of the program.
This program prompts the user to feed input and output file names. It controls the different activities of the 
project by calling the init_instance, load_data, solve_problem and dump_data procedures of the ekman_spiral Module
at their appropriate order of precedence.

ii. ekman_spiral.f90 :

This module contains class spiral and different procedures that are used to initialize data, load data from 
input file, solve the problem and dump data to an output file.

IV. Input to and output from the program

In order to solve the Ekman spiral phenomenon input parameter values, viz., x_wind, y_wind, H, K, f and N are 
required. The program prompts the user to feed a namelist file name containing the aformentioned parameters and 
their values. A default namelist input file named "ekman_input" is provided with the program. Thus users can either
modify parameter values provided in this file or prepare their own namelist file.

If simulation is successful, outputs from the program, viz., the number of points considered for analysis(N),
the water depth (z) vector column followed by two columns for the x and y direction components of flow velocity
are written to an output file. The user is prompted to feed name of this output file when starting the program.

V. Running the program

The batch file "Run_ekman_spiral.bat" automates the compilation and running of the program. 
A double click to this batch file starts the program. 
This project assumes the default fortran compiler in a given machine is NAG compiler.
