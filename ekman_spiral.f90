!##############################################################################################
!#\  ekman_spiral.f90
!#\ A module which contains class spiral and different procedures that are used to:  
!#\ initialize data, load input data, solve the problem and dump output data
!##############################################################################################
MODULE ekman_spiral
  !#\ A statement declaring explicit type definition of each data item used in this program
  IMPLICIT NONE
  !#\ Prevent direct access to module components unless they are specifically declared public 
  PRIVATE
  !#\ define class spiral
  TYPE, PUBLIC                       :: spiral
    !#\ a variable to hold name of the input file
    CHARACTER(LEN=80)                :: input_filename
    !#\ a variable to hold name of the output file
    CHARACTER(LEN=80)                :: output_filename
    !#\ a variable to hold x direction component of the complex variable wind velocity (w)
    REAL       :: x_wind  !REAL (KIND=8),INTENT(IN) is returning an error messege hence 
                          !such specifications are ommited in all variable declarations
    !#\ a variable to hold y direction component of the complex variable wind velocity (w)
    REAL       :: y_wind  
    !#\ a variable to hold the maximum water depth of analysis
    REAL       :: H  
    !#\ a variable to hold the water viscosity constant
    REAL       :: k   
    !#\ a variable to hold the Coriolis constant (due to the Earth's rotation)
    REAL       :: f    
     !#\ a variable representing length of the output vectors or the number of points for analysis
    INTEGER    :: N
    !#\ a pointer variable to store the 'a' diagonal values of matrix 'A' and initialised to null value
    COMPLEX, POINTER        :: a(:)=>null()
    !#\ a pointer variable to store the 'b' diagonal values of matrix 'A' initialised to null value
    COMPLEX, POINTER        :: b(:)=>null()
    !#\ a pointer variable to store the 'c' diagonal values of  matrix 'A' initialised to null value
    COMPLEX, POINTER        :: c(:)=>null()
    !#\ a pointer variable to store the right hand side vector (D) 
    !#\ values in the linear equation AW=D and initialised to null value
    COMPLEX, POINTER        :: d(:)=>null()
    !#\ a pointer variable to store the modified 'c' diagonal (c') 
    !#\ values of the  matrix 'A' and initialised to null value
    COMPLEX, POINTER        :: c_note(:)=>null()
    !#\ a pointer variable to store the modified right hand side vector (d') 
    !#\ values related to the linear equation AW=D. It is initialised to null value
    COMPLEX, POINTER        :: d_note(:)=>null()
    !#\ a pointer variable to store wind velocity values at different depths of the water body
    COMPLEX, POINTER        :: w_velocity(:)=>null()
    !#\ a pointer variable to store the x direction component of wind velocity values 
    REAL, POINTER           :: x_w_velocity(:)=>null()
    !#\ a pointer variable to store the y direction component of wind velocity values
    REAL, POINTER           :: y_w_velocity(:)=>null()
    !#\ a pointer variable to store water depth values at different points of analysis
    REAL, POINTER           :: water_depth(:)=>null()
   
     CONTAINS !#\ Class spiral contains the following procedures
        !#\ a procedure to initialise input data sets  
        PROCEDURE         :: init => init_instance
        !#\ a procedure to read values from an input file and load for further use
        PROCEDURE         :: load => load_data
        !#\ a procedure to determine values of the W complex at different
        !#\ water depth (z) values bassed on the ekman spiral concept
        PROCEDURE         :: solve => solve_problem
        !#\ a procedure to write contents of the W complex and the z vectors to an output file
        PROCEDURE         :: dump => dump_data
  !#\ End of Class spiral      
  END TYPE spiral   
   
   CONTAINS !#\ ekman_spiral module contains the following subroutines that accomplish different tasks
      !#\ a subroutine to initialise input data sets
      SUBROUTINE init_instance(this)
        !#\ declare an instance variable of class spiral
        CLASS(spiral)  :: this
        !#\ initialise the variables which are previously declared at class spiral
         this%input_filename  = ''
         this%output_filename = ''
         this%x_wind          = 0.0D0
         this%y_wind          = 0.0D0
         this%H               = 0.0D0
         this%K               = 0.0D0
         this%f               = 0.0D0
         this%N               = 0        
      !#\ end of the init_instance subroutine   
      END SUBROUTINE init_instance
      
      !#\ a subroutine for reading data from a namelist input file and 
      !#\ loading to their respective data objects for analysis by the solve_problem routine
      SUBROUTINE load_data(this)
        !#\ A statement declaring explicit type definition of local variables used in this subroutine
        IMPLICIT NONE
        !#\ declare an instance variable of class spiral
        CLASS(spiral)                    :: this
        !#\ the input file pointer (unit) 
        INTEGER, PARAMETER               :: file_unit = 10
        !#\ a variable to hold the return status in an attempt to open the namelist file
        INTEGER                          :: res
        !#\ variables of real type to hold input data as read from the namelist file
        REAL                             :: nml_x_wind, nml_y_wind, nml_H, nml_K, nml_f
        !#\ an integer variable to hold length of the output vectors
        INTEGER                          :: nml_N
        !#\ list of variables corresponding to the values in the namelist file
        NAMELIST /input_values/ nml_x_wind, nml_y_wind, nml_H, nml_K, nml_f, nml_N
        !#\ Open the namelist file   
        OPEN(UNIT=file_unit,FILE=this%input_filename,STATUS='OLD',IOSTAT=res)
        !#\ if the return status in an attempt to open the file is other 
        !#\ than zero then print an error message and stop further tasks        
        IF(res /= 0) THEN
           PRINT *, 'Error in opening the input file, status: ', res
           RETURN
        END IF 
        !#\ read the input values from the namelist file
        READ(UNIT=file_unit,NML=input_values,IOSTAT=res)
        !#\ close the namelist file with the specified file pointer number
        CLOSE(UNIT=file_unit)
        !#\ assign values read from the namelist file to the corresponding 
        !#\ public variables declared at class spiral
        this%x_wind = nml_x_wind
        this%y_wind = nml_y_wind
        this%H      = nml_H
        this%K      = nml_K
        this%f      = nml_f
        this%N      = nml_N
      !#\ end of the load_data subroutine  
      END SUBROUTINE load_data
      
      !#\ a subroutine to determine values of the W complex at different
      !#\ z values bassed on the ekman spiral concept. For underlying concept 
      !#\ refer to the readme file
      SUBROUTINE solve_problem(this)
        !#\ a statement declaring explicit type definition of local variables used in this subroutine
        IMPLICIT NONE
        !#\ declare an instance variable of class spiral
        CLASS(spiral)  :: this
        !#\ variable to store the calculated change in depth value
        REAL           :: delta_z 
        !#\ variable to store the calculated sigma value
        REAL           :: sigma
        !#\ variables used in the Do statemet as index counters
        INTEGER        :: pointer_index,counter  
        !#\ variables to hold the return status when allocating memory space to different pointers
        INTEGER        :: res_a,res_b,res_c,res_d,res_c_note,res_d_note 
        INTEGER        :: res_velocity,res_x_velocity,res_y_velocity,res_water_depth
        
        !#\ calculate the change in depth from the total depth and number of 
        !#\ intervals between sucessive points considered for analysis
        delta_z=this%H/FLOAT((this%N-1))    
        !#\ calculate sigma that represents the imaginary compponent of 
        !#\ the complex diagonal variable 'b' in martix 'A'
        sigma = ((delta_z**2)*this%f)/this%K       
        !#\ Set memory space using the ALLOCATE statement for pointers declared in class spiral.
        !#\ If any allocation failure occurs catch and return an error message
        ALLOCATE(this%a(this%N),STAT=res_a)
        IF(res_a /= 0) THEN
          PRINT *, 'Allocation failure at pointer-a, status: ', res_a
        END IF
        
        ALLOCATE(this%b(this%N),STAT=res_b)
        IF(res_b /= 0) THEN
          PRINT *, 'Allocation failure at pointer-b, status: ', res_b
        END IF
        
        ALLOCATE(this%c(this%N),STAT=res_c) 
        IF(res_c /= 0) THEN
          PRINT *, 'Allocation failure at pointer-c, status: ', res_c
        END IF
        
        ALLOCATE(this%d(this%N),STAT=res_d) 
        IF(res_d/= 0) THEN
          PRINT *, 'Allocation failure at pointer-d, status: ', res_d
        END IF
        
        ALLOCATE(this%c_note(this%N),STAT=res_c_note) 
        IF(res_c_note /= 0) THEN
          PRINT *, 'Allocation failure at pointer-c_note, status: ', res_c_note
        END IF
        
        ALLOCATE(this%d_note(this%N),STAT=res_d_note) 
        IF(res_d_note /= 0) THEN
          PRINT *, 'Allocation failure at pointer-d_note, status: ', res_d_note
        END IF
        
        ALLOCATE(this%w_velocity(this%N),STAT=res_velocity) 
        IF(res_velocity/= 0) THEN
          PRINT *, 'Allocation failure at pointer-w_velocity, status: ', res_velocity
        END IF
        
        ALLOCATE(this%x_w_velocity(this%N),STAT=res_x_velocity) 
        IF(res_x_velocity/= 0) THEN
          PRINT *, 'Allocation failure at pointer-x_w_velocity, status: ', res_x_velocity
        END IF
        
        ALLOCATE(this%y_w_velocity(this%N),STAT=res_y_velocity) 
        IF(res_y_velocity/= 0) THEN
          PRINT *, 'Allocation failure at pointer-y_w_velocity, status: ', res_y_velocity
        END IF
        ALLOCATE(this%water_depth(this%N),STAT=res_water_depth) 
        IF(res_water_depth/= 0) THEN
          PRINT *, 'Allocation failure at pointer-water_depth, status: ', res_water_depth
        END IF        
        
        !#\ set values for the vector diagonals a, b and c of Matrix 'A' and for the 'D' vector
        !#\ starting from the first index upto the last index
        DO pointer_index=1,this%N
          !#\ if the current index is one of the N-2 inner points
          IF((pointer_index >= 2).AND.(pointer_index < this%N))THEN
              this%a(pointer_index) = -1.0
              this%b(pointer_index) = CMPLX(2.0,sigma)
              this%c(pointer_index) = -1.0
              this%d(pointer_index) = 0.0
           !#\ if the current index represents the first index 
           ELSEIF (pointer_index == 1)THEN 
              this%b(pointer_index) = 1.0 
              this%c(pointer_index) = 0.0 
              this%d(pointer_index) = 0.0
          !#\ if the current index represents the last index
          ELSE   
              this%a(pointer_index) = 0.0 
              this%b(pointer_index) = 1.0 
              this%d(pointer_index) = CMPLX(this%x_wind,this%y_wind)
          !#\ end of the If statement block
          ENDIF
        !#\ end of the DO statement block
        END DO
        
        !#\ Modify the c-diagonal and values of vector 'D' (determine c' and d') 
        !#\ using appropriate expressions       
        DO pointer_index=1,this%N
          !#\ if the current index is the first one
          IF(pointer_index == 1)THEN 
            this%c_note(pointer_index) = (this%c(pointer_index))/(this%b(pointer_index))
            this%d_note(pointer_index) = (this%d(pointer_index))/(this%b(pointer_index))
          !#\ if the current index refers to indices other than the first point(for pointer_index = 2,3,...,N)
          ELSE 
            this%d_note(pointer_index) = ((this%d(pointer_index))-((this%a(pointer_index))*(this%d_note(pointer_index-1))))/&
                                         ((this%b(pointer_index))-((this%a(pointer_index))*(this%c_note(pointer_index-1))))
            this%c_note(pointer_index) = (this%c(pointer_index))/&
                                         ((this%b(pointer_index))-((this%a(pointer_index))*(this%c_note(pointer_index-1))))
          !#\ end of the If statement block
          ENDIF
        !#\ end of the DO statement block
        END DO
       
        !#\ Determine values of the wind velocity (W) vector in a back-ward order
        DO counter=1,this%N
          !#\ assign the last index of W with a value from the last index of d_note
          IF(counter == 1)THEN 
             this%w_velocity(this%N) = this%d_note(this%N)
          !#\ for pointer index = N-1,N-2,...,1 determine value of W using the following relationship
          ELSE
             this%w_velocity(this%N+1-counter)= (this%d_note(this%N+1-counter))-((this%w_velocity(this%N+2-counter))&
                                                 * (this%c_note(this%N+1-counter)))
          !#\ end of the If statement block
          ENDIF
        !#\ end of the DO statement block  
        END DO 
       
        !#\ get the x-direction (real) and y-direction (imaginary) components of the complex W vector
        DO pointer_index=1,this%N
           this%x_w_velocity(pointer_index)= REAL(this%w_velocity(pointer_index))
           this%y_w_velocity(pointer_index)= AIMAG(this%w_velocity(pointer_index))      
        !#\ end of the DO statement block
        END DO
   
        !#\ determine depth below the water surface (z) at the N points
        DO pointer_index=1,this%N
          !#\ if the current index is the first one assign negative value of the user inputed H value     
          IF(pointer_index == 1)THEN
           this%water_depth(pointer_index)= -this%H
          !#\ determine z value using the following relationship for the other indices 
          ELSE
           this%water_depth(pointer_index)= this%water_depth(pointer_index-1)+ delta_z
          !#\ end of the If statement block
          ENDIF
        !#\ end of the DO statement block
        END DO    
        !#\ end of the solve_problem subroutine 
      END SUBROUTINE solve_problem
      
      !#\ a subroutine dump_data with an arguement variable 'this'
      !#\ this subroutine is used to write values of output variables which are computed
      !#\ by the solve_problem subroutine of the ekman_spiral module
      SUBROUTINE dump_data(this)
       !#\ A statement declaring explicit type definition of local variables used in this subroutine
       IMPLICIT NONE
       !#\ declare an instance variable of class spiral
       CLASS(spiral)         :: this
       !#\ the output file pointer (unit)
       INTEGER, PARAMETER    :: outputfile_unit=20
       !#\ a variable to hold the return status in an attempt to open the output file
       INTEGER               :: FileStatus
       !#\ a variable used in the Do statemet for counting index
       INTEGER               :: output_index_counter
       
       !#\ open the output file
       OPEN(UNIT=outputfile_unit,FILE=this%output_filename,FORM ='FORMATTED',IOSTAT=FileStatus) 
       !#\ if the return status in an attempt to open the output file is other 
       !#\ than zero then print an error message and stop further tasks
       IF(FileStatus /= 0) THEN
           PRINT *, 'Error in opening the output file, status: ', FileStatus
           RETURN
       END IF
       !#\ write a brief title to the output file with over and underlying margins 
       WRITE(UNIT=outputfile_unit,FMT='(A)',IOSTAT=FileStatus)&
            '****************************************************************'
       WRITE(UNIT=outputfile_unit,FMT='(A)',IOSTAT=FileStatus)&
            'AN OUTPUT FILE GENERATED FROM THE EKMAN SPIRAL FORTRAN PROGRAM'     
       WRITE(UNIT=outputfile_unit,FMT='(A)',IOSTAT=FileStatus)&
            '****************************************************************'     
       
       !#\ first write value of N
       WRITE(UNIT=outputfile_unit,FMT='(I4)',IOSTAT=FileStatus)this%N  
       !#\ start writing the water depth and wind velocity vector values
       DO output_index_counter = 1, this%N
       
       !for test
       PRINT *, this%water_depth(output_index_counter), this%x_w_velocity(output_index_counter),&
           this%y_w_velocity(output_index_counter)
           
       !#\ write values of the water depth vector as well as x and y components of the W vector 
       !#\ with a number format and spacing as specified under the FMT parameter of the following statement  
         WRITE(UNIT=outputfile_unit,FMT='(F14.10,A,5X,F14.10,A,5X,F14.10,5X)',ADVANCE='YES',IOSTAT=FileStatus)&
           this%water_depth(output_index_counter),',', this%x_w_velocity(output_index_counter),',',&
           this%y_w_velocity(output_index_counter)       
       !#\ end of the DO statement block 
       END DO
       !#\ close the output file with the specified file pointer(UNIT) number 
       CLOSE(UNIT=outputfile_unit)
      !#\ end of the dump_data subroutine 
      END SUBROUTINE dump_data
!#\ end of the ekman_spiral module      
END MODULE ekman_spiral

      
   
   