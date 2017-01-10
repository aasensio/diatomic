# diatomic
Zeeman and Paschen Back Effects in Diatomic Molecules

This is the code associated with "Theory and Modeling of the Zeeman and Paschen-Back Effects in Molecular Lines" Asensio Ramos & Trujillo Bueno (2006), The Astrophysical Journal 636, 548 (2006)

### Compilation
The code is compiled by going to the directory `src` and typing
    
    make compiler=ifort

or 
    
    make compiler=gfortran

depending on the available Fortran compiler.

### Running the code

The code has two input configurations:

`config.dat`

    #
    #
    #
    # File with the information for the transition
    trans_from_idl.dat

    # Magnetic field strength in Gauss
    100.00000

This file indicates the transition to be computed and the magnetic field strength in Gauss.

`transition.dat`

    Wavelength:  5100.0000
    -----------
    Upper level
    -----------
    Lambda:        1.0000000
    S:       0.50000000
    J:        4.5000000
    i:        1.0000000
    par:        1.0000000
    Brot:        6.0900000
    Drot:        0.0000000
    Hrot:        0.0000000
    gamma_spin:        0.0000000
    lambda_ss:        0.0000000
    A:        34.987000
    A_D:        0.0000000
    gL:        1.0000000
    gS:        2.0000000
    gr:        0.0000000

    -----------
    Lower level
    -----------
    Lambda:        0.0000000
    S:       0.50000000
    J:        5.5000000
    i:        1.0000000
    par:        1.0000000
    Brot:        5.8300000
    Drot:        0.0000000
    Hrot:        0.0000000
    gamma_spin:      0.025000000
    lambda_ss:        0.0000000
    A:        0.0000000
    A_D:        0.0000000
    gL:        1.0000000
    gS:        2.0000000
    gr:        0.0000000

This file contains all the quantitative information for the upper and lower level of the transition, including all angular momenta, rotation and coupling constants.

The code is run with `./diatomic` and the output Zeeman pattern is written in file `pattern.dat`

### Graphical interface

There is a small graphical application to run the code. It is written in IDL and it is just used to read the input parameters of a transition (given by the `*.transition` files) and calling the code. It also plots the Zeeman pattern. It can be used to have an intuition on how to read the output.
