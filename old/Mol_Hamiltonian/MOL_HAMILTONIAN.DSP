# Microsoft Developer Studio Project File - Name="MOL_HAMILTONIAN" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=MOL_HAMILTONIAN - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "MOL_HAMILTONIAN.MAK".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MOL_HAMILTONIAN.MAK" CFG="MOL_HAMILTONIAN - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MOL_HAMILTONIAN - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "MOL_HAMILTONIAN - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "MOL_HAMILTONIAN - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD BASE RSC /l 0xc0a /d "NDEBUG"
# ADD RSC /l 0xc0a /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /incremental:yes /machine:I386 /out:"MOL_HAMILTONIAN.exe"

!ELSEIF  "$(CFG)" == "MOL_HAMILTONIAN - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
# ADD F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
# ADD BASE RSC /l 0xc0a /d "_DEBUG"
# ADD RSC /l 0xc0a /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /out:"DMOL_HAMILTONIAN.exe" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "MOL_HAMILTONIAN - Win32 Release"
# Name "MOL_HAMILTONIAN - Win32 Debug"
# Begin Source File

SOURCE=.\Fuentes\build.f90
DEP_F90_BUILD=\
	".\Release\hamiltonian_types.mod"\
	".\Release\maths.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Fuentes\hamiltonian.f90
DEP_F90_HAMIL=\
	".\Release\build_hamiltonian_routines.mod"\
	".\Release\hamiltonian_types.mod"\
	".\Release\i_o_routines.mod"\
	".\Release\maths.mod"\
	".\Release\strength.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Fuentes\input_output.f90
DEP_F90_INPUT=\
	".\Release\hamiltonian_types.mod"\
	".\Release\maths.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Fuentes\maths.f90
DEP_F90_MATHS=\
	".\Release\hamiltonian_types.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Fuentes\strength.f90
DEP_F90_STREN=\
	".\Release\build_hamiltonian_routines.mod"\
	".\Release\hamiltonian_types.mod"\
	".\Release\maths.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Fuentes\types.f90
# End Source File
# End Target
# End Project
