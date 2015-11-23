# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=agdrift32 - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to agdrift32 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "agdrift32 - Win32 Release" && "$(CFG)" !=\
 "agdrift32 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "agdrift32.mak" CFG="agdrift32 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "agdrift32 - Win32 Release" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "agdrift32 - Win32 Debug" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "agdrift32 - Win32 Debug"
F90=fl32.exe
RSC=rc.exe
MTL=mktyplib.exe

!IF  "$(CFG)" == "agdrift32 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "agdrift3"
# PROP BASE Intermediate_Dir "agdrift3"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\..
INTDIR=.\Release

ALL : "$(OUTDIR)\agdrift32.dll"

CLEAN : 
	-@erase "..\agdrift32.dll"
	-@erase ".\Release\Aginit.obj"
	-@erase ".\Release\Agdrin.obj"
	-@erase ".\Release\Agread.obj"
	-@erase ".\Release\Aglims.obj"
	-@erase ".\Release\Agnns.obj"
	-@erase ".\Release\Agvel.obj"
	-@erase ".\Release\Agkick.obj"
	-@erase ".\Release\Agovl.obj"
	-@erase ".\Release\Agfill.obj"
	-@erase ".\Release\Ageqn.obj"
	-@erase ".\Release\Agsmpl.obj"
	-@erase ".\Release\Agnums.obj"
	-@erase ".\Release\Agunf.obj"
	-@erase ".\Release\Agcan.obj"
	-@erase ".\Release\Agtox.obj"
	-@erase ".\Release\Agextd.obj"
	-@erase ".\Release\Agcon.obj"
	-@erase ".\Release\Agave.obj"
	-@erase ".\Release\Agn2d.obj"
	-@erase ".\Release\Agupds.obj"
	-@erase ".\Release\Agsblk.obj"
	-@erase ".\Release\Agsav.obj"
	-@erase ".\Release\Agvrf.obj"
	-@erase ".\Release\Agkrn.obj"
	-@erase ".\Release\Agterr.obj"
	-@erase ".\Release\Agdep.obj"
	-@erase ".\Release\Agwtb.obj"
	-@erase ".\Release\Agn4s.obj"
	-@erase ".\Release\Agnnd.obj"
	-@erase ".\Release\Aglibr.obj"
	-@erase ".\Release\Agkrr.obj"
	-@erase ".\Release\Agsbin.obj"
	-@erase ".\Release\Agends.obj"
	-@erase ".\Release\Agcov.obj"
	-@erase ".\Release\Agsetl.obj"
	-@erase ".\Release\Aggrnd.obj"
	-@erase ".\Release\Agtraj.obj"
	-@erase ".\Release\Agnozl.obj"
	-@erase ".\Release\Agstrm.obj"
	-@erase ".\Release\Agspln.obj"
	-@erase ".\Release\Agsmex.obj"
	-@erase ".\Release\Agbkg.obj"
	-@erase ".\Release\Agorch.obj"
	-@erase ".\Release\Agaver.obj"
	-@erase ".\Release\Agn3s.obj"
	-@erase ".\Release\Agint.obj"
	-@erase ".\Release\Agsmti.obj"
	-@erase ".\Release\Agdrp.obj"
	-@erase ".\Release\Agn4d.obj"
	-@erase ".\Release\Agparm.obj"
	-@erase ".\Release\Agtrgo.obj"
	-@erase ".\Release\Agrtrn.obj"
	-@erase ".\Release\Agsbck.obj"
	-@erase ".\Release\Agmore.obj"
	-@erase ".\Release\Agdsrn.obj"
	-@erase ".\Release\Agchk.obj"
	-@erase ".\Release\Agsgrd.obj"
	-@erase ".\Release\Agkirk.obj"
	-@erase ".\Release\Agwplt.obj"
	-@erase ".\Release\Agdrop.obj"
	-@erase ".\Release\Agarea.obj"
	-@erase ".\Release\Agn2s.obj"
	-@erase ".\Release\Agdsr.obj"
	-@erase ".\Release\Agn3d.obj"
	-@erase ".\Release\Agsome.obj"
	-@erase ".\Release\Agsend.obj"
	-@erase ".\Release\Agkln.obj"
	-@erase ".\Release\Agsmck.obj"
	-@erase ".\Release\Agdsd.obj"
	-@erase ".\Release\Agwdrs.obj"
	-@erase ".\Release\Agdrot.obj"
	-@erase "..\agdrift32.lib"
	-@erase "..\agdrift32.exp"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE F90 /Ox /I "agdrift3/" /c /nologo /MT
# ADD F90 /Ox /I "Release/" /c /nologo /MT
F90_PROJ=/Ox /I "Release/" /c /nologo /MT /Fo"Release/" 
F90_OBJS=.\Release/
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/agdrift32.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:windows /dll /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)/agdrift32.pdb" /machine:I386 /out:"$(OUTDIR)/agdrift32.dll"\
 /implib:"$(OUTDIR)/agdrift32.lib" 
LINK32_OBJS= \
	"$(INTDIR)/Aginit.obj" \
	"$(INTDIR)/Agdrin.obj" \
	"$(INTDIR)/Agread.obj" \
	"$(INTDIR)/Aglims.obj" \
	"$(INTDIR)/Agnns.obj" \
	"$(INTDIR)/Agvel.obj" \
	"$(INTDIR)/Agkick.obj" \
	"$(INTDIR)/Agovl.obj" \
	"$(INTDIR)/Agfill.obj" \
	"$(INTDIR)/Ageqn.obj" \
	"$(INTDIR)/Agsmpl.obj" \
	"$(INTDIR)/Agnums.obj" \
	"$(INTDIR)/Agunf.obj" \
	"$(INTDIR)/Agcan.obj" \
	"$(INTDIR)/Agtox.obj" \
	"$(INTDIR)/Agextd.obj" \
	"$(INTDIR)/Agcon.obj" \
	"$(INTDIR)/Agave.obj" \
	"$(INTDIR)/Agn2d.obj" \
	"$(INTDIR)/Agupds.obj" \
	"$(INTDIR)/Agsblk.obj" \
	"$(INTDIR)/Agsav.obj" \
	"$(INTDIR)/Agvrf.obj" \
	"$(INTDIR)/Agkrn.obj" \
	"$(INTDIR)/Agterr.obj" \
	"$(INTDIR)/Agdep.obj" \
	"$(INTDIR)/Agwtb.obj" \
	"$(INTDIR)/Agn4s.obj" \
	"$(INTDIR)/Agnnd.obj" \
	"$(INTDIR)/Aglibr.obj" \
	"$(INTDIR)/Agkrr.obj" \
	"$(INTDIR)/Agsbin.obj" \
	"$(INTDIR)/Agends.obj" \
	"$(INTDIR)/Agcov.obj" \
	"$(INTDIR)/Agsetl.obj" \
	"$(INTDIR)/Aggrnd.obj" \
	"$(INTDIR)/Agtraj.obj" \
	"$(INTDIR)/Agnozl.obj" \
	"$(INTDIR)/Agstrm.obj" \
	"$(INTDIR)/Agspln.obj" \
	"$(INTDIR)/Agsmex.obj" \
	"$(INTDIR)/Agbkg.obj" \
	"$(INTDIR)/Agorch.obj" \
	"$(INTDIR)/Agaver.obj" \
	"$(INTDIR)/Agn3s.obj" \
	"$(INTDIR)/Agint.obj" \
	"$(INTDIR)/Agsmti.obj" \
	"$(INTDIR)/Agdrp.obj" \
	"$(INTDIR)/Agn4d.obj" \
	"$(INTDIR)/Agparm.obj" \
	"$(INTDIR)/Agtrgo.obj" \
	"$(INTDIR)/Agrtrn.obj" \
	"$(INTDIR)/Agsbck.obj" \
	"$(INTDIR)/Agmore.obj" \
	"$(INTDIR)/Agdsrn.obj" \
	"$(INTDIR)/Agchk.obj" \
	"$(INTDIR)/Agsgrd.obj" \
	"$(INTDIR)/Agkirk.obj" \
	"$(INTDIR)/Agwplt.obj" \
	"$(INTDIR)/Agdrop.obj" \
	"$(INTDIR)/Agarea.obj" \
	"$(INTDIR)/Agn2s.obj" \
	"$(INTDIR)/Agdsr.obj" \
	"$(INTDIR)/Agn3d.obj" \
	"$(INTDIR)/Agsome.obj" \
	"$(INTDIR)/Agsend.obj" \
	"$(INTDIR)/Agkln.obj" \
	"$(INTDIR)/Agsmck.obj" \
	"$(INTDIR)/Agdsd.obj" \
	"$(INTDIR)/Agwdrs.obj" \
	"$(INTDIR)/Agdrot.obj"

"$(OUTDIR)\agdrift32.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "agdrift32 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "agdrift0"
# PROP BASE Intermediate_Dir "agdrift0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\..
INTDIR=.\Debug

ALL : "$(OUTDIR)\agdrift32.dll"

CLEAN : 
	-@erase "..\agdrift32.dll"
	-@erase ".\Debug\Agsgrd.obj"
	-@erase ".\Debug\Agkirk.obj"
	-@erase ".\Debug\Agwplt.obj"
	-@erase ".\Debug\Agbkg.obj"
	-@erase ".\Debug\Agdrop.obj"
	-@erase ".\Debug\Agarea.obj"
	-@erase ".\Debug\Agn3s.obj"
	-@erase ".\Debug\Agint.obj"
	-@erase ".\Debug\Agsome.obj"
	-@erase ".\Debug\Agsend.obj"
	-@erase ".\Debug\Agdrp.obj"
	-@erase ".\Debug\Agn4d.obj"
	-@erase ".\Debug\Agsmck.obj"
	-@erase ".\Debug\Agwdrs.obj"
	-@erase ".\Debug\Agdrot.obj"
	-@erase ".\Debug\Aginit.obj"
	-@erase ".\Debug\Agdrin.obj"
	-@erase ".\Debug\Agread.obj"
	-@erase ".\Debug\Aglims.obj"
	-@erase ".\Debug\Agkick.obj"
	-@erase ".\Debug\Agfill.obj"
	-@erase ".\Debug\Agsmpl.obj"
	-@erase ".\Debug\Agnums.obj"
	-@erase ".\Debug\Agchk.obj"
	-@erase ".\Debug\Agextd.obj"
	-@erase ".\Debug\Agn2s.obj"
	-@erase ".\Debug\Agupds.obj"
	-@erase ".\Debug\Agsblk.obj"
	-@erase ".\Debug\Agdsr.obj"
	-@erase ".\Debug\Agn3d.obj"
	-@erase ".\Debug\Agkln.obj"
	-@erase ".\Debug\Agterr.obj"
	-@erase ".\Debug\Agdsd.obj"
	-@erase ".\Debug\Aglibr.obj"
	-@erase ".\Debug\Agnns.obj"
	-@erase ".\Debug\Agvel.obj"
	-@erase ".\Debug\Agovl.obj"
	-@erase ".\Debug\Ageqn.obj"
	-@erase ".\Debug\Agsbin.obj"
	-@erase ".\Debug\Agends.obj"
	-@erase ".\Debug\Agunf.obj"
	-@erase ".\Debug\Agcan.obj"
	-@erase ".\Debug\Agtox.obj"
	-@erase ".\Debug\Agsetl.obj"
	-@erase ".\Debug\Aggrnd.obj"
	-@erase ".\Debug\Agtraj.obj"
	-@erase ".\Debug\Agcon.obj"
	-@erase ".\Debug\Agave.obj"
	-@erase ".\Debug\Agnozl.obj"
	-@erase ".\Debug\Agstrm.obj"
	-@erase ".\Debug\Agspln.obj"
	-@erase ".\Debug\Agn2d.obj"
	-@erase ".\Debug\Agsav.obj"
	-@erase ".\Debug\Agsmex.obj"
	-@erase ".\Debug\Agvrf.obj"
	-@erase ".\Debug\Agkrn.obj"
	-@erase ".\Debug\Agorch.obj"
	-@erase ".\Debug\Agdep.obj"
	-@erase ".\Debug\Agaver.obj"
	-@erase ".\Debug\Agwtb.obj"
	-@erase ".\Debug\Agsmti.obj"
	-@erase ".\Debug\Agn4s.obj"
	-@erase ".\Debug\Agnnd.obj"
	-@erase ".\Debug\Agparm.obj"
	-@erase ".\Debug\Agkrr.obj"
	-@erase ".\Debug\Agtrgo.obj"
	-@erase ".\Debug\Agrtrn.obj"
	-@erase ".\Debug\Agcov.obj"
	-@erase ".\Debug\Agsbck.obj"
	-@erase ".\Debug\Agmore.obj"
	-@erase ".\Debug\Agdsrn.obj"
	-@erase "..\agdrift32.ilk"
	-@erase "..\agdrift32.lib"
	-@erase "..\agdrift32.exp"
	-@erase "..\agdrift32.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE F90 /Zi /I "agdrift0/" /c /nologo /MT
# ADD F90 /Zi /I "Debug/" /c /nologo /MT
F90_PROJ=/Zi /I "Debug/" /c /nologo /MT /Fo"Debug/" /Fd"..\agdrift32.pdb" 
F90_OBJS=.\Debug/
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/agdrift32.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:windows /dll /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)/agdrift32.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/agdrift32.dll" /implib:"$(OUTDIR)/agdrift32.lib" 
LINK32_OBJS= \
	"$(INTDIR)/Agsgrd.obj" \
	"$(INTDIR)/Agkirk.obj" \
	"$(INTDIR)/Agwplt.obj" \
	"$(INTDIR)/Agbkg.obj" \
	"$(INTDIR)/Agdrop.obj" \
	"$(INTDIR)/Agarea.obj" \
	"$(INTDIR)/Agn3s.obj" \
	"$(INTDIR)/Agint.obj" \
	"$(INTDIR)/Agsome.obj" \
	"$(INTDIR)/Agsend.obj" \
	"$(INTDIR)/Agdrp.obj" \
	"$(INTDIR)/Agn4d.obj" \
	"$(INTDIR)/Agsmck.obj" \
	"$(INTDIR)/Agwdrs.obj" \
	"$(INTDIR)/Agdrot.obj" \
	"$(INTDIR)/Aginit.obj" \
	"$(INTDIR)/Agdrin.obj" \
	"$(INTDIR)/Agread.obj" \
	"$(INTDIR)/Aglims.obj" \
	"$(INTDIR)/Agkick.obj" \
	"$(INTDIR)/Agfill.obj" \
	"$(INTDIR)/Agsmpl.obj" \
	"$(INTDIR)/Agnums.obj" \
	"$(INTDIR)/Agchk.obj" \
	"$(INTDIR)/Agextd.obj" \
	"$(INTDIR)/Agn2s.obj" \
	"$(INTDIR)/Agupds.obj" \
	"$(INTDIR)/Agsblk.obj" \
	"$(INTDIR)/Agdsr.obj" \
	"$(INTDIR)/Agn3d.obj" \
	"$(INTDIR)/Agkln.obj" \
	"$(INTDIR)/Agterr.obj" \
	"$(INTDIR)/Agdsd.obj" \
	"$(INTDIR)/Aglibr.obj" \
	"$(INTDIR)/Agnns.obj" \
	"$(INTDIR)/Agvel.obj" \
	"$(INTDIR)/Agovl.obj" \
	"$(INTDIR)/Ageqn.obj" \
	"$(INTDIR)/Agsbin.obj" \
	"$(INTDIR)/Agends.obj" \
	"$(INTDIR)/Agunf.obj" \
	"$(INTDIR)/Agcan.obj" \
	"$(INTDIR)/Agtox.obj" \
	"$(INTDIR)/Agsetl.obj" \
	"$(INTDIR)/Aggrnd.obj" \
	"$(INTDIR)/Agtraj.obj" \
	"$(INTDIR)/Agcon.obj" \
	"$(INTDIR)/Agave.obj" \
	"$(INTDIR)/Agnozl.obj" \
	"$(INTDIR)/Agstrm.obj" \
	"$(INTDIR)/Agspln.obj" \
	"$(INTDIR)/Agn2d.obj" \
	"$(INTDIR)/Agsav.obj" \
	"$(INTDIR)/Agsmex.obj" \
	"$(INTDIR)/Agvrf.obj" \
	"$(INTDIR)/Agkrn.obj" \
	"$(INTDIR)/Agorch.obj" \
	"$(INTDIR)/Agdep.obj" \
	"$(INTDIR)/Agaver.obj" \
	"$(INTDIR)/Agwtb.obj" \
	"$(INTDIR)/Agsmti.obj" \
	"$(INTDIR)/Agn4s.obj" \
	"$(INTDIR)/Agnnd.obj" \
	"$(INTDIR)/Agparm.obj" \
	"$(INTDIR)/Agkrr.obj" \
	"$(INTDIR)/Agtrgo.obj" \
	"$(INTDIR)/Agrtrn.obj" \
	"$(INTDIR)/Agcov.obj" \
	"$(INTDIR)/Agsbck.obj" \
	"$(INTDIR)/Agmore.obj" \
	"$(INTDIR)/Agdsrn.obj"

"$(OUTDIR)\agdrift32.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "agdrift32 - Win32 Release"
# Name "agdrift32 - Win32 Debug"

!IF  "$(CFG)" == "agdrift32 - Win32 Release"

!ELSEIF  "$(CFG)" == "agdrift32 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\Agaver.for

"$(INTDIR)\Agaver.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agave.for

"$(INTDIR)\Agave.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agarea.for

"$(INTDIR)\Agarea.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agbkg.for
DEP_F90_AGBKG=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agbkg.obj" : $(SOURCE) $(DEP_F90_AGBKG) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agcan.for
DEP_F90_AGCAN=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agcan.obj" : $(SOURCE) $(DEP_F90_AGCAN) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agchk.for

"$(INTDIR)\Agchk.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agcon.for
DEP_F90_AGCON=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agcon.obj" : $(SOURCE) $(DEP_F90_AGCON) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agcov.for

"$(INTDIR)\Agcov.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agdep.for

"$(INTDIR)\Agdep.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agdrin.for
DEP_F90_AGDRI=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agdrin.obj" : $(SOURCE) $(DEP_F90_AGDRI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agdrop.for
DEP_F90_AGDRO=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agdrop.obj" : $(SOURCE) $(DEP_F90_AGDRO) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agdrot.for
DEP_F90_AGDROT=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agdrot.obj" : $(SOURCE) $(DEP_F90_AGDROT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agdrp.for
DEP_F90_AGDRP=\
	".\AGDSTRUC.INC"\
	

"$(INTDIR)\Agdrp.obj" : $(SOURCE) $(DEP_F90_AGDRP) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agdsd.for
DEP_F90_AGDSD=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agdsd.obj" : $(SOURCE) $(DEP_F90_AGDSD) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agdsr.for
DEP_F90_AGDSR=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agdsr.obj" : $(SOURCE) $(DEP_F90_AGDSR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agdsrn.for

"$(INTDIR)\Agdsrn.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agends.for
DEP_F90_AGEND=\
	".\AGCOMMON.INC"\
	".\AGSAMPLE.INC"\
	

"$(INTDIR)\Agends.obj" : $(SOURCE) $(DEP_F90_AGEND) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Ageqn.for
DEP_F90_AGEQN=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Ageqn.obj" : $(SOURCE) $(DEP_F90_AGEQN) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agextd.for

"$(INTDIR)\Agextd.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agfill.for

"$(INTDIR)\Agfill.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Aggrnd.for

"$(INTDIR)\Aggrnd.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Aginit.for
DEP_F90_AGINI=\
	".\AGDSTRUC.INC"\
	".\AGCOMMON.INC"\
	".\AGSAMPLE.INC"\
	

"$(INTDIR)\Aginit.obj" : $(SOURCE) $(DEP_F90_AGINI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agint.for

"$(INTDIR)\Agint.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agkick.for
DEP_F90_AGKIC=\
	".\AGDSTRUC.INC"\
	

"$(INTDIR)\Agkick.obj" : $(SOURCE) $(DEP_F90_AGKIC) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agkirk.for
DEP_F90_AGKIR=\
	".\AGDSTRUC.INC"\
	

"$(INTDIR)\Agkirk.obj" : $(SOURCE) $(DEP_F90_AGKIR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agkln.for

"$(INTDIR)\Agkln.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agkrn.for

"$(INTDIR)\Agkrn.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agkrr.for

"$(INTDIR)\Agkrr.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Aglibr.for

"$(INTDIR)\Aglibr.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Aglims.for
DEP_F90_AGLIM=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Aglims.obj" : $(SOURCE) $(DEP_F90_AGLIM) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agmore.for
DEP_F90_AGMOR=\
	".\AGCOMMON.INC"\
	".\AGSAMPLE.INC"\
	

"$(INTDIR)\Agmore.obj" : $(SOURCE) $(DEP_F90_AGMOR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agn2d.for

"$(INTDIR)\Agn2d.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agn2s.for

"$(INTDIR)\Agn2s.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agn3d.for

"$(INTDIR)\Agn3d.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agn3s.for

"$(INTDIR)\Agn3s.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agn4d.for

"$(INTDIR)\Agn4d.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agn4s.for

"$(INTDIR)\Agn4s.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agnnd.for

"$(INTDIR)\Agnnd.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agnns.for

"$(INTDIR)\Agnns.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agnozl.for

"$(INTDIR)\Agnozl.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agnums.for
DEP_F90_AGNUM=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agnums.obj" : $(SOURCE) $(DEP_F90_AGNUM) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agorch.for

"$(INTDIR)\Agorch.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agovl.for
DEP_F90_AGOVL=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agovl.obj" : $(SOURCE) $(DEP_F90_AGOVL) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agparm.for

"$(INTDIR)\Agparm.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agread.for
DEP_F90_AGREA=\
	".\AGCOMMON.INC"\
	".\AGSAMPLE.INC"\
	

"$(INTDIR)\Agread.obj" : $(SOURCE) $(DEP_F90_AGREA) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agrtrn.for

"$(INTDIR)\Agrtrn.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsav.for
DEP_F90_AGSAV=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agsav.obj" : $(SOURCE) $(DEP_F90_AGSAV) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsbck.for

"$(INTDIR)\Agsbck.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsbin.for
DEP_F90_AGSBI=\
	".\AGDSTRUC.INC"\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agsbin.obj" : $(SOURCE) $(DEP_F90_AGSBI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsblk.for
DEP_F90_AGSBL=\
	".\AGDSTRUC.INC"\
	

"$(INTDIR)\Agsblk.obj" : $(SOURCE) $(DEP_F90_AGSBL) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsend.for
DEP_F90_AGSEN=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agsend.obj" : $(SOURCE) $(DEP_F90_AGSEN) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsetl.for
DEP_F90_AGSET=\
	".\AGDSTRUC.INC"\
	

"$(INTDIR)\Agsetl.obj" : $(SOURCE) $(DEP_F90_AGSET) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsgrd.for
DEP_F90_AGSGR=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agsgrd.obj" : $(SOURCE) $(DEP_F90_AGSGR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsmck.for
DEP_F90_AGSMC=\
	".\AGSAMPLE.INC"\
	

"$(INTDIR)\Agsmck.obj" : $(SOURCE) $(DEP_F90_AGSMC) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsmex.for
DEP_F90_AGSME=\
	".\AGSAMPLE.INC"\
	

"$(INTDIR)\Agsmex.obj" : $(SOURCE) $(DEP_F90_AGSME) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsmpl.for
DEP_F90_AGSMP=\
	".\AGSAMPLE.INC"\
	

!IF  "$(CFG)" == "agdrift32 - Win32 Release"

# SUBTRACT F90 /Ox

"$(INTDIR)\Agsmpl.obj" : $(SOURCE) $(DEP_F90_AGSMP) "$(INTDIR)"
   $(F90) /I "Release/" /c /nologo /MT /Fo"Release/" $(SOURCE)


!ELSEIF  "$(CFG)" == "agdrift32 - Win32 Debug"


"$(INTDIR)\Agsmpl.obj" : $(SOURCE) $(DEP_F90_AGSMP) "$(INTDIR)"
   $(F90) /Zi /I "Debug/" /c /nologo /MT /Fo"Debug/" /Fd"..\agdrift32.pdb"\
 $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsmti.for
DEP_F90_AGSMT=\
	".\AGSAMPLE.INC"\
	

"$(INTDIR)\Agsmti.obj" : $(SOURCE) $(DEP_F90_AGSMT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agsome.for
DEP_F90_AGSOM=\
	".\AGCOMMON.INC"\
	".\AGSAMPLE.INC"\
	

"$(INTDIR)\Agsome.obj" : $(SOURCE) $(DEP_F90_AGSOM) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agspln.for
DEP_F90_AGSPL=\
	".\AGSAMPLE.INC"\
	

"$(INTDIR)\Agspln.obj" : $(SOURCE) $(DEP_F90_AGSPL) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agstrm.for
DEP_F90_AGSTR=\
	".\AGDSTRUC.INC"\
	

"$(INTDIR)\Agstrm.obj" : $(SOURCE) $(DEP_F90_AGSTR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agterr.for
DEP_F90_AGTER=\
	".\AGDSTRUC.INC"\
	

"$(INTDIR)\Agterr.obj" : $(SOURCE) $(DEP_F90_AGTER) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agtox.for
DEP_F90_AGTOX=\
	".\AGDSTRUC.INC"\
	

"$(INTDIR)\Agtox.obj" : $(SOURCE) $(DEP_F90_AGTOX) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agtraj.for
DEP_F90_AGTRA=\
	".\AGDSTRUC.INC"\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agtraj.obj" : $(SOURCE) $(DEP_F90_AGTRA) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agtrgo.for
DEP_F90_AGTRG=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agtrgo.obj" : $(SOURCE) $(DEP_F90_AGTRG) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agunf.for

"$(INTDIR)\Agunf.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agupds.for
DEP_F90_AGUPD=\
	".\AGDSTRUC.INC"\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agupds.obj" : $(SOURCE) $(DEP_F90_AGUPD) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agvel.for
DEP_F90_AGVEL=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agvel.obj" : $(SOURCE) $(DEP_F90_AGVEL) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agvrf.for
DEP_F90_AGVRF=\
	".\AGCOMMON.INC"\
	

"$(INTDIR)\Agvrf.obj" : $(SOURCE) $(DEP_F90_AGVRF) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agwdrs.for

"$(INTDIR)\Agwdrs.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agwplt.for

"$(INTDIR)\Agwplt.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Agwtb.for

"$(INTDIR)\Agwtb.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
