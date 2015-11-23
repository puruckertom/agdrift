# Microsoft Developer Studio Project File - Name="agdrift32" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=agdrift32 - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "agdrift32.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "agdrift32.mak" CFG="agdrift32 - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "agdrift32 - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "agdrift32 - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "agdrift32 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\agdrift3"
# PROP BASE Intermediate_Dir ".\agdrift3"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\"
# PROP Intermediate_Dir ".\Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /threads /I "agdrift3/"
# ADD F90 /compile_only /include:"Release/" /math_library:fast /nologo /threads
# ADD CPP /FD
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:windows /dll /machine:I386

!ELSEIF  "$(CFG)" == "agdrift32 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\agdrift0"
# PROP BASE Intermediate_Dir ".\agdrift0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\"
# PROP Intermediate_Dir ".\Debug"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /debug:full /nologo /threads /I "agdrift0/"
# ADD F90 /compile_only /debug:full /include:"Debug/" /nologo /optimize:0 /threads
# ADD CPP /FD
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:windows /dll /debug /machine:I386

!ENDIF 

# Begin Target

# Name "agdrift32 - Win32 Release"
# Name "agdrift32 - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\Agarea.for
# End Source File
# Begin Source File

SOURCE=.\Agave.for
# End Source File
# Begin Source File

SOURCE=.\Agaver.for
# End Source File
# Begin Source File

SOURCE=.\Agbkg.for
DEP_F90_AGBKG=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agcan.for
DEP_F90_AGCAN=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agchk.for
# End Source File
# Begin Source File

SOURCE=.\Agcon.for
DEP_F90_AGCON=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agcov.for
# End Source File
# Begin Source File

SOURCE=.\Agdep.for
# End Source File
# Begin Source File

SOURCE=.\Agdrin.for
DEP_F90_AGDRI=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agdrop.for
DEP_F90_AGDRO=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agdrot.for
DEP_F90_AGDROT=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agdrp.for
DEP_F90_AGDRP=\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agdsd.for
DEP_F90_AGDSD=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agdsr.for
DEP_F90_AGDSR=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agdsrn.for
# End Source File
# Begin Source File

SOURCE=.\Agends.for
DEP_F90_AGEND=\
	".\AGCOMMON.INC"\
	".\AGSAMPLE.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Ageqn.for
DEP_F90_AGEQN=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agextd.for
# End Source File
# Begin Source File

SOURCE=.\Agfill.for
# End Source File
# Begin Source File

SOURCE=.\Aggrnd.for
# End Source File
# Begin Source File

SOURCE=.\Aginit.for
DEP_F90_AGINI=\
	".\AGCOMMON.INC"\
	".\AGDSTRUC.INC"\
	".\AGSAMPLE.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agint.for
# End Source File
# Begin Source File

SOURCE=.\Agkick.for
DEP_F90_AGKIC=\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agkirk.for
DEP_F90_AGKIR=\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agkln.for
# End Source File
# Begin Source File

SOURCE=.\Agkrn.for
# End Source File
# Begin Source File

SOURCE=.\Agkrr.for
# End Source File
# Begin Source File

SOURCE=.\Aglibr.for
# End Source File
# Begin Source File

SOURCE=.\Aglims.for
DEP_F90_AGLIM=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agmore.for
DEP_F90_AGMOR=\
	".\AGCOMMON.INC"\
	".\AGSAMPLE.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agn2d.for
# End Source File
# Begin Source File

SOURCE=.\Agn2s.for
# End Source File
# Begin Source File

SOURCE=.\Agn3d.for
# End Source File
# Begin Source File

SOURCE=.\Agn3s.for
# End Source File
# Begin Source File

SOURCE=.\Agn4d.for
# End Source File
# Begin Source File

SOURCE=.\Agn4s.for
# End Source File
# Begin Source File

SOURCE=.\Agnnd.for
# End Source File
# Begin Source File

SOURCE=.\Agnns.for
# End Source File
# Begin Source File

SOURCE=.\Agnozl.for
# End Source File
# Begin Source File

SOURCE=.\Agnums.for
DEP_F90_AGNUM=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agorch.for
# End Source File
# Begin Source File

SOURCE=.\Agovl.for
DEP_F90_AGOVL=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agparm.for
# End Source File
# Begin Source File

SOURCE=.\Agread.for
DEP_F90_AGREA=\
	".\AGCOMMON.INC"\
	".\AGSAMPLE.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agrot.for
DEP_F90_AGROT=\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agrtrn.for
# End Source File
# Begin Source File

SOURCE=.\Agsav.for
DEP_F90_AGSAV=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agsbck.for
# End Source File
# Begin Source File

SOURCE=.\Agsbin.for
DEP_F90_AGSBI=\
	".\AGCOMMON.INC"\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agsblk.for
DEP_F90_AGSBL=\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agsend.for
DEP_F90_AGSEN=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agsetl.for
DEP_F90_AGSET=\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agsgrd.for
DEP_F90_AGSGR=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agsmck.for
DEP_F90_AGSMC=\
	".\AGSAMPLE.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agsmex.for
DEP_F90_AGSME=\
	".\AGSAMPLE.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agsmpl.for
DEP_F90_AGSMP=\
	".\AGSAMPLE.INC"\
	

!IF  "$(CFG)" == "agdrift32 - Win32 Release"

# ADD F90 /optimize:4

!ELSEIF  "$(CFG)" == "agdrift32 - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Agsmti.for
DEP_F90_AGSMT=\
	".\AGSAMPLE.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agsome.for
DEP_F90_AGSOM=\
	".\AGCOMMON.INC"\
	".\AGSAMPLE.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agspln.for
DEP_F90_AGSPL=\
	".\AGSAMPLE.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agstrm.for
DEP_F90_AGSTR=\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agterr.for
DEP_F90_AGTER=\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agtox.for
DEP_F90_AGTOX=\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agtraj.for
DEP_F90_AGTRA=\
	".\AGCOMMON.INC"\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agtrgo.for
DEP_F90_AGTRG=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agunf.for
# End Source File
# Begin Source File

SOURCE=.\Agupds.for
DEP_F90_AGUPD=\
	".\AGCOMMON.INC"\
	".\AGDSTRUC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agvel.for
DEP_F90_AGVEL=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agvrf.for
DEP_F90_AGVRF=\
	".\AGCOMMON.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\Agwdrs.for
# End Source File
# Begin Source File

SOURCE=.\Agwplt.for
# End Source File
# Begin Source File

SOURCE=.\Agwtb.for
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=.\AGCOMMON.INC
# End Source File
# Begin Source File

SOURCE=.\AGDSTRUC.INC
# End Source File
# Begin Source File

SOURCE=.\AGSAMPLE.INC
# End Source File
# End Target
# End Project
