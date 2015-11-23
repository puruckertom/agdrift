C $Id: agdstruc.inc,v 1.6 2006/11/08 15:18:12 tom Exp $
C Continuum Dynamics, Inc.
C
C PURPOSE
C  INCLUDE file defining data structures for FORTRAN/Visual Basic
C  communication, program AgDRIFT.
C
C  These structures must exactly match those described in AGDRIFT1.BAS
C
C REVISION HISTORY
C  12/ 5/95 1.0 TBC - Initial coding.
C  12/12/95 1.1 TBC - Folded USERCALC structure into USERDATA structure.
C                     Added VALID, CODEVERS, STRTDATE, STRTTIME to
C                     USERCALC structure.
C   1/24/96 1.2 TBC - Added support for new swath width description.
C                     Added SWTYPE above SWATHWID in CTLDATA.
C   1/26/96 1.3 TBC - Added CALCTYPE to Dropkick Data
C                     Removed single COV from Calc data and:
C                   - Added NUMCOV,COVVAL,COVESW to Calc data
C                     for COV/Effective Swath Width support
C   1/31/96 1.4 TBC - Added MSGLOG to CALC structure
C   2/6/96  1.5 TBC - Added MaxErrorLevel to DKDATA
C                     Added MaxErrorLevel to USERCALC
C   2/19/96 1.6 TBC - Changed TROUTON to ELONGVIS in DKDATA
C   3/21/96 1.7 TBC - Added HGTCAN to UD.MET
C                     Added NUMCON, CONVAL, CONDIST to CALCDATA
C                     Added AIRDRIFT and EVAPFRAC to CALCDATA
C   8/9/96  1.8 TBC - Combined comment section and code section into
C                     one section using inline comments.
C                   - Added APPEFF to CALCDATA
C                   - Increase size of DEPDIST and DEPVAL arrays in
C                     CALCDATA from 300 to 401
C                   - Added WD to UD.MET
C  8/19/96  1.9 TBC - Added PROPEFF to ACDATA
C                   - Added DRAG to ACDATA
C                   - Added WINDHGT to METDATA
C                   - Added PRESSURE to METDATA
C                   - Added VTXDECAY to METDATA
C                   - Added MAXTIME to CTLDATA
C 10/18/96  2.0 TBC - Increased Dep, PID arrays to 500 in CALCDATA
C 10/30/96  2.1 TBC - Increased Dep, PID arrays to 900 in CALCDATA
C                     Added MAXDWND to CTLDATA
C 11/22/96  2.2 TBC - Add DWNDEP to USERCALC
C  1/29/97  2.3 TBC - Added NUMSGL, SGLDIST, SGLVAL to CALCDATA
C   2/6/97  2.4 TBC - Added NUMSWATH to GADATA
C                   - Split out CALCDATA from USERCALC
C  2/14/97  2.5 TBC - Added DEPINTRP and PIDINTRP to USERCALC
C  2/26/98  2.6 TBC - Added BOOMWIDTH to NZDATA
C                   - Removed USERCALC structure completely
C                   - Added RCS header
C 11/18/98  2.7 TBC - NZDATA: Changed POSFWD, POSVERT to arrays (size 60) to support
C                     individual nozzle control.
C                     DSDDATA, DKDATA: Changed to array of three structures
C                     to support nozzle enhancements.
C 12/ 6/99  2.8 TBC - Removed NVamt, ACamt from SMDATA and added ACfrac
C  1/28/00  2.9 TBC - Removed F50, F141, F220 from DK, added Slope
C  2/29/00  3.0 TBC - Added HGTRNMN, HGTRNMX to CTLDATA
C  4/ 3/00  3.1 TBC - Extended definition of DSDDATA.TYPE to include Agkirk
C                   - New section BKDATA to support Agkirk
C                   - Added SPRANGLE, SPRTYPE to DKDATA
C 10/18/05  3.2 TBC - Added INSOL to METDATA
C                   - New structure HKDATA for Rotary Atomizers with:
C                     MXERRLV, SPEED, BLADEANG, FLOWRATE, SPRTYPE,
C                     ROTTYPE, RPM
C                   - Added SPECSRC to BKDATA
C                   - Added CALCINP, ACTSOLFR, ADDSOLFR, ACTNVFRA,
C                     ADDNVFRA, FLOWUNIT to SMDATA
C
C*****************************************************************************
C
C Definitions:
C
C Ground Application Data
      STRUCTURE /GADATA/
        INTEGER*2 BASICTYP      !0-2=low boom m,c,vc 3-5=high boom m,c,vc
        INTEGER*2 NUMSWATH      !Number of swaths for ground sprayer
      END STRUCTURE
C
C Orchard Airblast Application Data
      STRUCTURE /OADATA/
        INTEGER*2 BASICTYP      !0-2=combination 3-13=individual
        INTEGER*2 BEGTROW       !Beginning tree row
        INTEGER*2 ENDTROW       !Ending tree row
      END STRUCTURE
C
C Drop Size Distribution Data
      STRUCTURE /DSDDATA/
        INTEGER*2 TYPE          !0=basic 1=dropkick 2=user-def 3,4=lib
        INTEGER*2 BASICTYP      !BCPC Category: 0-3
        CHARACTER*40 NAME       !Name for DSD
        INTEGER*2 LNAME         !Length of Name
        INTEGER*2 NUMDROP       !Number of Drop Categories
        REAL*4 DIAM(100)        !Drop Diameters (um)
        REAL*4 MASSFRAC(100)    !Drop Volume Fractions
        INTEGER*2 LIBFLAG       !AGDSL Flag: 1=call AGDSL w/key 3 2=key 4
      END STRUCTURE
C
C USDA ARS Nozzle Models Data
      STRUCTURE /BKDATA/
        INTEGER*2 MXERRLV       !Max error level 0=none 1=warn 2=err
        INTEGER*2 NOZTYPE       !Nozzle Type: 0=user 1=lib
        CHARACTER*40 NAMENOZ    !Nozzle Name
        INTEGER*2 LNAMENOZ      !Length of NameNoz
        REAL*4 ORIFICE          !Orifice size (in) or Orifice Number
        REAL*4 SPEED            !Tunnel Speed (m/s)
        REAL*4 NOZANGLE         !Nozzle Deflector or Angle (deg)
        REAL*4 PRESSURE         !Nozzle Pressure (bar)
        INTEGER*2 SPRTYPE       !Spray Type: 0=Spray Quality 1=DSD
        INTEGER*2 SPECSRC       !Drop Spectrum Measurement Source 0=Malvern 1=PMS
      END STRUCTURE
C
C USDA FS Rotary Atomizer Models Data
      STRUCTURE /HKDATA/
        INTEGER*2 MXERRLV       !Max error level 0=none 1=warn 2=err
        INTEGER*2 MATTYPE       !Material Type: 0=water 1=StaPut 2=Hasten 3=Foray
        INTEGER*2 ROTTYPE       !Atomizer Type: 0=AU4000 1=AU5000
        REAL*4 SPEED            !Tunnel Speed (m/s)
        REAL*4 BLADEANG         !Nozzle Blade Angle (deg)
        REAL*4 RPM              !Rotation Rate (rpm)
        REAL*4 FLOWRATE         !Nozzle Flow Rate (L/min)
        INTEGER*2 SPRTYPE       !Spray Type: 0=Spray Quality 1=DSD
      END STRUCTURE
C
C Dropkick data
      STRUCTURE /DKDATA/
        INTEGER*2 MXERRLV       !Max error level 0=none 1=warn 2=err
        INTEGER*2 NOZTYPE       !Nozzle Type: 0=user 1=lib
        CHARACTER*40 NAMENOZ    !Nozzle Name
        INTEGER*2 LNAMENOZ      !Length of NameNoz
        REAL*4 VMD              !Volume Median Diameter (um)
        REAL*4 RSPAN            !Relative Span
        REAL*4 EFFDIAM          !Effective Diameter (cm)
        REAL*4 SPRANGLE         !Spray Angle (deg)
        INTEGER*2 MATTYPE       !Material Type: 0=user 1=lib
        CHARACTER*40 NAMEMAT    !Material Name
        INTEGER*2 LNAMEMAT      !Length of NameMat
        REAL*4 SURFTENS         !Dynamic Surface Tension (dynes/cm)
        REAL*4 SHEARVIS         !Shear Viscosity (cp)
        REAL*4 DENSITY          !Density (gm/cm3)
        REAL*4 ELONGVIS         !Elongational Viscosity (cp)
        REAL*4 SPEED            !Tunnel Speed (m/s)
        REAL*4 NOZANGLE         !Nozzle Angle (deg)
        REAL*4 PRESSURE         !Nozzle Pressure (bar)
        INTEGER*2 FLOWFLAG      !0=Flowrate from scaled value 1=input
        REAL*4 FLOW             !Flow Rate (l/min)
        INTEGER*2 SPRTYPE       !Spray Type: 0=Spray Quality 1=DSD
      END STRUCTURE
C
C Spray Material Data
      STRUCTURE /SMDATA/
        INTEGER*2 TYPE          !Type: 0=basic 1=user-def 2=lib
        INTEGER*2 BASICTYP      !Basic Type: 0=oil 1=water
        CHARACTER*40 NAME       !Spray Material Name
        INTEGER*2 LNAME         !Length of Name
        INTEGER*2 CALCINP       !Calculation input slection 0=enter rates 1=enter tank mix
        REAL*4 NVFRAC           !Nonvolatile Fraction
        REAL*4 ACFRAC           !Active fraction
        REAL*4 ACTSOLFR         !Fraction of tank mix that is Active Solution
        REAL*4 ADDSOLFR         !Fraction of tank mix that is Additive Solution
        REAL*4 ACTNVFRA         !Fraction of Active Solution that is nonvolatile
        REAL*4 ADDNVFRA         !Fraction of Additive Solution that is nonvolatile
        REAL*4 FLOWRATE         !Flow Rate (L/ha)
        INTEGER*2 FLOWUNIT      !Flow Rate units 0=L/ha 1=L/min
        REAL*4 SPECGRAV         !Specific Gravity of tank mix
        REAL*4 NONVGRAV         !Specific Gravity of nonvolatile
        REAL*4 EVAPRATE         !Evaporation Rate ()
      END STRUCTURE
C
C Aircraft Data
      STRUCTURE /ACDATA/
        INTEGER*2 TYPE          !0=basic 1=user-def 2=lib
        INTEGER*2 BASICTYP      !0=AgHusky 1=AT401 2=Wasp 3=AT502
        CHARACTER*40 NAME       !Aircraft Name from Library
        INTEGER*2 LNAME         !Length of Name
        INTEGER*2 WINGTYPE      !Aircraft Wing Type: 3=fixed 4=helicopter
        REAL*4 SEMISPAN         !Semispan or Rotor Radius (m)
        REAL*4 TYPSPEED         !Typical Spraying Speed (m/s)
        REAL*4 BIPLSEP          !Biplane Distance Between Wings (m)
        REAL*4 WEIGHT           !Weight (kg)
        REAL*4 PLANAREA         !Planform Area (m2)
        REAL*4 PROPRPM          !Propeller or Rotor RPM
        REAL*4 PROPRAD          !Propeller Radius (m)
        REAL*4 PROPEFF          !Propeller Efficiency
        REAL*4 ENGVERT          !Engine Vertical Position (m)
        REAL*4 ENGFWD           !Engine Forward Position (m)
        INTEGER*2 NUMENG        !Number of Engines
        REAL*4 ENGHORIZ(2)      !Engine Horizontal Position (m)
        REAL*4 WINGVERT         !Distance vertical from trailing edge to wingtip
        REAL*4 BOOMVERT         !Distance vertical from trailing edge to boom
        REAL*4 BOOMFWD          !Distance forward from trailing edge to boom
        REAL*4 DRAG             !Drag Coefficient
      END STRUCTURE
C
C Nozzle Data
      STRUCTURE /NZDATA/
        INTEGER*2 TYPE          !Distribution Type: 0=basic 1=user-def
        INTEGER*2 BASICTYP      !Basic Type (same as Basic Aircraft)
        CHARACTER*40 NAME       !Distribution Name
        INTEGER*2 LNAME         !Length of Name
        INTEGER*2 NUMNOZ        !Number of Nozzles
        INTEGER*2 NOZTYP(60)    !Nozzle (DSD) Type (0-2)
        REAL*4 POSHORIZ(60)     !Horizontal Positions (m)
        REAL*4 POSVERT(60)      !Vertical Positions (m)
        REAL*4 POSFWD(60)       !Forward (Axial) Positions (m)
        REAL*4 POSHLIMIT        !Wingspan % for user-def Dist Limit
        REAL*4 BOOMWIDTH        !Dist Limit % for basic dists
      END STRUCTURE
C  
C Meteorological Data
      STRUCTURE /METDATA/
        REAL*4 WINDSPD          !Wind Speed (m/s)
        REAL*4 WINDDIR          !Wind Direction (deg)
        REAL*4 WINDHGT          !Height of Wind Speed measurement (m)
        REAL*4 TEMP             !Temperature (deg C)
        REAL*4 HUMIDITY         !Relative Humidity (percent)
        REAL*4 PRESSURE         !Ambient Barometric Pressure (mb)
        REAL*4 VTXDECAY         !Vortex Decay Rate (m/s)
        REAL*4 SURFRUFF         !Surface Roughness (m)
        INTEGER*2 INSOL         !Insolation Index (0-6: Strong, Slight, etc.)
      END STRUCTURE
C
C Canopy Data
      STRUCTURE /CANDATA/
        INTEGER*2 TYPE          !Canopy Type 0=none 1=story 1=optical
        CHARACTER*40 NAME       !Canopy Name
        INTEGER*2 LNAME         !Length of Name
        REAL*4 ELESIZ           !Element Size (m)
        REAL*4 STANDEN          !Stand Density (stems/ha)
        INTEGER*2 NUMENV        !Number of tree envelope levels
        REAL*4 ENVHGT(25)       !Height (m)
        REAL*4 ENVDIA(25)       !Diameter (m)
        REAL*4 ENVPOP(25)       !Probability of Penetration
        INTEGER*2 OPTYPE        !Optical Canopy Type 1=user-def 2=library
        REAL*4 LIBHGT           !Height (m)
        REAL*4 LIBLAI           !Leaf Area Index
        REAL*4 LIBB             !Coefficient B
        REAL*4 LIBC             !Coefficient C
        INTEGER*2 NUMLAI        !Number of LAI levels
        REAL*4 LAIHGT(10)       !Height (m)
        REAL*4 LAICUM(10)       !Cumulative LAI
        REAL*4 TEMP             !Temperature within canopy (deg C)
        REAL*4 HUMIDITY         !Relative Humidity Within anopy (%)
        REAL*4 NDRUFF           !Nondimensional Surface Roughness
        REAL*4 NDDISP           !Nondimensional Displacement
        REAL*4 HEIGHT           !Canopy Height for Tier II/FS/Aerial (m)
      END STRUCTURE
C
C Terrain Data
C
      STRUCTURE /TRNDATA/
        REAL*4 ZREF             !Ground Reference (m)
        REAL*4 UPSLOPE          !Ground upslope angle (deg)
        REAL*4 SIDESLOPE        !Ground sideslope angle (deg)
      END STRUCTURE
C
C Control Data
      STRUCTURE /CTLDATA/
        REAL*4 HEIGHT           !Boom Height (m)
        INTEGER*2 NUMLINES      !Number of Spray Lines
        INTEGER*2 SWTYPE        !Swath Width Type 0=fixed 1=1.2 WS 2=fact*WS
        REAL*4 SWATHWID         !Swath Width (m)
        INTEGER*2 SDTYPE        !Disp Type: 0=1/2 sw 1=1 sw 2=% max 3=fixed
        REAL*4 SDVALUE          !Swath Displacement (m)
        REAL*4 FLXPLANE         !Flux Plane Location (m)
        REAL*4 HGTRNMN          !Min Transport Height (m)
        REAL*4 HGTRNMX          !Max Transport Height (m)
        REAL*4 MAXTIME          !Maximum Compute Time (sec)
        REAL*4 MAXDWND          !Maximum Downwind Distance (m)
        INTEGER*2 HALFBOOM      !Flag for half boom: 0=no 1=yes
      END STRUCTURE
C
C Container Structure For All User Input Data
      STRUCTURE /USERDATA/
        INTEGER*2 TIER          !Tier: 1, 2 or 3
        INTEGER*2 APPLMETH      !Application Method: 0=aerial 1=grd 2=oa
        INTEGER*2 SMOKEY        !Type: 0=regulatory 1=forest service
        INTEGER*4 TITLE         !Run Title (VB string, DO NOT USE)
        INTEGER*4 NOTES         !User Notes (VB string, DO NOT USE)
        RECORD /GADATA/ GA      !Ground Application Data
        RECORD /OADATA/ OA      !Orchard Airblast Application Data
        RECORD /DSDDATA/ DSD(3) !Drop Size Distribution Data
        RECORD /BKDATA/ BK(3)   !DropKirk Data
        RECORD /HKDATA/ HK(3)   !FS Nozzle Model Data
        RECORD /DKDATA/ DK(3)   !DropKick Data
        RECORD /SMDATA/ SM      !Spray Material Data
        RECORD /ACDATA/ AC      !Aircraft Data
        RECORD /NZDATA/ NZ      !Nozzle Data
        RECORD /METDATA/ MET    !Meteorological Data
        RECORD /CANDATA/ CAN    !Canopy Data
        RECORD /TRNDATA/ TRN    !Terrain Data
        RECORD /CTLDATA/ CTL    !Control Data
      END STRUCTURE