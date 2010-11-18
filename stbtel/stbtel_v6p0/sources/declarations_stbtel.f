C
C  DECLARATION OF GLOBAL DATA STRUCTURE IN TELEMAC-2D
C
      MODULE DECLARATIONS_STBTEL
C
C***********************************************************************
C  STBTEL VERSION 6.0
C***********************************************************************
C
C  AJOUTE PAR JMH POUR ADAPTATION A BIEF 6.0
C
      CHARACTER*144 NOMGEO,NOMFO1,NOMFON,NOMFO2,NOMIMP,NOMSOU,NOMFRC
      CHARACTER*144 NOMFOR,NOMCAS,NOMLIM,NOMRES
      INTEGER NGEO,NCLE,NCAS,NLIM,NFO1,NFON,NFO2,NIMP,NSOU,NFRC,NRES
C
C
C
C-----------------------------------------------------------------------
C
C       4) INTEGERS
C
C-----------------------------------------------------------------------
C
C       KEY-WORDS AND PARAMETERS
C
      INTEGER NBAT,LGVEC,NSOM,NSOM2,NBFOND,IHAUT,INOP5,NSEC2,NSEC3
      INTEGER NSEC11,NSEC12
C
C-----------------------------------------------------------------------
C
C       5) LOGICAL VALUES
C
C-----------------------------------------------------------------------
C
      LOGICAL OPTASS,DECTRI,COLOR,ELIDEP,DIV4,FONTRI,ADDFAS,PROJEX 
      LOGICAL FUSION,ELISEC,ELPSEC,STOTOT  
C
C-----------------------------------------------------------------------
C
C       6) REALS
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION EPSI,DM,CORTRI,SOM(10,2),SOM2(10,2),SEUSEC 
C
C-----------------------------------------------------------------------
C
C       7) STRINGS
C
C-----------------------------------------------------------------------
C
      CHARACTER*3  STD 
      CHARACTER*9  MAILLE 
      CHARACTER*72 FOND(5) 
C
        SAVE
C
      END MODULE DECLARATIONS_STBTEL 

