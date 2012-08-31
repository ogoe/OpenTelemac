!
!  DECLARATION OF GLOBAL DATA STRUCTURE IN TELEMAC-2D
!
      MODULE DECLARATIONS_STBTEL
!
!***********************************************************************
!  STBTEL VERSION 6.0
!***********************************************************************
!
!  AJOUTE PAR JMH POUR ADAPTATION A BIEF 6.0
!
      CHARACTER*144 NOMGEO,NOMFO1,NOMFON,NOMFO2,NOMIMP,NOMSOU,NOMFRC
      CHARACTER*144 NOMFOR,NOMCAS,NOMLIM,NOMRES
      INTEGER NGEO,NCLE,NCAS,NLIM,NFO1,NFON,NFO2,NIMP,NSOU,NFRC,NRES
!
!
!
!-----------------------------------------------------------------------
!
!       4) INTEGERS
!
!-----------------------------------------------------------------------
!
!       KEY-WORDS AND PARAMETERS
!
      INTEGER NBAT,LGVEC,NSOM,NSOM2,NBFOND,IHAUT,INOP5,NSEC2,NSEC3
      INTEGER NSEC11,NSEC12
!
!-----------------------------------------------------------------------
!
!       5) LOGICAL VALUES
!
!-----------------------------------------------------------------------
!
      LOGICAL OPTASS,DECTRI,COLOR,ELIDEP,DIV4,FONTRI,ADDFAS,PROJEX
      LOGICAL FUSION,ELISEC,ELPSEC,STOTOT
!
!-----------------------------------------------------------------------
!
!       6) REALS
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION EPSI,DM,CORTRI,SOM(10,2),SOM2(10,2),SEUSEC
!
!-----------------------------------------------------------------------
!
!       7) STRINGS
!
!-----------------------------------------------------------------------
!
      CHARACTER*3  STD
      CHARACTER*9  MAILLE
      CHARACTER*72 FOND(5)
!
        SAVE
!
      END MODULE DECLARATIONS_STBTEL
!