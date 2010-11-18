C                       *****************
                        SUBROUTINE INITRI
C                       *****************
C    
     *( NPOIN1,TYPELE,NGEO,NFO1)
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2      07.04.1993  P. LANG     (LHF)
C***********************************************************************
C
C     FONCTION  : INITIALISATION DES INFORMATIONS DANS LE CAS DE TRIGRID
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C !      NOM       |MODE!                   ROLE
C !________________|____!______________________________________________
C ! NPOIN1         !<-- ! NOMBRE DE POINTS DU MAILLAGE      
C ! TYPELE         !<-- ! TYPE D'ELEMENT DU MAILLAGE (ICI TRIAGLES)   
C !________________!____!______________________________________________
C ! COMMON:        !    !
C !  GEO:          !    !
C !    MESH        !<-- ! TYPE DES ELEMENTS DU MAILLAGE
C !    NDP         !<-- ! NOMBRE DE NOEUDS PAR ELEMENTS ( ICI FORCEMENT 
C !    NPOIN       !<-- ! NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C !    NELEM       !<-- ! NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
C !    NPMAX       ! -->! DIMENSION EFFECTIVE DES TABLEAUX X ET Y
C !                !    ! (NPMAX = NPOIN + 100)
C !    NELMAX      ! -->! DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
C !                !    ! LES ELEMENTS (NELMAX = NELEM + 200)
C !  FICH:         !    !
C !    NRES        ! -->! NUMERO DU CANAL DU FICHIER DE SERAFIN
C !    NGEO       ! -->! NUMERO DU CANAL DU FICHIER MAILLEUR
C !    NLIM      ! -->! NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
C ¸    NFO1      ¸ -->! NUMERO DU CANAL DU FICHIER TRIANGLE DE TRIGRID
C !________________!____!______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C APPELE PAR : HOMERE
C APPEL DE : 
C***********************************************************************
C     
      IMPLICIT NONE
C
      INTEGER       MESH, NDP, NPOIN, NELEM, NPMAX, NELMAX
      INTEGER       NGEO
      INTEGER       NPOIN1, NFO1
      CHARACTER*(*) TYPELE
      CHARACTER*1   ZDUMMY
C
C COMMON
C
      COMMON/GEO/MESH, NDP, NPOIN, NELEM, NPMAX, NELMAX
C
      REWIND (NGEO)
      REWIND (NFO1)
      READ (NGEO,*) NPOIN1
      NPOIN = NPOIN1
      NELEM = 0
 1    CONTINUE
        READ (NFO1, '(A1)', END=9000) ZDUMMY
        NELEM = NELEM + 1
      GOTO 1
C
 9000 CONTINUE
      TYPELE = 'TRIANGLES  '
      NDP = 3
      MESH = 3 
C
      RETURN
      END
