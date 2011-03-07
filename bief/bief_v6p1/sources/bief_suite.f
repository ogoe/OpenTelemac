!                    *********************
                     SUBROUTINE BIEF_SUITE
!                    *********************
!
     &(VARSOR,CLAND,NUMDEB,
     & NPRE,STD,HIST,NHIST,NPOIN,AT,TEXTPR,VARCLA,NVARCL,
     & TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NPLAN,DT,NDT)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    READS THE RESULTS WRITTEN TO A RESULTS FILE.
!
!history
!+        09/12/2008
!+
!+   STD IS NOW A STRING OF ANY SIZE
!
!history  J-M HERVOUET (LNHE)
!+        09/04/2009
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALIRE          |-->| VARIABLES QU'IL FAUT LIRE (POUR LES AUTRES ON
!|                |   | SAUTE L'ENREGISTREMENT CORRESPONDANT)
!|                |   | LES VARIABLES CLANDESTI-NES SONT LUES
!|                |   | SYSTEMATIQUEMENT.
!| AT             |-->| TEMPS
!| CLAND          |<--| BLOC DES VARIABLES CLANDESTI-NES
!| DT             |---|
!| FIN            |-->| VOIR LE TROISIEME ARGUMENT NUMDEB
!| HIST           |-->| TABLEAU DE VALEURS MISES DANS L'ENREGISTREMENT
!|                |   | DU TEMPS.
!| LISTIN         |-->| SI OUI, IMPRESSION D'INFORMATIONS SUR LISTING
!| MAXVAR         |-->| DIMENSION DES TABLEAUX DES VARIABLES : ALIRE, ETC
!| NDT            |---|
!| NHIST          |-->| NOMBRE DE VALEURS DANS LE TABLEAU HIST.
!| NPLAN          |---|
!| NPOIN          |-->| NOMBRE DE POINTS DANS LE MAILLAGE
!| NPRE           |-->| NUMERO DE CANAL DU FICHIER
!| NUMDEB         |<->| FIN = .TRUE. NUMERO DU DERNIER ENREGISTREMENT
!|                |   | FIN = .FALSE. : NUMERO DE L'ENREGISTREMENT
!|                |   | QUE L'ON VEUT LIRE.
!| NVARCL         |-->| NOMBRE DE VARIABLES CLANDESTI-NES.
!| STD            |-->| BINAIRE DU FICHIER : STD, IBM OU I3E
!| TEXTPR         |-->| NOMS ET UNITES DES VARIABLES.
!| TROUVE         |<--| INDIQUE (TROUVE(K)=1) LES VARIABLES TROUVEES
!|                |   | DANS LE FICHIER.
!|                |   | DE K =  1 A 26 VARIABLES NORMALES
!|                |   | DE K = 27 A 36 VARIABLES CLANDESTI-NES.
!| VARCLA         |-->| TABLEAU OU L'ON RANGE LES VARIABLES
!|                |   | CLANDESTIINES.
!| VARSOR         |<--| BLOC DES TABLEAUX CONTENANT LES VARIABLES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_SUITE => BIEF_SUITE
      USE M_MED
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARSOR,CLAND
      INTEGER, INTENT(IN), OPTIONAL :: NPLAN
      INTEGER, INTENT(IN)           :: NHIST,NVARCL,MAXVAR
      INTEGER                       :: NUMDEB,NPRE,NPOIN,TROUVE(MAXVAR)
      INTEGER                       :: ALIRE(MAXVAR)
      CHARACTER(LEN=*)              :: STD
      CHARACTER(LEN=32)             :: TEXTPR(MAXVAR),VARCLA(NVARCL)
      DOUBLE PRECISION              :: HIST(*),AT
      LOGICAL                       :: FIN,LISTIN
      DOUBLE PRECISION, INTENT(OUT), OPTIONAL :: DT
      INTEGER, INTENT(OUT), OPTIONAL :: NDT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NNPLAN, NNDT
      DOUBLE PRECISION DDT
!
!-----------------------------------------------------------------------
!
      IF(PRESENT(NPLAN)) THEN
        NNPLAN=NPLAN
      ELSE
        NNPLAN=1
      ENDIF
!
      SELECT CASE(STD)
!
        CASE ('SERAFIN ','SERAFIND')
!
          IF(PRESENT(DT)) THEN
            CALL SUITE_SERAFIN(VARSOR,CLAND,NUMDEB,NPRE,STD,HIST,
     &                         NHIST,NPOIN,AT,TEXTPR,VARCLA,NVARCL,
     &                         TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NNPLAN,
     &                         DT)
          ELSE
            CALL SUITE_SERAFIN(VARSOR,CLAND,NUMDEB,NPRE,STD,HIST,
     &                         NHIST,NPOIN,AT,TEXTPR,VARCLA,NVARCL,
     &                         TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NNPLAN)
          ENDIF
!
        CASE ('MED     ')
!
          CALL SUITE_MED(VARSOR,CLAND,NUMDEB,NPRE,STD,HIST,NHIST,
     &                   NPOIN,AT,TEXTPR,VARCLA,NVARCL,TROUVE,ALIRE,
     &                   LISTIN,FIN,MAXVAR,NNPLAN,DDT,NNDT)
          IF(PRESENT(DT)) DT=DDT
          IF(PRESENT(NDT)) NDT=NNDT
!
        CASE DEFAULT
!
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'BIEF_SUITE : MAUVAIS FORMAT : ',STD
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'BIEF_SUITE: BAD FILE FORMAT : ',STD
          ENDIF
          CALL PLANTE(1)
          STOP
!
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END