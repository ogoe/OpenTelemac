!                    *********************
                     SUBROUTINE BIEF_SUITE
!                    *********************
!
     &(VARSOR,CLAND,NUMDEB,
     & NPRE,STD,HIST,NHIST,NPOIN,AT,TEXTPR,VARCLA,NVARCL,
     & TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NPLAN,DT,NDT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
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
!| ALIRE          |-->| VARIABLES TO BE READ (FOR OTHERS RECORD SKIPPED)
!|                |   | CLANDESTINE VARIABLES ARE SYSTEMATICALLY READ
!| AT             |-->| TIME
!| CLAND          |<--| BLOCK OF CLANDESTINE VARIABLES
!| DT             |<--| TIME STEP
!| FIN            |-->| LOGICAL, SEE NUMDEB
!| HIST           |-->| CONTAINS TIME AND POSSIBLY OTHER VALUES
!| LISTIN         |-->| IF YES, INFORMATIONS PRINTED ON LISTING
!| MAXVAR         |-->| DIMENSION OF ARRAY RELATED TO VARIABLES: ALIRE,..
!| NDT            |-->| OPTIONAL, IF PRESENT THE NUMBER OF TIME STEPS
!|                |   | WILL BE RETURNED
!| NHIST          |-->| NUMBER OF VALUES IN HIST HIST.
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| NPRE           |-->| LOGICAL UNIT OF FILE
!| NUMDEB         |<->| FIN = .TRUE. LAST RECORD TAKEN
!|                |   | FIN = .FALSE. : RECORD NUMBED TAKEN
!| NVARCL         |-->| NUMBER OF CLANDESTINE VARIABLES
!| STD            |-->| FILE BINARY : STD, IBM OU I3E
!| TEXTPR         |-->| NAMES AND UNITS OF VARIABLES.
!| TROUVE         |<--| GIVES (TROUVE(K)=1) VARIABLES PRESENT IN THE FILE
!| VARCLA         |-->| BLOCK FOR CLANDESTINE VARIABLES
!| VARSOR         |<--| BLOCK OF NORMAL VARIABLES
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
