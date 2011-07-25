!                    ****************
                     SUBROUTINE CHECK
!                    ****************
!
     &(IKLE2,NBOR,NELBOR,IKLBOR,IKLE3,NELBO3,NULONE,NBOR3,NELEM2,NPOIN2,
     & NPTFR,NETAGE,NELEM3,NPTFR3,NTRAC,INFO)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CHECKS FOR COMMON ERRORS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+        V5P1
!+   FORTRAN95 VERSION
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
!| IKLBOR         |-->| CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
!| IKLE2          |-->| GLOBAL NUMBERS OF POINTS IN 2D ELEMENTS
!| IKLE3          |-->| GLOBAL NUMBERS OF POINTS IN 3D ELEMENTS
!| INFO           |-->| LISTING PRINTOUT OR NOT
!| NBOR           |-->| GLOBAL NUMBER OF 2D BOUNDARY POINTS
!| NBOR3          |-->| GLOBAL NUMBER OF 3D BOUNDARY POINTS
!| NELBO3         |-->| ASSOCIATION OF EACH BOUNDARY EDGE
!|                |   | TO THE CORRESPONDING 3D ELEMENT
!| NELBOR         |-->| NUMBER OF THE ADJACENT ELEMENT AT THE K TH
!|                |   | BOUNDARY SEGMENT
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D MESH
!| NETAGE         |-->| NUMBER OF PLANES - 1
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
!| NPTFR3         |-->| NUMBER OF BOUNDARY POINTS IN 3D
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| NULONE         |-->| GOES WITH ARRAY NELBOR. NELBOR GIVES THE 
!|                |   | ADJACENT ELEMENT, NULONE GIVES THE LOCAL
!|                |   | NUMBER OF THE FIRST NODE OF THE BOUNDARY EDGE
!|                |   | I.E. 1, 2 OR 3 FOR TRIANGLES.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: NELEM2, NPOIN2, NPTFR, NETAGE, NELEM3
      INTEGER, INTENT(IN) :: NPTFR3, NTRAC
      INTEGER, INTENT(IN) :: NELBOR(NPTFR), NBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE2(NELEM2,3)
      INTEGER, INTENT(IN) :: IKLBOR(NPTFR,NETAGE,4),IKLE3(NELEM3,6)
      INTEGER, INTENT(IN) :: NELBO3(NPTFR,NETAGE)
      INTEGER, INTENT(IN) :: NULONE(NPTFR,NETAGE,4)
      INTEGER, INTENT(IN) :: NBOR3(NPTFR3)
      LOGICAL, INTENT(IN) :: INFO
!
!-----------------------------------------------------------------------
!
      INTEGER IERR,IEL,N1,N2,IPTFR,IETAGE,ILOC
!
!***********************************************************************
!
! INITIALISES
!
! FATAL ERROR COUNT:
      IERR  = 0
!
!-----------------------------------------------------------------------
! CHECKS ARRAY NELBOR
!
      IF (NCSIZE.LE.1) THEN
        DO IPTFR = 1,NPTFR
          IEL = NELBOR(IPTFR)
          N1  = NBOR(IPTFR)
          IF (N1.NE.IKLE2(IEL,1).AND.N1.NE.IKLE2(IEL,2).AND.
     &        N1.NE.IKLE2(IEL,3)) THEN
            IF (LNG.EQ.1) WRITE(LU,11) IEL,IPTFR
            IF (LNG.EQ.2) WRITE(LU,12) IEL,IPTFR
            IERR = IERR + 1
          ENDIF
        END DO
!
!-----------------------------------------------------------------------
! CHECKS ARRAYS IKLBOR,NELBO3,NULONE
!
        DO ILOC = 1,4
          DO IETAGE = 1,NETAGE
            DO IPTFR = 1,NPTFR
              N1=NBOR3(IKLBOR(IPTFR,IETAGE,ILOC))
              N2=IKLE3(NELBO3(IPTFR,IETAGE),NULONE(IPTFR,IETAGE,ILOC))
              IF (N1.NE.N2) THEN
                IF (LNG.EQ.1) WRITE(LU,51) IPTFR,IETAGE,ILOC,N1,N2
                IF (LNG.EQ.2) WRITE(LU,52) IPTFR,IETAGE,ILOC,N1,N2
                IERR = IERR + 1
              ENDIF
            END DO
          END DO
        END DO
      ENDIF
!
!-----------------------------------------------------------------------
!
! PRINTS OUT THE RESULTS
!
      IF(IERR.EQ.0) THEN
         IF (LNG.EQ.1 .AND. INFO) WRITE(LU,111)
         IF (LNG.EQ.2 .AND. INFO) WRITE(LU,112)
      ELSEIF(IERR.EQ.1) THEN
         IF (LNG.EQ.1) WRITE(LU,121)
         IF (LNG.EQ.2) WRITE(LU,122)
         CALL PLANTE(1)
         STOP
      ELSE
         IF (LNG.EQ.1) WRITE(LU,131) IERR
         IF (LNG.EQ.2) WRITE(LU,132) IERR
         CALL PLANTE(1)
         STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
11    FORMAT(' CHECK: L''ELEMENT',
     &   I6,' N''EST PAS ADJACENT AU POINT DE BORD',I5)
12    FORMAT(' CHECK: ELEMENT',
     &   I6,' IS NOT ADJACENT TO BOUNDARY NODE',I5)
51    FORMAT(' CHECK: ERREUR SUR LA STRUCTURE DE DONNEES',/,
     &       'IPTFR,IETAGE,ILOC,N1,N2 :',5I5)
52    FORMAT(' CHECK: ERROR ON DATA STRUCTURE',/,
     &       'IPTFR,IETAGE,ILOC,N1,N2 :',5I5)
111   FORMAT(' CHECK: AUCUNE ERREUR N''A ETE DETECTEE',////)
112   FORMAT(' CHECK: NO ERROR HAS BEEN DETECTED',////)
121   FORMAT(' CHECK: 1 ERREUR FATALE . ARRET DU PROGRAMME',////)
122   FORMAT(' CHECK: 1 FATALE ERROR . BREAK IN THE PROGRAM',////)
131   FORMAT(' CHECK: ',I4,' ERREURS FATALES . ARRET DU PROGRAMME',////)
132   FORMAT(' CHECK: ',I4,' FATALE ERRORS . BREAK IN THE PROGRAM',////)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CHECK
