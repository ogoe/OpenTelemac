!                    ****************
                     SUBROUTINE UTIMP
!                    ****************
!
     & (LT,TIME,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRINTS OUT SPECIFIC RESULTS
!+               (SEE THE LIST OF VARIABLES IN DECLARATIONS_TELEMAC3D).
!+
!+            FOR BIEF_OBJ STRUCTURES, THE DOUBLE PRECISION ARRAY
!+                IS IN COMPONENT R, E.G. U%R FOR THE VELOCITY.
!
!history  C LE NORMANT(LNH)    ; F LEPEINTRE (LNH)
!+        25/11/97
!+        V5P2
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
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
!| GRADEBL        |-->| NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS
!| GRAPRDL        |-->| GRAPHIC PRINTOUT PERIOD
!| LISDEBL        |-->| NUMBER OF FIRST TIME STEP FOR LISTING PRINTOUTS
!| LISPRDL        |-->| LISTING PRINTOUT PERIOD
!| LT             |-->| ITERATION NUMBER
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER, INTENT(IN) :: LT,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IVAR,J,NRFO,INUM(14),IH
!
      DOUBLE PRECISION XMES(14),YMES(14),HAUT(14),ARR_TIME(3)
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL P_DMAX,P_DMIN
      LOGICAL FIND_ARR_TIME(3)
      LOGICAL DEJA_UTIMP
!
      DATA    DEJA_UTIMP /.FALSE./
!
      INTRINSIC MOD
!
      SAVE INUM,IH,HAUT,ARR_TIME,FIND_ARR_TIME,DEJA_UTIMP
!
!***********************************************************************
! USER OUTPUT ACCORDING TO THE VALUES: TO BE IMPLEMENTED
!
      IF(.NOT.DEJA_UTIMP) THEN
!
        XMES( 1) = 5550.D0
        XMES( 2) = 11900.D0
        XMES( 3) = 13000.D0
        XMES( 4) = 11900.D0
        XMES( 5) = 13000.D0
        XMES( 6) = 4947.D0
        XMES( 7) = 5717.D0
        XMES( 8) = 6775.D0
        XMES( 9) = 7128.D0
        XMES(10) = 8585.D0
        XMES(11) = 9674.D0
        XMES(12) = 10939.D0
        XMES(13) = 11724.D0
        XMES(14) = 12723.D0
!
        YMES( 1) = 4400.D0
        YMES( 2) = 3250.D0
        YMES( 3) = 2700.D0
        YMES( 4) = 3250.D0
        YMES( 5) = 2700.D0
        YMES( 6) = 4289.D0
        YMES( 7) = 4407.D0
        YMES( 8) = 3869.D0
        YMES( 9) = 3162.D0
        YMES(10) = 3443.D0
        YMES(11) = 3085.D0
        YMES(12) = 3044.D0
        YMES(13) = 2810.D0
        YMES(14) = 2485.D0
!
        DO J=1,14
          INUM(J) = 0
          HAUT(J) = 0.D0
        ENDDO
!
        CALL PROXIM(INUM,XMES,YMES,X2%R,Y2%R,14,NPOIN2,IKLE2%I,NELEM2,
     &              NELEM2)
!
        DO IVAR=1,MAXVAR
          IF (TEXTE(IVAR) == 'WATER DEPTH') IH = IVAR
        ENDDO
!
        DO J=1,3
          FIND_ARR_TIME(J) = .FALSE.
          ARR_TIME(J)      = 0.D0
        ENDDO
!
        DEJA_UTIMP = .TRUE.
!
      ENDIF
!
      DO J=1,14
        IF(INUM(J).NE.0) THEN
          HAUT(J) = MAX(H%R(INUM(J)),HAUT(J))
        ELSE
          HAUT(J) = 0.D0
        ENDIF
      ENDDO
!
      DO J=1,3
        IF(INUM(J).NE.0) THEN
          IF(H%R(INUM(J)).GT.1.D-4.AND..NOT.FIND_ARR_TIME(J)) THEN
            ARR_TIME(J) = AT
            FIND_ARR_TIME(J) = .TRUE.
          ENDIF
        ELSE
          ARR_TIME(J) = 0.D0
          FIND_ARR_TIME(J) = .FALSE.
        ENDIF
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        DO J=1,14
          HAUT(J) = P_DMAX(HAUT(J))+P_DMIN(HAUT(J))
        ENDDO
        DO J=1,3
          ARR_TIME(J) = P_DMAX(ARR_TIME(J))+P_DMIN(ARR_TIME(J))
        ENDDO
      ENDIF
!
      IF(LT.EQ.NIT) THEN
        NRFO = T3D_FILES(T3DRFO)%LU
        WRITE(NRFO,*) 'MAXIMUM WATER DEPTHS'
        DO J=6,14
          WRITE(NRFO,1001) 'MEASUREMENT POINT',J,' = ',HAUT(J),' M'
        ENDDO
!
        WRITE(NRFO,*)
        WRITE(NRFO,1002)'ARRIVAL TIME AT A :',ARR_TIME(1),' S'
        WRITE(NRFO,1002)'TIME FROM A TO B  :',ARR_TIME(2)-ARR_TIME(1),
     &                  ' S'
        WRITE(NRFO,1002)'TIME FROM A TO C  :',ARR_TIME(3)-ARR_TIME(1),
     &                  ' S'
      ENDIF
!
 1001 FORMAT((A,I3,A,F6.2,A))
 1002 FORMAT((A,F7.1,A))
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP

