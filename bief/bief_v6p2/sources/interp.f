!                    *****************
                     SUBROUTINE INTERP
!                    *****************
!
     & ( U , UTILD , SHP , NDP , SHZ , ETA , ELT , NP , NPOIN2 , NPLAN ,
     &   IELM , IKLE , NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INTERPOLATES THE VALUES OF A FUNCTION AT SOME OF THE
!+                MESH NODES ACCORDING TO THE BARYCENTRIC COORDINATES
!+                OF THE POINTS AND THE VALUES AT THE NODES OF THE
!+                FUNCTION.
!
!warning  DOES NOT WORK IF THE PROVIDED BARYCENTRIC COORDINATES
!+            DO NOT CORRESPOND TO THE ELEMENT OF THE FUNCTION
!warning  ELEMENTS OTHER THAN 11, 21 AND 41 ARE NOT IMPLEMENTED
!
!history  J-M JANIN (LNH)
!+        28/04/93
!+        V5P1
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
!| ELT            |-->| 2D ELEMENT AT THE FOOT OF CHARACTERISTIC LINES.
!| ETA            |-->| LAYER NUMBER AT THE FOOT OF CHARACTERISTIC LINES.
!| IELM           |-->| TYPE OF ELEMENT.
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT FOR U.
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NP             |-->| NOMBER OF POINTS TO BE INTERPOLATED.
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| SHP            |-->| 2D BARYCENTRIC COORDINATES AT THE FOOT OF
!|                |   | CHARACTERISTIC LINES.
!| SHZ            |-->| BARYCENTRIC COORDINATES ALONG Z AT THE FOOT OF
!|                |   | CHARACTERISTIC LINES (FOR TELEMAC-3D)
!| U              |-->| VALUES AT NODES FOR INTERPOLATION.
!| UTILD          |<--| INTERPOLATED VALUES.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_INTERP => INTERP
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NP,NELMAX,NPLAN,NPOIN2,NDP,IELM
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),ELT(NP),ETA(NP)
!
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: SHP(NDP,NP),SHZ(NP)
      DOUBLE PRECISION, INTENT(INOUT) :: UTILD(NP)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.11.OR.IELM.EQ.12) THEN
!
!    P1 TRIANGLES
!    ============
!
      DO IP = 1 , NP
         UTILD(IP) = U(IKLE(ELT(IP),1),1) * SHP(1,IP)
     &             + U(IKLE(ELT(IP),2),1) * SHP(2,IP)
     &             + U(IKLE(ELT(IP),3),1) * SHP(3,IP)
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELM.EQ.13) THEN
!
!    P2 TRIANGLES
!    ============
!
      DO IP = 1 , NP
         UTILD(IP) = U(IKLE(ELT(IP),1),1) *
     &               (2.D0*SHP(1,IP)-1.D0)* SHP(1,IP)
     &             + U(IKLE(ELT(IP),2),1) *
     &               (2.D0*SHP(2,IP)-1.D0)* SHP(2,IP)
     &             + U(IKLE(ELT(IP),3),1) *
     &               (2.D0*SHP(3,IP)-1.D0)* SHP(3,IP)
     &             + U(IKLE(ELT(IP),4),1) * 4.D0 * SHP(1,IP)*SHP(2,IP)
     &             + U(IKLE(ELT(IP),5),1) * 4.D0 * SHP(2,IP)*SHP(3,IP)
     &             + U(IKLE(ELT(IP),6),1) * 4.D0 * SHP(3,IP)*SHP(1,IP)
      ENDDO
!
!------------------------------------------------------------------------
!
      ELSEIF(IELM.EQ.41) THEN
!
!    TELEMAC-3D PRISMS
!    =====================
!
      DO IP = 1 , NP
         UTILD(IP) =
     &     U(IKLE(ELT(IP),1),ETA(IP))   * SHP(1,IP) * (1.D0-SHZ(IP))
     &   + U(IKLE(ELT(IP),2),ETA(IP))   * SHP(2,IP) * (1.D0-SHZ(IP))
     &   + U(IKLE(ELT(IP),3),ETA(IP))   * SHP(3,IP) * (1.D0-SHZ(IP))
     &   + U(IKLE(ELT(IP),1),ETA(IP)+1) * SHP(1,IP) * SHZ(IP)
     &   + U(IKLE(ELT(IP),2),ETA(IP)+1) * SHP(2,IP) * SHZ(IP)
     &   + U(IKLE(ELT(IP),3),ETA(IP)+1) * SHP(3,IP) * SHZ(IP)
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,11) IELM
        IF(LNG.EQ.2) WRITE(LU,12) IELM
11      FORMAT(1X,'INTERP : TYPE D''ELEMENT INCONNU : ',I6)
12      FORMAT(1X,'INTERP : UNKNOWN TYPE OF ELEMENT : ',I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
