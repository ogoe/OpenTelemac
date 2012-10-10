!                    **********************
                     SUBROUTINE BIEF_INTERP
!                    **********************
!
     &( U , UTILD , SHP , NDP , SHZ , ETA , ELT , NP , NPOIN2 , NPLAN ,
     &  IELM , IKLE , NELMAX , PERIODIC )
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
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
!history  J-M HERVOUET (LNHE)
!+        20/06/2012
!+        V6P2
!+   Adding Quasi-bubble interpolation
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
      USE BIEF, EX_BIEF_INTERP => BIEF_INTERP
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
      LOGICAL, OPTIONAL, INTENT(IN)   :: PERIODIC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP,ETAP1
      DOUBLE PRECISION SHP11,SHP12,SHP14
      DOUBLE PRECISION SHP22,SHP23,SHP24
      DOUBLE PRECISION SHP33,SHP31,SHP34
      LOGICAL PERIO
!
!     SHOULD BE SAME EPSILO THAN SCHAR11
      DOUBLE PRECISION EPSILO
      DATA EPSILO / 1.D-6 /
!
      IF(PRESENT(PERIODIC)) THEN
        PERIO=PERIODIC
      ELSE
        PERIO=.FALSE.      
      ENDIF 
!
!-----------------------------------------------------------------------
!
!     TO BE IMPLEMENTED FOR QUASI-BUBBLE.... AN APPROXIMATION HERE
!
      IF(IELM.EQ.11) THEN
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
      ELSEIF(IELM.EQ.12) THEN
!
!    QUASI-BUBBLE TRIANGLES
!    ======================
!
      DO IP = 1 , NP
!
!        BARYCENTRIC COORDINATES OF SUB-TRIANGLES AS A FUNCTION OF
!        BARYCENTRIC COORDINATES OF THE ORIGINAL TRIANGLE
!        (A NICE GEOMETRY PROBLEM...)
!
         SHP11=SHP(1,IP)-SHP(3,IP)
         SHP12=SHP(2,IP)-SHP(3,IP)
         SHP14=3.D0*SHP(3,IP)
         SHP22=SHP(2,IP)-SHP(1,IP)
         SHP23=SHP(3,IP)-SHP(1,IP)
         SHP24=3.D0*SHP(1,IP)
         SHP33=SHP(3,IP)-SHP(2,IP)
         SHP31=SHP(1,IP)-SHP(2,IP)
         SHP34=3.D0*SHP(2,IP)
!
!        FINDING IN WHICH SUB-TRIANGLE WE ARE
!
!        IF(     SHP11.GT.0.D0 .AND. SHP11.LT.1.D0 .AND.
!    &           SHP12.GT.0.D0 .AND. SHP12.LT.1.D0 .AND.
!    &           SHP14.GT.0.D0 .AND. SHP14.LT.1.D0 ) THEN
!          UTILD(IP) = U(IKLE(ELT(IP),1),1) * SHP11
!    &               + U(IKLE(ELT(IP),2),1) * SHP12
!    &               + U(IKLE(ELT(IP),4),1) * SHP14
!        ELSEIF( SHP22.GT.0.D0 .AND. SHP22.LT.1.D0 .AND.
!    &           SHP23.GT.0.D0 .AND. SHP23.LT.1.D0 .AND.
!    &           SHP24.GT.0.D0 .AND. SHP24.LT.1.D0 ) THEN
!          UTILD(IP) = U(IKLE(ELT(IP),2),1) * SHP22
!    &               + U(IKLE(ELT(IP),3),1) * SHP23
!    &               + U(IKLE(ELT(IP),4),1) * SHP24
!        ELSEIF( SHP33.GT.0.D0 .AND. SHP33.LT.1.D0 .AND.
!    &           SHP31.GT.0.D0 .AND. SHP31.LT.1.D0 .AND.
!    &           SHP34.GT.0.D0 .AND. SHP34.LT.1.D0 ) THEN
!          UTILD(IP) = U(IKLE(ELT(IP),3),1) * SHP33
!    &               + U(IKLE(ELT(IP),1),1) * SHP31
!    &               + U(IKLE(ELT(IP),4),1) * SHP34
!
!        OPTIMISED VERSION WITH TRUNCATION ERRORS
!        SHP14, SHP24 AND SHP34 POSITIVITY ALREADY ENSURED
!
         IF(     SHP11.GT.    -2.D0*EPSILO .AND. 
     &           SHP11.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP12.GT.    -2.D0*EPSILO .AND. 
     &           SHP12.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP14.LT.1.D0+4.D0*EPSILO ) THEN
           UTILD(IP) = U(IKLE(ELT(IP),1),1) * SHP11
     &               + U(IKLE(ELT(IP),2),1) * SHP12
     &               + U(IKLE(ELT(IP),4),1) * SHP14
         ELSEIF( SHP22.GT.    -2.D0*EPSILO .AND. 
     &           SHP22.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP23.GT.    -2.D0*EPSILO .AND. 
     &           SHP23.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP24.LT.1.D0+4.D0*EPSILO ) THEN
           UTILD(IP) = U(IKLE(ELT(IP),2),1) * SHP22
     &               + U(IKLE(ELT(IP),3),1) * SHP23
     &               + U(IKLE(ELT(IP),4),1) * SHP24
         ELSEIF( SHP33.GT.    -2.D0*EPSILO .AND. 
     &           SHP33.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP31.GT.    -2.D0*EPSILO .AND. 
     &           SHP31.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP34.LT.1.D0+4.D0*EPSILO ) THEN
           UTILD(IP) = U(IKLE(ELT(IP),3),1) * SHP33
     &               + U(IKLE(ELT(IP),1),1) * SHP31
     &               + U(IKLE(ELT(IP),4),1) * SHP34
!
!        THE FOLLOWING CASE MAY HAPPEN IN PARALLEL
!        BECAUSE EVEN LOST CHARACTERISTICS ARE INTERPOLATED
!        AT GENERATION 0
!
         ELSEIF(NCSIZE.EQ.0) THEN
           WRITE(LU,*) 'INTERP: POINT ',IP,' NOT IN ELEMENT ',ELT(IP)
           WRITE(LU,*) 'SHP(1,IP)=',SHP(1,IP)
           WRITE(LU,*) 'SHP(2,IP)=',SHP(2,IP)
           WRITE(LU,*) 'SHP(3,IP)=',SHP(3,IP)
           WRITE(LU,*) 'EPSILO=',EPSILO,' IPID=',IPID
           CALL PLANTE(1)
           STOP
         ENDIF
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
      IF(PERIO) THEN      
        DO IP = 1 , NP
          IF(ETA(IP).EQ.NPLAN) THEN
            ETAP1=1
          ELSE
            ETAP1=ETA(IP)+1
          ENDIF
          UTILD(IP) =
     &     ( U(IKLE(ELT(IP),1),ETA(IP)) * SHP(1,IP) 
     &     + U(IKLE(ELT(IP),2),ETA(IP)) * SHP(2,IP) 
     &     + U(IKLE(ELT(IP),3),ETA(IP)) * SHP(3,IP) ) * (1.D0-SHZ(IP))
     &   + ( U(IKLE(ELT(IP),1),ETAP1)   * SHP(1,IP) 
     &     + U(IKLE(ELT(IP),2),ETAP1)   * SHP(2,IP) 
     &     + U(IKLE(ELT(IP),3),ETAP1)   * SHP(3,IP) ) * SHZ(IP)
        ENDDO
      ELSE
        DO IP = 1 , NP
          UTILD(IP) =
     &      U(IKLE(ELT(IP),1),ETA(IP))   * SHP(1,IP) * (1.D0-SHZ(IP))
     &    + U(IKLE(ELT(IP),2),ETA(IP))   * SHP(2,IP) * (1.D0-SHZ(IP))
     &    + U(IKLE(ELT(IP),3),ETA(IP))   * SHP(3,IP) * (1.D0-SHZ(IP))
     &    + U(IKLE(ELT(IP),1),ETA(IP)+1) * SHP(1,IP) * SHZ(IP)
     &    + U(IKLE(ELT(IP),2),ETA(IP)+1) * SHP(2,IP) * SHZ(IP)
     &    + U(IKLE(ELT(IP),3),ETA(IP)+1) * SHP(3,IP) * SHZ(IP)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,11) IELM
        IF(LNG.EQ.2) WRITE(LU,12) IELM
11      FORMAT(1X,'BIEF_INTERP : TYPE D''ELEMENT INCONNU : ',I6)
12      FORMAT(1X,'BIEF_INTERP : UNKNOWN TYPE OF ELEMENT : ',I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
