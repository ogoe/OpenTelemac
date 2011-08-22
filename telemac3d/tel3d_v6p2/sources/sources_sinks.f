!                    ************************
                     SUBROUTINE SOURCES_SINKS
!                    ************************
!
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE SOURCE TERMS TO ADD IN 2D AND 3D
!+                CONTINUITY EQUATIONS.
!
!history  J-M HERVOUET (LNHE)
!+        07/04/2009
!+        V5P9
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,I,IP
!
!-----------------------------------------------------------------------
!
!     INITIALISING SMH
!
      CALL OS ('X=0     ',X=SMH)
!
!     SOURCES AND SINKS
!
      IF(NSCE.GT.0) THEN
!
!       HERE T3_02 LIKE VOLU, BUT CALL PARCOM (AND ZPROP INSTEAD OF Z)
        CALL VECTOR(T3_02,'=','MASBAS          ',IELM3,1.D0,SVIDE,
     &              SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &              MESH3D,.FALSE.,MASKEL)
        IF(NCSIZE.GT.1) CALL PARCOM(T3_02,2,MESH3D)
        CALL CPSTVC(T3_02,T3_03)
        DO IS=1,NSCE
          CALL OS('X=0     ',X=T3_03)
!         IN PARALLEL IF ISCE(IS)=0, THE POINT IS OUTSIDE THE SUBDOMAIN
          IF(ISCE(IS).GT.0) THEN
            I=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
            T3_03%R(I)=QSCE2(IS)/MAX(1.D-8,T3_02%R(I))
          ENDIF
          CALL VECTOR(SOURCES%ADR(IS)%P,'=','MASVEC          ',
     &                IELM3,1.D0,T3_03,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &                MESH3D,MSK,MASKEL)
        ENDDO
!       SUMS ON THE VERTICAL TO GET THE 2D SOURCES
        DO IS=1,NSCE
          DO IP=1,NPLAN
            DO I=1,NPOIN2
              SMH%R(I)=SMH%R(I)+SOURCES%ADR(IS)%P%R(I+NPOIN2*(IP-1))
            ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     RAIN AND EVAPORATION (NEGATIVE RAIN)
!
      IF(RAIN) THEN
!       PLUIE IS NON ASSEMBLED IN PARALLEL
        CALL OS('X=CY    ',X=PLUIE,Y=VOLU2D,C=RAIN_MMPD/86400000.D0)
        CALL OS('X=X+Y   ',X=SMH,Y=PLUIE)
        IF(NCSIZE.GT.1) THEN
!         USING V2DPAR AVOIDS A CALL PARCOM OF A COPY OF PLUIE
          CALL OS('X=CY    ',X=PARAPLUIE,Y=V2DPAR,
     &                       C=RAIN_MMPD/86400000.D0)
!       ELSE
!         PARAPLUIE%R=>PLUIE%R  ! DONE ONCE FOR ALL IN POINT_TELEMAC3D
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     PARALLELISM, REAL VALUES REQUIRED IN SOURCES FOR MURD3D
!     BUT BEWARE IN TRIDW2, PARCOM MUST NOT BE DONE TWICE ON SOURCES
!
!     12/06/2007 : VALUES WITHOUT PARCOM STORED IN ADDRESS IS+NSCE
!                  SIZE CHANGED ACCORDINGLY IN POINT_TELEMAC3D
!
      IF(NCSIZE.GT.1) THEN
        IF(NSCE.GT.0) THEN
          DO IS=1,NSCE
            CALL OS('X=Y     ',X=SOURCES%ADR(IS+NSCE)%P,
     &                         Y=SOURCES%ADR(IS     )%P)
            CALL PARCOM(SOURCES%ADR(IS)%P,2,MESH3D)
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
