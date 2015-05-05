!                    *************************
                     SUBROUTINE READ_MESH_COORD
!                    *************************
!
     &(FFORMAT,NFIC,X,Y,NPOIN,PROJECTION,LATI0,LONGI0,Z)
!
!***********************************************************************
! HERMES   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    Reads the coordinates in the geometry file.
!+        Latitude-longitude coordinates transformed into mercator.
!
!history  J-M HERVOUET (LNH)     
!+        29/04/04
!+        V5P5
!+   First version.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT FOR GEOMETRY FILE
!| NFIC           |-->| LOGICAL UNIT FOR GEOMETRY FILE
!| X              |<--| X COORDINATES
!| Y              |<--| Y COORDINATES
!| PROJECTION     |<--| TYPE OF PROJECTION
!| LATI0          |<--| LATITUDE
!| LONGI0         |<--| LONGITUDE
!| Z              |<--| Z COORDINATES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPOIN,NFIC
      DOUBLE PRECISION, INTENT(OUT) :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(OUT), OPTIONAL :: Z(NPOIN)
      INTEGER, INTENT(INOUT)        :: PROJECTION
      DOUBLE PRECISION, INTENT(IN)  :: LATI0,LONGI0
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: IERR, I, NDIM
!
      REAL, ALLOCATABLE :: RB(:)
      DOUBLE PRECISION, PARAMETER :: R=6.37D6
      DOUBLE PRECISION PS4,TAN1,TAN2,LONGIRAD
!
      INTRINSIC LOG,TAN,ATAN
!
!-----------------------------------------------------------------------
!
!     total number of dimension
      NDIM = 2
      IF (PRESENT(Z)) NDIM = 3

      CALL GET_MESH_COORD(FFORMAT,NFIC,1,NDIM,NPOIN,X,IERR)
      IF(IERR.NE.0) THEN
        IF (LNG.EQ.1) WRITE(LU,*) 'ERREUR LORS DE LA LECTURE DE X'
        IF (LNG.EQ.2) WRITE(LU,*) 'ERROR WHILE READING X ARRAY'
        CALL PLANTE(1)
      ENDIF
      CALL GET_MESH_COORD(FFORMAT,NFIC,2,NDIM,NPOIN,Y,IERR)
      IF(IERR.NE.0) THEN
        IF (LNG.EQ.1) WRITE(LU,*) 'ERREUR LORS DE LA LECTURE DE Y'
        IF (LNG.EQ.2) WRITE(LU,*) 'ERROR WHILE READING Y ARRAY'
        CALL PLANTE(1)
      ENDIF
      IF(PRESENT(Z)) THEN
        CALL GET_MESH_COORD(FFORMAT,NFIC,3,NDIM,NPOIN,Z,IERR)
        IF(IERR.NE.0) THEN
          IF (LNG.EQ.1) WRITE(LU,*) 'ERREUR LORS DE LA LECTURE DE Z'
          IF (LNG.EQ.2) WRITE(LU,*) 'ERROR WHILE READING Z ARRAY'
          CALL PLANTE(1)
        ENDIF
      ENDIF
!
      CALL CORRXY(X,Y,NPOIN)
!
!     LATITUDE-LONGITUDE COORDINATES (DEGREES) 
!     CHANGED INTO MERCATOR PROJECTION
!
      IF(PROJECTION.EQ.3) THEN
        PS4=ATAN(1.D0)
        LONGIRAD=LONGI0*PS4/90.D0
        TAN2=TAN(LATI0*PS4/180.D0+PS4)
        IF(TAN2.LT.0.D0) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'LATI0=',LATI0,' EST PROBABLEMENT FAUSSE'
          ENDIF 
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'LATI0=',LATI0,' IS PROBABLY WRONG'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        DO I=1,NPOIN
          X(I)=R*(X(I)*PS4/90.D0-LONGIRAD)
          TAN1=TAN(Y(I)*PS4/180.D0+PS4)
          IF(TAN1.LT.0.D0) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'LA LATITUDE DOIT ETRE DONNEE EN DEGRES'
              WRITE(LU,*) 'ICI Y(I)=',Y(I),' POUR I=',I
              WRITE(LU,*) 'UTILISEZ CORRXY (BIEF) POUR LA CONVERSION'
            ENDIF 
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'LATITUDE MUST BE GIVEN IN DEGREES'
              WRITE(LU,*) 'HERE Y(I)=',Y(I),' FOR I=',I
              WRITE(LU,*) 'USE CORRXY (BIEF) FOR CONVERSION'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          Y(I)=R*(LOG(TAN1)-LOG(TAN2))
        ENDDO 
!       NOW IT IS MERCATOR
        PROJECTION=2
        WRITE(LU,*) ' '
        WRITE(LU,*) 'REAGEO3 :' 
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'COORDONNEES CHANGEES EN PROJECTION DE MERCATOR'
          WRITE(LU,*) 'AVEC LATITUDE DU POINT ORIGINE = ',LATI0
          WRITE(LU,*) 'ET  LONGITUDE DU POINT ORIGINE = ',LONGI0
        ENDIF 
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'COORDINATES CHANGED INTO MERCATOR PROJECTION'
          WRITE(LU,*) 'WITH LATITUDE OF ORIGIN POINT = ',LATI0
          WRITE(LU,*) 'AND LONGITUDE OF ORIGIN POINT = ',LONGI0
        ENDIF
        WRITE(LU,*) ' '      
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
