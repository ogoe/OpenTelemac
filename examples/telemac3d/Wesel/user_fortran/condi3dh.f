!                    *******************
                     SUBROUTINE CONDI3DH
!                    *******************
!
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    INITIALISES DEPTH
!
!history  C.-T. PHAM (LNHE)
!+        24/03/2017
!+        V7P3
!+   Creation from not splitted CONDIM
!+   Called by CONDIM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_CONDI3DH => CONDI3DH
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE TPXO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I,NSEC,NFO1
!
!-----------------------------------------------------------------------
!
!     INITIALISES H, THE WATER DEPTH
!
      IF(.NOT.SUIT2) THEN
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=COTINI)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=HAUTIN)
      ELSEIF(CDTINI(1:25).EQ.'ALTIMETRIE SATELLITE TPXO'.OR.
     &       CDTINI(1:24).EQ.'TPXO SATELLITE ALTIMETRY') THEN
        CALL OS('X=-Y    ',X=H,Y=ZF)
        CALL CONDI_TPXO(NPOIN2,MESH2D%NPTFR,MESH2D%NBOR%I,
     &                  X2%R,Y2%R,H%R,U2D%R,V2D%R,
     &                  LIHBOR%I,LIUBOL%I,KENT,KENTU,
     &                  GEOSYST,NUMZONE,LATIT,LONGIT,
     &                  T3D_FILES,T3DBB1,T3DBB2,
     &                  MARDAT,MARTIM,INTMICON,MSL,
     &                  TIDALTYPE,BOUNDARY_COLOUR,ICALHWG)
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!     USER INPUT :
!     PROGRAM HERE SPECIAL INITIAL CONDITIONS ON DEPTH
        NFO1=T3D_FILES(T3DFO1)%LU
!jaj free surface initialisation from a file and using surfini
!
        READ(NFO1,*)
        READ(NFO1,*) NSEC
        WRITE(LU,*) 'CONDIM: READING FREE SURFACE INITIALISATION FILE'
        WRITE(LU,*) 'CONDIM: NSEC = ',NSEC
        WRITE(LU,*) ' '
        WRITE(LU,'(5(1X,A15))') 'XLEFT','YLEFT','XRIGHT','YRIGHT',
     &                          'WATER_LEVEL'
        DO I=1,NSEC
          READ(NFO1,*) T3_01%R(I), T3_02%R(I), T3_04%R(I),
     &                 T3_05%R(I), T3_03%R(I)
          T3_06%R(I) = T3_03%R(I)
          WRITE(LU,'(5(1X,G15.6))') T3_01%R(I), T3_02%R(I), T3_04%R(I),
     &                              T3_05%R(I), T3_03%R(I)
        ENDDO
        WRITE(LU,*) ' '
!
        WRITE(LU,*) 'CONDIM: COTINI = ',COTINI
        CALL OS( 'X=C     ' , X=H, C=COTINI)
!
        CALL SURFINI(T3_01%R,T3_02%R,T3_03%R,T3_04%R,T3_05%R,T3_06%R,
     &               T3_07%R,T3_08%R,T3_09%R,MESH3D%X%R,MESH3D%Y%R,
     &               H%R,ZF%R,IT1%I,IT2%I,NSEC,NPOIN2)
!
        CALL OS( 'X=X-Y   ' , H , ZF , ZF , 0.D0 )
!     END OF SPECIAL INITIAL CONDITIONS
!     END OF USER INPUT
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIM : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIM: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'HAUTEUR LUE DANS LE FICHIER BINAIRE 1'
        IF(LNG.EQ.2) WRITE(LU,*) 'DEPTH IS READ IN THE BINARY FILE 1'
      ENDIF
!
!     CLIPS H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),0.D0)
      ENDDO
!
      CALL OS ('X=Y     ',X=HN,Y=H)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
