!                    *****************
                     SUBROUTINE CONWAC
!                    *****************
!
     &( CX    , CY    , CT    , XK    , CG    , COSF  , TGF   , DEPTH ,
     &  DZX   , DZY   , FREQ  , COSTET, SINTET, NPOIN2, NPLAN , JF    ,
     &  NF    , PROINF, SPHE  , PROMIN, TRA01 , TRA02 )
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE ADVECTION FIELD (3D WITHOUT CURRENT).
!
!warning  IN THIS CASE THE X AXIS IS VERTICAL ORIENTED UPWARDS AND
!+            THE Y AXIS IS HORIZONTAL ORIENTED TOWARDS THE RIGHT;
!+            TETA IS THE DIRECTION WRT NORTH, CLOCKWISE
!
!history  M. BENOIT (EDF LNHE)
!+        19/01/2004
!+        V5P4
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
!| CG             |-->| VITESSE DE GROUPE DISCRETISEE
!| COSF           |-->| COSINUS DES LATITUDES DES POINTS 2D
!| COSTET         |-->| COSINUS TETA
!| CX,CY,CT       |<--| CHAMP CONVECTEUR SELON X(OU PHI),
!|                |   | Y(OU LAMBDA) ET TETA
!| DEPTH          |-->| PROFONDEUR
!| DZX            |-->| GRADIENT DE FOND SELON X
!| DZY            |-->| GRADIENT DE FOND SELON Y
!| FREQ           |-->| FREQUENCES DISCRETISEES
!| JF             |-->| FREQUENCES COURANTE
!| NF             |-->| NOMBRE DE FREQUENCES
!| NPLAN          |-->| NOMBRE DE PLANS OU DE DIRECTIONS
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
!| PROINF         |-->| LOGIQUE INDIQUANT SI ON EST EN PROF INFINIE
!| PROMIN         |-->| VALEUR MINIMALE DE LA PROFONDEUR D'EAU
!| SINTET         |-->| SINUS TETA
!| SPHE           |-->| LOGIQUE INDIQUANT SI ON EST EN COORD. SPHER.
!| TGF            |-->| TANGENTES DES LATITUDES DES POINTS 2D
!| TRA01          |<->| TABLEAU DE TRAVAIL
!| TRA02          |<->| TABLEAU DE TRAVAIL
!| XK             |-->| NOMBRE D'ONDE DISCRETISE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER          NF    , NPLAN , NPOIN2, JF
      DOUBLE PRECISION PROMIN
      DOUBLE PRECISION DEPTH(NPOIN2) , DZX(NPOIN2)   , DZY(NPOIN2)
      DOUBLE PRECISION COSF(NPOIN2)  , TGF(NPOIN2)   , FREQ(NF)
      DOUBLE PRECISION COSTET(NPLAN) , SINTET(NPLAN)
      DOUBLE PRECISION TRA01(NPLAN)  , TRA02(NPLAN)
      DOUBLE PRECISION CG(NPOIN2,NF) , XK(NPOIN2,NF)
      DOUBLE PRECISION CX(NPOIN2,NPLAN),CY(NPOIN2,NPLAN)
      DOUBLE PRECISION CT(NPOIN2,NPLAN)
      LOGICAL          PROINF, SPHE
!
!.....LOCAL VARIABLES
!     """"""""""""""""""
      INTEGER          JP    , IP
      DOUBLE PRECISION GSQP  , SR    , R     , SRCF  , TFSR
      DOUBLE PRECISION DDDN  , DSDNSK, GRADEG, DEUKD , DEUPI
!
!
      GSQP=0.780654996D0
      R=6400.D3
      DEUPI=6.283185307D0
!
      IF (PROINF) THEN
!-----------------------------------------------------------------------
!     INFINITE WATER DEPTH ...
!-----------------------------------------------------------------------
!
        DO JP=1,NPLAN
          TRA01(JP)=GSQP/FREQ(JF)*COSTET(JP)
          TRA02(JP)=GSQP/FREQ(JF)*SINTET(JP)
        ENDDO
!
        IF (.NOT.SPHE) THEN
!       ----------------------------------------------------------------
!       ... AND IN CARTESIAN COORDINATE SYSTEM
!       ----------------------------------------------------------------
          DO IP=1,NPOIN2
            DO JP=1,NPLAN
              CX(IP,JP)=TRA01(JP)
              CY(IP,JP)=TRA02(JP)
              CT(IP,JP)=0.D0
            ENDDO
          ENDDO
!
        ELSE
!       ----------------------------------------------------------------
!       ... AND IN SPHERICAL COORDINATE SYSTEM
!       ----------------------------------------------------------------
          SR=1.D0/R
          GRADEG=180.D0/3.1415926D0
          DO IP=1,NPOIN2
            SRCF=SR/COSF(IP)
            TFSR=TGF(IP)*SR
            DO JP=1,NPLAN
              CX(IP,JP)=TRA01(JP)*SR*GRADEG
              CY(IP,JP)=TRA02(JP)*SRCF*GRADEG
              CT(IP,JP)=TRA02(JP)*TFSR
            ENDDO
          ENDDO
!
        ENDIF
!
!
      ELSE
!-----------------------------------------------------------------------
!     FINITE WATER DEPTH ....
!-----------------------------------------------------------------------
!
        IF (.NOT.SPHE) THEN
!       ----------------------------------------------------------------
!       ... AND IN CARTESIAN COORDINATE SYSTEM
!       ----------------------------------------------------------------
          DO IP=1,NPOIN2
            IF (DEPTH(IP).GT.PROMIN) THEN
              DO JP=1,NPLAN
                DDDN=-SINTET(JP)*DZX(IP)+COSTET(JP)*DZY(IP)
                CX(IP,JP)=CG(IP,JF)*COSTET(JP)
                CY(IP,JP)=CG(IP,JF)*SINTET(JP)
                DEUKD=2.0D0*XK(IP,JF)*DEPTH(IP)
                IF (DEUKD.GT.7.0D2) THEN
                  DSDNSK=0.0D0
                ELSE
                  DSDNSK=DEUPI*FREQ(JF)/SINH(DEUKD)
                ENDIF
                CT(IP,JP)=-DSDNSK*DDDN
              ENDDO
            ELSE
              DO JP=1,NPLAN
                CX(IP,JP)=0.0D0
                CY(IP,JP)=0.0D0
                CT(IP,JP)=0.0D0
              ENDDO
            ENDIF
          ENDDO
!
        ELSE
!       ----------------------------------------------------------------
!       ... AND IN SPHERICAL COORDINATE SYSTEM
!       ----------------------------------------------------------------
          SR=1.D0/R
          GRADEG=180.D0/3.1415926D0
          DO IP=1,NPOIN2
            IF (DEPTH(IP).GT.PROMIN) THEN
              SRCF=SR/COSF(IP)
              TFSR=TGF(IP)*SR
              DO JP=1,NPLAN
                DDDN=-SINTET(JP)*DZX(IP)*SR+COSTET(JP)*DZY(IP)*SRCF
                CX(IP,JP)=(CG(IP,JF)*COSTET(JP))*SR*GRADEG
                CY(IP,JP)=(CG(IP,JF)*SINTET(JP))*SRCF*GRADEG
                DEUKD=2.D0*XK(IP,JF)*DEPTH(IP)
                IF (DEUKD.GT.7.D2) THEN
                  DSDNSK=0.D0
                ELSE
                  DSDNSK=DEUPI*FREQ(JF)/SINH(DEUKD)
                ENDIF
                CT(IP,JP)=CG(IP,JF)*SINTET(JP)*TFSR-DSDNSK*DDDN*GRADEG
              ENDDO
            ELSE
              DO JP=1,NPLAN
                CX(IP,JP)=0.0D0
                CY(IP,JP)=0.0D0
                CT(IP,JP)=0.0D0
              ENDDO
            ENDIF
          ENDDO
!
        ENDIF
!
      ENDIF
!-----------------------------------------------------------------------
!
      RETURN
      END