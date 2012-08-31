!                    *****************
                     SUBROUTINE ANGLES
!                    *****************
!
     &( XLAMD , DTPLUS, DTMOIN)
!
!***********************************************************************
! TOMAWAC   V6P1                                   08/06/2011
!***********************************************************************
!
!brief    COMPUTES THE ANGLES OF RESONANT VECTORS IN THE CASE
!+                OF THE STANDARD INTERACTION CONFIGURATION - DIA METHOD
!+                PROPOSED BY HASSELMANN AND HASSELMANN (1985).
!+
!+
!+            PROCEDURE SPECIFIC TO THE CASE WHERE THE DIRECTIONS
!+                ARE EVENLY DISTRIBUTED OVER [0;2.PI].
!
!note     THE ANGLES DTPLUS AND DTMOIN ARE IN DEGREES AND ARE BOTH
!+          POSITIVE BY CONVENTION.
!
!reference  HASSELMANN S., HASSELMANN K. ET AL.(1985) :
!+                     "COMPUTATIONS AND PARAMETERIZATIONS OF THE NONLINEAR
!+                      ENERGY TRANSFER IN GRAVITY-WAVE SPECTRUM. PART2 :
!+                      PARAMETERIZATIONS OF THE NONLINEAR ENERGY TRANSFER
!+                      FOR APPLICATION IN WAVE MODELS". JPO, VOL 15, PP 1378-1391.
!
!history  M. BENOIT
!+        26/06/96
!+        V1P2
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
!history  G.MATTAROLO (EDF - LNHE)
!+        08/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DTMOIN         |<--| ANGLE ASSOCIATED TO THE FREQUENCY (1-XLAMD).FREQ
!| DTPLUS         |<--| ANGLE ASSOCIATED TO THE FREQUENCY (1+XLAMD).FREQ
!| XLAMD          |-->| DIA STANDARD CONFIGURATION LAMBDA COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!.....VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      DOUBLE PRECISION XLAMD , DTPLUS, DTMOIN
!
!.....LOCAL VARIABLES
!     """"""""""""""""""
      DOUBLE PRECISION CNVD  , AUX
!
!
!.....1. CHECKS THAT LAMBDA RANGES BETWEEN 0 AND 0.5.
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""
      IF ((XLAMD.LT.0.D0).OR.(XLAMD.GT.0.5D0)) THEN
        IF(LNG.EQ.1) THEN
           WRITE(LU,1001) XLAMD
        ELSE
           WRITE(LU,1002) XLAMD
        ENDIF
        STOP
      ENDIF
!
!.....2. COMPUTES TETA_PLUS (DTPLUS) AND TETA_MOINS (DTMOIN).
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""
      CNVD=180.D0/3.141592654D0
      AUX=2.D0*XLAMD*(1.D0+XLAMD*XLAMD)
      DTPLUS=ACOS( (1.D0+AUX)/(1.D0+XLAMD)**2 )*CNVD
      DTMOIN=ACOS( (1.D0-AUX)/(1.D0-XLAMD)**2 )*CNVD
!
!
 1001 FORMAT('/!/-----------------------------------------------/!/'/
     &       '/!/  ARRET DU PROGRAMME DANS SUBROUTINE ANGLES    /!/'/
     &       '/!/-----------------------------------------------/!/'/
     &       '/!/  LA VALEUR CHOISIE POUR LE PARAMETRE LAMBDA   /!/'/
     &       '/!/  INTERVENANT DANS LA METHODE DISCRETE INTER-  /!/'/
     &       '/!/  ACTION APPROXIMATION DOIT ETRE INCLUSE DANS  /!/'/
     &       '/!/  L INTERVALLE [ 0. ; 0.5 ].                   /!/'/
     &       '/!/  OR LA VALEUR UTILISEE EST : ', G10.4,'       /!/'/
     &       '/!/-----------------------------------------------/!/')
!
 1002 FORMAT('/!/-----------------------------------------------/!/'/
     &       '/!/        PROGRAM STOP IN SUBROUTINE ANGLES      /!/'/
     &       '/!/-----------------------------------------------/!/'/
     &       '/!/       THE VALUE OF THE LAMBDA PARAMETER       /!/'/
     &       '/!/             USED IN THE DIA METHOD            /!/'/
     &       '/!/  MUST BE INCLUDED IN THE INTERVAL [ 0. ; 0.5 ]/!/'/
     &       '/!/  THE VALUE HERE IMPOSED IS : ', G10.4,'       /!/'/
     &       '/!/-----------------------------------------------/!/')
!
      RETURN
      END
