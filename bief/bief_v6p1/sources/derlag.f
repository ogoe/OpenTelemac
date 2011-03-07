!                    *****************
                     SUBROUTINE DERLAG
!                    *****************
!
     &( U , V , DT , X , Y , IKLE , IFABOR , LT , IELM , NDP , NPOIN ,
     &  NELEM , NELMAX , SURDET , XLAG , YLAG , DX , DY ,
     &  NSP , SHPLAG , DEBLAG , FINLAG , ELTLAG , NLAG , RESUX , RESUY ,
     &  NBOR , NELBOR , NULONE , NPTFR , MSK,MASKEL,MASKPT,T8)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    - SETS THE BARYCENTRIC COORDINATES IN THE MESH,
!+                  AT THE START OF COMPUTATION FOR EACH FLOAT.
!+
!+            - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
!+                 (SUBSEQUENT TIMESTEPS).
!
!history  J-M JANIN (LNH)
!+        02/09/08
!+        V5P9
!+   CALLS GTSH11 INSTEAD OF GTSHP11
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
!| DEBLAG         |-->| NUMEROS DES PAS DE TEMPS DE DEBUT DE CALCUL
!|                |   | DES DERIVES.
!| DT             |-->| PAS DE TEMPS.
!| DX,DY          |---| STOCKAGE DES SOUS-PAS .
!| ELTLAG         |<->| NUMEROS DES ELEMENTS DANS LESQUELS SE TROUVE
!|                |   | A CET INSTANT CHACUNE DES DERVIES.
!| FINLAG         |-->| NUMEROS DES PAS DE TEMPS DE FIN DE CALCUL DES
!|                |   | DERIVES.
!| IELM           |-->| TYPE DE MAILLAGE.
!| IFABOR         |-->| NUMEROS DES ELEMENTS AYANT UNE FACE COMMUNE
!|                |   | AVEC L'ELEMENT .  SI IFABOR
!|                |   | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
!| IKLE           |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!|                |   | ET GLOBALE.
!| LT             |-->| NUMERO DU PAS DE TEMPS
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
!| MASKPT         |-->| TABLEAU DE MASQUAGE DES POINTS.
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD.
!| NDP            |-->| NOMBRE DE POINTS PAR ELEMENT
!| NELBOR         |-->| NUMERO DE L'ELEMENT ADJACENT AU K IEME
!|                |   | SEGMENT DE BORD.
!| NELEM          |-->| NOMBRE D'ELEMENTS.
!| NELMAX         |-->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
!| NLAG           |-->| NOMBRE DE DERIVES.
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
!| NSP            |---| NOMBRE DE SOUS PAS DE RUNGE KUTTA.
!| NULONE         |-->| NUMERO LOCAL D'UN POINT DE BORD DANS
!|                |   | L'ELEMENT ADJACENT DONNE PAR NELBOR.
!| RESUX,RESUY    |<--| RESULTAT POUR ECRITURE SUR FICHIER DE LA
!|                |   | DERNIERE DERIVE ACHEVEE.
!| SHPLAG         |<->| COORDONNEES BARYCENTRIQUES INSTANTANNEES DES
!|                |   | DERIVES DANS LEURS ELEMENTS RESPECTIFS.
!| SURDET         |-->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
!| T8             |---|
!| U,V            |-->| COMPOSANTE DE LA VITESSE
!| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE.
!| XLAG,YLAG      |<->| POSITIONS INSTANTANNEES DES DERIVES.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF   !, EX_DERLAG => DERLAG
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,LT,IELM,NDP,NELEM,NLAG
      INTEGER         , INTENT(IN)    :: NPTFR,NELMAX
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),DT
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      INTEGER         , INTENT(IN)    :: IFABOR(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: XLAG(NPOIN,NLAG)
      DOUBLE PRECISION, INTENT(INOUT) :: YLAG(NPOIN,NLAG)
      INTEGER         , INTENT(INOUT) :: DEBLAG(NLAG),FINLAG(NLAG)
      INTEGER         , INTENT(INOUT) :: ELTLAG(NPOIN,NLAG)
      DOUBLE PRECISION, INTENT(INOUT) :: T8(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPOIN),DY(NPOIN)
      INTEGER         , INTENT(INOUT) :: NSP(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: RESUX(NPOIN),RESUY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPLAG(NDP,NPOIN,NLAG)
      INTEGER         , INTENT(IN)    :: NBOR(NPTFR),NELBOR(NPTFR)
      INTEGER         , INTENT(IN)    :: NULONE(NPTFR)
      LOGICAL         , INTENT(IN)    :: MSK
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELMAX),MASKPT(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ILAG,JLAG,LTT,NRK,IPOIN
!
      DOUBLE PRECISION Z(1),C
!
!-----------------------------------------------------------------------
!
      DO 10 ILAG=1,NLAG
!
        IF(LT.EQ.DEBLAG(ILAG)) THEN
!
!-----------------------------------------------------------------------
!
!   - SETS THE BARYCENTRIC COORDINATES IN THE MESH , AT THE START
!     OF COMPUTATION FOR EACH FLOAT
!
!-----------------------------------------------------------------------
!
          CALL OV( 'X=CY    ' , XLAG(1,ILAG) , U , Z , -1.D0 , NPOIN )
          CALL OV( 'X=CY    ' , YLAG(1,ILAG) , V , Z , -1.D0 , NPOIN )
!
          IF(IELM.EQ.11) THEN
!
!  P1 TRIANGLES
!  ============
!
!      FILLS THE SHP AND ELT (OPTIMISED)
!
            CALL GTSH11(XLAG(1,ILAG),YLAG(1,ILAG),X,Y,SHPLAG(1,1,ILAG),
     &                  ELTLAG(1,ILAG),IKLE,NSP,NSP,NPOIN,
     &                  NELEM,NELMAX,1,MSK,MASKEL)
!
          ELSE
           IF(LNG.EQ.1) THEN
             WRITE(LU,*) IELM,' : ELEMENT NON PREVU DANS DERLAG'
           ENDIF
           IF(LNG.EQ.2) THEN
             WRITE(LU,*) IELM,': ELEMENT NOT IMPLEMENTED IN DERLAG'
           ENDIF
           STOP
          ENDIF
!
          CALL OV( 'X=Y     ' , XLAG(1,ILAG) , X , Z , C , NPOIN )
          CALL OV( 'X=Y     ' , YLAG(1,ILAG) , Y , Z , C , NPOIN )
!
        ELSEIF(LT.GT.DEBLAG(ILAG).AND.LT.LE.FINLAG(ILAG)) THEN
!
!-----------------------------------------------------------------------
!
!   - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
!     (SUBSEQUENT TIMESTEPS)
!
!-----------------------------------------------------------------------
!
! NUMBER OF RUNGE-KUTTA SUB-STEPS, BY CROSSED ELEMENT
! ======================================================
!
          NRK     =  3
!
!  P1 TRIANGLES
!  ============
!
          CALL CHAR11( U , V , DT , NRK , X , Y , IKLE , IFABOR ,
     &                 XLAG(1,ILAG) , YLAG(1,ILAG) , DX , DY ,
     &                 SHPLAG(1,1,ILAG) , ELTLAG(1,ILAG) , NSP ,
     &                 NPOIN , NPOIN , NELEM , NELMAX , SURDET , 1 ,T8)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
!   - CANCELS THE FLOATS LEAVING THE DOMAIN
!
!-----------------------------------------------------------------------
!
        IF(LT.EQ.FINLAG(ILAG)) THEN
          DO IPOIN=1,NPOIN
            IF(ELTLAG(IPOIN,ILAG).LE.0) THEN
              XLAG(IPOIN,ILAG) = X(IPOIN)
              YLAG(IPOIN,ILAG) = Y(IPOIN)
            ENDIF
          ENDDO
        ENDIF
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
!   - STORAGE FOR RESULTS OUTPUT OF THE LAST COMPUTED FLOAT
!
!-----------------------------------------------------------------------
!
      CALL OV( 'X=C     ' , RESUX , Y , Z , 0.D0 , NPOIN )
      CALL OV( 'X=C     ' , RESUY , Y , Z , 0.D0 , NPOIN )
      LTT=0
      JLAG=1
      DO ILAG=1,NLAG
        IF(FINLAG(ILAG).GT.LTT.AND.FINLAG(ILAG).LE.LT) THEN
          LTT=FINLAG(ILAG)
          JLAG=ILAG
        ENDIF
      ENDDO
      IF(LTT.NE.0) THEN
        CALL OV( 'X=Y-Z   ' , RESUX , XLAG(1,JLAG) , X , C , NPOIN )
        CALL OV( 'X=Y-Z   ' , RESUY , YLAG(1,JLAG) , Y , C , NPOIN )
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END