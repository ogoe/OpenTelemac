C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MANAGES THE COMPUTATION OF DISCHARGES AND
!>                DETERMINES BOUNDARY CONDITIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHESTR, GRAV, H, IOPTAN, KARMAN, LIHBOR, LITBOR, LIUBOR, LIVBOR, NBOR, NKFROT, NPSING, NPSMAX, NTRAC, NUMDIG, NWEIRS, PHIDIG, T, TBOR, UBOR, UNORM, VBOR, X, Y, ZDIG, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> HMIN, I, IA, IB, N, NA, NB, PHI, QAB, YAA, YBB, YDEN, YS
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CLSING
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CLHUVT(), LOIDEN(), LOINOY()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.6                                       </center>
!> </td><td> 23/11/2005
!> </td><td> J.-M. HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <td><center>                                           </center>
!> </td><td> 19/04/1996
!> </td><td> V. GUINOT (LHF)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHESTR
!></td><td>--></td><td>COEFFICIENTS DE FROTTEMENT SUR LE FOND.
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITE.
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR AU PAS DE TEMPS COURANT.
!>    </td></tr>
!>          <tr><td>IOPTAN
!></td><td>--></td><td>OPTION DE CALCUL DES VITESSES TANGENTIELLES.
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>CONSTANTE DE KARMAN.
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LOI DE FROTTEMENT SUR LE FOND.
!>    </td></tr>
!>          <tr><td>LIHBOR(J)
!></td><td><--</td><td>TYPE DE LA CONDITION EN HAUTEUR AU
!>                  J-EME POINT LIMITE
!>    </td></tr>
!>          <tr><td>LITBOR(J)
!></td><td><--</td><td>TYPE DE LA CONDITION EN TRACEUR AU
!>                  J-EME POINT LIMITE
!>    </td></tr>
!>          <tr><td>LIUBOR(J)
!></td><td><--</td><td>TYPE DE LA CONDITION EN VITESSE U AU
!>                  J-EME POINT LIMITE
!>    </td></tr>
!>          <tr><td>LIVBOR(J)
!></td><td><--</td><td>TYPE DE LA CONDITION EN VITESSE V AU
!>                  J-EME POINT LIMITE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROTATION GLOBALE DES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>NKFROT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPSING(N)
!></td><td>--></td><td>NOMBRE DE POINTS DE CHAQUE COTE DE LA
!>                  SINGULARITE N.
!>    </td></tr>
!>          <tr><td>NPSMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE POINTS POUR UN COTE D'UNE
!>                  SINGULARITE.
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NUMDIG(K,N,I)
!></td><td>--></td><td>NUMERO DES POINTS DES DIGUES
!>                  DANS LA NUMEROTATION DES POINTS DE BORD
!>                  DES CONDITIONS AUX LIMITES) DU I-EME
!>                  POINT SUR LE COTE K DE L'OUVRAGE N
!>    </td></tr>
!>          <tr><td>NWEIRS
!></td><td>--></td><td>NOMBRE DE SINGULARITES LINEIQUES.
!>    </td></tr>
!>          <tr><td>PHIDIG(N,I)
!></td><td>--></td><td>COEFFICIENT DE DEBIT AU I-EME POINT DE
!>                  LA N-IEME SINGULARITE
!>    </td></tr>
!>          <tr><td>T
!></td><td>--></td><td>TRACEUR AU PAS DE TEMPS COURANT.
!>    </td></tr>
!>          <tr><td>TBOR(J)
!></td><td><--</td><td>VALEUR DE LA CONDITION EN TRACEUR AU
!>                  J-EME POINT LIMITE
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>SI OUI, IL Y A UN TRACEUR.
!>    </td></tr>
!>          <tr><td>UBOR(J)
!></td><td><--</td><td>VALEUR DE LA CONDITION EN VIESSE U AU
!>                  J-EME POINT LIMITE
!>    </td></tr>
!>          <tr><td>UNORM (J)
!></td><td>--></td><td>VITESSE NORMALE A LA SOUS-ITERATION COURANTE
!>    </td></tr>
!>          <tr><td>VBOR(J)
!></td><td><--</td><td>VALEUR DE LA CONDITION EN VITESSE V A
!>                  J-EME POINT LIMITE
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES NOUEDS.
!>    </td></tr>
!>          <tr><td>ZDIG(N,I)
!></td><td>--></td><td>COTE DE LA DIGUE AU I-EME POINT DE
!>                  LA N-IEME SINGULARITE
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FOND.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CLSING
     &(NWEIRS,NPSING,NPSMAX,NUMDIG,X,Y,ZF,CHESTR,NKFROT,KARMAN,
     & ZDIG,PHIDIG,NBOR,H,T,NTRAC,IOPTAN,UNORM,
     & UBOR,VBOR,TBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR,GRAV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHESTR         |-->| COEFFICIENTS DE FROTTEMENT SUR LE FOND.
C| GRAV           |-->| GRAVITE.
C| H             |-->| HAUTEUR AU PAS DE TEMPS COURANT.
C| IOPTAN         |-->| OPTION DE CALCUL DES VITESSES TANGENTIELLES.
C| KARMAN         |-->| CONSTANTE DE KARMAN.
C| KFROT          |-->| LOI DE FROTTEMENT SUR LE FOND.
C| LIHBOR(J)      |<--| TYPE DE LA CONDITION EN HAUTEUR AU
C|                |   | J-EME POINT LIMITE
C| LITBOR(J)      |<--| TYPE DE LA CONDITION EN TRACEUR AU
C|                |   | J-EME POINT LIMITE
C| LIUBOR(J)      |<--| TYPE DE LA CONDITION EN VITESSE U AU
C|                |   | J-EME POINT LIMITE
C| LIVBOR(J)      |<--| TYPE DE LA CONDITION EN VITESSE V AU
C|                |   | J-EME POINT LIMITE
C| NBOR           |-->| NUMEROTATION GLOBALE DES POINTS DE BORD.
C| NKFROT         |---| 
C| NPSING(N)      |-->| NOMBRE DE POINTS DE CHAQUE COTE DE LA
C|                |   | SINGULARITE N.
C| NPSMAX         |-->| NOMBRE MAXIMUM DE POINTS POUR UN COTE D'UNE
C|                |   | SINGULARITE.
C| NTRAC          |---| 
C| NUMDIG(K,N,I)  |-->| NUMERO DES POINTS DES DIGUES
C|                |   | DANS LA NUMEROTATION DES POINTS DE BORD
C|                |   | DES CONDITIONS AUX LIMITES) DU I-EME
C|                |   | POINT SUR LE COTE K DE L'OUVRAGE N
C| NWEIRS         |-->| NOMBRE DE SINGULARITES LINEIQUES.
C| PHIDIG(N,I)    |-->| COEFFICIENT DE DEBIT AU I-EME POINT DE
C|                |   | LA N-IEME SINGULARITE
C| T             |-->| TRACEUR AU PAS DE TEMPS COURANT.
C| TBOR(J)        |<--| VALEUR DE LA CONDITION EN TRACEUR AU
C|                |   | J-EME POINT LIMITE
C| TRAC           |-->| SI OUI, IL Y A UN TRACEUR.
C| UBOR(J)        |<--| VALEUR DE LA CONDITION EN VIESSE U AU
C|                |   | J-EME POINT LIMITE
C| UNORM (J)      |-->| VITESSE NORMALE A LA SOUS-ITERATION COURANTE
C| VBOR(J)        |<--| VALEUR DE LA CONDITION EN VITESSE V A
C|                |   | J-EME POINT LIMITE
C| X,Y            |-->| COORDONNEES DES NOUEDS.
C| ZDIG(N,I)      |-->| COTE DE LA DIGUE AU I-EME POINT DE
C|                |   | LA N-IEME SINGULARITE
C| ZF             |-->| COTE DU FOND.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_CLSING => CLSING
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NWEIRS,NPSMAX,IOPTAN
      INTEGER, INTENT(IN) :: NKFROT(*),NBOR(*)
      INTEGER, INTENT(IN) :: NPSING(NWEIRS),NUMDIG(2,NWEIRS,NPSMAX)
      INTEGER, INTENT(INOUT) :: LIUBOR(*),LIVBOR(*),LIHBOR(*)
      INTEGER, INTENT(IN) :: NTRAC
      DOUBLE PRECISION, INTENT(IN) :: PHIDIG(NWEIRS,NPSMAX)
      DOUBLE PRECISION, INTENT(IN) :: ZDIG(NWEIRS,NPSMAX),H(*)
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),ZF(*),CHESTR(*)
      DOUBLE PRECISION, INTENT(IN) :: KARMAN,GRAV
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UNORM(*)
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR,LITBOR
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,N,IA,IB,NA,NB
C
      DOUBLE PRECISION HMIN,PHI,QAB,YAA,YBB,YDEN,YS
C
C-----------------------------------------------------------------------
C
      HMIN=1.D-3
C
C     COMPUTES UNIT DISCHARGES
C
      DO 10 N=1,NWEIRS
      DO 20 I=1,NPSING(N)
        IA=NUMDIG(1,N,I)
        IB=NUMDIG(2,N,I)
        NA=NBOR(IA)
        NB=NBOR(IB)
        YAA=H(NA)+ZF(NA)
        YBB=H(NB)+ZF(NB)
        YS=ZDIG(N,I)
        PHI=PHIDIG(N,I)
C
        IF(YAA.GT.YBB) THEN
C         CASE WHERE A IS UPSTREAM
          YDEN=YS/3.D0+2.D0*YAA/3.D0
          IF(YBB.LT.YDEN) THEN
            CALL LOIDEN(YAA,YS,PHI,QAB,GRAV)
          ELSE
            CALL LOINOY(YAA,YBB,YS,PHI,QAB,GRAV)
          ENDIF
        ELSE
C         CASE WHERE B IS UPSTREAM
          YDEN=YS/3.D0+2.D0*YBB/3.D0
          IF(YAA.LT.YDEN) THEN
            CALL LOIDEN(YBB,YS,PHI,QAB,GRAV)
          ELSE
            CALL LOINOY(YBB,YAA,YS,PHI,QAB,GRAV)
          ENDIF
          QAB=-QAB
        ENDIF
C
C COMPUTES THE NORMAL DISCHARGE
C
        IF(H(NA).LE.HMIN) THEN
          UNORM(IA)=0.D0
        ELSE
          UNORM(IA)=-QAB/H(NA)
        ENDIF
C
        IF(H(NB).LE.HMIN) THEN
          UNORM(IB)=0.D0
        ELSE
          UNORM(IB)=-QAB/H(NB)
        ENDIF
C
20    CONTINUE
10    CONTINUE
C
C     DETERMINES THE NUMERICAL VALUE
C     OF THE BOUNDARY CONDITIONS:
C
      CALL CLHUVT(NWEIRS,NPSING,NPSMAX,NUMDIG,ZDIG,X,Y,ZF,
     &            IOPTAN,UNORM,CHESTR,NKFROT,KARMAN,T,NTRAC,H,
     &            UBOR,VBOR,TBOR,NBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C