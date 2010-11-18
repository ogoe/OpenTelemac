C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DEFINES THE DEPTHS, VELOCITIES, ... TO BE IMPOSED
!>                AT THE NODES, FROM THE DEPTHS AND AVERAGE FLOWS
!>                ON THE SEGMENTS CONSTITUTING THE SINGULARITY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHESTR, H, IOPTAN, KARMAN, LIHBOR, LITBOR, LIUBOR, LIVBOR, NBOR, NKFROT, NPSING, NPSMAX, NTRAC, NUMDIG, NWEIRS, T, TBOR, UBOR, UNORM, VBOR, X, Y, ZDIG, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CZ, DL, HH, I, I1, I2, ITRAC, K, N, N0, N1, N2, NX, NY, PENTE, TX, TY, UTAN, XX
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CLSING()

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
!>      <tr>
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
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT.
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR.
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
!>          <tr><td>T
!></td><td>--></td><td>TRACEUR.
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
!>          <tr><td>UNORM(J)
!></td><td>--></td><td>VITESSE NORMALE.
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
                        SUBROUTINE CLHUVT
     &(NWEIRS,NPSING,NPSMAX,NUMDIG,ZDIG,
     & X,Y,ZF,IOPTAN,UNORM,CHESTR,
     & NKFROT,KARMAN,T,NTRAC,H,UBOR,VBOR,TBOR,NBOR,
     & LIHBOR,LIUBOR,LIVBOR,LITBOR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHESTR         |-->| COEFFICIENT DE FROTTEMENT.
C| H             |-->| HAUTEUR.
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
C| T             |-->| TRACEUR.
C| TBOR(J)        |<--| VALEUR DE LA CONDITION EN TRACEUR AU
C|                |   | J-EME POINT LIMITE
C| TRAC           |-->| SI OUI, IL Y A UN TRACEUR.
C| UBOR(J)        |<--| VALEUR DE LA CONDITION EN VIESSE U AU
C|                |   | J-EME POINT LIMITE
C| UNORM(J)       |-->| VITESSE NORMALE.
C| VBOR(J)        |<--| VALEUR DE LA CONDITION EN VITESSE V A
C|                |   | J-EME POINT LIMITE
C| X,Y            |-->| COORDONNEES DES NOUEDS.
C| ZDIG(N,I)      |-->| COTE DE LA DIGUE AU I-EME POINT DE
C|                |   | LA N-IEME SINGULARITE
C| ZF             |-->| COTE DU FOND.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NWEIRS,NPSMAX,IOPTAN
      INTEGER, INTENT(IN)    :: NPSING(NWEIRS),NUMDIG(2,NWEIRS,NPSMAX)
      INTEGER, INTENT(IN)             :: NBOR(*),NKFROT(*)
      INTEGER, INTENT(INOUT)          :: LIUBOR(*),LIHBOR(*),LIVBOR(*)
      INTEGER, INTENT(IN)             :: NTRAC
      DOUBLE PRECISION, INTENT(IN)    :: ZDIG(NWEIRS,NPSMAX)
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),ZF(*),CHESTR(*),H(*)
      DOUBLE PRECISION, INTENT(IN)    :: UNORM(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR,LITBOR
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,I1,I2,K,N,N0,N1,N2,ITRAC
C
      DOUBLE PRECISION DL,NX,NY,PENTE,CZ,HH,TX,TY,UTAN,XX
C
      INTRINSIC ABS,SQRT,SIGN
C
C-----------------------------------------------------------------------
C
C     LOOPS ON THE HYDRAULIC STRUCTURES
C
      DO 10 N=1,NWEIRS
C
C       LOOPS ON THE CRESTS OF THE HYDRAULIC STRUCTURES
C
        DO 20 K=1,2
C
C         LOOPS ON THE NODES OF EACH CREST
C
          DO 30 I=1,NPSING(N)
C
          I1=NUMDIG(K,N,I)
          N1=NBOR(I1)
C
          IF(I.EQ.1) THEN
            N0=N1
            N2=NBOR(NUMDIG(K,N,I+1))
            XX=0.D0
          ELSEIF(I.LT.NPSING(N)) THEN
            N0=NBOR(NUMDIG(K,N,I-1))
            N2=NBOR(NUMDIG(K,N,I+1))
            XX=1.D0
          ELSE
            N0=NBOR(NUMDIG(K,N,I-1))
            N2=N1
            XX=0.D0
          ENDIF
C
C         CALCULATES THE NORMAL VECTOR, OUTGOING CREST 1, ENTERING CREST 2
C
          TX=X(N2)-X(N0)
          TY=Y(N2)-Y(N0)
          DL=SQRT(TX*TX+TY*TY)
          TX=TX/DL
          TY=TY/DL
          NX=-TY
          NY=TX
C
C         CALCULATES THE TANGENTIAL VELOCITY
C
          IF (IOPTAN.EQ.0) THEN
C
             UTAN=0.D0
C
          ELSEIF(IOPTAN.EQ.1) THEN
C
C            ONE TAKES THE HEIGHT ON THE CREST (TO BE DISCUSSED)
C            HH = H (N1)
             HH = H(N1)+ZF(N1)-ZDIG(N,I)
C            LINE ADDED ON 23/11/2005 BY JMH (HH MAY BE NEGATIVE)
             HH=MAX(HH,0.D0)
             PENTE=(H(N0)-H(N2)+ZF(N0)-ZF(N2))/DL
C
             IF (NKFROT(N1).EQ.2) THEN
                UTAN = CHESTR(N1)*SQRT(HH*ABS(PENTE))*SIGN(1.D0,PENTE)
             ELSEIF (NKFROT(N1).EQ.3) THEN
                UTAN = CHESTR(N1)*HH**(2.D0/3.D0)*SQRT(ABS(PENTE))
     &                                           *SIGN(1.D0,PENTE)
             ELSEIF (NKFROT(N1).EQ.4) THEN
                UTAN = HH**(2.D0/3.D0)*SQRT(ABS(PENTE))
     &                                *SIGN(1.D0,PENTE)/CHESTR(N1)
             ELSEIF (NKFROT(N1).EQ.5) THEN
                HH   = MAX(HH,1.D-9)
              CZ = MAX(1.D-9,LOG(11.D0*HH/MAX(CHESTR(N1),1.D-9))/KARMAN)
                UTAN = CZ*SQRT(HH*ABS(PENTE))*SIGN(1.D0,PENTE)
             ELSE
                IF (LNG.EQ.1) THEN
                   WRITE(LU,*)'CLHUVT : OPTION INCONNUE :',NKFROT(N1)
                   WRITE(LU,*)'         POUR LA LOI DE FROTTEMENT'
                ELSEIF(LNG.EQ.2) THEN
                   WRITE(LU,*)'CLHUVT : UNKNOWN OPTION:',NKFROT(N1)
                   WRITE(LU,*)'         FOR THE FRICTION LAW'
                ENDIF
                CALL PLANTE(1)
                STOP
             ENDIF
C            TO GET ZERO TANGENTIAL VELOCITIES IN THE CORNERS
             UTAN = XX*UTAN
          ELSE
             IF (LNG.EQ.1) THEN
                WRITE(LU,*)'CLHUVT : OPTION INCONNUE :',IOPTAN
                WRITE(LU,*)'         POUR LES VITESSES TANGENTIELLES'
             ELSEIF(LNG.EQ.2) THEN
                WRITE(LU,*)'CLHUVT : UNKNOWN OPTION:',IOPTAN
                WRITE(LU,*)'         FOR THE TANGENTIAL VELOCITY'
             ENDIF
             CALL PLANTE(1)
             STOP
          ENDIF
C
C         ONE CALCULATES VELOCITY COMPONENTS U AND V
C         IN THE ORDINARY COORDINATE SYSTEM (X,Y).
C
          UBOR(I1)=UTAN*TX+UNORM(I1)*NX
          VBOR(I1)=UTAN*TY+UNORM(I1)*NY
C
30    CONTINUE
20    CONTINUE
C
C
C-----------------------------------------------------------------------
C
C  TYPES OF CONDITIONS FOR THE DEPTH AND THE VELOCITY:
C
      DO 40 I=1,NPSING(N)
C
        I1=NUMDIG(1,N,I)
        I2=NUMDIG(2,N,I)
        LIHBOR(I1)=4
        LIUBOR(I1)=6
        LIVBOR(I1)=6
        LIHBOR(I2)=4
        LIUBOR(I2)=6
        LIVBOR(I2)=6
C
C       CORRECTION: SOLID WALL TYPE IF NORMAL VELOCITY IS ZERO
C
        IF(ABS(UNORM(I1)).LT.1.D-10) THEN
          LIHBOR(I1)=2
          LIUBOR(I1)=2
          LIVBOR(I1)=2
          LIHBOR(I2)=2
          LIUBOR(I2)=2
          LIVBOR(I2)=2
          IF(NTRAC.GT.0) THEN
            DO ITRAC=1,NTRAC
              LITBOR%ADR(ITRAC)%P%I(I1)=2
              LITBOR%ADR(ITRAC)%P%I(I2)=2
            ENDDO
          ENDIF
        ENDIF
C
40    CONTINUE
C
C-----------------------------------------------------------------------
C
C  TYPES OF CONDITIONS FOR THE TRACER AND VALUES TO BE IMPOSED
C
      IF(NTRAC.GT.0) THEN
C
        DO ITRAC=1,NTRAC
        DO I=1,NPSING(N)
C
          I1=NUMDIG(1,N,I)
          I2=NUMDIG(2,N,I)
C
          IF(UNORM(I1).LT.-1.D-8) THEN
C           OUTGOING SPEED IN 1, ENTERING IN 2
            LITBOR%ADR(ITRAC)%P%I(I1)=4
            LITBOR%ADR(ITRAC)%P%I(I2)=5
            TBOR%ADR(ITRAC)%P%R(I2)=T%ADR(ITRAC)%P%R(NBOR(I1))
          ELSEIF(UNORM(I1).GT.1.D-8) THEN
C           OUTGOING SPEED IN 2, ENTERING IN 1
            LITBOR%ADR(ITRAC)%P%I(I1)=5
            TBOR%ADR(ITRAC)%P%R(I1)=T%ADR(ITRAC)%P%R(NBOR(I2))
            LITBOR%ADR(ITRAC)%P%I(I2)=4
          ELSE
C           ZERO VELOCITY
            LITBOR%ADR(ITRAC)%P%I(I1)=2
            LITBOR%ADR(ITRAC)%P%I(I2)=2
          ENDIF
C
        ENDDO
        ENDDO
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C END OF THE LOOP ON THE CREST
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C