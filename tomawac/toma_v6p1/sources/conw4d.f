C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE ADVECTION FIELD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  IN THIS CASE THE X AXIS IS VERTICAL ORIENTED UPWARDS AND
!>            THE Y AXIS IS HORIZONTAL ORIENTED TOWARDS THE RIGHT;
!>            TETA IS THE DIRECTION WRT NORTH, CLOCKWISE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, CG, COSF, COSTET, COURAN, CT, CX, CY, DEPTH, DUX, DUY, DVX, DVY, DZHDT, DZX, DZY, FREQ, JF, NF, NPLAN, NPOIN2, PROINF, SINTET, SPHE, TGF, TRA01, TRA02, U, V, XK
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DDDN, DEUKD, DEUPI, DSDD, DSDNSK, GRADEG, GSQP, IP, IPOIN, LSDUDN, LSDUDS, R, SR, SRCF, TFSR, USDPI, USGD
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PREPRO()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/95
!> </td><td> F MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CG
!></td><td>--></td><td>VITESSE DE GROUPE DISCRETISEE
!>    </td></tr>
!>          <tr><td>COSF
!></td><td>--></td><td>COSINUS DES LATITUDES DES POINTS 2D
!>    </td></tr>
!>          <tr><td>COSTET
!></td><td>--></td><td>COSINUS TETA
!>    </td></tr>
!>          <tr><td>COURAN
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON A UN COURANT
!>    </td></tr>
!>          <tr><td>CX,CY,CT,CF
!></td><td><--</td><td>CHAMP CONVECTEUR SELON X(OU PHI),
!>                  Y(OU LAMBDA) ET TETA  ET FREQ
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>--></td><td>PROFONDEUR
!>    </td></tr>
!>          <tr><td>DUX
!></td><td>--></td><td>GRADIENT DE COURANT U SELON X
!>    </td></tr>
!>          <tr><td>DUY
!></td><td>--></td><td>GRADIENT DE COURANT U SELON Y
!>    </td></tr>
!>          <tr><td>DVX
!></td><td>--></td><td>GRADIENT DE COURANT V SELON X
!>    </td></tr>
!>          <tr><td>DVY
!></td><td>--></td><td>GRADIENT DE COURANT V SELON Y
!>    </td></tr>
!>          <tr><td>DZHDT
!></td><td>--></td><td>GRADIENT DE FOND SELON T
!>    </td></tr>
!>          <tr><td>DZX
!></td><td>--></td><td>GRADIENT DE FOND SELON X
!>    </td></tr>
!>          <tr><td>DZY
!></td><td>--></td><td>GRADIENT DE FOND SELON Y
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>--></td><td>FREQUENCES DISCRETISEES
!>    </td></tr>
!>          <tr><td>JF
!></td><td>--></td><td>FREQUENCES COURANTE
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS OU DE DIRECTIONS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>PROINF
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON EST EN PROF INFINIE
!>    </td></tr>
!>          <tr><td>SINTET
!></td><td>--></td><td>SINUS TETA
!>    </td></tr>
!>          <tr><td>SPHE
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON EST EN COORD. SPHER.
!>    </td></tr>
!>          <tr><td>TGF
!></td><td>--></td><td>TANGENTES DES LATITUDES DES POINTS 2D
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRA02
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DU CHAMP DE COURANT
!>    </td></tr>
!>          <tr><td>XK
!></td><td>--></td><td>NOMBRE D'ONDE DISCRETISE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CONW4D
     &(CX,CY,CT,CF,U,V,XK,CG,COSF,TGF,DEPTH,DZHDT,DZX,DZY,DUX,DUY,
     & DVX,DVY,FREQ,COSTET,SINTET,NPOIN2,NPLAN,JF,NF,PROINF,SPHE,
     & COURAN,TRA01,TRA02)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CG             |-->| VITESSE DE GROUPE DISCRETISEE
C| COSF           |-->| COSINUS DES LATITUDES DES POINTS 2D
C| COSTET         |-->| COSINUS TETA
C| COURAN         |-->| LOGIQUE INDIQUANT SI ON A UN COURANT
C| CX,CY,CT,CF    |<--| CHAMP CONVECTEUR SELON X(OU PHI),
C|                |   | Y(OU LAMBDA) ET TETA  ET FREQ
C| DEPTH          |-->| PROFONDEUR
C| DUX            |-->| GRADIENT DE COURANT U SELON X
C| DUY            |-->| GRADIENT DE COURANT U SELON Y
C| DVX            |-->| GRADIENT DE COURANT V SELON X
C| DVY            |-->| GRADIENT DE COURANT V SELON Y
C| DZHDT          |-->| GRADIENT DE FOND SELON T
C| DZX            |-->| GRADIENT DE FOND SELON X
C| DZY            |-->| GRADIENT DE FOND SELON Y
C| FREQ           |-->| FREQUENCES DISCRETISEES
C| JF             |-->| FREQUENCES COURANTE
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPLAN          |-->| NOMBRE DE PLANS OU DE DIRECTIONS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| PROINF         |-->| LOGIQUE INDIQUANT SI ON EST EN PROF INFINIE
C| SINTET         |-->| SINUS TETA
C| SPHE           |-->| LOGIQUE INDIQUANT SI ON EST EN COORD. SPHER.
C| TGF            |-->| TANGENTES DES LATITUDES DES POINTS 2D
C| TRA01          |<->| TABLEAU DE TRAVAIL
C| TRA02          |<->| TABLEAU DE TRAVAIL
C| U,V            |-->| COMPOSANTES DU CHAMP DE COURANT
C| XK             |-->| NOMBRE D'ONDE DISCRETISE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NF,NPLAN,NPOIN2
      INTEGER JF,IP,IPOIN
C
      DOUBLE PRECISION CX(NPOIN2,NPLAN,JF),CY(NPOIN2,NPLAN,JF)
      DOUBLE PRECISION CT(NPOIN2,NPLAN,JF),CF(NPOIN2,NPLAN,JF)
      DOUBLE PRECISION FREQ(NF)
      DOUBLE PRECISION CG(NPOIN2,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION DEPTH(NPOIN2),DZHDT(NPOIN2)
      DOUBLE PRECISION U(NPOIN2),V(NPOIN2),DZX(NPOIN2),DZY(NPOIN2)
      DOUBLE PRECISION DUX(NPOIN2),DUY(NPOIN2),DVX(NPOIN2),DVY(NPOIN2)
      DOUBLE PRECISION COSTET(NPLAN),SINTET(NPLAN)
      DOUBLE PRECISION COSF(NPOIN2),TGF(NPOIN2)
C
      DOUBLE PRECISION TRA01(NPLAN),TRA02(NPLAN)
      DOUBLE PRECISION GSQP,SR,R,SRCF,TFSR
      DOUBLE PRECISION DDDN,DSDNSK,LSDUDN,GRADEG,LSDUDS
      DOUBLE PRECISION DSDD,USGD,USDPI,DEUPI,DEUKD
C
      LOGICAL PROINF,SPHE,COURAN
C
C***********************************************************************
C
      GSQP=0.780654996D0
      R=6400.D3
      USDPI=0.159154943D0
      DEUPI=6.283185307D0
C
C-----------------------------------------------------------------------
C     INFINITE WATER DEPTH ...
C-----------------------------------------------------------------------
C
      IF (PROINF) THEN
C
        DO IP=1,NPLAN
          TRA01(IP)=GSQP/FREQ(JF)*COSTET(IP)
          TRA02(IP)=GSQP/FREQ(JF)*SINTET(IP)
        ENDDO
C
C       ----------------------------------------------------------------
C       ... AND IN CARTESIAN COORDINATE SYSTEM
C       ----------------------------------------------------------------
C
        IF (.NOT.SPHE) THEN
C
          DO IPOIN=1,NPOIN2
            DO IP=1,NPLAN
              CX(IPOIN,IP,JF)=TRA01(IP)
              CY(IPOIN,IP,JF)=TRA02(IP)
              CT(IPOIN,IP,JF)=0.D0
            ENDDO
          ENDDO
C
          IF (COURAN) THEN
            DO IPOIN=1,NPOIN2
             DO IP=1,NPLAN
              LSDUDN= SINTET(IP)*
     &                 (-COSTET(IP)*DUX(IPOIN)-SINTET(IP)*DVX(IPOIN))
     &              + COSTET(IP)*
     &                 ( COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
              LSDUDS= COSTET(IP)*
     &                 (COSTET(IP)*DUX(IPOIN)+SINTET(IP)*DVX(IPOIN))
     &              + SINTET(IP)*
     &                 (COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
              CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF)+U(IPOIN)
              CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF)+V(IPOIN)
              CT(IPOIN,IP,JF)= -LSDUDN
              CF(IPOIN,IP,JF)= -CG(IPOIN,JF)*XK(IPOIN,JF)*
     &                          LSDUDS*USDPI
             ENDDO
            ENDDO
          ENDIF
C
C       ----------------------------------------------------------------
C       ... AND IN SPHERICAL COORDINATE SYSTEM
C       ----------------------------------------------------------------
C
        ELSE
C
          SR=1.D0/R
          GRADEG=180.D0/3.1415926D0
          DO IPOIN=1,NPOIN2
            SRCF=SR/COSF(IPOIN)
            TFSR=TGF(IPOIN)*SR
            DO IP=1,NPLAN
              CX(IPOIN,IP,JF)=TRA01(IP)*SR*GRADEG
              CY(IPOIN,IP,JF)=TRA02(IP)*SRCF*GRADEG
              CT(IPOIN,IP,JF)=TRA02(IP)*TFSR
            ENDDO
          ENDDO
C
          IF (COURAN) THEN
            DO IPOIN=1,NPOIN2
             SRCF=SR/COSF(IPOIN)
             DO IP=1,NPLAN
              LSDUDN= SINTET(IP)*SR*
     &                 (-COSTET(IP)*DUX(IPOIN)-SINTET(IP)*DVX(IPOIN))
     &                 + COSTET(IP)*SRCF*
     &                 ( COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
              LSDUDS= COSTET(IP)*SR*
     &                 (COSTET(IP)*DUX(IPOIN)+SINTET(IP)*DVX(IPOIN))
     &                + SINTET(IP)*SRCF*
     &                 (COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
              CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF) + U(IPOIN)*SR*GRADEG
              CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF) + V(IPOIN)*SRCF*GRADEG
              CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF) - LSDUDN*GRADEG
              CF(IPOIN,IP,JF)= - LSDUDS*GRADEG*
     &                          CG(IPOIN,JF)*XK(IPOIN,JF)*USDPI
             ENDDO
            ENDDO
          ENDIF
        ENDIF
C
C
C-----------------------------------------------------------------------
C     FINITE WATER DEPTH ....
C-----------------------------------------------------------------------
C
      ELSE
C
C       ----------------------------------------------------------------
C       ... AND IN CARTESIAN COORDINATE SYSTEM
C       ----------------------------------------------------------------
C
        IF (.NOT.SPHE) THEN
C
          DO IPOIN=1,NPOIN2
            DO IP=1,NPLAN
              DDDN=-SINTET(IP)*DZX(IPOIN)+COSTET(IP)*DZY(IPOIN)
              CX(IPOIN,IP,JF)=CG(IPOIN,JF)*COSTET(IP)
              CY(IPOIN,IP,JF)=CG(IPOIN,JF)*SINTET(IP)
              DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
              IF (DEUKD.GT.7.D2) THEN
                DSDNSK = 0.D0
              ELSE
                DSDNSK = DEUPI*FREQ(JF)/SINH(DEUKD)
              ENDIF
              CT(IPOIN,IP,JF)=-DSDNSK*DDDN
            ENDDO
          ENDDO
C
          IF (COURAN) THEN
            DO IPOIN=1,NPOIN2
              DO IP=1,NPLAN
                LSDUDN= SINTET(IP)*
     &                 (-COSTET(IP)*DUX(IPOIN)-SINTET(IP)*DVX(IPOIN))
     &                + COSTET(IP)*
     &                 ( COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
                LSDUDS= COSTET(IP)*
     &                 (COSTET(IP)*DUX(IPOIN)+SINTET(IP)*DVX(IPOIN))
     &                + SINTET(IP)*
     &                 (COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
                DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
                IF (DEUKD.GT.7.D2) THEN
                  DSDD = 0.D0
                ELSE
                  DSDD = XK(IPOIN,JF)*DEUPI*FREQ(JF)/SINH(DEUKD)
                ENDIF
                USGD=U(IPOIN)*DZX(IPOIN)+V(IPOIN)*DZY(IPOIN)
      	        CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF) + U(IPOIN)
                CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF) + V(IPOIN)
                CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF) - LSDUDN
                CF(IPOIN,IP,JF)= (DSDD*(USGD+DZHDT(IPOIN))
     &                 - LSDUDS*CG(IPOIN,JF)*XK(IPOIN,JF))*USDPI
              ENDDO
            ENDDO
          ENDIF
C
C       --------------------------------------------------------------
C       ... AND IN SPHERICAL COORDINATE SYSTEM
C       --------------------------------------------------------------
C
        ELSE
C
          SR=1.D0/R
          GRADEG=180.D0/3.1415926D0
          DO IPOIN=1,NPOIN2
            SRCF=SR/COSF(IPOIN)
            TFSR=TGF(IPOIN)*SR
            DO IP=1,NPLAN
             DDDN=-SINTET(IP)*DZX(IPOIN)*SR+COSTET(IP)*DZY(IPOIN)*SRCF
             CX(IPOIN,IP,JF)=(CG(IPOIN,JF)*COSTET(IP))*SR*GRADEG
             CY(IPOIN,IP,JF)=(CG(IPOIN,JF)*SINTET(IP))*SRCF*GRADEG
             DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
             IF (DEUKD.GT.7.D2) THEN
               DSDNSK = 0.D0
             ELSE
               DSDNSK = DEUPI*FREQ(JF)/SINH(DEUKD)
             ENDIF
             CT(IPOIN,IP,JF)=CG(IPOIN,JF)*SINTET(IP)*TFSR
     &                                  -DSDNSK*DDDN*GRADEG
            ENDDO
          ENDDO
C
          IF (COURAN) THEN
            DO IPOIN=1,NPOIN2
              SRCF=SR/COSF(IPOIN)
              DO IP=1,NPLAN
                LSDUDN= SINTET(IP)*SR*
     &                 (-COSTET(IP)*DUX(IPOIN)-SINTET(IP)*DVX(IPOIN))
     &                + COSTET(IP)*SRCF*
     &                 ( COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
                LSDUDS= COSTET(IP)*SR*
     &                 ( COSTET(IP)*DUX(IPOIN)+SINTET(IP)*DVX(IPOIN))
     &                + SINTET(IP)*SRCF*
     &                 ( COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
                DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
                IF (DEUKD.GT.7.D2) THEN
                  DSDD = 0.D0
                ELSE
                  DSDD = XK(IPOIN,JF)*DEUPI*FREQ(JF)/SINH(DEUKD)
                ENDIF
                USGD=U(IPOIN)*DZX(IPOIN)*SR+V(IPOIN)*DZY(IPOIN)*SRCF
                CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF) + U(IPOIN)*SR*GRADEG
                CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF) + V(IPOIN)*SRCF*GRADEG
                CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF) - LSDUDN*GRADEG
                CF(IPOIN,IP,JF)=  (DSDD*(USGD*GRADEG+DZHDT(IPOIN))
     &            -LSDUDS*GRADEG*CG(IPOIN,JF)*XK(IPOIN,JF))*USDPI
              ENDDO
            ENDDO
          ENDIF
C
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C