C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SAINT VENANT-KINETIC.
!><br>            COMPUTES THE ADVECTIVE FLUXES AT BOUNDARIES.
!>                UA(1,IS) = H;  UA(2,IS)=U;  UA(3,IS)=V.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CE, CFL, DT, DTHAUT, FLUENT, FLUHBTEMP, FLUSORT, G, HBOR, KDIR, KNEU, LIMPRO, NBOR, NPTFR, NS, NTRAC, UA, UBOR, VBOR, XNEBOR, YNEBOR
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, A1, A2, A3, ALP, ALP2, ALP3, ALPHA0, ALPHA1, ALPHA2, AM, AUX, C, CA1, DEST, DTL, FHMOINS, FHPLUS, FLUH, FLUTMP, FLUU, FLUV, FUMOINS, FUPLUS, H, HG, HRH, HRHG, HUIN, HVIN, IS, ITRAC, K, NIT, RA3, RA32, RA33, RH, RHG, RUN, RVG, SG, SIGMAX, SQ2, U, UG, UIN, UNN, UNORM, V, VG, VIN, VNL, VNN, VNX, VNX1, VNY, VNY1, VP1, VP2, VP3
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ZEROPHI(), ZEROPSI()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FLUHYD()

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
!>      <td><center> 5.8                                       </center>
!> </td><td>
!> </td><td> INRIA
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CE
!></td><td><-></td><td>FLUX
!>    </td></tr>
!>          <tr><td>CFL
!></td><td>--></td><td>NOMBRE DE CFL
!>    </td></tr>
!>          <tr><td>DT
!></td><td><-></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>DTHAUT
!></td><td>--></td><td>UTILISE POUR CONDITION CFL
!>    </td></tr>
!>          <tr><td>FLUENT,FLUSORT
!></td><td><--</td><td>FLUX MASSE ENTREE ET SORTIE
!>    </td></tr>
!>          <tr><td>FLUHBTEMP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>G
!></td><td>--></td><td>CONSTANTE DE GRAVITE
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE H
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONVENTION POUR LES POINTS DIRICHLET
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>--></td><td>CONVENTION POUR LES POINTS NEUMANN
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NS
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
!>    </td></tr>
!>          <tr><td>UA
!></td><td>--></td><td>UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!>    </td></tr>
!>          <tr><td>UBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE U
!>    </td></tr>
!>          <tr><td>VBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE V
!>    </td></tr>
!>          <tr><td>XNEBOR,YNEBOR
!></td><td>--></td><td>NORMALE AUX POINTS FRONTIERE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CDL
     &(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,G,HBOR,
     & UBOR,VBOR,UA,CE,FLUENT,FLUSORT,DTHAUT,DT,CFL,FLUHBTEMP,NTRAC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CE             |<->| FLUX
C| CFL            |-->| NOMBRE DE CFL
C| DT             |<->| PAS DE TEMPS
C| DTHAUT         |-->| UTILISE POUR CONDITION CFL
C| FLUENT,FLUSORT |<--| FLUX MASSE ENTREE ET SORTIE
C| FLUHBTEMP      |---| 
C| G             |-->| CONSTANTE DE GRAVITE
C| HBOR           |-->| VALEURS IMPOSEES DE H
C| KDIR           |-->| CONVENTION POUR LES POINTS DIRICHLET
C| KNEU           |-->| CONVENTION POUR LES POINTS NEUMANN
C| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
C| NTRAC          |---| 
C| TRAC           |-->| LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
C| UA             |-->| UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
C| UBOR           |-->| VALEURS IMPOSEES DE U
C| VBOR           |-->| VALEURS IMPOSEES DE V
C| XNEBOR,YNEBOR  |-->| NORMALE AUX POINTS FRONTIERE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NS,NPTFR,KDIR,KNEU,NTRAC
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR),UA(3,NS),DTHAUT(*)
      DOUBLE PRECISION, INTENT(IN)    :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFL
      DOUBLE PRECISION, INTENT(INOUT) :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: CE(3,NS),FLUENT,FLUSORT
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUHBTEMP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS,K,NIT,ITRAC
C
      DOUBLE PRECISION RA3,RA32,RA33, ALP,ALP2,ALP3,SG,SQ2
      DOUBLE PRECISION VNX,VNY,VNX1,VNY1,VNL,H,U,V,RUN
      DOUBLE PRECISION FLUH,FLUU,FLUV,AUX,FLUTMP,RH,HRH,UNN,VNN
      DOUBLE PRECISION FHPLUS,FUPLUS,FHMOINS,FUMOINS
      DOUBLE PRECISION A,A1,A2,A3,ALPHA0,ALPHA1,ALPHA2,C,VP1,VP2 ,VP3
      DOUBLE PRECISION HG ,RHG,HRHG,UG,VG,DEST,RVG,CA1,AM
      DOUBLE PRECISION UIN,VIN,HUIN,HVIN,SIGMAX,DTL,UNORM
C
      SQ2   = SQRT(2.D0)
      SG    = SQRT(G)
      RA3   = SQRT(1.5D0*G)
      RA32  = RA3**2
      RA33  = RA3*RA32
      ALP   = 0.5D0/RA3
      ALP2  = 0.5D0 *ALP
      ALP3  = ALP/3.D0
C
      FLUENT=0.D0
      FLUSORT=0.D0
C
      DO K=1,NPTFR
       IS=NBOR(K)
       VNX1=XNEBOR(K)
       VNY1=YNEBOR(K)
       VNX=XNEBOR(K+NPTFR)
       VNY=YNEBOR(K+NPTFR)
       VNL=SQRT(VNX**2+VNY**2)
C
       H   = UA(1,IS)
       RH  = SQRT(H)
       U   = UA(2,IS)
       V   = UA(3,IS)
C
C         SOLID WALLS
C         **************
C
C      SLIP CONDITIONS
C      ***********************
C
       IF(LIMPRO(K,1).EQ.KNEU) THEN
C
         AUX=0.5D0*G*H**2
         FLUH = 0.D0
         FLUU = AUX*VNX
         FLUV = AUX*VNY
       ELSE
C
C        LIQUID BOUNDARIES
C        *******************
C
C     CALCULATES F+(H,U,V)
C
       HRH = RH * H
C
       IF(H.LE.0.D0) THEN
         U=0.D0
         V=0.D0
         UNN=0.D0
         VNN=0.D0
         FHPLUS = 0.D0
         FUPLUS = 0.D0
       ELSE
         UNN= +VNX1*U+VNY1*V
         VNN= -VNY1*U+VNX1*V
C
         A=MIN(RA3,MAX(-RA3,-UNN/RH))
         A2 =A * A
         A3 =A2 * A
         ALPHA0=ALP*(RA3-A)
         ALPHA1=ALP2*(RA32-A2)
         ALPHA2=ALP3*(RA33-A3)
C
         FHPLUS = H*UNN*ALPHA0 + HRH*ALPHA1
         FUPLUS = UNN*(FHPLUS+HRH*ALPHA1) + H*H*ALPHA2
      ENDIF
C
C
C     CALCULATES THE FICTITIOUS STATE (HG,UG,VG)
C     ----------------------------------
C
C     H PROVIDED
C     ########
C
        IF(LIMPRO(K,1).EQ.KDIR) THEN
C
         C   = SG*RH
         VP1 = UNN
         VP2 = VP1  + C
         VP3 = VP1  - C
C
         HG     =HBOR(K)
         RHG    =SQRT (HG)
         HRHG   =RHG*HG
C
        IF (VP2*VP3.LE.0.D0.OR. VP1.LE.0.D0) THEN
C
         IF(HG.EQ.0.D0) THEN
           UG=0.D0
           VG=0.D0
           FHMOINS = 0.D0
           FUMOINS = 0.D0
           SIGMAX=1.D-2
         ELSE
C
C    FLUVIAL REGIME
C    --------------
C
          IF (VP2*VP3.LE.0.D0) THEN
C
           UG=UNN+2.D0*SG*(RH-RHG)
           VG=VNN
C
C    TORRENTIAL REGIME
C    -----------------
C
          ELSE
C
C   FLUX IMPOSED
C   -----------
          IF(LIMPRO(K,2).EQ.KDIR) THEN
C
            UIN = UBOR(K)
            VIN = VBOR(K)
            HUIN = H*UIN
            HVIN = H*VIN
C
            DEST=HUIN*VNX1+HVIN*VNY1
            RVG =-HUIN*VNY1+HVIN*VNX1
C
            A1 = DEST-FHPLUS
            CA1= SQ2*A1/(SG*HG*RHG)
            CALL ZEROPHI(-1.D0,AM,NIT,CA1)
C
            UG= AM*SG*RHG
            VG=RVG/HG
C
          ELSE
C
C   DATA MISSING, ONE SUPPOSES STILL WATER
C
            UG= 0.D0
            VG= 0.D0
C
          ENDIF
C
           ENDIF
C
         GOTO 220
      ENDIF
      GOTO 200
C
C  THE DOWNSTREAM IS IN FACT TORRENTIAL, ONE CANNOT MAINTAIN
C   THE CONDITION OF H IMPOSED
C
       ELSE
       GOTO 100
C
      ENDIF
C
C
C     VELOCITY IMPOSED
C     ################
C
        ELSE IF(LIMPRO(K,2).EQ.KDIR) THEN
C
        UIN = UBOR(K)
        VIN = VBOR(K)
        HUIN = H*UIN
        HVIN = H*VIN
C
         DEST=HUIN*VNX1+HVIN*VNY1
         RVG =-HUIN*VNY1+HVIN*VNX1
C     ATTENTION: MODIFICATION OF THE SIGN / INRIA REPORT
            A1 = -DEST+FHPLUS
            A2 = -UNN - 2.D0*SG*RH
C
         IF (A1.LE.0.D0) THEN
C
C     FH- =-A1 CANNOT BE SATISFIED
C
         FHMOINS = 0.D0
         FUMOINS = 0.D0
         VG=0.D0
        SIGMAX=1.E-2
           ELSE
           CA1= 1.D0/(G*SQ2*A1)**(1.D0/3.D0)
           CALL ZEROPSI(-0.5D0,AM,NIT,CA1,A2)
C
           RHG =A2/(SG*(AM-2.D0))
           HG= RHG * RHG
           HRHG= RHG * HG
C
         IF (HG.EQ.0.D0) THEN
         UG=0.D0
         VG=0.D0
         FHMOINS = 0.D0
         FUMOINS = 0.D0
               SIGMAX=1.D-2
         ELSE
            UG=-AM*A2/(AM-2.D0)
            VG=RVG/HG
        GOTO 220
      ENDIF
      ENDIF
        GOTO 200
C
C   NO CONDITION
C
        ELSE
C
        GOTO 100
C
        ENDIF
       GOTO 1000
C
C
C     CALCULATES F-(HG,UG,VG)
C
 220   CONTINUE
C
         A=MIN(RA3,MAX(-RA3,-UG/RHG))
         A2 =A * A
         A3 =A2 * A
         ALPHA0=ALP*(A+RA3)
         ALPHA1=ALP2*(A2-RA32)
         ALPHA2=ALP3*(A3+RA33)
C
         FHMOINS = HG*UG*ALPHA0 + HRHG*ALPHA1
         FUMOINS = UG*(FHMOINS + HRHG*ALPHA1)
     &  + HG*HG*ALPHA2
C
            SIGMAX= RHG
            UNORM=SQRT(UG *UG + VG*VG)
            SIGMAX=MAX( 1.D-2, RA3 *SIGMAX +UNORM )
C
C     CALCULATES FLUXES AND OPPOSITE ROTATION
C
 200   CONTINUE
         FLUH=(FHPLUS +FHMOINS)*VNL
         FLUU=(FUPLUS +FUMOINS)*VNL
C
         IF (FLUH.GE.0.D0) THEN
         FLUV= VNN*FLUH
         ELSE
         FLUV= VG*FLUH
         ENDIF
C
      FLUTMP=FLUU
      FLUU = +VNX1*FLUTMP-VNY1*FLUV
      FLUV = +VNY1*FLUTMP+VNX1*FLUV
C
C       CORRECTION OF THE TIME STEP
C
       DTL = CFL*DTHAUT(IS)/SIGMAX
       DT  = MIN(DT, DTL)
C
       GOTO 1000
100    CONTINUE
       RUN     = H*UNN
C
       FLUH =  RUN* VNL
       FLUU =  (U *RUN + 0.5D0*G*H**2* VNX)*VNL
       FLUV =  (V *RUN + 0.5D0*G*H**2* VNY)*VNL
C
 1000  CONTINUE
       ENDIF
C
       IF(LIMPRO(K,1).EQ.KDIR)  FLUSORT = FLUSORT + FLUH
       IF(LIMPRO(K,2).EQ.KDIR)  FLUENT = FLUENT +FLUH
C
       CE(1,IS)  = CE(1,IS) - FLUH
       CE(2,IS)  = CE(2,IS) - FLUU
       CE(3,IS)  = CE(3,IS) - FLUV
C
       IF(NTRAC.GT.0) THEN
         DO ITRAC=1,NTRAC
           FLUHBTEMP%ADR(ITRAC)%P%R(K)=FLUH
         ENDDO
       ENDIF
C
       ENDDO
C
C-----------------------------------------------------------------------
C
       RETURN
       END
C
C#######################################################################
C