C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       UPDATES THE TRACER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRE, AIRS, AIRST, BETA, CMI, CVIST, DIFT, DIMT, DJXT, DJYT, DLIMT, DPX, DPY, DSZ, DTT, DXT, DYT, FLUHBOR, FLUTENT, FLUTSOR, FLUXT, HCSTOK, HSTOK, HT, HTN, JMI, MASSOU, NBOR, NORDRE, NPTFR, NS, NSEG, NT, NU, NUBO, SMTR, TBOR, TN, X, Y, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AIX, AIY, AJX, AJY, AMDS, CET, CORRT, DEJA, DSM, DSP, DST, ERR, FLU11, FLU41, FLUH, FLUT, GRADI, GRADIJ, GRADJ, GRADJI, HI0, HJ0, ILIM, IS, J, K, NSG, NUBO1, NUBO2, UAS41, UAS42, ZF1, ZF2
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MAJTRAC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> EXLIM(), GRADNODT(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>RESOLU()

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
!>      <td><center> 5.4                                       </center>
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
!>          <tr><td>AIRE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AIRS
!></td><td>--></td><td>AIRES DES CELLULES
!>    </td></tr>
!>          <tr><td>AIRST
!></td><td>--></td><td>AIRES DES SOUS-TRIANGLES DANS CELLULES
!>    </td></tr>
!>          <tr><td>BETA
!></td><td>---</td><td>COEFFICIENT EXTRAPOLATION POUR ORDRE 2
!>    </td></tr>
!>          <tr><td>CMI
!></td><td>--></td><td>COORDONNEES DES POINTS MILIEUX D'INTERFACE
!>    </td></tr>
!>          <tr><td>CVIST
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION DU TRACEUR
!>    </td></tr>
!>          <tr><td>DIFT
!></td><td>--></td><td>LOGIQUE INDIQUANT S'IL Y A DIFFUSION TRACEUR
!>    </td></tr>
!>          <tr><td>DIMT
!></td><td>--></td><td>DIMENSION DU TRACEUR
!>    </td></tr>
!>          <tr><td>DJXT,DJYT
!></td><td>---</td><td>GRADIENTS PAR TRIANGLES
!>    </td></tr>
!>          <tr><td>DLIMT
!></td><td>--></td><td>DIMENSION DU TRACEUR AU BORD
!>    </td></tr>
!>          <tr><td>DPX, DPY
!></td><td>--></td><td>GRADIENTS DES FONCTIONS DE BASE
!>    </td></tr>
!>          <tr><td>DSZ
!></td><td>--></td><td>VARIATION DE Z POUR ORDRE 2
!>    </td></tr>
!>          <tr><td>DTT
!></td><td>--></td><td>PAS DE TEMPS TRACEUR
!>    </td></tr>
!>          <tr><td>DXT,DYT
!></td><td>---</td><td>GRADIENTS PAR NOEUDS
!>    </td></tr>
!>          <tr><td>FLUHBOR
!></td><td>--></td><td>FLUX DE MASSE FRONTIERE
!>    </td></tr>
!>          <tr><td>FLUTENT,FLUTSOR
!></td><td><--</td><td>FLUX TRACEUR ENTREE ET SORTIE
!>    </td></tr>
!>          <tr><td>FLUXT
!></td><td>--></td><td>FLUX DE MASSE
!>    </td></tr>
!>          <tr><td>HCSTOK
!></td><td>--></td><td>H RECONSTRUIT ORDRE 2   CORRIGE  STOCKE
!>    </td></tr>
!>          <tr><td>HSTOK
!></td><td>--></td><td>HAUTEURS D'EAU  STOCKEES
!>    </td></tr>
!>          <tr><td>HT
!></td><td><--</td><td>HT AU TEMPS N+1
!>    </td></tr>
!>          <tr><td>HTN,TN
!></td><td>--></td><td>HT, T  AU TEMPS N
!>    </td></tr>
!>          <tr><td>JMI
!></td><td>--></td><td>NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
!>                  POINT MILIEU DE L'INTERFACE
!>    </td></tr>
!>          <tr><td>MASSOU
!></td><td><--</td><td>MASSE DE TRACEUR AJOUTEE PAR TERME SOURCE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NORDRE
!></td><td>--></td><td>ORDRE DU SCHEMA
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NS
!></td><td>--></td><td>NOMBRE DE NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE D'ARETES
!>    </td></tr>
!>          <tr><td>NT
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NU
!></td><td>--></td><td>NUMEROS DES NOEUDS PAR TRIANGLE
!>    </td></tr>
!>          <tr><td>NUBO
!></td><td>--></td><td>NUMEROS GLOBAUX DES EXTREMITES DES ARETES
!>    </td></tr>
!>          <tr><td>SMTR
!></td><td>--></td><td>TERMES SOURCES DU TRACEUR
!>    </td></tr>
!>          <tr><td>TBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR T
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTES DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MAJTRAC
     & (NS,NT,DIMT,DLIMT,NSEG,NPTFR,NUBO,
     & X,Y,AIRS,NU,AIRE,HT,HTN,TN,ZF,NBOR,
     & TBOR,FLUTENT,FLUTSOR,SMTR,NORDRE,CMI,JMI,
     & DJXT,DJYT,DXT,DYT,
     & DPX,DPY,DIFT,CVIST,BETA,DSZ,AIRST,HSTOK,
     & HCSTOK,FLUXT,FLUHBOR,MASSOU,DTT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRE           |---| 
C| AIRS           |-->| AIRES DES CELLULES
C| AIRST          |-->| AIRES DES SOUS-TRIANGLES DANS CELLULES
C| BETA           |---| COEFFICIENT EXTRAPOLATION POUR ORDRE 2
C| CMI            |-->| COORDONNEES DES POINTS MILIEUX D'INTERFACE
C| CVIST          |-->| COEFFICIENT DE DIFFUSION DU TRACEUR
C| DIFT           |-->| LOGIQUE INDIQUANT S'IL Y A DIFFUSION TRACEUR
C| DIMT           |-->| DIMENSION DU TRACEUR
C| DJXT,DJYT      |---| GRADIENTS PAR TRIANGLES
C| DLIMT          |-->| DIMENSION DU TRACEUR AU BORD
C| DPX, DPY       |-->| GRADIENTS DES FONCTIONS DE BASE
C| DSZ            |-->| VARIATION DE Z POUR ORDRE 2
C| DTT            |-->| PAS DE TEMPS TRACEUR
C| DXT,DYT        |---| GRADIENTS PAR NOEUDS
C| FLUHBOR        |-->| FLUX DE MASSE FRONTIERE
C| FLUTENT,FLUTSOR|<--| FLUX TRACEUR ENTREE ET SORTIE
C| FLUXT          |-->| FLUX DE MASSE
C| HCSTOK         |-->| H RECONSTRUIT ORDRE 2   CORRIGE  STOCKE
C| HSTOK          |-->| HAUTEURS D'EAU  STOCKEES
C| HT             |<--| HT AU TEMPS N+1
C| HTN,TN         |-->| HT, T  AU TEMPS N
C| JMI            |-->| NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
C|                |   | POINT MILIEU DE L'INTERFACE
C| MASSOU         |<--| MASSE DE TRACEUR AJOUTEE PAR TERME SOURCE
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
C| NORDRE         |-->| ORDRE DU SCHEMA
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NS             |-->| NOMBRE DE NOEUDS DU MAILLAGE
C| NSEG           |-->| NOMBRE D'ARETES
C| NT             |-->| NOMBRE D'ELEMENTS
C| NU             |-->| NUMEROS DES NOEUDS PAR TRIANGLE
C| NUBO           |-->| NUMEROS GLOBAUX DES EXTREMITES DES ARETES
C| SMTR           |-->| TERMES SOURCES DU TRACEUR
C| TBOR           |-->| CONDITIONS AUX LIMITES SUR T
C| X,Y            |-->| COORDONNEES DES NOEUDS DU MAILLAGE
C| ZF             |-->| COTES DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_TELEMAC2D, EX_MAJTRAC => MAJTRAC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL, INTENT(IN) :: DIFT
      INTEGER, INTENT(IN) :: NSEG,NPTFR,NORDRE,DIMT,DLIMT,NS,NT
      INTEGER, INTENT(IN) :: NUBO(2,NSEG),NU(NT,3)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),JMI(*)
      DOUBLE PRECISION, INTENT(INOUT) :: HT(DIMT),FLUTENT,FLUTSOR
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      DOUBLE PRECISION, INTENT(IN)    :: TBOR(DLIMT),DSZ(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: X(NS),Y(NS),AIRS(NS),AIRE(NT)
      DOUBLE PRECISION, INTENT(IN)    :: HTN(DIMT),TN(DIMT),ZF(*)
      DOUBLE PRECISION, INTENT(IN)    :: SMTR(DIMT),DPX(3,NT),DPY(3,NT)
      DOUBLE PRECISION, INTENT(IN)    :: CMI(2,*),AIRST(2,*),CVIST
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(*),DJYT(*),DXT(*),DYT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: BETA
      DOUBLE PRECISION, INTENT(IN)    :: HSTOK(*)
      DOUBLE PRECISION, INTENT(IN)    :: HCSTOK(2,*),FLUXT(*)
      DOUBLE PRECISION, INTENT(IN)    :: FLUHBOR(*),DTT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS,K,NSG,NUBO1,NUBO2,J,ILIM,ERR
C
      DOUBLE PRECISION ZF1,ZF2,FLUH,FLUT,HI0,HJ0,AIX,AIY,AJX,AJY,AMDS
      DOUBLE PRECISION GRADI,GRADJ,GRADIJ,GRADJI,FLU11,FLU41,UAS41,UAS42
C
C     DYNAMIC ARRAY ALLOCATION !!!!!!!!
C
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: CET(:),DST(:,:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: DSP(:),DSM(:),CORRT(:)
      LOGICAL DEJA
      DATA DEJA/.FALSE./
C
C-----------------------------------------------------------------------
C
      IF(.NOT.DEJA) THEN
        ALLOCATE(CET(NS),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DST(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSP(NS),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSM(NS),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(CORRT(NS),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        GO TO 1002
1001    CONTINUE
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000    FORMAT(1X,'MAJTRAC : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     &        'CODE D''ERREUR : ',1I6)
2000    FORMAT(1X,'MAJTRAC: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &        'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
1002    CONTINUE
        DEJA=.TRUE.
      ENDIF
C
C-----------------------------------------------------------------------
C
C  INITIALISES
C
      DO IS=1,NS
        CET(IS) = 0.D0
      ENDDO
C
C   COMPUTES THE TRACER GRADIENTS BY TRIANGLE AND BY NODE
C   COMPUTES THE DIFFUSION TERM
C
      IF(DIFT.OR.NORDRE.EQ.2) CALL GRADNODT(NS,NT,NU,AIRE,AIRS,
     &HSTOK,TN,DPX,DPY,DJXT,DJYT,DXT,DYT,DIFT,CVIST,CET,DTT)
C
      IF(NORDRE.EQ.2) THEN
C
C  REBUILDS 2ND ORDER FOR TRACER
C  *************************************
C
      DO IS=1,NS
        DSP(IS)=0.D0
        DSM(IS)=0.D0
      ENDDO
C
      DO  NSG=1,NSEG
C
         J         = JMI(NSG)
C
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
C
         ZF1   =    ZF(NUBO1)
         ZF2   =    ZF(NUBO2)
C
         HI0   =HSTOK(NUBO1)
         HJ0   =HSTOK(NUBO2)
C
C   STICKS TO 1ST ORDER FOR A COVERED EDGE
C
         IF(ZF1.GE. (HJ0+ZF2) .OR. ZF2.GE. (HI0+ZF1)
     &  .OR. 2.*ABS(DSZ(1,NSG)).GE.HI0
     &  .OR. 2.*ABS(DSZ(1,NSG)).GE.HJ0
     &  .OR. 2.*ABS(DSZ(2,NSG)).GE.HI0
     &  .OR. 2.*ABS(DSZ(2,NSG)).GE.HJ0)  THEN
         DST(1,NSG) =0.D0
         DST(2,NSG) =0.D0
        ELSE
C
         AIX       = CMI(1,NSG)-X(NUBO1)
         AIY       = CMI(2,NSG)-Y(NUBO1)
         AJX       = CMI(1,NSG)-X(NUBO2)
         AJY       = CMI(2,NSG)-Y(NUBO2)
C
         GRADI  = AIX*DXT(NUBO1) + AIY*DYT(NUBO1)
         GRADJ  = AJX*DXT(NUBO2) + AJY*DYT(NUBO2)
         GRADIJ  = AIX*DJXT(J) + AIY*DJYT(J)
         GRADJI  = AJX*DJXT(J) + AJY*DJYT(J)
C
C    EXTRAPOLATES THE GRADIENTS AND SLOPE LIMITOR
C
         ILIM=2
         BETA=0.3333D0
         DST(1,NSG)  = EXLIM (ILIM,BETA,GRADI,GRADIJ)
         DST(2,NSG)  = EXLIM (ILIM,BETA,GRADJ,GRADJI)
C
       ENDIF
       ENDDO
C
      DO NSG=1,NSEG
C
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
C
         IF(DST(1,NSG).GE.0.D0) THEN
         DSP(NUBO1) = DSP(NUBO1) +
     &  AIRST(1,NSG)* HCSTOK(1,NSG)*DST(1,NSG)
         ELSE
         DSM(NUBO1) = DSM(NUBO1) -
     &  AIRST(1,NSG)* HCSTOK(1,NSG)*DST(1,NSG)
         ENDIF
         IF(DST(2,NSG).GE.0.) THEN
         DSP(NUBO2) = DSP(NUBO2) +
     &  AIRST(2,NSG)* HCSTOK(2,NSG)*DST(2,NSG)
         ELSE
         DSM(NUBO2) = DSM(NUBO2) -
     &  AIRST(2,NSG)* HCSTOK(2,NSG)*DST(2,NSG)
         ENDIF
C
      ENDDO
C
C     COMPUTES THE CORRECTIONS TO ENSURE CONSERVATION OF HT
C                  ***********           ******************
C
      DO IS=1,NS
       CORRT(IS) =  DSM(IS) - DSP(IS)
       AMDS =MAX(DSP(IS),DSM(IS))
        IF(AMDS.GT.0.D0) THEN
        CORRT(IS) = CORRT(IS)/AMDS
        ENDIF
      ENDDO
 12       CONTINUE
C
      ENDIF
C
C     COMPUTES FLUXES FOR THE INTERNAL INTERFACES
C
      DO 500 NSG=1,NSEG
C
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
C
         UAS41     = TN(NUBO1)
         UAS42     = TN(NUBO2)
C
         FLU11=FLUXT(NSG)
C
         IF (FLU11.GE.0.) THEN
       IF(NORDRE.GE.2) THEN
          UAS41 = UAS41  + DST(1,NSG) +
     & MIN(0.D0,CORRT(NUBO1))*MAX(0.D0,DST(1,NSG))+
     & MAX(0.D0,CORRT(NUBO1))*MAX(0.D0,-DST(1,NSG))
        ENDIF
         FLU41 =  UAS41 * FLU11
         ELSE
       IF(NORDRE.GE.2) THEN
           UAS42 = UAS42 + DST(2,NSG) +
     & MIN(0.D0,CORRT(NUBO2))*MAX(0.D0,DST(2,NSG))+
     & MAX(0.D0,CORRT(NUBO2))*MAX(0.D0,-DST(2,NSG))
       ENDIF
         FLU41 =  UAS42 * FLU11
        ENDIF
C
         CET(NUBO1) = CET(NUBO1) - FLU41
C
         CET(NUBO2) = CET(NUBO2) + FLU41
C
500   CONTINUE
C
C     BOUNDARY FLUX
C
      DO K=1,NPTFR
       IS=NBOR(K)
C
       FLUH =FLUHBOR(K)
C
       IF(FLUH.GE.0.D0) THEN
         FLUT= TN(IS)*FLUH
         FLUTSOR = FLUTSOR +FLUT
       ELSE
         FLUT= TBOR(K)*FLUH
         FLUTENT = FLUTENT +FLUT
       ENDIF
C
       CET(IS)  = CET(IS) - FLUT
C
      ENDDO
C
C     UPDATES HT
C
      DO  IS =1,NS
C
        HT(IS)  = HTN(IS) +  (CET(IS)+SMTR(IS))/AIRS(IS)
        MASSOU = MASSOU + SMTR(IS)
C
        IF(HT(IS).LE.1.D-15) HT(IS)=0.D0
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