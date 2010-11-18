C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES FLUXES OF ROE TYPE (INTERNAL AND BOUNDARY FLUXES)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRS, DDMIN, EPS, FLUENT, FLUSCE, FLUSORT, FLUX, G, KDDL, KDIR, KNEU, LIMPRO, NBOR, NELEM, NPOIN, NPTFR, NSEG, NUBO, VNOIN, W, WINF, X, XNEBOR, Y, YNEBOR, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, A2, CT, D1, DIJ, FLULOC, HI, HJ, IEL, IEL1, IEL2, IELM, INDIC, ISEGIN, K, KPRINT, PRI, RNORM, UI, UJ, USN, VI, VJ, XN, YN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FLUROE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FLUSRC(), FLUXE()
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
!>      <td><center> 5.2                                       </center>
!> </td><td> 19/08/1994
!> </td><td> N.GOUTAL
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AIRS
!></td><td>--></td><td>AIRES DES CELLULES DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>DDMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EPS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUENT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUSORT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUX
!></td><td><--</td><td>TABLEAU DES FLUX DE TYPE ROE
!>    </td></tr>
!>          <tr><td>G
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NDIM
!></td><td>--></td><td>DIMENSION DE L'ESPACE.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE TOTAL DE SEGMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NUBO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VNOIN
!></td><td>--></td><td>NORMALE DU SEGMENT INTERNE
!>                  (2 PREMIERES COMPOSANTES) ET
!>                  LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
!>    </td></tr>
!>          <tr><td>W
!></td><td>--></td><td>VARIABLES CONSERVATIVES DU PB A L'INSTANT N
!>    </td></tr>
!>          <tr><td>WINF
!></td><td>--></td><td>FLUX FRONTIERE ENTREE-SORTIE SI STAT.
!>                  WINF INITIAL SI INSTAT.
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>FOND.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUROE
     &(W,FLUSCE,NUBO,VNOIN,WINF,FLUX,FLUSORT,FLUENT,
     & NELEM,NSEG,NPTFR,NPOIN,X,Y,AIRS,ZF,EPS,DDMIN,G,
     & XNEBOR,YNEBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRS           |-->| AIRES DES CELLULES DU MAILLAGE.
C| DDMIN          |---| 
C| EPS            |---| 
C| FLUENT         |---| 
C| FLUSCE         |---| 
C| FLUSORT        |---| 
C| FLUX           |<--| TABLEAU DES FLUX DE TYPE ROE
C| G             |---| 
C| KDDL           |---| 
C| KDIR           |---| 
C| KNEU           |---| 
C| LIMPRO         |---| 
C| NBOR           |---| 
C| NDIM           |-->| DIMENSION DE L'ESPACE.
C| NELEM          |---| 
C| NPOIN          |---| 
C| NPTFR          |---| 
C| NSEG           |-->| NOMBRE TOTAL DE SEGMENTS DU MAILLAGE
C| NUBO           |---| 
C| VNOIN          |-->| NORMALE DU SEGMENT INTERNE
C|                |   | (2 PREMIERES COMPOSANTES) ET
C|                |   | LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
C| W             |-->| VARIABLES CONSERVATIVES DU PB A L'INSTANT N
C| WINF           |-->| FLUX FRONTIERE ENTREE-SORTIE SI STAT.
C|                |   | WINF INITIAL SI INSTAT.
C| X             |---| 
C| XNEBOR         |---| 
C| Y             |---| 
C| YNEBOR         |---| 
C| ZF             |---| FOND.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_TELEMAC2D, EX_FLUROE => FLUROE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NELEM,NSEG,NPTFR,KDIR,KNEU,KDDL
      INTEGER, INTENT(IN) :: NUBO(2,*),LIMPRO(NPTFR,6),NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN),W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: AIRS(NPOIN),ZF(NPOIN),VNOIN(3,*)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: WINF(3,NPTFR),G,EPS
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(NPOIN,3),DDMIN
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSCE(3,NPOIN),FLUSORT,FLUENT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IEL,ISEGIN,IEL1,IEL2,INDIC,K,KPRINT,IELM(2)
C
      DOUBLE PRECISION DIJ,A1,A2,FLULOC(3)
      DOUBLE PRECISION D1,HI,UI,VI,HJ,VJ,UJ,XN,YN,RNORM,CT,PRI,USN
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
C------
C 1. INITIALISATIONS
C------
C
      D1 = 0.3D0
C
      FLULOC(1) = 0.D0
      FLULOC(2) = 0.D0
      FLULOC(3) = 0.D0
C
      DO 20  IEL =  1 , NPOIN
        FLUX(IEL,1) = 0.D0
        FLUX(IEL,2) = 0.D0
        FLUX(IEL,3) = 0.D0
        FLUSCE(1,IEL) = 0.D0
        FLUSCE(2,IEL) = 0.D0
        FLUSCE(3,IEL) = 0.D0
20    CONTINUE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C------
C 2. COMPUTES INTERNAL FLUXES
C------
C
C XXX   LOOP ON INTERNAL SEGMENTS
C       ----------------------------------
C
        DDMIN = 10000.D0
C
        DO 100 ISEGIN = 1 , NSEG
C
         IEL1 = NUBO(1,ISEGIN)
         IEL2 = NUBO(2,ISEGIN)
         KPRINT =0
       INDIC =0
C
C
C   --->    SOME INTERMEDIATE CALCULATIONS
C           ------------------------------
C
       HI = W(1,IEL1)
C
       IF(HI.GT.EPS) THEN
         UI = W(2,IEL1) / HI
         VI = W(3,IEL1) / HI
       ELSE
         INDIC = INDIC + 1
         UI = 0.D0
         VI = 0.D0
         HI = 0.D0
       ENDIF
C
       HJ = W(1,IEL2)
C
       IF(HJ.GT.EPS) THEN
         UJ = W(2,IEL2) / HJ
         VJ = W(3,IEL2) / HJ
       ELSE
         INDIC = INDIC + 1
         UJ = 0.D0
         VJ = 0.D0
         HJ = 0.D0
       ENDIF
C
       DIJ = SQRT((X(IEL1)-Y(IEL2))**2+(Y(IEL1)-Y(IEL2))**2)
       IF(DIJ.LE.DDMIN) THEN
         DDMIN=DIJ
         IELM(1)=IEL1
         IELM(2)=IEL2
       ENDIF
       RNORM = VNOIN(3,ISEGIN)
C
       IF(INDIC.LT.2) THEN
         XN = VNOIN (1,ISEGIN)
         YN = VNOIN (2,ISEGIN)
C
       CALL FLUXE(HJ,UJ,VJ,HI,UI,VI,XN,YN,RNORM,A1,A2,G,FLULOC)
C
       CALL FLUSRC(IEL1,IEL2,ISEGIN,VNOIN,W,FLUSCE,X,Y,AIRS,NPOIN,
     &              NSEG,ZF,EPS,G)
C
          ELSE
        FLULOC(1)= 0.D0
        FLULOC(2)= 0.D0
        FLULOC(3)= 0.D0
        FLUSCE(1,IEL1)=0.D0
        FLUSCE(2,IEL1)=0.D0
        FLUSCE(3,IEL1)=0.D0
        FLUSCE(1,IEL2)=0.D0
        FLUSCE(2,IEL2)=0.D0
        FLUSCE(3,IEL2)=0.D0
C
         ENDIF
C
         FLUX(IEL1,1)=FLUX(IEL1,1)+FLULOC(1)*RNORM+FLUSCE(1,IEL1)
         FLUX(IEL1,2)=FLUX(IEL1,2)+FLULOC(2)*RNORM+FLUSCE(2,IEL1)
         FLUX(IEL1,3)=FLUX(IEL1,3)+FLULOC(3)*RNORM+FLUSCE(3,IEL1)
         FLUX(IEL2,1)=FLUX(IEL2,1)-FLULOC(1)*RNORM+FLUSCE(1,IEL2)
         FLUX(IEL2,2)=FLUX(IEL2,2)-FLULOC(2)*RNORM+FLUSCE(2,IEL2)
         FLUX(IEL2,3)=FLUX(IEL2,3)-FLULOC(3)*RNORM+FLUSCE(3,IEL2)
C
 100     CONTINUE
C
C
C------
C 3. COMPUTES BOUNDARY FLUXES
C------
C
C        SETS TO ZERO
C
         FLUSORT = 0.D0
         FLUENT  = 0.D0
C
C ====>   ENTRY FLUXES
C
      DO 200 K = 1 , NPTFR
C
      IEL = NBOR(K)
      INDIC = 0
C
      IF(LIMPRO(K,1).EQ.KDIR.OR.LIMPRO(K,1).EQ.KDDL) THEN
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C   --->    SOME INTERMEDIATE CALCULATIONS
C           -------------------------------
C
C     IF H IMPOSED
      IF(LIMPRO(K,1).EQ.KDIR) THEN
       HJ = WINF(1,K)
       IF(HJ.GT.EPS) THEN
         UJ = WINF(2,K) / HJ
         VJ = WINF(3,K) / HJ
       ELSE
         HJ = 0.D0
         UJ = 0.D0
         VJ = 0.D0
         INDIC = INDIC+1
       ENDIF
       HI = W(1,IEL)
       IF(HI.GT.EPS) THEN
         UI = W(2,IEL) / HI
         VI = W(3,IEL) / HI
       ELSE
         HI = 0.D0
         UI = 0.D0
         VI = 0.D0
         INDIC = INDIC+1
       ENDIF
       XN = XNEBOR(K)
       YN = YNEBOR(K)
       RNORM = SQRT( XNEBOR(K+NPTFR)**2 + YNEBOR(K+NPTFR)**2 )
C
       IF(INDIC.LT.2) THEN
       CALL FLUXE(HJ,UJ,VJ,HI,UI,VI,XN,YN,RNORM,A1,A2,G,FLULOC)
       FLUSORT = FLUSORT + FLULOC(1)*RNORM
       ELSE
        FLULOC(1)= 0.D0
        FLULOC(2)= 0.D0
        FLULOC(3)= 0.D0
       ENDIF
C
       ELSE
       HI = WINF(1,K)
       IF(HI.GT.EPS) THEN
         UI = WINF(2,K) / HI
         VI = WINF(3,K) / HI
       ELSE
         HI = 0.D0
         UI = 0.D0
         VI = 0.D0
         INDIC = INDIC+1
       ENDIF
       HJ = W(1,IEL)
       IF(HJ.GT.EPS) THEN
         UJ = W(2,IEL) / HJ
         VJ = W(3,IEL) / HJ
       ELSE
         HJ = 0.D0
         UJ = 0.D0
         VJ = 0.D0
         INDIC = INDIC+1
       ENDIF
       XN = XNEBOR(K)
       YN = YNEBOR(K)
       RNORM = SQRT( XNEBOR(K+NPTFR)**2 + YNEBOR(K+NPTFR)**2 )
C
       IF(INDIC.LT.2) THEN
C
C
       CALL FLUXE(HI,UI,VI,HJ,UJ,VJ,XN,YN,RNORM,A1,A2,G,FLULOC)
C
         FLUENT = FLUENT + FLULOC(1)*RNORM
C
          ELSE
        FLULOC(1)= 0.D0
        FLULOC(2)= 0.D0
        FLULOC(3)= 0.D0
C
         ENDIF
         ENDIF
C
         FLUX(IEL,1)=FLUX(IEL,1)+FLULOC(1)*RNORM
         FLUX(IEL,2)=FLUX(IEL,2)+FLULOC(2)*RNORM
         FLUX(IEL,3)=FLUX(IEL,3)+FLULOC(3)*RNORM
C
C------
C 5. COMPUTES SOLID BOUNDARY FLUXES
C------
C
       ELSEIF(LIMPRO(K,1).EQ.KNEU) THEN
C
C
        IF(W(1,IEL).GT.0.) THEN
          PRI =G*( W(1,IEL)*W(1,IEL))/2.D0
          USN = (W(2,IEL)*XNEBOR(K)+W(3,IEL)*YNEBOR(K))/W(1,IEL)
          CT = SQRT(G*W(1,IEL))
C
          IF((USN+2.D0*CT).LE.0.D0) THEN
CGODUNOV
             PRI =0.D0
C
          ELSEIF(((USN+2.D0*CT).GE.0.D0) .AND. (USN.LE.0.D0)) THEN
CGODUNOV
             PRI = PRI * (1.D0 +  USN / 2.D0 / CT)
     &                 * (1.D0 +  USN / 2.D0 / CT)
     &                 * (1.D0 +  USN / 2.D0 / CT)
     &                 * (1.D0 +  USN / 2.D0 / CT)
C
          ELSEIF(USN.GE.0.D0) THEN
CVFROENC
             PRI = PRI*(1.D0 + 2.D0 * USN / CT)
C
          ENDIF
C
          FLUX(IEL,2) = FLUX(IEL,2) + XNEBOR(K+NPTFR) * PRI
          FLUX(IEL,3) = FLUX(IEL,3) + YNEBOR(K+NPTFR) * PRI
C
        ENDIF
C
C
CCCCCCCCCCCCCC
C
       ENDIF
200   CONTINUE
C
C-----------------------------------------------------------------------
C
 1000 FORMAT(' LE SEGMENT ',I2,' NE CORRESPOND PAS A UNE ENTREE ')
 1010 FORMAT(' LE SEGMENT ',I2,' NE CORRESPOND PAS A UNE SORTIE ')
 1001 FORMAT(' VALEUR DE < U , N > : ',E12.5)
C
      RETURN
      END
C
C#######################################################################
C