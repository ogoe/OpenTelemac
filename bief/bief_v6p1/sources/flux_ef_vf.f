C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODIFIES THE FLUXES FOR THE FINITE VOLUME SCHEME.
!><br>           (LEO POSTMA'S METHOD, SAME AS IN DELWAQ).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELTSEG, FLOW, FN, IKLE, INIFLO, IOPT, NELEM, NSEG, ORISEG, PHIEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, A2, A3, ALFA, BETA1FI, BETA2FI, BETA3FI, CSTE, F1, F12, F13, F2, F21, F23, F3, F31, F32, FI, FIMAX1, FIMAX2, FLU12, FLU13, FLU21, FLU23, FLU31, FLU32, FMINUS, FN1, FN2, FN3, FP, FPLUS, IELEM, IELMAX, ISEG, THIRD
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FLUX_EF_VF
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CVTRVF(), CVTRVF_POS(), FILTER_H(), FLUX_EF_VF_3D(), POSITIVE_DEPTHS(), TEL4DEL()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 27/10/2009
!> </td><td> LEO POSTMA (DELTARES)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 01/10/2009
!> </td><td> JMH
!> </td><td> OPTION -1 ADDED, ARGUMENT FN ADDED, PSI SCHEME ADDED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 06/05/2009
!> </td><td> JMH
!> </td><td> OPTIMISATION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ELTSEG
!></td><td>--></td><td>SEGMENTS OF EVERY TRIANGLE.
!>    </td></tr>
!>          <tr><td>FLOW
!></td><td><--</td><td>FLUXES PER SEGMENTS
!>    </td></tr>
!>          <tr><td>FN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INIFLO
!></td><td>--></td><td>IF(YES) FLOW WILL BE INITIALISED AT 0.
!>    </td></tr>
!>          <tr><td>IOPT
!></td><td>--></td><td>OPTION FOR THE CONSTANT PER ELEMENT
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE DE SEGMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>ORISEG
!></td><td>--></td><td>ORIENTATION OF SEGMENTS OF EVERY TRIANGLE.
!>    </td></tr>
!>          <tr><td>PHIEL
!></td><td>--></td><td>PER ELEMENT, FLUXES LEAVING POINTS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUX_EF_VF
     &(FLOW,PHIEL,NSEG,NELEM,ELTSEG,ORISEG,IKLE,INIFLO,IOPT,FN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELTSEG         |-->| SEGMENTS OF EVERY TRIANGLE.
C| FLOW           |<--| FLUXES PER SEGMENTS
C| FN             |---| 
C| IKLE           |---| 
C| INIFLO         |-->| IF(YES) FLOW WILL BE INITIALISED AT 0.
C| IOPT           |-->| OPTION FOR THE CONSTANT PER ELEMENT
C| NELEM          |---| 
C| NSEG           |-->| NOMBRE DE SEGMENTS DANS LE MAILLAGE.
C| ORISEG         |-->| ORIENTATION OF SEGMENTS OF EVERY TRIANGLE.
C| PHIEL          |-->| PER ELEMENT, FLUXES LEAVING POINTS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_FLUX_EF_VF => FLUX_EF_VF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)                  :: NSEG,IOPT,NELEM
      INTEGER, INTENT(IN)                  :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)                  :: ORISEG(NELEM,3)
      INTEGER, INTENT(IN)                  :: IKLE(NELEM,3)
      DOUBLE PRECISION, INTENT(INOUT)      :: FLOW(NSEG)
      DOUBLE PRECISION, INTENT(IN)         :: PHIEL(NELEM,3)
      LOGICAL, INTENT(IN)                  :: INIFLO
      TYPE(BIEF_OBJ), INTENT(IN), OPTIONAL :: FN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,ISEG
      DOUBLE PRECISION A1,A2,A3,F1,F2,F3,THIRD,CSTE,F12,F23,F31
      DOUBLE PRECISION FP,FPLUS,FMINUS,FN1,FN2,FN3,F21,F32,F13,ALFA
      DOUBLE PRECISION FLU12,FLU21,FLU23,FLU32,FLU13,FLU31
      DOUBLE PRECISION BETA1FI,BETA2FI,BETA3FI,FI
C
      DOUBLE PRECISION FIMAX1,FIMAX2
      INTEGER IELMAX
C
      INTRINSIC ABS,MIN,MAX
C
      THIRD=1.D0/3.D0
C
C-----------------------------------------------------------------------
C
C     INITIALISES FLOW TO 0.D0
C
      IF(INIFLO) THEN
        DO ISEG = 1,NSEG
          FLOW(ISEG) = 0.D0
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(IOPT.EQ.-1) THEN
C
C-----------------------------------------------------------------------
C
C     FLUXES ALREADY COMPUTED BEFORE CALLING THIS SUBROUTINE
C     THEY ARE JUST ASSEMBLED HERE
C
      DO IELEM = 1,NELEM
C       SEGMENT 1
        ISEG  = ELTSEG(IELEM,1)
        IF(ORISEG(IELEM,1).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + PHIEL(IELEM,1)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - PHIEL(IELEM,1)
        ENDIF
C       SEGMENT 2
        ISEG  = ELTSEG(IELEM,2)
        IF(ORISEG(IELEM,2).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + PHIEL(IELEM,2)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - PHIEL(IELEM,2)
        ENDIF
C       SEGMENT 3
        ISEG  = ELTSEG(IELEM,3)
        IF(ORISEG(IELEM,3).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + PHIEL(IELEM,3)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - PHIEL(IELEM,3)
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(IOPT.EQ.0) THEN
C
C-----------------------------------------------------------------------
C
C     WITH NO CONSTANT
C
      DO IELEM = 1,NELEM
        F1 = PHIEL(IELEM,1)
        F2 = PHIEL(IELEM,2)
        F3 = PHIEL(IELEM,3)
C       SEGMENT 1
        ISEG  = ELTSEG(IELEM,1)
        IF(ORISEG(IELEM,1).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F1-F2)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F1-F2)
        ENDIF
C       SEGMENT 2
        ISEG  = ELTSEG(IELEM,2)
        IF(ORISEG(IELEM,2).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F2-F3)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F2-F3)
        ENDIF
C       SEGMENT 3
        ISEG  = ELTSEG(IELEM,3)
        IF(ORISEG(IELEM,3).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F3-F1)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F3-F1)
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(IOPT.EQ.1) THEN
C
C-----------------------------------------------------------------------
C
C     MINIMISES MAX ( ABS(FLOW) )
C
      DO IELEM = 1,NELEM
        F1 = PHIEL(IELEM,1)
        F2 = PHIEL(IELEM,2)
        F3 = PHIEL(IELEM,3)
        CSTE=-0.5D0*(MIN(F1-F2,F2-F3,F3-F1)+MAX(F1-F2,F2-F3,F3-F1))
C       SEGMENT 1
        ISEG  = ELTSEG(IELEM,1)
        IF(ORISEG(IELEM,1).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F1-F2+CSTE)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F1-F2+CSTE)
        ENDIF
C       SEGMENT 2
        ISEG  = ELTSEG(IELEM,2)
        IF(ORISEG(IELEM,2).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F2-F3+CSTE)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F2-F3+CSTE)
        ENDIF
C       SEGMENT 3
        ISEG  = ELTSEG(IELEM,3)
        IF(ORISEG(IELEM,3).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F3-F1+CSTE)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F3-F1+CSTE)
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(IOPT.EQ.2) THEN
C
C-----------------------------------------------------------------------
C
C     LEO POSTMA'S METHOD (EQUIVALENT TO FLUXES GIVEN BY N-SCHEME)
C
      DO IELEM = 1,NELEM
        F1 = PHIEL(IELEM,1)
        F2 = PHIEL(IELEM,2)
        F3 = PHIEL(IELEM,3)
        A1 = ABS(F1)
        A2 = ABS(F2)
        A3 = ABS(F3)
        IF(A1.GE.A2.AND.A1.GE.A3) THEN
C         ALL FLOW TO AND FROM NODE 1
          ISEG  = ELTSEG(IELEM,1)
          IF(ORISEG(IELEM,1).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) - F2
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F2
          ENDIF
          ISEG = ELTSEG(IELEM,3)
          IF(ORISEG(IELEM,3).EQ.2) THEN
            FLOW(ISEG) = FLOW(ISEG) - F3
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F3
          ENDIF
        ELSEIF(A2.GE.A1.AND.A2.GE.A3) THEN
C         ALL FLOW TO AND FROM NODE 2
          ISEG = ELTSEG(IELEM,1)
          IF(ORISEG(IELEM,1).EQ.2) THEN
            FLOW(ISEG) = FLOW(ISEG) - F1
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F1
          ENDIF
          ISEG = ELTSEG(IELEM,2)
          IF(ORISEG(IELEM,2).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) - F3
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F3
          ENDIF
        ELSE
C         ALL FLOW TO AND FROM NODE 3
          ISEG = ELTSEG(IELEM,2)
          IF(ORISEG(IELEM,2).EQ.2) THEN
            FLOW(ISEG) = FLOW(ISEG) - F2
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F2
          ENDIF
          ISEG = ELTSEG(IELEM,3)
          IF(ORISEG(IELEM,3).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) - F1
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F1
          ENDIF
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(IOPT.EQ.3.AND.PRESENT(FN)) THEN
C
C-----------------------------------------------------------------------
C
C     PSI-SCHEME
C
      DO IELEM = 1,NELEM
        F1 = PHIEL(IELEM,1)
        F2 = PHIEL(IELEM,2)
        F3 = PHIEL(IELEM,3)
!
        FN1=FN%R(IKLE(IELEM,1))
        FN2=FN%R(IKLE(IELEM,2))
        FN3=FN%R(IKLE(IELEM,3))
!
C       STARTS WITH N-SCHEME (EQUIVALENT TO LEO POSTMA'S IMPLEMENTATION)
!
        F12=MAX(MIN(F1,-F2),0.D0)
        F23=MAX(MIN(F2,-F3),0.D0)
        F31=MAX(MIN(F3,-F1),0.D0)
        F21=MAX(MIN(F2,-F1),0.D0)
        F32=MAX(MIN(F3,-F2),0.D0)
        F13=MAX(MIN(F1,-F3),0.D0)
!
        BETA1FI=F12*(FN1-FN2)+F13*(FN1-FN3)
        BETA2FI=F21*(FN2-FN1)+F23*(FN2-FN3)
        BETA3FI=F31*(FN3-FN1)+F32*(FN3-FN2)
!
        FI=BETA1FI+BETA2FI+BETA3FI
!
C       NOW PSI-SCHEME
!
C       WHAT FOLLOWS IS INSPIRED FROM SUBROUTINE VC08AA
C       WHERE FIJ IS LIJ
!
        IF(FI.GT.0.D0) THEN
          IF(BETA1FI.GT.FI) THEN
            F12=F12*FI/BETA1FI
            F13=F13*FI/BETA1FI
          ELSEIF(BETA1FI.LT.0.D0) THEN
            F12=0.D0
            F13=0.D0
          ENDIF
          IF(BETA2FI.GT.FI) THEN
            F21=F21*FI/BETA2FI
            F23=F23*FI/BETA2FI
          ELSEIF(BETA2FI.LT.0.D0) THEN
            F21=0.D0
            F23=0.D0
          ENDIF
          IF(BETA3FI.GT.FI) THEN
            F31=F31*FI/BETA3FI
            F32=F32*FI/BETA3FI
          ELSEIF(BETA3FI.LT.0.D0) THEN
            F31=0.D0
            F32=0.D0
          ENDIF
        ELSEIF(FI.LT.0.D0) THEN
          IF(BETA1FI.LT.FI) THEN
            F12=F12*FI/BETA1FI
            F13=F13*FI/BETA1FI
          ELSEIF(BETA1FI.GT.0.D0) THEN
            F12=0.D0
            F13=0.D0
          ENDIF
          IF(BETA2FI.LT.FI) THEN
            F21=F21*FI/BETA2FI
            F23=F23*FI/BETA2FI
          ELSEIF(BETA2FI.GT.0.D0) THEN
            F21=0.D0
            F23=0.D0
          ENDIF
          IF(BETA3FI.LT.FI) THEN
            F31=F31*FI/BETA3FI
            F32=F32*FI/BETA3FI
          ELSEIF(BETA3FI.GT.0.D0) THEN
            F31=0.D0
            F32=0.D0
          ENDIF
        ELSE
          F12=0.D0
          F23=0.D0
          F31=0.D0
          F21=0.D0
          F32=0.D0
          F13=0.D0
        ENDIF
C
C       ASSEMBLES
C
C       SEGMENT 1
        ISEG  = ELTSEG(IELEM,1)
        IF(ORISEG(IELEM,1).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + F12 - F21
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - F12 + F21
        ENDIF
C       SEGMENT 2
        ISEG  = ELTSEG(IELEM,2)
        IF(ORISEG(IELEM,2).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + F23 - F32
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - F23 + F32
        ENDIF
C       SEGMENT 3
        ISEG  = ELTSEG(IELEM,3)
        IF(ORISEG(IELEM,3).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + F31 - F13
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - F31 + F13
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
      ELSE
C
       IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'FLUX_EF_VF :'
          WRITE(LU,*) 'OPTION INCONNUE : ',IOPT
          IF(IOPT.EQ.3.AND..NOT.PRESENT(FN)) THEN
            WRITE(LU,*) 'OPTION 3 : FONCTION CONVECTEE REQUISE'
          ENDIF
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'FLUX_EF_VF:'
          WRITE(LU,*) 'UNKNOWN OPTION: ',IOPT
          IF(IOPT.EQ.3.AND..NOT.PRESENT(FN)) THEN
            WRITE(LU,*) 'OPTION 3: ADVECTED FUNCTION REQUIRED'
          ENDIF
        ENDIF
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C