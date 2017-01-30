! global variable for recirculation per class
        MODULE M1_RECIRCMODULE
!              use declarations_sisyphe :: nsicla

        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
!              DOUBLE PRECISION  :: Q_OUTCLA(NSICLA)
              DOUBLE PRECISION  :: Q_OUTCLA(10)=0.0000001
!
              INTEGER       :: OUTPUTCOUNTER = 0
!
        END MODULE M1_RECIRCMODULE
